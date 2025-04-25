options(scipen = 100000, digits = 4) # cancel scientific number format
library(tidyverse)
library(dplyr)
library(DT)
library(lubridate)
library(magrittr)
library(scales)
library(clipr)
library(readxl)
library(janitor)
library(stringr)
library(plotly)
source(file = "~/Yuval V/mything/updated/funcs.R")
# dataset %>% write_csv(file = "../../Desktop/assignment_data.csv")
raw_data <- readxl::read_excel(path = "~/Yuval V/mything/updated/CAS Analytics Craft Demo v2.0.xlsx", sheet = "dataset") 
dataset <- raw_data %>% 
  mutate(
    across(.cols = c(createDate, lastAccessedDate, fraud_date, label_date, login_start_ts, login_end_ts), .fns = as_datetime),
    across(.cols = c(accountType, profileStatus, product_category), .fns = factor),
    across(.cols = c(successful_login_ind, new_device_ind), .fns = as.logical),
    model_score = as.numeric(model_score),
    is_confirmed_fraud = if_else(is_confirmed_fraud==1, T, F),
    case_id = ifelse(case_id=='null', NA, case_id),

    account_age_at_login = as.numeric(difftime(login_start_ts, createDate, units = 'days')),
    maturation_days = as.numeric(difftime(label_date, fraud_date, units = 'days')),
    login_session_time = as.numeric(difftime(login_end_ts, login_start_ts, units = 'mins')),
    login_month = floor_date(login_start_ts, unit = 'month'),
    login_week = floor_date(login_start_ts, unit = 'week'),
    login_day = floor_date(login_start_ts, unit = 'day'),
    # model_percentile = ntile(x = model_score, n = 100),
    # session_time_percentile = ntile(x = login_session_time, n = 100),
    # challenge_failed = (challenge_path != successful_challenge_path),
    login_time_of_day = hour(login_start_ts),
    model_percentile = ntile(model_score, 100), 
    fraud_vs_login_date = (as.Date(fraud_date) == as.Date(login_start_ts)),
    login_status = factor(case_when((is_confirmed_fraud & successful_login_ind) ~ 'Fraud - Successful Login',
                                    (is_confirmed_fraud & (!successful_login_ind)) ~ 'Fraud - Login Blocked',
                                    (!is_confirmed_fraud & successful_login_ind)  ~ 'Legit - Successful Login',
                                    (!is_confirmed_fraud & !(successful_login_ind))  ~ 'Legit - Login Blocked'), 
                          levels = c('Legit - Login Blocked', 'Legit - Successful Login', 'Fraud - Login Blocked', 'Fraud - Successful Login' ), 
                          # labels = , 
                          ordered = T)
    
    
  ) 




kpi_overview <- dataset %>% 
  calc_kpis()
kpi_by_prod <- dataset %>% 
  group_by(product_category) %>% 
  calc_kpis()



kpis_over_time <- list(
  monthly = dataset %>% 
    calc_kpis_over_time(time_col = 'login_month'),
  
  weekly = dataset %>% 
    calc_kpis_over_time(time_col = 'login_week'),
  
  daily = dataset %>% 
    calc_kpis_over_time(time_col = 'login_day')
)

kpis_over_time_plots <- list(
  confirmd_fraud_rate_monthly = kpis_over_time$monthly %>% 
    select(login_month, confirmed_fruad_rate, net_fraud_rate) %>% 
    pivot_longer(cols = c(confirmed_fruad_rate, net_fraud_rate), names_to = "metric") %>% 
    mutate(metric = ifelse(metric == "confirmed_fruad_rate", "Confirmed Fraud", "Net Fraud")) %>% 
    plot_kpi_over_time(date_col = login_month, kpi_col = value, color_sep = metric, title = "Monthly - Fraud Rate") +
    theme(legend.position="bottom") +
    ylab("Fraud Rate"),
  
  confirmd_fraud_rate_weekly = kpis_over_time$weekly %>% 
    select(login_week, confirmed_fruad_rate, net_fraud_rate) %>% 
    pivot_longer(cols = c(confirmed_fruad_rate, net_fraud_rate), names_to = "metric") %>% 
    mutate(metric = ifelse(metric == "confirmed_fruad_rate", "Confirmed Fraud", "Net Fraud")) %>% 
    plot_kpi_over_time(date_col = login_week, kpi_col = value, color_sep = metric, title = "Weekly - Fraud Rate") +
    theme(legend.position="bottom") +
    ylab("Fraud Rate"),
  
  fraud_coverage_monthly = kpis_over_time$monthly %>% 
    plot_kpi_over_time(date_col = login_month, kpi_col = fraud_coverage, title = "Monthly - Fraud Coverage"),
  
  fraud_coverage_weekly = kpis_over_time$weekly %>% 
    plot_kpi_over_time(date_col = login_week, kpi_col = fraud_coverage, title = "Weekly - Fraud Coverage"),
  
  insult_rate_monthly = kpis_over_time$monthly %>% 
    plot_kpi_over_time(date_col = login_month, kpi_col = insult_rate, title = "Monthly - Insult Rate"),
  
  insult_rate_weekly = kpis_over_time$weekly %>% 
    group_by(login_week<='2024-04-14') %>% 
    mutate(moid_april_avg = mean(insult_rate)) %>%
    plot_kpi_over_time(date_col = login_week, kpi_col = insult_rate) +
    geom_point(aes(x = login_week, y = moid_april_avg), shape = 3, size = 0.8, color = "blue", alpha = 0.7) +
    ggtitle("Weekly - Insult Rate", "Horizontal Line: Mid April Avg (before/after)")
    
)


confirmed_fraud_colors <- c('FALSE' = 'darkgreen', 'TRUE' = 'darkred')
confirmed_fraud_colors2 <- c('Good Login' = 'darkgreen', 'Fraud Login' = 'darkred')
fraud_status <- function(df){
  df %>% 
    mutate(is_confirmed_fraud = ifelse(is_confirmed_fraud, 'Fraud Login', 'Good Login'))
}


login_cnt_over_time <- list(
  monthly_fig = dataset %>% 
    filter(year(login_start_ts)<2025) %>% 
    mutate(is_confirmed_fraud = ifelse(is_confirmed_fraud, 'Fraud Login', 'Good Login')) %>% 
    group_by(login_month, is_confirmed_fraud) %>% # login_status
    summarise(login_cnt = n()) %>% 
    ggplot() +
    geom_col(aes(x = login_month, y = login_cnt, fill = is_confirmed_fraud), alpha = 0.4) +
    scale_color_manual("", values = confirmed_fraud_colors2, aesthetics = c('color', 'fill')) +
    scale_y_continuous(name = "Count", labels = comma_format()) +
    scale_x_datetime("Month of Login", date_labels = "%b-%y") +
    
    ggtitle("Monthly Login Volume") +
    theme_bw() +
    theme(legend.position = 'bottom', text = element_text(size = 18),
          axis.text.x = element_text(angle = 45, hjust = 1)), # pix ratio: 650 x 700
  
  weekly_fig = dataset %>% 
    group_by(login_week, is_confirmed_fraud) %>% 
    summarise(login_cnt = n()) %>% 
    ggplot() +
    geom_col(aes(x = login_week, y = login_cnt, color = is_confirmed_fraud, fill = is_confirmed_fraud), alpha = 0.4) +
    scale_color_manual(values = confirmed_fraud_colors, aesthetics = c('color', 'fill')) +
    theme_bw() +
    ggtitle("Weekly Login Volume"),
  
  daily_fig = dataset %>% 
    group_by(login_day, is_confirmed_fraud) %>% 
    summarise(login_cnt = n()) %>% 
    ggplot() +
    geom_col(aes(x = login_day, y = login_cnt, fill = is_confirmed_fraud), alpha = 0.4) +
    scale_color_manual(values = confirmed_fraud_colors, aesthetics = c('color', 'fill')) +
    theme_bw() +
    ggtitle("Weekly Login Volume"),
  
  monthly_fig_by_cat = dataset %>% 
    filter(year(login_start_ts)<2025) %>% 
    mutate(is_confirmed_fraud = ifelse(is_confirmed_fraud, 'Fraud Login', 'Good Login')) %>% 
    group_by(product_category, login_month, is_confirmed_fraud) %>% # login_status
    summarise(login_cnt = n()) %>% 
    ggplot() +
    geom_col(aes(x = login_month, y = login_cnt, fill = is_confirmed_fraud), alpha = 0.4) +
    scale_color_manual("", values = confirmed_fraud_colors2, aesthetics = c('color', 'fill')) +
    facet_wrap(vars(product_category)) +
    scale_y_continuous(name = "Count", labels = comma_format()) +
    scale_x_datetime("Month of Login", date_labels = "%b-%y") +
    theme_bw() +
    ggtitle("Monthly Login Volume", "by Product Category") +
    theme(legend.position = 'bottom', text = element_text(size = 16),
          axis.text.x = element_text(angle = 45, hjust = 1)), # pix ratio: 650 x 700
  
  weekly_fig_by_cat = dataset %>% 
    group_by(product_category, login_week, is_confirmed_fraud) %>% # login_status
    summarise(login_cnt = n()) %>% 
    ggplot() +
    geom_col(aes(x = login_week, y = login_cnt, fill = is_confirmed_fraud), alpha = 0.4) +
    scale_color_manual(values = confirmed_fraud_colors, aesthetics = c('color', 'fill')) +
    facet_wrap(vars(product_category)) +
    theme_bw() +
    ggtitle("Weekly Login Volume - by Product Category")
)

### account age
account_age_density <- dataset %>%
  group_by(is_confirmed_fraud) %>% 
  mutate(med = median(account_age_at_login, na.rm = T)) %>% 
  ggplot2::ggplot() + 
  ggplot2::geom_density(aes(x = account_age_at_login, color = is_confirmed_fraud, fill = is_confirmed_fraud), alpha = 0.4) + 
  scale_color_manual(values = confirmed_fraud_colors, aesthetics = c('color', 'fill')) +
  ggplot2::geom_vline(aes(xintercept = med, colour = is_confirmed_fraud), linetype = 2, alpha = 0.6) +
  ggtitle("Density Account Age At Login", "Median age in V-line") +
  theme_bw() 

### score_distribution
score_nas <- dataset %>% 
  group_by(is_confirmed_fraud) %>% 
  summarise(login_count = n(),
            null_model_score_count = sum(is.na(model_score))) %>% 
  janitor::adorn_totals() %>% 
  mutate(percent_null = null_model_score_count / login_count)

score_distribution <- dataset %>%
  group_by(is_confirmed_fraud) %>% 
  mutate(med = median(model_score, na.rm = T)) %>% 
  ggplot2::ggplot() + 
  ggplot2::geom_density(aes(x = model_score, color = is_confirmed_fraud, fill = is_confirmed_fraud), alpha = 0.4) + 
  scale_color_manual(values = confirmed_fraud_colors, aesthetics = c('color', 'fill')) +
  ggplot2::geom_vline(aes(xintercept = med, colour = is_confirmed_fraud), linetype = 2, alpha = 0.6) +
  ggtitle("Model Score Distribution by Fraud Status", "Median score in V-line") +
  theme_bw() 
### time of day distribution
login_hour_distribution <- dataset %>%
  group_by(is_confirmed_fraud) %>% 
  mutate(med = median(login_time_of_day, na.rm = T)) %>% 
  ggplot2::ggplot() + 
  ggplot2::geom_density(aes(x = login_time_of_day, color = is_confirmed_fraud, fill = is_confirmed_fraud), alpha = 0.4) + 
  scale_color_manual(values = confirmed_fraud_colors, aesthetics = c('color', 'fill')) +
  ggplot2::geom_vline(aes(xintercept = med, colour = is_confirmed_fraud), linetype = 2, alpha = 0.6) +
  ggtitle("Login Hour Distribution by Fraud Status", "Median score in V-line") +
  theme_bw() 

 ### session time
session_time_summary <- dataset %>%
  group_by(is_confirmed_fraud) %>% 
  summarise(median_session_time = median(login_session_time, na.rm = T), 
            avg_session_time = mean(login_session_time, na.rm = T), 
            min_session_time = min(login_session_time), 
            max_session_time = max(login_session_time), 
            null_cnt = sum(is.na(login_session_time)))

session_length_density <- dataset %>%
  mutate(session_time_percentile = ntile(login_session_time, 100)) %>% 
  group_by(is_confirmed_fraud) %>% 
  mutate(med = median(session_time_percentile, na.rm = T)) %>% 
  ggplot2::ggplot() + 
  ggplot2::geom_density(aes(x = session_time_percentile, color = is_confirmed_fraud, fill = is_confirmed_fraud), alpha = 0.4) + 
  scale_color_manual(values = confirmed_fraud_colors, aesthetics = c('color', 'fill')) +
  ggplot2::geom_vline(aes(xintercept = med, colour = is_confirmed_fraud), linetype = 2, alpha = 0.6) +
  ggtitle("Density of Session Time (percentiles)", "Median value in V-line") +
  theme_bw() 

### profileStatus
profile_status_plot <- dataset %>% 
  group_by(profileStatus, is_confirmed_fraud) %>% #
  summarise(count = n()) %>% 
  mutate(share = count/sum(count)) %>% 
  ggplot() +
  geom_col(aes(x = profileStatus, y = count, fill = is_confirmed_fraud), alpha = 0.4) +
  scale_color_manual(values = confirmed_fraud_colors, aesthetics = c('fill', 'color')) +
  theme_bw() +
  ggtitle("Fraud Distribution by Account Status")
### New devices
new_device_sahre_plot <- dataset %>% 
  group_by(new_device_ind, is_confirmed_fraud) %>% #
  summarise(count = n()) %>% 
  mutate(share = count/sum(count)) %>% 
  ggplot() +
  geom_col(aes(x = new_device_ind, y = share, fill = is_confirmed_fraud), alpha = 0.4) +
  scale_color_manual(values = confirmed_fraud_colors, aesthetics = c('fill', 'color')) +
  theme_bw() +
  ggtitle("Fraud Share by Device Status")
new_device_sahre_overtime_plot <- dataset %>% 
  filter(year(login_start_ts)<2025) %>% 
  group_by(new_device_ind, is_confirmed_fraud, login_month) %>% #
  summarise(count = n()) %>% 
  mutate(share = count/sum(count)) %>% 
  ggplot() +
  geom_line(aes(x = login_month, y = share, color = is_confirmed_fraud)) +
  facet_wrap(vars(new_device_ind)) +
  scale_color_manual(values = confirmed_fraud_colors, aesthetics = c('fill', 'color')) +
  theme_bw() +
  ggtitle("Device Status over time")

dataset %>% 
  group_by(device_id) %>%
  summarise(login_cnt = n(),
         dist_accounts = n_distinct(cust_id),
         fraud_logins = sum(is_confirmed_fraud),
         first_loigin = difftime(time1 = max(login_day), time2 = min(login_day), units = 'day')
  ) %>% 
  arrange(desc(dist_accounts)) %>% 
  head(5) #%>% 
  # copy_df()
  


### Maturation
maturation_tab <- dataset %>% 
  filter(is_confirmed_fraud) %>% 
  mutate(maturation_days_int = ceiling(maturation_days)) %>% 
  group_by(maturation_days_int) %>% # product_category, 
  summarise(cnt = n()) %>% 
  mutate(cumm_cnt = cumsum(cnt),
         maturation_pct = cumm_cnt / sum(cnt))


maturation_fig <- maturation_tab %>% 
  ggplot() +
  geom_line(aes(x = maturation_days_int, y = maturation_pct)) + #, colour = product_category
  scale_x_continuous("Maturation Days") +
  scale_y_continuous("% Matured", labels = percent_format(), n.breaks = 6) +
  ggtitle("Days to Maturaion") +
  theme_bw()

### devices
devices <- dataset %>% 
  group_by(device_id) %>% 
  summarise(cnt = n(), 
            new_device_cnt = sum(new_device_ind),
            fraud_cnt = sum(is_confirmed_fraud), 
            fraud_share = fraud_cnt / cnt,
            logged_once = (cnt==1)) %>% 
  group_by(logged_once) %>% 
  summarise(devices_cnt = n(),
            logs_cnt = sum(cnt),
            avg_logs_per_device = logs_cnt / devices_cnt)


### challenges

parse_challenge_path <- function(char_vec){
  a <- gsub(pattern = '\\[|\\]|"', replacement = '', x = char_vec) %>% 
    str_split(",") %>% 
    purrr::map(str_trim) %>% 
    purrr::map_chr(~ paste0(paste(sort(.), collapse = ',')))
  return(a)
    
}

faild_at <- function(test_vec, success_vec, my_str){
  df <- tibble(
    challenge = test_vec,
    success = success_vec,
    res = case_when(!grepl(pattern = my_str, x = test_vec) ~ NA,
                    T ~ grepl(pattern =  my_str, x = test_vec) & (!grepl(pattern = my_str, x = success_vec))
                    )
  )
  return(df$res)
  
}

false_or_na <- function(vec){
  df <- tibble(tst = vec,
               res = (is.na(vec) | (!vec)))
  return(df$res)
}
true_or_na <- function(vec){
  df <- tibble(tst = vec,
               res = (is.na(vec) | (vec)))
  return(df$res)
}

### ask: what % of good guys fail at X and what % of bad guys fail at it
challenges_data <- dataset %>% 
  mutate(
    challenge_path_parssed = parse_challenge_path(challenge_path),
    successful_challenge_path_parssed = parse_challenge_path(successful_challenge_path),
    failed_anything = challenge_path_parssed!=successful_challenge_path_parssed,
    failed_sms = faild_at(test_vec = challenge_path_parssed, success_vec = successful_challenge_path_parssed, my_str = 'sms'),
    failed_pswd = faild_at(test_vec = challenge_path_parssed, success_vec = successful_challenge_path_parssed, my_str = 'pswd'),
    failed_email = faild_at(test_vec = challenge_path_parssed, success_vec = successful_challenge_path_parssed, my_str = 'email'),
    failed_voice = faild_at(test_vec = challenge_path_parssed, success_vec = successful_challenge_path_parssed, my_str = 'voice'),
    failed_next_gen = faild_at(test_vec = challenge_path_parssed, success_vec = successful_challenge_path_parssed, my_str = 'next_gen'),
    
    no_challenge = (challenge_path_parssed==""),
    failed_other = (!no_challenge) & failed_anything & 
      false_or_na(failed_sms) & false_or_na(failed_pswd) & false_or_na(failed_email) & false_or_na(failed_voice) & false_or_na(failed_next_gen)
  )

# tst <- challenges_data %>% 
#   filter(!no_challenge) %>% 
#   mutate(failed_anything = ifelse(failed_anything, "Failed challenge", "Succeeded challenge")) %>% 
#   group_by(is_confirmed_fraud) %>% # failed_anything
#   summarise(login_cnt = n(),
#             sms_challenge_cnt = sum(!is.na(failed_sms)),
#             sms_challenge_fail = sum(failed_sms, na.rm = T),
#             sms_challenge_succes =sum(!failed_sms, na.rm = T),
#             
#             pswd_challenge_cnt = sum(!is.na(failed_pswd)),
#             pswd_challenge_fail = sum(failed_pswd, na.rm = T),
#             pswd_challenge_succes =sum(!failed_pswd, na.rm = T),
#             
#             email_challenge_cnt = sum(!is.na(failed_email)),
#             email_challenge_fail = sum(failed_email, na.rm = T),
#             email_challenge_succes =sum(!failed_email, na.rm = T),
#             
#             voice_challenge_cnt = sum(!is.na(failed_voice)),
#             voice_challenge_fail = sum(failed_voice, na.rm = T),
#             voice_challenge_succes =sum(!failed_voice, na.rm = T),
#             
#             next_gen_challenge_cnt = sum(!is.na(failed_next_gen)),
#             next_gen_challenge_fail = sum(failed_next_gen, na.rm = T),
#             next_gen_challenge_succes =sum(!failed_next_gen, na.rm = T)
#   ) %>% 
#   pivot_longer(cols = c(sms_challenge_cnt, sms_challenge_fail, sms_challenge_succes, 
#                         pswd_challenge_cnt, pswd_challenge_fail, pswd_challenge_succes, 
#                         email_challenge_cnt, email_challenge_fail, email_challenge_succes, 
#                         voice_challenge_cnt, voice_challenge_fail, voice_challenge_succes, 
#                         next_gen_challenge_cnt, next_gen_challenge_fail, next_gen_challenge_succes), 
#                names_to = c("challenge", "stat"), names_pattern = "(sms|pswd|email|voice|next_gen)_challenge_(fail|cnt|succes)") %>% 
#   filter(stat!= 'cnt') %>% 
#   select(is_confirmed_fraud, stat, challenge, value)
  

#   pivot_wider(id_cols = c(is_confirmed_fraud, challenge), )
  
  


# are we challenging the right people???
any_challenge_rate <- challenges_data %>% 
  filter(year(login_start_ts)<2025) %>% 
  mutate(device_status = ifelse(new_device_ind, "New Device", "Existing Device")) %>% 
  group_by(is_confirmed_fraud, device_status, login_month) %>% # login_week
  summarise(cnt= n(), 
            challenge_rate = sum(!no_challenge) / cnt) %>% 
  ggplot() +
  geom_line(aes(x = login_month, y = challenge_rate, colour = is_confirmed_fraud)) +
  facet_wrap(vars(device_status), nrow = 2) +
  scale_color_manual(values = confirmed_fraud_colors, aesthetics = c('fill', 'color')) +
  scale_y_continuous(name = "Challenge Rate", labels = percent_format()) +
  xlab("Login Week") +
  ggtitle("Challenge Rate - Monthly", "Any Challenge") +
  theme_bw() +
  theme(legend.position = "bottom")

# how are we challenging them?
challenge_type <- challenges_data %>% 
  filter(!no_challenge) %>% 
  group_by(is_confirmed_fraud) %>% 
  summarise(login_cnt = n(),
            sms_challenge_cnt = sum(!is.na(failed_sms)),
            pswd_challenge_cnt = sum(!is.na(failed_pswd)),
            email_challenge_cnt = sum(!is.na(failed_email)),
            voice_challenge_cnt = sum(!is.na(failed_voice)),
            next_gen_challenge_cnt = sum(!is.na(failed_next_gen)),
            other_challenge_cnt = sum(is.na(failed_sms) & is.na(failed_pswd) & is.na(failed_email) & is.na(failed_voice) & is.na(failed_next_gen)),
            challenges_cnt = sum(sms_challenge_cnt, pswd_challenge_cnt, email_challenge_cnt, voice_challenge_cnt, next_gen_challenge_cnt, other_challenge_cnt)
            )

challenge_type_distribution <- list(
  overall = challenge_type %>% 
    pivot_longer(cols = c(sms_challenge_cnt, pswd_challenge_cnt, email_challenge_cnt, voice_challenge_cnt, next_gen_challenge_cnt, other_challenge_cnt)) %>% 
    group_by(is_confirmed_fraud) %>% 
    mutate(challenge_share = value/challenges_cnt,
           challenge = GLITr::toproper(str_remove(string = name, pattern = "_challenge_cnt"))) %>% 
    ggplot() +
    geom_col(aes(x = is_confirmed_fraud, y = challenge_share, fill = challenge), alpha = 0.7) +
    scale_y_continuous(name = "Challenge Share", labels = percent_format()) +
    ggtitle("Challenge Type Distribution", "as % of all challenges\nincluding challenged logins only") +
    theme_bw(),
  
  failed_nonfailed_comparison = challenges_data %>% 
    filter(!no_challenge) %>% 
    mutate(failed_anything = ifelse(failed_anything, "Failed challenge", "Succeeded challenge")) %>% 
    group_by(is_confirmed_fraud, failed_anything) %>% 
    summarise(login_cnt = n(),
              sms_challenge_cnt = sum(!is.na(failed_sms)),
              pswd_challenge_cnt = sum(!is.na(failed_pswd)),
              email_challenge_cnt = sum(!is.na(failed_email)),
              voice_challenge_cnt = sum(!is.na(failed_voice)),
              next_gen_challenge_cnt = sum(!is.na(failed_next_gen)),
              other_challenge_cnt = sum(is.na(failed_sms) & is.na(failed_pswd) & is.na(failed_email) & is.na(failed_voice) & is.na(failed_next_gen)),
              challenges_cnt = sum(sms_challenge_cnt, pswd_challenge_cnt, email_challenge_cnt, voice_challenge_cnt, next_gen_challenge_cnt, other_challenge_cnt)
    ) %>% 
    pivot_longer(cols = c(sms_challenge_cnt, pswd_challenge_cnt, email_challenge_cnt, voice_challenge_cnt, next_gen_challenge_cnt, other_challenge_cnt)) %>% 
    group_by(is_confirmed_fraud, failed_anything) %>% 
    mutate(challenge_share = value/challenges_cnt,
           challenge = GLITr::toproper(str_remove(string = name, pattern = "_challenge_cnt"))) %>%
    ggplot() +
    geom_col(aes(x = is_confirmed_fraud, y = challenge_share, fill = challenge), alpha = 0.7) +
    scale_y_continuous(name = "Challenge Share", labels = percent_format()) +
    facet_wrap(vars(failed_anything), nrow = 1) +
    ggtitle("Challenge Type Distribution", "as % of all challenges\nincluding challenged logins only") +
    theme_bw()
)
  



challenge_type_over_time <- challenges_data %>% 
  filter(!no_challenge, year(login_start_ts)<2025) %>% 
  group_by(is_confirmed_fraud, login_month) %>% 
  summarise(login_cnt = n(),
            sms_challenge_cnt = sum(!is.na(failed_sms)) / login_cnt,
            pswd_challenge_cnt = sum(!is.na(failed_pswd)) / login_cnt,
            email_challenge_cnt = sum(!is.na(failed_email)) / login_cnt,
            voice_challenge_cnt = sum(!is.na(failed_voice)) / login_cnt,
            next_gen_challenge_cnt = sum(!is.na(failed_next_gen)) / login_cnt,
            other_challenge_cnt = sum(is.na(failed_sms) & is.na(failed_pswd) & is.na(failed_email) & is.na(failed_voice) & is.na(failed_next_gen)) / login_cnt
  ) %>% 
  pivot_longer(cols = c(sms_challenge_cnt, pswd_challenge_cnt, email_challenge_cnt, voice_challenge_cnt, next_gen_challenge_cnt, other_challenge_cnt)) %>% 
  mutate(challenge = GLITr::toproper(str_remove(string = name, pattern = "_challenge_cnt"))) %>% 
  ggplot() +
  geom_line(aes(x = login_month, y = value, colour = is_confirmed_fraud)) +
  facet_wrap(vars(challenge)) +
  scale_color_manual(values = confirmed_fraud_colors, aesthetics = c('fill', 'color')) +
  scale_y_continuous(name = "Share of Logins", labels = percent_format()) +
  xlab("Login Month") +
  ggtitle("Challenge Type Rate Over Time", "as % of Logins") +
  theme_bw()




fail_rate_over_time <- challenges_data %>% 
  filter(year(login_start_ts)<2025) %>% 
  group_by(is_confirmed_fraud, login_month) %>% 
  summarise(login_cnt= n(), 
            failed_anything_cnt = sum(failed_anything),
            failed_anything_rate = failed_anything_cnt / login_cnt,
            failed_sms_rate = sum(failed_sms, na.rm = T) / sum(!is.na(failed_sms)),
            failed_pswd_rate = sum(failed_pswd, na.rm = T) / sum(!is.na(failed_pswd)),
            failed_email_rate = sum(failed_email, na.rm = T) / sum(!is.na(failed_email)),
            failed_voice_rate = sum(failed_voice, na.rm = T) / sum(!is.na(failed_voice)),
            failed_next_gen_rate = sum(failed_next_gen, na.rm = T) / sum(!is.na(failed_next_gen))
            ) 
fail_rate_over_time_figs <- list(
  failed_anything = fail_rate_over_time %>% 
    ggplot() +
    geom_line(aes(x = login_month, y = failed_anything_rate, colour = is_confirmed_fraud)) +
    scale_color_manual(values = confirmed_fraud_colors, aesthetics = c('fill', 'color')) +
    scale_y_continuous(name = "Rate", labels = percent_format()) +
    xlab("Login Month") +
    ggtitle("Challenge Failure Rate", "% of all logins") +
    theme_bw(),
  
  fail_rate_specific = fail_rate_over_time %>% 
    pivot_longer(cols = c(failed_sms_rate, failed_pswd_rate, failed_email_rate)) %>% 
    # mutate(challenge = GLITr::toproper(str_remove(string = name, pattern = "_challenge_cnt"))) %>% 
    ggplot() +
    geom_line(aes(x = login_month, y = value, colour = is_confirmed_fraud)) +
    facet_wrap(vars(name)) +
    scale_color_manual(values = confirmed_fraud_colors, aesthetics = c('fill', 'color')) +
    scale_y_continuous(name = "Failure Rate", labels = percent_format()) +
    xlab("Login Month") +
    ggtitle("Failure Rate by Challenge Type") +
    theme_bw()
)

  
  




summary(raw_data)
summary(dataset)

# dataset %>% group_by(login_id) %>% summarise(cnt = n()) %>% arrange(desc(cnt))# %>% view()

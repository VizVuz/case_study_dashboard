#####
parse_challenge_path_2 <- function(char_vec){
  a <- gsub(pattern = '\\[|\\]|"', replacement = '', x = char_vec) %>% 
    str_split(",") %>% 
    purrr::map(str_trim) #%>% 
    # purrr::map_chr(~ paste0(paste(sort(.), collapse = ',')))
  return(a)
  
}
# Function to check if a challenge failed
check_failed <- function(challenge, successful_list) {
  !is.na(challenge) & !(challenge %in% successful_list)
}


#opt

seg_th <- tibble(
  segment = c("main blocked", "main allowed", "new_device_1_challenge_success_sms_email"), # 
  th = c(0.74, 0.74, 0.45) 
)

opt_data <- dataset %>% 
  filter(!is.na(model_score)) %>% 
  
  mutate(
    # Extract all challenges into a list
    all_challenges_list = str_remove_all(challenge_path, '\\[|\\]|"') %>%
      str_split(",") %>%
      purrr::map(str_trim),
    # Extract successful challenges into a list
    successful_challenges_list = str_remove_all(successful_challenge_path, '\\[|\\]|"') %>%
      str_split(",") %>%
      purrr::map(str_trim),
    # Create placeholder columns
    first_challenge = purrr::map_chr(all_challenges_list, ~ ifelse(length(.) >= 1, .[1], NA_character_)),
    second_challenge = purrr::map_chr(all_challenges_list, ~ ifelse(length(.) >= 2, .[2], NA_character_)),
    third_challenge = purrr::map_chr(all_challenges_list, ~ ifelse(length(.) >= 3, .[3], NA_character_)),
    # Determine if each challenge failed
    first_challenge_failed = purrr::map2_lgl(first_challenge, successful_challenges_list, check_failed),
    second_challenge_failed = purrr::map2_lgl(second_challenge, successful_challenges_list, check_failed),
    third_challenge_failed = purrr::map2_lgl(third_challenge, successful_challenges_list, check_failed),
    # Clean up intermediate list columns
    all_challenges_list = NULL,
    successful_challenges_list = NULL
  ) %>% 
  mutate(first_challenge = ifelse(first_challenge=="", NA, first_challenge),
         first_challenge_failed = ifelse(is.na(first_challenge), NA, first_challenge_failed),
         second_challenge_failed = ifelse(is.na(second_challenge), NA, second_challenge_failed),
         third_challenge_failed = ifelse(is.na(third_challenge), NA, third_challenge_failed)) 



opt_data1 <- opt_data %>% #### one naive TH on main
  mutate(segment = case_when((model_score >= (seg_th[seg_th$segment=="main blocked", ]$th)) ~ "main blocked", # Login already blocked by main
                             T ~ "main allowed")) %>% 
  left_join(y = tibble(segment = c("main blocked", "main allowed"), # 
                       th = c(0.74, 0.74)), 
            by = "segment") %>% 
  mutate(allow_login = ((model_score<th) & (successful_login_ind)))

opt_data1 %>% group_by(segment, th) %>% 
  summarise(cnt = n(), 
            allow_cnt = sum(allow_login),
            min_score = min(model_score),
            max_score = max(model_score),
            med_score = median(model_score),
            avg_score = mean(model_score),
            fraud_cnt = sum(is_confirmed_fraud),
            legit_cnt = sum(!is_confirmed_fraud), 
            
            .groups = 'drop'
            ) %>% 
  mutate(share_of_fraud = fraud_cnt / sum(fraud_cnt),
         share_of_legit = legit_cnt / sum(legit_cnt)
         )

opt_data2<- opt_data %>% ####  main TH + new device 1st challenge success
  mutate(segment = case_when((model_score >= (seg_th[seg_th$segment=="main blocked", ]$th)) ~ "main blocked", # Login already blocked by main
                             # ((first_challenge %in% c('sms', 'email')) & (!first_challenge_failed) & new_device_ind & login_start_ts<='2024-04-15') ~ 
                             #                                    "new_device_1_challenge_success_sms_email_TT",
                             ((first_challenge %in% c('sms', 'email')) & (!first_challenge_failed) & new_device_ind) ~
                               "new_device_1_challenge_success_sms_email",
                             T ~ "main allowed")) %>% 
  left_join(y = tibble(segment = c("main blocked", "main allowed", "new_device_1_challenge_success_sms_email_TT", "new_device_1_challenge_success_sms_email"), # 
                       th = c(0.74, 0.74, 0.15, 0.15)), 
            by = "segment") %>% 
  mutate(allow_login = ((model_score<th) & (successful_login_ind)))




opt_data2 %>% group_by(segment, th) %>% 
  summarise(cnt = n(), 
            allow_cnt = sum(allow_login),
            min_score = min(model_score),
            max_score = max(model_score),
            med_score = median(model_score),
            avg_score = mean(model_score),
            fraud_cnt = sum(is_confirmed_fraud),
            legit_cnt = sum(!is_confirmed_fraud),
            legit_block = sum((!is_confirmed_fraud) * (!allow_login)),
            
            .groups = 'drop'
  ) %>% 
  mutate(share_of_fraud = fraud_cnt / sum(fraud_cnt),
         share_of_legit = legit_cnt / sum(legit_cnt)
  )


opt_data2_plus_safety <- opt_data %>% ####  main TH + new device 1st challenge success
  group_by(device_id, login_month) %>% 
  arrange(device_id, login_start_ts) %>% 
  mutate(dist_accounts_log = n_distinct(cust_id),
         dist_accounts_log_number = row_number()) %>% 
  ungroup() %>% 
  mutate(segment = case_when((model_score >= (seg_th[seg_th$segment=="main blocked", ]$th)) ~ "main blocked", # Login already blocked by main
                             # ((first_challenge %in% c('sms', 'email')) & (!first_challenge_failed) & new_device_ind & login_start_ts<='2024-04-15') ~ 
                             #                                    "new_device_1_challenge_success_sms_email_TT",
                             ((first_challenge %in% c('sms', 'email')) & (!first_challenge_failed) & new_device_ind) ~
                               "new_device_1_challenge_success_sms_email",
                             (dist_accounts_log_number>=3 & new_device_ind) ~ "device_velo_5plus_dist_accounts_month",
                             T ~ "main allowed")) %>% 
  left_join(y = tibble(segment = c("main blocked", "main allowed", "new_device_1_challenge_success_sms_email_TT", "new_device_1_challenge_success_sms_email", "device_velo_5plus_dist_accounts_month"), # 
                       th = c(0.74, 0.74, 0.15, 0.15, 0.102)), 
            by = "segment") %>% 
  mutate(allow_login = ((model_score<th) & (successful_login_ind)))




opt_data2_plus_safety %>% group_by(segment, th) %>% 
  summarise(cnt = n(), 
            allow_cnt = sum(allow_login),
            min_score = min(model_score),
            max_score = max(model_score),
            med_score = median(model_score),
            avg_score = mean(model_score),
            fraud_cnt = sum(is_confirmed_fraud),
            legit_cnt = sum(!is_confirmed_fraud),
            legit_block = sum((!is_confirmed_fraud) * (!allow_login)),
            
            .groups = 'drop'
  ) %>% 
  mutate(share_of_fraud = fraud_cnt / sum(fraud_cnt),
         share_of_legit = legit_cnt / sum(legit_cnt)
  )


opt_data3<- opt_data %>% ####  main TH + new device 1st challenge success + new device 1st challenge pswd fail
  mutate(segment = case_when((model_score >= (seg_th[seg_th$segment=="main blocked", ]$th)) ~ "main blocked", # Login already blocked by main
                             # ((first_challenge %in% c('sms', 'email')) & (!first_challenge_failed) & new_device_ind & login_start_ts<='2024-04-15') ~ 
                             #   "new_device_1_challenge_success_sms_email_TT",
                             ((first_challenge %in% c('sms', 'email')) & (!first_challenge_failed) & new_device_ind) ~
                               "new_device_1_challenge_success_sms_email",
                             (first_challenge %in% c('pswd') & (first_challenge_failed) & new_device_ind & login_session_time > 1.5) ~ "new_device_1_challenge_fail_pswd_longsession",
                             T ~ "main allowed")) %>% 
  left_join(y = tibble(segment = c("main blocked", "main allowed", "new_device_1_challenge_success_sms_email_TT", "new_device_1_challenge_success_sms_email", "new_device_1_challenge_fail_pswd_longsession"), # 
                       th = c(0.74, 0.74, 0.15, 0.15, 0.1102)), 
            by = "segment") %>% 
  mutate(allow_login = ((model_score<th) & (successful_login_ind)))

opt_data3 %>% group_by(segment, th) %>% 
  summarise(cnt = n(), 
            allow_cnt = sum(allow_login),
            min_score = min(model_score),
            max_score = max(model_score),
            med_score = median(model_score),
            avg_score = mean(model_score),
            fraud_cnt = sum(is_confirmed_fraud),
            legit_cnt = sum(!is_confirmed_fraud), 
            
            .groups = 'drop'
  ) %>% 
  mutate(share_of_fraud = fraud_cnt / sum(fraud_cnt),
         share_of_legit = legit_cnt / sum(legit_cnt)
  )

opt_data4<- opt_data %>% ####  main TH + new device 1st challenge success + new device 1st challenge pswd fail
  mutate(segment = case_when((model_score >= (seg_th[seg_th$segment=="main blocked", ]$th)) ~ "main blocked", # Login already blocked by main
                             # ((first_challenge %in% c('sms', 'email')) & (!first_challenge_failed) & new_device_ind & login_start_ts<='2024-04-15') ~ 
                             #   "new_device_1_challenge_success_sms_email_TT",
                             ((first_challenge %in% c('sms', 'email')) & (!first_challenge_failed) & new_device_ind) ~
                               "new_device_1_challenge_success_sms_email",
                             (first_challenge %in% c('pswd') & (first_challenge_failed) & new_device_ind & login_session_time > 1.5) ~ "new_device_1_challenge_fail_pswd_longsession",
                             (first_challenge %in% c('pswd') & (!first_challenge_failed) & new_device_ind & login_session_time>2) ~ "new_dev_1_challenge_success_pswd",
                             T ~ "main allowed")) %>% 
  left_join(y = tibble(segment = c("main blocked", "main allowed", "new_device_1_challenge_success_sms_email_TT", "new_device_1_challenge_success_sms_email", "new_device_1_challenge_fail_pswd_longsession", "new_dev_1_challenge_success_pswd"), # 
                       th = c(0.74, 0.74, 0.15, 0.15, 0.1102, 0.3648)), 
            by = "segment") %>% 
  mutate(allow_login = ((model_score<th) & (successful_login_ind)))

opt_data4 %>% group_by(segment, th) %>% 
  summarise(cnt = n(), 
            allow_cnt = sum(allow_login),
            min_score = min(model_score),
            max_score = max(model_score),
            med_score = median(model_score),
            avg_score = mean(model_score),
            fraud_cnt = sum(is_confirmed_fraud),
            legit_cnt = sum(!is_confirmed_fraud), 
            
            .groups = 'drop'
  ) %>% 
  mutate(share_of_fraud = fraud_cnt / sum(fraud_cnt),
         share_of_legit = legit_cnt / sum(legit_cnt)
  )

kpis_over_time_opt <- bind_rows(
  opt_data %>% 
    calc_kpis_over_time(time_col = 'login_week') %>% 
    mutate(optimization_status = "Before Optimization"),
  
  opt_data1 %>% 
    calc_kpis_over_time(time_col = 'login_week', login_success_col = allow_login) %>% 
    mutate(optimization_status = "Naive TH Optimization"),
  
  opt_data2 %>% 
    calc_kpis_over_time(time_col = 'login_week', login_success_col = allow_login) %>% 
    mutate(optimization_status = "main + 1seg Optimization"),
  
  opt_data3 %>%
    calc_kpis_over_time(time_col = 'login_week', login_success_col = allow_login) %>%
    mutate(optimization_status = "main + 2seg Optimization"),
  
  opt_data4 %>%
    calc_kpis_over_time(time_col = 'login_week', login_success_col = allow_login) %>%
    mutate(optimization_status = "main + 3seg Optimization")
  
) %>% 
  select(login_week, optimization_status, confirmed_fruad_rate, net_fraud_rate, fraud_coverage, insult_rate) %>% 
  pivot_longer(cols = c(net_fraud_rate, fraud_coverage, insult_rate), names_to = "kpi") %>% 
  group_by(optimization_status, kpi) %>% 
  mutate(kpi = factor(kpi, levels = c("net_fraud_rate", "fraud_coverage", "insult_rate")),
         yearly_avg = mean(value)) %>% 
  ggplot() +
  geom_line(aes(x = login_week, y = value, colour = optimization_status)) +
  scale_y_continuous(labels = percent_format(), n.breaks = 11) +
  facet_wrap(vars(kpi),ncol = 1) +
  geom_hline(aes(yintercept = yearly_avg, colour = optimization_status), linetype = 2, alpha = 0.6) +
  theme_bw() +
  xlab("Login Week") +
  ylab("Rate") +
  theme(legend.position = "bottom") +
  ggtitle("Performance Simulation")
ggplotly(kpis_over_time_opt)


tst_segment1 <- opt_data2 %>% 
  filter(allow_login) %>%
  group_by(device_id, login_month) %>% 
  arrange(device_id, login_start_ts) %>% 
  mutate(dist_accounts_log = n_distinct(cust_id),
         dist_accounts_log_number = row_number()) %>% 
  ungroup() %>% #select(login_month, device_id, cust_id, login_start_ts, dist_accounts_log, dist_accounts_log_number) %>% filter(dist_accounts_log>2) %>% view()
  mutate(
    model_percentile = ntile(model_score, 200),
    seg = (dist_accounts_log_number>=5 & new_device_ind)
    # seg = (first_challenge %in% c('pswd') & (first_challenge_failed)),
    # seg =(true_or_na(failed_pswd) & login_session_time>1.6),
    # seg= ((new_device_ind | true_or_na(failed_pswd)) & login_session_time>1.6 & model_score>0.5)
  ) %>% 
  tst_seg_plot() +
  ggtitle("segment 1")
ggplotly(tst_segment1, tooltip = "text")

tst_segment2 <- opt_data2 %>% 
  filter(allow_login) %>% 
  mutate(
    model_percentile = ntile(model_score, 200),
    seg = (first_challenge %in% c('pswd') & (first_challenge_failed) & new_device_ind & login_session_time > 1.5)
    
  ) %>% 
  tst_seg_plot() +
  ggtitle("segment 2")
ggplotly(tst_segment2, tooltip = "text")

tst_segment3 <- opt_data1 %>% 
  filter(allow_login) %>% 
  mutate(
    model_percentile = ntile(model_score, 100),
    seg = ((first_challenge_failed) & new_device_ind)
    
  ) %>% 
  tst_seg_plot() +
  ggtitle("segment 3")
ggplotly(tst_segment3, tooltip = "text")






########### What do we learn from the below:
# for the group that succeeded in first challenge SMS/Email
#   the majority goes to a 2nd challenge: in most cases SMS -> Email & Email -> SMS but not always
#   They are still arnt failing in that 2nd challenge and are able to log in!
###########
dataset %>% 
 mutate(
    # Extract all challenges into a list
    all_challenges_list = str_remove_all(challenge_path, '\\[|\\]|"') %>%
      str_split(",") %>%
      purrr::map(str_trim),
    # Extract successful challenges into a list
    successful_challenges_list = str_remove_all(successful_challenge_path, '\\[|\\]|"') %>%
      str_split(",") %>%
      purrr::map(str_trim),
    # Create placeholder columns
    first_challenge = purrr::map_chr(all_challenges_list, ~ ifelse(length(.) >= 1, .[1], NA_character_)),
    second_challenge = purrr::map_chr(all_challenges_list, ~ ifelse(length(.) >= 2, .[2], NA_character_)),
    third_challenge = purrr::map_chr(all_challenges_list, ~ ifelse(length(.) >= 3, .[3], NA_character_)),
    # Determine if each challenge failed
    first_challenge_failed = purrr::map2_lgl(first_challenge, successful_challenges_list, check_failed),
    second_challenge_failed = purrr::map2_lgl(second_challenge, successful_challenges_list, check_failed),
    third_challenge_failed = purrr::map2_lgl(third_challenge, successful_challenges_list, check_failed),
    # Clean up intermediate list columns
    all_challenges_list = NULL,
    successful_challenges_list = NULL
  ) %>% 
  mutate(first_challenge = ifelse(first_challenge=="", NA, first_challenge),
         first_challenge_failed = ifelse(is.na(first_challenge), NA, first_challenge_failed),
         second_challenge_failed = ifelse(is.na(second_challenge), NA, second_challenge_failed),
         third_challenge_failed = ifelse(is.na(third_challenge), NA, third_challenge_failed))  %>% 
  
  filter(first_challenge %in% c('sms', 'email'),
         !first_challenge_failed,
         ) %>% 
  group_by(first_challenge#,
           # new_second_challenge = case_when(is.na(second_challenge) ~ "No 2nd Challenge",
           #                                  second_challenge %in% c('email', 'sms', 'pswd') ~ second_challenge,
           #                                  T ~ 'Other'
           #                                  )
           ) %>% 
  summarise(#fraud_cnt = sum(is_confirmed_fraud), 
            #good_cnt = sum(!is_confirmed_fraud),
            #failed_2nd_challenge = sum(second_challenge_failed, na.rm = T),
            #success_2nd_challenge = sum(!second_challenge_failed, na.rm = T),
            # no_sec_challenge = sum(is.na(second_challenge)),
            # login_cnt= n(),
            sec_chal_fail_fraud = sum(second_challenge_failed * is_confirmed_fraud, na.rm = T),
            sec_chal_fail_good = sum(second_challenge_failed * (!is_confirmed_fraud), na.rm = T),
            sec_chal_success_fraud = sum((!second_challenge_failed) * is_confirmed_fraud, na.rm = T),
            sec_chal_success_good = sum((!second_challenge_failed) * (!is_confirmed_fraud), na.rm = T),
            no_2nd_chal_fraud = sum(is.na(second_challenge) * is_confirmed_fraud),
            no_2nd_chal_good = sum(is.na(second_challenge) * (!is_confirmed_fraud))
            )

  
  
  
#   mutate(med = median(model_score, na.rm = T)) %>% 
#   ggplot2::ggplot() + 
#   ggplot2::geom_density(aes(x = model_score, color = is_confirmed_fraud, fill = is_confirmed_fraud), alpha = 0.4) + 
#   scale_color_manual(values = confirmed_fraud_colors, aesthetics = c('color', 'fill')) +
#   ggplot2::geom_vline(aes(xintercept = med, colour = is_confirmed_fraud), linetype = 2, alpha = 0.6) +
#   ggtitle("First Challenge Success - SMS & Email - Score Distribution", "Median score in V-line") +
#   theme_bw() 
# view()







# challenges_data_2 <- challenges_data %>% 
#   # filter(is_confirmed_fraud) %>% 
#   # select(is_confirmed_fraud, challenge_path, successful_challenge_path) %>% 
#   mutate(
#     # Extract all challenges into a list
#     all_challenges_list = str_remove_all(challenge_path, '\\[|\\]|"') %>%
#       str_split(",") %>%
#       purrr::map(str_trim),
#     # Extract successful challenges into a list
#     successful_challenges_list = str_remove_all(successful_challenge_path, '\\[|\\]|"') %>%
#       str_split(",") %>%
#       purrr::map(str_trim),
#     # Create placeholder columns
#     first_challenge = purrr::map_chr(all_challenges_list, ~ ifelse(length(.) >= 1, .[1], NA_character_)),
#     second_challenge = purrr::map_chr(all_challenges_list, ~ ifelse(length(.) >= 2, .[2], NA_character_)),
#     third_challenge = purrr::map_chr(all_challenges_list, ~ ifelse(length(.) >= 3, .[3], NA_character_)),
#     # Determine if each challenge failed
#     first_challenge_failed = purrr::map2_lgl(first_challenge, successful_challenges_list, check_failed),
#     second_challenge_failed = purrr::map2_lgl(second_challenge, successful_challenges_list, check_failed),
#     third_challenge_failed = purrr::map2_lgl(third_challenge, successful_challenges_list, check_failed),
#     # Clean up intermediate list columns
#     all_challenges_list = NULL,
#     successful_challenges_list = NULL
#   ) %>% 
#   mutate(first_challenge = ifelse(first_challenge=="", NA, first_challenge),
#          first_challenge_failed = ifelse(is.na(first_challenge), NA, first_challenge_failed),
#          second_challenge_failed = ifelse(is.na(second_challenge), NA, second_challenge_failed),
#          third_challenge_failed = ifelse(is.na(third_challenge), NA, third_challenge_failed))
#   
# 
# tst %>% 
#   mutate(new_first_challenge = case_when(first_challenge %in% c('pswd', 'sms', 'email') ~ first_challenge,
#                                          is.na(first_challenge) ~ "No Challenge",
#                                          T ~ "Other")) %>% 
#   group_by(is_confirmed_fraud, new_first_challenge) %>% 
#   # filter(first_challenge %in% c('pswd', 'sms', 'email')) %>% 
#   summarise(failed_cnt = sum(first_challenge_failed, na.rm = T),
#             success_cnt = sum(!first_challenge_failed, na.rm = T),
#             login_cnt = n()) #%>% 
  #view()
  


  

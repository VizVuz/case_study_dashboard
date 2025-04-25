#opt

seg_th <- tibble(
  segment = c("main"),
  th = 0.7
)

opt_data <- dataset %>% 
  filter(!is.na(model_score)) %>% 
  mutate(segment = case_when(T ~ "main")) %>% 
  left_join(seg_th, by = "segment") %>% 
  mutate(allow_login = ((model_score<th) & (successful_login_ind)))

kpis_over_time_opt <- bind_rows(
  opt_data %>% 
    calc_kpis_over_time(time_col = 'login_week') %>% 
    mutate(optimization_status = "Before Optimization"),
  
  opt_data %>% 
    calc_kpis_over_time(time_col = 'login_week', login_success_col = allow_login) %>% 
    mutate(optimization_status = "After Optimization")
) %>% 
  select(login_week, optimization_status, confirmed_fruad_rate, net_fraud_rate, fraud_coverage, insult_rate) %>% 
  pivot_longer(cols = c(net_fraud_rate, fraud_coverage, insult_rate), names_to = "kpi") %>% 
  mutate(kpi = factor(kpi, levels = c("net_fraud_rate", "fraud_coverage", "insult_rate"))) %>% 
  ggplot() +
  geom_line(aes(x = login_week, y = value, colour = optimization_status)) +
  scale_y_continuous(labels = percent_format()) +
  facet_wrap(vars(kpi),ncol = 1) +
  theme_bw() +
  xlab("Login Week") +
  ylab("Rate") +
  theme(legend.position = "bottom") +
  ggtitle("Performance Simulation")

##create a tradeoff plots

tradeoff <- opt_data %>%
  calc_tradeoff_df()

tradeoff_plt <- ggplotly(
  tradeoff %>% plot_tradeoff_df(), 
  tooltip = "text")
  
opt_wip <- opt_data %>% 
  filter(allow_login) %>% 
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

tst_segment <- opt_wip %>% 
  mutate(
    model_percentile = ntile(model_score, 100),
    seg = (true_or_na(failed_pswd) & login_session_time>1.6),
    # seg= ((new_device_ind | true_or_na(failed_pswd)) & login_session_time>1.6 & model_score>0.5)
  ) %>% 
  tst_seg_plot()
ggplotly(tst_segment, tooltip = "text")

  



# tst_segment <- opt_wip %>% 
#   mutate(
#     seg = (true_or_na(failed_pswd) & login_session_time>1.6),
#     # seg= ((new_device_ind | true_or_na(failed_pswd)) & login_session_time>1.6 & model_score>0.5)
#     ) %>% 
#   group_by(seg) %>% 
#   summarise(all_cnt = n(),
#             legit_cnt = sum(!is_confirmed_fraud), 
#             fraud_cnt = sum(is_confirmed_fraud), 
#             ratio = fraud_cnt /legit_cnt,
#             fraud_rate = fraud_cnt/all_cnt
#             ) %>% 
#   mutate(share_of_all_fraud = fraud_cnt / sum(fraud_cnt))









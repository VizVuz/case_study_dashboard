calc_kpis <- function(df, login_success_col = successful_login_ind){
  df %>%
    summarise(
      login_cnt = n(),
      net_login_cnt = sum({{login_success_col}}),
      total_fraud_cnt = sum(is_confirmed_fraud),
      net_fraud_cnt = sum(is_confirmed_fraud * {{login_success_col}}),
      
      confirmed_fruad_rate = total_fraud_cnt / login_cnt,
      net_fraud_rate = net_fraud_cnt / net_login_cnt,
      fraud_coverage = (total_fraud_cnt-net_fraud_cnt) / total_fraud_cnt,
      insult_rate = sum((!is_confirmed_fraud) & (!{{login_success_col}})) / sum(!is_confirmed_fraud)
    )
}
calc_kpis_over_time <- function(df, time_col, login_success_col = successful_login_ind){
  df %>% 
    rename(date_col = time_col) %>% 
    group_by(date_col) %>% 
    GLITr::group_split_named() %>% 
    imap(.f = function(x, i){
      x %>% 
        calc_kpis(login_success_col = {{login_success_col}}) %>% 
        mutate(date_col = as.Date(i), .before = 1)
    }) %>% 
    bind_rows() %>% 
    rename({{time_col}} := date_col)
}

plot_kpi_over_time <- function(kpi_df, date_col, kpi_col, color_sep = NULL, title = NULL){
  kpi_df %>% 
    ggplot() +
    geom_line(aes(x = {{date_col}}, y = {{kpi_col}}, colour = {{color_sep}})) +
    scale_y_continuous(labels = percent_format()) +
    theme_bw() +
    xlab(GLITr::toproper(deparse(substitute(date_col)))) +
    ylab(GLITr::toproper(deparse(substitute(kpi_col)))) +
    ggtitle(title)
}

calc_tradeoff_df <- function(df) {
  df %>%
    filter(successful_login_ind) %>%
    group_by(model_percentile) %>%
    summarise(
      min_score = min(model_score),
      max_score = max(model_score),
      login_cnt = n(),
      legit_cnt = sum(!is_confirmed_fraud),
      fraud_cnt = sum(is_confirmed_fraud),
      fraud_rate = fraud_cnt / login_cnt
    ) %>%
    arrange(desc(model_percentile)) %>%
    mutate(
      share_of_all_fraud = fraud_cnt / sum(fraud_cnt),
      cumm_share_of_fraud = cumsum(share_of_all_fraud),
      cumm_share_of_legit = cumsum(legit_cnt / sum(legit_cnt))
    )
}
plot_tradeoff_df <- function(df, xtitle = "Share of Fraud", ytitle = "Insult Rate", title = "Trade-off plot: cost of blockoing fraud in terms of insult rate"){
  df %>% 
    select(model_percentile, th = min_score, share_of_blocked_fraud = cumm_share_of_fraud, insult_rate = cumm_share_of_legit) %>% 
    ggplot() +
    geom_line(aes(x = share_of_blocked_fraud, 
                  y = insult_rate, 
                  group = 1,
                  text = paste0(
                    "TH: ", round(th, 4), " (", model_percentile, " percentile)\n",
                    "Share of Fraud: ", percent(share_of_blocked_fraud, 0.1), "\n",
                    "Insult Rate: ", percent(insult_rate, 0.1)
                  )
    )) +
    scale_y_continuous(labels = percent_format()) +
    scale_x_continuous(labels = percent_format()) +
    xlab(xtitle) +
    ylab(ytitle) +
    ggtitle("Trade-off plot: cost of blockoing fraud in terms of insult rate") +
    theme_bw()
}

tst_seg_plot <- function(df){
  df %>% 
    filter(successful_login_ind) %>%
    group_by(seg, model_percentile) %>%
    summarise(
      min_score = min(model_score),
      max_score = max(model_score),
      login_cnt = n(),
      legit_cnt = sum(!is_confirmed_fraud),
      fraud_cnt = sum(is_confirmed_fraud),
      fraud_rate = fraud_cnt / login_cnt
    ) %>%
    arrange(desc(model_percentile)) %>%
    mutate(
      share_of_all_fraud = fraud_cnt / sum(fraud_cnt),
      cumm_share_of_fraud = cumsum(share_of_all_fraud),
      cumm_share_of_legit = cumsum(legit_cnt / sum(legit_cnt))
    ) %>% 
    select(seg, model_percentile, th = min_score, share_of_blocked_fraud = cumm_share_of_fraud, insult_rate = cumm_share_of_legit) %>% 
    ggplot() +
    geom_line(aes(x = share_of_blocked_fraud, 
                  y = insult_rate, 
                  group = seg,
                  color = seg,
                  text = paste0(
                    "Segment: ", seg, "\n",
                    "TH: ", round(th, 4), " (", model_percentile, " percentile)\n",
                    "Share of Fraud: ", percent(share_of_blocked_fraud, 0.1), "\n",
                    "Insult Rate: ", percent(insult_rate, 0.1)
                  )
    )) +
    scale_y_continuous(labels = percent_format()) +
    scale_x_continuous(labels = percent_format()) +
    xlab("Share of Fraud") +
    ylab("Insult Rate") +
    ggtitle("Trade-off plot: cost of blockoing fraud in terms of insult rate") +
    theme_bw()
}



# tradeoff_plt_1 <- tradeoff %>% 
#   select(model_percentile, th = min_score, share_of_blocked_fraud = cumm_share_of_fraud, insult_rate = cumm_share_of_legit) %>% 
#   pivot_longer(cols = c(share_of_blocked_fraud, insult_rate)) %>% 
#   ggplot() +
#   geom_line(aes(x = model_percentile, 
#                 y = value, 
#                 colour = name,
#                 group = name, 
#                 text = paste0(
#                   "TH: ", round(th, 4), " (", model_percentile, " percentile)\n",
#                   "Cost / Reward:", percent(value, accuracy = 0.1)
#                 ))) +
#   scale_color_manual(values = c("insult_rate" = "darkgreen", "share_of_blocked_fraud" = "darkred")) +
#   scale_y_continuous("Rate", labels = percent_format()) +
#   xlab("Model Percentile") +
#   ggtitle(title) +
#   theme_bw() +
#   theme(legend.position = 'bottom')
# ggplotly(tradeoff_plt, tooltip = "text")


copy_df <- function(df){
  df %>% 
    GLITr::copy_df()
}

pad_html <- function(str, header_num = 0, tab = F, pad = "\n\n"){
  cat(paste0(pad, 
             str_dup("#", header_num), 
             " ", 
             str,
             dplyr::if_else(tab, " {.tabset }", ""),
             pad))
}

create_rmd_table_2 <- function (dt, caption = NULL, col_to_percent = 0, col_to_dollar = 0, col_to_factor = 0,
                                col_to_cockpit = 0, col_to_comma = 0, rownames = FALSE, table_height = NULL, 
                                buttons = NULL, other_extensions = "Buttons", other_options = NULL, fixed_col=NULL,
                                ...) 
{
  filter_val = c("top")
  if (is.null(table_height) & dim(dt)[1]<15){
    table_height_ <- TRUE
  } else if (is.null(table_height) & dim(dt)[1]>=15){
    table_height_ <- 500
  } else table_height_ <- table_height
  colnames(dt) <- colnames(dt) %>% GLITr::toproper()
  
  dt %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(dplyr::across(tidyselect::everything()[col_to_percent], ~scales::percent(.x, accuracy = 0.01))) %>% 
    dplyr::mutate(dplyr::across(tidyselect::everything()[col_to_dollar], ~scales::dollar(.x, accuracy = 1))) %>% 
    dplyr::mutate(dplyr::across(tidyselect::everything()[col_to_comma], ~scales::comma(.x, accuracy = 1))) %>%
    dplyr::mutate(dplyr::across(tidyselect::everything()[col_to_factor], factor)) %>%
    DT::datatable(rownames = rownames, 
                  # filter = "top",
                  caption = caption, 
                  escape = FALSE, 
                  extensions = c("Scroller", "FixedColumns", other_extensions), 
                  options = append(list(#dom = paste0("t", 
                    #             if_else(is.null(buttons), "", "B")), 
                    # dom = "Bfcrtip",
                    scrollX = TRUE, 
                    deferRender = TRUE, 
                    scrollY = table_height_, 
                    scroller = TRUE, 
                    buttons = list(I("colvis"), "csv", "copy"),
                    fixedColumns = list(leftColumns = fixed_col)),
                    other_options), 
                  ...)
}




# str_sym <- function (symbol){
#   tryCatch({
#     suppressWarnings(is.character(symbol))
#     return(symbol)
#   }, error = function(e) {
#     symbol <- rlang::as_label(rlang::enquo(symbol))
#     if (symbol == "NULL" | symbol == "<empty>") {
#       symbol <- NULL
#     }
#     else {
#       symbol <- stringr::str_remove_all(symbol, "^c\\(|\\)$") %>% 
#         stringr::str_split(pattern = ", ") %>% unlist()
#     }
#     return(symbol)
#   })
# }



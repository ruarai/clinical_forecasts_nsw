
plot_morbidity_trajectories <- function(
  morbidity_trajectories_state,
  state_modelled,
  forecast_dates,
  
  morbidity_window_width,
  plot_dir
) {
  
  #cutoff_date <- forecast_dates$hospital_data - days(morbidity_window_width / 2)
  cutoff_date <- forecast_dates$forecast_horizon
  
  plot_data_age <- morbidity_trajectories_state %>%
    filter(date >= ymd("2022-02-01")) %>%
    select(bootstrap, date, age_group, pr_age_given_case) %>%
    
    group_by(date, age_group) %>%
    
    summarise(median = median(pr_age_given_case),
              lower_90 = quantile(pr_age_given_case, 0.05),
              upper_90 = quantile(pr_age_given_case, 0.95)) %>%
    
    filter(date <= cutoff_date)
  
  
  ggplot(plot_data_age) +
    geom_line(aes(x = date, y = median, color = age_group)) +
    
    geom_line(aes(x = date, y = median, group = age_group_old, color = age_group_old),
              alpha = 0.3,
              plot_data_age %>% rename(age_group_old = age_group)) +
    
    geom_ribbon(aes(x = date, ymin = lower_90, ymax = upper_90, fill = age_group),
                alpha = 0.5,
                plot_data_age) +
    
    scale_x_date(date_breaks = "months", labels = scales::label_date_short()) +
    
    geom_vline(xintercept = forecast_dates$hospital_data - ddays(morbidity_window_width / 2),
               linetype = 'dashed') +
    
    ggokabeito::scale_colour_okabe_ito(name = "Age group") +
    ggokabeito::scale_fill_okabe_ito(name = "Age group") +
    scale_linetype(name = "Type") +
    
    facet_wrap(~age_group) +
    
    theme_minimal() +
    ylab(NULL) + xlab("Date") +
    
    ggtitle(paste0(state_modelled, " \u2013 Age distribution, observed"))
  
  ggsave(paste0(plot_dir, paste0("/", state_modelled, "_age_distribution.png")),
         width = 14, height = 9, bg = "white")
  
  
  
  plot_data_hosp <- morbidity_trajectories_state %>%
    filter(date >= ymd("2022-01-10")) %>%
    select(bootstrap, date, age_group, pr_hosp) %>%
    
    group_by(date, age_group) %>%
    
    summarise(median = median(pr_hosp),
              lower_90 = quantile(pr_hosp, 0.05),
              upper_90 = quantile(pr_hosp, 0.95)) %>%
    
    filter(date <= cutoff_date)
  
  
  
  
  ggplot(plot_data_hosp) +
    geom_line(aes(x = date, y = median),
              color = "#b53aa0") +
    
    geom_ribbon(aes(x = date, ymin = lower_90, ymax = upper_90),
                fill = "#b53aa0",
                alpha = 0.3,
                plot_data_hosp) +
    
    geom_vline(xintercept = forecast_dates$hospital_data - ddays(morbidity_window_width / 2),
               linetype = 'dashed') +
    
    scale_x_date(date_breaks = "months", labels = scales::label_date_short()) +
    
    geom_blank(aes(y = 0)) +
    
    
    facet_wrap(~age_group, scales = "free_y") +
    
    theme_minimal() +
    ylab(NULL) + xlab("Date") +
    
    ggtitle(paste0(state_modelled, " \u2013 Probability of hospitalisation, observed"))
  
  
  ggsave(paste0(plot_dir, paste0("/", state_modelled, "_pr_hosp.png")),
         width = 14, height = 9, bg = "white")
  
  
  
  plot_data_hosp_summ <- morbidity_trajectories_state %>%
    filter(date >= ymd("2022-01-10")) %>%
    select(bootstrap, date, age_group, pr_hosp, pr_age_given_case) %>%
    
    group_by(bootstrap, date) %>%
    
    summarise(pr_hosp_total = sum(pr_age_given_case * pr_hosp)) %>%
    
    group_by(date) %>%
    
    summarise(median = median(pr_hosp_total),
              lower_90 = quantile(pr_hosp_total, 0.05),
              upper_90 = quantile(pr_hosp_total, 0.95)) %>%
    
    filter(date <= cutoff_date)
  
  
  ggplot(plot_data_hosp_summ) +
    geom_line(aes(x = date, y = median),
              color = "#b53aa0") +
    
    geom_ribbon(aes(x = date, ymin = lower_90, ymax = upper_90),
                fill = "#b53aa0",
                alpha = 0.3) +
    
    geom_vline(xintercept = cutoff_date,
               linetype = 'dashed') +
    
    scale_x_date(date_breaks = "months", labels = scales::label_date_short()) +
    geom_blank(aes(y = 0)) +
    
    theme_minimal() +
    ylab(NULL) + xlab("Date") +
    
    ggtitle(paste0(state_modelled, " \u2013 Probability of hospitalisation, all ages"))
  
  
  ggsave(paste0(plot_dir, paste0("/", state_modelled, "_pr_hosp_summ.png")),
         width = 14, height = 9, bg = "white")
  
  plot_data_ICU <- morbidity_trajectories_state %>%
    select(bootstrap, date, age_group, pr_ICU) %>%
    
    group_by(date, age_group) %>%
    
    summarise(median = median(pr_ICU),
              lower_90 = quantile(pr_ICU, 0.05),
              upper_90 = quantile(pr_ICU, 0.95)) %>%
    
    filter(date <= cutoff_date)
  
  
  ggplot(plot_data_ICU) +
    geom_line(aes(x = date, y = median),
              color = "#008200") +
    
    geom_ribbon(aes(x = date, ymin = lower_90, ymax = upper_90),
                fill = "#008200",
                alpha = 0.3) +
    
    geom_vline(xintercept = forecast_dates$hospital_data - ddays(7),
               linetype = 'dashed') +
    
    
    scale_x_date(date_breaks = "months", labels = scales::label_date_short()) +
    
    scale_linetype(name = "Type") +
    
    geom_blank(aes(y = 0)) +
    
    
    facet_wrap(~age_group, scales = "free_y") +
    
    theme_minimal() +
    ylab(NULL) + xlab("Date") +
    
    ggtitle(paste0(state_modelled, " \u2013 Probability of ICU admission, observed"))
  
  
  ggsave(paste0(plot_dir, paste0("/", state_modelled, "_pr_ICU.png")),
         width = 14, height = 9, bg = "white")
  
  
  return("")
}


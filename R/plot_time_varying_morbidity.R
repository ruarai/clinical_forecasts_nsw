
plot_morbidity_trajectories <- function(
  morbidity_trajectories_state,
  state_modelled,
  forecast_dates,
  
  morbidity_window_width,
  plot_dir
) {
  
  if("pr_hosp_old" %in% colnames(morbidity_trajectories_state)) {
    
    plot_data_age <- morbidity_trajectories_state %>%
      filter(date >= ymd("2022-02-01")) %>%
      select(bootstrap, date, age_group, pr_age_adj = pr_age_given_case, pr_age_old) %>%
      
      pivot_longer(c(pr_age_adj, pr_age_old), names_prefix = "pr_age_") %>%
      
      group_by(date, age_group, name) %>%
      
      summarise(median = median(value),
                lower_90 = quantile(value, 0.05),
                upper_90 = quantile(value, 0.95))
    
    
    ggplot(plot_data_age) +
      geom_line(aes(x = date, y = median, linetype = name, color = age_group)) +
      
      geom_line(aes(x = date, y = median, linetype = name, group = interaction(age_group_old, name), color = age_group_old),
                alpha = 0.3,
                plot_data_age %>% rename(age_group_old = age_group) %>% filter(name == "adj")) +
      
      geom_ribbon(aes(x = date, ymin = lower_90, ymax = upper_90, fill = age_group),
                  alpha = 0.5,
                  plot_data_age %>% filter(name == "adj")) +
      
      scale_x_date(date_breaks = "months", labels = scales::label_date_short()) +
      
      geom_vline(xintercept = forecast_dates$NNDSS - ddays(morbidity_window_width / 2),
                 linetype = 'dashed') +
      
      ggokabeito::scale_colour_okabe_ito(name = "Age group") +
      ggokabeito::scale_fill_okabe_ito(name = "Age group") +
      scale_linetype(name = "Type") +
      
      facet_wrap(~age_group) +
      
      theme_minimal() +
      ylab(NULL) + xlab("pr_age_given_case") +
      
      ggtitle(paste0(state_modelled, " -- Age distribution, observed and adjusted"))
    
    ggsave(paste0(plot_dir, paste0("/", state_modelled, "_age_distribution.png")),
           width = 14, height = 9, bg = "white")
    
    
    
    plot_data_hosp <- morbidity_trajectories_state %>%
      filter(date >= ymd("2022-02-01")) %>%
      select(bootstrap, date, age_group, pr_hosp_adj = pr_hosp, pr_hosp_old) %>%
      
      pivot_longer(c(pr_hosp_adj, pr_hosp_old), names_prefix = "pr_hosp_") %>%
      
      group_by(date, age_group, name) %>%
      
      summarise(median = median(value),
                lower_90 = quantile(value, 0.05),
                upper_90 = quantile(value, 0.95))
    
    
    
    
    ggplot(plot_data_hosp) +
      geom_line(aes(x = date, y = median, linetype = name),
                color = "#b53aa0") +
      
      geom_ribbon(aes(x = date, ymin = lower_90, ymax = upper_90),
                  fill = "#b53aa0",
                  alpha = 0.3,
                  plot_data_hosp %>% filter(name == "adj")) +
      
      geom_vline(xintercept = forecast_dates$NNDSS - ddays(morbidity_window_width / 2),
                 linetype = 'dashed') +
      
      scale_x_date(date_breaks = "months", labels = scales::label_date_short()) +
      
      ggokabeito::scale_colour_okabe_ito(name = "Age group") +
      ggokabeito::scale_fill_okabe_ito(name = "Age group") +
      scale_linetype(name = "Type") +
      
      geom_blank(aes(y = 0)) +
      
      
      facet_wrap(~age_group, scales = "free_y") +
      
      theme_minimal() +
      ylab(NULL) + xlab("pr_hosp") +
      
      ggtitle(paste0(state_modelled, " -- Probability of hospitalisation, observed and adjusted"))
    
    
    ggsave(paste0(plot_dir, paste0("/", state_modelled, "_pr_hosp.png")),
           width = 14, height = 9, bg = "white")
    
    
    
    plot_data_ICU <- morbidity_trajectories_state %>%
      select(bootstrap, date, age_group, pr_ICU) %>%
      
      group_by(date, age_group) %>%
      
      summarise(median = median(pr_ICU),
                lower_90 = quantile(pr_ICU, 0.05),
                upper_90 = quantile(pr_ICU, 0.95))
    
    
    ggplot(plot_data_ICU) +
      geom_line(aes(x = date, y = median),
                color = "#008200") +
      
      geom_ribbon(aes(x = date, ymin = lower_90, ymax = upper_90),
                  fill = "#008200",
                  alpha = 0.3) +
      
      geom_vline(xintercept = forecast_dates$NNDSS - ddays(7),
                 linetype = 'dashed') +
      
      
      scale_x_date(date_breaks = "months", labels = scales::label_date_short()) +
      
      scale_linetype(name = "Type") +
      
      geom_blank(aes(y = 0)) +
      
      
      facet_wrap(~age_group, scales = "free_y") +
      
      theme_minimal() +
      ylab(NULL) + xlab("pr_ICU") +
      
      ggtitle(paste0(state_modelled, " -- Probability of ICU admission, observed"))
    
    
    ggsave(paste0(plot_dir, paste0("/", state_modelled, "_pr_ICU.png")),
           width = 14, height = 9, bg = "white")
  } else{
    
    plot_data_age <- morbidity_trajectories_state %>%
      filter(date >= ymd("2022-02-01")) %>%
      select(bootstrap, date, age_group, pr_age_given_case) %>%
      
      group_by(date, age_group) %>%
      
      summarise(median = median(pr_age_given_case),
                lower_90 = quantile(pr_age_given_case, 0.05),
                upper_90 = quantile(pr_age_given_case, 0.95))
    
    
    ggplot(plot_data_age) +
      geom_line(aes(x = date, y = median, color = age_group)) +
      
      geom_line(aes(x = date, y = median, group = age_group_old, color = age_group_old),
                alpha = 0.3,
                plot_data_age %>% rename(age_group_old = age_group)) +
      
      geom_ribbon(aes(x = date, ymin = lower_90, ymax = upper_90, fill = age_group),
                  alpha = 0.5,
                  plot_data_age) +
      
      scale_x_date(date_breaks = "months", labels = scales::label_date_short()) +
      
      geom_vline(xintercept = forecast_dates$NNDSS - ddays(morbidity_window_width / 2),
                 linetype = 'dashed') +
      
      ggokabeito::scale_colour_okabe_ito(name = "Age group") +
      ggokabeito::scale_fill_okabe_ito(name = "Age group") +
      scale_linetype(name = "Type") +
      
      facet_wrap(~age_group) +
      
      theme_minimal() +
      ylab(NULL) + xlab("pr_age_given_case") +
      
      ggtitle(paste0(state_modelled, " -- Age distribution, observed"))
    
    ggsave(paste0(plot_dir, paste0("/", state_modelled, "_age_distribution.png")),
           width = 14, height = 9, bg = "white")
    
    
    
    plot_data_hosp <- morbidity_trajectories_state %>%
      filter(date >= ymd("2022-02-01")) %>%
      select(bootstrap, date, age_group, pr_hosp) %>%
      
      group_by(date, age_group) %>%
      
      summarise(median = median(pr_hosp),
                lower_90 = quantile(pr_hosp, 0.05),
                upper_90 = quantile(pr_hosp, 0.95))
    
    
    
    
    ggplot(plot_data_hosp) +
      geom_line(aes(x = date, y = median),
                color = "#b53aa0") +
      
      geom_ribbon(aes(x = date, ymin = lower_90, ymax = upper_90),
                  fill = "#b53aa0",
                  alpha = 0.3,
                  plot_data_hosp) +
      
      geom_vline(xintercept = forecast_dates$NNDSS - ddays(morbidity_window_width / 2),
                 linetype = 'dashed') +
      
      scale_x_date(date_breaks = "months", labels = scales::label_date_short()) +
      
      ggokabeito::scale_colour_okabe_ito(name = "Age group") +
      ggokabeito::scale_fill_okabe_ito(name = "Age group") +
      scale_linetype(name = "Type") +
      
      geom_blank(aes(y = 0)) +
      
      
      facet_wrap(~age_group, scales = "free_y") +
      
      theme_minimal() +
      ylab(NULL) + xlab("pr_hosp") +
      
      ggtitle(paste0(state_modelled, " -- Probability of hospitalisation, observed"))
    
    
    ggsave(paste0(plot_dir, paste0("/", state_modelled, "_pr_hosp.png")),
           width = 14, height = 9, bg = "white")
    
    
    
    plot_data_ICU <- morbidity_trajectories_state %>%
      select(bootstrap, date, age_group, pr_ICU) %>%
      
      group_by(date, age_group) %>%
      
      summarise(median = median(pr_ICU),
                lower_90 = quantile(pr_ICU, 0.05),
                upper_90 = quantile(pr_ICU, 0.95))
    
    
    ggplot(plot_data_ICU) +
      geom_line(aes(x = date, y = median),
                color = "#008200") +
      
      geom_ribbon(aes(x = date, ymin = lower_90, ymax = upper_90),
                  fill = "#008200",
                  alpha = 0.3) +
      
      geom_vline(xintercept = forecast_dates$NNDSS - ddays(7),
                 linetype = 'dashed') +
      
      
      scale_x_date(date_breaks = "months", labels = scales::label_date_short()) +
      
      scale_linetype(name = "Type") +
      
      geom_blank(aes(y = 0)) +
      
      
      facet_wrap(~age_group, scales = "free_y") +
      
      theme_minimal() +
      ylab(NULL) + xlab("pr_ICU") +
      
      ggtitle(paste0(state_modelled, " -- Probability of ICU admission, observed"))
    
    
    ggsave(paste0(plot_dir, paste0("/", state_modelled, "_pr_ICU.png")),
           width = 14, height = 9, bg = "white")
  }
  
  
  return("")
}


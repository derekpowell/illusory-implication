library(tidybayes)
make_exp_figure <- function(df_plotting, fit_model){
  
  
  obs_diffs <- df_plotting %>% 
    filter(phase=="test") %>% 
    filter(item_type == "B") %>% 
    mutate(
      exposed = if_else((exposed == TRUE | exposed==1), "Exposed", "Unexposed"),
      B_implied_true = if_else(B_implied_true == "T implied F", "Contradicted", "Entailed") 
    ) %>% 
    group_by(exposed, B_implied_true, response) %>% 
    summarize(
      N = n()
    ) %>%
    group_by(B_implied_true, exposed) %>%
    mutate(P = N/sum(N)) %>%
    ungroup() %>% 
    mutate(exposed = if_else((exposed == TRUE | exposed=="Exposed"), "Exposed", "Unexposed")) %>% 
    
    mutate(response = if_else(grepl("True", response), "True", "False")) %>%
    group_by(B_implied_true, response, exposed) %>%
    summarize(
      P = sum(P),
      SE = sqrt( P*(1-P) / sum(N))
    ) %>% 
    ungroup() %>%
    pivot_wider(names_from = exposed, values_from = c(P, SE)) %>%
    mutate(
      diff = P_Exposed - P_Unexposed,
      diff_se = sqrt(SE_Exposed^2 + SE_Unexposed^2)
    ) %>%
    rename(Implication = B_implied_true)
  
  
  schema_preds  <-  df_plotting %>%
    filter(phase=="test") %>%
    filter(item_type == "B") %>%
    # filter(implication != 0) %>%
    distinct(B_implied_true, familiarity, implication, .keep_all = TRUE) %>%
    select(B_implied_true, familiarity, implication) %>% 
    mutate(item = "new_item", subj_id = 9999) %>% 
    add_epred_draws(fit_model, allow_new_levels = TRUE, re_formula = NA) %>% 
    ungroup() %>% 
    mutate(
      exposed = if_else((familiarity == TRUE | familiarity==1), "Exposed", "Unexposed"),
      B_implied_true = if_else(B_implied_true == "T implied F", "Contradicted", "Entailed") 
    ) %>%
    select(-implication, -familiarity, -.row, -.chain, -.iteration) %>%
    ungroup() %>% 
    rename(response = .category) %>% 
    mutate(exposed = if_else((exposed == TRUE | exposed=="Exposed"), "Exposed", "Unexposed")) %>% 
    spread(exposed, .epred) %>% 
    mutate(diff = Exposed - Unexposed) %>%
    mutate(response = if_else(grepl("True", response), "True", "False")) %>%
    group_by(B_implied_true, response, .draw) %>%
    mutate(diff = sum(diff)) %>%
    group_by(B_implied_true, response) %>% 
    summarize(
      M = mean(diff),
      ul = quantile(diff, .75),
      ll = quantile(diff, .25)
    ) %>% 
    rename(Implication = B_implied_true)
  
  plt_binned <- schema_preds %>%   
    ggplot(
      aes(y = M,  x = response, color = Implication, ymin =ll, ymax = ul)
    ) +
    geom_col(data = obs_diffs, aes(x = response, y = diff, fill = Implication), position="dodge", inherit.aes = FALSE, alpha = .25) +
    # geom_errorbar(data = obs_diffs, aes(x = response, y = diff, ymin = diff - diff_se, ymax = diff + diff_se, color = Implication), position = position_dodge(width = 1), inherit.aes=FALSE, width=0) +
    geom_pointrange(size = .3, position = position_dodge(width = 1)) +
    geom_hline(yintercept = 0) +
    scale_color_manual(values = c('#4AABD3', '#0067B1')) + 
    scale_fill_manual(values = c('#4AABD3', '#0067B1')) + 
    theme_bw(base_size=9) +
    theme(panel.grid=element_blank(), legend.position="bottom") +
    theme(legend.direction = "vertical") +
    labs(y = "Exposed - Unexposed\nResponse Proportions", x = "Response", title = "B")
  
  obs <- df_plotting %>% 
    filter(phase=="test") %>% 
    filter(item_type == "B") %>% 
    mutate(
      exposed = if_else((exposed == TRUE | exposed=="Exposed"), "Exposed", "Unexposed"),
      B_implied_true = if_else(B_implied_true == "T implied F", "Contradicted", "Entailed") 
    ) %>% 
    group_by(exposed, B_implied_true, response) %>% 
    summarize(
      N = n()
    ) %>%
    group_by(B_implied_true, exposed) %>%
    mutate(
      P = N/sum(N),
      se = sqrt((P*(1-P))/N)
    ) %>% 
    mutate(Exposed = exposed)
  
  
  schema_preds2  <-  df_plotting %>%
    filter(phase=="test") %>%
    filter(item_type == "B") %>%
    # filter(implication != 0) %>%
    distinct(B_implied_true, familiarity, implication, .keep_all = TRUE) %>%
    select(B_implied_true, familiarity, implication) %>% 
    # mutate(item = "new_item", subj_id = 9999) %>% 
    add_epred_draws(fit_model, allow_new_levels = TRUE, re_formula = NA) %>% 
    ungroup() %>% 
    mutate(
      exposed = if_else((familiarity == TRUE | familiarity==1), "Exposed", "Unexposed"),
      B_implied_true = if_else(B_implied_true == "T implied F", "Contradicted", "Entailed") 
    ) %>%
    select(-implication, -familiarity, -.row, -.chain, -.iteration) %>%
    ungroup() %>% 
    rename(response = .category) %>% 
    group_by(B_implied_true, response, exposed) %>%
    summarize(
      M = mean(.epred),
      ul = quantile(.epred, .975),
      ll = quantile(.epred, .025)
    ) %>% 
    mutate(Exposed = exposed)
  
  
  plt_raw <- schema_preds2 %>%   
    ggplot(
      aes(y = M,  x = response, color = Exposed, ymin =ll, ymax = ul)
    ) +
    # geom_tile() +
    geom_col(data = obs, aes(x = response, y = P, fill = Exposed), position="dodge", inherit.aes = FALSE, alpha =.5) +
    # geom_errorbar(data = obs, aes(x= response , y = P, ymin = P - se, ymax = P + se, color = Exposed), position = position_dodge(width=1), width = 0) +
    # geom_hline(yintercept = 0) +
    geom_pointrange(size = .3, position = position_dodge(width = 1)) +
    facet_wrap(~B_implied_true, nrow = 2) +
    scale_color_manual(values = c('#A41881', '#320A64')) +  
    scale_fill_manual(values = c('#A41881', '#320A64')) +  
    theme_bw(base_size=9) +
    theme(panel.grid=element_blank(), legend.position="bottom") +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
    theme(legend.direction = "vertical") +
    labs(y = "Proportion of responses", x = "Response", title = "A", fill = "Exposure", color = "Exposure")
  
  
  plt_raw + plt_binned + plot_layout(widths = c(3,1), guides = "collect")
  
}


make_exp_figure_two_models <- function(df_plotting, fit_model1, fit_model2){
  
  
  obs_diffs <- df_plotting %>% 
    filter(phase=="test") %>% 
    filter(item_type == "B") %>% 
    mutate(
      exposed = if_else((exposed == TRUE | exposed==1), "Exposed", "Unexposed"),
      B_implied_true = if_else(B_implied_true == "T implied F", "Contradicted", "Entailed") 
    ) %>% 
    group_by(exposed, B_implied_true, response) %>% 
    summarize(
      N = n()
    ) %>%
    group_by(B_implied_true, exposed) %>%
    mutate(P = N/sum(N)) %>%
    ungroup() %>% 
    mutate(exposed = if_else((exposed == TRUE | exposed=="Exposed"), "Exposed", "Unexposed")) %>% 
    
    mutate(response = if_else(grepl("True", response), "True", "False")) %>%
    group_by(B_implied_true, response, exposed) %>%
    summarize(
      P = sum(P),
      SE = sqrt( P*(1-P) / sum(N))
    ) %>% 
    ungroup() %>%
    pivot_wider(names_from = exposed, values_from = c(P, SE)) %>%
    mutate(
      diff = P_Exposed - P_Unexposed,
      diff_se = sqrt(SE_Exposed^2 + SE_Unexposed^2)
    ) %>%
    rename(Implication = B_implied_true)
  
  
  schema_preds  <-  df_reg %>% filter(exposure_cond=="quiz") %>%
    filter(phase=="test") %>%
    filter(item_type == "B") %>%
    # filter(implication != 0) %>%
    distinct(B_implied_true, familiarity, implication, .keep_all = TRUE) %>%
    select(B_implied_true, familiarity, implication) %>% 
    mutate(item = "new_item", subj_id = 9999) %>% 
    add_epred_draws(fit_imp_quiz_int_bin, allow_new_levels = TRUE, re_formula = NA) %>% 
    ungroup() %>% 
    mutate(
      exposed = if_else((familiarity == TRUE | familiarity==1), "Exposed", "Unexposed"),
      B_implied_true = if_else(B_implied_true == "T implied F", "Contradicted", "Entailed") 
    ) %>%
    select(-implication, -familiarity, -.row, -.chain, -.iteration) %>%
    ungroup() %>% 
    mutate(True = .epred, False = 1-.epred) %>%
    gather(response, .epred, True, False) %>% 
    mutate(exposed = if_else((exposed == TRUE | exposed=="Exposed"), "Exposed", "Unexposed")) %>% 
    spread(exposed, .epred) %>% 
    mutate(diff = Exposed - Unexposed) %>%
    # mutate(response = if_else(grepl("True", response), "True", "False")) %>%
    group_by(B_implied_true, response, .draw) %>%
    mutate(diff = sum(diff)) %>%
    group_by(B_implied_true, response) %>%
    summarize(
      M = mean(diff),
      ul = quantile(diff, .75),
      ll = quantile(diff, .25)
    ) %>% 
    rename(Implication = B_implied_true)
  
  plt_binned <- schema_preds %>%   
    ggplot(
      aes(y = M,  x = response, color = Implication, ymin =ll, ymax = ul)
    ) +
    geom_col(data = obs_diffs, aes(x = response, y = diff, fill = Implication), position="dodge", inherit.aes = FALSE, alpha = .25) +
    # geom_errorbar(data = obs_diffs, aes(x = response, y = diff, ymin = diff - diff_se, ymax = diff + diff_se, color = Implication), position = position_dodge(width = 1), inherit.aes=FALSE, width=0) +
    geom_pointrange(size = .3, position = position_dodge(width = 1)) +
    geom_hline(yintercept = 0) +
    scale_color_manual(values = c('#4AABD3', '#0067B1')) + 
    scale_fill_manual(values = c('#4AABD3', '#0067B1')) + 
    theme_bw(base_size=9) +
    theme(panel.grid=element_blank(), legend.position="bottom") +
    theme(legend.direction = "vertical") +
    labs(y = "Exposed - Unexposed\nResponse Proportions", x = "Response", title = "B")
  
  obs <- df_plotting %>% 
    filter(phase=="test") %>% 
    filter(item_type == "B") %>% 
    mutate(
      exposed = if_else((exposed == TRUE | exposed=="Exposed"), "Exposed", "Unexposed"),
      B_implied_true = if_else(B_implied_true == "T implied F", "Contradicted", "Entailed") 
    ) %>% 
    group_by(exposed, B_implied_true, response) %>% 
    summarize(
      N = n()
    ) %>%
    group_by(B_implied_true, exposed) %>%
    mutate(
      P = N/sum(N),
      se = sqrt((P*(1-P))/N)
    ) %>% 
    mutate(Exposed = exposed)
  
  
  schema_preds2  <-  df_plotting %>%
    filter(phase=="test") %>%
    filter(item_type == "B") %>%
    # filter(implication != 0) %>%
    distinct(B_implied_true, familiarity, implication, .keep_all = TRUE) %>%
    select(B_implied_true, familiarity, implication) %>% 
    # mutate(item = "new_item", subj_id = 9999) %>% 
    add_epred_draws(fit_model1, allow_new_levels = TRUE, re_formula = NA) %>% 
    ungroup() %>% 
    mutate(
      exposed = if_else((familiarity == TRUE | familiarity==1), "Exposed", "Unexposed"),
      B_implied_true = if_else(B_implied_true == "T implied F", "Contradicted", "Entailed") 
    ) %>%
    select(-implication, -familiarity, -.row, -.chain, -.iteration) %>%
    ungroup() %>% 
    rename(response = .category) %>%
    group_by(B_implied_true, response, exposed) %>%
    summarize(
      M = mean(.epred),
      ul = quantile(.epred, .975),
      ll = quantile(.epred, .025)
    ) %>% 
    mutate(Exposed = exposed)
  
  
  plt_raw <- schema_preds2 %>%   
    ggplot(
      aes(y = M,  x = response, color = Exposed, ymin =ll, ymax = ul)
    ) +
    # geom_tile() +
    geom_col(data = obs, aes(x = response, y = P, fill = Exposed), position="dodge", inherit.aes = FALSE, alpha =.5) +
    # geom_errorbar(data = obs, aes(x= response , y = P, ymin = P - se, ymax = P + se, color = Exposed), position = position_dodge(width=1), width = 0) +
    # geom_hline(yintercept = 0) +
    geom_pointrange(size = .3, position = position_dodge(width = 1)) +
    facet_wrap(~B_implied_true, nrow = 2) +
    scale_color_manual(values = c('#A41881', '#320A64')) +  
    scale_fill_manual(values = c('#A41881', '#320A64')) +  
    theme_bw(base_size=9) +
    theme(panel.grid=element_blank(), legend.position="bottom") +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
    theme(legend.direction = "vertical") +
    labs(y = "Proportion of responses", x = "Response", title = "A", fill = "Exposure", color = "Exposure")
  
  
  plt_raw + plt_binned + plot_layout(widths = c(3,1), guides = "collect")
  
}
# 
# schema_preds  <-  df_reg %>% filter(exposure_cond=="quiz") %>%
#   filter(phase=="test") %>%
#   filter(item_type == "B") %>%
#   # filter(implication != 0) %>%
#   distinct(B_implied_true, familiarity, implication, .keep_all = TRUE) %>%
#   select(B_implied_true, familiarity, implication) %>% 
#   mutate(item = "new_item", subj_id = 9999) %>% 
#   add_epred_draws(fit_imp_quiz_int_bin, allow_new_levels = TRUE, re_formula = NA) %>% 
#   ungroup() %>% 
#   mutate(
#     exposed = if_else((familiarity == TRUE | familiarity==1), "Exposed", "Unexposed"),
#     B_implied_true = if_else(B_implied_true == "T implied F", "Contradicted", "Entailed") 
#   ) %>%
#   select(-implication, -familiarity, -.row, -.chain, -.iteration) %>%
#   ungroup() %>% 
#   mutate(True = .epred, False = 1-.epred) %>%
#   gather(response, .epred, True, False) %>% 
#   mutate(exposed = if_else((exposed == TRUE | exposed=="Exposed"), "Exposed", "Unexposed")) %>% 
#   spread(exposed, .epred) %>% 
#   mutate(diff = Exposed - Unexposed) %>%
#   # mutate(response = if_else(grepl("True", response), "True", "False")) %>%
#   group_by(B_implied_true, response, .draw) %>%
#   mutate(diff = sum(diff)) %>%
#   group_by(B_implied_true, response) %>%
#   summarize(
#     M = mean(diff),
#     ul = quantile(diff, .75),
#     ll = quantile(diff, .25)
#   ) %>% 
#   rename(Implication = B_implied_true)

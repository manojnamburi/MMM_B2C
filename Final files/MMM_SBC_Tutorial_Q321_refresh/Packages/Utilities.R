##################################################################
###     Custom Functions: Mixed Models                         ###
##################################################################
extract_random_coefs <- function(model, re = NULL, ci_level = NULL, digits = 3, ...) {
  # https://github.com/m-clark/mixedup/blob/master/R/extract_random_coefs.R
  
  # 1. Extract Random Effects
  random_effects = extract_random_effects(model, re = re)
  
  
  # 2. Extract Fixed Effects
  fixed_effects  = extract_fixed_effects(model) %>%
    dplyr::rename(effect   = term,
                  se_fe    = se,
                  value_fe = value)
  
  
  # 3. Compute Random Coefficients
  random_coef = random_effects %>%
    dplyr::left_join(fixed_effects, by = 'effect') %>%
    dplyr::mutate(value = value + value_fe) %>%
    dplyr::select(group_var, effect, group, value) %>% 
    dplyr::mutate_if(is.numeric, round, digits = digits)
  
  random_coef
}


extract_fixed_effects <- function(model, ci_level = .95, digits = 3, 
                                  ci_args = list(method = 'Wald'), ...) {
  # https://github.com/m-clark/mixedup/blob/master/R/extract_fixed_effects.R
  
  # 1. Extract Fixed Coefficients
  fixed_effects  = as.data.frame(stats::coef(summary(model)))
  fixed_effects  = fixed_effects %>%
    dplyr::mutate(term = rownames(.)) %>%
    dplyr::select(term, dplyr::everything())
  
  colnames(fixed_effects) = c('term', 'value', 'se', 't_value')
  
  
  # 2. Compute Lower/Upper Bounds per a given ci_level
  lower = (1 - ci_level)/2
  upper = 1 - lower

  ci    = do.call(confint,
                  c(list(object = model, parm = 'beta_', level = ci_level, oldNames = FALSE),
                    ci_args))
  colnames(ci) = paste0(c('lower_', 'upper_'), c(lower, upper) * 100)

  
  # 3. Construct Fixed Coefficients in tibble
  fixed_effects = data.frame(fixed_effects, ci)
  fixed_effects = fixed_effects %>%
    dplyr::mutate_if(is.numeric, round, digits = digits) %>%
    dplyr::mutate(term = gsub(term, pattern = '[\\(,\\)]', replacement = '')) %>%
    dplyr::as_tibble()
  
  fixed_effects
}


extract_random_effects <- function(model, re = NULL, ci_level = .95, digits = 3, ...) {
  # https://github.com/m-clark/mixedup/blob/master/R/extract_random_effects.R
  
  # 1. Extract Random Effects
  lmer_re        = lme4::ranef(model, condVar = TRUE)
  all_re_names   = names(lmer_re)
  random_effects = as.data.frame(lmer_re)
  colnames(random_effects) = c('group_var', 'effect', 'group', 'value', 'se')
  
  
  # 2. Compute Lower/Upper Bounds per a given ci_level 
  lower = (1 - ci_level)/2
  upper = 1 - lower
  mult  = stats::qnorm(upper)
  
  random_effects = random_effects %>%
    dplyr::mutate(lower = value - mult * se,
                  upper = value + mult * se)
  
  colnames(random_effects)[colnames(random_effects) %in% c('lower', 'upper')] =
    paste0(c('lower_', 'upper_'), c(lower, upper) * 100)
  
  
  # 3. Construct Random Effects in tibble
  if (!is.null(re)) {
    random_effects = random_effects %>%
      dplyr::filter(group_var == re)
  }
  
  random_effects = random_effects %>%
    dplyr::mutate(effect = gsub(effect, pattern = '[\\(, \\)]', replacement = '')) %>%
    dplyr::mutate_if(is.numeric, round, digits = digits) %>%
    dplyr::as_tibble()
  
  random_effects
}





##################################################################
###     Custom Functions: Pair Plots                           ###
##################################################################
# https://bookdown.org/content/4857/the-haunted-dag-the-causal-terror.html
library(GGally)

plot_density <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) + 
    geom_density(fill = "steelblue", color = "black")
}


plot_scatter <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) + 
    geom_smooth(method = "lm", color = "orange", se = F) +
    geom_point(alpha = .8, size = 1/4, color = "blue")
}





# ##################################################################
# ###     Custom Functions: DAG                                  ###
# ##################################################################
# library(ggdag)
# 
# gg_simple_dag <- function(d) {
#   
#   d %>% 
#     ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
#     geom_dag_point(color = "steelblue", alpha = 1/2, size = 6.5) +
#     geom_dag_text(color = "black") +
#     geom_dag_edges() + 
#     theme_dag()
#   
# }
# 
# 
# gg_fancy_dag <- function(d, x = 1, y = 1, circle = "U") {
#   
#   d %>% 
#     ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
#     geom_dag_point(aes(color = name == circle),
#                    alpha = 1/2, size = 6.5, show.legend = F) +
#     geom_point(x = x, y = y, 
#                size = 6.5, shape = 1, stroke = 1, color = "orange") +
#     geom_dag_text(color = "black") +
#     geom_dag_edges() + 
#     scale_color_manual(values = c("steelblue", "orange")) +
#     theme_dag()
#   
# }
# 
# 
# 
# # check it out
# dag %>% 
#   gg_fancy_dag(x = 2.5, y = 1, circle = "M")
# 
# # try it out!
# dag %>% 
#   gg_simple_dag()
# 
# dag_coords <-
#   tibble(name = c("L", "D", "F", "K"),
#          x    = c(1, 2, 3, 2),
#          y    = c(2, 2, 2, 1))
# 
# dagify(L ~ D,
#        F ~ D,
#        K ~ L + F,
#        coords = dag_coords) %>%
#   
#   ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
#   geom_dag_point(aes(color = name == "D"),
#                  alpha = 1/2, size = 6.5, show.legend = F) +
#   geom_point(x = 2, y = 2, 
#              size = 6.5, shape = 1, stroke = 1, color = "orange") +
#   geom_dag_text(color = "black") +
#   geom_dag_edges() +
#   scale_color_manual(values = c("steelblue", "orange")) +
#   scale_x_continuous(NULL, breaks = NULL, expand = c(.1, .1)) +
#   scale_y_continuous(NULL, breaks = NULL, expand = c(.1, .1))
# 
# dag_coords <-
#   tibble(name = c("E", "U", "W"),
#          x    = c(1, 2, 3),
#          y    = c(1, 2, 1))
# 
# dagify(E ~ U,
#        W ~ E + U,
#        coords = dag_coords) %>%
#   gg_simple_dag()
# 
# 
# 
# d1 <- 
#   dagify(X ~ Z,
#          Y ~ Z,
#          coords = tibble(name = c("X", "Y", "Z"),
#                          x = c(1, 3, 2),
#                          y = c(2, 2, 1)))
# 
# d2 <- 
#   dagify(Z ~ X,
#          Y ~ Z,
#          coords = tibble(name = c("X", "Y", "Z"),
#                          x = c(1, 3, 2),
#                          y = c(2, 1, 1.5)))
# 
# d3 <- 
#   dagify(Z ~ X + Y,
#          coords = tibble(name = c("X", "Y", "Z"),
#                          x = c(1, 3, 2),
#                          y = c(1, 1, 2)))
# 
# d4 <- 
#   dagify(Z ~ X + Y,
#          D ~ Z,
#          coords = tibble(name = c("X", "Y", "Z", "D"),
#                          x = c(1, 3, 2, 2),
#                          y = c(1, 1, 2, 1.05)))
# 
# p1 <- gg_simple_dag(d1) + labs(subtitle = "The Fork")
# p2 <- gg_simple_dag(d2) + labs(subtitle = "The Pipe")
# p3 <- gg_simple_dag(d3) + labs(subtitle = "The Collider")
# p4 <- gg_simple_dag(d4) + labs(subtitle = "The Descendant")
# 
# library(patchwork)
# 
# (p1 | p2 | p3 | p4) &
#   theme(plot.subtitle = element_text(hjust = 0.5)) &
#   plot_annotation(title = "The four elemental confounds") 
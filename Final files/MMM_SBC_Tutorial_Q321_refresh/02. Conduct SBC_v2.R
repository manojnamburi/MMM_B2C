################################################################################
###                                                                          ###
###          R Code to Conduct Search Bias Correction (SBC)                  ###
###                                                                          ###
################################################################################



################################################################################
### Inputs 
### 1. Preprocessed SBC Data
###     "RData/df_sbc_all.RData"
###
###
### Outputs:
###     ""
###
###
### Note: 
### 
###
################################################################################



### 0. Load Required Packages.
library(tidyverse)
library(lme4)
# library(brms)
# library(rstan)
library(GGally)
# library(ggdag)
# library(dagitty)
library(broom)
source("Packages/Utilities.R")
# remotes::install_github('m-clark/mixedup')
options(scipen=100000)



### 1. Load Data for Paid Search Bias Correction
load("RData/df_sbc_all.RData")
glimpse(df_sbc_dma)
names(df_sbc_dma)





### 2. Draw Pair Plots
# 800 x 800
pair_cols_ls <- c('y_TGA','PaidSearch_Brand','Google_Altice','Google_Comp','Google_NonBrand','Google_Total')
pair_plot_OPT = ggpairs(data    = df_sbc_dma %>% dplyr::filter(brand == "OPT"), 
                        columns = pair_cols_ls,
                        upper   = list(continuous = wrap("cor", family = "sans", color = "black")),
                        diag    = list(continuous = plot_density),
                        lower   = list(continuous = plot_scatter))

pair_plot_SDL = ggpairs(data    = df_sbc_dma %>% dplyr::filter(brand == "SDL"), 
                        columns = pair_cols_ls,
                        upper   = list(continuous = wrap("cor", family = "sans", color = "black")),
                        diag    = list(continuous = plot_density),
                        lower   = list(continuous = plot_scatter))

# ggpairs(data    = df_sbc_brand %>% dplyr::filter(Geo == "SDL"), 
#         columns = c(4:5, 7:10),
#         upper   = list(continuous = wrap("cor", family = "sans", color = "black")),
#         diag    = list(continuous = plot_density),
#         lower   = list(continuous = plot_scatter))
# 
# ggpairs(data    = df_sbc_altice, 
#         columns = c(4:5, 7:10),
#         upper   = list(continuous = wrap("cor", family = "sans", color = "black")),
#         diag    = list(continuous = plot_density),
#         lower   = list(continuous = plot_scatter))


## Compute Correlation
gg_theme = theme(plot.title      = element_text(hjust = 0.5, size = 20, face = "bold"),
                 legend.position = "none",
                 axis.text.x     = element_text(angle = 90, hjust = 1))

feature_levels = c("PaidSearch_Brand", 
                   "Google_Altice", "Google_Comp", "Google_NonBrand", "Google_Total",
                   "TV", "DM", "Display", "PaidSearch_UnBrand", "Video", "Radio", 
                   "Social", "Print", "Audio")

target_var     = 'y_TGA'

df_corr = df_sbc_dma %>% 
  group_by(Geo) %>% 
  do(data.frame(Cor = t(cor(.[, feature_levels], .[, target_var], use = "pairwise")))) %>% 
  pivot_longer(cols      = starts_with("Cor"),
               names_to  = "Feature",
               values_to = "Corr") %>% 
  mutate(Feature = stringr::str_remove(Feature, "Cor."),
         Feature = factor(Feature, levels = c(feature_levels)))

# 1000 * 600
df_corr%>% 
  ggplot(aes(x = Feature, y = Corr, fill = Feature)) +
  geom_boxplot() + 
  geom_hline(yintercept = 0, color='red', lwd = 1.5) + 
  gg_theme + xlab("") + ylab("Correlation") +
  geom_point(df_corr %>% filter(Geo == "NEW YORK, NY"),
             mapping = aes(x = Feature, y = Corr),
             shape = 8, colour = "red", size = 1.5)





### 3. Estimate Linear Regression by DMA
# https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html


## Create a model formula
# mf1 = as.formula("y_Sales ~ PaidSearch_Brand")
# mf2 = as.formula("y_Sales ~ PaidSearch_Brand + 
#                  Google_Altice + Google_Comp + Google_NonBrand")
# mf3 = as.formula("y_Sales ~ PaidSearch_Brand + 
#                  Google_Altice + Google_Comp + Google_NonBrand +
#                  Audio + Display + DM + PaidSearch_UnBrand + Print + 
#                  Radio + Social + TV + Video")
# mf4 = as.formula("y_Sales ~ PaidSearch_Brand + Google_Total")

lf1 = as.formula("y_TGA ~ PaidSearch_Brand")
lf2 = as.formula("y_TGA ~ PaidSearch_Brand + Google_Total")
lf3 = as.formula("y_TGA ~ PaidSearch_Brand + 
                 Google_Altice + Google_Comp + Google_NonBrand")
lf4 = as.formula("y_TGA ~ PaidSearch_Brand + 
                 Google_Altice + Google_Comp + Google_NonBrand +
                 Audio + Display + DM + PaidSearch_UnBrand + Print + 
                 Radio + Social + TV + Video")


fit_lm1 = df_sbc_dma %>% 
  tidyr::nest(data = -Geo) %>% 
  dplyr::mutate(fit       = map(data, ~ lm(lf1, data = .x)),
                tidied    = map(fit, tidy)) %>% 
  tidyr::unnest(tidied) %>% 
  dplyr::filter(term == "PaidSearch_Brand") %>% 
  dplyr::select(Geo, estimate:p.value)


fit_lm2 = df_sbc_dma %>% 
  tidyr::nest(data = -Geo) %>% 
  dplyr::mutate(fit       = map(data, ~ lm(lf2, data = .x)),
                tidied    = map(fit, tidy)) %>% 
  tidyr::unnest(tidied) %>% 
  dplyr::filter(term == "PaidSearch_Brand") %>% 
  dplyr::select(Geo, estimate:p.value)


fit_lm3 = df_sbc_dma %>% 
  tidyr::nest(data = -Geo) %>% 
  dplyr::mutate(fit       = map(data, ~ lm(lf3, data = .x)),
                tidied    = map(fit, tidy)) %>% 
  tidyr::unnest(tidied) %>% 
  dplyr::filter(term == "PaidSearch_Brand") %>% 
  dplyr::select(Geo, estimate:p.value)


fit_lm4 = df_sbc_dma %>% 
  tidyr::nest(data = -Geo) %>% 
  dplyr::mutate(fit       = map(data, ~ lm(lf4, data = .x)),
                tidied    = map(fit, tidy)) %>% 
  tidyr::unnest(tidied) %>% 
  dplyr::filter(term == "PaidSearch_Brand") %>% 
  dplyr::select(Geo, estimate:p.value)





### 3. Mixed Model
## Custom Function to Get Selected Fixed Effect
get_coef = function(model_fit, digits=4, variable="PaidSearch_Brand", 
                    Model="M1: Naive", Heterogeneity="Random Intercept") {
  
  df_coef = model_fit %>% 
    extract_fixed_effects(digits = digits) %>% 
    filter(term == variable) %>% 
    mutate(Model = Model, Heterogeneity = Heterogeneity) %>% 
    select(Model, Heterogeneity, Estimate = value, everything(), -term)
    
  df_coef
}

## Model Formula: rc (Random Coefficient) vs ri (Random Intercept)
## M1: Naïve Estimate
rc1     = as.formula("y_TGA ~ PaidSearch_Brand + (1 + PaidSearch_Brand | Geo)")
ri1     = as.formula("y_TGA ~ PaidSearch_Brand + (1 | Geo)")

# Random Coefficient
fit_rc1 = lmer(rc1, data = df_sbc_dma)
fit_rc1 %>% summary()
df_rc1  = get_coef(fit_rc1, Model="M1: Naive", Heterogeneity="Random Coefficient")

# Random Intercept
fit_ri1 = lmer(ri1, data = df_sbc_dma)
fit_ri1 %>% summary()
df_ri1  = get_coef(fit_ri1, Model="M1: Naive", Heterogeneity="Random Intercept")



## M2:Demand-Adjusted Estimate
# # Old
# rc2     = as.formula("y_TGA ~ PaidSearch_Brand + Google_Total +
#                  (1 + PaidSearch_Brand + Google_Total | Geo)")
# ri2     = as.formula("y_TGA ~ PaidSearch_Brand + Google_Total + (1 | Geo)")

# New
rc2     = as.formula("y_TGA ~ PaidSearch_Brand + Google_Altice +
                 (1 + PaidSearch_Brand + Google_Altice | Geo)")
ri2     = as.formula("y_TGA ~ PaidSearch_Brand + Google_Altice + (1 | Geo)")

# Random Coefficient
fit_rc2 = lmer(rc2, data = df_sbc_dma)
fit_rc2 %>% summary()
df_rc2  = get_coef(fit_rc2, Model="M2: Search - Altice", Heterogeneity="Random Coefficient")

# Random Intercept
fit_ri2 = lmer(ri2, data = df_sbc_dma)
fit_ri2 %>% summary()
df_ri2  = get_coef(fit_ri2, Model="M2: Search - Altice", Heterogeneity="Random Intercept")



## M3: Paid Search Bias Correction
rc3     = as.formula("y_TGA ~ PaidSearch_Brand + 
                Google_Altice + Google_Comp + Google_NonBrand + 
                (1 + PaidSearch_Brand + Google_Altice + Google_Comp + Google_NonBrand | Geo)")
ri3     = as.formula("y_TGA ~ PaidSearch_Brand + 
                Google_Altice + Google_Comp + Google_NonBrand + 
                (1 | Geo)")

# Random Coefficient
fit_rc3 = lmer(rc3, data = df_sbc_dma)
fit_rc3 %>% summary()
df_rc3  = get_coef(fit_rc3, Model="M3: Search - All", Heterogeneity="Random Coefficient")

# Random Intercept
fit_ri3 = lmer(ri3, data = df_sbc_dma)
fit_ri3 %>% summary()
df_ri3  = get_coef(fit_ri3, Model="M3: Search - All", Heterogeneity="Random Intercept")



## M4: SBC with Other Media
rc4 = as.formula("y_TGA ~ PaidSearch_Brand + 
                 Google_Altice + Google_Comp + Google_NonBrand +
                 Audio + Display + DM + PaidSearch_UnBrand + Print + 
                 Radio + Social + TV + Video + 
                 (1 + PaidSearch_Brand + 
                 Google_Altice + Google_Comp + Google_NonBrand +
                 Audio + Display + DM + PaidSearch_UnBrand + Print + 
                 Radio + Social + TV + Video | Geo)")

ri4 = as.formula("y_TGA ~ PaidSearch_Brand + 
                 Google_Altice + Google_Comp + Google_NonBrand +
                 Audio + Display + DM + PaidSearch_UnBrand + Print + 
                 Radio + Social + TV + Video + 
                 (1 | Geo)")


# Random Coefficient
fit_rc4 = lmer(rc4, data = df_sbc_dma)
fit_rc4 %>% summary()
df_rc4  = get_coef(fit_rc4, Model="M4: Search + Media", Heterogeneity="Random Coefficient")

# Random Intercept
fit_ri4 = lmer(ri4, data = df_sbc_dma)
fit_ri4 %>% summary()
df_ri4  = get_coef(fit_ri4, Model="M4: Search + Media", Heterogeneity="Random Intercept")



## Combine All Model Results
df_causal = bind_rows(df_rc1, df_ri1, df_rc2, df_ri2, df_rc3, df_ri3, df_rc4, df_ri4) %>% 
  arrange(Heterogeneity, Model)

save(df_causal, file = "RData/df_causal_v2.RData")
write.csv(df_causal, file = "RData/df_causal_v2.csv", row.names = FALSE)


### Plot in the deck. Use the given width/height
df_causal %>% 
  ggplot(aes(x = Model, y = Estimate, fill = Heterogeneity)) + 
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = lower_2.5, ymax = upper_97.5),
                width = .2, position = position_dodge(.9)) +
  xlab("") + ylab("Paid Search Estimate")





### 4. Mixed Model: Un-Branded Paid Search
# 800 x 800
pair_cols_ls <- c('y_TGA','PaidSearch_UnBrand','Google_Altice','Google_Comp','Google_NonBrand','Google_Total')
pair_plot_OPT_NB = ggpairs(data    = df_sbc_dma %>% dplyr::filter(brand == "OPT"), 
                           columns = pair_cols_ls,
                           upper   = list(continuous = wrap("cor", family = "sans", color = "black")),
                           diag    = list(continuous = plot_density),
                           lower   = list(continuous = plot_scatter))

pair_plot_SDL_NB = ggpairs(data    = df_sbc_dma %>% dplyr::filter(brand == "SDL"), 
                           columns = pair_cols_ls,
                           upper   = list(continuous = wrap("cor", family = "sans", color = "black")),
                           diag    = list(continuous = plot_density),
                           lower   = list(continuous = plot_scatter))



## Custom Function to Get Selected Fixed Effect
get_coef = function(model_fit, digits=4, variable="PaidSearch_UnBrand", 
                    Model="M1: Naive", Heterogeneity="Random Intercept") {
  
  df_coef = model_fit %>% 
    extract_fixed_effects(digits = digits) %>% 
    filter(term == variable) %>% 
    mutate(Model = Model, Heterogeneity = Heterogeneity) %>% 
    select(Model, Heterogeneity, Estimate = value, everything(), -term)
  
  df_coef
}

## Model Formula: rc (Random Coefficient) vs ri (Random Intercept)
## M1: Naïve Estimate
rc1     = as.formula("y_TGA ~ PaidSearch_UnBrand + (1 + PaidSearch_UnBrand | Geo)")
ri1     = as.formula("y_TGA ~ PaidSearch_UnBrand + (1 | Geo)")

# Random Coefficient
fit_rc1 = lmer(rc1, data = df_sbc_dma)
fit_rc1 %>% summary()
df_rc1  = get_coef(fit_rc1, Model="M1: Naive", Heterogeneity="Random Coefficient")

# Random Intercept
fit_ri1 = lmer(ri1, data = df_sbc_dma)
fit_ri1 %>% summary()
df_ri1  = get_coef(fit_ri1, Model="M1: Naive", Heterogeneity="Random Intercept")



## M2:Demand-Adjusted Estimate
rc2     = as.formula("y_TGA ~ PaidSearch_UnBrand + Google_Comp + Google_NonBrand +
                 (1 + PaidSearch_UnBrand + Google_Comp + Google_NonBrand | Geo)")
ri2     = as.formula("y_TGA ~ PaidSearch_UnBrand + Google_Comp + Google_NonBrand + (1 | Geo)")

# Random Coefficient
fit_rc2 = lmer(rc2, data = df_sbc_dma)
fit_rc2 %>% summary()
df_rc2  = get_coef(fit_rc2, Model="M2: Search - Comp + NonBrand", Heterogeneity="Random Coefficient")

# Random Intercept
fit_ri2 = lmer(ri2, data = df_sbc_dma)
fit_ri2 %>% summary()
df_ri2  = get_coef(fit_ri2, Model="M2: Search - Comp + NonBrand", Heterogeneity="Random Intercept")



## M3: Paid Search Bias Correction
rc3     = as.formula("y_TGA ~ PaidSearch_UnBrand + 
                Google_Altice + Google_Comp + Google_NonBrand + 
                (1 + PaidSearch_UnBrand + Google_Altice + Google_Comp + Google_NonBrand | Geo)")
ri3     = as.formula("y_TGA ~ PaidSearch_UnBrand + 
                Google_Altice + Google_Comp + Google_NonBrand + 
                (1 | Geo)")

# Random Coefficient
fit_rc3 = lmer(rc3, data = df_sbc_dma)
fit_rc3 %>% summary()
df_rc3  = get_coef(fit_rc3, Model="M3: Search - All", Heterogeneity="Random Coefficient")

# Random Intercept
fit_ri3 = lmer(ri3, data = df_sbc_dma)
fit_ri3 %>% summary()
df_ri3  = get_coef(fit_ri3, Model="M3: Search - All", Heterogeneity="Random Intercept")



## Combine All Model Results
df_causal = bind_rows(df_rc1, df_ri1, df_rc2, df_ri2, df_rc3, df_ri3) %>% 
  arrange(Heterogeneity, Model)

save(df_causal, file = "RData/df_causal_NB.RData")
write.csv(df_causal, file = "RData/df_causal_NB.csv", row.names = FALSE)



### Plot in the deck. Use the given width/height
df_causal %>% 
  ggplot(aes(x = Model, y = Estimate, fill = Heterogeneity)) + 
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = lower_2.5, ymax = upper_97.5),
                width = .2, position = position_dodge(.9)) +
  xlab("") + ylab("Non-Branded Paid Search Estimate")



### Why Un-Branded Paid Search Doesn’t Change Much
df_corr_BPS = df_sbc_dma %>% 
  group_by(Geo) %>% 
  do(data.frame(Cor = t(cor(.[, 9:12], .[, 8], use = "pairwise")))) %>% 
  pivot_longer(cols      = starts_with("Cor"),
               names_to  = "Feature",
               values_to = "Corr") %>% 
  mutate(Feature = stringr::str_remove(Feature, "Cor."),
         Feature = factor(Feature, levels = c(feature_levels)),
         Target  = "PaidSearch - Brand")

df_corr_UBPS = df_sbc_dma %>% 
  group_by(Geo) %>% 
  do(data.frame(Cor = t(cor(.[, 9:12], .[, 16], use = "pairwise")))) %>% 
  pivot_longer(cols      = starts_with("Cor"),
               names_to  = "Feature",
               values_to = "Corr") %>% 
  mutate(Feature = stringr::str_remove(Feature, "Cor."),
         Feature = factor(Feature, levels = c(feature_levels)),
         Target  = "PaidSearch - UnBrand")

df_corr_PS = df_corr_BPS %>% bind_rows(df_corr_UBPS)

# 1000 * 600
df_corr_PS %>% 
  ggplot(aes(x = Feature, y = Corr, fill = Target)) +
  geom_boxplot() + 
  geom_hline(yintercept = 0, color='red', lwd = 1.5) + 
  xlab("") + ylab("Correlation")
# gg_theme + xlab("") + ylab("Correlation") +
# geom_point(df_corr_PS %>% filter(Geo == "NEW YORK, NY"),
#            mapping = aes(x = Feature, y = Corr),
#            shape = 8, colour = "red", size = 1.5)

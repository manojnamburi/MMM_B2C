################################################################################
###                                                                          ###
###          R Code to Clean SBC Data                                        ###
###                                                                          ###
################################################################################



################################################################################
### Inputs 
### 1. Input Data consisting of target,media and the google search idx
###     "RawData/mmm_b2c_acq_q121_refresh_raw_data.RData"
###
###
### Outputs:
###     "RData/df_sbc_all.RData"
###
###
### Note: Use `df_sbc_all.RData` for modeling.
### 
###
################################################################################



### 0. Load Required Packages.
library(tidyverse)
# library(caret)
# library(broom)
# library(car)
# library(rms)
# library(Hmisc)
# library(lubridate)





### 1. Load Data for Paid Search Bias Correction
load("RawData/mmm_b2c_acq_q321_refresh_raw_data.RData")
# names(final_obj)

df_raw    = final_obj$Raw_data %>% as_tibble()
names(df_raw) = c("period", "dma", 
                  "Radio", "TV", "Print", "DM", "Audio", "Display", "Video",
                  "PaidSearch_Brand", "PaidSearch_UnBrand", "Social",
                  "Google_Altice", "Google_Comp", "Google_NonBrand", 
                  "y_TGA")
# [1] "period"                      "dma"                        
# [3] "radio_spend"                 "tv_spend"                   
# [5] "print_spend"                 "DM_spend"                   
# [7] "audio_spend"                 "display_spend"              
# [9] "video_spend"                 "paid_search_spend_brand"    
# [11] "paid_search_spend_unbranded" "social_spend"               
# [13] "google_altice_search_idx"    "google_comp_search_idx"     
# [15] "google_non_brand_search_idx" "gross_add_true"             
# [17] "sales_true"                  "sales"

df_nonsub = final_obj$Non_Sub_data %>% as_tibble()
df_comp   = final_obj$Norm_Comp_data %>% as_tibble()

df_sbc    = df_raw %>%
  dplyr::inner_join(df_nonsub, by = c("dma" = "dma")) %>% 
  dplyr::mutate(brand = ifelse(dma == "NEW YORK, NY", "OPT", "SDL")) %>% 
  tidyr::pivot_longer(cols = Radio:y_TGA, names_to = "feature")
# df_missing = df_sbc %>% map_df(~sum(is.na(.)))


media_ls = c("Radio", "TV", "Print", "DM", "Audio", "Display", "Video",
             "PaidSearch_Brand", "PaidSearch_UnBrand", "Social")

df_spending1 = df_sbc %>% 
  filter(feature %in% media_ls) %>% 
  group_by(feature) %>% 
  summarise(Spending = sum(value)) %>% 
  mutate(Spending_pct = 100 * Spending / sum(Spending)) 

df_spending2 = df_sbc %>% 
  filter(feature %in% media_ls, brand == "OPT") %>% 
  group_by(feature) %>% 
  summarise(Spending = sum(value)) %>% 
  mutate(Spending_pct = 100 * Spending / sum(Spending))

df_spending3 = df_sbc %>% 
  filter(feature %in% media_ls, brand == "SDL") %>% 
  group_by(feature) %>% 
  summarise(Spending = sum(value)) %>% 
  mutate(Spending_pct = 100 * Spending / sum(Spending))

df_spending = df_spending1 %>% 
  left_join(df_spending2, by = "feature") %>% 
  left_join(df_spending3, by = "feature") %>% 
  arrange(desc(Spending.x))

names(df_spending) = c("feature", "Spending_All", "Spending_pct_All", 
                       "Spending_OPT", "Spending_pct_OPT", "Spending_SDL", "Spending_pct_SDL")





### 2. Create Data by Geo Level
## Data Standardization
## 1. Divide all values by Non-Sub at a given geo level.
## 2. Use the standardized values for the following modeling.

## Company Level
df_sbc_altice = df_sbc %>% 
  dplyr::group_by(period, feature) %>% 
  dplyr::summarise(non_sub = sum(non_sub),
                   value   = sum(value)) %>% 
  dplyr::mutate(value      = 100 * value / non_sub) %>% 
  tidyr::pivot_wider(names_from = feature, values_from = value) %>% 
  dplyr::mutate(Geo          = "Altice", 
                Google_Total = Google_Altice + Google_Comp + Google_NonBrand) %>% 
  dplyr::select(Geo, period, non_sub, y_TGA,  
                PaidSearch_Brand, Google_Altice, Google_Comp, Google_NonBrand, Google_Total,
                everything())

  
## Brand Level
df_sbc_brand = df_sbc %>% 
  dplyr::group_by(brand, period, feature) %>% 
  dplyr::summarise(non_sub = sum(non_sub),
                   value   = sum(value)) %>% 
  dplyr::mutate(value      = 100 * value / non_sub) %>% 
  tidyr::pivot_wider(names_from = feature, values_from = value) %>% 
  dplyr::mutate(Google_Total = Google_Altice + Google_Comp + Google_NonBrand) %>% 
  dplyr::select(Geo = brand, period, non_sub, y_TGA,  
                PaidSearch_Brand, Google_Altice, Google_Comp, Google_NonBrand, Google_Total,
                everything())


## DMA Level
df_sbc_dma = df_sbc %>% 
  dplyr::group_by(dma, period, feature) %>% 
  dplyr::summarise(non_sub = sum(non_sub),
                   value   = sum(value)) %>% 
  dplyr::mutate(value      = 100 * value / non_sub) %>% 
  tidyr::pivot_wider(names_from = feature, values_from = value) %>% 
  dplyr::mutate(brand        = ifelse(dma == "NEW YORK, NY", "OPT", "SDL"),
                Google_Total = Google_Altice + Google_Comp + Google_NonBrand) %>% 
  dplyr::select(Geo = dma, brand, period, non_sub, y_TGA,  
                PaidSearch_Brand, Google_Altice, Google_Comp, Google_NonBrand, Google_Total,
                everything())


## Save Cleaned SBC Data in RData
save(df_sbc_altice, df_sbc_brand, df_sbc_dma, df_spending, file = "RData/df_sbc_all.RData")

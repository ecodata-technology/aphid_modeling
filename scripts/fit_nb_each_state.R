library(tidyverse)
library(brms)
library(tidybayes)
library(ecodatamisc)

theme_set(theme_ecodata())

d <- read_csv("data/cleaned/r_padi_cdd.csv")

dstates <- d %>% 
  nest_by(state) %>% 
  mutate(n = nrow(data)) %>% 
  ungroup() %>% 
  slice_max(n, n = 5) %>% 
  select(-n)

# dstates %>% 
#   pwalk(function(state, data){
#     model <- brm(aphid_count ~ s(cdd), 
#                  family = negbinomial,
#                  data = data, 
#                  cores = 4, chains = 4, 
#                  iter = 5000, warmup = 1000, 
#                  control = list(max_treedepth = 15, 
#                                 adapt_delta = 0.9))
#     write_rds(model, paste0("fit_models/negbinom_gam_", state, ".rds"))
#   })
# 
# dstates %>% 
#   filter(state == "Minnesota") %>% 
#   pwalk(function(state, data){
#     model <- brm(aphid_count ~ s(cdd), 
#                  family = negbinomial,
#                  data = data, 
#                  cores = 4, chains = 4, 
#                  iter = 5000, warmup = 1000, 
#                  control = list(max_treedepth = 15, 
#                                 adapt_delta = 0.95))
#     write_rds(model, paste0("fit_models/negbinom_gam_", state, ".rds"))
#   })

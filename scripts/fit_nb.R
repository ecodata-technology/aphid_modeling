library(tidyverse)
library(brms)

d <- read_csv("data/cleaned/r_padi_cdd.csv")


m3 <- brm(aphid_count ~ s(cdd), 
          family = negbinomial,
          data = d, 
          cores = 4, chains = 4, 
          iter = 5000, warmup = 1000, 
          control = list(max_treedepth = 15, 
                         adapt_delta = 0.9))

write_rds(m3, "fit_models/negbinom_gam.rds")

library(tidyverse)
library(brms)
library(tidybayes)
library(MCMsBasics)

d <- read_csv(here::here("data/cleaned/r_padi_cdd.csv"))

d <- mutate(d, state = factor(state))

mp <- c(prior(student_t(3, 0, 1.5), class = b),
        #prior(student_t(3, 0, 1.5), class = Intercept),
        #prior(student_t(3, 0, 1.5), class = sd),
        prior(student_t(3, 0, 0.5), class = sd),
        #prior(student_t(3, 0, 1.5), class = sds),
        prior(student_t(3, 0, 0.5), class = sds)
        #prior(gamma(0.5, 0.5), class = shape)
        ) %>% 
  validate_prior(aphid_count ~ s(cdd, by = state) + (1|state), 
                 family = poisson,
                 data = d)

rgamma(1000, 0.005, 0.005) %>%
  vector_hist()

rstudent_t(1000, 2, 0, 0.1) %>% 
  vector_hist()

rcauchy(1000, 0, 0.1) %>% 
  vector_hist()

m4 <- brm(aphid_count ~ s(cdd, by = state) + (1|state), 
          family = poisson,
          data = d, 
          prior = mp,
          cores = 4, chains = 4, 
          iter = 5000, warmup = 1000, 
          control = list(max_treedepth = 15, 
                         adapt_delta = 0.9), 
          sample_prior = "only")

m4_prior_preds <- m4$data %>% 
  add_predicted_draws(m4, n = 100)

m4_prior_preds %>% 
  ggplot(aes(x = .prediction)) +
  geom_histogram()

m4_prior_preds %>% 
  ggplot(aes(x = .prediction)) +
  stat_ecdf(geom = "step", pad = F) +
  scale_x_continuous(trans = "pseudo_log")

conditional_effects(m4)




write_rds(m4, "fit_models/negbinom_gam_state.rds")

library(mgcv)

m5 <- gam(aphid_count ~ s(cdd, by = state), 
          family = nb(),
          data = d)

m5

gratia::draw(m5)

summary(m5)

d %>% 
  filter(!is.na(aphid_count), !is.na(cdd)) %>% 
  mutate(pred = predict(m5)) %>% 
  ggplot(aes(x = cdd)) +
  geom_point(aes(y = aphid_count), alpha = 0.1) +
  geom_line(aes(y = pred), color = "red") +
  facet_wrap(vars(state)) +
  theme_minimal() +
  coord_cartesian(ylim = c(0,100))

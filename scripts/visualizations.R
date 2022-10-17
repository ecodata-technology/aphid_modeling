library(tidyverse)
library(brms)
library(tidybayes)
library(ecodatamisc)
library(lubridate)

theme_set(theme_ecodata())

d <- read_csv("data/cleaned/r_padi_cdd.csv")



# model estimates ---------------------------------------------------------


mn <- read_rds("fit_models/negbinom_gam_Minnesota.rds")
mi <- read_rds("fit_models/negbinom_gam_Michigan.rds")
ind <- read_rds("fit_models/negbinom_gam_Indiana.rds")
il <- read_rds("fit_models/negbinom_gam_Illinois.rds")
wi <- read_rds("fit_models/negbinom_gam_Wisconsin.rds")

m <- lst(Minnesota = mn, Michigan = mi, Indiana = ind, Illinois = il, Wisconsin = wi)

preds <- map_dfr(m, ~tibble(cdd = seq(from = 0, to = max(.x[["data"]][["cdd"]]))) %>%
                   add_fitted_draws(.x, n = 100), .id = "state")

ps <- preds %>%
  group_by(state, cdd) %>%
  median_hdci()

dp <-
  d %>%
  filter(state %in% unique(ps$state))


p1 <- ps %>%
  ggplot(aes(x = cdd)) +
  geom_point(data = dp, aes(y = aphid_count), alpha = 0.02) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), fill = "grey70", alpha = 0.8) +
  geom_line(aes(y = .value)) +
  facet_wrap(vars(state), scales = "free_x") +
  ylim(0,100) +
  theme(panel.border = element_rect(fill = NA, color = "grey80")) +
  labs(y = "Estimated Sampling Abundance",
       x = "Cumulative Degree Days",
       title = "Empirical <span style='font-family:LexendDecaLight;'>R. padi</span> densities and Bayesian GAM estimates",
       subtitle = "Grey band is Bayesian 95% credible interval.<br>Data are pooled across years from 2005-2019.<br>Y axis is truncated at 100 to visualize curves, though empirical values exceed 100.")


ggsave(plot = p1, "images/Rpadi_core_states_GAM.jpg", bg = "white", width = 10, height = 8, device = grDevices::jpeg)

# empirical data only -----------------------------------------------------

aphids <- d %>% 
  group_by(state, site, year) %>% 
  complete(date = seq(min(floor_date(date, unit = "year")), 
                      max(ceiling_date(date, unit = "year") - ddays(1)), by = 'day')) %>% 
  arrange(state, site, year, date) %>% 
  mutate(cumu_count = slider::slide_index_dbl(aphid_count, date, sum, .before = Inf, na.rm = T)) %>% 
  ungroup()

aphids %>% 
  filter(!is.na(state), !is.na(year)) %>% 
  filter(state %in% c("Illinois", "Indiana", "Iowa", 
                      "Michigan", "Minnesota", "Wisconsin")) %>% 
  ggplot(aes(x = date, y = cumu_count, color = state, group = site)) +
  geom_line(alpha = 0.5) +
  facet_grid(rows = vars(state), cols = vars(year), scales = "free_x") +
  scale_y_continuous(trans = scales::pseudo_log_trans(), breaks = c(0,10,100,1000)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b") +
  scale_color_brewer(palette = "Dark2") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "none") +
  labs(y = "Cumulative Adult Aphid Count",
       x = "Date",
       title = "Cumulative Log Abundance of <span style='font-family:LexendDecaLight;'>R. padi</span>",
       subtitle = "Each line represents one site.")

ggsave("images/Rpadi_core_states_cumulative_log_plot.jpg", 
       bg = "white", width = 12, height = 10, device = grDevices::jpeg)

aphids %>% 
  filter(!is.na(state), !is.na(year)) %>% 
  filter(state %in% c("Illinois", "Indiana", "Iowa", 
                      "Michigan", "Minnesota", "Wisconsin")) %>% 
  mutate(noyear = ymd(paste(1960, month(date), day(date)))) %>% 
  ggplot(aes(x = noyear, y = aphid_count, color = year, group = site)) +
  geom_point(alpha = 0.2) +
  facet_wrap(vars(state), scales = "free") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b") +
  scale_color_viridis_c() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = c(0.67, 1.07), legend.direction = "horizontal", 
        panel.border = element_rect(fill = NA, color = "grey80")) +
  labs(y = "Adult Aphid Count",
       x = "Date",
       title = "Abundance of <span style='font-family:LexendDecaLight;'>R. padi</span>",
       subtitle = "Each point represents an observation of one site.")

ggsave("images/Rpadi_core_states_abundance_plot.jpg", 
       bg = "white", width = 12, height = 10, device = grDevices::jpeg)

aphids %>% 
  filter(!is.na(state), !is.na(year)) %>% 
  filter(state %in% c("Illinois", "Indiana", "Iowa", 
                      "Michigan", "Minnesota", "Wisconsin")) %>% 
  mutate(noyear = ymd(paste(1960, month(date), day(date)))) %>% 
  group_by(state, date, noyear, year) %>% 
  summarise(mean_aphid_count = mean(aphid_count)) %>% 
  ggplot(aes(x = noyear, y = mean_aphid_count, color = year)) +
  geom_point(alpha = 0.5) +
  facet_wrap(vars(state), scales = "free") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b") +
  scale_color_viridis_c() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = c(0.67, 1.07), legend.direction = "horizontal", 
        panel.border = element_rect(fill = NA, color = "grey80")) +
  labs(y = "Adult Aphid Count",
       x = "Date",
       title = "Mean abundance of <span style='font-family:LexendDecaLight;'>R. padi</span>",
       subtitle = "Each point represents the mean for a given date across all sites.")

ggsave("images/Rpadi_core_states_mean_abundance_plot.jpg", 
       bg = "white", width = 12, height = 10, device = grDevices::jpeg)

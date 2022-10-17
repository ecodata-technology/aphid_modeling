library(tidyverse)
library(lubridate)

aphids <- read_tsv("data/raw/Rpadi_counts.txt")

theme_set(theme_minimal())

aphids %>% 
  ggplot(aes(x = Date, y = Aphid.count, color = State, group = Site)) +
  geom_point(alpha = 0.2) +
  scale_y_continuous(trans = scales::pseudo_log_trans(), breaks = c(0,10,100,1000)) +
  facet_wrap(vars(Year), scales = "free_x")

aphids %>% 
  filter(Site == "Sutherland",
         Year == 2005) %>% 
  group_by(State, Site, Year) %>% 
  complete(Date = seq(min(floor_date(Date, unit = "year")), 
                      max(ceiling_date(Date, unit = "year") - ddays(1)), by = 'day')) %>% 
  print(n = Inf) %>% 
  ggplot(aes(x = Date, y = Aphid.count)) +
  geom_point()



# to make x axes go from Jan to Dec of a given year, have to use complete()
aphids <- aphids %>% 
  group_by(State, Site, Year) %>% 
  complete(Date = seq(min(floor_date(Date, unit = "year")), 
                      max(ceiling_date(Date, unit = "year") - ddays(1)), by = 'day')) %>% 
  arrange(State, Site, Year, Date) %>% 
  mutate(cumu_count = slider::slide_index_dbl(Aphid.count, Date, sum, .before = Inf, na.rm = T)) %>% 
  ungroup()

aphids %>% 
  filter(!is.na(State), !is.na(Year)) %>% 
  ggplot(aes(x = Date, y = cumu_count, color = State, group = Site)) +
  geom_line(alpha = 0.5) +
  facet_grid(rows = vars(State), cols = vars(Year), scales = "free_x") +
  scale_y_continuous(trans = scales::pseudo_log_trans(), breaks = c(0,10,100,1000)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "none") +
  ylab("Cumulative Adult Aphid Count")

ggsave("images/Rpadi_cumulative_log_plot.jpg", bg = "white", width = 10, height = 12)


# core states -------------------------------------------------------------


aphids %>% 
  filter(!is.na(State), !is.na(Year)) %>% 
  filter(State %in% c("Illinois", "Indiana", "Iowa", 
                      "Michigan", "Minnesota", "Wisconsin")) %>% 
  ggplot(aes(x = Date, y = cumu_count, color = State, group = Site)) +
  geom_line(alpha = 0.5) +
  facet_grid(rows = vars(State), cols = vars(Year), scales = "free_x") +
  scale_y_continuous(trans = scales::pseudo_log_trans(), breaks = c(0,10,100,1000)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "none") +
  ylab("Cumulative Adult Aphid Count")

ggsave("images/Rpadi_core_states_cumulative_log_plot.jpg", 
       bg = "white", width = 12, height = 10)

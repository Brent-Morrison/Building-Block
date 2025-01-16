library(ggplot2)
library(lubridate)

# Custom theme
cust_theme1 <- theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position.inside = c(0.9,0.9),
    legend.background = element_blank(),
    legend.key = element_blank(),
    plot.caption = element_text(size = 8, color = "grey55", face = 'italic'), 
    axis.title.y = element_text(size = 8, color = "darkslategrey"),
    axis.title.x = element_text(size = 8, color = "darkslategrey"),
    axis.text.y = element_text(size = 7, color = "darkslategrey"),
    axis.text.x = element_text(size = 7, color = "darkslategrey"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )

cust_theme2 <- theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.position.inside = c(0.9,0.9),
    legend.background = element_blank(),
    legend.key = element_blank(),
    plot.caption = element_text(size = 8, color = "grey55", face = 'italic'), 
    axis.title.y = element_text(size = 8, color = "darkslategrey"),
    axis.title.x = element_text(size = 8, color = "darkslategrey"),
    axis.text.y = element_text(size = 7, color = "darkslategrey"),
    axis.text.x = element_text(size = 7, color = "darkslategrey"),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )


monthly_indicators %>%
  #filter(mon %in% (1:5*12)) %>% 
  filter(mon %in% seq(3, 60, by = 3)) %>% 
  mutate(date = make_date(
    yr+2022, 
    if_else(mon > 12, mon-((yr-1)*12), mon), 
    1)
  ) %>% 
  select(yr, mon, date, gearing, cash_int_cover) %>% 
  pivot_longer(
    cols = c(gearing, cash_int_cover),
    names_to = 'indicator', 
    values_to = 'value'
   ) %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(vars(indicator), scales = "free_y") +
  labs(x = '',
       y = '',
       title = 'Titles',
       subtitle = 'Subtitle',
       caption = 'Caption') +
  #scale_y_continuous(breaks = seq(1,2,0.5)) +
  scale_x_date(date_breaks = '1 year',
               date_labels = '%Y') + 
  cust_theme2

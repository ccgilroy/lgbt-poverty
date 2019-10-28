# adding uncertainty to figures from Badgett et al 2019, 
# "LGBT Poverty in the United States"
library(tidyverse)
library(ungeviz)

# data for figure 2, page 8
# from supplemental table 1, page 39
fig2_data <- read_csv("fig2.csv")

# rough version of original figure
fig2_data %>%
  mutate(SOGI = fct_reorder(SOGI, `%`)) %>%
  ggplot(mapping = aes(x = SOGI, y = `%`)) + 
  geom_col() +
  theme_minimal()

# pull out confidence intervals
fig2_data_new <- 
  fig2_data %>%
  mutate(ci95 = str_remove_all(`95% CI`, pattern = "[\\(|\\)]"), 
         ci95 = str_split(ci95, pattern = ", "), 
         ci95_low = as.numeric(map_chr(ci95, 1)), 
         ci95_high = as.numeric(map_chr(ci95, 2)))

# hex codes thanks to https://html-color-codes.info/colors-from-image/
report_colors <- c(
  "#386185", # dark blue
  "#3184BF", # medium blue
  "#89C8D4", # light blue / teal
  "#FFCA2B", # light orange
  "#F8981C", # dark orange
  "#F4DB6B"  # yellow
)

# modify data for plot
fig2_data_plot <- 
  fig2_data_new %>%
  mutate(fig2_color = case_when(
    SOGI %in% c("Cis-straight men", "Cis-gay men") ~ "#386185",
    SOGI %in% c("Cis-straight women", "Cis-lesbian women") ~ "#3184BF", 
    SOGI == "Cis-bisexual men" ~ "#89C8D4", 
    SOGI %in% c("Cis-bisexual women", "Transgender") ~ "#FFCA2B"
  )) %>%
  mutate(SOGI2 = SOGI, 
         SOGI = str_c(SOGI, " (N=", scales::comma(N), ")")) %>%
  mutate(SOGI = fct_reorder(SOGI, `%`))

# version 1: bar chart with capped error bars 
fig2_version1 <-
  ggplot(fig2_data_plot, aes(x = SOGI, y = `%`, fill = fig2_color)) +
  geom_col(width = .6, alpha = .8) +
  geom_errorbar(aes(ymin = ci95_low, ymax = ci95_high), width = 0.1, 
                size = .75, 
                color = "grey40") + 
  scale_y_continuous(labels = function(x) scales::percent(x, scale = 1), 
                     limits = c(0, 35)) +
  scale_x_discrete(labels = scales::wrap_format(16)) +
  scale_fill_identity() +
  labs(x = NULL, y = NULL, 
       title = "Poverty rates by sexual orientation and gender identity", 
       subtitle = "means and 95% confidence intervals (source: BRFSS, 2014-2017)",
       caption = "Connor Gilroy, University of Washington\nbased on Figure 2 in Badgett et al. 2019, \"LGBT Poverty in the United States\"") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank())

# version 2: confidence strips
fig2_version2 <-
  ggplot(fig2_data_plot, aes(y = SOGI, x = `%`)) +
  stat_confidence_density(aes(moe = ci95_high - `%`, fill = fig2_color), 
                          height = .8, confidence = .95) +
  scale_x_continuous(labels = function(x) scales::percent(x, scale = 1), 
                     limits = c(0, 35)) +
  scale_y_discrete(labels = scales::wrap_format(16)) +
  scale_fill_identity() +
  coord_flip() +
  labs(x = NULL, y = NULL, 
       title = "Poverty rates by sexual orientation and gender identity", 
       subtitle = "confidence strips based on 95% CI (source: BRFSS, 2014-2017)",
       caption = "Connor Gilroy, University of Washington\nbased on Figure 2 in Badgett et al. 2019, \"LGBT Poverty in the United States\"") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank())

# save figures
ggsave(filename = "fig2_barchart.png", plot = fig2_version1, 
       width = 8, height = 5)
ggsave(filename = "fig2_confidencestrips.png", plot = fig2_version2, 
       width = 8, height = 5)

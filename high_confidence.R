library(dplyr)

df_plot_month <- all_pred %>%
  group_by(month, species_rf) %>%
  summarise(
    n_clicks = n(),      # or n_distinct(UID) if you prefer
    .groups = "drop"
  ) %>%
  arrange(month)

##restricting to high-confidence clicks
df_plot_month <- all_pred %>%
  filter(max_prob >= 0.8) %>%
  group_by(month, species_rf) %>%
  summarise(
    n_clicks = n(),
    .groups = "drop"
  ) %>%
  arrange(month)

#####Plot monthly acoustic presence
library(ggplot2)

ggplot(df_plot_month,
       aes(x = factor(month),
           y = n_clicks,
           fill = species_rf)) +
  geom_col(position = "dodge") +
  labs(
    x = "Month (1–12)",
    y = "Number of classified clicks",
    fill = "Species (RF)",
    title = "Monthly acoustic presence of NBW, NW and SPW"
  ) +
  theme_bw()

##Line plot:
ggplot(df_plot_month,
       aes(x = month,
           y = n_clicks,
           color = species_rf,
           group = species_rf)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:12) +
  labs(
    x = "Month",
    y = "Number of classified clicks",
    color = "Species (RF)",
    title = "Monthly acoustic presence of NBW, NW and SPW"
  ) +
  theme_bw()

####table with how many high‑confidence clicks per species were detected each month
library(dplyr)

# High-confidence core (max_prob >= 0.8)
core_counts_month <- all_pred %>%
  filter(max_prob >= 0.8) %>%            # keep only high-confidence clicks
  group_by(month, species_rf) %>%        # month is numeric 1–12
  summarise(
    n_clicks = n(),                      # or n_distinct(UID) if you prefer
    .groups = "drop"
  ) %>%
  arrange(month, species_rf)

core_counts_month_wide <- core_counts_month %>%
  tidyr::pivot_wider(
    names_from  = species_rf,
    values_from = n_clicks,
    values_fill = 0
  ) %>%
  arrange(month)

core_counts_month_wide

core_counts_month
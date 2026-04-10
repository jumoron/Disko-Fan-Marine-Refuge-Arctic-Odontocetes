library(dplyr)
library(tidyr) 

# Narwhal descriptive stats with min/max
fs <- 192000
nwonly <- nwonly %>%
  mutate(duration_sec = sampleDuration / fs) ##sampleDuration is in samples, so divide by your sampling rate fs to get seconds.

nw_stats <- nwonly %>%
  select(X3db_center_frequency, `X3db_min`, `X3db_max`, 
         peak_frequency, sampleDuration, zeroCrossings, TOL_25kHz, TOL_40kHz, RL_peak, `R_25_40`) %>%
  summarise(across(everything(), 
                   list(
                     Mean = ~ round(mean(., na.rm = TRUE), 1),
                     SD = ~ round(sd(., na.rm = TRUE), 1),
                     Median = ~ round(median(., na.rm = TRUE), 1),
                     P10 = ~ round(quantile(., 0.10, na.rm = TRUE), 1),
                     P90 = ~ round(quantile(., 0.90, na.rm = TRUE), 1),
                     Min = ~ round(min(., na.rm = TRUE), 1),
                     Max = ~ round(max(., na.rm = TRUE), 1),
                     N = ~ sum(!is.na(.))
                   ),
                   .names = "{col}_{fn}")) %>%
  pivot_longer(everything(), names_to = "Parameter_Stat", values_to = "Value") %>%
  separate(Parameter_Stat, into = c("Parameter", "Stat"), sep = "_(?=[A-Z])") %>%
  pivot_wider(names_from = Stat, values_from = Value) %>%
  select(Parameter, Mean, SD, Median, P10, P90, Min, Max, N) %>%
  arrange(Parameter)

print(nw_stats, n = nrow(nw_stats))

# Save as CSV
write.csv(nw_stats, "E:/2020-2021/NAR02-02_SM2M/Feb2021_NAR0202/nw_acoustic_stats.csv", row.names = FALSE)
###one pipeline for violin boxplots
library(dplyr)
library(tidyr)
library(ggplot2)

vars <- c("sampleDuration", "zeroCrossings", "RL_peak",
          "TOL_25kHz", "TOL_40kHz")

plot_data <- tudo %>%
  filter(event_type %in% c("NW", "SPW", "NBW")) %>%        # keep 3 types
  select(event_type, all_of(vars)) %>%
  pivot_longer(cols = all_of(vars),
               names_to = "Parameter",
               values_to = "Value") %>%
  filter(!is.na(Value))

# nice labels if you want
param_labs <- c(
  sampleDuration = "Duration (samples)",
  zeroCrossings  = "Zero crossings",
  RL_peak        = "RL peak (dB re 1 µPa)",
  TOL_25kHz      = "TOL 25 kHz (dB)",
  TOL_40kHz      = "TOL 40 kHz (dB)"
)

p <- ggplot(plot_data, aes(x = event_type, y = Value, fill = event_type)) +
  geom_violin(alpha = 0.7, trim = FALSE) +
  geom_boxplot(width = 0.2, fill = "white", alpha = 0.8, outlier.size = 0.5) +
  facet_wrap(~ Parameter, scales = "free_y",
             labeller = labeller(Parameter = param_labs)) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Event type", y = NULL,
       title = "Acoustic parameters for NW, SPW, and NBW clicks") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )

print(p)
# Optional save
ggsave("NW_SPW_NBW_violin_boxplots.png", p, width = 10, height = 8, dpi = 300)
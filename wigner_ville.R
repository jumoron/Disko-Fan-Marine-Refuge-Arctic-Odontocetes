##wigner-ville plot
library(ggplot2)

# 1) Extract components
tfr  <- wv$tfr   # [freq x time], 128 x 93
time <- wv$t     # seconds, length 93
freq <- wv$f     # Hz, length 128

# 2) Convert to long format with proper units
wvd_df <- expand.grid(
  time_idx = seq_along(time),   # 1..93
  freq_idx = seq_along(freq)    # 1..128
)

wvd_df$time_us  <- time[wvd_df$time_idx] * 1e6
wvd_df$freq_khz <- freq[wvd_df$freq_idx] / 1000
wvd_df$power    <- as.vector(tfr)

# 3) Optional: restrict to a sensible frequency band (e.g. 20–180 kHz)
wvd_df <- wvd_df %>%
  dplyr::filter(freq_khz >= 20, freq_khz <= 180)

# 4) Take log power to enhance contrast and rescale
wvd_df$log_power <- log10(wvd_df$power + 1e-12)

# 5) Plot Wigner–Ville distribution
p_wvd <- ggplot(wvd_df, aes(x = time_us, y = freq_khz, fill = log_power)) +
  geom_raster(interpolate = TRUE) +
  scale_fill_viridis_c(option = "magma") +
  labs(x = "Time (µs)", y = "Frequency (kHz)", title = "Wigner–Ville") +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),
    axis.text  = element_text(size = 10)
  )

print(p_wvd)
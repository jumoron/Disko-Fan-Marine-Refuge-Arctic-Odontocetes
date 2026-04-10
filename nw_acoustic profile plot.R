####NW acoustic profile plot
# -------------------------------------------------
# 1. Rebuild a UID–waveform table from .pgdf
# -------------------------------------------------
library(PamBinaries)
library(dplyr)

click_files <- list.files(
  "E:/2020-2021/NAR02-02_SM2M/Feb2021_NAR0202/binary/20210201",  # adjust path to BINARIES (pgdf)
  pattern = "Click.*\\.pgdf$",
  full.names = TRUE
)

extract_clicks_with_wave <- function(pgdf_file) {
  cat("Loading:", basename(pgdf_file), "\n")
  x <- loadPamguardBinaryFile(pgdf_file, skipLarge = FALSE, convertDate = TRUE)
  if(length(x$data) == 0) return(NULL)
  
  do.call(rbind, lapply(x$data, function(cl) {
    uid  <- cl$UID
    wave <- cl$wave
    if(is.null(uid) || is.null(wave) || length(wave) == 0) return(NULL)
    
    data.frame(
      UID  = uid,
      file = basename(pgdf_file),
      wave = I(list(as.numeric(wave))),  # store as list-column
      stringsAsFactors = FALSE
    )
  }))
}

click_list <- lapply(click_files, extract_clicks_with_wave)
click_waves <- bind_rows(click_list)

# Check
nrow(click_waves)
head(click_waves)

# -------------------------------------------------
# 2. Extract UID 39918000358 from this table
# -------------------------------------------------
target_uid <- 39918000358  #adapt to your UID

cw <- click_waves %>%
  filter(UID == target_uid)

if(nrow(cw) == 0) stop("UID not found with waveform in click_waves")
if(nrow(cw) > 1) warning("More than one waveform row for this UID; using first")

cw <- cw[1, ]
wave <- cw$wave[[1]]
if(is.null(wave) || length(wave) == 0) stop("Waveform is empty for this UID")

n  <- length(wave)
sr <- fs  # or your known sampling rate
if(is.null(sr)) stop("Set fs (sample rate) first")

time_us <- seq(0, n - 1) / sr * 1e6

# -------------------------------------------------
# 3. Plot waveform and Wigner–Ville
# -------------------------------------------------
library(ggplot2)
library(patchwork)
library(PAMmisc)

# Waveform
wave_df <- data.frame(
  time_us   = time_us,
  amplitude = wave
)

p_wave <- ggplot(wave_df, aes(x = time_us, y = amplitude)) +
  geom_line(color = "black", linewidth = 0.5) +
  labs(x = "Time (µs)", y = "Amplitude",
       title = paste("Waveform (UID", target_uid, ")")) +
  theme_classic()

# Wigner–Ville
wave <- cw$wave[[1]]      # numeric vector
sr   <- fs                # e.g. 500000 or 192000; set this explicitly

# Assuming PAMmisc::wignerTransform() output has tfr, time, freq
tfr  <- wv$tfr   # time–frequency matrix [freq x time]
time <- wv$t  # time vector (seconds)
freq <- wv$f  # frequency vector (Hz)

dim(tfr)
length(time)
length(freq)

wvd_df <- expand.grid(
  time_idx = seq_along(time),
  freq_idx = seq_along(freq)
)

wvd_df$time_us  <- time[wvd_df$time_idx] * 1e6
wvd_df$freq_khz <- freq[wvd_df$freq_idx] / 1000
wvd_df$power    <- as.vector(tfr)  # length = n_freq * n_time

tfr  <- wv$tfr          # 128 x 93 matrix [freq x time]
time <- wv$t            # seconds, length 93
freq <- wv$f            # Hz, length 128

# Build data frame
wvd_df <- expand.grid(
  time_idx = seq_along(time),   # 1..93
  freq_idx = seq_along(freq)    # 1..128
)

wvd_df$time_us  <- time[wvd_df$time_idx] * 1e6
wvd_df$freq_khz <- freq[wvd_df$freq_idx] / 1000
wvd_df$power    <- as.vector(tfr)        # length 128*93 = 11904

# Quick sanity checks
nrow(wvd_df)          # should be 11904
length(wvd_df$power)  # 11904
dim(tfr)              # 128 93

library(ggplot2)

p_wvd <- ggplot(wvd_df, aes(x = time_us, y = freq_khz, fill = power)) +
  geom_raster(interpolate = TRUE) +
  scale_fill_viridis_c(option = "magma") +
  labs(x = "Time (µs)", y = "Frequency (kHz)", title = "Wigner–Ville") +
  theme_classic() +
  theme(legend.position = "none")

# 4) Power spectrum panel ----------------------------------------------
spec <- abs(fft(wave))^2
freq <- seq(0, sr, length.out = n)

spec_df <- data.frame(
  freq_khz = freq[1:floor(n/2)] / 1000,
  power_db = 10 * log10(spec[1:floor(n/2)] /
                          max(spec[1:floor(n/2)], na.rm = TRUE))
)

p_spec <- ggplot(spec_df, aes(x = freq_khz, y = power_db)) +
  geom_line(color = "black", linewidth = 0.6) +
  labs(x = "Frequency (kHz)", y = "Relative power (dB)") +
  theme_classic()

# 5) Combine -----------------------------------------------------------
final_plot <- p_wave + p_wvd + p_spec + plot_layout(ncol = 3)
print(final_plot)

ggsave("NW_click_UID_39918000358_profile.png",
       final_plot, width = 12, height = 4, dpi = 300)
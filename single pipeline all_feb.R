####single pipeline for event_type, UID, sampleDuration, min freq, max freq, amplitude, TOL_16kHz, TOL_25kHz, 
##TOL_40kHz, R_16_25, R_25_40, RL_peak, RL_rms, zero crossings, peak frequency, 
#-3 dB and -10 dB central frequency, min/max frequency, and bandwidth

## PACKAGES ----
library(signal)
library(dplyr)
library(PAMpal)

##load("/my/path/to/file.RData") to access data exported from PG
load("E:/2020-2021/NAR02-02_SM2M/Feb2021_NAR0202/PAM_2021_01_31_160225_Tracked_Events.RData")
ls()
detList <- det_20210131_160225_123_super_detection #febnar0202

eventsDf <- do.call(rbind, lapply(detList, function(ev) {
  data.frame(
    event.id   = ev$event_id,
    event_type = ev$event_type,
    comment    = ev$comment,
    stringsAsFactors = FALSE
  )
}))
eventsDf <- unique(eventsDf)  # in case of duplicates
head(eventsDf)
##This eventsDf is your event‑level table. Later you can add a species column when you have labels

##Build clicks table (one row per click)
##Inside each event, subdetections is a list of clicks_... objects, each with fields such as millis, startSample, minFreq, maxFreq, amplitude, wave
get1 <- function(x, nm, default = NA) {
  val <- x[[nm]]
  if (is.null(val) || length(val) == 0) return(default)
  val[[1]]
}

clickDf <- do.call(rbind, lapply(detList, function(ev) {
  evType <- get1(ev, "event_type", NA_character_)
  subs <- ev$subdetections
  if (is.null(subs) || length(subs) == 0) return(NULL)
  
  rows <- lapply(subs, function(cl) {
    data.frame(
      event_type     = evType,
      millis         = get1(cl, "millis"),
      startSample    = get1(cl, "startSample"),
      sampleDuration = get1(cl, "sampleDuration"),
      minFreq        = get1(cl, "minFreq"),
      maxFreq        = get1(cl, "maxFreq"),
      amplitude      = get1(cl, "amplitude"),
      UID            = get1(cl, "UID"),
      type           = get1(cl, "type"),
      comment        = get1(cl, "comment"),
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, rows)
}))

head(clickDf)
##Now clickDf is your click‑level table: many rows, with an event.id column that links back to eventsDf.
write.csv(clickDf, "E:/2020-2021/NAR02-02_SM2M/Feb2021_NAR0202/clickDf.csv", row.names = FALSE)

##Add species labels to events. For BANTER, eventsDf needs a species column. A simple first pass is to map your event_type codes to species labels (or to temporary labels you refine later):
eventsDf$species <- dplyr::case_when(
    eventsDf$event_type == "NW"  ~ "Narwhal",      # example; adjust to your codes
  eventsDf$event_type == "WW"  ~ "Beluga",
  eventsDf$event_type == "UNK"  ~ "Unknown",
  TRUE                         ~ "Unknown"
)
head(eventsDf)

library(signal)   # for hanning()

fs <- 192000   # Hz; change to your true sampling rate #fs <- 232302 marcoux

# Define 1/3-octave bands (example 10–160 kHz; adjust as needed)
cfs <- c(16e3, 25e3, 40e3)
k   <- 2^(1/6)
bands <- data.frame(
  fc = cfs,
  fl = cfs / k,
  fh = cfs * k
)

getTOL_one_click <- function(wave, fs, bands, p0 = 1e-6) {
  x  <- as.numeric(wave[, 1])     # one-channel waveform
  x  <- x - mean(x)
  w  <- signal::hanning(length(x))
  xw <- x * w
  
  N <- length(xw)
  X <- fft(xw)
  kf <- 0:floor(N/2)
  f  <- kf * fs / N
  P  <- (Mod(X[kf + 1])^2) / (sum(w^2) * fs)
  
  sapply(seq_len(nrow(bands)), function(i) {
    idx <- which(f >= bands$fl[i] & f < bands$fh[i])
    if (length(idx) == 0) return(NA_real_)
    df     <- mean(diff(f[idx]))
    P_band <- sum(P[idx]) * df
    10 * log10(P_band / (p0^2))
  })
}
##This implements standard third‑octave band levels from an FFT.

tolList <- lapply(detList, function(ev) {
  evType <- ev$event_type          # e.g. "nw", "spw", "bw"
  subs   <- ev$subdetections
  if (is.null(subs) || length(subs) == 0) return(NULL)
  
  do.call(rbind, lapply(subs, function(cl) {
    tol <- getTOL_one_click(cl$wave, fs = fs, bands = bands)
    df  <- data.frame(
      event_type = evType,        # <- replace event.id with event_type
      UID        = cl$UID,
      t(tol),
      stringsAsFactors = FALSE
    )
    df
  }))
})

tolDf <- do.call(rbind, tolList)
colnames(tolDf)[-(1:2)] <- paste0("TOL_", round(bands$fc/1000), "kHz")

head(tolDf)  #Now tolDf has one row per click with TOL columns

names(clickDf)
names(tolDf)

# Merge TOL columns into clickDf
clickDf_TOL <- clickDf %>%
  left_join(
    tolDf,
    by = c("event_type", "UID")
  )

# Inspect result
colnames(clickDf_TOL)
head(clickDf_TOL)

##Merge TOLs into clickDf. Assuming clickDf from earlier has event.id and UID:
clickDf_TOL <- clickDf %>%
  left_join(tolDf, by = c("event_type", "UID"))

##You can also add simple band‑ratio feature. Band ratios are effective for echolocating species classification
clickDf_TOL$R_16_25 <- clickDf_TOL$TOL_16kHz - clickDf_TOL$TOL_25kHz
clickDf_TOL$R_25_40 <- clickDf_TOL$TOL_25kHz - clickDf_TOL$TOL_40kHz

head(clickDf_TOL)

## 1) Sort by event_type + time (ICI within each event_type)
clickDf_TOL_ICI <- clickDf_TOL %>%
  arrange(event_type, millis) %>%
  group_by(event_type) %>%
  mutate(
    ICI = c(NA, diff(millis / 1000))  # ICI in seconds
  ) %>%
  ungroup()

# 2) Check
head(clickDf_TOL_ICI[, c("event_type", "millis", "ICI")])
summary(clickDf_TOL_ICI$ICI)
# ICI já está em clickDf_full 
###RL [dB re 1μPa @ 1m] = 20 * log10(amplitude_RMS) + calib_offset

# 1. RMS aproximado da amplitude (PAMGuard já dá peak, mas RMS é ~peak/3 para cliques)
clickDf_TOL_ICI$RL_peak <- 20 * log10(clickDf_TOL_ICI$amplitude + 1e-10)  # evita log(0)
# 2. RL_rms mais preciso (fórmula padrão cetáceos)
clickDf_TOL_ICI$RL_rms <- 20 * log10(clickDf_TOL_ICI$amplitude / 3 + 1e-10) + 
  10 * log10(clickDf_TOL_ICI$sampleDuration / 1000)  # duração em ms → s
# 3. RL usando TOL máxima (banda mais energética)
clickDf_TOL_ICI$TOL_max <- pmax(clickDf_TOL_ICI[, paste0("TOL_", c(16,25,40),"kHz")], na.rm = TRUE)
clickDf_TOL_ICI$RL_TOLmax <- clickDf_TOL_ICI$TOL_max

write.csv(clickDf_TOL_ICI, "E:/2020-2021/NAR02-02_SM2M/Feb2021_NAR0202/clickDf_TOL_ICI.csv", row.names = FALSE)

library(dplyr)
library(PAMpal)  # For standardClickCalcs

metrics_rows <- lapply(detList, function(ev) {
  subs <- ev$subdetections
  if (is.null(subs) || length(subs) == 0) return(NULL)
  
  rows <- lapply(subs, function(cl) {
    UID <- get1(cl, "UID")
    wave <- cl$wave
    if (is.null(wave) || length(wave) == 0) return(NULL)
    
    calcs <- standardClickCalcs(list(wave = wave, sr = fs))
    
    data.frame(
      UID = UID,
      peak_frequency = calcs$peak,
      peak2 = calcs$peak2,
      peak3 = calcs$peak3,
      trough = calcs$trough,
      trough2 = calcs$trough2,
      `3db_min` = calcs$fmin_3dB,
      `3db_max` = calcs$fmax_3dB,
      `3db_center_frequency` = calcs$centerkHz_3dB * 1000,  # Convert kHz to Hz
      `3db_bandwidth` = calcs$BW_3dB * 1000,
      `10db_min` = calcs$fmin_10dB,
      `10db_max` = calcs$fmax_10dB,
      `10db_center_frequency` = calcs$centerkHz_10dB * 1000,
      `10db_bandwidth` = calcs$BW_10dB * 1000,
      zeroCrossings = sum(diff(sign(wave)) != 0) / 2,  # Zero crossings
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, rows[!sapply(rows, is.null)])
})

metrics_table <- do.call(rbind, metrics_rows) %>%
  distinct(UID, .keep_all = TRUE)

# Final join
clickDf_final <- clickDf_TOL_ICI %>%
  left_join(metrics_table, by = "UID")

# Check
names(clickDf_final)
summary(clickDf_final[, grep("peak|3db|10db|zero", names(clickDf_final), value = TRUE)])
write.csv(clickDf_final, "E:/2020-2021/NAR02-02_SM2M/Feb2021_NAR0202/clickDf_final.csv", row.names = FALSE)
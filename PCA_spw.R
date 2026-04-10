# Numeric columns for PCA (adjust to your names) sampleDuration	amplitude	UID	TOL_16kHz	TOL_25kHz	TOL_40kHz	
#R_16_25	R_25_40	ICI	RL_peak	RL_rms	peak_frequency	peak2	peak3	trough	trough2	X3db_min	X3db_max	
#X3db_center_frequency	X3db_bandwidth	X10db_min	X10db_max	X10db_center_frequency	X10db_bandwidth	zeroCrossings
pca_vars <- c("peak_frequency", "X3db_bandwidth", "X3db_center_frequency", 
              "X10db_bandwidth", "X10db_center_frequency", "zeroCrossings",
              "peak2", "peak3", "trough", "sampleDuration",	"amplitude", "TOL_16kHz","TOL_25kHz","TOL_40kHz",	
              "R_16_25","R_25_40", "ICI", "RL_peak","RL_rms","X3db_min",	"X3db_max", "X10db_min","X10db_max")  # Add more like duration if wanted

library(FactoMineR)
library(factoextra)
library(dplyr)

spw <- read.csv("E:/2020-2021/NAR02-02_SM2M/Sep2021_NAR0202/clickDf_final_spw.csv")

spw_pca <- spw %>%
  select(all_of(pca_vars)) %>%
  filter(complete.cases(.))

# Impute if needed
if(anyNA(spw_pca)) {
  imp <- imputePCA(spw_pca, ncp = 2)
  spw_complete <- imp$completeObs
} else {
  spw_complete <- spw_pca
}

# Run PCA
res_pca_spw <- PCA(spw_complete, scale.unit = TRUE, graph = FALSE)

# Variables (contribution)
fviz_pca_var(res_pca_spw, col.var = "contrib", 
             gradient.cols = c("blue", "red"),
             title = "SPW PCA - Variable Contributions")

# Scree
fviz_screeplot(res_pca_spw, addlabels = TRUE, title = "SPW Scree Plot")

#top contributors table
contrib_spw <- data.frame(
  Variable = rownames(res_pca_spw$var$contrib),
  PC1_Contrib = round(res_pca_spw$var$contrib[,1], 1),
  PC2_Contrib = round(res_pca_spw$var$contrib[,2], 1),
  Cos2_PC1 = round(res_pca_spw$var$cos2[,1], 2)
) %>%
  arrange(desc(PC1_Contrib))

print(contrib_spw[1:10, ])
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

nwonly <- read.csv("E:/2020-2021/NAR02-02_SM2M/csv/clickDf_final_nwonly.csv")

nw_pca <- nwonly %>%
  select(all_of(pca_vars)) %>%
  filter(complete.cases(.))

# Impute if needed
if(anyNA(nw_pca)) {
  imp <- imputePCA(nw_pca, ncp = 2)
  nw_complete <- imp$completeObs
} else {
  nw_complete <- nw_pca
}

# Run PCA
res_pca_nw <- PCA(nw_complete, scale.unit = TRUE, graph = FALSE)

# Individuals
fviz_pca_ind(res_pca_nw, repel = TRUE, title = "NW PCA - Individuals")

# Variables (contribution)
fviz_pca_var(res_pca_nw, col.var = "contrib", 
             gradient.cols = c("blue", "red"),
             title = "NW PCA - Variable Contributions")

# Scree
fviz_screeplot(res_pca_nw, addlabels = TRUE, title = "NW Scree Plot")

#top contributors table
contrib_nw <- data.frame(
  Variable = rownames(res_pca_nw$var$contrib),
  PC1_Contrib = round(res_pca_nw$var$contrib[,1], 1),
  PC2_Contrib = round(res_pca_nw$var$contrib[,2], 1),
  Cos2_PC1 = round(res_pca_nw$var$cos2[,1], 2)
) %>%
  arrange(desc(PC1_Contrib))

print(contrib_nw[1:10, ])
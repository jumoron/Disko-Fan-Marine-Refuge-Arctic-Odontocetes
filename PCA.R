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

pca_df <- tudo %>%
  select("peak_frequency", "X3db_bandwidth", "X3db_center_frequency", 
         "X10db_bandwidth", "X10db_center_frequency", "zeroCrossings",
         "peak2", "peak3", "trough", "sampleDuration",	"amplitude", "TOL_16kHz","TOL_25kHz","TOL_40kHz",	
         "R_16_25","R_25_40", "ICI", "RL_peak","RL_rms","X3db_min",	"X3db_max", "X10db_min","X10db_max", "event_type") %>%
  filter(complete.cases(.)) %>%
  mutate(event_type = factor(event_type))

res.pca <- PCA(
  pca_df,
  scale.unit = TRUE,
  quali.sup = ncol(pca_df),
  graph = FALSE
)

fviz_pca_ind(
  res.pca,
  habillage = pca_df$event_type,
  addEllipses = TRUE,
  ellipse.level = 0.95
) +
  scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a"))

levels(factor(pca_df$event_type))
nlevels(factor(pca_df$event_type))

# Top contributing variables on PC1 and PC2
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

# Variable quality of representation
fviz_pca_var(res.pca, col.var = "cos2")

# Variable contributions
res.pca$var$contrib

# Variable cos2
res.pca$var$cos2

# Sorted top variables for PC1
sort(res.pca$var$contrib[, 1], decreasing = TRUE)

# Sorted top variables for PC2
sort(res.pca$var$contrib[, 2], decreasing = TRUE)

#####Simple correlation screen
num_df <- tudo %>%
  select(where(is.numeric)) %>%
  select(-UID)   # if UID is numeric, remove it

cor_mat <- cor(num_df, use = "pairwise.complete.obs")
round(cor_mat, 2)

###quick list of highly correlated pairs
which(abs(cor_mat) > 0.9 & abs(cor_mat) < 1, arr.ind = TRUE)

###Automatically flag redundant variables
library(caret)

num_df <- tudo %>%
  select(where(is.numeric)) %>%
  select(-UID)

num_df <- num_df %>%
  select(where(~ !all(is.na(.)))) %>%
  select(where(~ sd(., na.rm = TRUE) > 0))

cor_mat <- cor(num_df, use = "pairwise.complete.obs")

high_corr <- findCorrelation(cor_mat, cutoff = 0.9, names = TRUE)
high_corr

###remove high colinear parameters
pca_vars_clean <- num_df %>%
  select(-all_of(high_corr))

#########reduced PCA code
library(FactoMineR)
library(factoextra)
library(dplyr)

# Reduced variables
pca_vars <- c("sampleDuration", "peak_frequency", "X3db_bandwidth", "zeroCrossings", 
              "RL_peak", "TOL_16kHz", "TOL_25kHz", "TOL_40kHz")

# Prep data with event_type
pca_df <- tudo %>%
  select(all_of(pca_vars), event_type) %>%
  filter(complete.cases(across(all_of(pca_vars))))

# Impute if needed, then PCA
if(anyNA(pca_df[, pca_vars])) {
  library(missMDA)
  imp <- imputePCA(pca_df[, pca_vars], ncp = 2)
  pca_complete <- imp$completeObs
} else {
  pca_complete <- pca_df[, pca_vars]
}

res.pca <- PCA(pca_complete, scale.unit = TRUE, graph = FALSE)

##key plots
# Individuals colored by event_type
fviz_pca_ind(res.pca,
             habillage = pca_df$event_type,
             addEllipses = TRUE,
             palette = "Dark2",
             legend.title = "Event Type")

# Variable contributions
fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 8)

# Scree plot
fviz_screeplot(res.pca, addlabels = TRUE)
library(dplyr)
library(ggplot2)
library(factoextra)

# 1. Select NW, SPW, NBW and features
vars <- c("sampleDuration", "zeroCrossings", "RL_peak",
          "TOL_25kHz", "TOL_40kHz")

km_df <- tudo %>%
  filter(event_type %in% c("NW", "SPW", "NBW")) %>%
  select(event_type, all_of(vars)) %>%
  filter(complete.cases(.))

# 2. Scale numeric variables
X <- scale(km_df[, vars])

# 3. Choose k (example: 3 clusters)
set.seed(123)
km3 <- kmeans(X, centers = 3, nstart = 50)

# Attach cluster labels
km_df$cluster <- factor(km3$cluster)

# 4. Inspect cluster sizes and composition
table(km_df$cluster)
table(km_df$cluster, km_df$event_type)

# 5. Visualize in PCA space colored by k-means cluster
res.pca <- prcomp(X, scale. = FALSE)

p_clusters <- fviz_cluster(
  list(data = X, cluster = km3$cluster),
  geom = "point",
  ellipse.type = "norm",
  palette = "Dark2",
  ggtheme = theme_minimal()
)

p_clusters

fviz_nbclust(X, kmeans, method = "wss")        # elbow
fviz_nbclust(X, kmeans, method = "silhouette") # silhouette

#-------------------
####CART
#------------------
library(dplyr)
library(rpart)
library(rpart.plot)

cart_vars <- c("sampleDuration", "zeroCrossings", "RL_peak",
               "TOL_25kHz", "TOL_40kHz")

cart_df <- tudo %>%
  filter(event_type %in% c("NW", "SPW", "NBW")) %>%
  select(event_type, all_of(cart_vars)) %>%
  filter(complete.cases(.)) %>%
  mutate(event_type = factor(event_type))

##fit a cart model
set.seed(123)

cart_mod <- rpart(
  event_type ~ .,
  data = cart_df,
  method = "class",
  control = rpart.control(
    minsplit = 50,  # adjust based on N
    cp       = 0.001
  )
)

printcp(cart_mod)      # complexity parameter table
cart_mod_pruned <- prune(cart_mod, cp = cart_mod$cptable[which.min(cart_mod$cptable[,"xerror"]), "CP"])

#plot and interpret tree
rpart.plot(cart_mod_pruned,
           type = 2, extra = 104, under = TRUE,
           faclen = 0, varlen = 0,
           main = "CART tree for NW / SPW / NBW clicks")

#--------------
######Refit with a higher minimum node size
#--------------------
library(rpart)
library(rpart.plot)
library(dplyr)

cart_vars <- c("sampleDuration", "zeroCrossings", "RL_peak",
               "TOL_25kHz", "TOL_40kHz")

cart_df <- tudo %>%
  filter(event_type %in% c("NW", "SPW", "NBW")) %>%
  select(event_type, all_of(cart_vars)) %>%
  filter(complete.cases(.)) %>%
  mutate(event_type = factor(event_type))

set.seed(123)
cart_mod <- rpart(
  event_type ~ .,
  data = cart_df,
  method = "class",
  control = rpart.control(
    minsplit = 100,   # larger → shallower tree
    minbucket = 50,
    cp = 0.001        # small cp, we’ll prune via xerror
  )
)

###Prune using the 1‑SE rule
printcp(cart_mod)

cp_tab <- cart_mod$cptable
minrow <- which.min(cp_tab[, "xerror"])
xerr_min <- cp_tab[minrow, "xerror"]
xerr_se  <- cp_tab[minrow, "xstd"]

cp_1se <- cp_tab[cp_tab[, "xerror"] <= xerr_min + xerr_se, ][1, "CP"]

cart_pruned <- prune(cart_mod, cp = cp_1se)

##plot with clearer tables
rpart.plot(
  cart_pruned,
  type = 2,           # split labels under nodes
  extra = 104,        # class + prob + n
  under = TRUE,
  faclen = 0,
  varlen = 0,
  fallen.leaves = TRUE,
  main = "Pruned CART tree for NW / SPW / NBW clicks"
)

#check performance and confusion matrix
pred <- predict(cart_pruned, cart_df, type = "class")
table(Predicted = pred, True = cart_df$event_type)
mean(pred == cart_df$event_type)

library(clue)
library(mclust)
library(tidymodels)
library(tidyverse)
library(tidyclust)
library(factoextra)
library(FactoMineR)
library(cluster)
library(mlr)
library(GGally)
library(ClusterR)
library(vegan)
library(NbClust)
library(gridExtra)
library(grid)
library(lattice)
require(igraph)
library(recipes)
library(rsample)
library(workflows)
library(tune)
library(parameters)
library(pvclust)
library(clustertend)
library(hopkins)
library(plotly)


df<-read.csv("C:/Users/usuario/OneDrive/Desktop/Proyecto 2 Diplomado/Penguins/Club_Penguin/penguins.csv")

###Variable de clasificacion: species



df <- df %>% 
  mutate(species = as.factor(species))


df %>% dim()

df %>% glimpse()

df %>% head()

df %>% count(species)

table(df$species)


df1 <- df %>%
  select(where(is.numeric)) %>%  # solo variables numéricas
  select(-year) %>%              # excluimos el año 
  drop_na()                      # eliminamos los valores nulos 


df1_con_clase <- df %>%
  select(species, where(is.numeric)) %>% 
  select(-year) %>% 
  drop_na()

## 
set.seed(123)

nc<-n_clusters(
  df1,
  standardize = TRUE,
  include_factors = FALSE,
  package = c("easystats", "NbClust", "mclust"),
  fast = TRUE,
  nbclust_method = "kmeans",
  n_max = 10
)

nc

plot(nc)

###

set.seed(123)

elbow<-n_clusters_elbow(
  df1,
  standardize = TRUE,
  include_factors = FALSE,
  clustering_function = stats::kmeans,
  n_max = 10
)

elbow

plot(elbow)

###

set.seed(123)

gap<-n_clusters_gap(
  df1,
  standardize = TRUE,
  include_factors = FALSE,
  clustering_function = stats::kmeans,
  n_max = 10,
  gap_method = "firstSEmax"
)

gap

plot(gap)

###

set.seed(123)

n_clusters_silhouette(
  df1,
  standardize = TRUE,
  include_factors = FALSE,
  clustering_function = stats::kmeans,
  n_max = 10
)

###

set.seed(123)

dbscan<-n_clusters_dbscan(
  df1,
  standardize = TRUE,
  include_factors = FALSE,
  method = c("kNN", "SS"),
  min_size = 0.1,
  eps_n = 50,
  eps_range = c(0.1, 3)
)

dbscan

plot(dbscan)


###K-Means

###Otra forma de explorar (kMeans)

rec_df <- recipe(~.,data = df1_con_clase) %>%
  update_role(species, new_role = "id") %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors()) 

rec_df

kmeans_spec <- k_means(num_clusters = tune())

kmeans_wf <- workflow(rec_df, kmeans_spec)

kmeans_wf <- kmeans_wf %>% 
  update_model(kmeans_spec)

grid <- tibble(num_clusters = 1:10)

set.seed(123)
boots <- bootstraps(df1_con_clase, times = 10)

res <- tune_cluster(
  kmeans_wf,
  resamples = boots,
  grid = grid,
  metrics = cluster_metric_set(sse_within_total, sse_total, sse_ratio)
)

res_metrics <- collect_metrics(res)%>% print(n=Inf)

best <- res %>%
  select_best(metric="sse_ratio")  ###Observese que este criterio es un poco "chafa", ya que esta medida
best                               ### en general va decreciendo con el aumento en el valor de K

res_metrics %>%
  filter(.metric == "sse_ratio") %>%
  ggplot(aes(x = num_clusters, y = mean)) +
  geom_point(col="darkblue",size=2) +
  geom_line(col="red") +
  theme_minimal() +
  ylab("mean WSS/TSS ratio") +
  xlab("Número de clusters") +
  scale_x_continuous(breaks = 1:10)

###Validacion cruzada

df_cv <- vfold_cv(df1_con_clase, v = 10)

clust_num_grid <- grid_regular(num_clusters(),levels = 10)

clust_num_grid

res1 <- tune_cluster(
  kmeans_wf,
  resamples = df_cv,
  grid = clust_num_grid,
  control = control_grid(save_pred = TRUE, extract = identity),
  metrics = cluster_metric_set(sse_within_total, sse_total, sse_ratio)
)


res1_metrics <- res1 %>% collect_metrics()%>% print(n=Inf)


best1 <- res1 %>%
  select_best(metric="sse_ratio")  ###Misma observación que antes
best1


res1_metrics %>%
  filter(.metric == "sse_ratio") %>%
  ggplot(aes(x = num_clusters, y = mean)) +
  geom_point(col="darkblue",size=2) +
  geom_line(col="red") +
  theme_minimal() +
  ylab("mean WSS/TSS ratio cv") +
  xlab("Number of clusters") +
  scale_x_continuous(breaks = 1:10)


###A traves del metodo de silueta (silhouette)

df1 %>% scale()

k.max <- 10 

sil_avg <- rep(0, k.max) 

for (k in 2:k.max) {
kmeans_result <- kmeans(df1, centers = k, nstart = 25)
sil_info <- silhouette(kmeans_result$cluster, dist(df1))
sil_avg[k] <- mean(sil_info[, "sil_width"])
}

plot(2:k.max, sil_avg[2:k.max], type = "b",
xlab = "Number of clusters (k)",
ylab = "Average Silhouette Width",
main = "Silhouette Method for Optimal k")

optimal_k <- which.max(sil_avg[2:k.max]) + 1 
print(paste("Optimal number of clusters (k):", optimal_k))

###Explorando el numero de clustes K subyacentes a estos datos con otros metodos

fviz_nbclust(df1, kmeans, method = "wss")+labs(x ="Número de clusters")+labs(y="Total suma de cuadrados intra clusters")+labs(title = "Número óptimo de clusters")

opt<-Optimal_Clusters_KMeans(df1, max_clusters=10,plot_clusters = TRUE,criterion="WCSSE")

fviz_nbclust(df1, kmeans, method = "silhouette")+labs(x ="Número de clusters")+labs(y="Promedio de silueta")+labs(title = "Número óptimo de clusters")

opt1<-Optimal_Clusters_KMeans(df1, max_clusters=10, plot_clusters = TRUE, criterion="silhouette")

opt2<-Optimal_Clusters_KMeans(df1, max_clusters=10, plot_clusters = TRUE, criterion = "variance_explained",fK_threshold = 0.90)

fviz_nbclust(df1, kmeans, method = "gap_stat")+labs(x ="Número de clusters")+labs(y="GAP")+labs(title = "Número óptimo de clusters")

fit <- cascadeKM(df1, 2, 10, iter = 500)
plot(fit, sortg = TRUE, grpmts.plot = TRUE)

opt_aic<-Optimal_Clusters_KMeans(df1, 10, 'euclidean', plot_clusters=TRUE,criterion="AIC")

nb <- NbClust(df1, distance = "euclidean", min.nc = 2, max.nc = 10, method = "ward.D", index ="all")

names(nb) 

nb$Best.nc

###¿2, 3, o 4, clusters?

k2 <- kmeans(df1, centers = 2, nstart = 25)
k3 <- kmeans(df1, centers = 3, nstart = 25)
k4 <- kmeans(df1, centers = 4, nstart = 25)

k2
k3
k4

###Suma del error cuadrático
###Una métrica sencilla es el error cuadrático de suma dentro de un conglomerado (WSS), 
###que mide la suma de todas las distancias desde las observaciones hasta el centro de su conglomerado. 
###A veces, esto se escala con el error cuadrático de suma total (TSS), la distancia desde todas las observaciones hasta 
###el centroide global; en particular, a menudo se calcula la relación WSS/TSS. 
###En principio, los valores pequeños de WSS o de la relación WSS/TSS sugieren que las observaciones dentro 
###de los conglomerados están más cerca (son más similares) entre sí que con respecto a los otros conglomerados.

k2$tot.withinss/k2$totss; k3$tot.withinss/k3$totss; k4$tot.withinss/k4$totss

p2 <- fviz_cluster(k2, geom = "point", data = df1)+ ggtitle("k = 2")
p3 <- fviz_cluster(k3, geom = "point",  data = df1) + ggtitle("k = 3")
p4 <- fviz_cluster(k4, geom = "point",  data = df1) + ggtitle("k = 4")


grid.arrange(p2, p3, p4, ncol = 3)

###Cuncluimos que hay K=2 grupos de pacientes

fviz_cluster(k2, geom = "point",  data = df1) + ggtitle("Número de grupos de especies: 2")

fviz_cluster(k2, data = df1,
             palette=c("deeppink3", "magenta3"),
             ellipse.type = "euclid",
             star.plot = T,
             repel = T,
             ggtheme = theme())+ ggtitle("Número de grupos de especies: 2")

fviz_cluster(k2, df1, ellipse.type = "norm")

fviz_cluster(k2, df1, palette = "Set2", ggtheme = theme_minimal())

require(tibble)

k2 %>%
  extract_centroids()%>% as_tibble() %>% print(width=Inf)

kmeans_clusters <- 
  bind_cols(df1, cluster=k2$cluster)

kmeans_clusters %>%
  pivot_longer(-cluster) %>% 
  ggplot(aes(x = as.factor(cluster), y = value, fill = as.factor(cluster))) +
  geom_boxplot(show.legend = FALSE) +
  facet_wrap(vars(name), scales = "free") 

kmeans_clusters %>% 
  group_by(cluster) %>% 
  summarise(num_users = n()) %>% 
  mutate(pct_users = num_users / sum(num_users))

TC<-table(df1_con_clase$species,k2$cluster)
TC

sum(diag(TC))/sum(TC)

###Y si hacemos cluster, primero haciendo reduccion de dimension a traves de PCA


kk<-eigen(cor(df1))
sum(kk$values[1:3])/sum(kk$values); sum(kk$values[1:4])/sum(kk$values)

###Clusters herarquicos

df_pca_rec <- recipe(~ ., data = df1_con_clase) %>%
  update_role(species, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = 4)

df_pca_wf <- workflow() %>%
  add_recipe(df_pca_rec)

###Cluster herarquicos trae pca incluido

df_pca_hier <- df_pca_wf %>%
  add_model(hier_clust(linkage_method = "ward.D")) %>%
  fit(data = df1_con_clase) %>%
  extract_fit_engine() %>%
  plot()

df_pca_hier <- df_pca_wf %>%
  add_model(hier_clust(linkage_method = "ward.D2")) %>%
  fit(data = df1_con_clase) %>%
  extract_fit_engine() %>%
  plot()

df_pca_hier <- df_pca_wf %>%
  add_model(hier_clust(linkage_method = "ward.D")) %>%
  fit(data = df1_con_clase) %>%
  extract_fit_engine() %>%
  fviz_dend(k = 2, main = "Dendograma basado en PCA: Liga Ward")%>%
  plot()

###K-Means

kmeans_specific <- k_means(num_clusters = 2) %>%
  set_engine("ClusterR")
  kmeans_specific

kmeans_workf <- workflow(df_pca_rec, kmeans_specific)

kmeans_proc <- fit(kmeans_workf, data = df1_con_clase)
kmeans_proc

kmeans_specific1 <- kmeans_specific %>% 
  set_args(num_clusters = tune())

kmeans_workf1 <- workflow(df_pca_rec, kmeans_specific1)
kmeans_workf1

set.seed(123)
boots <- bootstraps(df1_con_clase, times = 10)

tune_res <- tune_cluster(
  kmeans_workf1,
  resamples = boots
)

collect_metrics(tune_res)

extract_cluster_assignment(kmeans_proc) %>% print(n=Inf)

extract_centroids(kmeans_proc)

predict(kmeans_proc, new_data = slice_sample(as.data.frame(df),n = 10))

kmeans_summary <- kmeans_proc %>%
  extract_fit_summary()

kmeans_summary

tibble(
  orig_labels = kmeans_summary$orig_labels,
  standard_labels = kmeans_summary$cluster_assignments  ###Mismas asignaciones con distintas etiquetas
)

table(kmeans_summary$orig_labels,kmeans_summary$cluster_assignments)

TC_CP<-table(df1_con_clase$species,kmeans_summary$orig_labels)
TC_CP

###Comparacion de clasificacion por K-Means: Todas las variables vs. CP

class_pca<-kmeans_summary$cluster_assignments

head(class_pca)

table(kmeans_clusters$cluster,class_pca)

kmeans_proc %>%
  silhouette_avg(df)

##Graficas finales

pca_cluster <-recipe(~ ., data = df1_con_clase) %>%
  update_role(species, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = 4) %>%
  prep(df1_con_clase) %>%
  bake(df1_con_clase)

pca_cluster

pca_clusters2 <- 
  bind_cols(pca_cluster, cluster=k2$cluster)

cluster_plot <- pca_clusters2 %>% 
  ggplot(mapping = aes(x = PC1, y = PC2)) +
  geom_point(aes(shape = factor(cluster)), size = 2) +
  scale_color_manual(values = c("darkorange","purple","cyan4"))

ggplotly(cluster_plot)

clust_spc_plot <- pca_clusters2 %>% 
  ggplot(mapping = aes(x = PC1, y = PC2)) +
  geom_point(aes(shape = factor(cluster), color = species), size = 2, alpha = 0.8) +
  scale_color_manual(values = c("darkorange","purple","cyan4"))

clust_spc_plot

ggplotly(clust_spc_plot)






install.packages("dbscan")
library(tidymodels)
library(tidyverse)
library(tidyclust)
library(factoextra)
library(cluster)
library(mclust)
library(recipes)
library(rsample)
library(workflows)
library(tune)
library(gridExtra)
library(plotly)
library(dbscan)

# 1) Cargar datos
df<-read.csv("/Users/yendalifaz/Documents/Club_Penguin/penguins.csv")

###Variable de clasificación: species

df <- df %>% mutate(species = as.factor(species))

# Exploración rápida

df %>% dim()
df %>% glimpse()
df %>% head()
df %>% count(species)
table(df$species)

# 2) Selección de variables numéricas y limpieza
df1 <- df %>%
  select(where(is.numeric)) %>%  # solo variables numéricas
  select(-year) %>%              # excluimos el año 
  drop_na()                      # eliminamos los valores nulos 

df1_con_clase <- df %>%
  select(species, where(is.numeric)) %>% 
  select(-year) %>% 
  drop_na()

#Estandarizamos

X_scaled <- scale(X)

# MÉTODO 1: K-MEANS (k = 2)

set.seed(123)
k2 <- kmeans(X_scaled, centers = 2, nstart = 25)

cat("\n--- K-means (k=2) ---\n")
print(k2)
cat("\nTamaños de cluster:\n")
print(table(k2$cluster))

# WSS/TSS
cat("\nWSS/TSS ratio:", k2$tot.withinss / k2$totss, "\n")

# Silueta promedio (sobre datos escalados)
sil_k2 <- silhouette(k2$cluster, dist(X_scaled))
cat("Silhouette promedio (k=2):", round(mean(sil_k2[, "sil_width"]), 4), "\n")

# Visualización con factoextra (usa X_scaled para que sea consistente)
fviz_cluster(k2, data = X_scaled) + ggtitle("K-means (k=2) sobre datos estandarizados")

# Perfil de variables por cluster (interpretación)
kmeans_clusters <- bind_cols(as.data.frame(df1), cluster = factor(k2$cluster))

kmeans_clusters %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean), .groups = "drop") %>%
  print()

kmeans_clusters %>%
  pivot_longer(-cluster) %>%
  ggplot(aes(x = cluster, y = value)) +
  geom_boxplot(show.legend = FALSE) +
  facet_wrap(~name, scales = "free") +
  theme_minimal() +
  labs(title = "K-means (k=2): Distribución de variables por cluster")

# Comparación con species (si existe)
TC <- table(df1_con_clase$species, k2$cluster)
cat("\nTabla species vs cluster (K-means):\n")
print(TC)
cat("\nProporciones por especie:\n")
print(round(prop.table(TC, 1), 3))

# Visualización en PCA
pca2_km <- pca2 %>% mutate(cluster = factor(k2$cluster), species = df1_con_clase$species)

ggplot(pca2_km, aes(PC1, PC2, color = cluster)) +
  geom_point(alpha = 0.85) +
  theme_minimal() +
  labs(title = "K-means (k=2) en PCA", color = "Cluster")

ggplot(pca2_km, aes(PC1, PC2, color = species, shape = cluster)) +
  geom_point(alpha = 0.85) +
  theme_minimal() +
  labs(title = "PCA: species vs cluster (K-means)", color = "Species", shape = "Cluster")

# MÉTODO 2: DBSCAN

minPts <- ncol(X_scaled) + 1
cat("minPts sugerido =", minPts, "\n")

# Elegir eps con kNNdistplot (tu equipo decide el valor viendo el 'codo')
kNNdistplot(X_scaled, k = minPts)
abline(h = 1.2, lty = 2)  # AJUSTA este valor según el gráfico (ejemplo 1.2)

set.seed(123)
db <- dbscan(X_scaled, eps = 1.2, minPts = minPts)

cat("\nDBSCAN resumen:\n")
print(db)
cat("\nTamaños por cluster (0 = ruido):\n")
print(table(db$cluster))

# PCA plot DBSCAN
pca2_db <- pca2 %>% mutate(cluster = factor(db$cluster), species = df1_con_clase$species)

ggplot(pca2_db, aes(PC1, PC2, color = cluster)) +
  geom_point(alpha = 0.85) +
  theme_minimal() +
  labs(title = "DBSCAN en PCA (0 = ruido)", color = "Cluster")

# Perfil DBSCAN (sin ruido)
db_profile <- bind_cols(as.data.frame(df1), cluster = db$cluster) %>%
  filter(cluster != 0) %>%
  mutate(cluster = factor(cluster)) %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean), .groups = "drop")

cat("\nPerfil por cluster (DBSCAN, sin ruido):\n")
print(db_profile)

# Comparación con species
tab_db <- table(df1_con_clase$species, db$cluster)
cat("\nTabla species vs cluster (DBSCAN):\n")
print(tab_db)

# MÉTODO 3: GMM (Mezclas Gaussianas con mclust)


# Selección del número de componentes por BIC
BIC <- mclustBIC(X_scaled, G = 1:10)
plot(BIC, main = "Selección de G por BIC (mclust)")

set.seed(123)
gmm <- Mclust(X_scaled, G = 1:10)

summary(gmm)
cat("\nG elegido por mclust:", gmm$G, "\n")
cat("\nTamaños por cluster (GMM):\n")
print(table(gmm$classification))

# Graficar clasificación e incertidumbre
plot(gmm, what = "classification")
plot(gmm, what = "uncertainty")

# PCA plot GMM
pca2_gmm <- pca2 %>%
  mutate(cluster = factor(gmm$classification),
         uncertainty = gmm$uncertainty,
         species = df1_con_clase$species)

ggplot(pca2_gmm, aes(PC1, PC2, color = cluster)) +
  geom_point(alpha = 0.85) +
  theme_minimal() +
  labs(title = "GMM (mclust) en PCA", color = "Cluster")

ggplot(pca2_gmm, aes(PC1, PC2, color = uncertainty)) +
  geom_point(alpha = 0.85) +
  theme_minimal() +
  labs(title = "GMM: Incertidumbre en PCA", color = "Uncertainty")

# Perfil por cluster (GMM)
gmm_profile <- bind_cols(as.data.frame(df1), cluster = factor(gmm$classification)) %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean), .groups = "drop")

cat("\nPerfil por cluster (GMM):\n")
print(gmm_profile)

# Comparación con species
tab_gmm <- table(df1_con_clase$species, gmm$classification)
cat("\nTabla species vs cluster (GMM):\n")
print(tab_gmm)
cat("\nProporciones por especie (GMM):\n")
print(round(prop.table(tab_gmm, 1), 3))

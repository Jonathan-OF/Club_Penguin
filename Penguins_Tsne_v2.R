# Librerías
library(learnr)
library(tidyverse)
library(tidymodels)
library(embed)
library(corrr)
library(tidytext)
library(gradethis)
library(sortable)
library(rstatix)
library(broom)
library(rgl)
library(plotly)
library(GGally)
library(tidyr)
library(FactoMineR)
library(factoextra)
library(ggord)
library(modeldata)
library(tsne)     # package 'tsne'
library(uwot)
library(scattermore)
library(readr)
library(Rtsne)    # package 'Rtsne'
library(gganimate)
library(data.table)
library(dplyr)

theme_set(theme_bw(16))

# -------------------------
# Helper: función moda robusta
# -------------------------
get_mode <- function(x) {
  x_no_na <- x[!is.na(x)]
  if (length(x_no_na) == 0) return(NA_character_)
  ux <- unique(x_no_na)
  ux[which.max(tabulate(match(x_no_na, ux)))]
}

# -------------------------
# Lectura
# -------------------------
df <- read.csv("C:\\Users\\luis2\\OneDrive\\Documents\\Diplomado\\Modulo_2\\Proyecto1\\penguins.csv",
               stringsAsFactors = FALSE)

# Asegurar existencia de columna species
if (!"species" %in% names(df)) stop("No encuentro la columna 'species' en el CSV")

# -------------------------
# Imputación (numéricos: mediana; categóricos: moda)
# -------------------------
# Convertir factores a character para imputación segura
df <- df %>% mutate(across(where(is.factor), as.character))

# Imputar numéricos por mediana
df_imputado <- df %>%
  mutate(across(
    .cols = where(is.numeric),
    .fns = ~ ifelse(is.na(.), median(., na.rm = TRUE), .)
  ))

# Imputar categóricos (character) por la moda y volver a factor
df_imputado <- df_imputado %>%
  mutate(across(
    .cols = where(is.character),
    .fns = ~ {
      vec <- .
      vec[is.na(vec)] <- get_mode(vec)
      factor(vec)
    }
  ))

# Asignar de vuelta
df <- df_imputado

# Verificación rápida
print(colSums(is.na(df)))
print(dim(df))
glimpse(df)
head(df)
df %>% count(species)

# -------------------------
# Asegurar orden/nivel de referencia para species
# -------------------------

df <- df %>%
  mutate(species = factor(as.character(species),
                          levels = c("Adelie", "Gentoo", "Chinstrap")))

# -------------------------
# Preparar matriz numérica escalada (reusar)
# -------------------------
df_numeric <- df %>% select(where(is.numeric))
scaled_matrix <- scale(df_numeric)   # usarlo en todas las corridas t-SNE/Rtsne

# -------------------------
# t-SNE (paquete 'tsne')
# -------------------------
set.seed(123)
start.time <- proc.time()
df_tsne <- tsne::tsne(scaled_matrix)   # devuelve matriz n x 2 por defecto
print(proc.time() - start.time)

str(df_tsne)
head(df_tsne)

datos <- data.frame(tSNE1 = df_tsne[,1], tSNE2 = df_tsne[,2], especie = df$species)
head(datos)

ggplot(datos, aes(x = tSNE1, y = tSNE2)) +
  geom_point(size = 1.1, color = "darkblue") +
  labs(title = "WDBC: t-sne")

ggplot(datos, aes(x = tSNE1, y = tSNE2, color = especie)) +
  geom_point(size = 1.1) +
  labs(title = "Penguins Species: t-sne")

ggplot(datos, aes(x = especie, y = tSNE1)) +
  geom_boxplot(fill = "darkblue", colour = 3, show.legend = FALSE)

ggplot(datos, aes(x = especie, y = tSNE2)) +
  geom_boxplot(fill = "darkblue", colour = 5, show.legend = FALSE)

tsne_df <- data.frame(tSNE1 = df_tsne[,1], tSNE2 = df_tsne[,2], especie = df$species)
colors <- c("#E6194B", "#3CB44B", "#2e2ed2")

ggplot(tsne_df, aes(x = tSNE1, y = tSNE2, color = factor(especie))) +
  geom_point(size = 1.5) +
  scale_color_manual(values = colors) +
  labs(title = "t-SNE: Penguins Species", x = "t-SNE Dimension 1", y = "t-SNE Dimension 2") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20))

# -------------------------
# Rtsne (paquete Rtsne)
# -------------------------
set.seed(123)
start.time <- proc.time()
df_Rtsne <- Rtsne::Rtsne(scaled_matrix, check_duplicates = FALSE, verbose = TRUE)
print(proc.time() - start.time)

str(df_Rtsne)
head(df_Rtsne)
names(df_Rtsne)

datos1 <- data.frame(Rtsne1 = df_Rtsne$Y[,1], Rtsne2 = df_Rtsne$Y[,2], especie = df$species)
head(datos1)

ggplot(datos1, aes(x = Rtsne1, y = Rtsne2)) +
  geom_point(size = 1.1, color = "darkred") +
  labs(title = "Penguins Species: Rtsne")

ggplot(datos1, aes(x = Rtsne1, y = Rtsne2, color = especie)) +
  geom_point(size = 1.1) +
  labs(title = "Penguins Species: Rtsne")

ggplot(datos1, aes(x = especie, y = Rtsne1)) +
  geom_boxplot(fill = "darkblue", colour = 3, show.legend = FALSE)

ggplot(datos1, aes(x = especie, y = Rtsne2)) +
  geom_boxplot(fill = "darkblue", colour = 5, show.legend = FALSE)

Rtsne_df <- data.frame(Rtsne1 = df_Rtsne$Y[,1], Rtsne2 = df_Rtsne$Y[,2], especie = df$species)
colors <- c("#F58231", "#911EB4", "#2e2ed2")
ggplot(Rtsne_df, aes(x = Rtsne1, y = Rtsne2, color = factor(especie))) +
  geom_point(size = 1.5) +
  scale_color_manual(values = colors) +
  labs(title = "t-SNE: Penguins Species", x = "t-SNE Dimension 1", y = "t-SNE Dimension 2") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20))

# -------------------------
# t-SNE 3D (tsne::tsne con k = 3)
# -------------------------
set.seed(123)
df_tsne3 <- tsne::tsne(scaled_matrix, k = 3)

tsne_df3 <- data.frame(
  tSNE1 = df_tsne3[,1],
  tSNE2 = df_tsne3[,2],
  tSNE3 = df_tsne3[,3],
  especie = factor(df$species)
)

colors <- c("#F58231", "#911EB4", "#2e2ed2")
hover_text <- paste(
  "especie:", tsne_df3$especie, "<br>",
  "Dimension 1:", round(tsne_df3$tSNE1, 3), "<br>",
  "Dimension 2:", round(tsne_df3$tSNE2, 3), "<br>",
  "Dimension 3:", round(tsne_df3$tSNE3, 3)
)

plot_ly(
  data = tsne_df3,
  x = ~tSNE1, y = ~tSNE2, z = ~tSNE3,
  type = "scatter3d", mode = "markers",
  marker = list(size = 6),
  text = hover_text, hoverinfo = "text",
  color = ~especie, colors = colors
) %>%
  layout(title = "t-SNE_3D: Penguins Species",
         scene = list(
           xaxis = list(title = "t-SNE Dimension 1"),
           yaxis = list(title = "t-SNE Dimension 2"),
           zaxis = list(title = "t-SNE Dimension 3")
         ))

# -------------------------
# Rtsne 3D
# -------------------------
set.seed(123)
df_Rtsne3 <- Rtsne::Rtsne(scaled_matrix, dims = 3, check_duplicates = FALSE, verbose = TRUE)

tsne_Rdf3 <- data.frame(
  tSNE1 = df_Rtsne3$Y[,1],
  tSNE2 = df_Rtsne3$Y[,2],
  tSNE3 = df_Rtsne3$Y[,3],
  especie = factor(df$species)
)

hover_text <- paste(
  "especie:", tsne_Rdf3$especie, "<br>",
  "Dimension 1:", round(tsne_Rdf3$tSNE1, 3), "<br>",
  "Dimension 2:", round(tsne_Rdf3$tSNE2, 3), "<br>",
  "Dimension 3:", round(tsne_Rdf3$tSNE3, 3)
)

plot_ly(
  data = tsne_Rdf3,
  x = ~tSNE1, y = ~tSNE2, z = ~tSNE3,
  type = "scatter3d", mode = "markers",
  marker = list(size = 6),
  text = hover_text, hoverinfo = "text",
  color = ~especie, colors = colors
) %>%
  layout(title = "t-SNE_3D: Penguins Species",
         scene = list(
           xaxis = list(title = "t-SNE Dimension 1"),
           yaxis = list(title = "t-SNE Dimension 2"),
           zaxis = list(title = "t-SNE Dimension 3")
         ))

# -------------------------
# Exploración de parámetros tsne (paquete tsne)
# -------------------------
set.seed(123)
tsne_params <- expand.grid(perplexity = c(10, 15, 20, 25, 30, 50))

start.time <- proc.time()
tsne_res <- lapply(seq_len(nrow(tsne_params)), function(i) {
  message("tsne iteration: ", i, " perplexity=", tsne_params$perplexity[i])
  tsne::tsne(X = scaled_matrix, max_iter = 500, perplexity = tsne_params$perplexity[i])
})
print(proc.time() - start.time)

d1 <- rbindlist(lapply(seq_len(nrow(tsne_params)), function(i) {
  data.table(
    x = tsne_res[[i]][,1],
    y = tsne_res[[i]][,2],
    perplexity = tsne_params$perplexity[i],
    group = df$species
  )
}))

p1 <- ggplot(d1) +
  geom_scattermore(aes(x = x, y = y, colour = group), pointsize = 2) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.position = "none") +
  facet_wrap(~perplexity, labeller = label_both, scales = "free")
p1

# -------------------------
# Exploración de parámetros Rtsne
# -------------------------
set.seed(123)
Rtsne_params <- expand.grid(perplexity = c(10, 15, 20, 25, 30, 50))

start.time <- proc.time()
Rtsne_res <- lapply(seq_len(nrow(Rtsne_params)), function(i) {
  message("Rtsne iteration: ", i, " perplexity=", Rtsne_params$perplexity[i])
  Rtsne::Rtsne(X = scaled_matrix, max_iter = 500, verbose = TRUE,
               perplexity = Rtsne_params$perplexity[i], check_duplicates = FALSE)
})
print(proc.time() - start.time)

d2 <- rbindlist(lapply(seq_len(nrow(Rtsne_params)), function(i) {
  data.table(
    x = Rtsne_res[[i]]$Y[,1],
    y = Rtsne_res[[i]]$Y[,2],
    perplexity = Rtsne_params$perplexity[i],
    group = df$species
  )
}))

p2 <- ggplot(d2) +
  geom_scattermore(aes(x = x, y = y, colour = group), pointsize = 2) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.position = "none") +
  facet_wrap(~perplexity, labeller = label_both, scales = "free")
p2

# -------------------------
# Exploración Rtsne con eta y perplexity (grid)
# -------------------------
set.seed(123)
Rtsne_params2 <- expand.grid(perplexity = c(10, 15, 20, 25, 30), eta = c(10, 50, 100, 150))

Rtsne_res2 <- lapply(seq_len(nrow(Rtsne_params2)), function(i) {
  message("Rtsne grid iter: ", i, " p=", Rtsne_params2$perplexity[i], " eta=", Rtsne_params2$eta[i])
  Rtsne::Rtsne(X = scaled_matrix, max_iter = 500, verbose = TRUE,
               perplexity = Rtsne_params2$perplexity[i], check_duplicates = FALSE,
               eta = Rtsne_params2$eta[i], pca = FALSE)
})

d3 <- rbindlist(lapply(seq_len(nrow(Rtsne_params2)), function(i) {
  data.table(
    x = Rtsne_res2[[i]]$Y[,1],
    y = Rtsne_res2[[i]]$Y[,2],
    perplexity = Rtsne_params2$perplexity[i],
    eta = Rtsne_params2$eta[i],
    group = df$species
  )
}))

p3 <- ggplot(d3) +
  geom_scattermore(aes(x = x, y = y, colour = group), pointsize = 2) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.position = "none") +
  facet_grid(eta ~ perplexity, labeller = label_both, scales = "free")
p3

# -------------------------
# Grid más amplio (ejemplo)
# -------------------------
set.seed(123)
Rtsne_params3 <- expand.grid(perplexity = c(10, 15, 20, 30), eta = c(10, 50, 100, 150, 200))

Rtsne_res3 <- lapply(seq_len(nrow(Rtsne_params3)), function(i) {
  message("Rtsne grid iter 2: ", i, " p=", Rtsne_params3$perplexity[i], " eta=", Rtsne_params3$eta[i])
  Rtsne::Rtsne(X = scaled_matrix, max_iter = 500, verbose = TRUE,
               perplexity = Rtsne_params3$perplexity[i], check_duplicates = FALSE,
               eta = Rtsne_params3$eta[i], pca = FALSE)
})

d4 <- rbindlist(lapply(seq_len(nrow(Rtsne_params3)), function(i) {
  data.table(
    x = Rtsne_res3[[i]]$Y[,1],
    y = Rtsne_res3[[i]]$Y[,2],
    perplexity = Rtsne_params3$perplexity[i],
    eta = Rtsne_params3$eta[i],
    group = df$species
  )
}))

p4 <- ggplot(d4) +
  geom_scattermore(aes(x = x, y = y, colour = group), pointsize = 2) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.position = "none") +
  facet_grid(eta ~ perplexity, labeller = label_both, scales = "free")
p4

# -------------------------
# Animaciones (ejemplos)
# -------------------------
colores <- c('#E178C5','#EB5B00','#2e2ed2')
names(colores) <- c("Adelie", "Gentoo", "Chinstrap")

anim_plot1 <- d4 %>%
  filter(perplexity %in% c(10, 15, 20, 30)) %>%
  mutate(parametros = factor(paste('perplexity:', perplexity, 'eta:', eta))) %>%
  ggplot(aes(x = x, y = y, col = group)) +
  geom_scattermore(pointsize = 2) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.position = "none") +
  scale_color_manual(values = colores) +
  labs(title = 'Verificación de los parámetros') +
  transition_states(parametros, transition_length = 2, state_length = 3) +
  labs(title = '{closest_state}')

animate(anim_plot1, nframes = 50)

anim_plot2 <- d4 %>%
  filter(eta %in% c(10, 50, 100, 150, 200)) %>%
  mutate(parametros = factor(paste('perplexity:', perplexity, 'eta:', eta))) %>%
  ggplot(aes(x = x, y = y, col = group)) +
  geom_scattermore(pointsize = 2) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.position = "none") +
  scale_color_manual(values = colores) +
  labs(title = 'Verificación de los parámetros') +
  transition_states(parametros, transition_length = 3, state_length = 1) +
  labs(title = '{closest_state}')

animate(anim_plot2, nframes = 300)

# -------------------------
# Ejemplo de ejecución final con parámetros elegidos
# -------------------------
set.seed(123)
# Nota: si usas perplexity = 50 y eta = 0.5 (ejemplo), Rtsne acepta eta numérico
df_RtsneF <- Rtsne::Rtsne(scaled_matrix, perplexity = 50, eta = 0.5,
                          check_duplicates = FALSE, verbose = TRUE)

datos2 <- data.frame(
  Rtsne1 = df_RtsneF$Y[,1],
  Rtsne2 = df_RtsneF$Y[,2],
  especie = df$species
)

# ----- asegurar niveles + colores correctos -----
colores_vec <- c('#E178C5','#EB5B00','#2e2ed2')
colores_mapped <- setNames(colores_vec, levels(datos2$especie))

if (interactive() && .Platform$OS.type == "windows") win.graph()
ggplot(datos2, aes(x = Rtsne1, y = Rtsne2, color = especie)) +
  geom_point(size = 1.5) +
  scale_color_manual(values = colores_mapped) +
  labs(title = "Penguins Species: Rtsne (Modelo final)",
       subtitle = "perplexity = 50, eta = 0.5")
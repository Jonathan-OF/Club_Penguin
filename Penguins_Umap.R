library(tidyverse)
library(tidymodels)
library(modeldata)
library(embed)
library(tsne)
library(uwot)         # uso explícito de uwot::umap para evitar conflictos
library(scattermore)
library(readr)
library(Rtsne)
library(baguette)
library(discrim)
library(gganimate)
library(data.table)
library(plotly)

tidymodels_prefer()

set.seed(123) # reproducibilidad

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

# -------------------------
# Imputación (numéricos: mediana; categóricos: moda)
# -------------------------
# Convertir factores a character para imputación segura
df <- df %>% mutate(across(where(is.factor), as.character))

# Tratar cadenas vacías como NA en columnas character (opcional pero útil)
df <- df %>% mutate(across(where(is.character), ~na_if(trimws(.), "")))

# Imputar numéricos por mediana (mantengo lógica original)
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
# UMAP (2D) usando uwot::umap 
# -------------------------
numeric_mat <- df %>% dplyr::select(where(is.numeric)) %>% as.matrix()
numeric_scaled <- scale(numeric_mat)

# UMAP 2D
df_umap <- uwot::umap(numeric_scaled, n_components = 2)  # resultado: matrix (n x 2)

# Resultados (comprobación)
str(df_umap)
dim(df_umap)

# Base de datos para ggplot
datos <- data.frame(UMAP1 = df_umap[, 1], UMAP2 = df_umap[, 2], especie = df$species)
head(datos)

ggplot(datos, aes(x = UMAP1, y = UMAP2, color = especie)) +
  geom_point(size = 2) +
  labs(title = "Penguins Species: UMAP") +
  theme_minimal()

# -------------------------
# Otra forma: recipe con step_umap (embed tiene step_umap)
# -------------------------

umap_rec <- recipe(~., data = df) %>%
  update_role(species, new_role = "id") %>%
  # convertir caracteres a factores si quedan (seguro)
  step_mutate(across(where(is.character), ~ factor(.))) %>%
  # convertir variables nominales predictoras a dummies (ahora serán numéricas)
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  # normalizar solo predictores numéricos (incluye dummies)
  step_normalize(all_numeric_predictors()) %>%
  # UMAP sobre los predictores (ahora todos numéricos)
  step_umap(all_predictors(), num_comp = 2)

umap_res <- prep(umap_rec)

# Visualizar juice (si contiene species como id, la columna species se conserva)
juice(umap_res) %>%
  ggplot(aes(UMAP1, UMAP2)) +
  geom_point(aes(color = species), size = 1.5) +
  labs(color = NULL) +
  theme_minimal()

# -------------------------
# UMAP 3D con uwot::umap(n_components = 3)
# -------------------------
df_umap3 <- uwot::umap(numeric_scaled, n_components = 3)

umap_df3 <- data.frame(
  UMAP1 = df_umap3[, 1],
  UMAP2 = df_umap3[, 2],
  UMAP3 = df_umap3[, 3],
  especie = factor(df$species)
)

head(umap_df3)

colores <- c("#E6194B", "#3CB44B", "#2e2ed2")

hover_text <- paste(
  "especie:", umap_df3$especie, "<br>",
  "Dimension 1:", round(umap_df3$UMAP1, 3), "<br>",
  "Dimension 2:", round(umap_df3$UMAP2, 3), "<br>",
  "Dimension 3:", round(umap_df3$UMAP3, 3)
)

plot_ly(
  data = umap_df3,
  x = ~UMAP1,
  y = ~UMAP2,
  z = ~UMAP3,
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 6),
  text = hover_text,
  hoverinfo = "text",
  color = ~especie,
  colors = colores
) %>%
  layout(
    title = "UMAP_3D: Penguins Species",
    scene = list(
      xaxis = list(title = "UMAP Dimension 1"),
      yaxis = list(title = "UMAP Dimension 2"),
      zaxis = list(title = "UMAP Dimension 3")
    )
  )

# -------------------------
# UMAP: Exploración de hiper-parámetros
# -------------------------
# Primer grid
umap_params <- expand.grid(n_neighbors = c(10, 20, 50, 100), min_dist = c(0.5, 0.75, 1.1))

umaps <- lapply(seq_len(nrow(umap_params)), function(i) {
  uwot::umap(
    X = numeric_scaled,
    n_neighbors = umap_params$n_neighbors[i],
    min_dist = umap_params$min_dist[i],
    n_threads = 1
  )
})

d <- rbindlist(lapply(seq_len(nrow(umap_params)), function(i) {
  data.table(
    x = umaps[[i]][, 1],
    y = umaps[[i]][, 2],
    n_neighbors = umap_params$n_neighbors[i],
    min_dist = umap_params$min_dist[i],
    group = df$species
  )
}))

p <- ggplot(d) +
  geom_scattermore(aes(x = x, y = y, colour = group), pointsize = 2) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  ) +
  facet_wrap(min_dist ~ n_neighbors, labeller = label_both, scales = "free") +
  theme_bw() +
  labs(title = "Penguins Species: UMAP")

print(p)

# Segundo grid (valores más grandes)
umap_params1 <- expand.grid(n_neighbors = c(50, 100, 150, 200), min_dist = c(0.5, 0.75, 1.1))

umaps1 <- lapply(seq_len(nrow(umap_params1)), function(i) {
  uwot::umap(
    X = numeric_scaled,
    n_neighbors = umap_params1$n_neighbors[i],
    min_dist = umap_params1$min_dist[i],
    n_threads = 1
  )
})

d1 <- rbindlist(lapply(seq_len(nrow(umap_params1)), function(i) {
  data.table(
    x = umaps1[[i]][, 1],
    y = umaps1[[i]][, 2],
    n_neighbors = umap_params1$n_neighbors[i],
    min_dist = umap_params1$min_dist[i],
    group = df$species
  )
}))

p1 <- ggplot(d1) +
  geom_scattermore(aes(x = x, y = y, colour = group), pointsize = 2) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  ) +
  facet_wrap(min_dist ~ n_neighbors, labeller = label_both, scales = "free") +
  theme_bw() +
  labs(title = "Penguins Species: UMAP (grid 2)")

print(p1)

# -------------------------
# Animaciones (uso de para los primeros n_neighbors 10/20/50/100)
# -------------------------
colores_named <- c("Adelie" = "#E6194B", "Gentoo" = "#3CB44B", "Chinstrap" = "#2e2ed2")

anim_plot1 <- d %>%
  filter(n_neighbors %in% c(10, 20, 50, 100)) %>%
  mutate(parametros = factor(paste('n_neighbors:', n_neighbors, 'min_dist:', min_dist))) %>%
  ggplot() +
  geom_scattermore(aes(x = x, y = y, col = group), pointsize = 2) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  ) +
  theme_bw() +
  labs(title = 'Verificación de los parámetros') +
  scale_color_manual(values = colores_named) +
  transition_states(parametros, transition_length = 2, state_length = 3) +
  labs(title = '{closest_state}')

# Animar (puede tardar según tamaño)
# animate(anim_plot1, nframes = 300)

anim_plot2 <- d1 %>%
  filter(min_dist %in% c(0.5, 0.75, 1.1)) %>%
  mutate(parametros = factor(paste('n_neighbors:', n_neighbors, 'min_dist:', min_dist))) %>%
  ggplot() +
  geom_scattermore(aes(x = x, y = y, col = group), pointsize = 2) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  ) +
  theme_bw() +
  labs(title = 'Selección de parámetros') +
  scale_color_manual(values = colores_named) +
  transition_states(parametros, transition_length = 2, state_length = 3) +
  labs(title = '{closest_state}')

# animate(anim_plot2, nframes = 300)

# -------------------------
# Aquí decidimos cuál es nuestra mejor selección y ajustamos el modelo final
# Por ejemplo: min_dist = 0.75 y n_neighbors = 150
# -------------------------
Final_plot <- d1 %>%
  filter(min_dist == 0.75, n_neighbors == 150) %>%
  mutate(parametros = factor(paste('n_neighbors:', n_neighbors, 'min_dist:', min_dist))) %>%
  ggplot() +
  geom_scattermore(aes(x = x, y = y, col = group), pointsize = 1.1) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  ) +
  theme_bw() +
  labs(title = 'UMAP: Modelo final') +
  scale_color_manual(values = colores_named)

print(Final_plot)

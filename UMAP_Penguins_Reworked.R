###PCA_WDBCtidymodels2

library(tidyverse)
library(tidymodels)
library(modeldata)
library(embed)
library(tsne)
library(uwot)
library(scattermore)
library(readr)
library(Rtsne)
library(baguette)
library(discrim)
library(gganimate)
library(data.table)
library(plotly)
tidymodels_prefer()



df<-read.csv("C:/Users/usuario/OneDrive/Desktop/diplomado 2025/R/datos/BreastCancerDiagnosisWisconsin.csv")

df %>% dim()

df %>% glimpse()

df %>% head()

# UMAP sin valores de los hiper-parametros

library(embed)

df_umap <- df %>%
  dplyr::select(where(is.numeric)) %>%
  scale() %>%
  umap()

# Resultados

str(df_umap)

names(df_umap)

#Base de datos

datos<-data.frame(UMAP1=df_umap[,1],UMAP2=df_umap[,2],diagnostico=df$diagnosis)
head(datos)

ggplot(datos, aes(x = UMAP1, y = UMAP2, color = diagnostico)) +
  geom_point(size = 2) +
  labs(title="WDBC: UMAP")

###Otra forma

umap_rec <- recipe(~., data = df) %>%
  update_role(diagnosis, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_umap(all_predictors())

umap_res <- prep(umap_rec)

umap_res

juice(umap_res) %>%
  ggplot(aes(UMAP1, UMAP2)) +
  geom_point(aes(color = diagnosis), size = 1.5)+
  labs(color = NULL)

###3D

df_umap3 <- df %>%
  dplyr::select(where(is.numeric)) %>%
  scale() %>%
  umap(n_components=3)

head(df_umap3)


umap_df3 <- data.frame(
  UMAP1 = df_umap3[, 1],
  UMAP2 = df_umap3[, 2],
  UMAP3 = df_umap3[, 3],
  diagnostico = factor(df$diagnosis)
)

head(umap_df3)

colors <- c("#F58231", "#911EB4")
hover_text <- paste(
  "diagnostico:", umap_df3$diagnostico, "",
  "Dimension 1:", round(umap_df3$UMAP1, 3),
  "Dimension 2:", round(umap_df3$UMAP2, 3),
  "Dimension 3:", round(umap_df3$UMAP3, 3)
)

conflicted::conflicts_prefer(plotly::layout)


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
  color = ~diagnostico,
  colors = colors
) %>%
  layout(
    title = "UMAP_3D:WDBC ",
    scene = list(
      xaxis = list(title = "UMAP Dimension 1"),
      yaxis = list(title = "UMAP Dimension 2"),
      zaxis = list(title = "UMAP Dimension 3")
    )
  )


###Argumentos de esta funcion

###n_neighbors: The size of local neighborhood (in terms of number of neighboring sample points)
###             In general values should be in the range 2 to 100

###n_components: The dimension of the space to embed into

###metric: Type of distance metric to use to find nearest neighbors ("euclidean" (the default))

###n_epochs: Number of epochs to use during the optimization of the embedded coordinates (default: 500)

###learning_rate: Initial learning rate used in optimization of the coordinates, entre otros

###No hay una menera automatizada de seleccionar los "mejores parametros". Asi que procederemos
###de forma semejante al modelo de tSNE

### UMAP: Exploracion de hiper-parametros

#parametros

umap_params = expand.grid(n_neighbors = c(10, 20, 50, 100),min_dist = c(0.5, 0.75, 1.1))

umaps = lapply(seq(nrow(umap_params)), function(i) {
    emb = umap(
      X = df[,-1],
      n_neighbors = umap_params$n_neighbors[i],
      min_dist  = umap_params$min_dist[i])

  return(emb)
})

d = rbindlist(lapply(seq(nrow(umap_params)), function(i) {
  data.table(
    x = umaps[[i]][,1],
    y = umaps[[i]][,2],
    n_neighbors = umap_params$n_neighbors[i],
    min_dist = umap_params$min_dist[i],
    group = df$diagnosis
  )
}))

p<-ggplot(d) +
  geom_scattermore(
    mapping = aes(x = x, y = y,colour=group),
    pointsize = 2
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  ) +
  facet_wrap(min_dist ~ n_neighbors ,
             labeller = label_both,
             scales = "free") +
  theme_bw() +
  labs(title = "WDBC: UMAP")

p

umap_params1 = expand.grid(n_neighbors = c(50, 100, 150, 200),min_dist = c(0.5, 0.75, 1.1))

umaps = lapply(seq(nrow(umap_params1)), function(i) {
    emb = umap(
      X = df[,-1],
      n_neighbors = umap_params1$n_neighbors[i],
      min_dist  = umap_params1$min_dist[i])

  return(emb)
})

d1 = rbindlist(lapply(seq(nrow(umap_params1)), function(i) {
  data.table(
    x = umaps[[i]][,1],
    y = umaps[[i]][,2],
    n_neighbors = umap_params1$n_neighbors[i],
    min_dist = umap_params1$min_dist[i],
    group = df$diagnosis
  )
}))

p1<-ggplot(d1) +
  geom_scattermore(
    mapping = aes(x = x, y = y,,colour=group),
    pointsize = 2
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  ) +
  facet_wrap(min_dist ~ n_neighbors ,
             labeller = label_both,
             scales = "free") +
  theme_bw() +
  labs(title = "WDBC: UMAP")

p1

colores = c('#E178C5','#EB5B00')
names(colores) = c("B","M")

anim_plot1<-d1 %>% 
  filter(n_neighbors == 10 | n_neighbors == 20 | n_neighbors == 50 | n_neighbors == 100) %>% 
  mutate(parametros = factor(paste('n_neighbors:', n_neighbors, 'min_dist:', min_dist))) %>% ggplot() +
  geom_scattermore(
    mapping = aes(x = x, y = y, col = group),
    pointsize = 2
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  ) +
  theme_bw()+
  labs(title = 'Verificación de los parámetros')+
  scale_color_manual(values = colores) + 
  transition_states(parametros, transition_length = 2, state_length = 3) +
  labs(title = '{closest_state}')

animate(anim_plot1, nframes = 300)

anim_plot2<-d1 %>% 
  filter(min_dist == 0.5 | min_dist == 0.75 | min_dist == 1.1) %>% 
  mutate(parametros = factor(paste('n_neighbors:', n_neighbors, 'min_dist:', min_dist))) %>% ggplot() +
  geom_scattermore(
    mapping = aes(x = x, y = y, col = group),
    pointsize = 2
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  ) +
  theme_bw()+
  labs(title = 'Selección de parámetros')+
  scale_color_manual(values = colores) + 
  transition_states(parametros, transition_length = 2, state_length = 3) +
  labs(title = '{closest_state}')

animate(anim_plot2, nframes = 300)

###Aqui decidimos cual es nuestra mejor seleccion y ajustamos el modelo final

###Por ejemplo min_dist=0.5 y n_neighbors=100

Final_plot<-d1 %>% 
  filter(min_dist == 0.5 , n_neighbors == 200) %>% 
  mutate(parametros = factor(paste('n_neighbors:', n_neighbors, 'min_dist:', min_dist))) %>% ggplot() +
  geom_scattermore(
    mapping = aes(x = x, y = y, col = group),
    pointsize = 1.1
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  ) +
  theme_bw()+
  labs(title = 'UMAP: Modelo final')

Final_plot

























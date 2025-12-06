###WDBC2005_2

#install.packages(c("learnr","tidyverse","tidymodels","embed","corrr","tidytext","sortable","learntidymodels","rstatix","broom",
#                 "rgl","plotly","GGally","tidyr","FactoMineR","factoextra","ggord","modeldata","tsne","uwot","scattermore","readr","Rtsne","gganimate","data.table","ggplot2"))

library(learnr)
library(tidyverse)
library(tidymodels)
library(embed)
library(corrr)
library(tidytext)
library(gradethis)
library(sortable)
library(learntidymodels)
library(rstatix)
library(broom)
library(rgl)
library(plotly)
library(GGally)
library(tidyr)
library(FactoMineR)  
library(factoextra)
library(ggord)
library(modeldata) ###libreria nueva
library(tsne)  ###libreria nueva
library(uwot)  ###libreria nueva
library(scattermore)  ###libreria nueva
library(readr)  ###libreria nueva
library(Rtsne)  ###libreria nueva
library(gganimate)  ###libreria nueva
library(data.table)  ###libreria nueva
theme_set(theme_bw(16))


df<-read.csv("C:/Users/usuario/OneDrive/Desktop/diplomado 2025/R/datos/BreastCancerDiagnosisWisconsin.csv")

###Variable de clasificacion: diagnosis

df <- df %>% 
  mutate(diagnosis = relevel(as.factor(diagnosis), "B", "M"))

df %>% dim()

df %>% glimpse()

df %>% head()

df %>% count(diagnosis)

##t-sne

###Sin proponer hiper-parametros, i.e., con los hiper-parametros de default

set.seed(123)

start.time <- proc.time()

df_tsne <- df %>%
  dplyr::select(where(is.numeric)) %>%
  scale() %>%
  tsne()

proc.time()-start.time

str(df_tsne)

head(df_tsne)

# Adicionando los resultados de  t-SNE a la base

datos<-data.frame(tSNE1=df_tsne[,1],tSNE2=df_tsne[,2],diagnostico=df$diagnosis)
head(datos)

ggplot(datos,aes(x=tSNE1,y=tSNE2))+
geom_point(size=1.1,color="darkblue")+
labs(title="WDBC: t-sne")

ggplot(datos,aes(x=tSNE1,y=tSNE2,color=diagnostico))+
geom_point(size=1.1)+
labs(title="WDBC: t-sne")

ggplot(datos, aes(x = diagnostico, y = tSNE1)) + geom_boxplot(aes(fill="darkblue"),colour = 3,show.legend = FALSE)

ggplot(datos, aes(x = diagnostico, y = tSNE2)) + geom_boxplot(aes(fill="darkblue"),colour = 5,show.legend = FALSE)

tsne_df <- data.frame(
  tSNE1 = df_tsne[, 1],
  tSNE2 = df_tsne[, 2],
  diagnostico = df$diagnosis
)
colors <- c("#E6194B", "#3CB44B")

ggplot(tsne_df, aes(x=tSNE1,y=tSNE2, color = factor(diagnostico))) +
  geom_point(size = 1.5) +
  scale_color_manual(values = colors) +
  labs(
    title = "t-SNE: WDBC",
    x = "t-SNE Dimension 1",
    y = "t-SNE Dimension 2"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20)
  )

###Los argumentos de esta funcion son:

###k: the dimension of the resulting embedding

###initial_dims: The number of dimensions to use in reduction method.

###perplexity: Perplexity parameter. (optimal number of neighbors)

###max_iter: Maximum number of iterations to perform.

###min_cost: The minimum cost value (error) to halt iteration.

###epoch_callback: A callback function used after each epoch (an epoch here means a set number of iterations)
###                (Una función de devolución de llamada utilizada después de cada época (una época aquí significa un número determinado de iteraciones))

###epoch: The number of iterations in between update messages.

###Se puede correr con una selección arbitraria de los parámetros más importantes que son: perplexity, max_iter y min_cost, en ese orden

###Otra funcion para hacerlo

set.seed(123)

start.time <- proc.time()

df_Rtsne <- df %>%
  dplyr::select(where(is.numeric)) %>%
  scale() %>%
  Rtsne() 

proc.time()-start.time


str(df_Rtsne)

head(df_Rtsne)

names(df_Rtsne)

datos1<-data.frame(Rtsne1=df_Rtsne$Y[,1],Rtsne2=df_Rtsne$Y[,2],diagnostico=df$diagnosis)
head(datos1)

ggplot(datos1,aes(x=Rtsne1,y=Rtsne2))+
geom_point(size=1.1,color="darkred")+
labs(title="WDBC: Rtsne")

ggplot(datos1,aes(x=Rtsne1,y=Rtsne2,color=diagnostico))+
geom_point(size=1.1)+
labs(title="WDBC: Rtsne")

ggplot(datos1, aes(x = diagnostico, y = Rtsne1)) + geom_boxplot(aes(fill="darkblue"),colour = 3,show.legend = FALSE)

ggplot(datos1, aes(x = diagnostico, y = Rtsne2)) + geom_boxplot(aes(fill="darkblue"),colour = 5,show.legend = FALSE)

Rtsne_df <- data.frame(
  Rtsne1 = df_Rtsne$Y[, 1],
  Rtsne2 = df_Rtsne$Y[, 2],
  diagnostico = df$diagnosis
)
colors <- c("#F58231", "#911EB4")

ggplot(Rtsne_df, aes(x=Rtsne1,y=Rtsne2, color = factor(diagnostico))) +
  geom_point(size = 1.5) +
  scale_color_manual(values = colors) +
  labs(
    title = "t-SNE: WDBC",
    x = "t-SNE Dimension 1",
    y = "t-SNE Dimension 2"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20)
  )

###3D

set.seed(123)

df_tsne3 <- df %>%
  dplyr::select(where(is.numeric)) %>%
  scale() %>%
  tsne(k=3)

str(df_tsne3)

head(df_tsne3)

tsne_df3 <- data.frame(
  tSNE1 = df_tsne3[, 1],
  tSNE2 = df_tsne3[, 2],
  tSNE3 = df_tsne3[, 3],
  diagnostico = factor(df$diagnosis)
)

head(tsne_df3)

colors <- c("#F58231", "#911EB4")
hover_text <- paste(
  "diagnostico:", tsne_df3$diagnostico, "",
  "Dimension 1:", round(tsne_df3$tSNE1, 3),
  "Dimension 2:", round(tsne_df3$tSNE2, 3),
  "Dimension 3:", round(tsne_df3$tSNE3, 3)
)

plot_ly(
  data = tsne_df3,
  x = ~tSNE1,
  y = ~tSNE2,
  z = ~tSNE3,
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 6),
  text = hover_text,
  hoverinfo = "text",
  color = ~diagnostico,
  colors = colors
) %>%
  layout(
    title = "t-SNE_3D:WDBC ",
    scene = list(
      xaxis = list(title = "t-SNE Dimension 1"),
      yaxis = list(title = "t-SNE Dimension 2"),
      zaxis = list(title = "t-SNE Dimension 3")
    )
  )

###3D Rtsne

set.seed(123)

df_Rtsne3 <- df %>%
  dplyr::select(where(is.numeric)) %>%
  scale() %>%
  Rtsne(dims=3)

str(df_Rtsne3)

head(df_Rtsne3)

tsne_Rdf3 <- data.frame(
  tSNE1 = df_Rtsne3$Y[, 1],
  tSNE2 = df_Rtsne3$Y[, 2],
  tSNE3 = df_Rtsne3$Y[, 3],
  diagnostico = factor(df$diagnosis)
)

head(tsne_Rdf3)

colors <- c("#F58231", "#911EB4")
hover_text <- paste(
  "diagnostico:", tsne_Rdf3$diagnostico, "",
  "Dimension 1:", round(tsne_Rdf3$tSNE1, 3),
  "Dimension 2:", round(tsne_Rdf3$tSNE2, 3),
  "Dimension 3:", round(tsne_Rdf3$tSNE3, 3)
)

plot_ly(
  data = tsne_Rdf3,
  x = ~tSNE1,
  y = ~tSNE2,
  z = ~tSNE3,
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 6),
  text = hover_text,
  hoverinfo = "text",
  color = ~diagnostico,
  colors = colors
) %>%
  layout(
    title = "t-SNE_3D:WDBC ",
    scene = list(
      xaxis = list(title = "t-SNE Dimension 1"),
      yaxis = list(title = "t-SNE Dimension 2"),
      zaxis = list(title = "t-SNE Dimension 3")
    )
  )

###Con ambas funciones deberíamos de explorar con varios valores de los hiper-parametros 

###Exploración de parametros tsne

set.seed(123)

tsne_params <-  expand.grid(perplexity=c(10,15,20,25,30,50)) ###Explorando parametro de perplejidad: perplexity. max_iter fijo 

set.seed(123)

start.time <- proc.time()

tsne_res <- lapply(seq(nrow(tsne_params)), function(i) {
	print(i)
	res <- tsne::tsne(
		X = df[,-1],
            max_iter = 500,
		perplexity = tsne_params[[1]][i]
		
	)
	
	return(res)
})

proc.time()-start.time


library(data.table)
d1 <- rbindlist(lapply(seq(nrow(tsne_params)), function(i) {
	data.table(
		x = tsne_res[[i]][,1],
		y = tsne_res[[i]][,2],
		perplexity = tsne_params[[1]][i],
		group = df$diagnosis
	)
}))
												  

p1 <- ggplot(d1) +
	geom_scattermore(
		mapping = aes(x = x, y = y,colour=group,size=2),
		pointsize = 2
	) +
	theme(
		axis.text = element_blank(),
		axis.ticks = element_blank(),
		axis.title = element_blank(),
		legend.position = "none"
	) +
	facet_wrap("perplexity" , 
		labeller = label_both,
		scales = "free")
p1 <- p1 + theme_minimal() +
	theme(legend.position = "none")
p1


###Rtsne exploracion de parametros

set.seed(123)

start.time <- proc.time()

Rtsne_params <-  expand.grid(perplexity=c(10,15,20,25,30,50))
	
Rtsne_res <- lapply(seq(nrow(Rtsne_params)), function(i) {
	print(i)
	Rres <- Rtsne::Rtsne(
		X = df[,-1],
            max_iter = 500,
            verbose=TRUE,
            perplexity = Rtsne_params[[1]][i],
            check_duplicates = FALSE
		
	)
	
	return(Rres)
})

proc.time()-start.time

d2 <- rbindlist(lapply(seq(nrow(Rtsne_params)), function(i) {
	data.table(
		x = Rtsne_res[[i]]$Y[,1],
		y = Rtsne_res[[i]]$Y[,2],
		perplexity = Rtsne_params[[1]][i],
		group = df$diagnosis
	)
}))
												  
p2 <- ggplot(d2) +
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
	facet_wrap("perplexity" , 
		labeller = label_both,
		scales = "free")
p2 <- p2 + theme_minimal() +
	theme(legend.position = "none")
p2

###tsne es muy lento. Para juzgar cómo funciona este algoritmo haciendo una exploracion con dos
###parametros, usaremo Rtsne

set.seed(123)
Rtsne_params2 = expand.grid(perplexity=c(10,15,20,25,30), eta = c(10, 50, 100, 150))  ###eta: tasa de aprendizaje

Rtsne_res2 = lapply(seq(nrow(Rtsne_params2)), function(i) {
  Rres = Rtsne(
    X = df[,-1],
    max_iter = 500,
    verbose=TRUE,
    perplexity = Rtsne_params2$perplexity[i],
    check_duplicates = FALSE,
    eta = Rtsne_params2$eta[i],
    pca = F
  )
  return(Rres)
})

d3 = rbindlist(lapply(seq(nrow(Rtsne_params2)), function(i) {
  data.table(
    x = Rtsne_res2[[i]]$Y[,1],
    y = Rtsne_res2[[i]]$Y[,2],
    perplexity = Rtsne_params2[[1]][i],
    eta = Rtsne_params2[[2]][i],
    group = df$diagnosis
  )
}))


p3<-ggplot(d3) +
  geom_scattermore(
    mapping = aes(x = x, y = y ,colour=group),
    pointsize = 2
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  ) +
  facet_wrap(eta~perplexity ,
             labeller = label_both,
             scales = "free",
             ncol = 5) +
  theme_bw() 

p3

###

set.seed(123)
Rtsne_params3 = expand.grid(perplexity=c(10,15,20,30), eta = c(10, 50, 100, 150, 200))  ###eta: tasa de aprendizaje

Rtsne_res3 = lapply(seq(nrow(Rtsne_params3)), function(i) {
  Rres = Rtsne(
    X = df[,-1],
    max_iter = 500,
    verbose=TRUE,
    perplexity = Rtsne_params3$perplexity[i],
    check_duplicates = FALSE,
    eta = Rtsne_params3$eta[i],
    pca = F
  )
  return(Rres)
})

d4 = rbindlist(lapply(seq(nrow(Rtsne_params3)), function(i) {
  data.table(
    x = Rtsne_res3[[i]]$Y[,1],
    y = Rtsne_res3[[i]]$Y[,2],
    perplexity = Rtsne_params3[[1]][i],
    eta = Rtsne_params3[[2]][i],
    group = df$diagnosis
  )
}))


p4<-ggplot(d4) +
  geom_scattermore(
    mapping = aes(x = x, y = y ,colour=group),
    pointsize = 2
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  ) +
  facet_wrap(eta~perplexity ,
             labeller = label_both,
             scales = "free",
             ncol = 5) +
  theme_bw() 

p4


colores = c('#E178C5','#EB5B00')
names(colores) = c("B","M")

anim_plot1<-d4 %>% 
  filter(perplexity == 10 | perplexity == 15 | perplexity == 20 | perplexity == 30) %>% 
  mutate(parametros = factor(paste('perplexity:', perplexity, 'eta:', eta))) %>% ggplot() +
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

animate(anim_plot1, nframes = 200)
## no olvidar instalar av o lo que sea que se necesita para la animacion 


anim_plot2<-d4 %>% 
  filter(eta == 10 | eta == 50 | eta == 100 | eta == 150 | eta == 200) %>% 
  mutate(parametros = factor(paste('perplexity:', perplexity, 'eta:', eta))) %>% ggplot() +
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
  transition_states(parametros,transition_length = 3, state_length = 1) +
  labs(title = '{closest_state}')

animate(anim_plot2, nframes = 300)

###Despues, se tendria que decidir que seleccion de estos parametros es la que mejor representa
###nuestros datos y correr un modelo con ellos

###Por ejemplo perplexity=30 y eta=150

set.seed(123)

df_RtsneF <- df %>%
  dplyr::select(where(is.numeric)) %>%
  scale() %>%
  Rtsne(perplexity=30, eta=150) 


datos2<-data.frame(Rtsne1=df_RtsneF$Y[,1],Rtsne2=df_RtsneF$Y[,2],diagnostico=df$diagnosis)

win.graph()
ggplot(datos2,aes(x=Rtsne1,y=Rtsne2,color=diagnostico))+
geom_point(size=1.5)+
labs(title="WDBC: Rtsne")

###O bien

final_plot<-d4 %>% 
  filter(perplexity==30, eta == 200) %>% 
  mutate(parametros = factor(paste('perplexity:', perplexity, 'eta:', eta))) %>% ggplot() +
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
  labs(title = 'tSNE: Modelo final')

win.graph()
final_plot


final_plot1<-d4 %>% filter(perplexity==30 & eta == 200) %>% 
  ggplot(aes(x = x, y = y, col = group)) +
  geom_point() +
  theme_bw() +
  scale_color_manual(values = colores) +
  labs(title = 'tSNE: Modelo final',
       subtitle = 'perplexity: 30 and eta: 200')

win.graph()
final_plot1




































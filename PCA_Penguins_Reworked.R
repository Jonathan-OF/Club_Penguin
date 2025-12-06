#pemguins
install.packages("ggcorrplot")
install.packages("tinytex")
install.packages("rmarkdown")
tinytex::install_tinytex()

library(learnr)
library(tidyverse)
library(tidymodels)
library(embed)
library(ggcorrplot)
library(corrr)
library(tidytext)
library(gradethis)
library(psych)
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
theme_set(theme_bw(16))

df<-read.csv("C:/Users/usuario/OneDrive/Desktop/Proyecto 2 Diplomado/Student_Results/Student_Exam_Results.csv")

class(df)

df %>% dim() # dimension de la base de datos

df %>% glimpse() # tipo de variables de la base

df %>% head() # muestra los primeros 6 registros

df1<-df[,1:9] # colocar en una variable los nombres de todas las columnas

df1 %>% head()


###Valores faltantes

colSums(is.na(df1)) # sin datos faltantes

df1 %>% summary() # resumen estadístico

### eliminacion de la columna total 


df1 <- df1 %>% select(-Total)




###Variable de clasificacion (target): Results & Div 

df1 <- df1 %>% 
  mutate(
    Results = as.factor(Results),
    Div = as.factor(Div)
  )

df1 %>% count(Results)
df1 %>% count(Div)


table(df$Results)
table(df$Div)


df1 %>% glimpse() # confirmamos que Results y Div cambiaron 

# Haciendo graficas con ggplot
df1 %>%
  group_by(Results) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = Results, y = n)) +
  geom_col(fill = c("#CC0033", "#e319dc")) +
  geom_text(
    aes(label = paste0(n, " | ", signif(prop * 100, digits = 4), "%")),
    nudge_y = 10
  ) +
  ggtitle("Distribución de Resultados (No Aprobado vs Aprobado)") +
  theme_gray()

df1 %>%
  group_by(Div) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = Div, y = n)) +
  geom_col(fill = c("#CC0033", "#e319dc", "#33b9ff", "#33ff66")) +
  geom_text(
    aes(label = paste0(n, " | ", signif(prop * 100, digits = 4), "%")),
    nudge_y = 10
  ) +
  ggtitle("Distribución de Divisiones (0, 1, 2, 3)") +
  theme_gray()


# media de variables numericas
df1 %>%
  select(where(is.numeric)) %>%
  colMeans()

###Histogramas

# le quitamos las variables factor a la que no se le puede hacer un histograma 
# estas son solo variables predictoras, las cuales no tienen una distribucion asociada, por lo que no es 
# correcto sacar análisis de la simetria de las distribuciones
df1 %>% 
  pivot_longer(
    cols = !c(Results, Div),     # exclude your factor columns
    names_to = "Variable",
    values_to = "Score"
  ) %>%
  ggplot(aes(x = Score)) +
  geom_histogram(
    aes(y = ..density..),
    bins = 20,
    colour = "black",
    fill = "darkmagenta"
  ) +
  facet_wrap(~ Variable, ncol = 4, scales = "free") +
  theme_minimal()

###box-plot

df1 %>% 
  pivot_longer(
    cols = !c(Results, Div),     # exclude factor columns
    names_to = "Variable",
    values_to = "Score"
  ) %>%
  ggplot(aes(y = Score)) +
  geom_boxplot(
    fill = "darkred",
    color = "black"
  ) +
  facet_wrap(~ Variable, ncol = 4, scales = "free") +
  theme_minimal()


###Densidad

df1 |> 
  pivot_longer((!Results & !Div)
, 
               names_to = "Variable",
               values_to = "Score") |>
  ggplot(aes(x = Score, fill = factor(Results))) +
  geom_density(alpha = 0.4, colour = NA) +
  facet_wrap("Variable", ncol = 4, scales = "free") +
  scale_fill_manual(values = c("0" = "darkred", "1" = "steelblue"),
                    name = "Results",
                    labels = c("0" = "Not approved", "1" = "Approved")) +
  theme_minimal()


df1 |> 
  pivot_longer((!Results & !Div), 
               names_to = "Variable",
               values_to = "Score") |>
  ggplot(aes(x = Score, fill = factor(Div))) +
  geom_density(alpha = 0.4, colour = NA) +
  facet_wrap("Variable", ncol = 4, scales = "free") +
  scale_fill_brewer(palette = "Set2", name = "Div") +
  theme_minimal()


###elegimos la variable Results como nuestra variable de respuesta ya que no es claro que es Div y comparamos

df1_long <- df1 %>% 
  pivot_longer(
    cols = !c(Results, Div),
    names_to = "predictores",
    values_to = "values"
  )


df1_long %>% 
  ggplot(mapping = aes(x = Results, y = values, fill = predictores)) +
  geom_boxplot() + 
  facet_wrap(~ predictores, scales = "free", ncol = 4) +
  scale_color_viridis_d(option = "plasma", end = .7) +
  theme(legend.position = "none") +
  labs(title = "Comparación vía box-plot")

df1_long |> ggplot(mapping = aes(values, fill = Results)) +
  geom_histogram(color = "white") +
  facet_wrap(~predictores, scales = "free", ncol= 4) +
  scale_color_viridis_d(option = "plasma", end = .7) +
  labs(title = "Variables Distribution") +
  theme_light()+
  labs(title = "Comparación vía histograma")

df1_long |> ggplot(mapping = aes(values, fill = Results)) +
  geom_density(color = "white") +
  facet_wrap(~predictores, scales = "free", ncol= 4) +
  scale_color_viridis_d(option = "plasma", end = .7) +
  labs(title = "Variables Distribution") +
  theme_light()+
  labs(title = "Comparación a través de densidad")

### verificar columnas numericas
str(df1)


## aplicamos ggpairs
ggpairs(
  df1,
  mapping = aes(color = Results),
  columns = 1:6   # only numeric columns
)


### correlacion ###

grades_corr <- df1 %>%
  select(-Results, -Div) %>%
  correlate(method = "spearman")


grades_corr 

corr_stg <- cor(df1[, -c(7, 8)], method = "spearman")

corr_stg

ggcorrplot::ggcorrplot(corr = corr_stg,
                       type = "lower", 
                       show.diag = TRUE,
                       lab = TRUE) 
# Estructura de asociacion entre variables                       
# si determinantes es cercano a cero, correlacion es fuerte, si no, las variables están lejanas a estar relacionadas                       lab_size = 3)
det(corr_stg) 

# El KMO deve tener valores por encima a 0.8, ie cercanos a 1 para indicar una estructura de asociacion fuerte entre variables
psych::KMO(corr_stg)

# estos dos valores muestran que aplicar un PCA  dará un resultado útil
### vemos que la correlación es bajisima y PCA tal vez no sea la mejor opción para este conjunto de datos

###Prueba de Bartlett
# fuera de la diagonal de la matrix de corr, los valores son distintos de cero? si lo son, se trata de la misma matriz de corr
# si no lo son, hay relacion entre distintas variables

psych::cortest.bartlett(corr_stg,n=dim(df1)[1])

###Todas estas medidas indican que hay una estructura de asociacion muy baja

cor.df1 <- df1 %>% 
  select(-Results, -Div) %>% 
  cor_mat()

cor.df1

options(scipen=999)

cor.df1 %>% cor_get_pval()

cor.df1 %>%
  cor_reorder() %>%
  pull_lower_triangle() %>%
  cor_plot(label = TRUE)

cor.df1 %>% cor_gather() %>% print(n=Inf)

### Aún con esta bajisima correlación debemos continuar

###PCA

###

pca_recipe <-
  recipe(~., data = df1) %>% 
  update_role(Results, new_role = "id") %>%   # <-- Remplaza diagnosis
  update_role(Div, new_role = "id") %>%        # Optional, ya que Div no es claro 
  step_dummy(all_nominal_predictors()) %>%  
  step_normalize(all_predictors()) %>%
  step_pca(all_numeric_predictors(), threshold = 0.80, id= "pca")


pca_recipe

stg_pca <- prep(pca_recipe)

pca_loading <- tidy(stg_pca, id="pca")
pca_loading

pca_variances <- tidy(stg_pca, id = "pca", type = "variance")
pca_variances

pca_var_percent<- tidy(stg_pca, id = "pca", type = "variance")%>%
  filter(str_detect(terms, "percent variance"))
pca_var_percent

pca_var_cum_percent<-tidy(stg_pca, id = "pca", type = "variance")%>%
  filter(str_detect(terms, "cumulative percent variance"))
pca_var_cum_percent

stg_pca %>% 
  tidy(id = "pca", type = "variance") %>% 
  dplyr::filter(terms == "percent variance") %>% 
  ggplot(aes(x = component, y = value)) + 
  geom_col(fill = "#B53389") + 
  xlim(c(0, 7)) + 
  geom_point(size=3) +
  geom_line(color="darkblue", size=1.1)+
  labs(x="PC", y="% de varianza", title="Scree plot")

stg_pca %>% 
  tidy(id = "pca", type = "variance") %>% 
  dplyr::filter(terms == "cumulative percent variance") %>% 
  ggplot(aes(x = component, y = value)) + 
  geom_col(fill = "#F25E52") + 
  xlim(c(0, 7)) + 
  geom_point(size=3) +
  geom_line(color="darkblue", size=1.1)+
  labs(x="PC", y="% acumulado de varianza", title="Scree plot")

variance_exp <- tidy(stg_pca,id = "pca", type = "variance")%>%
  filter(str_detect(terms, "percent variance"))
variance_exp


### Notamos que solo retenemos el 85% de la unformación con 5 componentes principales lo cual hace imposible visualizar bien 
### la información en graficas

###Grafica de los primeros dos componentes yeast

VE <- paste("Varianza explicada por dos componentes:"
            ,round(variance_exp$value[[1]]+variance_exp$value[[2]], digits = 2),"%")
VE

datos_pca<-bake(wdbc_pca, new_data = NULL)
datos_pca

datos_pca %>%
  ggplot(aes(x = PC1, y = PC2))+
  geom_point()+
  labs(x = paste0("PC1: ",round(variance_exp$value[[1]],2), "%"),
       y = paste0("PC2: ",round(variance_exp$value[[2]],2), "%"))+
  ggtitle(paste0("WDBC: Gráfica de componentes principales","\n",VE))

datos_pca %>%
  ggplot(aes(x = PC1, y = PC2,color = factor(diagnosis)))+
  geom_point()+
  labs(x = paste0("PC1: ",round(variance_exp$value[[1]],2), "%"),
       y = paste0("PC2: ",round(variance_exp$value[[2]],2), "%"))+
  ggtitle(paste0("WDBC: Gráfica de componentes principales","\n",VE))

juice(wdbc_pca) %>%
  ggplot(aes(PC1, PC2, label = diagnosis)) +
  geom_point(aes(color = diagnosis), alpha = 0.9, size = 2) +
  geom_text(check_overlap = TRUE, hjust = "inward", family = "IBMPlexSans") +
  labs(color = NULL)

###Componentes

wdbc_pca %>%
  tidy(id = "pca") %>% 
  mutate(terms = tidytext::reorder_within(terms, 
                                          abs(value), 
                                          component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  tidytext::scale_y_reordered() +
  scale_fill_manual(values = c("#b6dfe2", "#0A537D")) +
  labs(
    x = "Absolute value of contribution",
    y = NULL, fill = "¿Positive?"
  ) 


wdbc_pca %>%
  tidy(id = "pca") %>%
  filter(component %in% paste0("PC", 1:3)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL)

###

###3D

plot3d( 
  x=datos_pca$PC1, y=datos_pca$PC2, z=datos_pca$PC3, 
  col = as.numeric(datos_pca$diagnosis), 
  type = 's', 
  radius = .1,
  xlab="PC1", ylab="PC2", zlab="PC3")

g.df1 <- plot_ly(datos_pca, x = ~PC1, y = ~PC2, z = ~PC3, color = ~diagnosis, colors =c("#0000FF", "#FF00FF") )
g.df1 <- g.df1 %>% add_markers()
g.df1 <- g.df1 %>% layout(scene = list(xaxis = list(title = 'PC1'),
                                       yaxis = list(title = 'PC2'),
                                       zaxis = list(title = 'PC3')))

g.df1


datos_pca$diagnosis[which(datos_pca$diagnosis == "B" )] <- "B"
datos_pca$diagnosis[which(datos_pca$diagnosis == "M" )] <- "M"
datos_pca$diagnosis <- as.factor(datos_pca$diagnosis)

fig <- plot_ly(datos_pca, x = ~PC1, y = ~PC2, z = ~PC3, color = ~diagnosis, colors = c('#BF382A', '#0C4B8E'))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'PC1'),
                                   yaxis = list(title = 'PC2'),
                                   zaxis = list(title = 'PC3')))

fig

###EXTRAS

ggpairs(df1, mapping = aes(color = diagnosis))

ggpairs(datos_pca, mapping = aes(color = diagnosis))

###

res.pca = PCA(df1[,-1],  scale.unit=TRUE) 

fviz_pca_var(res.pca,
             alpha.var = "contrib",
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = 'Influencia de las variables en PCA1 y PCA2',
             repel = TRUE)

fviz_pca_ind(res.pca,
             col.ind = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title='Distribución de los individuos en PCA1 y PCA2',
             repel = TRUE)

fviz_pca_biplot(res.pca, repel = TRUE,
                title='Biplot',
                col.var = "#2E9FDF",
                col.ind = "#696969")

ggord(res.pca, df1$diagnosis)

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "blue", 
                col.ind = df$diagnosis, 
                palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                addEllipses = TRUE, ellipse.level = 0.95)
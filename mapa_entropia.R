#28 de abril 2021
#Entropia para politicas publicas
#Indice de Shannon

library(tidyverse)
library(sf)
library(ggtext)



df <- read.csv('datos/politicasEstatalesCovidMexico.csv')
mapa <- read_sf('shp/Mexico_states.shp')

#Hacemos los datos en formato tidy






politicas <- df %>% 
  janitor::clean_names() %>% 
  rename('politicas' = estado) %>% 
  pivot_longer(!politicas,
               names_to = 'estado',
               values_to = 'valor') %>% 
  pivot_wider(names_from = politicas,
              values_from = valor) %>% 
  janitor::clean_names() %>% 
  mutate(state = mapa$state)

#agregamos los datos de entrpia


shannon <- tibble()

for (i in 1:32) {
  
  aux<- 0
  
  for (j in 1:10) {
    if (politicas[i, j+1] !=0){
      
      aux <- (politicas[i, j+1]/sum(politicas[i, 2:11]))*
        log(politicas[i, j+1]/sum(politicas[i, 2:11])) + aux
      
    }
    shannon[i, 'entropia'] = -aux
  }
}

politicas <- cbind(politicas, shannon)

#hacesmos un csv con el tibble

write.csv(politicas, file = 'datos/politicas_tidy.csv')

#hacemos el data frame con los datos geometricos

politicas_sf <- mapa %>% 
  full_join(politicas, by = 'state')



##Shannon taller####

#politicas <- read.csv('datos/politicasEstatalesCovidMexico.csv')
#
#categorias <- c('ficcion', 'fantasia', 'historias', 'drama',
#                'comedia', 'romance', 'suspenso')

#videocentro1 <- c(100, 20, 30, 22, 80, 15, 33)
#videocentro2 <- c(25, 37, 28, 60, 127, 85, 38)

#sum(videocentro1) #300
#sum(videocentro2) #400
#peliculas <- tibble(categorias, videocentro1, videocentro2)


#variable auxiliar

#shannon <- 0


#for (i in 1:7) {
#  
#  shannon <- peliculas$videocentro1[i]/sum(peliculas$videocentro1)*
#    log(peliculas$videocentro1[i]/sum(peliculas$videocentro1)) + shannon
#  
#}

#print(-shannon)



#shannon2 <- 0

#for (j in 1:7) {
  
#  shannon2 <- peliculas$videocentro2[j]/sum(peliculas$videocentro2)*
#    log(peliculas$videocentro2[j]/sum(peliculas$videocentro2)) + shannon2
  
#}

#print(-shannon2)


#proporcion/suma(frecuenciasvc2)*log(proporcion/suma(frecuenciasvc2)) + res








#res <-0
#shannonH <- function(x,y){
#  for (i in 1:y){
#    if (x[i] != 0) {
#      res <- (x[i]/sum(x)) * log(x[i]/sum(x)) + res
#   }
#  }                           
#  return(-res)   #el resultado negativo (- res) es parte de la fórmula del estadístico
#}


#puebla<- shannonH(politicas$Puebla, 10)





    






## Mapa de mx con entropia####

politicas_sf %>% 
  ggplot()+
  geom_sf(mapping = aes(fill = entropia))+
  scale_fill_viridis_c(option = 'G', direction = -1)+
  labs(title = 'Políticas económicas al comienzo de la pandemia',
       subtitle = 'Entropía de Shannon',
       fill = 'Entropía')+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
         barwidth = unit(20, 'lines')),
         barheight = unit(.5, 'lines'))+
  theme(legend.position = 'top',
        panel.background = element_rect(fill = NA),
        axis.text = element_blank(),
        axis.ticks = element_blank())

  
ggsave("plots/entropia.png", 
       dpi = 400, width = 8, height = 7)

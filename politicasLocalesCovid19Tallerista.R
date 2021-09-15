# 28 abril de 2021
# Ciudad de México
# Dr. Enrique García-Tejeda  mail: cgarcia@up.edu.mx  tw: @enriquegtejeda
# https://www.garciatejeda.com/publicaciones/

# Código en R-base que implementa la estimación de Shannon´s H y replica la investigación
# "Las políticas económicas frente al COVID19 en México". Disponible en:
# http://www.gigapp.org/ewp/index.php/GIGAPP-EWP/article/view/202
# ¿Son las políticas locales diversas? ¿son amplias? ¿son diferentes?
# 
# En este archivo encontrará comentarios que inician con "Tallerista" con el objetivo
# de que pueda escribir su propio código en esas partes indicadas


# 0. Limpieza y preparación del espacio de trabajo
rm(list=ls())
graphics.off()
# Tallerista: Coloque en esta línea su directorio de trabajo
politicas <- read.csv("politicasEstatalesCovidMexico.csv")


# 0. Definición de Función de Shannon
# Argumentos formales (x:vector de frecuencias; y:número de categorias)
# El término log(x[i]/sum(x))) corresponde a la Normalización de Shannon

res <-0
shannonH <- function(x,y){
  for (i in 1:y){
    if (x[i] != 0) {
      res <- (x[i]/sum(x)) * log(x[i]/sum(x)) + res
    }
  }                           
  return(-res)   #el resultado negativo (- res) es parte de la fórmula del estadístico
}

# 1. Ejecución de la Función de Shannon 
# Tallerista: ¿Por qué existe una estructura if dentro de la estructura iterativa for?
# Tallerista: Pruebe la función con los argumentos efectivos de cualquier estado


# 2. Creación de estructura de datos (dataFrame) para almacenar la estimación del
# índice de Entropía de Shannon (Boydstun et al 2014)
estados <- colnames(politicas)
indicador <- rep(0, 32)
indiceSH <-data.frame(estados[2:33], indicador)


# 3. Estimación del Índice de Entropía de Shannon para 32 estados
for (j in 1:32){
  indiceSH$indicador[j] <- shannonH(politicas[,j+1],10)
}


# 3.1 Gráfico de columnas con Shannon's H (alfabético y por Índice de Entropía)
# Tallerista: Inserte elementos estéticos
barplot(indiceSH$indicador)

indiceSHOrdenado <- indiceSH[order(indiceSH[,2], decreasing = TRUE),]

barplot(indiceSHOrdenado$indicador)


# 4. Prueba exacta de Fisher de frecuencias esperadas vs frecuencias observadas
# Tallerista: ¿Por qué la advertencia de la consola sugiere que Pearson's Chi-squared test
# puede no ser una buena aproximación?
prueba <- politicas[,2:33]
chisq.test(prueba)
fisher.test(prueba, simulate.p.value = TRUE)


# 5. Número de políticas económicas subnacionales frente al COVID19
indiceSH$total <- rep(0,32)

for (k in 1:32){
  indiceSH$total[k] <- sum(politicas[,k+1])
}

# 6. Visualización del número de medidas alfabética y por número
barplot(indiceSH$total,
        names.arg = indiceSH$estados.2.33.,
        main = "Políticas Económicas Subnacionales frente al COVID19 en México",
        col = "darkorchid",
        las = 2,
        cex.names= 0.7)

estadosOrdenado <- indiceSH[order(indiceSH[,3], decreasing = FALSE),]

barplot(estadosOrdenado$total,
        names.arg = estadosOrdenado$estados.2.33.,
        main = "Políticas Económicas vs COVID19",
        col = "gold1",
        las = 2,
        cex.names=0.7)

# 7. Tallerista ¿Son las políticas locales diversas? ¿son amplias? ¿son diferentes?
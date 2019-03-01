# Cargar paquetes
library(dplyr)
library(ggplot2)

# Cargar datos
spanLexData <- read.csv("spanLexData.csv")

# Analizar datos
str(spanLexData)
glimpse(spanLexData)

# Ver los niveles de la columna pos
unique(spanLexData$pos)

# Mostrar los primeros 100 observaciones
head(spanLexData, n=100)

# Reordernar según rank
spanLexData <- spanLexData %>%
  arrange(rank)

# Crear gráfica
ggplot(spanLexData, aes(y=rawFreq,x=rank,color=pos)) +
  geom_point() +
  scale_y_log10()

# Ver por una gráfica de dispersión por pos
ggplot(spanLexData, aes(y=rawFreq,x=rank)) +
  geom_point() +
  scale_y_log10() +
  facet_wrap(~pos)
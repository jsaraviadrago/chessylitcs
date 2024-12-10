# Análisis de árbol de decisión con base de datos avocados


library(tidyverse)
library(rpart)
library(caret)
library(dplyr)


data_avocado <- read.csv("/home/juan-carlos/Descargas/Avocado_HassAvocadoBoard_20152023v1.0.1.csv")

data_avocado_modelo <- data_avocado |> 
  select(-Date)

head(data_avocado_modelo)

model <- rpart(TotalVolume ~ ., data_avocado_modelo)
par(xpd = NA)

plot(model)
text(model, digits = 3)

print(model, digits = 2)

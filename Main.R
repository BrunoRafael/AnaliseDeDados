library(dplyr)
library(caret)
library(lars)
library(gmodels)
library(ggplot2)
library(elasticnet)
library(gridExtra)
library(corrplot)

data <- read.table("C:/Users/Signove/Documents/UFCG/ad2/problem3/prostate.txt", header=T, dec=".")

data_training <- filter(data, train == TRUE)
  
data_testing <- filter(data, train == FALSE)


#gráficos de comparação entre as variáveis e o nível de psa no sangue

graph_vol <- qplot(data$lcavol, data$lpsa, xlab="Volume", ylab="psa ng/ml")
graph_weight <- qplot(data$lcavol, data$lpsa, xlab="Peso", ylab="psa ng/ml")
graph_age <- qplot(data$lcavol, data$lpsa, xlab="Idade", ylab="psa ng/ml")
graph_bph <- qplot(data$lcavol, data$lpsa, xlab="Hiperplasia prostática benigna", ylab="psa ng/ml")
graph_svi <- qplot(data$lcavol, data$lpsa, xlab="Invasão das vesículas seminais", ylab="psa ng/ml")
graph_lcp <- qplot(data$lcavol, data$lpsa, xlab="Penetração capsular", ylab="psa ng/ml")
graph_gleason <- qplot(data$lcavol, data$lpsa, xlab="Escore Gleason", ylab="psa ng/ml")
graph_pgg45 <- qplot(data$lcavol, data$lpsa, xlab="Porcentagem escore Gleason (4 ou 5)", ylab="psa ng/ml")

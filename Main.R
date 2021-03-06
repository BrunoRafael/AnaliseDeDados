#bibliotecas usadas
library(dplyr)
library(caret)
library(lars)
library(gmodels)
library(ggplot2)
library(elasticnet)
library(gridExtra)
library(corrplot)

#Conjunto de dados usado
data <- read.table('E:/UFCG2/AD2/pesquisas/prostate- atividade3/prostate.dat')

data_training <- filter(data, train == TRUE)

data_test <- filter(data, train == FALSE)


#Gr�ficos de compra��o entre as vari�veis e o n�vel de psa no sangue

graph_vol <- qplot(data$lcavol, data$lpsa, xlab="Volume", ylab="psa ng/ml")
graph_weight <- qplot(data$lweight, data$lpsa, xlab="Peso", ylab="psa ng/ml")
graph_age <- qplot(data$age, data$lpsa, xlab="Idade", ylab="psa ng/ml")
graph_bph <- qplot(data$lbph, data$lpsa, xlab="Hiperplasia prost�tica benigna", ylab="psa ng/ml")
graph_svi <- qplot(data$svi, data$lpsa, xlab="Invas�o das ves�culas seminais", ylab="psa ng/ml")
graph_lcp <- qplot(data$lcp, data$lpsa, xlab="Penetra��o capsular", ylab="psa ng/ml")
graph_gleason <- qplot(data$gleason, data$lpsa, xlab="Escore Gleason", ylab="psa ng/ml")
graph_pgg45 <- qplot(data$pgg45, data$lpsa, xlab="Porcentagem escore Gleason (4 ou 5)", ylab="psa ng/ml")

#Coeficientes de correla��o(com gr�fico ajustado para melhor visualiza��o)
cex.before <- par("cex")
par(cex = 0.7)
corrplot(cor(data), insig = "blank", method = "color",
         addCoef.col="grey", 
         order = "AOE", tl.cex = 1/par("cex"),
         cl.cex = 1/par("cex"), addCoefasPercent = TRUE)
par(cex = cex.before)

#Gr�ficos organizados lado a lado
grid.arrange(graph_vol, graph_weight, graph_age, graph_bph, ncol=2, nrow=2)
grid.arrange(graph_svi, graph_lcp, graph_gleason, graph_pgg45, ncol=2, nrow=2)

#modelo de regress�o linear m�ltipla
multiple_regression_line <- lm(lpsa ~ lcavol+svi+lcp, data = data_training)

#Array de predi��es com os resultados vindos do modelo de regress�o
predictions <- predict(multiple_regression_line, select(data_test,lcavol, svi, lcp))

#dataframe com as predi��es
lm_predictions <- data.frame(pred = predictions, obs = data_test$lpsa)

#Gr�fico dos valores resultantes do modelo de regress�o e o valor observado do conjunto de dados teste
ggplot(lm_predictions, aes(x = pred, y = obs)) + geom_point(alpha = 0.5, position = position_jitter(width=0.2)) + geom_abline(colour = "blue") + ggtitle("Predi��o X psa")

#y - f(x)
trash = data_test$lpsa - predictions

#gr�fico dos res�duos
qplot(predictions, trash, ylab = "Res�duos", xlab = "Predi��es") + geom_hline(yintercept = 0, color='blue') + ggtitle("Predi��o x Res�duos")

RMSE(predictions, data_test$lpsa)

lasso <- train(lpsa ~ ., data=select(data_training, lcavol, lweight, age, lbph, svi, lcp, gleason, pgg45, lpsa), method='lasso', metric='RMSE',tuneLength=10)

plot(lasso)
plot(varImp(lasso), xlab = "Import�ncia")

lasso_predictions <- predict(lasso, select(data_test,lcavol, lweight, age, lbph, svi, lcp, gleason, pgg45))

la_predictions <- data.frame(pred = lasso_predictions, obs = data_test$lpsa)

ggplot(la_predictions, aes(x = pred, y = obs)) + geom_point(alpha = 0.5, position = position_jitter(width=0.2)) + geom_abline(colour = "blue") + ggtitle("Observados X Previstos (LASSO)")

#RMSE do modelo LASSO
RMSE(lasso_predictions, data_test$lpsa)

#comparando os modelos
compare <- lm_predictions
lm_predictions$model <- "RL"
la_predictions$model <- "LASSO"

compare <- rbind(lm_predictions, la_predictions)

ggplot(compare, aes(x = pred, y = obs)) + 
  geom_point(alpha = 0.5, position = position_jitter(width=0.2)) + 
  facet_grid(. ~ model) + 
  geom_abline() +
  ggtitle("Observado x Previs�o (valida��o)")
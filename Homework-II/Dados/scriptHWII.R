#Verificando se os pacotes necessários estão instalados, e instalando-os caso contrário

required_packages <- c("e1071", "AppliedPredictiveModeling", "caret", "corrplot", "ggplot2", "elasticnet")
installed_packages <- rownames(installed.packages())
for (requirement in required_packages){
  if(!(requirement %in% installed_packages)){
    install.packages(requirement)
  }
}

#Carregando o arquivo auxliliar
source("utils.R")

library(e1071)
library(AppliedPredictiveModeling)
library(caret)
library(corrplot)
library(ggplot2)
data(solubility)

#Criando uma pasta para salvar as figuras geradas
figures_path = file.path(getwd(),"figures")

#Selecionando os preditores que não são "FP(Fingerprints)"
notFP<-!grepl("FP",names(solTrainX))
contPredTrain<-solTrainX[,notFP]+1
contPredTest<-solTestX[,notFP]+1

#Gerando os Histogramas para cada preditor antes de remover a skewness
make_histograms(data=contPredTrain, file_path=file.path(figures_path, "before"))

pp<-preProcess(contPredTrain,method="BoxCox")
contPredTrain<-predict(pp,contPredTrain)
contPredTest<-predict(pp,contPredTest)
dataTrans<-rbind(contPredTrain,contPredTest)
correlation<-cor(dataTrans)
corrplot(correlation,method="square",type="lower",order="FPC")

#Gerando os Histogramas para cada preditor depois de remover a skewness.
make_histograms(data=contPredTrain, file_path=file.path(figures_path, "after"))


#Step 2

library(elasticnet)

trainingSet = cbind(solTrainX[,!notFP], contPredTrain)
testSet = cbind(solTestX[,!notFP], contPredTest)

ridgeModel <- enet(x = as.matrix(trainingSet), y = solTrainY,lambda = 0.001)
ridgePred <- predict(ridgeModel, newx = as.matrix(trainingSet),s = 1, mode = "fraction",type = "fit")
head(ridgePred$fit)

ctrl <- trainControl(method = "cv", number = 10)
ridgeGrid <- data.frame(.lambda = seq(0, .1, length = 10))
set.seed(100)
ridgeRegFit <- train(trainingSet, solTrainY,method = "ridge", tuneGrid = ridgeGrid,trControl = ctrl,preProc = c("center", "scale"))

newRidgePred <- predict(ridgeRegFit, testSet)
ridgeValues <- data.frame(obs = solTestY, pred = newRidgePred)

plot(ridgeRegFit$results$lambda, ridgeRegFit$results$RMSE, xlab = "Penalty", ylab = "RMSE (Cross Validation)")
lines(ridgeRegFit$results$lambda, ridgeRegFit$results$RMSE)
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
make_histograms(data=contPredTrain, file_path=file.path(figures_path, "beforeBoxCox"))

pp<-preProcess(contPredTrain,method="BoxCox")
contPredTrain<-predict(pp,contPredTrain)
contPredTest<-predict(pp,contPredTest)
dataTrans<-rbind(contPredTrain,contPredTest)
correlation<-cor(dataTrans)
corrplot(correlation,method="square",type="lower",order="FPC")

#Gerando os Histogramas para cada preditor depois de remover a skewness.
make_histograms(data=contPredTrain, file_path=file.path(figures_path, "afterBoxCox"))

#Executando o PCA
X <- contPredTrain

scaledX <- scale(X)
corr.mat <- cor(scaledX)
cov.eig <- eigen(corr.mat)
feature.vector1 <- as.matrix(cov.eig$vectors[,1],ncol=1)
feature.vector2 <- as.matrix(cov.eig$vectors[,c(1,2)],ncol=2)
final1 <- t(feature.vector1) %*% t(scaledX)
final2<- t(feature.vector2) %*% t(scaledX)

PCs = data.frame(-final2[1,],-final2[2,],Type)
colnames(PCs)<-c("PC1", "PC2", "Type")
ggplot(data = PCs, aes(x=PC1, y=PC2)) + geom_point(aes(col=Type))


#Step 1

trainingData <- cbind(solTrainX[,!notFP], contPredTrain)
trainingData$Solubility <- solTrainY

lmFitAllPredictors <- lm(Solubility ~ ., data = trainingData)

lmPred1 <- predict(lmFitAllPredictors, solTestXtrans)
head(lmPred1)

lmValues1 <- data.frame(obs = solTestY, pred = lmPred1)
defaultSummary(lmValues1)

plot(lmValues1)

#CV 5
ctrl <- trainControl(method = "cv", number = 5)
lmFit1 <- train(x = solTrainXtrans, y = solTrainY,method = "lm", trControl = ctrl)
lmFit1

#CV 10
ctrl <- trainControl(method = "cv", number = 10)
lmFit1 <- train(x = solTrainXtrans, y = solTrainY,method = "lm", trControl = ctrl)
lmFit1

xyplot(solTrainY ~ predict(lmFit1),
       ## plot the points (type = 'p') and a background grid ('g')
       type = c("p", "g"),
       xlab = "Predicted", ylab = "Observed")

xyplot(resid(lmFit1) ~ predict(lmFit1),
       type = c("p", "g"),
       xlab = "Predicted", ylab = "Residuals")


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


#Step 3
ctrl <- trainControl(method = "cv", number = 10)
plsTune <- train(trainingSet , solTrainY ,method = "pls",tuneLength = 20,trControl = ctrl)
plsTune

plot(plsTune)

plsPred <- predict(plsTune , testSet , ncomp = 12)
PLSdf <- data.frame(pred = plsPred , obs = solTestY)
defaultSummary(PLSdf)
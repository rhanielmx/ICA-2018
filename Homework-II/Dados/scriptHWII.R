#Verificando se os pacotes necessários estão instalados, e instalando-os caso contrário

required_packages <- c("e1071", "AppliedPredictiveModeling", "caret", "corrplot")
installed_packages <- rownames(installed.packages())
for (requirement in required_packages){
  if(!(requirement %in% installed_packages)){
    install.packages(requirement)
  }
}

library(e1071)
library(AppliedPredictiveModeling)
library(caret)
library(corrplot)
data(solubility)

notFP<-names(solTrainX)[!grepl("FP",names(solTrainX))]
contPredTrain<-solTrainX[,notFP]+1
contPredTest<-solTestX[,notFP]+1
pp<-preProcess(contPredTrain,method="BoxCox")
contPredTrain<-predict(pp,contPredTrain)
contPredTest<-predict(pp,contPredTest)
dataTrans<-rbind(contPredTrain,contPredTest)
correlacao<-cor(dataTrans)
corrplot(correlacao,method="square",type="lower",order="FPC")
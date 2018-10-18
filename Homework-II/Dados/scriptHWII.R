#Verificando se os pacotes necessários estão instalados, e instalando-os caso contrário
source("util.R")

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

#Criando uma pasta para salvar as figuras geradas
figures_path = file.path(getwd(),"figures")

notFP<-names(solTrainX)[!grepl("FP",names(solTrainX))]
contPredTrain<-solTrainX[,notFP]+1
contPredTest<-solTestX[,notFP]+1

#Gerando os Histogramas para cada preditor antes de remover a skewness
make_histograms(data=contPredTrain, main_path=figures_path, sub_path = "before")

pp<-preProcess(contPredTrain,method="BoxCox")
contPredTrain<-predict(pp,contPredTrain)
contPredTest<-predict(pp,contPredTest)
dataTrans<-rbind(contPredTrain,contPredTest)
correlacao<-cor(dataTrans)
corrplot(correlacao,method="square",type="lower",order="FPC")

#Gerando os Histogramas para cada preditor depois de remover a skewness
make_histograms(data=contPredTrain, main_path=main_path, sub_path = "after")

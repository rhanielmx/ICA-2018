#Verificando se os pacotes necessÃ¡rios estÃ£o instalados, e instalando-os caso contrÃ¡rio

required_packages <- c("e1071", "AppliedPredictiveModeling", "caret", "corrplot", "ggplot2", "elasticnet")
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
library(ggplot2)

#Criando uma pasta para salvar as figuras geradas
figures_path = file.path(getwd(),"figures")

load("/home/romulofff/Documentos/UFC/Disciplinas/s6/ica/ICA-2018/Homework-III/dados/grantData_hw3/grantData_HW3.RData")
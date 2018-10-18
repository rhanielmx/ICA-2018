required_packages <- c("e1071", "AppliedPredictiveModeling", "caret")
installed_packages <- rownames(installed.packages())
for (requirement in required_packages){
  if(!(requirement %in% installed_packages)){
    install.packages(requirement)
  }
}

library(e1071)
library(AppliedPredictiveModeling)
library(caret)
data(solubility)

binary_predictors <- grep('FP', names(solTrainX))
trainingData <- solTrainX[-binary_predictors]
testData <- solTestX[-binary_predictors]

library(e1071)

#AJEITAR O DIRETÓRIO PRA CARREGAR DIREITIN
#load("C:/Users/Marcus Vinicius/Downloads/TI0077_HW3_assignment/grantData_hw3/grantData_HW3.RData")
#reducedSet <- read.csv(file="C:/Users/Marcus Vinicius/Downloads/TI0077_HW3_assignment/grantData_hw3/reducedSet.csv", header=TRUE)

#Reduzindo os dados apenas para os preditores que interessam e a coluna de Resultados
dados_reduzidos <- testing[c(t(reducedSet), 'Class')]

#Criamos o modelo com a SVM utilizando os dados reduzidos
svm_modelo <- svm(Class ~ ., data=dados_reduzidos, scale=FALSE)

#Separamos apenas os preditores
no_class <- testing[ ,t(reducedSet)]

#Predizemos utilizando o modelo desenvolvido com os preditores
result <- predict(svm_modelo, no_class)

#Matriz de confusão com nossos resultados de predição e com os resultados que já temos
table(result, dados_reduzidos$Class)
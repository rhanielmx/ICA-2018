library(e1071)

#AJEITAR O DIRET?RIO PRA CARREGAR DIREITIN
load("/home/marcusv/HW/ICA-2018/Homework-III/dados/grantData_hw3/grantData_HW3.RData")
reducedSet <- read.csv(file="/home/marcusv/HW/ICA-2018/Homework-III/dados/grantData_hw3/reducedSet.csv", header=TRUE)

#Reduzindo os dados apenas para os preditores que interessam e a coluna de Resultados
dados_reduzidos <- testing[c(t(reducedSet), 'Class')]

#Criamos o modelo com a SVM utilizando os dados reduzidos
svm_modelo <- svm(Class ~ ., data=dados_reduzidos, scale=FALSE)

#Separamos apenas os preditores
no_class <- testing[ ,t(reducedSet)]

#Predizemos utilizando o modelo desenvolvido com os preditores
result <- predict(svm_modelo, no_class)

#Matriz de confus?o com nossos resultados de predi??o e com os resultados que j? temos
table(result, dados_reduzidos$Class)

#TUNANDO O MODELO
#Tunando utilizando o kernel radial e especificando uma lista de valores Gamma e C
tune_svm <- tune.svm(Class ~ ., data=dados_reduzidos, kernel="radial", gamma = 10^(-1:-3), cost = 10^(1:3), scale = FALSE)

#Vemos os valores Gamma e C que geram melhor resultado
summary(tune_svm)

#Elaboramos um Modelo utilizando os melhores valores de C e Gamma
svm_modelo_tunado <- svm(Class ~ ., data=dados_reduzidos, kernel="radial", cost=100, gamma=0.001, scale=FALSE)

#Predizemos os valores usando o modelo tunado
result_tunado <- predict(svm_modelo_tunado, no_class)

#Matriz de confusão com os nossos resultados de predição e com os resultados que já temos
table(result_tunado, dados_reduzidos$Class)
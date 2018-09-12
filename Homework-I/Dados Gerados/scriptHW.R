#Verificando se os pacotes necessários já estão instalados e instalando-os em caso contrário.
required_packages <- c("e1071", "mlbench", "corrplot", "ggplot2","ggfortify", "GGally", "dplyr")
installed_packages <- rownames(installed.packages())
for (requirement in required_packages){
  if(!(requirement %in% installed_packages)){
    install.packages(requirement)
  }
}

#Carregando os pacotes
library(dplyr)
library(e1071)
library(mlbench)
library(corrplot)
library(ggplot2)
library(ggfortify)
library(GGally)

#Função retirada da internet
#Verifica a existência de um diretório e o cria caso ainda não exista

mkdirs <- function(dir){
  if(!file.exists(dir)){
    mkdirs(dirname(dir))
    dir.create(dir)
  }
}

#Carregando o Dataset
data(Glass)
attach(Glass)

data <- Glass[,-10]
elements <- colnames(data)

#Item 1

#Criando o caminho, caso ainda não exista, para os arquivos serem salvos
main_path = file.path(getwd(),"figures")
file_path=file.path(main_path,"unconditional")
mkdirs(file_path)

#Salvando os histogramas não condicionais na pasta correspondete
for(element in elements){
    file_name=paste0("Histogram_of_", element)
    png(file.path(file_path, paste0(file_name,".png")))
    suppressMessages(print(ggplot(data = data, aes(data[[element]])) + xlab(element) + ggtitle(paste("Histogram of",element)) + theme(plot.title = element_text(size = 24, face = "bold")) + geom_histogram(colour="black", fill="gray")))
    dev.off()
}

#Calculando as estatísticas descritivas do conjunto de dados
mean_vec <- mapply(mean, data) %>% round(6)
std_vec <- mapply(sd, data) %>% round(6)
skewness_vec <- mapply(skewness, data) %>% round(6)

#Salvando as estatísticas em um arquivo .csv
statistics <- data.frame(rbind(mean_vec,std_vec,skewness_vec))
rownames(statistics) <- c("Mean", "SD", "Skewness")
write.csv(statistics,file.path(file_path, "Unconditional_Statistics.csv"),row.names = TRUE)


#Item 2

for(class in unique(Type)){
  #Criando as pastas referentes a cada classe
  file_path=file.path(main_path,paste0("class-conditional/Class",class))
  mkdirs(file_path)
  
  #Separando os dados por classe e calculando suas estatísticas descritivas
  df <- data[Type==class,]
  mean_vec <- mapply(mean, df) %>% round(6)
  std_vec <- mapply(sd, df) %>% round(6)
  skewness_vec <- mapply(skewness, df) %>% round(6)
  
  #Salvando essas estatísticas em um arquivo .csv
  cc_statistics <- data.frame(rbind(mean_vec,std_vec,skewness_vec))
  rownames(cc_statistics) <- c("Mean", "SD", "Skewness")
  write.csv(cc_statistics, file.path(file_path, paste0("Conditional_Statistics-Class",class,".csv")),row.names = FALSE)
  
  #Salvando os histogramas condicionados às classes nas suas pastas correspondentes
  for(element in elements){
        file_name=paste0("Histogram_of_", element)
        png(file.path(file_path, paste0(file_name,"-Class",class,".png")))
        suppressMessages(print(ggplot(data = df, aes(df[[element]])) + xlab(element) + ggtitle(paste("Histogram of",element)) + theme(plot.title = element_text(size = 24, face = "bold")) + geom_histogram(colour="black", fill="gray")))
        dev.off()
  }
}

#Item 3

#Plots dois a dois dos preditores do conjunto de dados

p <- ggpairs(Glass[-10], aes(colour = Type, alpha = 0.4), upper = list(continuous = "points", combo = "dot_no_facet"),
        diag = list(continuous = "barDiag"), legend=3)
p <- p + theme(axis.text.x = element_text(angle = 50, hjust = 1))
file_name = "figures/pairsPlot.png"
png(file_name)
p
dev.off()

#Criando a matriz de correlação e salvando os valores e seu plot
corr.mat <- cor(Glass[-10])
write.csv(corr.mat, "figures/correlationMatrix.csv",row.names = FALSE)

file_name = "figures/correlationMatrix.png"
png(file_name)
corrplot(corr=corr.mat, method = "square")
dev.off()

#Item 4

#Item 4

#Plot dos dados transformados nos componentes principais e os autovetores associados a cada preditor original
#Representação bidimensional com os dois primeiros componentes 

pcaObject <- prcomp(Glass[,1:9],center = TRUE, scale. = TRUE)
file_name = "figures/pcaPlot.png"
png(file_name)
autoplot(pcaObject,data=Glass,colour ='Type', loadings = TRUE, loadings.colour = 'blue', loadings.label = TRUE, loadings.label.size = 3)
dev.off()

#Plot da variância de cada componente

file_name = "figures/varPlot.png"
png(file_name)
plot(pcaObject, main="Variância dos Componentes Principais")
dev.off()
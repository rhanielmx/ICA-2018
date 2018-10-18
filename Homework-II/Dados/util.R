#Função que gera as pastas
mkdirs <- function(dir){
  if(!file.exists(dir)){
    mkdirs(dirname(dir))
    dir.create(dir)
  }
}

#Função que gera os histogramas a partir de um data.frame
make_histograms <- function(data, main_path, sub_path){
  hist_names=colnames(data)
  file_path=file.path(main_path, sub_path)
  mkdirs(file_path)
  
  for(name in hist_names){
    file_name=paste0("Histogram_of_", name)
    png(file.path(file_path, paste0(file_name,".png")))
    suppressMessages(print(ggplot(data = data, aes(data[[name]])) + xlab(name) + ggtitle(paste("Histogram of",name)) + theme(plot.title = element_text(size = 24, face = "bold")) + geom_histogram(colour="black", fill="gray")))
    dev.off()
  }
}
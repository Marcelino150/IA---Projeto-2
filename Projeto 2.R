install.packages("rgl")
install.packages("purrr")
library(rgl)
library(purrr)

#Importa dataset do arquivo breast-cancer-wisconsin.data
importDf <- function() {
  
  #Importação
  dfImported <- read.csv("breast-cancer-wisconsin.data", sep = ",")
  dfcolNames <- c("Sample code number",
                  "Clump Thickness",
                  "Uniformity of Cell Size",
                  "Uniformity of Cell Shape",
                  "Marginal Adhesion",
                  "Single Epithelial Cell Size",
                  "Bare Nuclei",
                  "Bland Chromatin",
                  "Normal Nucleoli",
                  "Mitoses",
                  "Class");
  #Definição dos nomes das colunas
  colnames(dfImported)<-dfcolNames
  
  #Procura linhas com dados inválidos
  todelete <- which(dfImported[,7] == '?')
  #Exclui linhas com dados inválidos
  dfImported <- dfImported[-todelete,];
  
  dfImported <- dfImported[,-1]
  classes <- dfImported[,10]
  
  return(dfImported);
}

#Plotagem de gráfico em 3 dimensões
plot3DChart <- function(df) {
  
  #Separação de classe por cores (Azul = Benigno(2) | Vermelhor = Maligno(4))
  mycolors <- c('white', 'blue','white','red')
  color <- mycolors[as.numeric(df$Class)]
  
  plot3d( 
    x=df$`Clump Thickness`, y=df$`Uniformity of Cell Size`, z=df$`Uniformity of Cell Shape`, 
    col = color, 
    type = 's', 
    radius = .2,
    xlab="Clump Thickness", ylab="Uniformity of Cell Size", zlab="Uniformity of Cell Shape")
}

kmeansfunc <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

#Plotagem do gráfico de cotovelo
elbowChart <- function(k){

  k.values <- 1:k
  
  wss_values <- map(k.values, kmeansfunc)
  
  plot(k.values, wss_values,
       type="b", pch = 19, frame = FALSE, 
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares");
}

#Calculo de acurácia
calcAcuracy <- function(df){
  
  hit <- 0;
  
  #Comparação das classes do dataset com a separação em clusters
  #A separação em clusters pode alternar em diferentes execuções, trocando em qual cluster um elemento é colocado
  for( i in 1:nrow(df[10])){
    
    if(clusters$cluster[i] == 2 && df[i,10] == 2){
      hit = hit + 1;
    }
    else if(clusters$cluster[i] == 1 && df[i,10] == 4){
      hit = hit + 1
    }
  }
  
  acuracy <- (hit/nrow(df[10]))*100;
  
  return(acuracy);
}

df <- importDf();
plot3DChart(df);
set.seed(123);
elbowChart(15);
clusters <- kmeans(df,2);
calcAcuracy(df);


install.packages("rgl")
install.packages("purrr")
library(rgl)
library(purrr)

#Importa dataset do arquivo breast-cancer-wisconsin.data
importDf <- function() {
  
  #Importa��o
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
  #Defini��o dos nomes das colunas
  colnames(dfImported)<-dfcolNames
  
  #Procura linhas com dados inv�lidos
  todelete <- which(dfImported[,7] == '?')
  #Exclui linhas com dados inv�lidos
  dfImported <- dfImported[-todelete,];
  
  dfImported <- dfImported[,-1]
  classes <- dfImported[,10]
  
  return(dfImported);
}

#Plotagem de gr�fico em 3 dimens�es
plot3DChart <- function(df) {
  
  #Separa��o de classe por cores (Azul = Benigno(2) | Vermelhor = Maligno(4))
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

#Plotagem do gr�fico de cotovelo
elbowChart <- function(k){

  k.values <- 1:k
  
  wss_values <- map(k.values, kmeansfunc)
  
  plot(k.values, wss_values,
       type="b", pch = 19, frame = FALSE, 
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares");
}

#Calculo de acur�cia
calcAcuracy <- function(df){
  
  hit <- 0;
  
  #Compara��o das classes do dataset com a separa��o em clusters
  #A separa��o em clusters pode alternar em diferentes execu��es, trocando em qual cluster um elemento � colocado
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


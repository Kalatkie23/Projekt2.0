
#załadowanie bibliotek
library(dplyr)
library(tidyr)
library(ggplot2)
library(tseries)





rmix<- function(n,family_1,par_1,family_2,par_2,p){
  #każdy indeks w próbce o długości n bedzie rozlosowany z prawdopodobieństwem p
  index<-sample(0:1,replace = T,prob = c(p,1-p),size = n)
  
  
  #obliczenie długość wektorów
  length_1<- sum(length(which(index==0)))
  length_2<-sum(length(which(index==1)))
  
  #stworzenie nazwy funkcji tworzącej próbke 
  function_1<- get(paste('r',family_1, sep = ""))
  function_2<- get(paste('r',family_2, sep = ""))
  
  if(length(par_1)==1){
    sample_1<-function_1(length_1,par_1[1])
    
  }
  else if(length(par_1)==2){
    sample_1<-function_1(length_1,par_1[1],par_1[2])
  }
  
  if(length(par_2)==1){
    sample_2<-function_2(length_2,par_2[1])
    
  }
  else if(length(par_2)==2){
    sample_2<-function_2(length_2,par_2[1],par_2[2])
  }
  
  c(sample_1,sample_2)
}


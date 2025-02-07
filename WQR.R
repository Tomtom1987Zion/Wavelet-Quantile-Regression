
## Wavelet quantile regression

## by Tomiwa Sunday Adebayo & Oktay Ozkan

## Twaikline@gmail.com

## https://doi.org/10.1016/j.jclepro.2023.140321

## Investigating the influence of socioeconomic conditions, renewable energy and eco-innovation on environmental degradation in the United States: A wavelet quantile-based analysis


rm(list=ls(all=TRUE))

#Set working directory
setwd("C:/Users/Admin/Desktop/BRAINBOX/NEW CODESS")

# Load packages

library(tidyquant) #for stock data
library(tidyverse)  # for data analysis
library(crypto2)    #for cryptocurrency download
library(bootstrap)  #for bootstrapping
library(waveslim)   #for wavelet
library(wranglR) #install from Github
library(lattice)   #for plotting
library(viridisLite) #for plotting
library(RColorBrewer) #for plotting
library(QCSIS) #for quantile estimation
library("readxl")
library(quantreg)
library(wesanderson)

# Loading DATA, where it is located and name of file.
Data<- read_excel("DATA.xlsx")
attach(Data)

DEP <- DEP
IND <- IND

## To install WranglR
## install.packages("remotes")
## remotes::install_github("phively/wranglR")
# all other packages can be installed from CRAN normally
#estimation
# you can change wavelet parameters in d1 and d2 as you require



tau <- seq(0.05,0.95,0.05)
d1=as.data.frame(mra(DEP, wf = "la8", J = 5, method = "modwt", boundary = "periodic"))
d2=as.data.frame(mra(IND, wf = "la8", J = 5, method = "modwt", boundary = "periodic"))
cols <- intersect(colnames(d1), colnames(d2))
res <- lapply(cols, function(x) {
  model <- coef(rq(d1[, x] ~ d2[, x], tau))
  result <- list(tau = tau, rho = model[2,])
  return(result)
})
names(res) <- cols
op=cbind.data.frame(tau,ListExtract(res,"rho"))

## Plotting
plot=op[,c(-1,-10)] 
plot=as.matrix(plot)
row.names(plot)=tau 

a <- plot[,c(1,2,3)]

## you can use any results you want to use. for example if you want to use results of 1,2,3 use this: a <- plot[,c(1,2,3)] and arrange the following colnames as colnames(a)=c("D1", "D2", "D3"). you can also change colnames as colnames(a)=c("Short", "Medium", "Long")

colnames(a)=c("Short", "Medium", "Long")

pallete <- colorRampPalette(c("darkseagreen1","orange","red"))(20)

levelplot(a, col.regions = pallete,xlab="Quantiles",ylab="Periods",main="") 




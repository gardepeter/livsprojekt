#library(Rcpp)
#library(RcppArmadillo)
library(ggplot2)
library(readr)
library(tidyverse)
library(gridExtra)
library(tikzDevice)
library(dplyr)

#source("logic/helperFunctions.R")
#Rcpp::sourceCpp("logic/cpp/helperFunctions.cpp")

#Plot of probabilities
probabilities <- read_csv("probabilities.csv")
probabilities$age_in_month<-c(1:600)

probabilities_0<-probabilities[,c(1:3,10)]
probabilities_1<-probabilities[,c(4:6,10)]

probabilitis_0_long<-pivot_longer(probabilities,c(p_00,p_01,p_02),
                  names_to="transitions",
                  values_to="probabilities")

probabilitis_1_long<-pivot_longer(probabilities,c(p_10,p_11,p_12),
                                  names_to="transitions",
                                  values_to="probabilities")

grid.arrange(
  ggplot(probabilitis_0_long, aes(x = age_in_month, y = probabilities, color = transitions)) +
  geom_line()+
    labs(title="From active state")+
    ylab(bquote(p[ij]))+
    theme(legend.position="bottom",
          legend.title=element_blank()),
  ggplot(probabilitis_1_long, aes(x = age_in_month, y = probabilities, color = transitions)) +
    geom_line()+
    labs(title="From disabled state")+
    theme(axis.title.y=element_blank(),
          legend.position="bottom",
          legend.title=element_blank())+
    scale_fill_discrete(name = "transitions", labels = c("A", "B", "C")),
  nrow=1
)



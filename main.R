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

probabilities_0_long <- pivot_longer(probabilities_0, c(p_00, p_01, p_02),
                                     names_to = "transitions",
                                     values_to = "probabilities")

probabilities_1_long <- pivot_longer(probabilities_1, c(p_10, p_11, p_12),
                                     names_to = "transitions",
                                     values_to = "probabilities")

legend_labels <- c(expression(paste("p"["00"])), 
                   expression(paste("p"["01"])),
                   expression(paste("p"["02"])),
                   expression(paste("p"[10])),
                   expression(paste("p"[11])),
                   expression(paste("p"[12])))

plot_0 <- ggplot(probabilities_0_long, aes(x = age_in_month, y = probabilities, color = transitions)) +
  geom_line() +
  labs(title = "From active state") +
  ylab("Probabilities") +
  scale_color_discrete(labels = legend_labels[1:3]) +  
  theme(legend.position = "bottom",
        legend.title = element_blank())

plot_1 <- ggplot(probabilities_1_long, aes(x = age_in_month, y = probabilities, color = transitions)) +
  geom_line() +
  labs(title = "From disabled state") +
  theme(axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) +
  scale_color_discrete(labels = legend_labels[4:6])  

grid.arrange(plot_0, plot_1, nrow = 1)



#Betalingfunktion. Opdater senere?
b_1 <- function(x) {
  return(1500)
}

#Gammel cashflow
#cashflow<-function(t,s){
# result<-(b_1(s+probabilities$age_in_month)*probabilities$p_11)-
#   (b_1(t+probabilities$age_in_month)*probabilities$p_11)
#  return(result)
#}

#Ny cashflow
#Husk at ændre age in month til rigtig. 1 timestep frem
#cashflow<-function(s,x){
#  result<-probabilities[x,2]*(b_1(x+s))
#  return(result)
#}

probabilities_cashflow<-mutate(probabilities,cashflow=p_11*10000)

ggplot(probabilities_cashflow,aes(x=age_in_month,y=cashflow))+
  geom_line()

#Reserve
forward_rates <- read_delim("forward rates.csv", 
                            delim = ";", escape_double = FALSE, col_types = cols(år = col_integer()), 
                            trim_ws = TRUE)
#ZCB_P
reserve<-function(t,n){
  0.5*(
    exp(-forward_rates[t,2])*probabilities_cashflow[,11])
  
}

f<-function(s){
  exp(-forward_rates[s,2])*probabilities_cashflow[s,11]
  }

trapetz<-function(s){
  for(i in 1:s){
  sum<-0.5*(exp(-forward_rates[1,2])*probabilities_cashflow[i+12,11]+
         exp(-forward_rates[1,2])*probabilities_cashflow[i+12,11])
  sum<-sum+sum
  }
  return(sum)
}

trapetz(10)

for(i in 1:10){
  sum<-0.5*(exp(-forward_rates[1,2])*probabilities_cashflow[i+12,11]+
              exp(-as.numeric(unlist(forward_rates[1,2])))*probabilities_cashflow[i+12,11])
  sum<-sum+sum
}





forward_rates

library(pracma)

interp_rate <- function(t) {
  interp1(forward_rates$år, forward$rate, t)
}

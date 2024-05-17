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


probabilities <- read_csv("data/probabilities.csv")
probabilities$time<-c(1:600)

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

plot_0 <- ggplot(probabilities_0_long, aes(x = time, y = probabilities, color = transitions)) +
  geom_line() +
  labs(title = "From active state") +
  ylab("Probabilities") +
  scale_color_discrete(labels = legend_labels[1:3]) +  
  theme(legend.position = "bottom",
        legend.title = element_blank())

plot_1 <- ggplot(probabilities_1_long, aes(x = time, y = probabilities, color = transitions)) +
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

probabilities_cashflow<-mutate(probabilities,cashflow=p_11*10000)

ggplot(probabilities_cashflow,aes(x=time,y=cashflow))+
  geom_line()

#Reserve
forward_rates <- read_delim("forward rates.csv", 
                            delim = ";", escape_double = FALSE, col_types = cols(år = col_integer()), 
                            trim_ws = TRUE)

f<-approxfun(forward_rates$år , forward_rates$rate)
f(2)

#Bemærk at renten skal være i 2. kolonne

exponential<-function(t,s,n){
  delta_x<-(s-t)/n

    sum<-forward_rates[floor(t),2]#rente i 2. kolonne
  for(i in 1:n-1){
  sum<-sum+2*forward_rates[floor(i*(s-t)/n+t),2] #Tager floor for at undgå 
  }
  sum<-forward_rates[floor(s),2]
  
  T_n<-delta_x*0.5*(sum)
  
  exponential_output<- exp(-T_n)
  
  return(exponential_output)
}



reserve<-function(t,n,N){
  delta_x<-(N-t)/n
  
  sum<-exponential(t,t,n)*probabilities[floor(t),10]*10000 #Time column
    for(i in 1:n-1){
      sum<-sum+2*exponential(t,i*(N-t)/n+t,n)*probabilities[floor(i*(N-t)/n+t),10]*10000
      #Tager floor, da vi har en stepfunktion og ønsker vntre endepunkt (da det er der vi har data)
    }
  sum<- exponential(t,N,n)*probabilities[floor(N),10]*10000
  reserve_output<- delta_x*0.5*sum
  
  return(reserve_output)
}

reserve(2,500,100)
t_values <- seq(0, 100, length.out = 100)
reserve_values <- sapply(t_values, function(t) reserve(t, n, N))




library(pracma)

interp_rate <- function(t) {
  interp1(forward_rates$år, forward$rate, t)
}

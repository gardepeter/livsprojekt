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
forward_rates <- read_delim("data/forward rates.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

#Kontinuert rate funktion. 
rate<-approxfun(forward_rates$år , forward_rates$rate)

#Når denne rate funktion bruges, skal man være opmærksom på, at det er års-tidsenhed,
#så hvis du vil finde raten 30 måneder frem, skal du siger rate(30/12)


#Følgende er lavet ud fra månedsniveau (derfor vi dividerer med 12)
exponential<-function(t,s,n){
  delta_x<-(s-t)/n

    sum<-rate(t/12+1) #t/12 i raten for at få på års-niveau
  for(i in 1:n-1){
  sum<-sum+2*rate((i*(s-t)/n+t)/12+1) # +1, da vi skal bruge den forward rate der svarer til 1 år efter tid t
  }
  sum<-sum+rate(s/12+1)
  
  T_n<-delta_x*0.5*(sum)
  
  exponential_output<- exp(-T_n)
  
  return(exponential_output)
}

reserve<-function(t,n,N){
  delta_x<-(N-t)/n
  
  sum<-exponential(t,t,n)*probabilities[floor(t),2]*10000 #p_01 column
  
    for(i in 1:n-1){
      sum<-sum+2*exponential(t,i*(N-t)/n+t,n)*probabilities[floor(i*(N-t)/n+t),2]*10000
      #Tager floor, da vi har en stepfunktion og ønsker venstre endepunkt
    }
  
  sum<- exponential(t,N,n)*probabilities[floor(N),2]*10000+sum
  
  reserve_output<- delta_x*0.5*sum
  
  return(reserve_output)
}



reserve(1,1000,120)




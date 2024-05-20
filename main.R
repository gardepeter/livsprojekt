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

#-------------------------------------------------------------------------------
#Plot of probabilities
#-------------------------------------------------------------------------------
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
#------------------------------------------------------------------------------
#Beregning af reserve
#------------------------------------------------------------------------------
forward_rates <- read_delim("data/forward rates.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
colnames(forward_rates)<-c("year","rate")

#Kontinuert rate funktion. 
rate<-approxfun(forward_rates$year , forward_rates$rate)

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

#-------------------------------------------------------------------------------
#Funktionerne med generelt cashflow og rentekurve. Sørg for at rentekurverne ser ens ud i opbygning
#-------------------------------------------------------------------------------

#cashflow data
cashflow_data<-tibble("cashflow"=((rep(10000,600)*probabilities[,2])),
                      time=c(1:600))

exponential_rate<-function(t,s,n,rentekurve){
  rate<-approxfun(rentekurve$year , rentekurve$rate)
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

reserve_cashflow<-function(t,n,N,cashflow,rentekurve){
  delta_x<-(N-t)/n
  
  sum<-exponential_rate(t,t,n,rentekurve)*cashflow[t,1] #p_01 column
  
  for(i in 1:n-1){
    sum<-sum+2*exponential_rate(t,i*(N-t)/n+t,n,rentekurve)*cashflow[floor(i*(N-t)/n+t),1]
    #Tager floor, da vi har en stepfunktion og ønsker venstre endepunkt
  }
  
  sum<- exponential_rate(t,N,n,rentekurve)*cashflow[floor(N),1]+sum
  
  reserve_output<- delta_x*0.5*sum
  
  return(reserve_output)
}

reserve_cashflow(1,1000,120,cashflow_data,forward_rates)

reserve(1,1000,120)

#-------------------------------------------------------------------------------
#Funktionerne i semi-markov set-up
#-------------------------------------------------------------------------------
#... mangler data set-up



#-------------------------------------------------------------------------------
#Evaluering af reserve
#-------------------------------------------------------------------------------
unitCashflows <- read_csv("data/unitCashflows.csv")
spot_rate <- read_delim("data/FT RFR med VA pr. 16. maj.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
spot_rate<-na.omit(spot_rate)

reserve_cashflow(1,1000,150,unitCashflows[,2],forward_rates)
reserve_cashflow(1,1000,500,unitCashflows[,3],spot_rate)

exponential_rate(1,2,1000,forward_rates)
exponential_rate(1,2,1000,spot_rate)
  

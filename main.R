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

#-------------------------------------------------------------------------------
#Plot of unitCashflows
#-------------------------------------------------------------------------------
unitCashflows <- read_csv("data/unitCashflows.csv")

plot = unitCashflows %>%
  pivot_longer(!age, values_to = "value", names_to = "cashflow")

new_labels <- c("cashflow_0" = "Expected cash flow (i=0)", "cashflow_1" = "Expected cash flow (i=1)")

ggplot(plot, aes(age, value))+
  geom_line() + 
  scale_y_continuous(breaks = seq(0, 1.2, length.out = 7), limits = c(0, 1.2)) +
  facet_grid(~cashflow, labeller = labeller(cashflow = new_labels))+
  theme(axis.title.y=element_blank())

#-------------------------------------------------------------------------------
#unitReserve calculation
#-------------------------------------------------------------------------------
#rates
spot_rate <- read_delim("data/FT RFR med VA pr. 16. maj.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
spot_rate<-na.omit(spot_rate)
spot_rate<-mutate(spot_rate, rate=rate/100)

#cashflow
unitCashflows_final = cbind(unitCashflows[,1] - 40, unitCashflows[,2])
colnames(unitCashflows_final)<-c("year", "cashflow")

#Sørg for at cashflow har c("year","cashflow") kolonner og rate har c("year","rate) kolonner
reserve<-function(maxtime,interest_rate,cashflow_data){
  rate_cont = approxfun(unlist(interest_rate[,1]), unlist(interest_rate[,2]))
  bond_price = sapply(seq(0, maxtime), function(x) exp(-integrate(rate_cont, 0, x)$val))
  bond_price_cont = approxfun(seq(0, maxtime), bond_price)
  output = approxfun(unlist(cashflow_data[,1]),unlist(cashflow_data[,2]) * bond_price_cont(unlist(cashflow_data[,1])))
  return( integrate(output, 0, maxtime)$val )
}

reserve(maxtime=25,interest_rate=spot_rate,cashflow_data=unitCashflows_final)

#-------------------------------------------------------------------------------
#DV01
#-------------------------------------------------------------------------------
spot_rate_DV01<-spot_rate
spot_rate_DV01$DV01<-spot_rate$rate+0.0001
reserve(maxtime=25,interest_rate=spot_rate_DV01[,c(1,3)],cashflow_data=unitCashflows_final)

true_reserve<-reserve(maxtime=25,interest_rate=spot_rate,cashflow_data=unitCashflows_final)
oneBasisPoint_reserve<-reserve(maxtime=25,interest_rate=spot_rate_DV01[,c(1,3)],cashflow_data=unitCashflows_final)

DV01<- -((oneBasisPoint_reserve-true_reserve)/0.0001)

sum(25*(spot_rate$rate-spot_rate_DV01$DV01)/(1+spot_rate$rate)*true_reserve)


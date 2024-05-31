library(tidyverse)
options(scipen = 99)
claim_pre <- read_csv("claims.csv")
#frequency <- read_csv("frequency.csv")
claim = claim_pre[, 1:2] / 1000

log_likelihood_lognormal = function(theta){
  return( -sum( log(dlnorm(claim$Clr, theta[1], theta[2])) - log(plnorm(claim$Dedr, theta[1], theta[2], lower.tail = F)) ) )
}

log_likelihood_weibull = function(theta){
  return( -sum( log(dweibull(claim$Clr, theta[1], theta[2])) - log(pweibull(claim$Dedr, theta[1], theta[2], lower.tail = F)) ) )
}

f_pareto = function(x, kappa, alpha){
  return( (alpha / (kappa + x))*(kappa / (kappa + x))^alpha )
}

F_bar_pareto = function(x, kappa, alpha){
  return( (kappa / (kappa + x))^alpha )
}

log_likelihood_pareto = function(theta){
  return( -sum( log(f_pareto(claim$Clr, theta[1], theta[2])) - log(F_bar_pareto(claim$Dedr, theta[1], theta[2])) ) )
}

params_lognormal = optim(c(mean(claim$Clr),sd( claim$Clr) ), log_likelihood_lognormal)
params_weibull = optim(c(0.7342342, 364.9229076 ), log_likelihood_weibull)
params_pareto = optim(c(2.005486, 0.1 ), log_likelihood_pareto)

mean_excess_lognormal = function(x, theta){
  return( (theta[2]^2 * x) / (log(x) - theta[1]) )
}

mean_excess_weibull = function(x, theta){
  return( (x^theta[1]) /  (theta[1] * theta[2]))
}

mean_excess_pareto = function(x, theta){
  return( (theta[1] + x)/(theta[2] - 1) )
}

plot = tibble(x = seq(0, 10000)) %>%
  mutate(mean_excess_lognormal = mean_excess_lognormal(x, params_lognormal$par),
         mean_excess_weibull = mean_excess_weibull(x, params_weibull$par),
         mean_excess_pareto = mean_excess_pareto(x, params_pareto$par)) %>%
  mutate(mean_excess_lognormal = if_else(row_number() < 200, 0, mean_excess_lognormal))

plot_edited = plot %>%
  pivot_longer(!x, names_to = "dist")

ggplot(plot_edited, aes(x, value, color = dist)) +
  geom_line()



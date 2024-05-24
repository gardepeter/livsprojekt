options(scipen = 99)
library(tidyverse)

reativationData = read_csv("./data/reactivationData.csv")
deathGivenDisabilityData = read_csv("./data/deathGivenDisabilityData.csv")

###################### POISSON REGRESSION ###############################

reactivationGLM = glm(O ~ Alder + offset(log(E)), family = poisson(), reativationData)
deathGivenDisabilityGLM = glm(O ~ Alder + offset(log(E)), family = poisson(), deathGivenDisabilityData)

startAge = 20
endAge = 70
mu = tibble(age = startAge:endAge, 
              mu10 = predict(reactivationGLM, tibble(Alder = startAge:endAge, E = 1), type = "response"),
              mu12 = predict(deathGivenDisabilityGLM, tibble(Alder = startAge:endAge, E = 1), type = "response"))
# write.csv(mu, "markovIntensitiesFromSemiMarkovData.csv", row.names = F)

###################### ANALISYS OF MODEL ###############################

plot_mu = mu %>%
  pivot_longer(!age, values_to = "Intensity", names_to = "States")

pointData = reativationData %>% 
  mutate(mu10 = O / E) %>%
  select(-c(O, E)) %>%
  left_join(
    deathGivenDisabilityData %>% 
      mutate(mu12 = O / E) %>%
      select(-c(O, E))
  ) %>%
  pivot_longer(!Alder, values_to = "Intensity", names_to = "States")

ggplot(plot_mu, aes(age, Intensity, color = States)) +
  geom_line() +
  geom_point(data = pointData, 
             mapping = aes(x = Alder, y = Intensity, color = States))
#Fint? Behøves analyse af modellen






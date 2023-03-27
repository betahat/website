library(tidyverse)
library(broom)



options(scipen = 999)

SaratogaHouses <- mosaicData::SaratogaHouses

# Bedroom model

SaratogaHouses %>%
  lm(data = ., price ~ bedrooms) %>%
  summary() %>%
  tidy()

# Bedroom and area model

SaratogaHouses %>%
  lm(data = ., price ~ bedrooms + livingArea) %>%
  summary() %>%
  tidy()

# graphs

# break into living area groups

SaratogaHouses$house_size <-
  cut_number(SaratogaHouses$livingArea, 4)

SaratogaHouses %>%
  ggplot(aes(
    x = livingArea,
    y = price,
    color = factor(bedrooms)
  )) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

SaratogaHouses %>%
  ggplot(aes(x = bedrooms, y = price, color = house_size)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

#########################################
# This code is from Lukbe et. al, 2020

# Chain example

set.seed(123456) # Reproducibility
n <- 1000 # Sample Size
learning <- rnorm(n)
knowing <- 5 * learning + rnorm(n)
understanding <- 3 * knowing + rnorm(n)

lm(understanding ~ learning) %>% tidy
lm(understanding ~ knowing) %>% tidy
lm(understanding ~ learning + knowing) %>% tidy

#############################################

# Fork

set.seed(123456) # Reproducibility
n <- 1000 # Sample Size
intelligence <- rnorm(n, mean = 100, sd = 15)
learning.time <- 200 - intelligence + rnorm(n)
test.score <- 0.5 * intelligence + 0.1 * learning.time + rnorm(n)

lm(test.score ~ learning.time) %>% tidy
lm(test.score ~ learning.time + intelligence) %>% tidy

#############################################

# Collider

set.seed(123456) # Reproducibility
n <- 1000 # Sample Size
network <- rnorm(n)
competence <- rnorm(n)
promotion <- ((network > 1) | (competence > 1))

lm(competence ~ network) %>% tidy
lm(competence ~ network + promotion) %>% tidy

lm(competence ~ network, subset = (promotion == 1)) %>% tidy

luck <- rbinom(n, size = 1, prob = 0.05)
promotion <- (1 - luck) * promotion + luck * (1 - promotion)

luck
promotion

library(plotly)
library(tidyverse)
library(dplyr)
library(ggplot2)

oj<- read.csv("C:\\UW\\AUT 2022\\ECON 487\\HW 2\\oj.csv")

ggplot(data = oj)+
  geom_boxplot(mapping = aes(y = oj$price, fill = oj$brand))+
  labs(y = "Price", fill = "Brands")


ggplot(data = oj)+
  geom_boxplot(mapping = aes(y = log(oj$price), fill = oj$brand))+
  labs(y = "Log Price", fill = "Brands")


tropicana_data <- oj %>%
  filter(brand == "tropicana")

minute.maid_data <- oj %>%
  filter(brand == "minute.maid")

dominicks_data <- oj %>%
  filter(brand == "dominicks")

ggplot(data = oj)+
  geom_point(mapping = aes(x = oj$logmove, y = log(oj$price), color = oj$brand))+
  labs(x = "Log quantity", y = "Log Price", color = "Brands")
  
model1 <- lm(logmove ~ log(oj$price), data = oj)
summary(model1)

model2 <- glm(logmove ~ log(oj$price) + factor(brand), data = oj)
summary(model2)


ggplot(data = oj)+
  geom_point(mapping = aes(x = factor(brand), y = feat, color = factor(brand)), position = "jitter", alpha = 0.4)

           
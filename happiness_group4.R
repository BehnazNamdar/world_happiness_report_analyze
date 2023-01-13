


# required packages: tidyverse, effectsize, parameters, performance
# bonus for using ggeffects and psych (pairs.panels)
# bonus for beautifulness of ggplot


library(tidyverse)
library(effectsize)
library(parameters)
library(performance)
library(ggeffects)
library(psych)



# read csv file with command ???read.csv??? or using import in menu
# data frame name: world-happiness-report
# group4

## 1
# remove NA data
# choose years (2011, 2012, 2013) and
# set a new data group with name: "happiness_2"
## 2
# add a new column which equals to "Positive.affect - Negative.affect"

happiness_2 <- read.csv("world-happiness-report.csv") %>%
  tidyr:: drop_na() %>%
  filter(between(year, 2011, 2013)) %>%
  mutate(difference.Positive.Negative.affect = Positive.affect - Negative.affect)


# just to be familier with data:
summary(happiness_2)
hist(happiness_2$Healthy.life.expectancy.at.birth)
str(happiness_2)


## 3
# using "parameters" package, fit a linear regression that
# predict variable "Healthy.life.expectancy.at.birth" from 2 other variables
# in the data frame.

fit <- lm(Healthy.life.expectancy.at.birth ~ Log.GDP.per.capita + Life.Ladder, data = happiness_2)
fit


# and report the parameters

parameters::model_parameters(fit)

# get a report from model:

summary(report::report(fit))
report::report(fit)

## 4
# using "performance" package, fit at least 3 models, choose the best one
# and describe why you choose it.

fit <- lm(Healthy.life.expectancy.at.birth ~ Log.GDP.per.capita + Life.Ladder , data = happiness_2)
fit1 <- lm(Healthy.life.expectancy.at.birth ~ Log.GDP.per.capita + Social.support , data = happiness_2)
fit2 <- lm(Healthy.life.expectancy.at.birth ~ Log.GDP.per.capita + Generosity , data = happiness_2)

performance::model_performance(fit)
performance::model_performance(fit1)
performance::model_performance(fit2)
# or
performance::compare_performance(fit,fit1,fit2)



## 5
# plot your model using "ggplot"
# (y ~ x) = (Healthy.life.expectancy.at.birth ~ Log.GDP.per.capita + Life.Ladder)



#to convert continuous variable to categorical variable:
happiness_2$categorized.Life.Ladder <- cut_number(happiness_2$Life.Ladder,n = 3)

ggplot(data = happiness_2 ,aes(x = Log.GDP.per.capita ,
                               y = Healthy.life.expectancy.at.birth ,
                               color = categorized.Life.Ladder)) +
  geom_jitter(alpha=0.2) +
  geom_smooth(method = "lm",fullrange = TRUE) +
  scale_y_continuous(name = "Healthy Life Expectancy at Birth") +
  scale_x_continuous(name = "GDP per Capita") +
  scale_color_manual(name = "Life Ladder",values = c("blue", "yellow", 'Red')) +
  labs(title = "Regression Model" ) +
  theme_ggeffects()





## 6
# using "psych" package, show correlation and distribution of final variables

happiness_2 %>% select(Healthy.life.expectancy.at.birth,Log.GDP.per.capita,Life.Ladder) %>%
                psych::pairs.panels()


## 7
# using "ggeffect" for displaying output of your model

ggeffects::ggpredict(model = fit, terms = c("Log.GDP.per.capita",'Life.Ladder') ) %>%
  plot(.,add.data= TRUE , jitter = 0.2)





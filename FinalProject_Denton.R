library(tidyverse)
library(magrittr)
library(tidymodels)
library(modelsummary)
library(dplyr)
library(mice)

#### need to set working directory to source location of downloaded files

math_raw <- read.csv("student-mat.csv",sep = ";",header=TRUE)
port_raw <- read.csv("student-por.csv",sep = ";",header=TRUE)

# it looks like the 0s are NAs, going to try list-wise deletion
math_raw$G3[math_raw$G3 == 0] <- NA
port_raw$G3[port_raw$G3 == 0] <- NA
math_raw$G2[math_raw$G2 == 0] <- NA
port_raw$G2[port_raw$G2 == 0] <- NA
math_raw$G1[math_raw$G1 == 0] <- NA
port_raw$G1[port_raw$G1 == 0] <- NA

math <- math_raw %>% drop_na()
port <- port_raw %>% drop_na()


# need to change some vars to factors, write in codes for others
math$sex <- factor(math$sex)
math$address <- factor(math$address)
math$famsize <- factor(math$famsize)
math$Pstatus <- factor(math$Pstatus)
math$Mjob <- factor(math$Mjob)
math$Fjob <- factor(math$Fjob)
math$reason <- factor(math$reason)
math$guardian <- factor(math$guardian)
math$schoolsup <- factor(math$schoolsup)
math$famsup <- factor(math$famsup)
math$paid <- factor(math$paid)
math$activities <- factor(math$activities)
math$nursery <- factor(math$nursery)
math$higher <- factor(math$higher)
math$internet <- factor(math$internet)
math$romantic <- factor(math$romantic)

math %<>% mutate(studytime = factor(studytime),
                 studytime = fct_recode(studytime, less2 = "1", twotofive = "2",
                                        fivetoten = "3", morethanten = "4"))

math %<>% mutate(Fedu = factor(Fedu),
                 Fedu = fct_recode(Fedu, none="0", prmry="1", mdlschl="2", scndry="3", college="4"))
math %<>% mutate(Medu = factor(Medu),
                 Medu = fct_recode(Medu, none="0", prmry="1", mdlschl="2", scndry="3", college="4"))

# now mutate for portuguese class grades
port$sex <- factor(port$sex)
port$address <- factor(port$address)
port$famsize <- factor(port$famsize)
port$Pstatus <- factor(port$Pstatus)
port$Mjob <- factor(port$Mjob)
port$Fjob <- factor(port$Fjob)
port$reason <- factor(port$reason)
port$guardian <- factor(port$guardian)
port$schoolsup <- factor(port$schoolsup)
port$famsup <- factor(port$famsup)
port$paid <- factor(port$paid)
port$activities <- factor(port$activities)
port$nursery <- factor(port$nursery)
port$higher <- factor(port$higher)
port$internet <- factor(port$internet)
port$romantic <- factor(port$romantic)

port %<>% mutate(studytime = factor(studytime),
                 studytime = fct_recode(studytime, less2 = "1", twotofive = "2",
                                        fivetoten = "3", morethanten = "4"))

port %<>% mutate(Fedu = factor(Fedu),
                 Fedu = fct_recode(Fedu, none="0", prmry="1", mdlschl="2", scndry="3", college="4"))
port %<>% mutate(Medu = factor(Medu),
                 Medu = fct_recode(Medu, none="0", prmry="1", mdlschl="2", scndry="3", college="4"))

# estimate regression solely on study time for both math and portuguese classes
est <- lm(G3 ~ studytime, data = math)
est.p <- lm(G3 ~ studytime, data = port)
summary(est)
summary(est.p)

# estimate regression based on past grades
est2 <- lm(G3 ~ G1 + G2, data = math)
est2.p <- lm(G3 ~ G1 + G2, data = port)
summary(est2)
summary(est2.p)


# estimate regression of Math final grade on other vars
est3 <- lm(G3 ~ G1 + G2 + Medu + Fedu + studytime + address + famsize + Pstatus + activities, data = math)
est3.p <- lm(G3 ~ G1 + G2 + Medu + Fedu + studytime + address + famsize + Pstatus + activities, data = port)
summary(est3)
summary(est3.p)

# lm with every single variable
est4 <- lm(G3 ~ ., data = math)
est4.p <- lm(G3 ~ ., data = port)
summary(est5)
summary(est5.p)

# lm with only statistically significant variables 
est5.p <- lm(G3 ~ G1 + G2 + higher + failures +traveltime + Fjob + Mjob + age, data = port)
est5 <- lm(G3 ~ G1 + G2 + absences + health + goout + famrel + paid, data = math)
summary(est5)
summary(est5.p)

# model summaries for math and portuguese models (respectively)
modelsummary(list(est, est2, est3, est4, est5), output = "latex", stars = TRUE)
# modelsummary(list(est, est2, est3, est5), stars = TRUE)

modelsummary(list(est.p, est2.p, est3.p, est4.p, est5.p), output = "latex", stars = TRUE)
# modelsummary(list(est.p, est2.p, est3.p, est4,p, est5.p), stars = TRUE)

# model summary comparing model 1 and 2 across math and portuguese
modelsummary(list(est, est.p, est2, est2.p), output = "latex", stars = TRUE)
###### visualizations

# model summary comparing model 3 across math and portuguese
modelsummary(list(est3,est3.p), output = "latex", stars=TRUE)

# model summary for model 5 for math and Port
modelsummary(est5, output = "latex", stars = TRUE)

modelsummary(est5.p, output = "latex", stars = TRUE)

# visualization of dist of final grades
hist(math$G3, breaks = 25, main = "", xlab = "Final Grades in Math")
hist(port$G3, breaks = 25, main = '', xlab = "Final Grades in Portuguese")

# visualization of study time and final grades
ggplot(data = math,
       aes(x = studytime, y = G3)) +
       geom_jitter() +
       theme_minimal() +
       labs(x = "Time Studying per Week (hrs)", 
            y = "Final Grade")

ggplot(data = port,
       aes(x = studytime, y = G3)) +
       geom_jitter() +
       theme_minimal() +
       labs(x = "Time Studying per Week (hrs)", 
            y = "Final Grade")


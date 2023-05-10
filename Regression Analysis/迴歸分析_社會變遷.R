library(car)
library(haven)
library(dplyr)
library(ggplot2)
library(data.table)
tscs191 <- read_sav("Desktop/NCCU/111-2/三1234社會研究方法/HW/tscs191.sav") |> setDT()

tscs191 <- (tscs191[, Age := ifelse(a2y <= 100,  108 - a2y, ifelse(a2r <= 100, a2r, NA))]
                   [, 民法 := ifelse(d18 > 5, NA, d18)]
                   [, ":="(Line = factor(ifelse(d1002 == 1, 1, 0)),
                           IG = factor(ifelse(d1003 == 1, 1, 0)))]
                   [!is.na(民法), ])

model <- lm(data = tscs191, formula = 民法 ~ Line + Age)
summary(model)

ggplot(data = tscs191, aes(x = factor(民法), y = Age)) + 
    geom_boxplot()

#[, Age := Recode(Age, "18:29='18-29歲'; 30:39='30-39歲';
#                       40:49='40-49歲'; 50:59='50-59歲';
#                       60:69='60-69歲'; else='70歲以上'")]
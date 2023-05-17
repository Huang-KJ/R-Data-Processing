library(car)
library(descr)
library(haven)
library(dplyr)
library(ggplot2)
library(data.table)
tscs191 <- read_sav("~/Desktop/NCCU/111-2/三1234社會研究方法/HW/tscs191.sav") |> setDT()

tscs191 <- (tscs191[, Sex := factor(a1, levels = c(1:2), labels = c('男', '女'))]
                   [, Age := ifelse(a2y <= 100,  108 - a2y, ifelse(a2r <= 100, a2r, NA))]
                   [, 民法 := ifelse(d18 > 5, NA, d18)]
                   [!is.na(民法) & !d11 %in% c(96, 98, 99),]
                   [, 媒體 := Recode(as.numeric(d11), "1='FB'; 2='LINE'; 3='IG';
                                                       6='YT'; c(4, 5, 7)='其他';
                                                       8='都沒有'") |> factor()]
                   [, ":="(FB = ifelse(d11 == 1, 1, 0), LINE = ifelse(d11 == 2, 1, 0), 
                           IG = ifelse(d11 == 3, 1, 0), YT = ifelse(d11 == 6, 1, 0),
                           其他 = ifelse(d11 %in% c(4, 5, 7), 1, 0),
                           都沒有 = ifelse(d11 == 8, 1, 0))])

model1 <- lm(data = tscs191, formula = 民法 ~ Sex)
summary(model1)

model2 <- lm(data = tscs191, formula = 民法 ~ Sex + 媒體)
summary(model2)

model3 <- lm(data = tscs191, formula = 民法 ~ Sex + 媒體 * Age)
summary(model3)

#殘差常態性檢定
nortest::ad.test(model3$residuals)

#殘差獨立性檢定
durbinWatsonTest(model3)

#殘差同質性檢定
ncvTest(model3)

model <- lm(data = tscs191, formula = 民法 ~ LINE + IG + YT + 其他 + 都沒有)
summary(model)

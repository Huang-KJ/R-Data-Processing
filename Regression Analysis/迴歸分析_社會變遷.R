library(car)
library(descr)
library(haven)
library(dplyr)
library(ggplot2)
library(data.table)
tscs191 <- read_sav("~/Desktop/NCCU/111-2/三1234社會研究方法/HW/tscs191.sav") |> setDT()

tscs191 <- (tscs191[, Age := ifelse(a2y <= 100,  108 - a2y, ifelse(a2r <= 100, a2r, NA))]
                   [, AgeGroup := Recode(Age, "18:29='18-29歲'; 30:39='30-39歲';
                                               40:49='40-49歲'; 50:59='50-59歲';
                                               60:69='60-69歲'; else='70歲以上'")]
                   [, 民法 := ifelse(d18 > 5, NA, d18)]
                   [, ":="(Line = factor(ifelse(d1002 == 1, 1, 0)),
                           IG = factor(ifelse(d1003 == 1, 1, 0)))]
                   [!is.na(民法), ])

model1 <- lm(data = tscs191, formula = 民法 ~ IG)
summary(model1)

model2 <- lm(data = tscs191, formula = 民法 ~ Line)
summary(model2)

model3 <- lm(data = tscs191, formula = 民法 ~ IG + Age)
summary(model3)

model4 <- lm(data = tscs191, formula = 民法 ~ Line + Age)
summary(model4)

model5 <- lm(data = tscs191, formula = 民法 ~ Line + IG + Age)
summary(model5)

#殘差常態性檢定
nortest::ad.test(model5$residuals)

#殘差獨立性檢定
durbinWatsonTest(model5)

#殘差同質性檢定
ncvTest(model5)

ggplot() + 
    geom_boxplot(data = tscs191, aes(x = factor(民法, c(1:5), c('支持的人多很多', '支持的人多一些', '一半一半',
                                                                '反對的人多一些', '反對的人多很多')),
                                     y = Age)) +
    labs(title = '對於目前民法限定婚姻在一男一女的結合，請問您在網路上的朋友，\n是支持的多，還是反對的多?',
         x = '朋友支持度', y = '年齡') +
    theme(plot.title = element_text(size = 25, hjust = 0.5, face ="bold"),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 15))

CST <- crosstab(factor(tscs191$AgeGroup), factor(tscs191$民法), weight = tscs191$wr_19_5,
                plot = F, prop.r = T)
CST <- data.frame(CST$tab)
names(CST) <- c('AgeGroup', '民法', 'N')

friedman.test(y = CST$N, groups = CST$AgeGroup, blocks = CST$民法)

pairwise.wilcox.test(CST$N, CST$AgeGroup, p.adj = "bonf", exact = FALSE)

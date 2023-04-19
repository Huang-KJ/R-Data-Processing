################
# 多層反覆加權 #
################
library(car)
library(haven)
library(tidyr)
library(expss)
library(survey)
library(weights)
library(stringr)
library(data.table)

#匯入調查資料#
setwd("/Users/kyle/Desktop/NCCU/111-2/三1234社會研究方法/HW")
tscs201 <- read_dta('tscs201.dta') |> setDT()
tscs201 <- tscs201[, Sex := factor(x = v1, levels = c(1:2), labels = c("男", "女"))]
tscs201 <- apply_labels(data = tscs201, Sex = "性別")

tscs201 <- (copy(tscs201)[v2y < 100, Age := 110 - v2y]
                         [v2y > 100, Age := grep("[0-9]", v2r)]
                         [, Age := Recode(Age, recodes = '18:29="18-29歲"; 30:39="30-39歲";
                                                          40:49="40-49歲"; 50:59="50-59歲";
                                                          60:69="60-69歲"; else="70歲以上"')])
tscs201 <- apply_labels(data = tscs201, Age = "年齡")

Sample <- tscs201[, .(Sex, Age, id = c(1:nrow(tscs201)))]

#匯入人口母體資料#
setwd('/Users/kyle/Desktop/NCCU/社會調查師/R-Data-Processing/DataSets')
pop <- read.csv("10912村里戶數、單一年齡人口.csv", skip = 1)
names(pop) <- str_replace_all(string = names(pop), pattern = c("X" = "", "\\." = ""))
pop <- subset(x = pop, select = -c(統計年月, 戶數, 人口數, 人口數男, 人口數女))

popByAge <- pivot_longer(data = pop, 
                         cols = str_subset(string = names(pop), pattern = "^[0-9]"), 
                         names_to = "Group", values_to = "Population") |> setDT()

popByAge <- (popByAge[, .(Area = str_sub(區域別, 1, 3), 
                          Sex = str_extract(string = Group, pattern = "[男女]$"), 
                          年齡 = as.numeric(str_extract(string = Group, pattern = "(^[0-9]{1,3})")),
                          人口數 = as.numeric(Population))]
                     [年齡 >= 18 , Age := Recode(年齡, recodes = '18:29="18-29歲"; 30:39="30-39歲";
                                                                  40:49="40-49歲"; 50:59="50-59歲";
                                                                  60:69="60-69歲"; else="70歲以上"')])

Population <- popByAge[年齡 >= 18, .(人口數 = sum(人口數)), by = .(Sex, Age)]
sex <- Population[, .(人口數比例 = 人口數 / sum(人口數), Sex)][, .(Freq = sum(人口數比例)), by = .(Sex)] |> t()
age <- Population[, .(人口數比例 = 人口數 / sum(人口數), Age)][, .(Freq = sum(人口數比例)), by = .(Age)] |> t()

#使用卡方檢定檢查分佈情形#
chisq.test(table(tscs201$Sex), p = as.numeric(sex[2, ]))
chisq.test(table(tscs201$Age), p = round(as.numeric(age[2, ]), 2))

#進行反覆加權#
sex2 <- Population[, .(人口數比例 = 人口數 / sum(人口數), Sex)][, .(Freq = nrow(Sample) * sum(人口數比例)), by = .(Sex)]
age2 <- Population[, .(人口數比例 = 人口數 / sum(人口數), Age)][, .(Freq = nrow(Sample) * sum(人口數比例)), by = .(Age)]

unweighted <- svydesign(ids = ~ 0, data = Sample)
raking <- rake(design = unweighted, sample.margins = list(~ Sex, ~ Age), 
               population.margins = list(sex2, age2))
Sample$weights <- weights(raking)

#使用卡方檢定檢查加權後分佈情形#
chisq.test(wpct(Sample$Sex, Sample$weights), p = as.numeric(sex[2, ]))
chisq.test(wpct(Sample$Age, Sample$weights), p = round(as.numeric(age[2, ]), 2))

#檢視駕照比例#
table(tscs201$v118)
wpct(tscs201$v118, Sample$weights)

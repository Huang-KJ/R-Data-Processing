################
# 事後分層加權 #
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

tscs201 <- (copy(tscs201)
            [v2y < 100, Age := 109 - v2y]
            [v2y > 100, Age := grep("[0-9]", v2r)]
            [, Age := Recode(Age, recodes = '18:29="18-29歲"; 30:39="30-39歲";
                                             40:49="40-49歲"; 50:59="50-59歲";
                                             60:69="60-69歲"; else="70歲以上"')])
tscs201 <- apply_labels(data = tscs201, Age = "年齡")

Sample <- tscs201[, .(Sex, Age)]

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
                     [年齡 >= 18, ]
                     [, Age := Recode(年齡, recodes = '18:29="18-29歲"; 30:39="30-39歲";
                                                                  40:49="40-49歲"; 50:59="50-59歲";
                                                                  60:69="60-69歲"; else="70歲以上"')])

Population <- popByAge[, .(人口數 = sum(人口數)), by = .(Sex, Age)]
sex <- Population[, .(人口數比例 = 人口數 / sum(人口數), Sex)][, .(Freq = sum(人口數比例)), by = .(Sex)] |> t()
age <- Population[, .(人口數比例 = 人口數 / sum(人口數), Age)][, .(Freq = sum(人口數比例)), by = .(Age)] |> t()

#使用卡方檢定檢查分佈情形#
chisq.test(table(tscs201$Sex), p = as.numeric(sex[2, ]))
chisq.test(table(tscs201$Age), p = round(as.numeric(age[2, ]), 2))

#進行多變數反覆加權#
jointDisSam <- table(Sample$Sex, Sample$Age)
jointDisSam <- round(prop.table(jointDisSam), 3) |> as.data.table()
jointDisSam <- dcast(jointDisSam, V1 ~ V2, value.var="N")[, -1] |> as.data.frame()
row.names(jointDisSam) <- c('男', '女')

jointDisPop <- dcast(Population, Sex ~ Age, value.var="人口數")[, -1] |> as.data.frame()
row.names(jointDisPop) <- c('男', '女')
jointDisPop <- (jointDisPop / sum(sum(jointDisPop[1, ]) + sum(jointDisPop[2, ]))) |> round(3)

psWeight <- (jointDisPop / jointDisSam) |> round(3)

jointDisSam
jointDisPop
psWeight

psWeight <- tibble::rownames_to_column(psWeight, var = "Sex") |> 
            pivot_longer(!Sex, names_to = "Age", values_to = "Weight")

Sample <- Sample[, ":="(Sex = as.character(Sex), Age = as.character(Age))]

Sample <- dplyr::left_join(Sample, psWeight)

strata<-c("")
strata[Sample$Sex=="男" & Sample$Age=="18-29歲"] <- 1
strata[Sample$Sex=="男" & Sample$Age=="30-39歲"] <- 2
strata[Sample$Sex=="男" & Sample$Age=="40-49歲"] <- 3
strata[Sample$Sex=="男" & Sample$Age=="50-59歲"] <- 4
strata[Sample$Sex=="男" & Sample$Age=="60-69歲"] <- 5
strata[Sample$Sex=="男" & Sample$Age=="70歲以上"] <- 6

strata[Sample$Sex=="女" & Sample$Age=="18-29歲"] <- 7
strata[Sample$Sex=="女" & Sample$Age=="30-39歲"] <- 8
strata[Sample$Sex=="女" & Sample$Age=="40-49歲"] <- 9
strata[Sample$Sex=="女" & Sample$Age=="50-59歲"] <- 10
strata[Sample$Sex=="女" & Sample$Age=="60-69歲"] <- 11
strata[Sample$Sex=="女" & Sample$Age=="70歲以上"] <- 12
strata <- factor(strata, levels = c(1:12))
Sample <- cbind(Sample, strata)

Population$Freq <- Population$人口數 / sum(Population$人口數)
jointPop <- data.frame(strata = c(1:12),
                       percent = c(0.09363268, 0.08794697, 0.09298987, 0.08928223, 0.07505539, 0.05210569,
                                   0.08657803, 0.08644110, 0.09636962, 0.09278977, 0.08151928, 0.06528937))

unweighted <- svydesign(ids = ~ 1, data = Sample)
ps <- postStratify(design = unweighted, strata =  ~ strata, population = jointPop)
stack(table(weights(ps)))
Sample$weights <- weights(ps)

#使用卡方檢定檢查加權後分佈情形#
chisq.test(wpct(Sample$Sex, Sample$weights), p = as.numeric(sex[2, ]))
chisq.test(wpct(Sample$Age, Sample$weights), p = round(as.numeric(age[2, ]), 2))

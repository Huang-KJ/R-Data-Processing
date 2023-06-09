##################
# 多變項反覆加權 #
##################
library(car)
library(XML)
library(haven)
library(tidyr)
library(expss)
library(survey)
library(weights)
library(stringr)
library(data.table)

############
# 調查資料 #
############

# 匯入調查資料 #
setwd("/Users/kyle/Desktop/NCCU/111-2/三1234社會研究方法/HW")
tscs201 <- read_dta('tscs201.dta') |> setDT()
tscs201 <- tscs201[, Sex := factor(x = v1, levels = c(1:2), labels = c("男", "女"))]
tscs201 <- apply_labels(data = tscs201, Sex = "性別")

# 重新編碼變項 #
tscs201 <- (copy(tscs201)[v2y < 100, Age := 109 - v2y]
                         [v2y > 100, Age := grep("[0-9]", v2r)]
                         [, Age := Recode(Age, recodes = '18:29="18-29歲"; 30:39="30-39歲";
                                                          40:49="40-49歲"; 50:59="50-59歲";
                                                          60:69="60-69歲"; else="70歲以上"')]
                         [v118 != 97, ])
tscs201 <- apply_labels(data = tscs201, Age = "年齡")

Sample <- tscs201[, .(Sex, Age, License = v118, TW = v41, id = c(1:nrow(tscs201)))]

############
# 母體資料 #
############

# 匯入人口母體資料 #
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

# 計算母體性別及年齡比例 #
Population <- popByAge[年齡 >= 18, .(人口數 = sum(人口數)), by = .(Sex, Age)]
sex <- Population[, .(人口數比例 = 人口數 / sum(人口數), Sex)][, .(Freq = sum(人口數比例)), by = .(Sex)] |> t()
age <- Population[, .(人口數比例 = 人口數 / sum(人口數), Age)][, .(Freq = sum(人口數比例)), by = .(Age)] |> t()

# 使用卡方檢定檢查分佈情形 #
chisq.test(table(tscs201$Sex), p = as.numeric(sex[2, ]))
chisq.test(table(tscs201$Age), p = round(as.numeric(age[2, ]), 2))

# 依照母體比例計算期望值 #
sex2 <- Population[, .(人口數比例 = 人口數 / sum(人口數), Sex)][, .(Freq = nrow(Sample) * sum(人口數比例)), by = .(Sex)]
age2 <- Population[, .(人口數比例 = 人口數 / sum(人口數), Age)][, .(Freq = nrow(Sample) * sum(人口數比例)), by = .(Age)]

############
# 反覆加權 #
############

## 使用survey套件進行反覆加權 ##
unweighted <- svydesign(ids = ~ 0, data = Sample)
raking <- rake(design = unweighted, sample.margins = list(~ Sex, ~ Age), 
               population.margins = list(sex2, age2))
Sample$weights <- weights(raking)

# 使用卡方檢定檢查加權後分佈情形 #
chisq.test(wpct(Sample$Sex, Sample$weights) * nrow(Sample), p = as.numeric(sex[2, ]))
chisq.test(wpct(Sample$Age, Sample$weights) * nrow(Sample), p = round(as.numeric(age[2, ]), 2))


## 手動進行反覆加權 ##
# 加權性別 #
sex3 <- prop.table(table(Sample$Sex)) |> data.frame() |> t()
sexW <- data.frame(Sex = c('男', '女'), WS = as.numeric(sex[2,]) / as.numeric(sex3[2,]))
Sample <- merge(Sample, sexW, by = 'Sex')
Sample[, Sex := factor(Sex, levels = c('男', '女'))]

# 檢查加權性別後分佈 #
chisq.test(wpct(Sample$Sex, Sample$WS) * nrow(Sample), p = as.numeric(sex[2, ]))
chisq.test(wpct(Sample$Age, Sample$WS) * nrow(Sample), p = round(as.numeric(age[2, ]), 2))

# 加權年齡 #
SexAgeCount <- Sample[,.(.N), by = .(Age, Sex)]
AgeProb <- merge(SexAgeCount, sexW)
AgeProb <- AgeProb[, N := N * WS][, .(N = sum(N)), by = .(Age)][, ProbA := N / sum(N)][]

A <- Population[, .(人口數比例 = 人口數 / sum(人口數), Age)][, .(FreqA = sum(人口數比例)), by = .(Age)]
ageW <- merge(AgeProb, A)
ageW <- ageW[, .(Age, WA = FreqA / ProbA)]

Sample <- merge(Sample, ageW)

# 檢查加權年齡後分佈 #
chisq.test(wpct(Sample$Sex, Sample$WA) * nrow(Sample), p = as.numeric(sex[2, ]))
chisq.test(wpct(Sample$Age, Sample$WA) * nrow(Sample), p = round(as.numeric(age[2, ]), 2))

# 計算總權數 #
Sample[, weights2 := WS * WA]

# 檢查總權數加權後分佈 #
chisq.test(wpct(Sample$Sex, Sample$weights2) * nrow(Sample), p = as.numeric(sex[2, ]))
chisq.test(wpct(Sample$Age, Sample$weights2) * nrow(Sample), p = round(as.numeric(age[2, ]), 2))

###################
# v41台灣社會發展 #
###################
table(Sample$TW)
(wpct(Sample$TW, Sample$weights) * nrow(Sample)) |> round(1)

################
# 檢視駕照比例 #
################

# 計算母體不同年齡及性別持有駕照人數 #
setwd('/Users/kyle/Desktop/NCCU/社會調查師/R-Data-Processing/DataSets')
license <- xmlToDataFrame('領有駕駛執照人數2.XML') |> setDT()
license <- (license[Category1Title == '小型車普通駕駛人' & Category2Title %in% c('男', '女') & Category3Title != '計', ]
                   [, ":="(Sex = factor(Category2Title, levels = c('男', '女')),
                           Age = Recode(Category3Title, "c('未滿20歲', '20-未滿25歲', '25-未滿30歲')='18-29歲';
                                                         c('30-未滿35歲', '35-未滿40歲')='30-39歲';
                                                         c('40-未滿45歲', '45-未滿50歲')='40-49歲';
                                                         c('50-未滿55歲', '55-未滿60歲')='50-59歲';
                                                         '60歲以上'='60歲以上'"))])
# 調整樣本編碼 #
Sample$Age2 <- ifelse(Sample$Age %in% c('60-69歲', '70歲以上'), '60歲以上', Sample$Age)

# 比較母體及樣本 #
license <- license[Period == '11000', .(人數 = sum(as.numeric(Val))), keyby = .(Sex, Age)][, 比例 := 人數 / sum(人數)][]

Sample2 <- Sample[License == 1, .(人數 = .N, 加權人數 = sum(weights)), keyby = .(Sex, Age2)][]

# 檢查加權前後差異 #
chisq.test(Sample2$人數, p = license$比例)
chisq.test(Sample2$加權人數, p = license$比例)

# 計算全體有無駕照比例 #
l.Unweighted <- table(tscs201$v118)
l.Weighted <- wpct(tscs201$v118, Sample$weights) * 1854

有駕照比例 <- sum(license$人數) / sum(Population$人口數)
沒駕照比例 <- (sum(Population$人口數) - sum(license$人數)) / sum(Population$人口數)

l.stat <- data.frame(a = 有駕照比例 * 1854,  b = 沒駕照比例 * 1854)

# 檢查加權前後差異 #
chisq.test(l.Unweighted, p = c(有駕照比例, 沒駕照比例))
chisq.test(l.Weighted, p = c(有駕照比例, 沒駕照比例))


#devtools::install_github("rstudio/addinexamples", type = "source")

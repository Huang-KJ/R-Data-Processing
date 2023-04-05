########
# 加權 #
########
library(car)
library(haven)
library(tidyr)
library(expss)
library(survey)
library(stringr)
library(anesrake)
library(data.table)

NDCDSD2022 <- read_sav('/Users/kyle/Desktop/NCCU/RA/NDCDSD2022/NDCDSD-2022/NDC111033我國家庭經濟支持功能之研究.sav') |> setDT()

NDCDSD2022 <- NDCDSD2022[, Sex := factor(x = Q27, levels = c(1:2), labels = c("男", "女"))]
NDCDSD2022 <- apply_labels(data = NDCDSD2022, Sex = "性別")

NDCDSD2022 <- (copy(NDCDSD2022)
               [, Age := Recode(var = AGE, 
                                recodes = '1:3=1; 4:5=2;
                                           6:7=3; 8:9=4;
                                           10=5; 11=6;
                                           12=7')]
               [, Age := factor(x = Age,
                                levels = c(1:7),
                                labels = c("18-29歲", "30-39歲", "40-49歲", 
                                           "50-59歲", "60-64歲", "65-69歲",
                                           "70歲以上"))])
NDCDSD2022 <- apply_labels(data = NDCDSD2022, Age = "年齡")

NDCDSD2022 <- (copy(NDCDSD2022)
               [, Area := Q1]
               [, Area := factor(x = Area,
                                 levels = c(1:22),
                                 labels = c('新北市', '臺北市', '桃園市', '臺中市', '臺南市', '高雄市', 
                                            '宜蘭縣', '新竹縣', '苗栗縣', '彰化縣', '南投縣', '雲林縣', 
                                            '嘉義縣', '屏東縣', '臺東縣', '花蓮縣', '澎湖縣', '基隆市', 
                                            '新竹市', '嘉義市', '金門縣', '連江縣'))])
NDCDSD2022 <- apply_labels(data = NDCDSD2022, Area = "居住地")

Sample <- NDCDSD2022[, .(Sex, Age, Area, id = c(1:nrow(NDCDSD2022)))]

pop <- read.csv("./DataSets/11202村里戶數、單一年齡人口.csv", skip = 1)
names(pop) <- str_replace_all(string = names(pop), pattern = c("X" = "", "\\." = ""))
pop <- subset(x = pop, select = -c(統計年月, 戶數, 人口數, 人口數男, 人口數女))

popByAge <- pivot_longer(data = pop, 
                         cols = str_subset(string = names(pop), pattern = "^[0-9]"), 
                         names_to = "Group", values_to = "Population") |> setDT()

popByAge <- (popByAge[, .(Area = str_sub(區域別, 1, 3), 
                          Sex = str_extract(string = Group, pattern = "[男女]$"), 
                          年齡 = as.numeric(str_extract(string = Group, pattern = "(^[0-9]{1,3})")),
                          人口數 = as.numeric(Population))]
                     [年齡 >= 18 , Age := Recode(年齡,
                                                 recodes = '18:29="18-29歲"; 30:39="30-39歲";
                                                            40:49="40-49歲"; 50:59="50-59歲";
                                                            60:64="60-64歲";65:69="65-69歲";
                                                            else="70歲以上"')])

Population <- popByAge[年齡 >= 18, .(人口數 = sum(人口數)), by = .(Area, Sex, Age)]
area <- Population[, .(人口數比例 = 人口數 / sum(人口數), Area)][, .(Freq = nrow(Sample) * sum(人口數比例)), by = .(Area)]
sex <- Population[, .(人口數比例 = 人口數 / sum(人口數), Sex)][, .(Freq = nrow(Sample) * sum(人口數比例)), by = .(Sex)]
age <- Population[, .(人口數比例 = 人口數 / sum(人口數), Age)][, .(Freq = nrow(Sample) * sum(人口數比例)), by = .(Age)]

unweighted <- svydesign(ids = ~ 0, data = Sample)#宣告抽樣調查資料，並指定ids參數為0代表簡單隨機抽樣
raking <- rake(design = unweighted, sample.margins = list( ~ Area, ~ Sex, ~ Age), 
               population.margins = list(area, sex, age))#進行多變數反覆加權
Sample$weights<-weights(raking)

Area <- area$Freq
names(Area) <- area$縣市
Sex <- sex$Freq
names(Sex) <- sex$性別
Age <- age$Freq
names(Age) <- age$年齡層

targets <- list(Area, Sex, Age)
names(targets) <- c("Area", "Sex", "Age")

raking <- anesrake(targets, Sample, Sample$id,
                   choosemethod = "total",
                   pctlim = 0.05)
Sample$weights <- raking$weightvec

example$weights <- weights(raking)
example

area <- c(3434003, 2108370, 1895534, 2354568, 1592747, 2349921, 386371, 471022, 
          458162, 1059836, 417858, 575907, 434387, 698760, 183304, 274452, 94255, 
          317338, 364605, 221971, 126842, 12254) / 19832467 
sex <- c(9716939, 10115528) / 19832467 
age <- c(441397, 1345864, 1591927, 1589933, 1672483, 2009683,
         1827312, 1770183, 1782312, 1715580, 1489738, 2596055) / 19832467 
edu <- c(2233251, 2261269, 5530048, 2249415, 5953463, 1630168) / 19857614

targets <- list(area, sex, age, edu)
names(targets) <- c("Area", "Sex", "Age", "Edu")

anesrakefinder(targets, Sample, choosemethod = "total")

raking <- anesrake(targets, Sample, Sample$流水編號,
                   choosemethod = "total",
                   pctlim = 0.05)

Sample$weights <- raking$weightvec

##################
# 多變數反覆加權 #
##################
example <- read.csv("./DataSets/gender_age.csv", header = T, sep = ",")
head(example)

example$gender <- factor(example$gender, levels = c(1, 2), labels = c("Male", "Female"))
example$age <- factor(example$age, levels = c(1, 2, 3, 4), labels = c("Under 19", "20-39", "40-59", "Over 60"))
table(example$gender)
table(example$age)

chisq.test(table(example$gender), p = c(0.495, 0.505))
chisq.test(table(example$age), p = c(0.171, 0.273, 0.316, 0.240))

unweighted <- svydesign(ids = ~ 0, data = example)#宣告抽樣調查資料，並指定ids參數為0代表簡單隨機抽樣
gender_p <- data.frame(gender = c("Male", "Female"), Freq = nrow(example) * c(0.495, 0.505))
age_p <- data.frame(age = c("Under 19","20-39","40-59","Over 60"), Freq = nrow(example) * c(0.171, 0.273, 0.316, 0.240))

raking <- rake(design = unweighted, sample.margins = list( ~ gender, ~ age),
               population.margins = list(gender_p, age_p))#進行多變數反覆加權
stack(table(weights(raking)))#輸出加權結果

example$weights <- weights(raking)
example

svytable( ~ gender + age, unweighted)

weighted <- svydesign(id = ~ 0, data = example, weights = ~ weights)
summary(svytable( ~ gender + age, design = weighted))

svygofchisq( ~ gender, p = c(0.495, 0.505), design = weighted)
svygofchisq( ~ age, p = c(0.171, 0.273, 0.316, 0.240), design = weighted)

gender_target <- c(0.495, 0.505)
age_target <- c(0.171, 0.273, 0.316, 0.240)
names(gender_target) <- c("Male", "Female")
names(age_target) <- c("Under 19", "20-39", "40-59", "Over 60")
target <- list(gender_target, age_target)
names(target) <- c("gender", "age")
target

id <- 1:length(example$age)
example <- cbind(example, id)
head(example)

raking2 <- anesrake(target, example, caseid = example$id, choosemethod = "total")
stack(table(raking2$weightvec))
example$weights2 <- raking2$weightvec

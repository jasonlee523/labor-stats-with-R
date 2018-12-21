sink('output.txt')

rm(list=ls())
install.packages("tidyverse")
install.packages("AER")

library(tidyverse)
library(AER)
options(scipen=999)
setwd("/Users/jason/Downloads/labor")
load("Census1990_Small.Rda")
load("ACS2001_Small.Rda")
load("ACS2010_Small.Rda")
load("ACS2015_Small.Rda")

d90 <- Census1990_Small
d01 <- ACS2001_Small
d10 <- ACS2010_Small
d15 <- ACS2015_Small

#Problem 1 Part 2
Q1_90 <- Census1990_Small %>% filter(wkswork2==6 & uhrswork >= 35 & !(incwage == 999999) & !(incwage == 0))
quantile(Q1_90$incwage*Q1_90$cpi99, probs = c(0.1, 0.5, 0.9))
p10_90 <- 13977.60
p50_90 <- 31127.04
p90_90 <- 67200.00

Q1_01 <- ACS2001_Small %>% filter(wkswork2==6 & uhrswork >= 35 & !(incwage == 999999) & !(incwage == 0))
quantile(Q1_01$incwage*Q1_01$cpi99, probs = c(0.1, 0.5, 0.9))
p10_01 <- 14679.6
p50_01 <- 32935.0
p90_01 <- 72457.0

Q1_10 <- ACS2010_Small %>% filter(wkswork2==6 & uhrswork >= 35 & !(incwage == 999999) & !(incwage == 0))
quantile(Q1_10$incwage*Q1_10$cpi99, probs = c(0.1, 0.5, 0.9))
p10_10 <- 13752
p50_10 <- 32088
p90_10 <- 76400

Q1_15 <- ACS2015_Small %>% filter(wkswork2==6 & uhrswork >= 35 & !(incwage == 999999) & !(incwage == 0))
quantile(Q1_15$incwage*Q1_15$cpi99, probs = c(0.1, 0.5, 0.9))
p10_15 <- 13800
p50_15 <- 31395
p90_15 <- 77280

#Problem 2 Part 2
WS_90 <- Census1990_Small %>% filter(!(incwage == 999999) & !(incwage == 0))
WS_90$logIncome <- log(WS_90$incwage * WS_90$cpi99)
WS_90$college <- 0
WS_90$college[WS_90$educd >= 080] <- 1
results_90 <- lm(WS_90$logIncome ~ WS_90$college)

WS_01 <- ACS2001_Small %>% filter(!(incwage == 999999) & !(incwage == 0))
WS_01$logIncome <- log(WS_01$incwage * WS_01$cpi99)
WS_01$college <- 0
WS_01$college[WS_01$educd >= 080] <- 1
results_01 <- lm(WS_01$logIncome ~ WS_01$college)

WS_10 <- ACS2010_Small %>% filter(!(incwage == 999999) & !(incwage == 0))
WS_10$logIncome <- log(WS_10$incwage * WS_10$cpi99)
WS_10$college <- 0
WS_10$college[WS_10$educd >= 080] <- 1
results_10 <- lm(WS_10$logIncome ~ WS_10$college)

WS_15 <- ACS2015_Small %>% filter(!(incwage == 999999) & !(incwage == 0))
WS_15$logIncome <- log(WS_15$incwage * WS_15$cpi99)
WS_15$college <- 0
WS_15$college[WS_15$educd >= 080] <- 1
results_15 <- lm(WS_15$logIncome ~ WS_15$college)

#Problem 3, Part 2
WS_90$realIncome <- WS_90$incwage*WS_90$cpi99
WS_01$realIncome <- WS_01$incwage*WS_01$cpi99
WS_10$realIncome <- WS_10$incwage*WS_10$cpi99
WS_15$realIncome <- WS_15$incwage*WS_15$cpi99

results2_90 <- lm(log(WS_90$realIncome) ~ WS_90$age)
results2_01 <- lm(log(WS_01$realIncome) ~ WS_01$age)
results2_10 <- lm(log(WS_10$realIncome) ~ WS_10$age)
results2_15 <- lm(log(WS_15$realIncome) ~ WS_15$age)

incomegrowth <- results2_90$fitted.values
incomegrowth01 <- results2_01$fitted.values
incomegrowth10 <- results2_10$fitted.values
incomegrowth15 <- results2_15$fitted.values

results3_90 <- lm(incomegrowth ~ WS_90$college)
results3_01 <- lm(incomegrowth01 ~ WS_01$college)
results3_10 <- lm(incomegrowth10 ~ WS_10$college)
results3_15 <- lm(incomegrowth15 ~ WS_15$college)

# #Problems 1 and 2 
# WS90 <- Census1990_Small %>% filter(age>=18 & age<=65)
# View(prop.table(table(WS90$educd)))
# 
# WS01 <- ACS2001_Small %>% filter(age>=18 & age<=65)
# View(prop.table(table(WS01$educd)))
# 
# WS10 <- ACS2010_Small %>% filter(age>=18 & age<=65)
# View(prop.table(table(WS10$educd)))
# 
# WS15 <- ACS2015_Small %>% filter(age>=18 & age<=65)
# View(prop.table(table(WS15$educd)))
# 
# #Problem 3
# WS90_2 <- Census1990_Small %>% filter(age>=25 & age<=35)
# View(prop.table(table(WS90_2$educd)))
# 
# WS01_2 <- ACS2001_Small %>% filter(age>=25 & age<=35)
# View(prop.table(table(WS01_2$educd)))
# 
# WS10_2 <- ACS2010_Small %>% filter(age>=25 & age<=35)
# View(prop.table(table(WS10_2$educd)))
# 
# WS15_2 <- ACS2015_Small %>% filter(age>=25 & age<=35)
# View(prop.table(table(WS15_2$educd)))
# 
# #Problem 4
# WS90_M <- Census1990_Small %>% filter(sex==1)
# WS90_M$lnLF <- 1
# WS90_M$lnLF[WS90_M$empstat==1] <- 0
# View(prop.table(table(WS90_M$lnLF)))
# 
# WS90_F <- Census1990_Small %>% filter(sex==2)
# WS90_F$lnLF <- 1
# WS90_F$lnLF[WS90_F$empstat==1] <- 0
# View(prop.table(table(WS90_F$lnLF)))
# 
# WS01_M <- ACS2001_Small %>% filter(sex==1)
# WS01_M$lnLF <- 1
# WS01_M$lnLF[WS01_M$empstat==1] <- 0
# View(prop.table(table(WS01_M$lnLF)))
# 
# WS01_F <- ACS2001_Small %>% filter(sex==2)
# WS01_F$lnLF <- 1
# WS01_F$lnLF[WS01_F$empstat==1] <- 0
# View(prop.table(table(WS01_F$lnLF)))
# 
# WS10_M <- ACS2010_Small %>% filter(sex==1)
# WS10_M$lnLF <- 1
# WS10_M$lnLF[WS10_M$empstat==1] <- 0
# View(prop.table(table(WS10_M$lnLF)))
# 
# WS10_F <- ACS2010_Small %>% filter(sex==2)
# WS10_F$lnLF <- 1
# WS10_F$lnLF[WS10_F$empstat==1] <- 0
# View(prop.table(table(WS10_F$lnLF)))
# 
# WS15_M <- ACS2015_Small %>% filter(sex==1)
# WS15_M$lnLF <- 1
# WS15_M$lnLF[WS15_M$empstat==1] <- 0
# View(prop.table(table(WS15_M$lnLF)))
# 
# WS15_F <- ACS2015_Small %>% filter(sex==2)
# WS15_F$lnLF <- 1
# WS15_F$lnLF[WS15_F$empstat==1] <- 0
# View(prop.table(table(WS15_F$lnLF)))

sink()
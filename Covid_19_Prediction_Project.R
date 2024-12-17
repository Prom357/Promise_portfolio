#Covid death prediction code

# set working directory

setwd(dirname(file.choose()))
getwd()

# Data Preparation from csv file
COVID_DATA = read.csv("COVID_DATA.csv")

# Inspect top rows of the data
head(COVID_DATA)    
str(COVID_DATA)

attach(COVID_DATA)

#checking for missing data 

apply(COVID_DATA, MARGIN = 2, FUN = function(x) sum(is.na(x)))
library(Amelia)
missmap(COVID_DATA, col = c("black", "blue"), legend = FALSE)

#Indexing death_number variables per thousands
COVID_DATA <- within (COVID_DATA, pdeath_number <- (death_number / Tot_Health)*1000)
COVID_DATA <- within (COVID_DATA, pGood_h <- (Good_h/ Tot_Health)*1000)
COVID_DATA <- within (COVID_DATA, pFair_h <- (Fair_h / Tot_Health)*1000)
COVID_DATA <- within (COVID_DATA, pBad_h <- (Bad_h / Tot_Health)*1000)
COVID_DATA <- within (COVID_DATA, pAge0_29 <- (Age0_29 / Tot_AGE)*1000)
COVID_DATA <- within (COVID_DATA, pAge30_over <- (Age30_Over / Tot_AGE)*1000)
COVID_DATA <- within (COVID_DATA, pMale <- (Male / Tot_Sex)*1000)
COVID_DATA <- within (COVID_DATA, pFemale <- (Female / Tot_Sex)*1000)
COVID_DATA <- within (COVID_DATA, pWhite <- (White / Tot_Ethnic)*1000)
COVID_DATA <- within (COVID_DATA, pMixed <- (Mixed / Tot_Ethnic)*1000)
COVID_DATA <- within (COVID_DATA, pAsian <- (Asian / Tot_Ethnic)*1000)
COVID_DATA <- within (COVID_DATA, pBlack <- (Black / Tot_Ethnic)*1000)
COVID_DATA <- within (COVID_DATA, pOthers <- (Others / Tot_Ethnic)*1000)

attach(COVID_DATA)
summary(COVID_DATA)

#Checking the dependent variable for outliers using boxplot
#boxplot for variable of Covid death_number
boxplot(pdeath_number, xlab="pCOVID-19 Death", ylab="Count", col = "blue")
#qqnorm and qqline plot for dependent variable(Tota_Covid death)
qqnorm(pdeath_number, xlab= "pdeath_number", col=1)
qqline(pdeath_number, xlab= "pdeath_number", col=2)
hist(pdeath_number)
#Ks.test and Shapiro.test for dependent variable(Tota_Covid death)
ks.test(pdeath_number, "pnorm", mean(pdeath_number), sd(pdeath_number))
shapiro.test(pdeath_number)

#Checking the Independent variable for outliers using boxplot
boxplot(pGood_h, pFair_h, pBad_h, names= c("pGood_h", "pFair_h", "pBad_h"), xlab="Health Conditions", ylab="Count")
boxplot(pAge0_29, pAge30_over, names= c("pAge0_29", "pAge30_over"), xlab="Age", ylab="Count")
boxplot(pMale, pFemale, names = c("pMale", "pFemale"), xlab="Sex", ylab="Count")
boxplot(pWhite, pMixed, pAsian, pBlack, pOthers, names= c("pWhite", "pMixed", "pAsian", "pBlack", "pOthers"), xlab="Ethincity", ylab="Count")

#scatterplot for dependent and independent variables 

plot(pdeath_number, pGood_h, col="red", pch=16, cex=0.5)
plot(pdeath_number, pFair_h, col="blue", pch=16, cex=0.5)
plot(pdeath_number, pBad_h, col="red", pch=16, cex=0.5)
plot(pdeath_number, pAge0_29, col="red", pch=16, cex=0.5)
plot(pdeath_number, pAge30_over, col="blue", pch=16, cex=0.5)
plot(pdeath_number, pMale, col="red", pch=16, cex=0.5)
plot(pdeath_number, pFemale, col="red", pch=16, cex=0.5)
plot(pdeath_number, pWhite, col="red", pch=16, cex=0.5)
plot(pdeath_number, pMixed, col="red", pch=16, cex=0.5)
plot(pdeath_number, pAsian, col="red", pch=16, cex=0.5) 
plot(pdeath_number, pBlack, col="red", pch=16, cex=0.5)
plot(pdeath_number, pOthers, col="red", pch=16, cex=0.5)


#MULTIVARIATE SCATTER PLOT dependent variable and Health
pairs(~ pdeath_number + pGood_h + pFair_h + pBad_h, data = COVID_DATA,
      main = "multivariate scatterplot matrix")

#MULTIVARIATE SCATTER PLOT dependent variable and Age
pairs(~ pdeath_number + pAge0_29 + pAge30_over, data = COVID_DATA,
      main = "multivariate scatterplot matrix")

#MULTIVARIATE SCATTER PLOT dependent variable and Sex
pairs(~ pdeath_number +  pMale
      + pFemale, data = COVID_DATA,
      main = "multivariate scatterplot matrix")

#MULTIVARIATE SCATTER PLOT dependent variable and Ethnic
pairs(~ pdeath_number +  + pWhite + pMixed + pAsian + pBlack + pOthers, data = COVID_DATA,
      main = "multivariate scatterplot matrix")


#Correlation Matrix

# select subset of data
Covid.data2 <- data.frame(pdeath_number, pGood_h, pFair_h, pBad_h, pAge0_29, pAge30_over, pMale, pFemale,
                          pWhite, pMixed, pAsian, pBlack, pOthers)
# add column names
colnames(Covid.data2) <- c("pdeath_number", "pGood_h", "pFair_h", "pBad_h", "pAge0_29",
                           "pAge30_over", "pMale", "pFemale", "pWhite", "pMixed", "pAsian",
                           "pBlack","pOthers")

# basic correlation matrix
cor(Covid.data2, method = "spearman")
Covid.death2 <- cor(Covid.data2, method = "spearman")
round(Covid.death2, digits = 2)

#Testing principal Analysis

# select variables by excluding those not required
myvars <- names(COVID_DATA)%in% c("District", "Districtcode", "Tot_Health", "death_number",
                                  "Good_h", "Fair_h", "Bad_h", "Age0_29", "Male", "Female",
                                  "White", "Mixed", "Asian", "Black", "Others", "Tot_AGE", "Tot_Sex", "Age30_Over",
                                  "Total_Sex", "Tot_Ethnic")

Covid.data3 <- COVID_DATA[!myvars]
str(Covid.data3)
rm(myvars)

library(psych)
# Kaiser-Meyer-Olkin statistics: if overall MSA > 0.6, proceed to factor analysis
KMO(cor(Covid.data3))

#Regression Modeling

# Multiple Regression

# model with all variables
model1 <- lm(pdeath_number ~ pGood_h  + pFair_h + pBad_h + pAge0_29 + pAge30_over
             + pMale  + pFemale + pWhite + pMixed + pAsian + pBlack + pOthers)
summary(model1)
library(car)
vif(model1)
sqrt(vif(model1)) > 2  # if > 2 vif too high

model2 <- lm(pdeath_number ~ pGood_h  + pFair_h + pAge0_29
             + pMale + pWhite + pMixed + pAsian + pBlack)
summary(model2)
vif(model2)
sqrt(vif(model2)) > 2  # if > 2 vif too high

model3 <- lm(pdeath_number ~ pGood_h  + pFair_h + pAge0_29
             + pWhite + pMixed + pAsian + pBlack)
summary(model3)
vif(model3)
sqrt(vif(model3)) > 2  # if > 2 vif too high

model4 <- lm(pdeath_number ~ pGood_h + pAge0_29
             + pWhite + pMixed + pAsian + pBlack)
summary(model4)
vif(model4)
sqrt(vif(model4)) > 2  # if > 2 vif too high

model5 <- lm(pdeath_number ~ pGood_h + pAge0_29
             + pWhite + pAsian)
summary(model5)
vif(model5)
sqrt(vif(model5)) > 2  # if > 2 vif too high
library(relaimpo)
calc.relimp(model5, type = c("lmg"), rela = TRUE)

model6 <- lm(pdeath_number ~ pGood_h + pAge0_29
             + pAsian)
summary(model6)
vif(model6)
sqrt(vif(model6)) > 2  # if > 2 vif too high
calc.relimp(model6, type = c("lmg"), rela = TRUE)

#Test ANOVA
anova(model5, model6, test= "F")


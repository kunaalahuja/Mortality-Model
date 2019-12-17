library(dplyr)
library(ggplot2)
library(gridExtra)
library(rmarkdown)
library(car)
library(yhat)
library(lme4)
library(grplasso)
library(polycor)



#Columns we care about 
cols = c('resident_status', 'detail_age_type', 'detail_age', 'age_recode_27', 
         'education_reporting_flag', 'education_2003_revision', 'education_1989_revision',
         'sex', 'marital_status',
         'race_recode_5', 'hispanic_originrace_recode', 'X39_cause_recode', 'current_data_year' )


#Read the data from 2011 - 2015 and subsample each of them to 40K rows
set.seed(100) #to reproduce the results

Data1 = read.csv("2015_data.csv",header = TRUE)
Data1 = Data1[,cols]
Data1s <- Data1[sample(1:nrow(Data1), 40*10^3, replace=FALSE),]


Data2 = read.csv("2014_data.csv",header = TRUE)
Data2 = Data2[,cols]
Data2s <- Data2[sample(1:nrow(Data2), 40*10^3, replace=FALSE),]

Data3 = read.csv("2013_data.csv",header = TRUE)
Data3 = Data3[,cols]
Data3s <- Data3[sample(1:nrow(Data3), 40*10^3, replace=FALSE),]

Data4 = read.csv("2012_data.csv",header = TRUE)
Data4 = Data4[,cols]
Data4s <- Data4[sample(1:nrow(Data4), 40*10^3, replace=FALSE),]

Data5 = read.csv("2011_data.csv",header = TRUE)
Data5 = Data5[,cols]
Data5s <- Data5[sample(1:nrow(Data5), 40*10^3, replace=FALSE),]

#Data = Data[,cols]

#Data2 = read.csv("2014_data.csv",header = TRUE)
Data = rbind(Data1s, Data2s, Data3s, Data4s, Data5s)

#Save Memory by removing the data we've stitched together
rm(Data1, Data1s, Data2, Data2s, Data3, Data3s, Data4, Data4s, Data5, Data5s)

#########################################################################
## Data Cleaning

#Age
#get the cases where age is in years
Data     = Data[Data$detail_age_type == 1, ]
Data     = Data[Data$age_recode_27 != 27, ] #remove where age is not present
Data     = Data[Data$age_recode_27 >= 10, ] #filter for only adults >=19
Data$age = Data$detail_age
#Data$age_recode_27 = as.factor(Data$age_recode_27)

#Education
Data = Data[Data$education_reporting_flag != 2, ] #remove where education is not present
#recoding education into 3 levels : high school or less, less than 4 year college, greater than 4 years college
Data[, 'education'] <- ifelse(is.na(Data$education_2003_revision), cut(Data$education_1989_revision, breaks=c(-1, 12, 15, 17, 100)), cut(Data$education_2003_revision, breaks=c(0,3,5,8, 100)))
Data$education = as.factor(Data$education)
Data = Data[Data$education != 4,] #remove where education is not reported
levels(Data$education) <- c('Up to High School','College < 4 years', 'College > 4 years', 'unknown' )
Data$education <- recode(Data$education, 1 = '')
Data$education <- factor(Data$education)

#Resident Status
Data$resident_status = as.factor(Data$resident_status)
levels(Data$resident_status) <- c('Resident', 'Intrastate NR', 'Interstate NR', 'Foreign Residents')

#gender
#nothing to be done, already a categorical variable

#Race
Data$race = as.factor(Data$race_recode_5)
levels(Data$race) <- c('White', 'Black', 'American Indian', 'Asian/Pacific')

#recode hispanic into 3 levels: hispanic, nonhispanic, unreported
Data$hispanic_originrace_recode = as.numeric(Data$hispanic_originrace_recode)
Data[, 'hispanic'] <- cut(Data$hispanic_originrace_recode, breaks=c(0,5,8,10), labels=c('hispanic','non-hispanic','unknown'))
Data = Data[Data$hispanic !='unknown',] #remove where hispanic origin not reported
Data$hispanic <- factor(Data$hispanic)

#marital_status
levels(Data$marital_status) <- c('Divorced', 'Married', 'Never Married', 'Unknown', 'Widowed')

#X39_recode_cause
Data$cause_of_death = as.factor(Data$X39_cause_recode)
levels(Data$cause_of_death) = c("Tuberculosis",
                                "Syphilis",
                                "HIV",
                                "Malignant neoplasms",
                                "Malignant neoplasm Stomach",
                                "Malignant neoplasms of Colon",
                                "Malignant neoplasm of Pancreas (C25)",
                                "Malignant neoplasms of Lung",
                                "Malignant neoplasm of breast",
                                "Malignant neoplasms of Ovary",
                                "Malignant neoplasm of prostate",
                                "Malignant neoplasms of urinary tract",
                                "Non-Hodgkin's lymphoma",
                                "Leukemia",
                                "Other malignant neoplasms",
                                "Diabetes",
                                "Alzheimer's",
                                "Major cardiovascular diseases",
                                "Diseases of heart",
                                "Hypertensive heart disease",
                                "Ischemic heart diseases",
                                "Other diseases of heart",
                                "hypertension and hypertensive renal disease",
                                "Cerebrovascular diseases",
                                "Atherosclerosis",
                                "Other diseases of circulatory system",
                                "Influenza and pneumonia",
                                "Chronic lower respiratory diseases",
                                "Peptic ulcer",
                                "Chronic liver disease and cirrhosis",
                                "Nephritis",
                                "Pregnancy",
                                "Perinatal period",
                                "Congenital malformations",
                                "Sudden infant death syndrome",
                                "Abnormal clinical and laboratory findings",
                                "All other diseases (Residual)",
                                "Motor vehicle accidents",
                                "All other and unspecified accidents",
                                "Suicide",
                                "Assault",
                                "external causes")
#current year
Data$current_data_year = as.factor(Data$current_data_year)
Data$year = Data$current_data_year

#####################################################################

cols = c('resident_status', 'age',
         'education',
         'sex', 'marital_status',
         'race', 'hispanic', 'cause_of_death', 'year' )

Data = Data[,cols]

########################################################################
# Exploratory Data Analysis

#histogram of the response variable
hist(Data$age, breaks=100, xlab='Age at Time of Death', main='') #histogram of age at TOD, makes since it's left tailed

# boxplots of response vs qualitative predictors
boxplot(Data$age ~ Data$resident_status, ylab='Age', xlab='Resident Status', main='Variation of Age by Residency', col= rainbow(10))
boxplot(Data$age ~ Data$sex, ylab='Age', xlab='Sex', main='Variation of Age by Sex', col= rainbow(10))
boxplot(Data$age ~ Data$race, ylab='Age', xlab='Race', main='Variation of Age by Race', col= rainbow(10))
boxplot(Data$age ~ Data$hispanic, ylab='Age', xlab='Hispanic', main='Variation of Age by Hispanic Origin', col= rainbow(10))
boxplot(Data$age ~ Data$education, ylab='Age', xlab='Education Level', main='Variation of Age by Education', col= rainbow(10))
boxplot(Data$age ~ Data$marital_status, ylab='Age', xlab='Marital Status', main='Variation of Age by Marital Status', col= rainbow(10))
boxplot(Data$age ~ Data$year, ylab='Age', xlab='Year', main='Variation of Age by Year', col= rainbow(10))
boxplot(Data$age ~ Data$cause_of_death, ylab='Age', xlab='Cause of Death', main='Variation of Age by Cause of Death', col= rainbow(30))

#Correlation matrix
cor = hetcor(Data)
cor$correlations 
cor$correlations > 0.2 

#Build the model
model <- lm(age ~ resident_status + sex + marital_status + race + hispanic + education + cause_of_death + year, data=Data)
summary(model)$r.squared
summary(model)$adjusted.r.squared

ggplot(Data[,c('age', 'sex')], aes(x = sex, y = age)) + 
  geom_point(position = position_dodge(width = 0.4))

#Check for multicollinearity
vif = vif(model)
vif
threshold = 1/(1-summary(model)$r.squared)
threshold
threshold = max(10, threshold)
vif > threshold
#this shows that there is no multicollinearity



########################################################################
# model selection
attach(Data)

lambda = seq(0, 10, by=0.25)

# VARIABLE SELECTION
# Group Lasso  
predictors = cbind(resident_status, sex, marital_status, race, hispanic, education, cause_of_death)
index <- c(4,2,5,4,2,3,42)
grouplasso_model <- grplasso(y=Data$age,predictors, lambda=lambda, model=LinReg(), index=index,center=T, standardize=T)
summary(grouplasso_model)
grouplasso_model$coefficients
# None excluded

# MODEL BUIDING
# MLR Full Model
max_model <- lm(age ~ resident_status + sex + marital_status + race + hispanic + education + cause_of_death, data=Data)
summary(max_model)

# SubSampling for Full model - looking for significant variables
count=1
n = nrow(Data)
p = matrix(0,nrow = length(summary(max_model)$coefficients[,4]),ncol = 100)

while (count<101) {
  set.seed(100)
  subsample = sample(n, ceiling(n*0.02), replace = FALSE)
  subdata = Data[subsample,]
  submod = lm(age ~ resident_status + sex + marital_status + race + hispanic + education + cause_of_death, data=subdata)
  p[,count] = summary(submod)$coefficients[,4]
  count= count + 1
}
p[p>0.01]
# All significant at 0.05 level
summary(submod)

# CHECKING ASSUMPTIONS - FULL MODEL
full.resid = rstandard(max_model)
fits = max_model$fitted
cook = cooks.distance(max_model)
par(mfrow =c(1,1))
plot(fits, full.resid, xlab="Fitted Values", ylab="Residuals", main="Scatter Plot")
abline(0,0,col='blue')
plot(cook, type="h", lwd=3, col="red", ylab = "Cook's Distance", main="Cook's Distance")
qqPlot(full.resid, ylab="Residuals", main = "QQ Plot")
hist(full.resid, xlab="Residuals", main = "Histogram")



# LOG TRANSFORMATION OF AGE (RESPONSE) TO SEE IF FIT IMPROVES
log_model <- lm(log(age) ~ resident_status + sex + marital_status + race + hispanic + education + cause_of_death, data=Data)
summary(log_model)

# CHECKING MODEL ASSUMPTIONS - LOG TRANFORMED MODEL
log.resid = rstandard(log_model)
fits = log_model$fitted
plot(fits, log.resid, xlab="Fitted Values", ylab="Residuals", main="Scatter Plot")
abline(0,0,col='red')
qqPlot(log.resid, ylab="Residuals", main = "QQ Plot")
hist(log.resid, xlab="Residuals", main = "Histogram")


# MIXED EFFECTS MODEL
Data = within(Data,race<-relevel(race,ref='White'))
mixed_model <- lmer(age ~ resident_status + sex + marital_status + race + hispanic + education + cause_of_death+ (1|year), data=Data)
summary(mixed_model)
# vcov(mixed_model)

# CHECKING MODEL ASSUMPTIONS - MIXED EFFECTS MODEL
# Fitted Values VS Residuals
mixed.resid = resid(mixed_model)
plot(mixed_model,xlab="Fitted Values", ylab="Residuals")
qqPlot(mixed.resid, ylab="Residuals", main = "QQ Plot")
# qqnorm(mixed.resid, ylab="Residuals", main = "QQ Plot")
qqline(mixed.resid)
hist(mixed.resid, xlab="Residuals", main = "Histogram")


# MIXED EFFECTS MODEL WITH SUBSET OF DATA with AGE > 40
mixed_age40 <- lmer(age ~ resident_status + sex + marital_status + race + hispanic + education + cause_of_death+ (1|year), data=subdata[subdata$age>=40, ])
summary(mixed_age40)
mixed_age40.resid = resid(mixed_age40)
plot(mixed_age40,xlab="Fitted Values", ylab="Residuals")
abline(0,0,col="purple")
qqPlot(mixed_age40.resid, ylab="Residuals", main = "QQ Plot")
hist(mixed_age40.resid, xlab="Residuals", main = "Histogram")

# ANOVA
reduced.resident <- lm(age~resident_status,data=subdata[subdata$age>=40, ])
anova(mixed_age40, reduced.resident)

reduced.sex <- lm(age~resident_status + sex,data=subdata[subdata$age>=40, ])
anova(reduced.resident, reduced.sex)

reduced.marital_status <- lm(age~resident_status + sex + marital_status,data=subdata[subdata$age>=40, ])
anova(reduced.sex, reduced.marital_status)

reduced.race <- lm(age~resident_status + sex + marital_status + race,data=subdata[subdata$age>=40, ])
anova(reduced.marital_status, reduced.race)

reduced.education <- lm(age~resident_status + sex + marital_status + race + hispanic,data=subdata[subdata$age>=40, ])
anova(reduced.race, reduced.education)

reduced.hispanic <- lm(age~resident_status + sex + marital_status + race + hispanic,data=subdata[subdata$age>=40, ])
anova(reduced.race, reduced.hispanic)

# MIXED EFFECTS MODEL ON POPULATIONS WITH AGE <80
mixed_age80 <- lmer(age ~ resident_status + sex + marital_status + race + hispanic + education + cause_of_death+ (1|year), data=subdata[subdata$age<=80, ])
summary(mixed_age80)
mixed_age80.resid = resid(mixed_age80)
plot(mixed_age80,xlab="Fitted Values", ylab="Residuals")
abline(0,0,col="purple")
qqPlot(mixed_age80.resid, ylab="Residuals", main = "QQ Plot")
hist(mixed_age80.resid, xlab="Residuals", main = "Histogram")


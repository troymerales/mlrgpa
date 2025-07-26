install.packages('olsrr')
install.packages('corrplot')
install.packages('ggplot2')
install.packages('car')
install.packages('tidyverse')

library(olsrr)
library(corrplot)
library(ggplot2)
library(car)
library(tidyverse)

student_data<-read.csv("Student_performance_data.csv")
head(data)

# Setting up dummy variables
student_data$Gender <- factor(student_data$Gender)
student_data$Ethnicity <-factor(student_data$Ethnicity)
student_data$ParentalEducation <-factor(student_data$ParentalEducation)
student_data$Tutoring <-factor(student_data$Tutoring)
student_data$ParentalSupport <-factor(student_data$ParentalSupport)
student_data$Extracurricular <-factor(student_data$Extracurricular)
student_data$Sports <-factor(student_data$Sports)
student_data$Music <-factor(student_data$Music)
student_data$Volunteering <-factor(student_data$Volunteering)

# Renaming variables
ID <-student_data$StudentID
Age <-student_data$Age
Gender <-student_data$Gender
Ethnicity <-student_data$Ethnicity
ParentalEducation <-student_data$ParentalEducation
StudyTime <-student_data$StudyTimeWeekly
Absences <-student_data$Absences
Tutor <-student_data$Tutoring
ParentalSupport <-student_data$ParentalSupport
Extracurricular <-student_data$Extracurricular
Sports <-student_data$Sports
Music <-student_data$Music
Volunteering <-student_data$Volunteering
GPA <-student_data$GPA

contrasts(Gender)
contrasts(Ethnicity)
contrasts(ParentalEducation)
contrasts(Tutor)
contrasts(ParentalSupport)
contrasts(Extracurricular)
contrasts(Sports)
contrasts(Music)
contrasts(Volunteering)

# Initial model
full_model <- lm(GPA ~ Absences + Age + Ethnicity + Extracurricular + Gender + Music + ParentalEducation + ParentalSupport + Sports + StudyTime + Tutor + Volunteering)
summary(full_model)

## If p-value is very small (with */**/***) => significant, meaning that it affects the GPA of students



# 1. Checking for linearity

# IV vs DV
plot(ParentalSupport, GPA, xlab = 'ParentalSupport', ylab = 'GPA',  main = 'Scatter Plot of Parental vs GPA')
plot(Age, GPA, xlab = 'Age', ylab = 'GPA',  main = 'Scatter Plot of Age vs GPA')
plot(Gender, GPA, xlab = 'Gender', ylab = 'GPA',  main = 'Scatter Plot of Gender vs GPA')
plot(Ethnicity, GPA, xlab = 'Ethnicity', ylab = 'GPA',  main = 'Scatter Plot of Ethnicity vs GPA')
plot(ParentalEducation, GPA, xlab = 'Parental Education', ylab = 'GPA',  main = 'Scatter Plot of Parental Education vs GPA')

plot(StudyTime, GPA, xlab = 'Study Time', ylab = 'GPA',  main = 'Scatter Plot of Study Time vs GPA')
abline(lm(GPA ~ StudyTime), col = "blue")

plot(Absences, GPA, xlab = 'Absences', ylab = 'GPA',  main = 'Scatter Plot of Absences vs GPA')
abline(lm(GPA ~ Absences), col = "blue")

plot(Tutor, GPA, xlab = 'Tutor', ylab = 'GPA',  main = 'Scatter Plot of Tutoring vs GPA')
plot(Extracurricular, GPA, xlab = 'Extracurricular', ylab = 'GPA',  main = 'Scatter Plot of Extracurricular vs GPA')
plot(Sports, GPA, xlab = 'Sports', ylab = 'GPA',  main = 'Scatter Plot of Sports vs GPA')
plot(Music, GPA, xlab = 'Music', ylab = 'GPA',  main = 'Scatter Plot of Music vs GPA')
plot(Volunteering, GPA, xlab = 'Volunteering', ylab = 'GPA',  main = 'Scatter Plot of Volunteering vs GPA')

# Residuals vs IV
plot(Age, resid(full_model), xlab = 'Age', ylab = 'Residuals', main = 'Scatter Plot of Age vs Residuals')
plot(Gender, resid(full_model), xlab = 'Gender', ylab = 'Residuals', main = 'Scatter Plot of Gender vs Residuals')
plot(Ethnicity, resid(full_model), xlab = 'Ethnicity', ylab = 'Residuals', main = 'Scatter Plot of Ethnicity vs Residuals')
plot(ParentalEducation, resid(full_model), xlab = 'ParentalEducation', ylab = 'Residuals', main = 'Scatter Plot of ParentalEducation vs Residuals')
plot(StudyTime, resid(full_model), xlab = 'StudyTime', ylab = 'Residuals', main = 'Scatter Plot of StudyTime vs Residuals')
plot(Absences, resid(full_model), xlab = 'Absences', ylab = 'Residuals', main = 'Scatter Plot of Absences vs Residuals')
plot(Tutor, resid(full_model), xlab = 'Tutor', ylab = 'Residuals', main = 'Scatter Plot of Tutor vs Residuals')
plot(ParentalSupport, resid(full_model), xlab = 'ParentalSupport', ylab = 'Residuals', main = 'Scatter Plot of ParentalSupport vs Residuals')
plot(Extracurricular, resid(full_model), xlab = 'Extracurricular', ylab = 'Residuals', main = 'Scatter Plot of Extracurricular vs Residuals')
plot(Sports, resid(full_model), xlab = 'Sports', ylab = 'Residuals', main = 'Scatter Plot of Sports vs Residuals')
plot(Music, resid(full_model), xlab = 'Music', ylab = 'Residuals', main = 'Scatter Plot of Music vs Residuals')
plot(Volunteering, resid(full_model), xlab = 'Volunteering', ylab = 'Residuals', main = 'Scatter Plot of Volunteering vs Residuals')
plot(GPA, resid(full_model), xlab = 'GPA', ylab = 'Residuals', main = 'Scatter Plot of GPA vs Residuals')

# 2. Checking for normality
qqnorm(resid(full_model))
qqline(resid(full_model), col = 'red')
shapiro.test(resid(full_model))

## Not normal -- Transform variables for normality
student_data$logStudyTime <- log(student_data$StudyTime)
logStudyTime <- student_data$logStudyTime
new_model <- lm(GPA ~ Absences + Age + Ethnicity + Extracurricular + Gender + Music + ParentalEducation + ParentalSupport + Sports + logStudyTime + Tutor + Volunteering)
summary(new_model)
shapiro.test(resid(new_model))

## Now has p-value of 0.7762 > 0.05 = normally distributed

qqnorm(resid(new_model))
qqline(resid(new_model), col = 'red')

#  Variable selection
fwd.fit <- ols_step_forward_p(new_model, penter = 0.05, details = TRUE)
bwd.fit <- ols_step_backward_p(new_model, prem = 0.05, details = TRUE)

## Removed and added variables are the same for both forward and backward selection

new1_model <- lm(GPA ~ Absences + Age + Extracurricular + Gender + Music + ParentalSupport + Sports + logStudyTime + Tutor + Volunteering)
summary(new_model1)

# Check for normality (again)
shapiro.test(resid(new1_model))
qqnorm(resid(new1_model))
qqline(resid(new1_model), col = 'red')

# 3. Checking for Homoscedasticity

new1_model <- lm(GPA ~ Absences + Age + Gender + Music + ParentalSupport + Sports + logStudyTime + Tutor)
#new1_model <- lm(GPA ~ Absences + Age + Extracurricular + Music + Sports + log(StudyTime) + Tutor + Volunteering)
summary(new1_model)

shapiro.test(resid(new1_model))
ncvTest(new1_model)
plot(new1_model)

# 4. Checking for independence of error terms

dw <- durbinWatsonTest(new1_model)
dw

# 5. Checking for multicollinearity

v <- vif(new1_model)
v

# 6. Checking for outliers

cooks <- cooks.distance(new1_model)
cooks
plot(new1_model,which=4)
cutoff <- 4/(2392-10-1)
abline(h=cutoff,lty=2, col="blue")
summary(new1_model)

new_data<-student_data[-c(2012,620),]
#colSums(is.na(new_data))
#str(new_data)

new2_model<-lm(new_data$GPA ~ new_data$Absences + new_data$Age + new_data$Gender + new_data$Music + new_data$ParentalSupport + new_data$Sports + new_data$logStudyTime + new_data$Tutor)

#new2_model <- lm(GPA ~ Absences + Age + Gender + Music + ParentalSupport + Sports + logStudyTime + Tutor, data = new_data)

plot(new2_model,which=4)
cutoff <- 4/(2390-10-1)
abline(h=cutoff,lty=2, col="blue")
summary(new2_model)

shapiro.test(resid(new2_model))
ncvTest(new2_model)
dw1 <- durbinWatsonTest(new2_model)
dw1
v1 <- vif(new2_model)
v1

summary(new2_model)


#View(student_data)
#str(full_model)

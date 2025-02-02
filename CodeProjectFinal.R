install.packages("readxl")
library(readxl)

#import data
setwd("/Users/thanchanok/Documents/Ana2/Project")
medCost.df <- read_excel("medical_cleaned.xlsx")


#PartI
#Full model
medCost.lm <- lm(formula = charges ~ age + 
                   relevel(factor(sex), ref = "female") + 
                   bmi + 
                   children + 
                   relevel(factor(smoker), ref = "no") + 
                   relevel(factor(region), ref = "northwest"), 
                 data = medCost.df)
medCost.lm
summary(medCost.lm)


#check assumption for full model
library(car)
vif(medCost.lm)

plot(medCost.lm)

residuals <- residuals(medCost.lm)
hist(residuals, breaks = 20, main = "Histogram of Residuals", xlab = "Residuals")


#find best subset
medCost.df$sex <- factor(medCost.df$sex)
medCost.df$region <- factor(medCost.df$region)
medCost.df$smoker <- factor(medCost.df$smoker)
medCost.lm <- lm(charges ~ age + bmi + children + smoker + region + sex, data = medCost.df)
library(olsrr)
best_subset <- ols_step_best_subset(medCost.lm)
print(best_subset)



#PartII
#we select model 5
bestMedCost.lm <-lm(formula = charges ~ age + 
                      bmi + 
                      children + 
                      relevel(factor(smoker), ref = "no") + 
                      relevel(factor(region), ref = "northwest"),data = medCost.df)
summary(bestMedCost.lm)

#check assumption for best model
vif(bestMedCost.lm)
plot(bestMedCost.lm)

residuals <- residuals(bestMedCost.lm)
hist(residuals, breaks = 20, main = "Histogram of Residuals", xlab = "Residuals")


#Tranformation on y
#model1 log tranform on y
bestMedCost.lm1 <-lm(formula = log(charges) ~ age + 
                       bmi + 
                       children + 
                       relevel(factor(smoker), ref = "no") + 
                       relevel(factor(region), ref = "northwest"),data = medCost.df)
summary(bestMedCost.lm1)

#model2
bestMedCost.lm2 <- lm( formula = log(charges) ~ log(age) + 
                         log(bmi) + 
                         children + 
                         relevel(factor(smoker), ref = "no") + 
                         relevel(factor(region), ref = "northwest"),data = medCost.df)
summary(bestMedCost.lm2)

#model3
bestMedCost.lm3 <- lm(
  formula = charges ~ age + I(age^2) +
    bmi + I(bmi^2) + children+
    I(children^2) +
    relevel(factor(smoker), ref = "no") + 
    relevel(factor(region), ref = "northwest"),data = medCost.df
)
summary(bestMedCost.lm3)

#model4 square root transform (very high r square)
bestMedCost.lm4 <-lm(formula = sqrt(charges) ~ age + 
                       bmi + 
                       children + 
                       relevel(factor(smoker), ref = "no") + 
                       relevel(factor(region), ref = "northwest"),data = medCost.df)
summary(bestMedCost.lm4)

vif(bestMedCost.lm4)
plot(bestMedCost.lm4)
residuals <- residuals(bestMedCost.lm4)
hist(residuals, breaks = 20, main = "Histogram of Residuals", xlab = "Residuals")

install.packages("lm.beta")
library(lm.beta)
lm.beta(bestMedCost.lm4)

#model5
bestMedCost.lm5 <-lm(formula = log(charges) ~ sqrt(age) + 
                       sqrt(bmi) + 
                       sqrt(children) + 
                       relevel(factor(smoker), ref = "no") + 
                       relevel(factor(region), ref = "northwest"),data = medCost.df)
summary(bestMedCost.lm5)

#model6
bestMedCost.lm6 <-lm(formula = sqrt(charges) ~ age + I(age^2) +
                       bmi + I(bmi^2)+
                       children + I(children^2) +
                       relevel(factor(smoker), ref = "no") + 
                       relevel(factor(region), ref = "northwest"),data = medCost.df)
summary(bestMedCost.lm6)

#model7 square root transform on y and centering on x
attach(medCost.df)
age.c <- age-mean(age)
bmi.c <- bmi-mean(bmi)
children.c <- children-mean(children)
bestMedCost.lm7 <-lm(formula = sqrt(charges) ~ age.c + 
                       bmi.c + 
                       children.c + 
                       relevel(factor(smoker), ref = "no") + 
                       relevel(factor(region), ref = "northwest"),data = medCost.df)
summary(bestMedCost.lm7)

#model8 log transform on y and centering on x
bestMedCost.lm8 <-lm(formula = log(charges) ~ age.c + 
                       bmi.c + 
                       children.c + 
                       relevel(factor(smoker), ref = "no") + 
                       relevel(factor(region), ref = "northwest"),data = medCost.df)
summary(bestMedCost.lm8)



#Part III
#model with interaction term
reduceModelInteraction.lm <- lm(formula = charges ~
                              age *
                              bmi *
                              children *
                              smoker *
                              region,
                            data = medCost.df)

summary(reduceModelInteraction.lm)

#check assumption for Model with Interaction term
library(car)
vif(reduceModelInteraction.lm)
plot(reduceModelInteraction.lm)

residuals <- residuals(reduceModelInteraction.lm)
hist(residuals, breaks = 20, main = "Histogram of Residuals", xlab = "Residuals")

#solve multicollinearity by centering
age.c <- age-mean(age)
bmi.c <- bmi-mean(bmi)
children.c <- children-mean(children)


reduceModelInteraction.lm4 <- lm(formula = charges ~ age.c * bmi.c *children.c *smoker*region, data=medCost.df)
summary(reduceModelInteraction.lm4)

vif(reduceModelInteraction.lm4)
plot(reduceModelInteraction.lm4)
residuals <- residuals(reduceModelInteraction.lm4)
hist(residuals, breaks = 20, main = "Histogram of Residuals", xlab = "Residuals")

#Standardized Coefficients
install.packages("lm.beta")
library(lm.beta)
std_coeff <- lm.beta(reduceModelInteraction.lm4)
std_coeff_values <- coef(std_coeff)
std_coeff_values_no_intercept <- std_coeff_values[-1] 
ranked_coeff <- sort(abs(std_coeff_values_no_intercept), decreasing = TRUE)
print(ranked_coeff)

#End
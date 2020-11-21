# Title     : TODO
# Objective : TODO
# Created by: madelineluong
# Created on: 11/20/20
library(psych)
library(standardize)
library(dplyr)
library(MVN)
library(MASS)
library(car)

attach(developing2014)
environmentName(environment(vif))
#View(developing2014)


#dropping null values
developing2014<-na.omit(developing2014)
dim(developing2014)
#dropping unneeded values
developingdata<- developing2014[,c(-1)]
developingdata<- data.frame(developingdata)
#correlation
pairs.panels(developingdata)


#regression model
DD.lm<-lm(developingdata$Life.expectancy~.,data = developingdata)
summary(DD.lm) # Income.composition.of.resources r = 0.7897
anova(DD.lm) #Income.composition.of.resources,BMI,percentage.expenditure, and Alcohol
hist(DD.lm$residuals, main = "Histogram Linear Model Residual", xlab = "Residuals")
vif(DD.lm) # many values are highly correlated. might take out income or schooling
plot(DD.lm) # outliers are the lower countries

#taking out schooling
developingw_school<- developingdata[,-c(8)]
#linear model without schooling
DDW.lm<- lm(developingw_school$Life.expectancy~., data = developingw_school)
summary(DDW.lm)
plot(DDW.lm)

#boxcox without schooling
DDW.bc<- boxcox(DDW.lm, lambda = seq(-3,3))
bestw.lam<- DDW.bc$x[which(DDW.bc$y == max(DDW.bc$y))]
bestw.lam #2.454545 best lam value

#polynomial
model.BMI <- lm((developingw_school$Life.expectancy)^best.lam ~ poly(developingw_school$BMI,3))
summary(model.BMI) # linear

model.total <- lm((developingw_school$Life.expectancy)^best.lam ~ poly(developingw_school$Total.expenditure,3))
summary(model.total) # linear but bad

model.alcohol <- lm((developingw_school$Life.expectancy)^best.lam ~ poly(developingw_school$Alcohol,3))
summary(model.alcohol) # linear

model.GDP <- lm((developingw_school$Life.expectancy)^best.lam ~ poly(developingw_school$GDP,3))
summary(model.GDP) #linear

model.income <- lm((developingw_school$Life.expectancy)^best.lam ~ poly(developingw_school$Income.composition.of.resources,3))
summary(model.income) #linear


# back and forward model without schooling
fullmodel<- lm((developingw_school$Life.expectancy)^bestw.lam~.,data=developingw_school)
startmodel<-lm((developingw_school$Life.expectancy)^bestw.lam~ 1, data = developingw_school)
step(fullmodel,direction = "backward")
step(startmodel,direction = "forward",scope = formula(fullmodel))
step(fullmodel,direction = "both")
step(startmodel, direction = "both", scope = formula(fullmodel))

#linear model after backward / forward model with boxcox
DDback_forward.lm<- lm((developingw_school$Life.expectancy)^bestw.lam~developingw_school$Income.composition.of.resources+
                       developingw_school$Total.expenditure+developingw_school$percentage.expenditure, data = developingw_school)
summary(DDback_forward.lm)
plot(DDback_forward.lm)






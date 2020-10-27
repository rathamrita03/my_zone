getwd
setwd("F:\\DATA SCIENCE\\IMS Term 1\\Capstone")
library(MASS)
install.packages('readr')
install.packages('ggplot2')
install.packages('mlbench')
install.packages('corrplot')
install.packages('Amelia')
install.packages('caret')
install.packages('plotly')
install.packages('caTools')
install.packages('reshape2')
install.packages('dplyr')
library(readr)
library(ggplot2)
library(corrplot)
library(mlbench)
library(Amelia)
library(plotly)
library(reshape2)
library(caret)
library(caTools)
library(dplyr)
library(car)

Housing <- read.csv("HousingData.csv")
summary(Housing)

#replacing the NA values for variable CRIM mean Value
Housing$CRIM[is.na(Housing$CRIM)] = 3.61187

Housing$ZN[is.na(Housing$ZN)]= 11.21

Housing$INDUS[is.na(Housing$INDUS)] = 11.08

Housing$CHAS[is.na(Housing$CHAS)] = 0.06996

Housing$AGE[is.na(Housing$AGE)] = 68

Housing$LSTAT[is.na(Housing$LSTAT)] = 12.715

summary(Housing)

missmap(Housing,col=c('yellow','black'),y.at=1,y.labels='',legend=TRUE,main = "Missingness in Housing Dataset")

#draws a map of the missingness in a dataset using the image function

#correlation plot and Analysis
H = cor(Housing)
corrplot(H,method = "circle",title = "Correlation Matrix")

#taken all the parameters in Model.1
Model.1=lm(MEDV~CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO+B+LSTAT,data = Housing)

summary(Model.1)

vif(lm(MEDV~CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO+B+LSTAT,data = Housing))

outlierTest(Model.1)

Housing = Housing[-c(370,366,371,368),]

#have removed INDUS , AGE parameters by considering the p-values

Model.2=lm(MEDV~CRIM+ZN+CHAS+RM+B+LSTAT+NOX+DIS+PTRATIO+RAD+TAX,data = Housing)

summary(Model.2)

vif(lm(MEDV~CRIM+ZN+CHAS+RM+B+LSTAT+NOX+DIS+PTRATIO+RAD+TAX,data = Housing))

#have further removed ZN, RAD & TAX

Model.3=(lm(MEDV~CRIM+CHAS+RM+B+LSTAT+DIS+NOX+PTRATIO,data = Housing))

summary(Model.3)

vif(lm(MEDV~CRIM+CHAS+RM+B+LSTAT+DIS+NOX+PTRATIO,data = Housing))

#residulas analysis

hist(Model.3$residuals, xlab = "Residuals", main = "Histogram of Residuals", col="blue")

qqnorm(Model.3$residuals,main = "Normal Probability Plot", pch = 21, col = "blue")

qqline(Model.3$residuals)

par(mfrow = c (2,2))

plot(Model.3)

#analysis of Variance or anova Table
anova(Model.3,Model.2)
aov(Model.3)



#https://www.datacamp.com/community/tutorials/linear-regression-R

library(readxl)

#import excel data
ageandheight <- read_excel("ageandheight.xls", sheet = "Hoja2") #Upload the data

#fix incorrect data point
ageandheight[7, 'height'] = 79.9

#run regression
lmHeight = lm(height~age, data = ageandheight) #Create the linear regression

#show summary data
summary(lmHeight) #Review the results


lmHeight2 = lm(height~age + no_siblings, data = ageandheight) #Create a linear regression with two variables
summary(lmHeight2) #Review the results

pressure <- read_excel("pressure.xlsx") #Upload the data
lmTemp = lm(Pressure~Temperature, data = pressure) #Create the linear regression
plot(pressure, pch = 16, col = "blue") #Plot the results
abline(lmTemp) #Add a regression line

summary(lmTemp)

plot(lmTemp$residuals, pch = 16, col = "red")

lmTemp2 = lm(Pressure~Temperature + I(Temperature^2), data = pressure) #Create a linear regression with a quadratic coefficient
summary(lmTemp2) #Review the results

plot(lmTemp2$residuals, pch = 16, col = "red")

ageandheight[2, 2] = 7.7
head(ageandheight)

lmHeight3 = lm(height~age, data = ageandheight)#Create the linear regression
summary(lmHeight3)#Review the results
plot(cooks.distance(lmHeight3), pch = 16, col = "blue") #Plot the Cooks Distances.
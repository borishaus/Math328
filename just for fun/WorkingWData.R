airforcedata$`ID Gender` <- as.factor(airforcedata$`ID Gender`) # changing to categories 1 m 2 f
airforcedata$`ID PUMS Occupation` <- as.factor(airforcedata$`ID PUMS Occupation`)  # changing to


plot(airforcedata$`Total Population`,airforcedata$`Average Wage`)
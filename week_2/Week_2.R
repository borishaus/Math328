#BACLARK Week 2 Discussion
airforcedata<-GenderData_Jobs_Air_Force
airforcedata$`ID Gender` <- as.factor(airforcedata$`ID Gender`) # changing to categories
airforcedata$`ID PUMS Occupation` <- as.factor(airforcedata$`ID PUMS Occupation`)  # changing to categories

# PART 1
twowaytable<-table(airforcedata$`ID Gender`, airforcedata$`Year Enlisted` )
#copy and paste into post
#A: This table describes the population of  males (1) and females (2). There was nearly the same ratio of males to females in 2014 and 2015. Then in 2017 there were more females than males. In 2018, there were many more males than females. The mean number of males to females across all four years is described by tapply(..., mean); the mean number of males is slightly higher than females. The second tapply(...,median) describes the median number of males to females; the values are roughly the same but still higher median males. The third tapply(..., sd) shows that there was more variability in the number of females than the number of males.  
print("PART 1 TWO WAY TABLE")
print(twowaytable)

#Printing the requested tapply
print("PART 1 TAPPLYs")
print("tapply(airforcedata$`Average Wage`, airforcedata$Gender, mean)")
print(tapply(airforcedata$`Average Wage`, airforcedata$Gender, mean))
print("tapply(airforcedata$`Average Wage`, airforcedata$Gender, median)")
print(tapply(airforcedata$`Average Wage`, airforcedata$Gender, median))
print("tapply(airforcedata$`Average Wage`, airforcedata$Gender, sd)")
print(tapply(airforcedata$`Average Wage`, airforcedata$Gender, sd))

#Gather random sample.Commented after first run.

Sample <- airforcedata[sample(nrow(airforcedata),30,replace=FALSE,prob=NULL),]
#PART 2a
#What is the probability that a randomly selected wage from the population, (i.e., the original data), is higher than $65,000?
#A: This was calculated by taking the total population that fell above $65K and dividing by the total number in the population. The result was an 18% probability that a randomly selected wage would be above $65K.
p2a <- round(length(airforcedata$`Average Wage`[airforcedata$`Average Wage`>65000])/length(airforcedata$`Average Wage`),4)
print(paste("part 2a - " , p2a))

#PART 2b
#What percentage of the population wages, (i.e., the original data), are between $70,000 and $80,000 (exclusive)?
#A: This comes out to a 6.1% percentage of the population that falls between $70k and $80K (exclusive). This was determined by adding the population that falls above $70k (exclusive) and subtracting the population that falls above $80k (inclusive).
p2b <- sprintf("%0.1f%%",round(length(airforcedata$`Average Wage`[airforcedata$`Average Wage`>70000])/length(airforcedata$`Average Wage`)-length(airforcedata$`Average Wage`[airforcedata$`Average Wage`>=80000])/length(airforcedata$`Average Wage`),4)*100)
print(paste("part 2b - " , p2b))

#PART 3A
# Randomly select 1000  samples; each of size 30 from the population 'Average Wage'.  Then calculate the mean and the variance in each of the 1000 samples.  Display a density histogram for the sample means and one for the sample variances.  Comment on your distribution, graph and numbers.  Summarize what you see.  Here is some code that will help with 3a).
N<-1000
Y = matrix(nrow = 30, ncol = N)
Means <- c()
Variances<-Means
init<-TRUE # will not initialize unless TRUE
if(init){
  for(i in 1:N){
    Y[,i]<-sample(airforcedata$`Average Wage`,30)
    Means[i]<-mean(Y[,i]/1)
    Variances[i] <- var(Y[,i])
  }
}
hist(Means, freq=F, main ='3A - Distribution of the Sample Means')

lines(density(Means), lwd=1, col='red') 
options(scipen=999)
hist(Variances, freq=F, main ='3A - Distribution of the Sample Variances')
lines(density(Variances), lwd=1, col='red')
options(scipen=999)

#PART 3B 
#Part 3b:  What is the probability that a randomly selected sample mean is greater than $50,000?

p3b = round(
  length(Means[Means>50000])/length(Means),
  4
) 
print(paste("part 3b - " , p3b))

# remember to interpret

# PART 3C
p3c = round(
  (length(Means[Means>45000]) - length(Means[Means>=55000]))/length(Means),
  4
)
print(paste("part 3c - " , p3c))









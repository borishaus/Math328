#A series of 20 jobs arrive at a computing center with 50 processors. Assume that each of the jobs is equally likely to go through any of the processors.
jobs<-c(1:20) # represents the 20 unique but equally likely jobs to arrive at the processors
processes<-c(1:50)


# a) 	Find the probability that a processor is used at least twice.
# This is equal to the probability of the sample space (1) minus the probability of a processor being selected 0 or 1 times. 
p1a<- (1 - (
         (49/50)^20 # probability that a single processor is not selected
         + (1/50)*(49/50)^19) # probability that a single processor is selected once
       )

# b) 	What is the probability that at least one processor is idle?
# A: 100%. There are only 20 jobs and there are 50 processors. At least 30 processors will be idle. 

# c) 	If any processor can handle at most four jobs without being overloaded, what is the probability of an overload?
# This probability is equal to the probability of all outcomes minus the probability of a processor being selected 0, 1, 2, or 3 times. 

p1c <- 1  -(
    (49/50)^20
    +(1/50)*((49/50)^19) # once
    + ((1/50)^2)*((49/50)^18) # twice
    + ((1/50)^3)*((49/50)^17) # three times
  )

# PROBELM 2
set.seed(99609)
p2set<-1:100000

outcomes<-c()

for(i in 1:1000){
  e1set<-c(runif(2,1,100000))
  e1 <-c(floor(min(e1set)),ceiling(max(e1set)))
  e2set<-c(runif(2,1,100000))
  e2 <-c(floor(min(e2set)),ceiling(max(e2set)))
  e3set<-c(runif(2,1,100000))
  e3 <-c(floor(min(e3set)),ceiling(max(e3set)))

  p2answerFormula<-(
    length(p2set[p2set>=e1[1]&p2set<=e1[2]]) + #e1
      length(p2set[p2set>=e2[1]&p2set<=e2[2]]) + #e2
      length(p2set[p2set>=e3[1]&p2set<=e3[2]]) - #e3
      length(p2set[(p2set>=e1[1]&p2set<=e1[2])&(p2set>=e2[1]&p2set<=e2[2])])- # e1 & e2
      length(p2set[(p2set>=e3[1]&p2set<=e3[2])&(p2set>=e2[1]&p2set<=e2[2])])- # e2 & e3
      length(p2set[(p2set>=e1[1]&p2set<=e1[2])&(p2set>=e3[1]&p2set<=e3[2])]) + # e3 & e1
      length(p2set[(p2set>=e1[1]&p2set<=e1[2])&(p2set>=e2[1]&p2set<=e2[2])&(p2set>=e3[1]&p2set<=e3[2])]) # e1 & e2 &e3
  )/length(p2set)
  p2answerLogic <- length(p2set[(p2set>=e1[1]&p2set<=e1[2])|(p2set>=e2[1]&p2set<=e2[2])|(p2set>=e3[1]&p2set<=e3[2])])/length(p2set) # logical formula
  outcomes<-append(outcomes,p2answerFormula-p2answerLogic) # appends difference to each
}
plot(1:1000,outcomes, main = "Difference Between Given Formula and Logical Operation", ylab = "Formula - Logical")
print(paste("Number of instances not equal: ", length(outcomes[outcomes!=0])))

#In order to take the given formula from the problem in the homework and prove that it is equal to the probability of set1 or set2 or set3 the below R code was created. In this instance a simulation was ran with three completely different subsets of sequential numbers over 1000 iterations. It was found that over 1000 iterations there was not an instance where the formula did not prove to be true. 

#Problem 3
#A box contains four 40-W, five 60-W, and 6 75-W light bulbs. If bulbs are selected one by one in random order, what is the probability that at least two bulbs should be selected to obtain one that is rated 75-W? 
#Answer: This problem is worded in a way that is extremely difficult for me to understand. I found a question posted on a website asking "Suppose now that bulbs are to be selected one by one until a 75-W bulb is found. What is the probability that it is necessary to examine at least six bulbs" and I think this is the aim of the question. Wording in a way I can understand, I believe the question is asking: "What is the probability that it is necessary to examine at least two bulbs to obtain one that is a 75-W light bulb." There are only 15 bulbs to choose from and 6 of them are 75W. This means that after the 9th bulb is selected,  if a 75W bulb is not yet selected, a 75 W bulb will be selected. The probability that it takes one attempt is 6/15. The probability that it takes two attempts is (9/15)*(6/14). The probability that it takes three is (9/15)*(8/14)*(6/13) and so on. Continuing on this encompasses the entire sample space. Therefore the probability that it takes more than 2 for one 75W is the probability of the sample space (1) minus the probability of a 75 W on the first selection. This is 1- 6/15 which is 0.6. 
p3 <- 1-(6/15)
print(paste("P(sample space) - P(17W on first sample) = 1-.4 = ", p3))
  
  














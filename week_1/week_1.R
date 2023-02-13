#new script for week 1 homework, hopefully not submitted too late.   
#part 4A
y <- c()
x <- c()
for(i in 1:100){
  x<-append(x,i^3)
}
for(j in 1:100){
  y <- append(y,x[j]-mean(x))
}
print(mean(y)) # outputs 0
print(sd(x)-sd(y)) # outputs 0

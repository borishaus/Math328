
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
print("printing the mean of y and std x - std y")
print(mean(y)) # outputs 0
print(sd(x)-sd(y)) # outputs 0


#part 4B
x <- runif(1000,-1000,2000)
z <- c()
for(i in 1:1000){
  z <- append(z, (x[i]-mean(x))/sd(x))
}
print("printing the mean of z and sd of z")
print(mean(z))
print(sd(z))

#part 4C
x <- runif(1000,-7000, 7000)
w <- c()
M <- min(x)
N<- max(x)
for(i in 1:1000){
  top <- x[i]-M
  bot <- N-M
  w <- append(w,top/bot)
}
print("printing min w")
print(min(w))
print("printing max w")
print(max(w))
print("printing part b: mean(w)==(mean(x)-M)/(N-M)")
print(mean(w)==(mean(x)-M)/(N-M))
print("printing part c: sd(w)==sd(x)/(N-M)")
print(sd(w)==sd(x)/(N-M))


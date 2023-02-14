pall<-c()
x<-c()
y<-c()
ct<-30000/4

for (i in 1:ct) {
  pall<-append(pall,'red')
}

for (i in 1:ct) {
  pall<-append(pall,'blue')
}

for (i in 1:ct) {
  pall<-append(pall,'yellow')
}

for (i in 1:ct) {
  pall<-append(pall,'yellow')
}

x<- runif(30000,0,1)

for(i in 1:30000){
  y<-append(y,i)
}
plot(x,y, col=pall, main="COLOMBIA")



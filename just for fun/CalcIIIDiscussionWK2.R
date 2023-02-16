pall<-c()
its <- 100 # how many iterations to perform
rat <- 9 # divides x by this number
y<-c()
x<-(1:its)
showSeries<-FALSE #plot series (TRUE) or sequence (FALSE)
ct<-ceiling(its/3) # setting up colors
for (i in 1:ct) {
  pall<-append(pall,'brown2')
}

for (i in 1:ct) {
  pall<-append(pall,'brown3')
}

for (i in 1:ct) {
  pall<-append(pall,'brown4')
}
func<-function(xIn){
  b<-xIn/rat # b stands for x after ratio is applied
  yout<-log(b, base = 100)
  return(yout)
}
ySer <- c(0)
for(i in 1:its){
  y<-append(y,func(x[i]))
  if(i!=1&&showSeries){
    ySer<-append(ySer,y[i]+ySer[i-1])
  }
}
# add type = "l" to see a line graph 
if(showSeries){
  plot(x,ySer, main = "Series", col=pall, pch=20, cex=1)
}else{
  plot(x,y,main="Sequence", col=pall, pch=20, cex=1)
}



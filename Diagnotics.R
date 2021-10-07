x=1
y=runif(400,-0.5,0.5)
for( i in 1:400){
  x=c(x,x[i]+y[i])
}

png(file='D:/Study files/Ymsc/ST955_Dissertation/papers/latex/LatexTemplate/figures/Trace2.png', width = 500,height = 300)
plot(x,type='l',ylim=c(-5,3),main='Hidg serial correlation trace plot',xlab='',ylab='')

dev.off()
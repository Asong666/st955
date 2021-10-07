png(file='D:/Study files/Ymsc/ST955_Dissertation/papers/latex/LatexTemplate/figures/10His_edge.png', width = 1000,height = 600)
par(mfrow=c(2,2))
hist(output101df$edges,xlab='',main='Histogram of Edges, Uniform',col='lightblue')
hist(ou1$edges,xlab='',xlim=c(0,10),main='Histogram of Edges, TNT',col='lightblue')
hist(output103df$edges,xlab='', main='Histogram of Edges, Sqrt',col='lightblue')
hist(output104df$edges[1:9000],xlab='', main='Histogram of Edges, Barker',col='lightblue')
dev.off()

png(file='D:/Study files/Ymsc/ST955_Dissertation/papers/latex/LatexTemplate/figures/10_traceplot.png', width = 2000,height = 600)
par(mfrow=c(1,4))
plot(log(output101df$summarystatistics),type='l',col='red',ylim=c(60,100),xlab='Markov chain',ylab='Summary Statistics')
legend("top", inset=.0, title="", c("uniform"),lty=c(1),col=c("red"),cex=3)
plot(log(output102df$summarystatistics),type='l',col='green3',ylim=c(60,100),xlab='Markov chain',ylab='Summary Statistics')
legend("top", inset=.0, title="", c("TNT"),lty=c(1),col=c("green3"),cex=3)
plot(log(output102df$summarystatistics),type='l',col='blue',ylim=c(60,100),xlab='Markov chain',ylab='Summary Statistics')
legend("top", inset=.0, title="", c("Sqrt"),lty=c(1),col=c("blue"),cex=3)
plot(log(output102df$summarystatistics),type='l',col='black',ylim=c(60,100),xlab='Markov chain',ylab='Summary Statistics')
legend("top", inset=.0, title="", c("Barker"),lty=c(1),col=c("black"),cex=3)
dev.off()


#acf
png(file='D:/Study files/Ymsc/ST955_Dissertation/papers/latex/LatexTemplate/figures/10_ACF.png', width = 2000,height = 600)

par(mfrow=c(1,4))
acf(output101df$summarystatistics,lag.max = 500,col='red',main='') #uniform
legend("topright", inset=.0, title="", c("uniform"),lty=c(1),col=c("red"),cex=4)
acf(output102df$summarystatistics,lag.max = 500,col='green3',main='') #TNT
legend("topright", inset=.0, title="", c("TNT"),lty=c(1),col=c("green3"),cex=4)
acf(output103df$summarystatistics,lag.max = 500,col='blue',main='') #sqrt
legend("topright", inset=.0, title="", c("Sqrt"),lty=c(1),col=c("blue"),cex=4)
acf(output104df$summarystatistics,lag.max = 500,col='black',main='') #barker
legend("topright", inset=.0, title="", c("Barker"),lty=c(1),col=c("black"),cex=4)
dev.off()


#acrate
acrate101=vector()
acrate102=vector()
acrate103=vector()
acrate104=vector()

for(i in 5:15){
  print(i)
  acrate101<-c(acrate101,MCMC(i,0,1000,1000)[[2]])
  acrate102<-c(acrate102,MCMC(i,1,1000,1000)[[2]])
  acrate103<-c(acrate103,MCMC_locallybalanced(5,2,1000,1000)[[2]])
  acrate104<-c(acrate104,MCMC_locallybalanced(5,3,1000,1000)[[2]])
}
par(mfrow=c(1,1))
plot(rep(5:10),acrate1,col='red',type='o',ylim=c(0,1),ylab='',xlab='')
par(new = TRUE)
plot(rep(5:10),acrate2,col='blue',type='o',ylim=c(0,1),ylab='',xlab='')
par(new=TRUE)
plot(rep(5:10),acrate3,col='green',type='o',ylim=c(0,1),ylab='',xlab='')
par(new=TRUE)
plot(rep(5:10),acrate4,col='black',type='o',ylim=c(0,1),ylab='acceptance rate',xlab='num of nodes')
legend("topleft", inset=.05, title="acceptance rate", c("uniform","TNT","sqrt",'barker'),lty=c(1, 4),col=c("red", "blue",'green','black'))


#AR ESS
png(file='D:/Study files/Ymsc/ST955_Dissertation/papers/latex/LatexTemplate/figures/10_AR_ESS.png', width = 2000,height = 600)

par(fig=c(0,0.5,0,1))
plot(rep(5:10),acrate1,col='red',type='o',ylim=c(0,1),ylab='',xlab='')
par(fig=c(0,0.5,0,1),new=TRUE)
plot(rep(5:10),acrate2,col='green3',type='o',ylim=c(0,1),ylab='',xlab='')
par(fig=c(0,0.5,0,1),new=TRUE)
plot(rep(5:10),acrate3,col='blue',type='o',ylim=c(0,1),ylab='',xlab='')
par(fig=c(0,0.5,0,1),new=TRUE)
plot(rep(5:10),acrate4,col='black',type='o',ylim=c(0,1),ylab='acceptance rate',xlab='num of nodes')
legend("topleft", inset=.05,cex=2,title="acceptance rate", c("uniform","TNT","sqrt",'barker'),lty=c(1),col=c("red", "green3",'blue','black'))
##conect
par(fig=c(0.5,1,0,1),new=TRUE)
plot(rep(5:14),listESS1,col='red',type='o',xlim=c(4,15),ylim=c(0,1000),ylab='',xlab='')
par(fig=c(0.5,1,0,1),new=TRUE)
plot(rep(5:14),listESS2[1:10],col='green3',type='o',xlim=c(4,15),ylim=c(0,1000),ylab='',xlab='')
par(fig=c(0.5,1,0,1),new=TRUE)
plot(rep(5:14),listESS3[1:10],col='blue',type='o',xlim=c(4,15),ylim=c(0,1000),ylab='',xlab='')
par(fig=c(0.5,1,0,1),new=TRUE)
plot(rep(5:14),listESS4,col='black',type='o',xlim=c(4,15),ylim=c(0,1000),ylab='Effective Sample Size',xlab='num of nodes')
legend("topleft", inset=.05,cex=2,title="ESS", c("uniform","TNT",'sqrt','arker'),lty=c(1),col=c("red", "green3",'blue','black'))
dev.off()
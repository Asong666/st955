png(file='D:/Study files/Ymsc/ST955_Dissertation/papers/latex/LatexTemplate/figures/His_edge.png', width = 1000,height = 600)
par(mfrow=c(2,2))
hist(output1df$edges,xlab='',main='Histogram of Edges, Uniform',col='lightblue')
hist(op1$edges,xlab='',xlim=c(0,10),main='Histogram of Edges, TNT',col='lightblue')
hist(output3df$edges,xlab='', main='Histogram of Edges, Sqrt',col='lightblue')
hist(output3df$edges[1:9000],xlab='', main='Histogram of Edges, Barker',col='lightblue')
dev.off()

png(file='D:/Study files/Ymsc/ST955_Dissertation/papers/latex/LatexTemplate/figures/1_traceplot.png', width = 2000,height = 600)
par(mfrow=c(1,4))
plot(log(output1df$summarystatistics),type='l',col='red',ylim=c(0,16),xlab='Markov chain',ylab='Summary Statistics')
legend("top", inset=.0, title="", c("uniform"),lty=c(1),col=c("red"),cex=3)
plot(log(output2df$summarystatistics),type='l',col='green3',ylim=c(0,16),xlab='Markov chain',ylab='Summary Statistics')
legend("top", inset=.0, title="", c("TNT"),lty=c(1),col=c("green3"),cex=3)
plot(log(output2df$summarystatistics),type='l',col='blue',ylim=c(0,16),xlab='Markov chain',ylab='Summary Statistics')
legend("top", inset=.0, title="", c("Sqrt"),lty=c(1),col=c("blue"),cex=3)
plot(log(output2df$summarystatistics),type='l',col='black',ylim=c(0,16),xlab='Markov chain',ylab='Summary Statistics')
legend("top", inset=.0, title="", c("Barker"),lty=c(1),col=c("black"),cex=3)
dev.off()


#acf
png(file='D:/Study files/Ymsc/ST955_Dissertation/papers/latex/LatexTemplate/figures/1_ACF.png', width = 2000,height = 600)

par(mfrow=c(1,4))
acf(output1df$summarystatistics,lag.max = 2000,col='red',main='') #uniform
legend("topright", inset=.0, title="", c("uniform"),lty=c(1),col=c("red"),cex=4)
acf(output2df$summarystatistics,lag.max = 2000,col='green3',main='') #TNT
legend("topright", inset=.0, title="", c("TNT"),lty=c(1),col=c("green3"),cex=4)
acf(output3df$summarystatistics,lag.max = 2000,col='blue',main='') #sqrt
legend("topright", inset=.0, title="", c("Sqrt"),lty=c(1),col=c("blue"),cex=4)
acf(output4df$summarystatistics,lag.max = 2000,col='black',main='') #barker
legend("topright", inset=.0, title="", c("Barker"),lty=c(1),col=c("black"),cex=4)
dev.off()


par(mfrow=c(1,1))
plot(rep(5:10),acrate1,col='red',type='o',ylim=c(0,1),ylab='',xlab='')
par(new = TRUE)
plot(rep(5:10),acrate2,col='blue',type='o',ylim=c(0,1),ylab='',xlab='')
par(new=TRUE)
plot(rep(5:10),acrate3,col='green',type='o',ylim=c(0,1),ylab='',xlab='')
par(new=TRUE)
plot(rep(5:10),acrate4,col='black',type='o',ylim=c(0,1),ylab='acceptance rate',xlab='num of nodes')
legend("topleft", inset=.05, title="acceptance rate", c("uniform","TNT","sqrt",'barker'),lty=c(1, 4),col=c("red", "blue",'green','black'))

par(mfrow=c(1,1))
plot(rep(5:15),listESS1,col='red',type='o',ylim=c(0,1000),ylab='',xlab='')
par(new =TRUE)
plot(rep(5:15),listESS2,col='blue',type='o',ylim=c(0,1000),ylab='',xlab='')
par(new=TRUE)
plot(rep(5:15),listESS3,col='green',type='o',ylim=c(0,1000),ylab='',xlab='')
par(new=TRUE)
plot(rep(5:15),listESS4,col='black',type='o',ylim=c(0,1000),ylab='',xlab='')
legend("topleft", inset=.05, title="ESS", c("uniform","sqrt",'barker','TNT'),lty=c(1, 3),col=c("red", "blue",'green','black'))

#AR ESS
png(file='D:/Study files/Ymsc/ST955_Dissertation/papers/latex/LatexTemplate/figures/1_AR_ESS.png', width = 2000,height = 600)

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
legend("topleft", inset=.05,cex=2,title="ESS", c("uniform","sqrt",'barker','TNT'),lty=c(1),col=c("red", "green3",'blue','black'))
dev.off()
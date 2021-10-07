Probf <- function(adjmat,theta=c(1/3,1/3,1/3)){
  exp(c(Get_num_edges(adjmat),
        Get_num_triangles(adjmat),
        Get_num_twostars(adjmat)
  )%*%theta)
}

outputt2_1<-MCMC(5,0,1000,1000)#uniform

outputt2_2<-MCMC(5,1,1000,1000)#TNT

outputt2_3<-MCMC_locallybalanced(5,2,1000,1000)#,  currentmat,acrate,summarystatistic,numedges,numtwostars,numtriangles #g(t)=1, t, sqrt(t), 1/1+t

outputt2_4<-MCMC_locallybalanced(5,3,1000,1000)#,  currentmat,acrate,summarystatistic,numedges,numtwostars,numtriangles #g(t)=1, t, sqrt(t), 1/1+t


outputt2_1df <- data.frame(acrate=outputt2_1[[2]],
                          summarystatistics = outputt2_1[[3]],
                          edges = outputt2_1[[4]],
                          twostars=outputt2_1[[5]],
                          triangle=outputt2_1[[6]])
outputt2_2df <- data.frame(acrate=outputt2_2[[2]],
                          summarystatistics = outputt2_2[[3]],
                          edges = outputt2_2[[4]],
                          twostars=outputt2_2[[5]],
                          triangle=outputt2_2[[6]])
outputt2_3df <- data.frame(acrate=outputt2_3[[2]],
                          summarystatistics = outputt2_3[[3]],
                          edges = outputt2_3[[4]],
                          twostars=outputt2_3[[5]],
                          triangle=outputt2_3[[6]])
outputt2_4df <- data.frame(acrate=outputt2_4[[2]],
                          summarystatistics = outputt2_4[[3]],
                          edges = outputt2_4[[4]],
                          twostars=outputt2_4[[5]],
                          triangle=outputt2_4[[6]])

png(file='D:/Study files/Ymsc/ST955_Dissertation/papers/latex/LatexTemplate/figures/His_edge.png', width = 1000,height = 600)
par(mfrow=c(2,2))
hist(outputt2_1df$edges,xlab='',main='Histogram of Edges, Uniform',col='lightblue')
hist(outputt2_2df$edges,xlab='',main='Histogram of Edges, Uniform',col='lightblue')
hist(outputt2_3df$edges,xlab='',main='Histogram of Edges, Uniform',col='lightblue')
hist(outputt2_4df$edges,xlab='',main='Histogram of Edges, Uniform',col='lightblue')
dev.off()

png(file='D:/Study files/Ymsc/ST955_Dissertation/papers/latex/LatexTemplate/figures/t2_traceplot.png', width = 2000,height = 600)
par(mfrow=c(1,4))
plot(log(outputt2_1df$summarystatistics),type='l',col='red',ylim=c(0,16),xlab='Markov chain',ylab='Summary Statistics')
legend("top", inset=.0, title="", c("uniform"),lty=c(1),col=c("red"),cex=3)
plot(log(outputt2_2df$summarystatistics),type='l',col='green3',ylim=c(0,16),xlab='Markov chain',ylab='Summary Statistics')
legend("top", inset=.0, title="", c("TNT"),lty=c(1),col=c("green3"),cex=3)
plot(log(outputt2_3df$summarystatistics),type='l',col='blue',ylim=c(0,16),xlab='Markov chain',ylab='Summary Statistics')
legend("top", inset=.0, title="", c("Sqrt"),lty=c(1),col=c("blue"),cex=3)
plot(log(outputt2_4df$summarystatistics),type='l',col='black',ylim=c(0,16),xlab='Markov chain',ylab='Summary Statistics')
legend("top", inset=.0, title="", c("Barker"),lty=c(1),col=c("black"),cex=3)
dev.off()


#acf
png(file='D:/Study files/Ymsc/ST955_Dissertation/papers/latex/LatexTemplate/figures/t2_ACF.png', width = 2000,height = 600)
Cex=4
par(mfrow=c(1,4))
acf(outputt2_1df$summarystatistics,lag.max = 2000,col='red',main='') #uniform
legend("topright", inset=.0, title="", c("uniform"),lty=c(1),col=c("red"),cex=Cex)
acf(outputt2_2df$summarystatistics,lag.max = 2000,col='green3',main='') #TNT
legend("topright", inset=.0, title="", c("TNT"),lty=c(1),col=c("green3"),cex=Cex)
acf(outputt2_3df$summarystatistics,lag.max = 2000,col='blue',main='') #sqrt
legend("topright", inset=.0, title="", c("Sqrt"),lty=c(1),col=c("blue"),cex=Cex)
acf(outputt2_4df$summarystatistics,lag.max = 2000,col='black',main='') #barker
legend("topright", inset=.0, title="", c("Barker"),lty=c(1),col=c("black"),cex=Cex)
dev.off()

#AR
acratet2_1=vector()
acratet2_2=vector()
acratet2_3=vector()
acratet2_4=vector()

for(i in 5:15){
  print(i)
  acratet2_1<-c(acratet2_1,MCMC(i,0,1000,1000)[[2]])
  acratet2_2<-c(acratet2_2,MCMC(i,1,1000,1000)[[2]])
  acratet2_3<-c(acratet2_3,MCMC_locallybalanced(i,2,1000,1000)[[2]])
  acratet2_4<-c(acratet2_4,MCMC_locallybalanced(i,3,1000,1000)[[2]])
}

par(mfrow=c(1,1))
plot(rep(5:15),acratet2_1,col='red',type='o',ylim=c(0,1),ylab='',xlab='')
par(new = TRUE)
plot(rep(5:15),acratet2_2,col='blue',type='o',ylim=c(0,1),ylab='',xlab='')
par(new=TRUE)
plot(rep(5:15),acratet2_3,col='green',type='o',ylim=c(0,1),ylab='',xlab='')
par(new=TRUE)
plot(rep(5:15),acratet2_4,col='black',type='o',ylim=c(0,1),ylab='acceptance rate',xlab='num of nodes')
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


listnodes=c(5,6,7,8,9,10,11,12,13,14,15)
listESSt2_1=vector()
for(i in 1:11){
  outputx<-MCMC(listnodes[i],0,1000,1000)
  re=multiESS(matrix(data=c(outputx[[4]],outputx[[6]]),ncol=2))
  listESSt2_1=c(listESSt2_1,re)
  print(listnodes[i])
}

listnodes=c(5,6,7,8,9,10,11,12,13,14,15)

listESSt2_3=vector()
for(i in 1:11){
  outputx<-output3<-MCMC_locallybalanced(listnodes[i],2,1000,1000)
  re=multiESS(matrix(data=c(outputx[[4]],outputx[[6]]),ncol=2))
  listESSt2_3=c(listESSt2_3,re)
  print(listnodes[i])
}

listnodes=c(5,6,7,8,9,10,11,12,13,14,15)

listESSt2_4=vector()
for(i in 1:11){
  outputx<-MCMC_locallybalanced(listnodes[i],3,1000,1000)
  re=multiESS(matrix(data=c(outputx[[4]],outputx[[6]]),ncol=2))
  listESS2_4=c(listESS2_4,re)
  print(listnodes[i])
}
listESSt2_2=listESS4
#AR ESS
png(file='D:/Study files/Ymsc/ST955_Dissertation/papers/latex/LatexTemplate/figures/t2_AR_ESS.png', width = 2000,height = 600)

par(fig=c(0,0.5,0,1))
plot(rep(5:15),acratet2_1,col='red',type='o',ylim=c(0,1),ylab='',xlab='')
par(fig=c(0,0.5,0,1),new=TRUE)
plot(rep(5:15),acratet2_2,col='green3',type='o',ylim=c(0,1),ylab='',xlab='')
par(fig=c(0,0.5,0,1),new=TRUE)
plot(rep(5:15),acratet2_3,col='blue',type='o',ylim=c(0,1),ylab='',xlab='')
par(fig=c(0,0.5,0,1),new=TRUE)
plot(rep(5:15),acratet2_4,col='black',type='o',ylim=c(0,1),ylab='acceptance rate',xlab='num of nodes')
legend("topleft", inset=.05,cex=2,title="acceptance rate", c("uniform","TNT","sqrt",'barker'),lty=c(1),col=c("red", "green3",'blue','black'))
##conect
par(fig=c(0.5,1,0,1),new=TRUE)
plot(rep(5:14),listESSt2_1,col='red',type='o',xlim=c(4,15),ylim=c(0,1000),ylab='',xlab='')
par(fig=c(0.5,1,0,1),new=TRUE)
plot(rep(5:14),listESSt2_2[1:10],col='green3',type='o',xlim=c(4,15),ylim=c(0,1000),ylab='',xlab='')
par(fig=c(0.5,1,0,1),new=TRUE)
plot(rep(5:14),listESSt2_3[1:10],col='blue',type='o',xlim=c(4,15),ylim=c(0,1000),ylab='',xlab='')
par(fig=c(0.5,1,0,1),new=TRUE)
plot(rep(5:14),listESSt2_4,col='black',type='o',xlim=c(4,15),ylim=c(0,1000),ylab='Effective Sample Size',xlab='num of nodes')
legend("topleft", inset=.05,cex=2,title="ESS", c("uniform","TNT",'sqrt','barker'),lty=c(1),col=c("red", "green3",'blue','black'))
dev.off()
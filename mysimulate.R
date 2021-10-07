DAG.random <- function(v, nedges=1) {
  edges.max <- v*(v-1)/2
  # Assert length(v)==1 && 1 <= v
  # Assert 0 <= nedges <= edges.max
  index.edges <- lapply(list(1:(v-1)), function(k) rep(k*(k+1)/2, v-k)) 
  index.edges <- index.edges[[1]] + 1:edges.max
  graph.adjacency <- matrix(0, ncol=v, nrow=v)
  graph.adjacency[sample(index.edges, nedges)] <- 1
  graph.adjacency
}


num_nodes <- 5; num_edges <- 5
a <- DAG.random(num_nodes, num_edges)
a
currentmat<-adjmat<-a
Get_num_edges <-function(adjmat) {
  sum(adjmat)
}
Get_num_twostars <-function(adjmat) { # yik*yjk 
  result<-0
  n<-dim(adjmat)[1]
  for (k in 1:n){
    for (i in 1:(n-1)){
      if(i!=k & adjmat[i,k]==1){
        for(j in i:n){
          if(j!=k & adjmat[j,k]==1){
            result <- result+1
          }
        }
      }
    }
  }
  result
}
Get_num_triangles <-function(adjmat) { #  yij yik yjk
  result<-0
  n<-dim(adjmat)[1]
  for(i in 1:(n-2)){
    for(j in (i+1):(n-1)){
      for(k in (j+1):n){
        result<-result+adjmat[j,i]*adjmat[k,i]*adjmat[k,j]
      }
    }
  }
  result
}

Get_num_edges(a)
Get_num_twostars(a)f
Get_num_triangles(a)

Probf <- function(adjmat,theta=c(0.5,0.5)){
  exp(c(Get_num_edges(adjmat),
    Get_num_triangles(adjmat)
  )%*%theta)
}
Probf(a)

Uniformsampler<-function(adjmat,times=1){
  for(i in 1:times){
    nodes<-dim(adjmat)[1]
    edges.max <- nodes*(nodes-1)/2
    index.edges <- lapply(list(1:(nodes-1)), function(k) rep(k*(k+1)/2, nodes-k)) 
    index.edges <- index.edges[[1]] + 1:edges.max
    
    trigger<-sample(index.edges,1)
    adjmat[trigger]<-1-adjmat[trigger]
  }
  adjmat
}

a2<-Uniformsampler(a,100)

TNTsampler<-function(adjmat,times=1){
  for(i in 1:times){
    if(runif(1, min = 0, max = 1)>0.5){#0 to 1
      nodes<-dim(adjmat)[1]
      edges.max <- nodes*(nodes-1)/2
      index.edges <- lapply(list(1:(nodes-1)), function(k) rep(k*(k+1)/2, nodes-k)) 
      index.edges <- index.edges[[1]] + 1:edges.max
      zeross<-which(adjmat[index.edges]==0)
      if(length(zeross)==0){return(adjmat)}
      trigger<-index.edges[sample(zeross,1)]
      adjmat[trigger]<-1-adjmat[trigger]
    }
    else{
      nodes<-dim(adjmat)[1]
      edges.max <- nodes*(nodes-1)/2
      index.edges <- lapply(list(1:(nodes-1)), function(k) rep(k*(k+1)/2, nodes-k)) 
      index.edges <- index.edges[[1]] + 1:edges.max
      oness<-which(adjmat[index.edges]==1)
      if(length(oness)==0){return(adjmat)}
      trigger<-index.edges[sample(oness,1)]
      adjmat[trigger]<-1-adjmat[trigger]
    }
  }
  adjmat
}

a2<-TNTsampler(a,10000)

MCMC<-function(num_nodes,sampler=0,num_output=100,num_burn_in=100){ #0 means uniform 1 means TNT
  num_edges<-num_nodes
  initialmat <- DAG.random(num_nodes, num_edges)
  nodes<-dim(adjmat)[1]
  currentmat<-sum<-initialmat
  summarystatistic<-vector()
  numedges<-vector()
  numtwostars<-vector()
  numtriangles<-vector()
  acrate=0
  
  for(i in 1:num_burn_in){
    if(sampler==0){
      newmat<-Uniformsampler(currentmat)
    }
    else{newmat<-TNTsampler(currentmat)}
    
    if(runif(1,0,1)<min(1,Probf(newmat)/Probf(currentmat))){
      currentmat<-newmat
      #keep newmat
    }else{}#no change currentmat is still currentmat
  }
  for(i in 1:num_output){
    if(sampler==0){
      newmat<-Uniformsampler(currentmat)
    }
    else{newmat<-TNTsampler(currentmat)}
    
    if(runif(1,0,1)<min(1,Probf(newmat)/Probf(currentmat))){
      currentmat<-newmat
      acrate=acrate+1
      #keep newmat
    }else{}#no change currentmat is still currentmat
    sum<-sum+currentmat
    summarystatistic<-c(summarystatistic,Probf(currentmat))
    numedges<-c(numedges,Get_num_edges(currentmat))
    numtwostars<-c(numtwostars,Get_num_twostars(currentmat))
    numtriangles<-c(numtriangles,Get_num_triangles(currentmat))
  }
  acrate=acrate/num_output
  list(currentmat,acrate,summarystatistic,numedges,numtwostars,numtriangles)
}
proposal<-function(mat1,mat2,g){ #K=1/numofneighbors  0: g(t)=1,  1: t, 2: sqrt(t), 3: 1/1+t   mat1|mat2
  
  if(g==0){output=1}
  if(g==1){output=Probf(mat1)/Probf(mat2)}
  if(g==2){output=sqrt(Probf(mat1)/Probf(mat2))}
  if(g==3){output=1/(1+(Probf(mat1)/Probf(mat2)))}
  output
}

locallybalanced_sampler<-function(currentmat,g){
  
  nodes<-dim(currentmat)[1]
  edges.max <- nodes*(nodes-1)/2
  index.edges <- lapply(list(1:(nodes-1)), function(k) rep(k*(k+1)/2, nodes-k)) 
  index.edges <- index.edges[[1]] + 1:edges.max
  
  problist<-vector()
  for(i in 1:edges.max){
    problist<-c(problist,Probf_index(currentmat,index.edges[i],g))
  }
  problist<-problist/sum(problist)
  for(i in 2:edges.max){
    problist[i]<-problist[i]+problist[i-1]
  }
  
  ran<-runif(1,0,1)
  trigger<-sum(ran>problist)+1
  
  trigger<-index.edges[trigger]
  currentmat[trigger]<-1-currentmat[trigger]
  currentmat
}
Probf_index<-function(currentmat,index,g){
  currentmat1<-currentmat
  currentmat1[index]<-1-currentmat1[index]
  re<-proposal(currentmat1,currentmat,g)
  re
}

MCMC_locallybalanced<-function(num_nodes,proposalg=0,num_output=100,num_burn_in=100){ #g(t)=1, t, sqrt(t), 1/1+t
  num_edges<-num_nodes
  initialmat <- DAG.random(num_nodes, num_edges)
  
  nodes<-dim(adjmat)[1]
  currentmat<-sum<-initialmat
  summarystatistic<-vector()
  numedges<-vector()
  numtwostars<-vector()
  numtriangles<-vector() #0: g(t)=1,  1: t, 2: sqrt(t), 3: 1/1+t  mat1|mat2
  g<-proposalg
  acrate=0
  
  for(i in 1:num_burn_in){
    newmat<-locallybalanced_sampler(currentmat,g)
    
    if(runif(1,0,1)<min(1,Probf(newmat)*proposal(currentmat,newmat,g)/(Probf(currentmat)*proposal(newmat,currentmat,g)))){ # g(oldpoint|newpoint)/g(newpoint|oldpoint)
      currentmat<-newmat
      #keep newmat
    }else{}#no change currentmat is still currentmat
  }
  for(i in 1:num_output){
    newmat<-locallybalanced_sampler(currentmat,g)
    
    if(runif(1,0,1)<min(1,Probf(newmat)*proposal(currentmat,newmat,g)/(Probf(currentmat)*proposal(newmat,currentmat,g)))){
      currentmat<-newmat
      acrate=acrate+1
      #keep newmat
    }else{}#no change currentmat is still currentmat
    sum<-sum+currentmat
    summarystatistic<-c(summarystatistic,Probf(currentmat))
    numedges<-c(numedges,Get_num_edges(currentmat))
    numtwostars<-c(numtwostars,Get_num_twostars(currentmat))
    numtriangles<-c(numtriangles,Get_num_triangles(currentmat))
  }
  acrate=acrate/num_output
  list(currentmat,acrate,summarystatistic,numedges,numtwostars,numtriangles)
}

output1<-MCMC(5,0,1000,1000)#uniform
output10_1<-MCMC(10,0,10000,10000)#uniform
output1[[1]]
output1[[2]] #acrate
plot(output1[[3]][-1:-9500],type='b',lwd=1,pch=1)
acf(output1[[3]],lag.max = 1000)
output1[[3]][-1:-9950]
output1[[4]][-1:-9950] #edges
output1[[5]][-1:-9950]#twostars
output1[[6]][-1:-9950]#triangles



output2<-MCMC(5,1,10000,10000)#TNT
output10_2<-MCMC(10,1,10000,10000)#TNT
output2[[1]]
output2[[2]]
output2[[3]][-1:-9950]
output2[[4]][-1:-9950]
output2[[5]][-1:-9950]
output2[[6]][-1:-9950]


output3<-MCMC_locallybalanced(5,2,10000,10000)#,  currentmat,acrate,summarystatistic,numedges,numtwostars,numtriangles #g(t)=1, t, sqrt(t), 1/1+t
output10_3<-MCMC_locallybalanced(10,2,1000,1000)
output3[[1]]
output3[[2]]
plot(output3[[3]][-1:-9500],type='b',lwd=1,pch=1)
acf(output3[[3]],lag.max = 500)
output3[[4]][-1:-9950]
output3[[5]][-1:-9950]
output3[[6]][-1:-9950]


output4<-MCMC_locallybalanced(5,3,10000,10000)#,  currentmat,acrate,summarystatistic,numedges,numtwostars,numtriangles #g(t)=1, t, sqrt(t), 1/1+t
output10_4<-MCMC_locallybalanced(10,3,1000,1000)
output4[[1]]
output4[[2]]
plot(output4[[3]][-1:-9500],type='b',lwd=1,pch=1)
acf(output4[[3]],lag.max = 500)
output4[[4]][-1:-9950]
output4[[5]][-1:-9950]
output4[[6]][-1:-9950]

output101df <- data.frame(acrate=output10_1[[2]],
                        summarystatistics = output10_1[[3]],
                        edges = output10_1[[4]],
                        twostars=output10_1[[5]],
                        triangle=output10_1[[6]])
output102df <- data.frame(acrate=output10_2[[2]],
                        summarystatistics = output10_2[[3]],
                        edges = output10_2[[4]],
                        twostars=output10_2[[5]],
                        triangle=output10_2[[6]])
output103df <- data.frame(acrate=output10_3[[2]],
                        summarystatistics = output10_3[[3]],
                        edges = output10_3[[4]],
                        twostars=output10_3[[5]],
                        triangle=output10_3[[6]])
output104df <- data.frame(acrate=output10_4[[2]],
                        summarystatistics = output10_4[[3]],
                        edges = output10_4[[4]],
                        twostars=output10_4[[5]],
                        triangle=output10_4[[6]])


p2<-ggplot(df, aes(x=edges)) + geom_histogram()
p2
par(mfrow=c(1,1))
qqplot(output1df$edges,op1$edges)
qqplot(output2df$edges,op1$edges)
qqplot(output2df$edges,op2$edges)
qqplot(output2df$edges,op2$edges)

par(mfrow=c(2,2))
hist(op1$edges)
hist(op2$edges)
hist(output1df$edges)
hist(output2df$edges)
hist(output3df$edges)
hist(output4df$edges)



par(mfrow=c(3,2))
hist(op1$triangle)
hist(op2$triangle)
hist(output1df$triangle)
hist(output2df$triangle)
hist(output3df$triangle)
hist(output4df$triangle)

##measurements
#acf
par(mfrow=c(2,2))
acf(output1df$summarystatistics,lag.max = 2000) #uniform
acf(output2df$summarystatistics,lag.max = 2000) #TNT
acf(output3df$summarystatistics,lag.max = 2000) #sqrt
acf(output4df$summarystatistics,lag.max = 2000) #barker

#acrate
acrate1=vector()
acrate2=vector()
acrate3=vector()
acrate4=vector()

for(i in 5:15){
  print(i)
  acrate1<-c(acrate1,MCMC(i,0,1000,1000)[[2]])
  acrate2<-c(acrate2,MCMC(i,1,1000,1000)[[2]])
  acrate3<-c(acrate3,MCMC_locallybalanced(5,2,1000,1000)[[2]])
  acrate4<-c(acrate4,MCMC_locallybalanced(5,3,1000,1000)[[2]])
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


#traceplot

par(mfrow=c(1,1))

par(mfrow=c(1,4))
plot(log(output1df$summarystatistics),type='l',col='red',ylim=c(0,20),xlab='Markov chain',ylab='Summary Statistics')
legend("top", inset=.0, title="", c("uniform"),lty=c(1),col=c("red"))
plot(log(output2df$summarystatistics),type='l',col='green3',ylim=c(0,20),xlab='Markov chain',ylab='Summary Statistics')
legend("top", inset=.0, title="", c("TNT"),lty=c(1),col=c("green3"))
plot(log(output2df$summarystatistics),type='l',col='blue',ylim=c(0,20),xlab='Markov chain',ylab='Summary Statistics')
legend("top", inset=.0, title="", c("Sqrt"),lty=c(1),col=c("blue"))
plot(log(output2df$summarystatistics),type='l',col='black',ylim=c(0,20),xlab='Markov chain',ylab='Summary Statistics')
legend("top", inset=.0, title="", c("Barker"),lty=c(1),col=c("black"))

par(mfrow=c(1,1))
plot(log(output3df$summarystatistics),type='l',xlim=c(0,3000),ylim=c(0,20))

#ESS

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


listESS4=vector()
for (i in 1:11){
  net2<-simulate(network(listnodes[i],directed=is_directed) ~ edges+triangles, 
                 coef=inputs, nsim=1000, control=control.simulate(MCMC.burnin=1000,MCMC.interval=1,MCMC.prop.weights='TNT'))
  
  op2<-data.frame(summary(net2 ~ edges + triangles))
  
  re<-multiESS(op2)
  listESS4=c(listESS4,re)
  print(listnodes[i])
}
plot(rep(5:15),listESS1,col='red',type='o',ylim=c(0,1000),ylab='',xlab='')
par(new =TRUE)
plot(rep(5:15),listESS2,col='blue',type='o',ylim=c(0,1000),ylab='',xlab='')
par(new=TRUE)
plot(rep(5:15),listESS3,col='green',type='o',ylim=c(0,1000),ylab='',xlab='')
par(new=TRUE)
plot(rep(5:15),listESS4,col='black',type='o',ylim=c(0,1000),ylab='',xlab='')
legend("topleft", inset=.05, title="ESS", c("uniform","sqrt",'barker','TNT'),lty=c(1, 3),col=c("red", "blue",'green','black'))

png(file='D:/Study files/Ymsc/ST955_Dissertation/papers/latex/LatexTemplate/figures/Re_edge.png', width = 1000,height = 600)
par(mfrow=c(2,2))
hist(op1$edges,xlab='',main='Histogram of Edges, Uniform, ergm package',col='lightblue')
hist(op2$edges,xlab='',xlim=c(0,10),main='Histogram of Edges, TNT, ergm package',col='lightblue')
hist(output1df$edges,xlab='', main='Histogram of Edges, Uniform, own',col='lightblue')
hist(output2df$edges,xlab='', main='Histogram of Edges, TNT, own',col='lightblue')
dev.off()

png(file='D:/Study files/Ymsc/ST955_Dissertation/papers/latex/LatexTemplate/figures/Re_triangle.png', width = 1000,height = 600)
par(mfrow=c(2,2))
hist(op1$triangle,xlab='',main='Histogram of Triangles, Uniform, ergm package',col='lightblue')
hist(op2$triangle,xlab='',xlim=c(0,10),main='Histogram of Triangles, TNT, ergm package',col='lightblue')
hist(output1df$triangle,xlab='', main='Histogram of Triangles, Uniform, own',col='lightblue')
hist(op2$triangle[1:9000],xlab='', main='Histogram of Triangles, TNT, own',col='lightblue')
dev.off()






hist(op1$edges)
hist(op2$edges)
hist(output1df$edges)
hist(output2df$edges)

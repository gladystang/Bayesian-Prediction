# Bayes Fitting-app--------------------------------------------------------------------------

library(shiny)
library(neuralnet)
library(rjags)
library(MASS)
library(grid)
library(ggplot2)
library(gridExtra)
library(matrixStats)
#Loading the dataset--------------------------------------------------------------------------

Binary<-read.csv("E:/creative component/shiny-1/classification.csv",header=T)
Measure<-read.csv("E:/creative component/shiny-1/measures.csv",header=T)
dataset<-c("Binary", "Measure")

#----------------------------------------------------------------------------------------------
###########Keep on 1 hidden layer, allow multi hidden nodes for classification data
Classification = "
model
{
for(i in 1:n){
y[i] ~ dbern(p[i])
}
for(i in 1:n){
for(j in 1:nodes)
{
for(k in 1:cols)
{
aa[i,j,k] <- a[j,k]*x[i,k]
}
aas[i,j]<-sum(aa[i,j,])
z[i,j] =1/(1+exp(-(a0[j] +aas[i,j]))) 
temp[i,j]=b[j]*z[i,j] 
}
u[i]<- sum(temp[i,])
p[i] <- 1/(1+exp(-(u[i]+b0))) 
}
for(j in 1:nodes)
{
b[j]~ dnorm(0, .1)
a0[j] ~ dnorm(0, .1)
for(k in 1:cols){
a[j,k]~ dnorm(0, .1)
} 
}
b0 ~ dnorm(0, .1)
}
"


###########Keep on 1 hidden layer, allow multi hidden nodes for Numerical data
Numerical  = "
model
{
for(i in 1:n){
y[i] ~ dnorm(mu[i],sigma)
}

for(i in 1:n){
for(j in 1:nodes)
{
for(k in 1:cols)
{
aa[i,j,k] <- a[j,k]*x[i,k]
}
aas[i,j]<-sum(aa[i,j,])
z[i,j] =1/(1+exp(-(a0[j] +aas[i,j]))) 
temp[i,j]=b[j]*z[i,j] 
}
mu[i]<- sum(temp[i,])+b0

}


for(j in 1:nodes)
{
b[j]~ dnorm(0, .1)
a0[j] ~ dnorm(0, .1)
for(k in 1:cols){
a[j,k]~ dnorm(0, .1)
} 
}

b0 ~ dnorm(0, .1)
sigma~dgamma(1,1)
}
"
#-------------------prediction new cases based on applying rjags model for classification
prediction <- function(nodes,cols,x,data){
  
  n <- nrow(x) 
  p <- matrix(,nrow=1000,ncol=n)
  for(m in 1:1000){
    
    ##extract the parameters
    a <- matrix(,nrow=nodes,ncol=cols)
    a0 <- c()
    b <- c()
    b0 <- data[m,"b0"]
    for(j in 1:nodes)
    { 
      a0[j]<-data[m,paste("a0[",j,"]",sep="")]
      b[j]<-data[m,paste("b[",j,"]",sep="")]
      for(k in 1:cols)
      {
        a[j,k]<-data[m,paste("a[",j,",",k, "]",sep = "")]
      }
    }
    
    ##end of getting parameters, begin to setting the arrarys for prediction
    aa <- array(rep(NaN,n*nodes*cols),c(n,nodes,cols))
    aas <- matrix(,nrow=n,ncol=nodes)
    z <- matrix(,nrow=n,ncol=nodes)
    temp <- matrix(,nrow=n,ncol=nodes)
    
    u <- c()
    
    for(i in 1:n){
      for(j in 1:nodes)
      {
        for(k in 1:cols)
        {
          aa[i,j,k] <- a[j,k]*x[i,k]
          
        }
        
        aas[i,j]<-sum(aa[i,j,])
        
        z[i,j] =1/(1+exp(-(a0[j] +aas[i,j]))) 
        
        temp[i,j]=b[j]*z[i,j] 
        
      }
      
      u[i]<- sum(temp[i,])
      
      p[m,i] <- 1/(1+exp(-(u[i]+b0)))
      
    }
    
  }
  return(p)
}
#-------------------prediction new cases based on applying rjags model for Measure
predictionM <- function(nodes,cols,x,data){
  
  n <- nrow(x) 
  mu <- matrix(,nrow=1000,ncol=n)
  for(m in 1:1000){
    
    ##extract the parameters
    a <- matrix(,nrow=nodes,ncol=cols)
    a0 <- c()
    b <- c()
    b0 <- data[m,"b0"]
    for(j in 1:nodes)
    { 
      a0[j]<-data[m,paste("a0[",j,"]",sep="")]
      b[j]<-data[m,paste("b[",j,"]",sep="")]
      for(k in 1:cols)
      {
        a[j,k]<-data[m,paste("a[",j,",",k, "]",sep = "")]
      }
    }
    
    ##end of getting parameters, begin to setting the arrarys for prediction
    aa <- array(rep(NaN,n*nodes*cols),c(n,nodes,cols))
    aas <- matrix(,nrow=n,ncol=nodes)
    z <- matrix(,nrow=n,ncol=nodes)
    temp <- matrix(,nrow=n,ncol=nodes)
    
    for(i in 1:n){
      for(j in 1:nodes)
      {
        for(k in 1:cols)
        {
          aa[i,j,k] <- a[j,k]*x[i,k]
        }
        aas[i,j]<-sum(aa[i,j,])
        z[i,j] =1/(1+exp(-(a0[j] +aas[i,j]))) 
        temp[i,j]=b[j]*z[i,j] 
      }
      mu[m,i]<- sum(temp[i,])+b0
      
    }
    
  }
  return(mu)
}


#----------------------------------------------------server------------------------------------------
shinyServer(function(input, output) {
  
  # Return the requested dataset  
  datasetInput <- reactive({
    switch(input$dataset,
           "Binary" = Binary,
           "Measure" = Measure)
  })
  
  
  modelInput <- reactive({switch(input$model,"Classification"=Classification,"Numerical"=Numerical)})
  
  output$contents <- renderTable({         
    t(head(datasetInput(), n = input$obs))
  })
  
  output$B_MSE<-renderTable({
    
    datc<- datasetInput()
    model = modelInput() 
    y<-as.numeric(datc[,1])
    index_x <- input$cols+1
    x<-NULL
    for(i in 2:index_x){
      x<-cbind(x,datc[,i])
    }
    # x<-scale(x)
    
    
    if(names(datc)[1]== "case"){
      dat_set = list(n=nrow(datc), y=y,x=x,nodes=input$nodes,cols=input$cols)
      
      m_mod = jags.model(textConnection(model), dat_set) #a jags model object
      res = coda.samples(m_mod, c("p"), n.iter=1000)  #Generate posterior samples in mcmc.list format
      
      data <-as.data.frame(as.matrix(res[[1]])) 
      
      phatmean <- colMeans(data[,1:ncol(data)])
      
      error<-sum((phatmean - y)^2)/2
      names(error)<-"error"
      df <- as.data.frame(c(error,phatmean))
      names(df)<-"result"
      
    }
    else  {
      x<-scale(x)
      dat_set = list(n=nrow(datc), y=y,x=x,nodes=input$nodes,cols=input$cols)
      
      m_mod = jags.model(textConnection(model), dat_set) #a jags model object
      res = coda.samples(m_mod, c("mu"), n.iter=1000)  #Generate posterior samples in mcmc.list format
      data <-as.data.frame(as.matrix(res[[1]])) 
      mupred<- colMeans(data[,1:ncol(data)])
      
      error<-sum((mupred-y)^2)/2
      names(error)<-"error"
      df <- as.data.frame(c(error,mupred))
      names(df)<-"result"
    }
    
    t(head(df,11))
    
    
  })
  
  output$poste<-renderPlot({
    #datc<-Measure
    datc<- datasetInput()
    model = modelInput() 
    y<-as.numeric(datc[,1])
    index_x <- input$cols+1
    x<-NULL
    for(i in 2:index_x){
      x<-cbind(x,datc[,i])
    }
    
    
    
    
    if(names(datc)[1]== "case"){
      dat_set = list(n=nrow(datc), y=y,x=x,nodes=input$nodes,cols=input$cols)
      m_mod = jags.model(textConnection(model), dat_set) #a jags model object
      res = coda.samples(m_mod, c("p"), n.iter=1000)  #Generate posterior samples in mcmc.list format
      
      data <-as.data.frame(as.matrix(res[[1]])) 
      
      phatmean <- colMeans(data[,1:ncol(data)])
      predicy<-c()
      for(i in 1:length(phatmean))
      {
        if(phatmean[i]>0.5)
          predicy<-c(predicy,1)
        else
          predicy<-c(predicy,0)
      }
      Iteration=1:length(phatmean)
      
      plot(Iteration, predicy, ylim =c(-0.1, 1.1), col="red")
      
      points(y-0.05,pch=16, col="blue")
      
      legend("right",legend=c("Posterior","Origin"),pch=c(1,16),col=c("red","blue"))
      
    }
    else  {
      x<-scale(x)
      dat_set = list(n=nrow(datc), y=y,x=x,nodes=input$nodes,cols=input$cols)
      #dat_set = list(n=nrow(datc), y=y,x=x,nodes=2,cols=4)
      m_mod = jags.model(textConnection(model), dat_set) #a jags model object
      res = coda.samples(m_mod, c("mu"), n.iter=1000)  #Generate posterior samples in mcmc.list format
      data <-as.data.frame(as.matrix(res[[1]])) 
      mupred<- colMeans(data[,1:ncol(data)])
      Iteration=1:length(mupred)
      
      plot(Iteration,mupred, col="red")
      
      points(y-0.05,pch=16, col="blue")
      
      legend("right",legend=c("Posterior","Origin"),pch=c(1,16),col=c("red","blue"))
      
      #mupred<-mupred[order(mupred)]
      plot(y, mupred,xlab="origin", ylab="posterior" ,lwd= 1) 
      
    }
    
    
    
  })
  parameter <-reactive({switch(input$distribution,
                               'normal' = rnorm(1, input$normal_mean, input$normal_sd),
                               'point mass' = input$point_location,
                               't' = input$t_location+input$t_scale * rt(1,input$t_df),
                               'log-normal' = rlnorm(1, input$lognormal_location, input$lognormal_scale),
                               'gamma' = rgamma(1, input$gamma_shape, input$gamma_rate),
                               'beta' = rbeta(1, input$beta_shape1, input$beta_shape2),
                               'uniform' = runif(1, input$uniform_lb, input$uniform_ub))
  }) 
  
  
  output$NN <-renderPlot({
    
    dat<-datasetInput()
    neuraldat<-as.data.frame(cbind(y=dat[,1],x1=dat[,2],x2=dat[,3],x3=dat[,4],x4=dat[,5]))
    
    nn <- neuralnet(
      y~x1+x2+x3+x4,data=neuraldat,hidden=input$nodes)
    # visualization
    plot(nn)
  })
  output$B_Pred <-renderPlot({
    
    datc<- datasetInput()
    model = modelInput() 
    y<-as.numeric(datc[,1])
    index_x <- input$cols+1
    x<-NULL
    for(i in 2:index_x){
      x<-cbind(x,datc[,i])
    }
    
    
    if(names(datc)[1]== "case"){
      dat_set = list(n=nrow(datc), y=y,x=x,nodes=input$nodes,cols=input$cols)
      m_mod = jags.model(textConnection(model), dat_set) #a jags model object
      res = coda.samples(m_mod,  c( "a0","a","b0","b"), n.iter=1000)  #Generate posterior samples in mcmc.list format
      data <-as.data.frame(as.matrix(res[[1]])) 
      PRE_X <- matrix(c(26,6,1,2,42,1,1,0,39,6,2,0),nrow=3,byrow=TRUE)
      pred_data<-prediction(input$nodes,input$cols,PRE_X,data)
      Iterations <- 1:1000
      par(mfrow=c(nrow(PRE_X),2))
      for(i in 1:nrow(PRE_X)){
        
        plot(x=Iterations,y=pred_data[,i],type="l",ylab="prediction", main=paste("Trace of p[",i,"]",split = ""))
        plot(density(pred_data[,i]),xlab="N=1000",main=paste("Density of p[",i,"]",split = ""))
      }
      
    }
    else  {
      x<-scale(x)
      dat_set = list(n=nrow(datc), y=y,x=x,nodes=input$nodes,cols=input$cols)
      m_mod = jags.model(textConnection(model), dat_set) #a jags model object
      res = coda.samples(m_mod,  c("a0","a","b0","b"), n.iter=1000)  #Generate posterior samples in mcmc.list format
      data <-as.data.frame(as.matrix(res[[1]])) 
      PRE_X <- matrix(c(31,57623,15768,15.2,35,84935,16954,13.6,32.8,83656,15532,25),nrow=3,byrow=TRUE)
      PRE_X <- scale(PRE_X)
      pred_data<-predictionM(input$nodes,input$cols,PRE_X,data)
      Iterations <- 1:1000
      par(mfrow=c(nrow(PRE_X),1))
      for(i in 1:nrow(PRE_X)){
        
        plot(x=Iterations,y=pred_data[,i],type="l",ylab="prediction", main=paste("Trace of mu[",i,"]",split = ""))
        # plot(density(pred_data[,i]),xlab="N=1000",main=paste("Density of p[",i,"]",split = ""))
      }
    }
    
    
  })
  
  output$NN_MSE<-renderTable({
    
    dat<-datasetInput()
    
    neuraldat<-as.data.frame(cbind(y=dat[,1],x1=dat[,2],x2=dat[,3],x3=dat[,4],x4=dat[,5]))
    
    nn <- neuralnet(
      y~x1+x2+x3+x4,data=neuraldat,hidden=input$nodes, err.fct="sse",
      linear.output=FALSE)
    #mse<-sum((nn$net.result[[1]] - neuraldat$y)^2)/length(neuraldat$y)
    
    #print(as.character(mse))
    df <- as.data.frame(nn$result.matrix)
    names(df)<-"result"
    df
    
  })
  
})


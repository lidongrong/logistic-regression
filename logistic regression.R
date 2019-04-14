
logistic<-function(y,x,iteration=100){
  
  sample_size=dim(x)[1]
  ones=vector(sample_size,mode="numeric")
  ones=ones+1
  x=cbind(ones,x)
  
  e=exp(1)
  
  beta=vector(dim(x)[2],mode="numeric")
  beta=beta+1
  
  for(t in 1:iteration){
    p=vector(sample_size,mode="numeric")
    for( i in 1:sample_size){
      p[i]=e^(x[i,]%*%beta)/(1+e^(x[i,]%*%beta))
    }
    
    w=diag(p*(1-p))
    
    beta=beta+solve(t(x) %*% w %*% x) %*% t(x) %*% w %*% (x%*%beta+solve(w)%*%(y-p))
    
    
    
  }
  
  final_beta=beta
  fitted=p
  
  binary=p
  
  for(i in 1:length(p)){
    if(binary[i]>=0.7){
      binary[i]=1
    }
    else{
      binary[i]=0
    }
  }
  
  accuracy=sum(binary)/length(binary)
  
  
  model=list(coef=beta,fitted_values=p,y=y,x=x,accuracy=accuracy,predicted=binary)
  
  class(model)<-"logistic model"
  
  return(model)
  
  
  
}

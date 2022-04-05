library(MASS)
A<-rbind(c(0.9,0,0.5),
             c(1.1,0.5,0),
             c(1.2,0.3,0.2),
             c(0.8,0.1,0.7)
)
B<-t(A)
A.plus<-ginv(A)
B.plus<-ginv(B)
B.tilde.plus<-Null(B.plus)

B.bar<-cbind(B.plus,B.tilde.plus)
B.tilde<-solve(B.bar)[-c(1:length(A[1,])),]

w<-rep(0.25,4)

obj<-function(x){
  x<-x/sum(x)
  sigma_P<-as.numeric(sqrt(t(x)%*%sigma%*%x))
  RC.F<-(t(A)%*%x)*(A.plus%*%sigma%*%x)/sigma_P
  RC.tilde.F<-(B.tilde%*%x)*(B.tilde%*%sigma%*%x)/sigma_P
  
  sum((RC.F/sigma_P-1/3)^2)
}

res<-optim(x,obj,method="L-BFGS-B",lower=rep(0,4))
w.star<-res$par/sum(res$par)
RC.F<-(t(A)%*%w.star)*(A.plus%*%sigma%*%w.star)/sigma_P
RC.tilde.F<-(B.tilde%*%w.star)*(B.tilde%*%sigma%*%w.star)/sigma_P

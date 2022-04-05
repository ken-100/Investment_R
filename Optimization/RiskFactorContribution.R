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

RC.F<-(t(A)%*%w)*(A.plus%*%sigma%*%x)/sigma_P
RC.tilde.F<-(B.tilde%*%w)*(B.tilde%*%sigma%*%w)/sigma_P

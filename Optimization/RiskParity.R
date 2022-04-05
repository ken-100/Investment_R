#Covariance
sigma<-rbind(c(0.0449016,0.0396086,0.0442209,0.0323200),
c(0.0396086,0.0733868,0.0543290,0.0357016),
c(0.0442209,0.0543290,0.0689063,0.0400982),
c(0.0323200,0.0357016,0.0400982,0.0530842)
)

#Weight
w<-c(0.25,0.25,0.25,0.25)

#SD
sigma_P<-as.numeric(sqrt(t(w)%*%sigma%*%w))

#RiskContribution
RC<-w*(t(w)%*%sigma/sigma_P)



#Objective Function
obj<-function(x){
  x<-x/sum(x)
  sigma_P<-as.numeric(sqrt(t(w)%*%sigma%*%w))
  RC<-w*(t(w)%*%sigma/sigma_P)
  
  sum((RC/sigma_P-1/length(w))^2)
}

#Optimization
res<-optim(w,obj,method="L-BFGS-B",lower=rep(0,dim(sigma)[1]))
w.star<-res$par/sum(res$par)
RC.star<-w.star*(t(w.star)%*%sigma/sigma_P)

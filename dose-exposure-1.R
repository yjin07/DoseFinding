####Simulation
situ=7
qqq=5

###objective function of model 1 
log_DR_log_normal_LS_EMAX_simple<-function(par,data){
  E0=par[1]
  EMAX=par[2]
  h=par[3]
  EC50=par[4]
  
  if(sum(data[,3]==0)>=1){
    data0=data[data[,3]==0,]
    y0=data0[,1]
    c0=data0[,2]
    d0=data0[,3]
    est0=log(E0)
    data1=data[data[,3]!=0,]
    y1=data1[,1]
    c1=data1[,2]
    d1=data1[,3]
    n=dim(data)[1]
    n0=dim(data0)[1]
    n1=dim(data1)[1]
    est1=log(E0 + EMAX/(exp(h*(log(EC50)-log(d1)))+1))
    
    
    return((sum((log(y0)-est0)^2)+sum((log(y1)-est1)^2)))
    
  }
  if(sum(data[,3]==0)==0){
    data1=data[data[,3]!=0,]
    y1=data1[,1]
    c1=data1[,2]
    d1=data1[,3]
    n=dim(data)[1]
    n1=dim(data1)[1]
    n1=dim(data1)[1]
    est1=log(E0 + EMAX/(exp(h*(log(EC50)-log(d1)))+1))
    return((sum((log(y1)-est1)^2)))
  }
  
}

###objective function of model 2 with linear relationship beta1==1
log_DR_log_normal_LS_EMAX_total<-function(par,data){
  
  E0=par[1]
  EMAX=par[2]
  h=par[3]
  EC50=par[4]
  logTVCL=log(par[5])
  sigma_cl=par[6]
  
  if(sum(data[,3]==0)>=1){
    data0=data[data[,3]==0,]
    y0=data0[,1]
    c0=data0[,2]
    d0=data0[,3]
    est0=log(E0)
    data1=data[data[,3]!=0,]
    y1=data1[,1]
    c1=data1[,2]
    d1=data1[,3]
    n=dim(data)[1]
    n0=dim(data0)[1]
    n1=dim(data1)[1]
    est1=rep(NA,n1)
    for(jj in 1:n1){
      a=log(d1[jj])-logTVCL
      est1[jj]<-integrate(function(logc_scale) log(E0 + EMAX/(exp(h*(log(EC50)-(logc_scale*sigma_cl+a)))+1))*dnorm(logc_scale),lower=-Inf,upper=Inf)$value
    }
    
    return((sum((log(y0)-est0)^2)+sum((log(y1)-est1)^2)))
    
  }
  if(sum(data[,3]==0)==0){
    data1=data[data[,3]!=0,]
    y1=data1[,1]
    c1=data1[,2]
    d1=data1[,3]
    n=dim(data)[1]
    n1=dim(data1)[1]
    n1=dim(data1)[1]
    est1=rep(NA,n1)
    for(jj in 1:n1){
      a=log(d1[jj])-logTVCL
      est1[jj]<-integrate(function(logc_scale) log(E0 + EMAX/(exp(h*(log(EC50)-(logc_scale*sigma_cl+a)))+1))*dnorm(logc_scale),lower=-Inf,upper=Inf)$value
      
    }
    
    return((sum((log(y1)-est1)^2)))
  }
  
}

###objective function of model 3 (two step model)
log_DR_log_normal_LS_EMAX_twostep<-function(par,data){
  E0=par[1]
  EMAX=par[2]
  h=par[3]
  EC50=par[4]
  if(sum(data[,3]==0)>=1){
    data0=data[data[,3]==0,]
    y0=data0[,1]
    c0=data0[,2]
    d0=data0[,3]
    est0=log(E0)
    data1=data[data[,3]!=0,]
    y1=data1[,1]
    c1=data1[,2]
    d1=data1[,3]
    n=dim(data)[1]
    n0=dim(data0)[1]
    n1=dim(data1)[1]
    est1=log(E0 + EMAX/(exp(h*(log(EC50)-log(c1)))+1))
    
    
    return((sum((log(y0)-est0)^2)+sum((log(y1)-est1)^2)))
    
  }
  if(sum(data[,3]==0)==0){
    data1=data[data[,3]!=0,]
    y1=data1[,1]
    c1=data1[,2]
    d1=data1[,3]
    n=dim(data)[1]
    n1=dim(data1)[1]
    n1=dim(data1)[1]
    est1=log(E0 + EMAX/(exp(h*(log(EC50)-log(c1)))+1))
    return((sum((log(y1)-est1)^2)))
  }
  
}


###objective function of model 2 with power model for dose and exposure relationship 
log_DR_log_normal_LS_EMAX_total_power<-function(par,data){
  
  E0=par[1]
  EMAX=par[2]
  h=par[3]
  EC50=par[4]
  beta0=par[5]
  beta1=par[6]
  sigma=par[7]
  if(sum(data[,3]==0)>=1){
    data0=data[data[,3]==0,]
    y0=data0[,1]
    c0=data0[,2]
    d0=data0[,3]
    est0=log(E0)
    data1=data[data[,3]!=0,]
    y1=data1[,1]
    c1=data1[,2]
    d1=data1[,3]
    n=dim(data)[1]
    n0=dim(data0)[1]
    n1=dim(data1)[1]
    est1=rep(NA,n1)
    for(jj in 1:n1){
      a=beta1*log(d1[jj])+beta0
      est1[jj]<-integrate(function(logc_scale) log(E0 + EMAX/(exp(h*(log(EC50)-(logc_scale*sigma+a)))+1))*dnorm(logc_scale),lower=-Inf,upper=Inf)$value
    }
    
    return((sum((log(y0)-est0)^2)+sum((log(y1)-est1)^2)))
    
  }
  if(sum(data[,3]==0)==0){
    data1=data[data[,3]!=0,]
    y1=data1[,1]
    c1=data1[,2]
    d1=data1[,3]
    n=dim(data)[1]
    n1=dim(data1)[1]
    n1=dim(data1)[1]
    est1=rep(NA,n1)
    for(jj in 1:n1){
      a=beta1*log(d1[jj])+beta0
      est1[jj]<-integrate(function(logc_scale) log(E0 + EMAX/(exp(h*(log(EC50)-(logc_scale*sigma+a)))+1))*dnorm(logc_scale),lower=-Inf,upper=Inf)$value
      
    }
    
    return((sum((log(y1)-est1)^2)))
  }
  
}




num.sim=40
n=20
N=n*7
d=c(rep(20,n),rep(30,n),rep(48,n),rep(60,n),rep(80,n),rep(100,n),rep(110,n))
#n=50
#N=n*3
#d=c(rep(20,n),rep(60,n),rep(100,n))


E0=20
EMAX=100
h=4
EC50=5
sigma=0.5
TVCL=5
sigma_y=0.5
boot.num=200
beta0=-log(TVCL)
beta1=0.85

###save estimators####
#E0,EMAX,h,EC50,beta0,sigma,beta1
par<-array(NA,c(num.sim,7,3))
par.boot<-array(NA,c(num.sim,boot.num,7,3))


for(i in 1:num.sim){
  set.seed(i+40*(qqq-1))
  logc=rep(0,N)
  N1=sum(d!=0)
  logc[d!=0]=beta0+beta1*log(d[d!=0])+rnorm(N1,sd=sigma)
  
  c=exp(logc)
  top=EMAX*(c^h)
  bottom=EC50^h+c^h
  epi=rnorm(N,0,sd=sigma_y)
  y=exp(log(top/bottom+E0)+epi)
  data=data.frame(y=y,c=c,d=d)
  data1=data[c(1:20,41:60,81:100),]
  E0_ini=exp(rnorm(5,mean=log(E0),sd=0.5))
  EMAX_ini=exp(rnorm(5,mean=log(EMAX),sd=0.5))
  h_ini=1:5
  EC50_ini=exp(rnorm(5,mean=log(EC50),sd=0.5))
  sigma_ini=exp(rnorm(5,mean=log(sigma),sd=0.1))
  beta0_ini=rnorm(5,mean=beta0,sd=0.5)
  beta1_ini=rnorm(5,mean=beta1,sd=0.5)
  
  f1=cbind(E0_ini,EMAX_ini,h_ini,EC50_ini,beta0_ini,beta1_ini,sigma_ini)
  f2=cbind(E0_ini,EMAX_ini,h_ini,EC50_ini)
  
  out1<-apply(f1, 1, nlminb, objective=log_DR_log_normal_LS_EMAX_total_power, data=data,lower=c(1e-8,1e-8,0,1e-8,-Inf,-Inf,0))
  out2<-apply(f2, 1, nlminb, objective=log_DR_log_normal_LS_EMAX_simple, data=data,lower=c(1e-8,1e-8,0,1e-8))
  out3<-apply(f2, 1, nlminb, objective=log_DR_log_normal_LS_EMAX_twostep, data=data,lower=c(1e-8,1e-8,0,1e-8))
  c1=data[data[,3]!=0,2]
  d1=data[data[,3]!=0,3]
  fit2<-lm(log(c1)~log(d1))
  beta0_fit2=fit2$coefficients[1]
  beta1_fit2=fit2$coefficients[2]
  sigma_fit2=sqrt(sum(fit2$residuals^2)/fit2$df.residual)
  print('te')
  obj1<-rep(NA,5)
  obj2<-rep(NA,5)
  obj3<-rep(NA,5)
  for(tt in 1:5){
    obj1[tt]<-out1[[tt]]$obj
    obj2[tt]<-out2[[tt]]$obj
    obj3[tt]<-out3[[tt]]$obj
  }
  par[i,,1]<-out1[[which.min(obj1)]]$par ###total
  par[i,,2]<-c(out2[[which.min(obj2)]]$par,NA,NA,NA) ###simple
  par[i,,3]<-c(out3[[which.min(obj3)]]$par,beta0_fit2,beta1_fit2,sigma_fit2) ###twostep
  
  for(ind.boot in 1:boot.num){
    boot.ind<-sample(1:N,replace=TRUE)
    data.boot<-data[boot.ind,]
    boot.ind1<-sample(1:(N/2),replace=TRUE)
    data1.boot<-data[boot.ind1,]
    
    ###optimize objective function
    
    temp1<-nlminb(par[i,,1],objective=log_DR_log_normal_LS_EMAX_total_power, data=data.boot,lower=c(1e-8,1e-8,0,1e-8,-Inf,-Inf,0))
    temp2<-nlminb(par[i,1:4,2],objective=log_DR_log_normal_LS_EMAX_simple, data=data.boot,lower=c(1e-8,1e-8,0,1e-8))
    temp3<-nlminb(par[i,1:4,3],objective=log_DR_log_normal_LS_EMAX_twostep, data=data.boot,lower=c(1e-8,1e-8,0,1e-8))
    
    c1=data.boot[data.boot[,3]!=0,2]
    d1=data.boot[data.boot[,3]!=0,3]
    ###estimate beta1 and beta0 for two-step
    fit2<-lm(log(c1)~log(d1))
    beta0_fit2.boot=fit2$coefficients[1]
    beta1_fit2.boot=fit2$coefficients[2]
    sigma_fit2.boot=sqrt(sum(fit2$residuals^2)/fit2$df.residual)
    
    obj1<-rep(NA,5)
    obj2<-rep(NA,5)
    obj3<-rep(NA,5)
    for(tt in 1:5){
      obj1[tt]<-out1[[tt]]$obj
      obj2[tt]<-out2[[tt]]$obj
      obj3[tt]<-out3[[tt]]$obj
    }
    par.boot[i,ind.boot,,1]<-temp1$par ###total
    par.boot[i,ind.boot,,2]<-c(temp2$par,NA,NA,NA) ###simple
    par.boot[i,ind.boot,,3]<-c(temp3$par,beta0_fit2.boot,beta1_fit2.boot,sigma_fit2.boot) ###twostep
    print(paste("i=",i,",boot number=", ind.boot))
  }
  
  
  
}


save(par,par.boot,file=paste0('/usrfiles/Dose_Exposure/s',situ,'_f',qqq,'.RData'))



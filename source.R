#
# 1. mod list
# 2. vecfit function
# 3. summary function 
#
############################ define bayeisn model in vector fomate

#nmatrix <- matrix(12,3,4)
#ymatrix <- matrix(c(1:12),3,4)

#xlist<- list('y'=ymatrix,'n'=nmatrix,'cn' = 3, 'bn'=4)

mod_string1<- "model {
	for (i in 1:cn){
          for (j in 1:bn){
	        y[i,j] ~ dbin(p[i,j],n[i,j])
		  p[i,j] <- ilogit(z[i,j])
              z[i,j] <- mu[i,j]
              mu[i,j] ~ dnorm(0, 1E-4)
              }
      }
       
  }"
mod_string2<- "model {
      for (i in 1:cn){
           aa[i] ~ dnorm(0,1e-4)   
          for (j in 1:bn){
              y[i,j] ~ dbin(p[i,j],n[i,j])
		  p[i,j] <- ilogit(z[i,j])
              z[i,j] <- aa[i]+ bb[j]
                  
            }
        }
   
      for (j in 1:bn){
      bb[j] ~ dnorm(0,1e-4)   
      }
	
}"


mod_string3<- "model {
      for (i in 1:cn){          
          for (j in 1:bn){
	        y[i,j] ~ dbin(p[i,j],n[i,j])
		  p[i,j] <- ilogit(z[i,j])
              z[i,j] <- aa[i]
          
          }
       }
      for ( i in 1:cn){
        aa[i] ~ dnorm(0, 1e-4)
      }   
        	
}"


mod_string4<- "model {
	for (i in 1:cn){
          for (j in 1:bn){
	        y[i,j] ~ dbin(p[i,j],n[i,j])
		  p[i,j] <- ilogit(z[i,j])
              z[i,j] ~ dnorm(mu[i,j],1)              
              mu[i,j] ~ dnorm(phi[i],sigma)
          }
          phi[i]~ dnorm(0,tau)
      }
      sigma ~ dgamma(1,1)
      tau ~ dgamma(1,1)
       
  }"

#BayMSD(para=para, k.itm=3, modlist=mod_string2,stop.itm=1, n=c(10, 10, 10))

mod_string5<- "model {
	for (i in 1:cn){ 
        
          for (j in 1:bn){
	        y[i,j] ~ dbin(p[i,j],n[i,j])
		  p[i,j] <- ilogit(z[i,j])
              z[i,j] ~ dnorm(mu[i,j],1)
              mu[i,j] = aa[i] + bb[j]   
                 
           }
      }
      for (i in 1:cn){
        aa[i] ~ dnorm(0, tau)
      }
      for (j in 1:bn){
         bb[j] ~ dnorm(0, sigma)
      } 
      sigma ~ dgamma(1,1)
      tau ~ dgamma(1,1)
       
  }"

mod_string6<- "model {
	for (i in 1:cn){
          aa[i] ~ dnorm(0, tau)
          for (j in 1:bn){
	        y[i,j] ~ dbin(p[i,j],n[i,j])
		  p[i,j] <- ilogit(z[i,j])
              z[i,j] ~ dnorm(mu[i,j],sigma)
              mu[i,j] = aa[i]              
           }
      }
      
      sigma ~ dgamma(1,1)
      tau ~ dgamma(1,1)
       
  }"

#modlist <- list(mod_string1)
#VecFit(modstr=modlist, xlist = xlist)

VecFit <- function( modstr, xlist, thin=2,epsL=0.1, delta=0.7 ,Penalty=1,  nadapt=5000, niter=5000){

    PD <- NULL
    mk<- length(modstr)
    PD.s <- rep(0,mk)
    if (mk>1) {
      for (kk in 1:mk) {
           mod.s<-jags.model(textConnection(modstr[[kk]]),
                   data = xlist,
                   n.chains = 2, 
                   n.adapt = nadapt,
                   quiet=TRUE)
           capture.output(DIC.s<- dic.samples(mod.s, n.iter=niter, thin=2, type="pD"), file='NUL')
           PD.s[kk] <- sum(DIC.s$deviance)+sum(DIC.s$penalty)*Penalty
          }
    PDout <- which(PD.s==min(PD.s, na.rm=TRUE))                        # record which model being selected
    modstr[[PDout]]
    monstr.select<- modstr[[PDout]]                                    # assign the selected model

    } else {
        PDout <-1
    }
    mod1 <-jags.model(textConnection(modstr[[PDout]]),
                   data = xlist,
                   n.chains = 2, 
                   n.adapt = nadapt,
                   quiet=TRUE)

     capture.output( mcmc_samples1 <-coda.samples(mod1, variable.names=c( "p"), n.iter=niter, thin=2), file='NUL')
     
     out1 <- mcmc_samples1[[1]]
     utility.p<- apply(apply(out1,2, function (x) { ifelse (x>delta, 1, 0)}), 2, mean)
     outcome2 <- sapply(utility.p, function(x) {ifelse(x<epsL,0,1)})
     pi.post <-apply(out1,2,mean)
     return(list(outcome2, pi.post, PDout))
  }	


#modlist <- list(mod_string5)
#BayMSD(para=para, k.itm=3, modlist=modlist,stop.itm=1, n=c(10, 10, 10))
# run 1 round of simulation design, call vecfit.

BayMSD <- function(cn=3, bn=4, para, k.itm, n, modlist=modlist, stop.itm=1, nadapt=5000, niter=5000,epsL=0.1, delta=0.7){

  n.para <- cn*bn
  PD<-1
  n.success <- n.Subj<- n1 <- rep(0,n.para) 
  ctn.flag <- rep(1, n.para)
  modstr <<- modlist
  for (s in 1:k.itm){
    select <- ifelse(s>stop.itm, FALSE, TRUE) 
    n1<- rep(n[s],n.para)
    y1<- sapply(para, function(p){rbinom(1,n1,p)})
    n.success <- n.success + y1*ctn.flag
    n.Subj <- n.Subj+n1*ctn.flag    
    myData <- data.frame(n=n.Subj, y=n.success)
    nmatrix <- matrix(myData$n,cn,bn)
    ymatrix <- matrix(myData$y,cn,bn)
    xlist<- list('y'=ymatrix,'n'=nmatrix,'cn' = cn, 'bn'=bn)  
    output<- VecFit(modstr=modstr, xlist=xlist, nadapt=nadapt, niter=niter, epsL=epsL, delta=delta )
    PD <-ifelse(select,output[[3]],PD)
    ctn.flag <- unname(output[[1]])*ctn.flag
    if (s < stop.itm) {
       modstr<<- modlist
     } else {
      modstr<<- modlist[[PD]]
     } 
   }
   print(PD)
   pi.post <- output[[2]]
   return( list(continue=ctn.flag,pi.post=pi.post,event= n.success,subject= n.Subj,selection=PD))
} 


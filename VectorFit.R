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
          
          for (j in 1:bn){
	        y[i,j] ~ dbin(p[i,j],n[i,j])
		  p[i,j] <- ilogit(z[i,j])
              z[i,j] <- aa[i]+ bb[j]
           }
        }
        for (i in 1:cn){
            aa[i] ~ dnorm(0,1e-4)
        }
        for (j in 1:bn){
            bb[j] ~ dnorm(0, 1e-4)
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
        for (i in 1:cn){
            aa[i] ~ dnorm(0,1e-4)
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
          for (j in 1:bn){
	        y[i,j] ~ dbin(p[i,j],n[i,j])
		  p[i,j] <- ilogit(z[i,j])
              z[i,j] ~ dnorm(mu[i,j],sigma)
              mu[i,j] = aa[i]              
           }
      }
      for (i in 1:cn){
          aa[i] ~ dnorm(0, tau)
      }
      
      sigma ~ dgamma(1,1)
      tau ~ dgamma(1,1)
       
  }"

#modlist <- list(mod_string1, mod_string4)
#VecFit(modstr=modlist, xlist = xlist)
VecFit <- function( modstr, xlist, thin=2,epsL=0.1, delta=0.5 ,Penalty=1, selection=TRUE, nadapt=5000, niter=1000){

    PD <- NULL
    mk<- length(modstr)
    PD.s <- rep(0,mk)
    if (selection==TRUE) {
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

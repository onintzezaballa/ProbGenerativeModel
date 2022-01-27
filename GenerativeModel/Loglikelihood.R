
log_likelihood <- function(num.classes, max.stages, prob.c, mass.prob.likelihood, min.stages = max.stages){
  # This function calculates the loglikelihood of the model
  
  minvalueR <- 1e-323
  ll.env <- new.env()
  f_likelihood <- function(s,t){
    if (ll.env$f.matrix[s,t]!=-1){
      return(ll.env$f.matrix[s,t])
      
    } else if (t==length(X)) { 
      aux1 <- f_likelihood(s,t-1)*MarkovModel[[ci]][[1]][paste0(X[t-1],s),X[t]]*MarkovModel[[ci]][[2]][paste0(X[t],s),s]
      ll.env$f.matrix[s,t] <<- aux1
      return(ll.env$f.matrix[s,t])
      
    } else if (s==1) { 
      aux1 <- f_likelihood(s,t-1)*MarkovModel[[ci]][[1]][paste0(X[t-1],s),X[t]]*MarkovModel[[ci]][[2]][paste0(X[t],s),s]
      ll.env$f.matrix[s,t] <<- aux1
      return(ll.env$f.matrix[s,t])
      
    } else if (t>1 & s>1){
      aux1 <- f_likelihood(s,t-1)*MarkovModel[[ci]][[1]][paste0(X[t-1],s),X[t]]*MarkovModel[[ci]][[2]][paste0(X[t],s),s]
      aux2 <- f_likelihood(s-1,t-1)*MarkovModel[[ci]][[1]][paste0(X[t-1],s-1),X[t]]*MarkovModel[[ci]][[2]][paste0(X[t],s-1),s]
      ll.env$f.matrix[s,t] <<- aux1 + aux2
      return(ll.env$f.matrix[s,t])
    } 
  }
  

  sum.total <- 0
  j<-1
  for (j in 1:nrow(df)){
    X <- as.vector(as.matrix(df[j,!is.na(df[j,])])); X <- X[-1]
    ll.env$F.matrix <- list()
    likelihood.samp <- 0
    
    for (ci in 1:num.classes){
      ll.env$f.matrix <- matrix(rep(-1,max.stages*length(X)), ncol = length(X))
      ll.env$f11 <- initialization[[ci]]$PROB[which(initialization[[ci]]$X==paste0(X[1],1))]
      ll.env$f.matrix[,1] <- c(ll.env$f11, rep(0,max.stages -1))
      colnames(ll.env$f.matrix) <- X
      rownames(ll.env$f.matrix) <- 1:max.stages
      for (si in max.stages:min.stages){
        ll.env$f.prob <- f_likelihood(s = si, t = length(X)) 
      }
      ll.env$f.matrix[ll.env$f.matrix==-1] <- 0   
      ll.env$F.matrix[[ci]] <- ll.env$f.matrix
    }
    t <- length(X)
    s <- min.stages:max.stages
    s.prim <- min.stages:max.stages
    mass.prob <- list()
    for (ci in 1:num.classes){
      pp <- MarkovModel[[ci]][[1]][paste0(X[t-1],s),X[t]]*MarkovModel[[ci]][[2]][paste0(X[t],s), s.prim]
      mass.prob.i <- ll.env$F.matrix[[ci]][s,t-1]*pp 
      mass.prob[[ci]] <- mass.prob.i*prob.c[ci]
    }
    for (ci in 1:num.classes){
      if (length(mass.prob[[ci]])==1){
        if (mass.prob[[ci]]==0) {mass.prob[[ci]]<- minvalueR }
        likelihood.samp <- likelihood.samp + sum(log(mass.prob[[ci]])* mass.prob.likelihood[[j]][[ci]]) 
      } else {
        if (sum(rowSums(mass.prob[[ci]])==0)>0) {
          mass.prob[[ci]][which(rowSums(mass.prob[[ci]])==0),]<- minvalueR
        }
        likelihood.samp <- likelihood.samp + sum(log(rowSums(mass.prob[[ci]]))* mass.prob.likelihood[[j]][[ci]]) 
      }
    }
    sum.total <- sum.total + likelihood.samp 
  }
  return(sum.total)
}
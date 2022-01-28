likelihood.a <- function(df, num.classes, max.stages, theta_c, MarkovModel, initialization, min.stages){
  # This function calculates the likelihood, marginalizing by stage and class
  df <- apply(df, 2, function(A) {substr(A, 1, nchar(A)-1)})
  df <- df[,-c(ncol(df))]
  minvalueR <- 1e-323
  ll.env <- new.env()
  f_likelihood <- function(s,t){
    if (ll.env$f.matrix[s,t]!=-1){
      return(ll.env$f.matrix[s,t])
      
    } else if (t==length(A)) {
      aux1 <- f_likelihood(s,t-1)*MarkovModel[[ci]][[1]][paste0(A[t-1],s),A[t]]*MarkovModel[[ci]][[2]][paste0(A[t],s),s]
      ll.env$f.matrix[s,t] <<- aux1
      return(ll.env$f.matrix[s,t])
      
    } else if (s==1) { 
      aux1 <- f_likelihood(s,t-1)*MarkovModel[[ci]][[1]][paste0(A[t-1],s),A[t]]*MarkovModel[[ci]][[2]][paste0(A[t],s),s]
      ll.env$f.matrix[s,t] <<- aux1
      return(ll.env$f.matrix[s,t])
      
    } else if (t>1 & s>1){
      aux1 <- f_likelihood(s,t-1)*MarkovModel[[ci]][[1]][paste0(A[t-1],s),A[t]]*MarkovModel[[ci]][[2]][paste0(A[t],s),s]
      aux2 <- f_likelihood(s-1,t-1)*MarkovModel[[ci]][[1]][paste0(A[t-1],s-1),A[t]]*MarkovModel[[ci]][[2]][paste0(A[t],s-1),s]
      ll.env$f.matrix[s,t] <<- aux1 + aux2
      return(ll.env$f.matrix[s,t])
    } 
  }
  
  
  sum.total <- 0
  length.total <- 0
  j<-1
  mass.prob.likelihood<-list()
  for (j in 1:nrow(df)){
    A <- as.vector(as.matrix(df[j,!is.na(df[j,])])); A <- A[-1]
    ll.env$F.matrix <- list() 
    likelihood.samp <- 0
    length.total <- length.total + length(A)
    ci <-1
    for (ci in 1:num.classes){
      ll.env$f.matrix <- matrix(rep(-1,max.stages*length(A)), ncol = length(A))
      ll.env$f11 <- initialization[[ci]]$PROB[which(initialization[[ci]][,1]==paste0(A[1],1))]
      ll.env$f.matrix[,1] <- c(ll.env$f11, rep(0,max.stages -1))
      colnames(ll.env$f.matrix) <- A
      rownames(ll.env$f.matrix) <- 1:max.stages
      
      for (si in max.stages:min.stages){
        ll.env$f.prob <- f_likelihood(s = si, t = length(A)) 
      }
      ll.env$f.matrix[ll.env$f.matrix==-1] <- 0 # ponemos 0 por donde no pueden pasar caminos   
      ll.env$F.matrix[[ci]] <- ll.env$f.matrix
    }
    
    t <- length(A)
    s <- min.stages:max.stages
    s.prim <- min.stages:max.stages
    mass.prob <- list()
    theta_c.a <- c(rep(0,num.classes))
    ci<-1
    for (ci in 1:num.classes){
      pp <- MarkovModel[[ci]][[1]][paste0(A[t-1],s), A[t]]*MarkovModel[[ci]][[2]][paste0(A[t],s), s.prim]
      mass.prob.i <- ll.env$F.matrix[[ci]][s,t-1]*pp 
      mass.prob[[ci]] <- mass.prob.i/sum(mass.prob.i) 
      theta_c.a[ci] <- sum(mass.prob.i)
    }
    theta_c.a <- theta_c.a/sum(theta_c.a) 
    theta_c.a[which(is.na(theta_c.a))] <- 0 
    for (ci in 1:num.classes){
      mass.prob[[ci]] <- mass.prob[[ci]]*theta_c.a[ci]
      mass.prob[[ci]][is.na(mass.prob[[ci]])] <-  0  
    }
    mass.prob.likelihood[[j]] <- mass.prob
    
    t <- length(A)
    s <- min.stages:max.stages
    s.prim <- min.stages:max.stages
    mass.prob <- list()
    
    for (ci in 1:num.classes){
      pp <- MarkovModel[[ci]][[1]][paste0(A[t-1],s),A[t]]*MarkovModel[[ci]][[2]][paste0(A[t],s), s.prim]
      mass.prob.i <- ll.env$F.matrix[[ci]][s,t-1]*pp # en el tiempo t puede estar en 1 o en 2
      mass.prob[[ci]] <- mass.prob.i*as.numeric(theta_c[ci])
    }
    
    for (ci in 1:num.classes){
      if (length(mass.prob[[ci]])==1){
        if (mass.prob[[ci]]==0) {mass.prob[[ci]]<- minvalueR }
        likelihood.samp <- likelihood.samp +sum(log(mass.prob[[ci]])* mass.prob.likelihood[[j]][[ci]]) 
      } else {
        if (sum(rowSums(mass.prob[[ci]])==0)>0) {
          mass.prob[[ci]][which(rowSums(mass.prob[[ci]])==0),]<- minvalueR
        }
        likelihood.samp <- likelihood.samp +  sum(log(rowSums(mass.prob[[ci]]))* mass.prob.likelihood[[j]][[ci]]) 
      }
    }
    sum.total <- sum.total + likelihood.samp 
  }
  return(sum.total/length.total)
}



Test_Generator <- function(seed, N.total, theta_c, MarkovModel, initialization){
  # This function samples the test datasets with N.total sequence from the model (theta_c, initialization, MarkovModel)
  set.seed(seed+400)
  c <- sample(x=1:num.classes, size = N.total, replace =TRUE, prob = theta_c)
  PATIENTS <- list()
  N.max <- 1
  N.min <- Inf
  patient <-1
  cont <- 0
  while (patient <= N.total){
    ci <- c[patient]
    xi <- 1
    while (xi != FALSE){
      if (xi==1){ 
        prev.a <- sample(x = actions , size = 1, replace = TRUE, prob = initialization[[ci]][,2])
        prev.s <- 1
        sequence <- paste0(prev.a,prev.s)
        
      } else {
        a_t <- sample(x = c(actions,"END"), size = 1, replace = TRUE, prob = MarkovModel[[ci]][[1]][paste0(prev.a,prev.s),])
        s_t <- sample(x = 1:max.stages, size = 1, replace = TRUE, prob = MarkovModel[[ci]][[2]][paste0(a_t,prev.s),])
        sequence <- c(sequence, paste0(a_t,s_t))
        prev.a <- a_t
        prev.s <- s_t
      }
      if (sequence[length(sequence)] %in% paste0("END",min.stages:max.stages)) {
        xi <- FALSE
      } else {
        xi<- xi+1
      } 
    }
    if (length(sequence)>= (max.stages+5)){ 
      PATIENTS[[patient]] <- list(sequence,ci)
      N.max <- ifelse(N.max < length(sequence),length(sequence), N.max)
      N.min <- ifelse(N.min > length(sequence),length(sequence), N.min)
      patient <- patient + 1
    }
  }
  
  
  test <- as.data.frame(matrix(rep(NA, (N.total)*(N.max+2)), ncol=N.max+2 ))
  i<-(n+1)
  action <-1
  cont <- 1
  for (i in 1:N.total){
    test[cont,1]<- i # id
    test[cont, ncol(test)] <- PATIENTS[[i]][[2]] # class
    L <- length(PATIENTS[[i]][[1]])
    for (action in 1:L){
      a_t <-  PATIENTS[[i]][[1]][action]
      test[cont, action+1] <- a_t 
    }
    cont <- cont+1
  }
  return(test)
}

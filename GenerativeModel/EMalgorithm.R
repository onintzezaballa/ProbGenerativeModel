EM <- function( maxIter=1000,likelihoodEps = 1e-3, saveResults = TRUE){
  # Expectation-Maximization algorithm
  # INPUT
  # - maxIter: the maximum iteration of the algorithm
  # - likelihoodEps: the difference value between loglikelihood of two consecutive iterations in order to stop the algorithm
  # - saveResults: if TRUE, the results are saved in a folder
  
  # OUTPUT
  # - Markovmodel: learned generative model 
  # - initialization: learned generative model
  # - theta_c: learned generative model 
  # - LL: the loglikelihood of every iteration
  environment(Estep) <- environment()
  environment(Mstep) <- environment()
  iter<-1;stop <- FALSE
  LL<-c()
  
  while (stop == FALSE){
    print(iter)
    new.MarkovModel <- empty_MarkovModel(num.classes, num.stages, actions, min.stages) 
    new.initialization <- empty_initialModel(num.classes, num.stages, actions)
    new.theta_c <- c(rep(0,num.classes))
    mass.prob.likelihood <- list()
    for (i in 1:nrow(df)){
      A <- as.character(df[i,!is.na(df[i,])]); A <- A[-1]
      Estep_A <- Estep(A, initialization, MarkovModel)
      mass.prob.likelihood <- Mstep(A, initialization, MarkovModel, F.matrix=Estep_A[[1]] , G.matrix=Estep_A[[2]])
    }
    
    theta_c <- new.theta_c/sum(new.theta_c) 
    prev.MarkovModel <- MarkovModel
    prev.initialization <- initialization
    MarkovModel <- new.MarkovModel
    initialization <- new.initialization
    
    normalizedModels <- NormalizationAndSmoothing(MarkovModel, initialization)
    MarkovModel <- normalizedModels[[1]]
    initialization <- normalizedModels[[2]]
    
    LL<- c(LL,log_likelihood(num.classes, num.stages, theta_c, mass.prob.likelihood, min.stages))
    if (length(LL)>1){
      if (abs(LL[iter+1]-LL[iter]) < likelihoodEps | iter == maxIter) {stop <- TRUE}
    }
    iter <- iter+1
  }
  
  dataexport <- data.frame('Experiment' = seed,
                           'Numero Sec'= n,
                           'Numero clases'=num.classes,
                           'Numero stages'=num.stages,
                           'Minimum stage' = min.stages,
                           'Likelihood'=LL[length(LL)])
  print(dataexport)
  
  
  if (saveResults==TRUE){
    exportFile <- paste0(new.directory, "/generaldata_experiment",seed,".csv")
    write.table(x = dataexport, file = exportFile, sep = ',', row.names = F, dec = ".")
    
    for (ci in 1:num.classes){
      exportFile_MM1 <- paste0(new.directory,"/class",ci,"_MarkovModel1.csv")
      write.table(x = MarkovModel[[ci]][[1]], file = exportFile_MM1, sep = ',', row.names = T, dec = ".")
      exportFile_MM2 <- paste0(new.directory,"/class",ci,"_MarkovModel2.csv")
      write.table(x = MarkovModel[[ci]][[2]], file = exportFile_MM2, sep = ',', row.names = T, dec = ".")
      exportFile_init <- paste0(new.directory,"/class",ci,"_initialization.csv")
      write.table(x = initialization[[ci]], file = exportFile_init, sep = ',', row.names = T, dec = ".")
      
    }
    
    exportFile_classes <-  paste0(new.directory, "/probability_classes.csv")
    write.table(x = theta_c, file = exportFile_classes, sep = ',', row.names = F, dec = ".")
  }
  
  return(list(Markovmodel =MarkovModel, Initialization = initialization, ThetaC = theta_c, LogLikelihood= LL))
}




f <- function(s,t){
  # f function (dynamic programming) calculates the probability of all the possible sequences of pairs action-stage that
  # arrive at time t with the stage s assigned
  if (f.matrix[s,t]!=-1){
    return(f.matrix[s,t])
    
  } else if (t==length(A)) {
    aux1 <- f(s,t-1)*MarkovModel[[ci]][[1]][paste0(A[t-1],s),A[t]]*MarkovModel[[ci]][[2]][paste0(A[t],s),s]
    if (is.na(aux1)| aux1==-Inf | aux1==Inf) {aux1 <- 0}
    f.matrix[s,t] <<- aux1
    return(f.matrix[s,t])
    
  } else if (s==1) {
    aux1 <- f(s,t-1)*MarkovModel[[ci]][[1]][paste0(A[t-1],s),A[t]]*MarkovModel[[ci]][[2]][paste0(A[t],s),s]
    if (is.na(aux1)| aux1==-Inf | aux1==Inf) {aux1 <- 0}
    f.matrix[s,t] <<- aux1
    return(f.matrix[s,t])
    
  } else if (t>1 & s>1){
    aux1 <- f(s,t-1)*MarkovModel[[ci]][[1]][paste0(A[t-1],s),A[t]]*MarkovModel[[ci]][[2]][paste0(A[t],s),s]
    aux2 <- f(s-1,t-1)*MarkovModel[[ci]][[1]][paste0(A[t-1],s-1),A[t]]*MarkovModel[[ci]][[2]][paste0(A[t],s-1),s]
    if (is.na(aux1)| aux1==-Inf | aux1==Inf) {aux1 <- 0}
    if (is.na(aux2)| aux2==-Inf | aux2==Inf) {aux2 <- 0}
    f.matrix[s,t] <<- aux1 + aux2
    return(f.matrix[s,t])
  } 
}


g <- function(s,t){
  # g function (dynamic programming) calculates the probability of all the possible sequences of pairs action-stage that
  # departs from time t with the assigned stage s
  if (g.matrix[s,t]!=-1  ){
    return(g.matrix[s,t])
    
  } else if (s==num.stages) { 
    aux2 <- g(s,t+1)*MarkovModel[[ci]][[1]][paste0(A[t],s),A[t+1]]*MarkovModel[[ci]][[2]][paste0(A[t+1],s),s]
    if (is.na(aux2)| aux2==-Inf | aux2==Inf) {aux2 <- 0}
    g.matrix[s,t] <<- aux2
    return(g.matrix[s,t])
    
    
  } else if (t==length(A)-(num.stages-min.stages) & s < min.stages ) {
    aux1 <- 0
    g.matrix[s,t] <<- aux1
    return(g.matrix[s,t])
    
  } else if (t==length(A)-1) { 
    aux1 <- g(s,t+1)*MarkovModel[[ci]][[1]][paste0(A[t],s),A[t+1]]*MarkovModel[[ci]][[2]][paste0(A[t+1],s),s]
    if (is.na(aux1)| aux1==-Inf | aux1==Inf) {aux1 <- 0}
    g.matrix[s,t] <<- aux1
    return(g.matrix[s,t])
    
  } else if (t < length(A) & s < num.stages){
    aux1 <- g(s+1,t+1)*MarkovModel[[ci]][[1]][paste0(A[t],s),A[t+1]]*MarkovModel[[ci]][[2]][paste0(A[t+1],s),s+1]
    aux2 <- g(s,t+1)*MarkovModel[[ci]][[1]][paste0(A[t],s),A[t+1]]*MarkovModel[[ci]][[2]][paste0(A[t+1],s),s]
    if (is.na(aux1)| aux1==-Inf | aux1==Inf) {aux1 <- 0}
    if (is.na(aux2)| aux2==-Inf | aux2==Inf) {aux2 <- 0}
    g.matrix[s,t] <<- aux1 + aux2
    return(g.matrix[s,t])
  } 
}




Estep <- function(A, initialization, MarkovModel){
  # Expectation step of the algorithm
  # INPUT
  # A: a sequence of actions
  
  # OUTPUT
  # - F.matrix: a matrix with the probabily of arriving at stage s in time t (for all s and t)
  # - G.matrix: a matrix with the probability of departing from stage s in time t (for all s and t)
  environment(f) <- environment()
  environment(g) <- environment()
  F.matrix <- list() 
  G.matrix <- list() 
  for (ci in 1:num.classes){
    f.matrix <- matrix(rep(-1,num.stages*length(A)), ncol = length(A))
    f11 <- initialization[[ci]]$PROB[which(initialization[[ci]][,1]==paste0(A[1],1))]
    f.matrix[,1] <- c(f11,rep(0,num.stages -1))
    colnames(f.matrix) <- A
    rownames(f.matrix) <- 1:num.stages

    g.matrix <- matrix(rep(-1,num.stages*(length(A))), ncol = length(A))
    gnn <- MarkovModel[[ci]][[1]][paste0(A[length(A)-1],min.stages:num.stages),"END"]
    if (num.stages!=min.stages){
      g.matrix[,ncol(g.matrix)] <- c(rep(0,num.stages-length(gnn)),gnn)
      g.matrix[1:(min.stages-1),length(A)-(num.stages-min.stages)] <- 0
    } else {
      g.matrix[,ncol(g.matrix)] <- c(rep(0,num.stages-1),gnn)
      g.matrix[1:(num.stages-1),length(A)-1] <- 0
    }
    
    colnames(g.matrix) <- A
    rownames(g.matrix) <- 1:num.stages
    for (si in num.stages:min.stages){
      f.prob <- f(s = si, t = length(A))
    }
    g.prob <- g(s = 1, t = 1 )
    
    f.matrix[f.matrix==-1] <- 0
    g.matrix[g.matrix==-1] <- 0
    F.matrix[[ci]] <- f.matrix
    G.matrix[[ci]] <- g.matrix
    
  }

  return(list(F.matrix,G.matrix))
}


Mstep <- function(A, initialization, MarkovModel, F.matrix, G.matrix){
  # Maximimization step 
  # INPUT
  # - A: a sequence of actions
  # - F.matrix: a matrix with the probabily of arriving at stage s in time t (for all s and t)
  # - G.matrix: a matrix with the probability of departing from stage s in time t (for all s and t)
  
  # OUTPUT
  # mass.prob.likelihood: the probability mass of each possible sequence of stages for a given sequence of actions A
  mass.prob.likelihood.i <-list()
  for (t in length(A):2){ 
    if (t==2){ 
      s <- 1 
      s.prim <- 1:num.stages
      mass.prob <- list()
      for (ci in 1:num.classes){
        pp <- MarkovModel[[ci]][[1]][paste0(A[t-1],s),A[t]]*MarkovModel[[ci]][[2]][paste0(A[t],s), s.prim]
        mass.prob.i <- F.matrix[[ci]][s,t-1]*pp*G.matrix[[ci]][s.prim,t] 
        mass.prob[[ci]] <- mass.prob.i/sum(mass.prob.i)
      }
      for (ci in 1:num.classes){
        mass.prob[[ci]] <- mass.prob[[ci]]*theta_c.a[ci]
        mass.prob[[ci]][is.na(mass.prob[[ci]])] <- 0
        new.MarkovModel[[ci]][[1]][paste0(A[t-1],s), A[t] ] <<- new.MarkovModel[[ci]][[1]][paste0(A[t-1],s), A[t]] +  sum(mass.prob[[ci]])
        new.MarkovModel[[ci]][[2]][paste0(A[t],s), s.prim] <<- new.MarkovModel[[ci]][[2]][paste0(A[t],s), s.prim] + mass.prob[[ci]]
      }
      
     
    } else if (t==length(A)){
      s <- min.stages:num.stages
      s.prim <- min.stages:num.stages
      mass.prob <- list()
      theta_c.a <- c(rep(0,num.classes))
      ci<-1
      for (ci in 1:num.classes){
        pp <- MarkovModel[[ci]][[1]][paste0(A[t-1],s), A[t]]*MarkovModel[[ci]][[2]][paste0(A[t],s), s.prim]
        mass.prob.i <- F.matrix[[ci]][s,t-1]*pp 
        mass.prob[[ci]] <- mass.prob.i/sum(mass.prob.i)
        theta_c.a[ci] <- sum(mass.prob.i)
      }
      theta_c.a <- theta_c.a/sum(theta_c.a) 
      theta_c.a[which(is.na(theta_c.a))] <- 0 
      for (ci in 1:num.classes){
        mass.prob[[ci]] <- mass.prob[[ci]]*theta_c.a[ci]
        mass.prob[[ci]][is.na(mass.prob[[ci]])] <-  0 
        new.theta_c[ci] <<- new.theta_c[ci] + theta_c.a[ci] 
        if (min.stages == num.stages) {
          new.MarkovModel[[ci]][[1]][paste0(A[t-1],s), A[t]] <<- new.MarkovModel[[ci]][[1]][paste0(A[t-1],s), A[t]] + mass.prob[[ci]]
          new.MarkovModel[[ci]][[2]][paste0(A[t],s), s.prim] <<- new.MarkovModel[[ci]][[2]][paste0(A[t],s), s.prim] + mass.prob[[ci]]
        } else {
          new.MarkovModel[[ci]][[1]][paste0(A[t-1],s.prim), A[t] ] <<- new.MarkovModel[[ci]][[1]][paste0(A[t-1],s.prim), A[t]] + rowSums(mass.prob[[ci]])
          new.MarkovModel[[ci]][[2]][paste0(A[t],s.prim), s.prim] <<-  new.MarkovModel[[ci]][[2]][paste0(A[t],s.prim), s.prim] + mass.prob[[ci]]
        }
      }
      mass.prob.likelihood[[i]] <- mass.prob
      
    } else {
      if(A[t]=="END"){
        s.prim <- min.stages:num.stages
      } else {
        s.prim <- 1:num.stages
      }
      mass.prob <- list()
      for (ci in 1:num.classes){
        pp <- MarkovModel[[ci]][[1]][paste0(A[t-1],s.prim),A[t]]*MarkovModel[[ci]][[2]][paste0(A[t],s.prim), s.prim]
        mass.prob.i <- t(F.matrix[[ci]][s.prim,t-1]*pp)*G.matrix[[ci]][s.prim,t]
        mass.prob[[ci]] <- t(mass.prob.i)
        mass.prob[[ci]] <- mass.prob[[ci]]/sum(mass.prob[[ci]])
      }
      for (ci in 1:num.classes){
        mass.prob[[ci]] <- mass.prob[[ci]]*theta_c.a[ci]
        mass.prob[[ci]][is.na(mass.prob[[ci]])] <- 0
        new.MarkovModel[[ci]][[1]][paste0(A[t-1],s.prim), A[t] ] <<- new.MarkovModel[[ci]][[1]][paste0(A[t-1],s.prim), A[t]] + rowSums(mass.prob[[ci]])
        new.MarkovModel[[ci]][[2]][paste0(A[t],s.prim), s.prim] <<- new.MarkovModel[[ci]][[2]][paste0(A[t],s.prim), s.prim] + mass.prob[[ci]]
      }
    }
  }
  for (ci in 1:num.classes){
    new.initialization[[ci]]$PROB[which(new.initialization[[ci]][,1] == paste0(A[1],1))] <<- new.initialization[[ci]]$PROB[which(new.initialization[[ci]][,1] == paste0(A[1],1))] +
      theta_c.a[ci]
  }
  
  return( mass.prob.likelihood)
  
}



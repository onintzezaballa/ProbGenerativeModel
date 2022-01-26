# Model and sequence generator

empty_initialModel <- function(num.classes, num.stages, actions){
  # INPUT
  # - num.classes: number of classes
  # - num.stages: number of maximum stages
  # - actions: a vector with the possible values of actions
  
  # OUTPUT
  # - initialization: empty initialization matrices of the classes
  
  
  initialization <- list()  
  for (ci in 1:num.classes){
    PROB <- 0
    initialization[[ci]]<- data.frame(actions,PROB)
    colnames(initialization[[ci]]) <- c("X","PROB")
    initialization[[ci]][,1] <- paste0(initialization[[ci]][,1],1)
  }
  return(initialization)
}

empty_MarkovModel <- function(num.classes, num.stages, actions, min.stages = num.stages){
  # INPUT
  # - num.classes: number of classes
  # - num.stages: number of maximum stages
  # - actions: a vector with the possible values of actions
  # - min.stages: minimum number of stages
  
  # OUTPUT
  # - MarkovModel: empty markov model matrices for each class: p(a_t|a_{t-1}, s_{t-1},c) and p(s_t|s_{t-1}, a_t,c)
  
  if (min.stages>1){
    names.without.end.1<- paste0(rep(actions,times=min.stages-1),rep(1:num.stages, each = length(actions))) 
    names.without.end.2<- paste0(rep(actions,times=min.stages-1),rep(1:(min.stages-1), each = length(actions)))
  } else {
    names.without.end.1<- paste0(rep(actions,times=min.stages),rep(1:num.stages, each = length(actions))) 
    
    names.without.end.2<-c()
  }
  names.with.end.2 <- paste0(rep(c(actions,"END"),times=length(min.stages:num.stages)),rep(min.stages:num.stages, each = length(actions)+1)) 
  names.2 <-  c(names.without.end.2,names.with.end.2)

  MarkovModel<-  list()
  for (ci in 1:num.classes){
    MarkovModel[[ci]] <- list()
    #1: p(at|st-1,at-1)
    MarkovModel[[ci]][[1]] <- as.data.frame(matrix(rep(0,(length(names.without.end.1))*(length(actions)+1)), ncol = length(actions)+1))
    rownames(MarkovModel[[ci]][[1]]) <- names.without.end.1
    colnames(MarkovModel[[ci]][[1]]) <- c(actions,"END")
    #2: p(at|st-1,at)
    MarkovModel[[ci]][[2]] <- as.data.frame(matrix(rep(0,length(names.2)*num.stages), ncol = num.stages))
    rownames(MarkovModel[[ci]][[2]]) <- names.2
    colnames(MarkovModel[[ci]][[2]]) <- 1:num.stages
  }
  
  return(MarkovModel)
}



generateModel <- function(actions, n, num.classes, num.stages, min.stages,seed=1){ 
  
  # INPUT
  # - actions: a vector with the values of the possible actions
  # - n: number of sequences to be sampled
  # - num.classes: number of latent classes
  # - num.stages: maximum number of stages
  # - min.stages: minimum number of stages
  # seed
  
  # OUTPUT
  # - MarkovModel: a list with randomly generated markov model per class. For each class, the first model is p(a_t|a_{t-1},s_{t-1},c) and the second model
  #              is p(s_t|s{t-1}, a_t,c)
  # - initialization: a list with randomly generated initialization of actions per class.
  # - theta_c: randomly generated probability of the classes c

  print(paste0("Number of sequences: ", n))
  print(paste0("Number of classes: ", num.classes))
  print(paste0("Maximum number of stages: ", num.stages))
  print(paste0("Minimum number of stages: ", min.stages))
  print(paste0("Seed: ", seed))
  
  
  A_S <- paste0(rep(actions,times=num.stages),rep(1:num.stages, each = length(actions))) 
  A_S <- c(A_S,paste0("END",num.stages))
  
  set.seed(seed)
  MarkovModel <- empty_MarkovModel(num.classes, num.stages, actions, min.stages)
  
  for (ci in 1:num.classes){
    L <- length(actions)
    si <-2
    for (si in 1:num.stages){
      if (si == 1){
        row.pos1 <- L
        row.pos2 <- L
        MarkovModel[[ci]][[1]][ 1:row.pos1, 1:L] <- as.matrix(rdirichlet(n = L, alpha= c(rep(1, L))))
        MarkovModel[[ci]][[2]][ 1:row.pos2, 1:2] <- as.matrix(rdirichlet(n = L, alpha= c(0.7,0.2)))
        
      } else if (si==num.stages){
        MarkovModel[[ci]][[1]][ (row.pos1+1):(row.pos1+L), 1:(L+1)] <- as.matrix(rdirichlet(n = L, alpha= c(rep(1, L+1))))
        
        MarkovModel[[ci]][[2]][ (row.pos2+1):(row.pos2+L+1), num.stages] <- as.matrix(rep(1,L+1))
        
      } else if (si>=min.stages){
        MarkovModel[[ci]][[1]][ (row.pos1+1):(row.pos1+L), 1:(L+1)] <- as.matrix(rdirichlet(n = L, alpha= c(rep(0.7, L),0.3)))
        MarkovModel[[ci]][[2]][ (row.pos2+1):(row.pos2+L+1), si:(si+1)] <- as.matrix(rdirichlet(n = L+1, alpha= c(0.7,0.3)))
        MarkovModel[[ci]][[2]][paste0("END",si), as.integer(colnames(MarkovModel[[ci]][[2]])) != si] <- 0
        MarkovModel[[ci]][[2]][paste0("END",si), as.integer(colnames(MarkovModel[[ci]][[2]])) == si] <- 1
        row.pos1 <- row.pos1+L
        row.pos2 <- row.pos2+L+1
        
        
      } else {
        MarkovModel[[ci]][[1]][ (row.pos1+1):(row.pos1+L), 1:L] <- as.matrix(rdirichlet(n = L, alpha= c(rep(1, L))))
        MarkovModel[[ci]][[2]][ (row.pos2+1):(row.pos2+L), si:(si+1)] <- as.matrix(rdirichlet(n = L, alpha= c(0.7,0.3)))
        row.pos1 <- row.pos1 + L
        row.pos2 <- row.pos2 + L
      }
    }
  }
  
  initialization <- empty_initialModel(num.classes, num.stages, actions) 
  for (ci in 1:num.classes){
    initialization[[ci]][,2] <- as.vector(rdirichlet(1, alpha= rep(1, nrow(initialization[[ci]]))))
  }
  
  theta_c <- rdirichlet(1, alpha=rep(1,num.classes))
  
  return(list(MarkovModel, initialization, theta_c))
  
}

generateSequences <- function(n, num.stages, min.stages, MarkovModel, initialization, theta_c, new.directory, seed=1){
  # This function samples sequences from the generative model
  
  ### INPUT
  # - n: number of sequences to be sampled
  # - num.stages: maximum number of stages
  # - min.stages: minimum number of stages
  # - MarkovModel: a list with randomly generated markov model per class. For each class, the first model is p(a_t|a_{t-1},s_{t-1},c) and the second model
  #                is p(s_t|s{t-1}, a_t,c)
  # - initialization: a list with randomly generated initialization of actions per class.
  # - theta_c: randomly generated probability of the classes c
  # - seed

  
  ### OUTPUT
  # df: dataset with sequences randomly generated using the probabilistic model
  
  num.classes <- length(theta_c)
  actions <- colnames(MarkovModel[[1]][[1]])[-ncol(MarkovModel[[1]][[1]])]
  c <- sample(x=1:num.classes, size=n, replace =TRUE, prob = theta_c)
  
  PATIENTS <- list()
  N.max<-1
  N.min <- Inf
  cont <- 0
  patient <- 1
  while (patient <= n){
    ci <- c[patient]
    ai <- 1
    while (ai != FALSE){
      if (ai==1){ 
        prev.a <- sample(x = actions , size = 1, replace = TRUE, prob = initialization[[ci]][,2])
        prev.s <- 1
        sequence <- paste0(prev.a,prev.s)
        
      } else {
        a_t <- sample(x = c(actions,"END"), size = 1, replace = TRUE, prob = MarkovModel[[ci]][[1]][paste0(prev.a,prev.s),])
        s_t <- sample(x = 1:num.stages, size = 1, replace = TRUE, prob = MarkovModel[[ci]][[2]][paste0(a_t,prev.s),])
        sequence <- c(sequence, paste0(a_t,s_t))
        prev.a <- a_t
        prev.s <- s_t
      }
      if (sequence[length(sequence)] %in% paste0("END",min.stages:num.stages)) {
        ai <- FALSE
      } else {
        ai<- ai+1
      } 
    }
    if (length(sequence)>= (num.stages+5)){
      PATIENTS[[patient]] <- list(sequence,ci)
      N.max <- ifelse(N.max < length(sequence),length(sequence), N.max)
      N.min <- ifelse(N.min > length(sequence),length(sequence), N.min)
      patient <- patient + 1
    }
  }
  
  
  df <- as.data.frame(matrix(rep(NA, n*(N.max+2)), ncol=N.max+2 ))
  i<-1
  action <-1
  for (i in 1:length(PATIENTS)){
    df[i,1]<- i # id
    df[i, ncol(df)] <- PATIENTS[[i]][[2]] # class
    L <- length(PATIENTS[[i]][[1]])
    for (action in 1:L){
      a_t <-  PATIENTS[[i]][[1]][action]
      df[i, action+1] <- a_t 
    }
  }
  
  dir.create(new.directory, showWarnings=FALSE)
  exportFile <- paste0(new.directory, "/data.csv")
  write.table(x = df, file = exportFile, sep = ',', row.names = F, dec = ".")
  
  for (ci in 1:num.classes){
    exportFile_MM1 <- paste0(new.directory,"/real_class",ci,"_MarkovModel1.csv")
    write.table(x = MarkovModel[[ci]][[1]], file = exportFile_MM1, sep = ',', row.names = T, dec = ".")
    exportFile_MM2 <- paste0(new.directory,"/real_class",ci,"_MarkovModel2.csv")
    write.table(x = MarkovModel[[ci]][[2]], file = exportFile_MM2, sep = ',', row.names = T, dec = ".")
    exportFile_init <- paste0(new.directory,"/real_class",ci,"_initialization.csv")
    write.table(x = initialization[[ci]], file = exportFile_init, sep = ',', row.names = T, dec = ".")
    
  }
  exportFile_classes <-  paste0(new.directory, "/real_probability_classes.csv")
  write.table(x = theta_c, file = exportFile_classes, sep = ',', row.names = F, dec = ".")

  return(df)
}

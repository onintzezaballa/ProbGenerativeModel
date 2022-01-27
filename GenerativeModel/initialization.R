ParameterInitialization <- function(df, actions, num.classes, max.stages, min.stages = max.stages, seed=1){
  # This function randomly generates parameters of the model in order to initialize the EM algorithm
  # INPUT
  # - df: dataset with each sequence in a raw
  # - actions: possible medical actions
  # - num.classes: number of classes
  # - max.stages: maximum number of stages for a given sequence of actions
  # - min-stages: minumum number of stages for a given sequence of actions
  
  # OUTPUT
  # - initialization: probabilities of initial actions of the sequences
  # - MarkovModel: a list of models with the transition probabilities between actions and stages for each class

  num.actions <- length(actions)
  classes <- as.numeric(df[,ncol(df)]) # initialization of classes to create theta_c
  df <- df[,-ncol(df)]
  MarkovModel <- empty_MarkovModel(num.classes, max.stages, actions, min.stages)
  initialization <- empty_initialModel(num.classes, max.stages, actions) # list
  S <- list()
  for (patient in 1:nrow(df)){
    a <- as.character(df[patient,!is.na(df[patient,])])
    a <- a[-1] # patient id
    prob.c <- rep(0, num.classes)
    if (num.classes ==1){
      prob.c <- 1
    } else {
      prob.c[classes[patient]] <-(1/num.classes) + 0.1
      prob.c[-classes[patient]] <- (1-prob.c[classes[patient]])/(num.classes-1)
    }
    
    if (min.stages == max.stages){
      patient.i.stages <- max.stages
    } else {
      probability <- c(rep(0.2, length(min.stages:(max.stages-1))), 0.8)
      patient.i.stages <- sample(x = min.stages:max.stages, size = 1, prob=probability)
    }

    s_i <- cut(1:length(a), patient.i.stages, labels=F)
    a_i <- a
    a <- paste0(a,s_i)
    S[[patient]]<- a
    for (ci in 1:num.classes){
      initialization[[ci]]$PROB[which(initialization[[ci]][,1]==a[1])] <- initialization[[ci]]$PROB[which(initialization[[ci]][,1]==a[1])] +prob.c[ci]
      freqmatrix.Ai <- table(a[-length(a)], a_i[-1]) 
      MarkovModel[[ci]][[1]][rownames(freqmatrix.Ai), colnames(freqmatrix.Ai)]<- MarkovModel[[ci]][[1]][rownames(freqmatrix.Ai), colnames(freqmatrix.Ai)] + freqmatrix.Ai*prob.c[ci] 
      freqmatrix.Si <- table( c(paste0(a_i[-1], s_i[-length(s_i)])), (s_i[-1]))
      MarkovModel[[ci]][[2]][rownames(freqmatrix.Si), colnames(freqmatrix.Si)]<- MarkovModel[[ci]][[2]][rownames(freqmatrix.Si), colnames(freqmatrix.Si)] + freqmatrix.Si*prob.c[ci]
    }
  }
  
  return(list(initialization,MarkovModel))
}

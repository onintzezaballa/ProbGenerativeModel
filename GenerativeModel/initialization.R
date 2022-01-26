ParameterInitialization <- function(df, actions, num.classes, num.stages, num.actions, min.stages = num.stages, seed=1){
  # This function randomly generates parameters of the model for the initialization of the EM algorithm
  
  classes <- as.numeric(df[,ncol(df)])
  df <- df[,-ncol(df)]
  MarkovModel <- empty_MarkovModel(num.classes, num.stages, actions, min.stages)
  initialization <- empty_initialModel(num.classes, num.stages, actions) # list
  S <- list()
  for (patient in 1:nrow(df)){
    a <- as.character(df[patient,!is.na(df[patient,])])
    a <- a[-1]
    prob.c <- rep(0, num.classes)
    if (num.classes ==1){
      prob.c <- 1
    } else {
      prob.c[classes[patient]] <-(1/num.classes) + 0.1
      prob.c[-classes[patient]] <- (1-prob.c[classes[patient]])/(num.classes-1)
    }
    
    if (min.stages == num.stages){
      patient.i.stages <- num.stages
    } else {
      probability <- c(rep(0.2, length(min.stages:(num.stages-1))), 0.8)
      patient.i.stages <- sample(x = min.stages:num.stages, size = 1, prob=probability)
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
  
  return(list(initialization,MarkovModel, classes))
}

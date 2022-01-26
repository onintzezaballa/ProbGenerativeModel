
adjust_normalization_epsilon <- function(MarkovModel, value1, value2,  actions, num.stages, ci, min.stages = num.stages){
  # This function adjust the MarkovModel in order to have a minimum value in each transition
  # INPUT
  # - value1: the minimum probability for the transitions p(a_t|a_{t-1}, s_{t-1},c)
  # - value2: the minimum probability for the transitions p(s_t| s_{t-1}, a_t, c)
  
  # OUTPUT
  # - MarkovModel
  
  L <- length(actions)
  for (si in 1:num.stages){
    if (si == 1){
      row.pos1 <- L
      row.pos2 <- L
      MarkovModel[[ci]][[1]][ 1:row.pos1, 1:L] <- MarkovModel[[ci]][[1]][ 1:row.pos1, 1:L]*(1-(value1*L))
      MarkovModel[[ci]][[2]][ 1:row.pos2, si:(si+1)] <- MarkovModel[[ci]][[2]][ 1:row.pos2, si:(si+1)]*(1-(2*value2))
    } else if (si==num.stages){
      MarkovModel[[ci]][[1]][(row.pos1+1):(row.pos1+L), 1:(L+1)] <- MarkovModel[[ci]][[1]][(row.pos1+1):(row.pos1+L) ,1:(L+1)]*(1-(value1*(L+1)))
      MarkovModel[[ci]][[2]][ (row.pos2+1): (row.pos2+L+1), si] <- MarkovModel[[ci]][[2]][(row.pos2+1): (row.pos2+L+1), si]*(1-(value2))
      # este ult se podria eliminar
    } else if (si>=min.stages){
      MarkovModel[[ci]][[1]][ (row.pos1+1):(row.pos1+L), 1:(L+1)] <- MarkovModel[[ci]][[1]][(row.pos1+1):(row.pos1+L), 1:(L+1)]*(1-(value1*(L+1)))
      MarkovModel[[ci]][[2]][(row.pos2+1): (row.pos2+L),si:(si+1)] <- MarkovModel[[ci]][[2]][(row.pos2+1): (row.pos2+L),si:(si+1)]*(1-(2*value2))
      row.pos1 <- row.pos1 + L 
      row.pos2 <- row.pos2 + L + 1
    } else {
      MarkovModel[[ci]][[1]][ (row.pos1+1):(row.pos1+L),1:L] <- MarkovModel[[ci]][[1]][ (row.pos1+1):(row.pos1+L), 1:L]*(1-(value1*(L)))
      MarkovModel[[ci]][[2]][ (row.pos2+1): (row.pos2+L), si:(si+1)] <- MarkovModel[[ci]][[2]][(row.pos2+1): (row.pos2+L), si:(si+1)]*(1-(2*value2)) # solo pueden quedarse o avanzar
      row.pos1 <- row.pos1 + L
      row.pos2 <- row.pos2 + L
    }
  }
  return(MarkovModel)
}

addEpsilon <- function(value1, value2, actions, num.stages, min.stages = num.stages){
  # This function creates a matrix with epsilon values 
  
  # INPUT
  # - value1: the minimum probability for the transitions p(a_t|a_{t-1}, s_{t-1},c)
  # - value2: the minimum probability for the transitions p(s_t| s_{t-1}, a_t, c)
  
  # OUTPUT
  # - epsilon.matrix: a matrix with epsilon values
  names <- paste0(rep(actions,times=num.stages),rep(1:num.stages, each = length(actions))) 
  epsilon.matrix <- list()
  epsilon.matrix[[1]] <- matrix(rep(0,(length(names)*(length(actions)+1))), ncol = length(actions)+1)
  colnames(epsilon.matrix[[1]]) <- c(actions,"END")
  rownames(epsilon.matrix[[1]]) <- names
  L <- length(actions)
  si<-1
  for (si in 1:num.stages){
    if (si == 1){
      row.pos <- L
      epsilon.matrix[[1]][ 1: (si*L), 1:L] <- matrix(rep(rep(value1, L),L),nrow=L, byrow=T)
    } else if (si>=min.stages){
      epsilon.matrix[[1]][ (row.pos + 1): (row.pos + L ), 1:(L+1)] <- matrix(rep(rep(value1, L+1),L),nrow=L, byrow=T)
      row.pos <- row.pos + L 
    } else {
      epsilon.matrix[[1]][ (row.pos + 1): (row.pos + L), 1:L] <- matrix(rep(rep(value1, L),L),nrow=L, byrow=T)
      row.pos <- row.pos + L
    }
  }
  
  names.without.end<- paste0(rep(actions,times=min.stages-1),rep(1:(min.stages-1), each = length(actions))) #(xt,st-1)
  names.with.end <- paste0(rep(c(actions,"END"),times=length(min.stages:num.stages)),rep(min.stages:num.stages, each = length(actions)+1)) 
  names2 <- c(names.without.end,names.with.end)
  
  
  epsilon.matrix[[2]] <- matrix(rep(0,(length(names2)*(num.stages))), ncol = num.stages)
  rownames(epsilon.matrix[[2]]) <- names2
  colnames(epsilon.matrix[[2]]) <- 1:num.stages
  L <- length(actions)
  for (si in 1:num.stages){
    if (si == 1){
      row.pos <- L
      epsilon.matrix[[2]][ 1:L, si:(si+1)] <- matrix(rep(rep(value2, 2),L),nrow=L, byrow=T)
    } else if (si==num.stages){
      epsilon.matrix[[2]][ (row.pos + 1): (row.pos + L +1), si] <- matrix(rep(value2, L+1),ncol=L+1, byrow=T)
    } else if (si>=min.stages){
      epsilon.matrix[[2]][ (row.pos + 1): (row.pos + L+1), si:(si+1)] <-matrix(rep(rep(value2, 2),L+1),nrow=L+1, byrow=T)
      epsilon.matrix[[2]][paste0("END",si), as.integer(colnames(epsilon.matrix[[2]])) != si] <- 0
      epsilon.matrix[[2]][paste0("END",si), as.integer(colnames(epsilon.matrix[[2]])) == si] <- 0
      row.pos <- row.pos + L + 1
      
    } else {
      epsilon.matrix[[2]][ (row.pos + 1): (row.pos+L), si:(si+1)] <- matrix(rep(rep(value2, 2),L),nrow=L, byrow=T)
      row.pos <- row.pos + L
    }
  }
  return(epsilon.matrix)
}

NormalizationAndSmoothing <- function(MarkovModel, initialization, epsilon=1e-2){
  # This function performs an smoothing of the parameters of the model in order to have a minimum probability in every transition
  
  for (ci in 1:num.classes){
    MarkovModel[[ci]][[1]] <- MarkovModel[[ci]][[1]]/rowSums(MarkovModel[[ci]][[1]])
    MarkovModel[[ci]][[2]] <- MarkovModel[[ci]][[2]]/rowSums(MarkovModel[[ci]][[2]])
    MarkovModel[[ci]][[1]][(is.na(MarkovModel[[ci]][[1]]))] <- 0
    MarkovModel[[ci]][[2]][(is.na(MarkovModel[[ci]][[2]]))] <- 0
    MarkovModel <- adjust_normalization_epsilon(MarkovModel, value1 = epsilon, value2=epsilon,  actions, num.stages, ci, min.stages) # minimo prob de epsilon
    epsilon.matrix <-  addEpsilon(value1 = epsilon, value2 = epsilon,  actions, num.stages, min.stages)
    MarkovModel[[ci]][[1]] <- MarkovModel[[ci]][[1]] +  epsilon.matrix[[1]]
    MarkovModel[[ci]][[2]] <- MarkovModel[[ci]][[2]] +  epsilon.matrix[[2]]
    # Para los que son completamente 0
    MarkovModel[[ci]][[1]] <- MarkovModel[[ci]][[1]]/rowSums(MarkovModel[[ci]][[1]])
    MarkovModel[[ci]][[2]] <- MarkovModel[[ci]][[2]]/rowSums(MarkovModel[[ci]][[2]])
    # Normalization init
    initialization[[ci]]$PROB <- initialization[[ci]]$PROB/ sum(initialization[[ci]]$PROB)
    initialization[[ci]]$PROB <- initialization[[ci]]$PROB*(1-(epsilon*(length(initialization[[ci]]$PROB)+1)))
    epsilon.matrix <-  rep(epsilon, length(initialization[[ci]]$PROB))
    initialization[[ci]]$PROB<- initialization[[ci]]$PROB +  epsilon.matrix
    initialization[[ci]]$PROB <- initialization[[ci]]$PROB/ sum(initialization[[ci]]$PROB)
  }
  return(list(MarkovModel, initialization))  
  
}

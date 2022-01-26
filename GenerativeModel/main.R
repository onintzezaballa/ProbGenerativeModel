
source("github/generateModel.R")
source("github/initialization.R")
source("github/smoothing.R")
source("github/EMalgorithm.R")
source("github/Loglikelihood.R")


actions <- c( "A", "B", "C", "D", "E")
n <- 100
num.classes <- 2
num.stages <- 2
min.stages <- 2
seed <- 1

new.directory <- paste0("github/",n,"_",num.classes,"classes_",num.stages, "stages_",min.stages, "minstages_",seed)

gen_Model <- generateModel(actions, n, num.classes, num.stages, min.stages,  seed=1) 
original_MarkovModel <- gen_Model[[1]]
original_initialization <- gen_Model[[2]]
original_theta_c <- gen_Model[[3]]
originalClasses <- df[,ncol(df)]

df <- generateSequences(n, num.stages, min.stages, original_MarkovModel, original_initialization, original_theta_c, new.directory, seed=1);df <- assign("df", df, envir = .GlobalEnv)
invisible(sapply(1:nrow(df), function(x){  pos <- which(!is.na(df[x,])); pos <- pos[-c(1,length(pos))]  ; df[x,pos] <<- substr(df[x,pos],start= 1, stop=nchar(df[x,pos])-1)})) # eliminate the real class and stages


num.actions <- length(actions)

parameter_initialization <- ParameterInitialization(df, actions, num.classes, num.stages , num.actions, min.stages, seed=1) # initialization of Markov Models and classes randomly

initialization <- parameter_initialization[[1]]; assign("initialization", initialization, envir = .GlobalEnv)
MarkovModel <- parameter_initialization[[2]]; assign("MarkovModel", MarkovModel, envir = .GlobalEnv)
classes <- parameter_initialization[[3]]
num.classes <- length(MarkovModel)
normalizedModels <- NormalizationAndSmoothing(MarkovModel, initialization)
MarkovModel <- normalizedModels[[1]]
initialization <- normalizedModels[[2]]


df <- df[,-ncol(df)] 


(EMresults <- EM(maxIter=2))





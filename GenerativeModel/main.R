# 
# This file includes code for the paper A Probabilistic Generative Model for Learning the Progression Patterns of Treatments. 
# @author: Onintze Zaballa
# 
source("GenerativeModel/generateModel.R")
source("GenerativeModel/initialization.R")
source("GenerativeModel/smoothing.R")
source("GenerativeModel/EMalgorithm.R")
source("GenerativeModel/Loglikelihood.R")

source("github/GenerativeModel/generateModel.R")
source("github/GenerativeModel/initialization.R")
source("github/GenerativeModel/smoothing.R")
source("github/GenerativeModel/EMalgorithm.R")
source("github/GenerativeModel/Loglikelihood.R")

# Parameters of the probabilistic generative model:
actions <- c( "A", "B", "C", "D", "E") # possible actions in a sequence
n <- 100 # number of sequences to be sampled  
num.classes <- 2 # number of classes
max.stages <- 3 # maximum number of stages in a sequence
min.stages <- 2 # minimum number of stages in a sequence
seed <- 1

new.directory <- paste0("github/GenerativeModel/",n,"_",num.classes,"classes_",max.stages, "stages_",min.stages, "minstages_",seed) # create a directory to save the results
dir.create(new.directory, showWarnings=FALSE)

# Model generation and sequence sampling
gen_Model <- generateModel(actions, n, num.classes, max.stages, min.stages,  seed=1) 
original_MarkovModel <- gen_Model[[1]]
original_initialization <- gen_Model[[2]]
original_theta_c <- gen_Model[[3]]

df <- generateSequences(n, max.stages, min.stages, original_MarkovModel, original_initialization, original_theta_c, new.directory, seed=1); df <- assign("df", df, envir = .GlobalEnv)

originalClasses <- df[,ncol(df)]
originalStages <-df[,-ncol(df)]; invisible(sapply(1:nrow(originalStages), function(x){  pos <- which(!is.na(originalStages[x,])); pos <- pos[-1]  ;originalStages[x,pos] <<- substr(originalStages[x,pos],start= nchar(originalStages[x,pos]), stop = nchar(originalStages[x,pos]))}))
invisible(sapply(1:nrow(df), function(x){  pos <- which(!is.na(df[x,])); pos <- pos[-c(1,length(pos))]  ; df[x,pos] <<- substr(df[x,pos],start= 1, stop=nchar(df[x,pos])-1)})) 


# Random initialization of the parameters of the model
parameter_initialization <- ParameterInitialization(df, actions, num.classes, max.stages , min.stages, seed=1) # initialization of Markov Models and classes randomly
initialization <- parameter_initialization[[1]]; assign("initialization", initialization, envir = .GlobalEnv)
MarkovModel <- parameter_initialization[[2]]; assign("MarkovModel", MarkovModel, envir = .GlobalEnv)
num.classes <- length(MarkovModel)
df <- df[,-ncol(df)]
normalizedModels <- NormalizationAndSmoothing(MarkovModel, initialization)
MarkovModel <- normalizedModels[[1]]
initialization <- normalizedModels[[2]]


# Expectation-Maximization algorithm
(EMresults <- EM(maxIter=2))

# EM function returns the learned model parameters and the loglikelihood calculated in each iteration of the algorithm





require(ggplot2)
require(MCMCpack)
source("Evaluation/evaluation_functions.R")

#### PARAMETERS ####
N <- c(300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000)
num.classes <- 3
max.stages <- 4
min.stages <- 3
actions <- c( "A", "B", "C", "D", "E", "G", "H", "I", "J", "K")
seeds <- c(2) #, 5, 8, 10, 12)
#########

data <- data.frame('Seed'= 0,
                   'N.Sec'= 0,
                   'N.classes'= 0,
                   'N.stages'= 0,
                   'Minimum stage' = 0,
                   'll_train_train'= 0,
                   'll_train_test'= 0,
                   'll_real_train'= 0,
                   'll_real_test'= 0)


for (seed in seeds){
  n<- N[1]

  data.directory <- paste0("data/",n,"_",num.classes,"classes_",max.stages,"stages_",min.stages,"minstages_",seed)   # Import the original model
  
  # Original model:
  original_MarkovModel <- lapply(1:num.classes, function(ci){file1 <- paste0(data.directory,"/real_class",ci,"_MarkovModel1.csv"); file2 <- paste0(data.directory,"/real_class",ci,"_MarkovModel2.csv") 
    MM <- list(as.data.frame(read.csv(file1, header = T, sep = "," )), as.data.frame(read.csv(file2, header = T, sep = "," ) ));
    colnames(MM[[2]]) <- 1:max.stages;
    MM} )
  original_initialization <- lapply(1:num.classes, function(ci){ file <- paste0(data.directory,"/real_class",ci,"_initialization.csv");
    as.data.frame(read.csv(file, header = T, sep = ","))})
  
  original_theta.c <- read.csv(paste0(data.directory,"/real_probability_classes.csv"), header = T, sep = "," )

  test <- Test_Generator(seed, N.total=4000, original_theta.c, original_MarkovModel, original_initialization) # 4000 sequences for test dataset
  ll_real_test <- likelihood.a(test, num.classes, max.stages, original_theta.c, original_MarkovModel, original_initialization, min.stages) # evaluate the test dataset in the original model
  

  for (n in N){
    data.directory <- paste0("data/",n,"_",num.classes,"classes_",max.stages,"stages_",min.stages,"minstages_",seed) 

    train <- read.csv(paste0(data.directory,"/data.csv")) # train data
    
    # Learned model:
    learned_MarkovModel <- lapply(1:num.classes, function(ci) {file1 <- paste0(data.directory,"/class",ci,"_MarkovModel1.csv"); 
                                                              file2 <- paste0(data.directory,"/class",ci,"_MarkovModel2.csv");
                                                              MM <- list(as.data.frame(read.csv(file1, header = T, sep = "," )), 
                                                                         as.data.frame(read.csv(file2, header = T, sep = "," ) ));
                                                               colnames(MM[[2]]) <- 1:max.stages;
                                                               MM })
    learned_initialization <- lapply(1:num.classes, function(ci){ file <- paste0(data.directory,"/class",ci,"_initialization.csv");
                                                                 as.data.frame(read.csv(file, header = T, sep = ","))})
    learned_theta.c <- t(read.csv(paste0(data.directory,"/probability_classes.csv"), header = T, sep = "," ))
    
    
    ll_train_train <- likelihood.a(train, num.classes, max.stages, learned_theta.c, 
                           learned_MarkovModel, learned_initialization, min.stages) # train dataset on the learned model
    ll_real_train <- likelihood.a(train, num.classes, max.stages, original_theta.c, 
                          original_MarkovModel, original_initialization, min.stages) # train dataset on the original model
    ll_train_test <- likelihood.a(test, num.classes, max.stages, learned_theta.c, 
                          learned_MarkovModel, learned_initialization, min.stages) # test dataset on the learned model
    
    
    data_n <- data.frame('Seed'=seed,
                         'N.Sec'= n,
                         'N.classes'=num.classes,
                         'N.stages'=max.stages,
                         'Minimum stage' = min.stages,
                         'll_train_train'= ll_train_train,
                         'll_train_test'= ll_train_test,
                         'll_real_train'= as.numeric(ll_real_train),
                         'll_real_test'= as.numeric(ll_real_test)
    )
    data <- rbind(data,data_n)
  }
} 

data <- data[-1,]
data.mean <- data.frame('N.Sec'= unique(data$N.Sec),
                        'N.classes'=rep(num.classes,length(unique(data$N.Sec))),
                        'N.stages'= rep(max.stages,length(unique(data$N.Sec))),
                        'Minimum stage'= rep(min.stages,length(unique(data$N.Sec))),
                        'll_train_train'= rep(0,length(unique(data$N.Sec))),
                        'll_train_test'= rep(0,length(unique(data$N.Sec))),
                        'll_real_train'= rep(0,length(unique(data$N.Sec))),
                        'll_real_test'= rep(0,length(unique(data$N.Sec)))
)

data.mean$ll_train_train <- by(data$ll_train_train, data$N.Sec, function(x) mean(x))
data.mean$ll_train_test <- by(data$ll_train_test, data$N.Sec, function(x) mean(x))
data.mean$ll_real_train <- by(data$ll_real_train, data$N.Sec, function(x) mean(x))
data.mean$ll_real_test <- by(data$ll_real_test, data$N.Sec, function(x) mean(x))


data1 <- as.data.frame(matrix(rep(0,(nrow(data.mean)* 3)*4), ncol=3))
colnames(data1) <- c("Number.Sec","Type","Value")
data1$Number.Sec<- rep(data.mean$N.Sec,4)
data1$Data <- c(rep("train",nrow(data.mean)),rep("test",nrow(data.mean)),rep("train",nrow(data.mean)),rep("test",nrow(data.mean)))
data1$Model <- c(rep("train",nrow(data.mean)),rep("train",nrow(data.mean)),rep("real",nrow(data.mean)),rep("real",nrow(data.mean)))

data1$Value[1:nrow(data.mean)] <- data.mean$ll_train_train
data1$Value[(nrow(data.mean)+1): (2*nrow(data.mean))] <- data.mean$ll_train_test
data1$Value[(2*nrow(data.mean)+1): (3*nrow(data.mean))] <- data.mean$ll_real_train
data1$Value[(3*nrow(data.mean)+1): (4*nrow(data.mean))] <- data.mean$ll_real_test

# Plot
data1$int <- paste(data1$Data, data1$Model, sep=".")
(plot <-ggplot(data1, aes(x=Number.Sec, y=Value, colour = int, linetype=int, shape = int, size = int)) + theme_bw()+
    geom_line(lwd=0.7) + geom_point()+
    scale_shape_manual(name = "", values = c(16,16,17,17), labels =c("Generalization (generative model)","Generalization (learned model)",
                                                                     "Fitting (generative model)","Fitting (learned model)") )+
    scale_linetype_manual(name="",values= c( 'dotted','solid','dotted','solid'), labels =c("Generalization (generative model)",
                                                                                           "Generalization (learned model)","Fitting (generative model)",
                                                                                           "Fitting (learned model)"))+
    scale_colour_manual(name="", values = c("dodgerblue3", 'dodgerblue3',"darkorange3", "darkorange3"), labels =c("Generalization (generative model)",
                                                                                                                  "Generalization (learned model)",
                                                                                                                  "Fitting (generative model)",
                                                                                                                  "Fitting (learned model)"))+
    scale_size_manual(name = "",values = c(2.1, 2.1, 2.3, 2.3),  labels =c("Generalization (generative model)","Generalization (learned model)",
                                                                           "Fitting (generative model)","Fitting (learned model)") ) + 
    ylab("Average log likelihood") + xlab("Number of sequences")  +
    theme(legend.box.background = element_rect(size=0.5), legend.title = element_blank(), legend.margin =margin(2,2,2,2), 
          legend.position = c(0.75, 0.2))+
    theme(axis.text.x = element_text(colour = "black", size = 11), axis.text.y = element_text(colour = "black", size = 11),
          axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) +
    theme(legend.text=element_text(size=rel(1.3)))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5))
  
)


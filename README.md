# A Probabilistic Generative Model for Learning the Progression Patterns of Treatments

[![Made-with-R](https://img.shields.io/badge/Made%20with-R-blue)](/AMRC_Python) [![Ask Me Anything !](https://img.shields.io/badge/Ask%20me-anything-1abc9c.svg)](#support-and-author)
 
This repository contains code for the paper A Probabilistic Generative Model for Learning the Progression Patterns of Treatments. The proposed generative model discovers patterns of progression in sequences of medical actions of variable length. In particular, the model considers that a sequence of actions has an associated hierarchical structure of latent variables that both classifies the sequences based on their evolution over time, and segments the sequences in different progression stages. An Expectation-Maximization algorithm is used to recover the original model underlying the data.

## Implementation of the method

[![Made-with-R](https://img.shields.io/badge/Made%20with-R-blue)](/GenerativeModel)

GenerativeModel folder contains R scripts required to execute the method:

* main.R is the main file. In this file we can modify the values of the parameters to generate the sequences and the model, such as the values of actions, the number of classes and the number of stages. In addition, such script randonmly generates sequences of actions and learns a new model with the EM algorithm. Afterwards, the error between the learned model and the original one is computed to show that the proposed method recovers the real model unerlying the data.
* generateModel.R generates the original model and samples sequences of actions from it.
* initialization.R initializes the parameters of the model
* EMalgorithm.R efficiently performs the Expectation-Maximization algorithm, where the parameters are updated in each iteration with a dynamic programming based method.
* smoothing.R contains functions to carry out the smoothing and normalization of the parameters of a model.
* Loglikelihood.R computes the log-likelihood of a model
* MSE.R computes the Mean Squared Error of a trained model by comparing with the original model which the sequence were sampled from.


## Data

We use synthetic datasets generated with the proposed model, which can be find in

## Evaluation

We display in this reposity the evaluation perfomed with synthetic data on the paper to demonstrate that we are able to recover the generative model. Evaluation folder includes more details of the dataset generation, commands to execute the method, and results. [palabra](Evaluation\mainEvaluation.R)






# Contact
Onintze Zaballa Larumbe
onintzezaballa@gmail.com

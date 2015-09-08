################################################################################
###
###     Estimation  -- Nu-CLEAR
###     Created: 5/8/2015
###     Last Modified: 5/11/2015
###     Author: B C Smith
###
###     Estimates the initial model for Nuclear latency measure.
###
###     
################################################################################

rm(list=ls())
library(rstan)
library(foreign)

setwd("~/Google Drive/Research/Nuclear Latency")

load("Data/X_v2.RData")



# Create the data object to feed to Rstan 

N <- nrow(X)
K <- ncol(X)

stan.data <- list(N = N,                 # Number of rows in X
                  X = X,                  # Response data matrix
                  K = K)                  # Number of questions
                  
nuclear.stan <- '
data{
int<lower = 1> N;       // number of rows in X
int<lower = 1> K;       // number of columns in X
int X[N,K];             // create X, response data matrix
}

parameters{
real<lower=0> Beta[K];        // set the activity discrimination parameter
vector[N] Nu;          // set the latent nuclear capacity parameter
vector[K] alpha;      //  set the item difficulty parameter
}

model{
Beta ~ lognormal(0,1);
Nu ~ normal(0,3);
alpha ~ normal(0,3);

for (i in 1:N){
      for (j in 1:K){
            X[i,j] ~ bernoulli_logit(Beta[j]*(Nu[i] - alpha[j]));
      }
   }
}
'

iter <- 10

fit <- stan(model_code = nuclear.stan,
            data = stan.data,
            iter = iter,
            chains = 1,
            warmup = floor(iter/3))







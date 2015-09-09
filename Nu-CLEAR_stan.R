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

load("Data/X_v4.RData")


# Create the data object to feed to Rstan 

N <- nrow(X)
K <- max(X$activity)
countries = nrow(first)
stan.data <- list(N = N,                                # Number of rows in X
                  X = as.matrix(cbind(X$response,
                                      X$activity,
                                      X$nuindex)),     # Response data matrix
                  K = K,                                # Number of questions
                  first = first,                        # First appearances
                  indices = indices,                    # Other appearances
                  countries = countries,                # Num of first appearances
                  number = nrow(indices)                # Num of other appearances
                  )

nuclear.stan <- '
data{
int<lower = 1> N;       // number of rows in X
int<lower = 1> K;       // number of columns in X
int X[N,3];             // create X, response data matrix
int<lower = 1> countries; // number of countries
int first[countries,2];  // first responses
int<lower = 1> number;   // Indices of additional observations
int indices[number, 2];  // Indices of other observations
}

parameters{
real<lower=0> Beta[K];        // set the activity discrimination parameter
vector[N] Nu;          // set the latent nuclear capacity parameter
vector[K] alpha;      //  set the item difficulty parameter
}

model{
Beta ~ lognormal(0,1);
alpha ~ normal(0,3);

// Set prior for first appearances
for (i in 1:countries){
  Nu[first[i,2]] ~ normal(0,3);
}

for (i in 1:number){
Nu[i] ~ normal(Nu[indices[i,2]], 1);
}

for (i in 1:N){
            X[i,1] ~ bernoulli_logit(Beta[X[i,2]]*(Nu[X[i,3]] - alpha[X[i,2]]));
}
}
'

iter <- 10

fit <- stan(model_code = nuclear.stan,
            data = stan.data,
            iter = iter,
            chains = 1,
            warmup = floor(iter/3))







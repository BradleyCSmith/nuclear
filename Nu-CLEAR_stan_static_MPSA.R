################################################################################
###
###     Estimation  -- Nu-CLEAR -- static version
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

load("Data/X_v6.RData")


# Create the data object to feed to Rstan 

N <- nrow(X)
K <- max(X$activity)
cyears <- max(X$nuindex)
countries = nrow(first)
stan.data <- list(N = N,                                # Number of rows in X
                  X = as.matrix(cbind(X$response,
                                      X$activity,
                                      X$nuindex)),     # Response data matrix
                  K = K,                                # Number of questions
                  cyears <- cyears             # Number of country years
                  )

nuclear.stan <- '
data{
int<lower = 1> N;       // number of rows in X
int<lower = 1> K;       // number of columns in X
int X[N,3];             // create X, response data matrix
int<lower = 1> countries; // number of countries
int<lower = 1> cyears; // number of country-years
}

parameters{
real<lower = 0> Beta[K];        // set the activity discrimination parameter
vector[cyears] Nu;          // set the latent nuclear capacity parameter
vector[K] alpha;      //  set the item difficulty parameter
}

model{
Beta ~ lognormal(0,1);
alpha ~ normal(1,3);
Nu ~ normal(0,1);


for (i in 1:N){
            X[i,1] ~ bernoulli_logit(Beta[X[i,2]]*(Nu[X[i,3]]-alpha[X[i,2]]));
}
}
'

iter <- 100

fit <- stan(model_code = nuclear.stan,
            data = stan.data,
            iter = iter,
            chains = 1,
            warmup = floor(iter/3))


test <- extract(fit)
Beta <- as.matrix(colMeans(test$Beta))
alpha <- as.matrix(colMeans(test$alpha))
Nu <- as.matrix(colMeans(test$Nu))

################################################################################
###
###     Post-Processing  -- Nu-CLEAR
###     Created: 5/14/2015
###     Last Modified: 5/14/2015
###     Author: B C Smith
###
###     Extracts posterior density from stanfit object and calculates
###     credible intervals, etc. 
###
###     
################################################################################

rm(list = ls())
require(rstan)

setwd("~/Google Drive/Research/Nuclear Latency")
#setwd("C:/Users/bsmith/Desktop/Nuclear Latency")

# load in the results from simulation
load("Data/dynamic_10k.RData")


# Extract the posteriors
posterior <- extract(fit)

# Remove everything except for posterior, ID object to conserve memory
rm(list = setdiff(ls(), c("ID", "posterior")))

# Create an object that binds the posterior means of the Nu variable to the
# corresponding ID variables

Nu_posterior <- as.data.frame(posterior[[2]])
Nu_means <- colMeans(Nu_posterior)
Nu_quant_80 <- apply(Nu_posterior, 
                     2,
                     quantile,
                     probs = c(.1, .9))
Nu_quant_80 <- as.data.frame(cbind(Nu_quant_80[1,], Nu_quant_80[2,]))
names(Nu_quant_80) <- c("Nu_lower_80", "Nu_upper_80")

Nu_quant_95 <- apply(Nu_posterior,
                     2,
                     quantile,
                     probs = c(0.025, 0.975))

Nu_quant_95 <- as.data.frame(cbind(Nu_quant_95[1,], Nu_quant_95[2,]))
names(Nu_quant_95) <- c("Nu_lower_95", "Nu_upper_95")

# Create a data matrix
N_CLEAR <- cbind(ID[,c("ccode", 
                       "year",
                       "ID")],
                 Nu_means,
                 Nu_quant_80,
                 Nu_quant_95)

# Now we'll gather the alpha (difficulty) and beta (discrimination) parameters similarly
Beta_posterior <- as.data.frame(posterior[[1]])
alpha_posterior <- as.data.frame(posterior[[3]])

Beta_means <- colMeans(Beta_posterior)
alpha_means <- colMeans(alpha_posterior)

Beta_quant_80 <- apply(Beta_posterior,
                       2,
                       quantile,
                       probs = c(.1, .9))
Beta_quant_95 <- apply(Beta_posterior,
                       2,
                       quantile,
                       probs = c(0.025, 0.975))

alpha_quant_80 <- apply(alpha_posterior,
                        2,
                        quantile,
                        probs = c(0.1, 0.9))

alpha_quant_95 <- apply(alpha_posterior,
                        2,
                        quantile,
                        probs = c(0.025, 0.975))

varnames <- c(names(ID)[4:length(names(ID))])

# Now make data frame objects for the discrimination and difficulty parameters
Betas <- rbind(Beta_means, Beta_quant_80, Beta_quant_95)

Betas <- as.data.frame(Betas)
names(Betas) <- varnames

alphas <- rbind(alpha_means, alpha_quant_80, alpha_quant_95)
alphas <- as.data.frame(alphas)
names(alphas) <- varnames

# Clear everything out of the workspace and save the data
rm(list = setdiff(ls(), c("N_CLEAR", "alphas", "Betas")))

#save.image("~/Google Drive/Research/Nuclear Latency/Data/N_CLEAR_v3.RData")
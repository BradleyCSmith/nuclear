################################################################################
###
###     Data  -- Nu-CLEAR
###     Created: 5/8/2015
###     Last Modified: 5/8/2015
###     Author: B C Smith
###
###     This creates the data for MCMC estimation of measurement model.
###
###     OBJECTS CREATED:
###     X -- Data frame where an observation is a country-year-response
###     ID -- Data frame where an observation is a country-year
###     first -- Data frame with indices of first appearances of countries
###              in ID, for indexing purposes in Stan code.
###     indices -- Indices for non-first appearances for dynamic priors.
###
###
################################################################################





rm(list=ls())
require('foreign')
setwd("~/Google Drive/Research/Nuclear Latency")

#Load Jo & Gartzke data
data <- read.dta('Data/jo_gartzke_0207_nuccap_0906.dta')

#Load extended variables
ext <- read.csv('Data/newnukedata.csv')

#Load Jo & Gartzke replication data
rep <- read.dta('Data/jo_gartzke_0207_replicate_0906.dta')

#Load Brown & Kaplow, and subset to program variable
BK <- read.csv('Data/BrownKaplow_JCR2013_Replication.csv')
BK <- BK[,c("ccode",
            "year",
            "hasprog")]
BK <- BK[BK$year > 1992,]

data$ID <- data$ccode*10000+data$year
ext$ID <- ext$ccode*10000+ext$year
rep $ ID <- rep$ccode*10000 + rep$year 

Y <- rep[,c("ID",
            "nuke_df",
            "nuk_a_p")]

X <- merge(data,
           ext,
           by = intersect(names(data), names(ext)))

X <- merge(X, 
           Y, 
           by = intersect(names(X), names(Y)),
           all.x = TRUE)

X <- merge(X,
           BK,
           by = intersect(names(X), names(BK)),
           all.x = TRUE)

# Replace nuk_a_p NAs with hasprog
X$nuk_a_p[X$year>1992] <- X$hasprog[X$year>1992]

# Now we'll code a variable that is a 1 if any of the enrichment variables is 1,
# and equal to 0 otherwise.
X$enrichmentsum <- as.numeric(apply(X[,c("gas_diff",
                                         "gas_cent",
                                         "emis",
                                         "chem_and_ion_ex",
                                         "aero_iso_separation",
                                         "laser",
                                         "thermal_diff")],
                                    1,
                                    sum))

X$enrichment <- as.numeric(as.logical(X$enrichmentsum > 0))

ID <- X[,c("ccode",
           "year",
           "ID",
           "ura_i",
           "metal_i",
           "chemi_i",
           "nitric_i",
           "elect_i",
           "nuke_i",
           "explo_i",
           "test",
           "reprocess",
           "enrichment",
           "submarines"
           "nuk_a_p",
           "nuke_df")]


# Locate states with weapons in 1992
prog <- ID[ID$year == 1992,]
prog <- prog$ccode[prog$nuke_df ==1]

ID$nuke_df[ID$year>1992 & (ID$ccode %in% prog)] <- 1
ID$nuke_df[is.na(ID$nuke_df)] <- 0

# Now add programs for 2001
prog <- c(2,
          365,
          200,
          220,
          710,
          666,
          750,
          770,
          645,
          731)

ID$nuk_a_p[ID$year==2001] <- 0

ID$nuk_a_p[ID$year == 2001 & ID$ccode %in% prog] <- 1
# Eliminate all missing observations then make our data matrix for use, called X
#ID <- ID[complete.cases(ID),]

# Remove everything from the workspace except for our ID object
# Next we'll change the dimensions of this object and create objects to 
# feed to our Rstan script
rm(list = setdiff(ls(), c("ID")))


# Our first task is to change the shape of this object. The following loop 
# goes through the data, generating an observation for each state-year-activity.

# First, generate an object with activities
activities <- c("ura_i",
                "metal_i",
                "chemi_i",
                "nitric_i",
                "elect_i",
                "nuke_i",
                "explo_i",
                "test",
                "reprocess",
                "enrichment",
                "submarines"
                "nuk_a_p",
                "nuke_df")

# Create an empty bin that will serve as our data frame
bin <- rep(NA, 6)
X <- as.data.frame(t(bin))
names(X) <- c("ccode",
              "year",
              "ID",
              "activity",
              "activity_name",
              "response")

# This loop reshapes the data
for(i in 1:nrow(ID)){
  # First, pull out a country-year observation
  cyear <- ID[i,]
  # Next, loop over activities, generating an observation for each non-
  # missing one. Then, add that to our data matrix.
  for(j in 1:length(activities)){
    if(is.na(cyear[,activities[j]])==FALSE){
      obs <- as.data.frame(t(c(cyear$ccode,
                               cyear$year,
                               cyear$ID,
                               j,
                               activities[j],
                               as.numeric(as.logical(cyear[,activities[j]])))))
      names(obs) <- names(X)
      X <- rbind(X,obs)
    }
  } # Loop over j
  # Progress update
  if (i/50==floor(i/50)){
    cat(i, " out of ", nrow(ID), " country years generated.","\n")}
} # Loop over i

# Remove the top and bottom rows of X, which are NA-ridden
X <- X[-1,]
X <- X[-nrow(X),]

# Recode our variables, and save them as numeric
X$ccode <- as.numeric(X$ccode)
X$year <- as.numeric(X$year)
X$ID <- as.numeric(X$ID)
X$response <- as.numeric(X$response)
X$activity <- as.numeric(X$activity)

# Resort our data so that it is ordered by ccode, then year
X <- X[order(X$ccode, X$year),]

# Now generate a data frame that has unique country years, sorted
ID <- ID[order(ID$ccode, ID$year),]

# Now generate a number of countries by 2 object where first column corresponds
# to ccodes, and second corresponds to their first appearance in ID.
first <- as.data.frame(cbind(unique(ID$ccode), NA))
names(first) <- c("ccode", "first_index")

# Now populate second column of first with the first appearances of each country
for (i in 1:length(first$ccode)){
  first[i,2] <- min(which(ID$ccode == first$ccode[i]))
}

# With this, generate an object that corresponds to the indices of the non-first
# appearances in the data.
for(i in 1:(length(first$ccode)-1)){
  if(i == 1){
    indices <- c((first$first_index[i]+1) : (first$first_index[i+1]-1))
  }
  if(i>1){
    indices <- c(indices, c((first$first_index[i]+1) : (first$first_index[i+1]-1)))
  }
}

indices <- c(indices, c((max(indices)+2) : nrow(ID)))


# Now, take this object and bind it to an object which is just itself, 
# but with elementwise subtracting minus 1.
# With this, the first column is the index of the posterior, and the second is
# the index of the prior
indices <- as.data.frame(cbind(indices, indices-1))
names(indices) <- c("posterior", "prior")


# Next, we need to go into our long data matrix, X, and add an index variable so
# that stan knows which parameter to grab for each observation.
IDindex <- as.data.frame(cbind(ID$ID,c(1:length(ID$ID))))
names(IDindex) <- c("ID", "nuindex")

X$nuindex <- NA


# Now loop through X to generate a variable associating each response with an
# index in the vector of nu
for (i in 1:nrow(IDindex)){
  X$nuindex[X$ID == IDindex$ID[i]] <- IDindex$nuindex[i]
}

#Remove the first row of X, which is all NA (Not necessary now for some reason)
#X <- X[-1,]

# Now, clear the workspace and save
rm(list = setdiff(ls(), c("X",
                          "first",
                          "ID",
                          "indices")))

save.image("~/Google Drive/Research/Nuclear Latency/Data/X_v7.RData")


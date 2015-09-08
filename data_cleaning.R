################################################################################
###
###     Data  -- Nu-CLEAR
###     Created: 5/8/2015
###     Last Modified: 5/8/2015
###     Author: B C Smith
###
###     This creates the data for MCMC estimation of measurement model.
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
           "civpower",
           "test",
           "enr",
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
ID <- ID[complete.cases(ID),]

X <- ID[,c("ura_i",
                "metal_i",
                "chemi_i",
                "nitric_i",
                "elect_i",
                "nuke_i",
                "explo_i",
                "civpower",
                "test",
                "enr",
                "nuk_a_p",
                "nuke_df")]


# Now clear the workspace and prepare the data for estimation
rm(list=setdiff(ls(), c("X", "ID")))
save.image("~/Google Drive/Research/Nuclear Latency/Data/X_v3.RData")


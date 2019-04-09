#############################################
# Infer the number of "missing" individuals #
# in the population based on data from      #
# multiple locations. Based on method       #
# described in Durban et al. (2005) Marine  #
# Mammal Science 21(1): 80-92.              #
#                                           #
# REQUIRES: Numbers to be placed in 15 of   #
# the 16 cells of the table.                #
#############################################

infermissing <- function(histcounts) {

    #-------------------------------------------#
    # Load the necessary packages and libraries #
    #-------------------------------------------#
    library(rstan)
    options(mc.cores = parallel::detectCores())
    source("plotPost.R")
  
  
    #-------------------------#
    # subsample posteriors    #
    #-------------------------#
    nSamp = 100
    sampRows = floor(seq(from = 1, to = length(histcounts[, 1]), length = nSamp))
    histSamp = histcounts[sampRows, ]
  
    
    #--------------------------------------#
    # Concatenate sighting history counts  #
    #--------------------------------------#
    histList = c(histSamp[, "cell1234"], histSamp[, "cell123"], histSamp[, "cell124"], histSamp[, "cell134"], histSamp[, "cell234"], histSamp[, "cell12"], histSamp[, "cell13"], histSamp[, "cell14"], histSamp[, "cell23"], histSamp[, "cell24"], histSamp[, "cell34"], histSamp[, "cell1"], histSamp[, "cell2"], histSamp[, "cell3"], histSamp[, "cell4"])
  
  
    #--------------------------------------#
    # Create sighting indexes and add      #
    #--------------------------------------#
    sightingHistories = matrix(0, nrow = nSamp * 15, ncol = 4)
    
    for (i in 1:nSamp) {
      sightingHistories[i, ] = c(2, 2, 2, 2)
    }
    
    for (i in 1:nSamp) {
      sightingHistories[nSamp + i, ] = c(2, 2, 2, 1)
    }
    
    for (i in 1:nSamp) {
      sightingHistories[(2 * nSamp) + i, ] = c(2, 2, 1, 2)
    }
    
    for (i in 1:nSamp) {
      sightingHistories[(3 * nSamp) + i, ] = c(2, 1, 2, 2)
    }
    
    for (i in 1:nSamp) {
      sightingHistories[(4 * nSamp) + i, ] = c(1, 2, 2, 2)
    }
    
    for (i in 1:nSamp) {
      sightingHistories[(5 * nSamp) + i, ] = c(2, 2, 1, 1)
    }
    
    for (i in 1:nSamp) {
      sightingHistories[(6 * nSamp) + i, ] = c(2, 1, 2, 1)
    }
    
    for (i in 1:nSamp) {
      sightingHistories[(7 * nSamp) + i, ] = c(2, 1, 1, 2)
    }
    
    for (i in 1:nSamp) {
      sightingHistories[(8 * nSamp) + i, ] = c(1, 2, 2, 1)
    }
    
    for (i in 1:nSamp) {
      sightingHistories[(9 * nSamp) + i, ] = c(1, 2, 1, 2)
    }
    
    for (i in 1:nSamp) {
      sightingHistories[(10 * nSamp) + i, ] = c(1, 1, 2, 2)
    }
    
    for (i in 1:nSamp) {
      sightingHistories[(11 * nSamp) + i, ] = c(2, 1, 1, 1)
    }
    
    for (i in 1:nSamp) {
      sightingHistories[(12 * nSamp) + i, ] = c(1, 2, 1, 1)
    }
    
    for (i in 1:nSamp) {
      sightingHistories[(13 * nSamp) + i, ] = c(1, 1, 2, 1)
    }
    
    for (i in 1:nSamp) {
      sightingHistories[(14 * nSamp) + i, ] = c(1, 1, 1, 2)
    }
    
    dataSummary = data.frame(round(histList), sightingHistories)
    
    #----------------------------#
    # Organize the data for Stan #
    #----------------------------#
    y = as.integer(dataSummary[, 1])
    y = ifelse(y > 0, y, 0)     # Ensure no values are < 0
    loc1 = as.integer(dataSummary[, 2])
    loc2 = as.integer(dataSummary[, 3])
    loc3 = as.integer(dataSummary[, 4])
    loc4 = as.integer(dataSummary[, 5])
    N = length(y)
    nLevels = 2
    xMissing = as.integer(c(1, 1, 1, 1))
    Nx = 1
    
    
    #---------------------------------#
    # Prepare data as a list for Stan #
    #---------------------------------#
    dataList = list(
      N = N,
      nLevels = nLevels,
      y = y,
      loc1 = loc1,
      loc2 = loc2,
      loc3 = loc3,
      loc4 = loc4,
      xMissing = xMissing
    )
    
    #-----------------------------#
    # Define the model and write  #
    # a string for Stan           #
    #-----------------------------#
    modelstring = "
      data {
        int<lower=0> N;          // Sample size
        int<lower=0> nLevels;    // Number of categories for each locus
        int<lower=0> y[N];       // Vector of counts
        int loc1[N];             // Vector of data for location 1
        int loc2[N];             // Vector of data for location 2
        int loc3[N];             // Vector of data for location 3
        int loc4[N];             // Vector of data for location 4
        int xMissing[4];         // Vector of data for individuals not seen in any location
      }
    
      parameters {
        real b0;
        real b1[nLevels];        // Coefficient for effects of location 1
        real b2[nLevels];        // Coefficient for effects of location 2
        real b3[nLevels];        // Coefficient for effects of location 3
        real b4[nLevels];        // Coefficient for effects of location 4
        real phi;                // The overdispersion parameter
      }

      transformed parameters {
        vector[N] mu;

        for (i in 1:N) {
          mu[i] = exp(b0 + b1[loc1[i]] + b2[loc2[i]] + b3[loc3[i]] + b4[loc4[i]]);
        }
      }    

      model {
        // Likelihood
        y ~ neg_binomial_2(mu, phi);

        // Priors
        b0 ~ cauchy(0, 10);
        
        for (j in 1:nLevels) {
          b1[j] ~ cauchy(0, 2.5);
          b2[j] ~ cauchy(0, 2.5);
          b3[j] ~ cauchy(0, 2.5);
          b4[j] ~ cauchy(0, 2.5);
        }
      }

      generated quantities {
        int y_pred;
        real mu_pred;
        
        mu_pred = exp(b0 + b1[xMissing[1]] + b2[xMissing[2]] + b3[xMissing[3]] + b4[xMissing[4]]);

        y_pred = neg_binomial_2_rng(mu_pred, phi);
      }
    "
    writeLines(modelstring, con = "model.stan")
    
    
    #--------------------------#
    #     run STAN             #
    #--------------------------#
    stanFit <- stan(file = "model.stan", 
                    data = dataList, 
                    pars = c("b0", "b1", "b2", "b3", "b4", "phi", "y_pred"),
                    warmup = 2000,
                    iter = 12000, 
                    chains = 3)
    
    #--------------------------#
    # Assess MCMC Performance  #
    #--------------------------#
    print(stanFit)
    print(plot(stanFit, par = c("b0", "b1", "b2", "b3", "b4")))

    
    #---------------------------------#
    #  EXTRACT THE PREDICTIONS       #
    #---------------------------------#

    #---Convert chains to a matrix for easier handling---#
    mcmcChains = as.data.frame(stanFit)
    
    yPred = mcmcChains[, "y_pred"]
    par(mfrow = c(1, 1))
    histInfo = plotPost(yPred, xlab = "Missing Individuals", showMode = TRUE)

    #------------------------------#
    # Return posterior probability #
    #------------------------------#
    return(yPred)
}


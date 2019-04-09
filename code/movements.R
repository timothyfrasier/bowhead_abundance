###########################################
#            movements.R                  #
#                                         #
# This code estimates the movement rates  #
# between all pairs of sampled locations  #
# based on a binomial model and STAN. It  #
# also calculates the average movement    #
# rate across all pairs, which is taken   #
# as the estimate for the movement rates  #
# to and from the unsampled location(s).  #
###########################################
movements = function(nSites, filename, nIter = 10000, nChains = 4, print.diag = TRUE, plot.diag = TRUE) {

  #--- Load Libraries ---#
  library(rstan)

  #--------------------------#
  # Read Data Into R         #
  #--------------------------#
  mData = read.table(filename, header = TRUE, sep = ",")


  #-------------------#
  # Organize Data     #
  #-------------------#
  nPairs = (nSites * (nSites - 1)) / 2
  yList = list()

  #--- Create binary migration vectors ---#
  for (i in 1:nPairs) {
    yTemp = rep(0, times = mData[i, 3])
    counter = sample(1:length(yTemp), mData[i, 2], replace=FALSE)
  
    for (j in 1:length(counter)) {
      yTemp[counter] = 1
    }
  
    yList[[i]] = yTemp
  }

  #--- Concatenate Lists into One, and Add Labels ---#
  y = yList[[1]]

  for (i in 2:nPairs) {
    y = c(y, yList[[i]])
  }

  N = length(y)

  yLabels = NULL

  counter = 1
  for (i in 1:(nSites - 1)) {
    for (j in (i+1):nSites) {
      yLabels[counter] = paste(i, "-", j, sep = "")
      counter = counter + 1
    }
  }

  yLabelList = rep(yLabels[1], times = length(yList[[1]]))

  for (i in 2:nPairs) {
    yLabelList = c(yLabelList, (rep(yLabels[i], times = length(yList[[i]]))))
  }

  yLabelList = as.factor(yLabelList)


  #---------------------------------------------#
  # Specify data as a list to transport to STAN #
  #---------------------------------------------#
  x = as.numeric(yLabelList)
  xNames = levels(yLabelList)
  nMigrations = length(unique(yLabelList))

  dataList = list(
    N = N,
    y = y,
    x = x,
    nMigrations = nMigrations
  )


  #---------------------------------#
  # Define the model and parameters #
  #---------------------------------# 
  modelstring = "
    data { 
      int N;
      int nMigrations;
      int y[N];
      int x[N];
    }

    parameters {
      real<lower=0, upper=1> theta[nMigrations];
      real<lower=0, upper=1> mean_groups;
      real<lower=0> sd_groups;
    }

    model {
      // Hyperpriors
      mean_groups ~ normal(0.5, 0.5);
      sd_groups ~ cauchy(0, 1);

      // Priors
      for (j in 1:nMigrations) {
        theta[j] ~ normal(mean_groups, sd_groups);
      }

      // Likelihood
      for (i in 1:N) {
        y[i] ~ bernoulli(theta[x[i]]);
      }
    }
  "
  writeLines(modelstring, con = "model.stan")

  #---------------#
  # Run the model #
  #---------------#
  stanFit <- stan(file = "model.stan", 
                  data = dataList, 
                  iter = nIter,
                  chains = nChains
  )


  #-------------------------#
  # Check MCMC Performance  #
  #-------------------------#
  if (print.diag == TRUE) {
    print(stanFit)
  }
  
  if (plot.diag == TRUE) {
    print(stan_trace(stanFit, pars = "theta", inc_warmup = TRUE))
  }  


  #------------------------#
  #    Plot Parameters     #
  #------------------------#
  if (plot.diag == TRUE) {
    print(stan_plot(stanFit, par = "theta"))
  }
    

  #-----------------------#
  #    Extract Data       #
  #-----------------------#
  movement.rates = extract(stanFit)

  return(movement.rates)
}

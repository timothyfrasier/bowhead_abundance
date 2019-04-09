#######################################
#         abundance_Single.R          #
#                                     #
# This function estimates abundance   #
# from a matrix of sighting histories #
# where each row represents an        #
# individual, and the columns have 1s #
# or 0s depending on whether or not   #
# that individual was "captured" in   #
# that time period. It is called      #
# "closed" because here we are        #
# assuming a closed population.       #
#######################################

popabhist = as.matrix(pophist)
ncols = NCOL(popabhist)

  #-----------------------------#
  # Augment the data set by     #
  # ????? potential individuals #
  # (see p. 141-144)            #
  # !! This number should       #
  # change as needed!!!!        #
  #-----------------------------#
  aug = 30000 
  yaug = rbind(popabhist, array(0, dim=c(aug, ncols)))


  #--- Load Libraries ---#
  library(rstan)
  options(mc.cores = parallel::detectCores())

  #--- Load data into R ---#
  y = yaug
  M = nrow(y)
  T = ncol(y)
  
  
  #---------------------------------------------#
  # Specify data as a list to transport to STAN #
  #---------------------------------------------#
  dataList = list(
    M = M,
    T = T,
    y = y
  )
  
  
  #---------------------------------#
  # Define the model and parameters #
  #---------------------------------# 
  modelstring = "
    data { 
      int<lower=0> M;                 // Size of augmented data set
      int<lower=0> T;                 // Number of sampling occasions
      int<lower=0, upper=1> y[M, T];  // Capture-history matrix
    }  

    transformed data {
      int<lower=0> s[M];  // Totals in each row
      int<lower=0> C;     // Size of observed data set

      C = 0;
      for (i in 1:M) {
        s[i] = sum(y[i]);
        if (s[i] > 0) {
          C = C + 1;
        }
      }
    }

    parameters {
      real<lower=0, upper=1> omega;  // Inclusion probability
      real<lower=0, upper=1> p;      // Detection probability
    }
  
    model {
      // Priors
      omega ~ uniform(0, 1);
      p ~ uniform(0, 1);
  
      // Likelihood
      for (i in 1:M) {
        if (s[i] > 0) {
          target += bernoulli_lpmf(1 | omega) + binomial_lpmf(s[i] | T, p);
        } else {
          target += log_sum_exp(bernoulli_lpmf(1 | omega) + binomial_lpmf(0 | T, p), bernoulli_lpmf(0 | omega));
        }
      }
    }

    generated quantities {
      // prob present given never detected
      real omega_nd = (omega * (1 - p)^T) / (omega * (1 - p)^T + (1 - omega));
      int<lower=C, upper=M> N = C + binomial_rng(M - C, omega_nd);
    }
  "
  writeLines(modelstring, con = "model.stan")
  
  #---------------#
  # Run the model #
  #---------------#
  stanFit = stan(file = "model.stan", 
                  data = dataList, 
                  iter = 10000,
                  warmup = 10000/4,
                  chains = 4
  )
  
  #-------------------------#
  # Check MCMC Performance  #
  #-------------------------#
  if (print.diag = TRUE) {
    print(stanFit)
  }
  
  stan_trace(stanFit, inc_warmup = TRUE)
  if (plot.diag = TRUE) {
    stan_trace(stanFit, pars = "omega", inc_warmup = TRUE)
    stan_trace(stanFit, pars = "p", inc_warmup = TRUE)
    stan_trace(stanFit, pars = "N", inc_warmup = TRUE)
    stan_ac(stanFit, pars = "omega", inc_warmup = FALSE)
    stan_ac(stanFit, pars = "p", inc_warmup = FALSE)
    stan_ac(stanFit, pars = "N", inc_warmup = FALSE)
  }
  
  
  #------------------------#
  #    Plot Parameters     #
  #------------------------#
  if (plot.post = TRUE) {
    stan_plot(stanFit, par = "omega")
    stan_plot(stanFit, par = "p")
    stan_plot(stanFit, par = "N")
  }
  
  results = extract(stanFit)
  write.csv(results, "../results/li-5Y_TotalPop.csv", quote = FALSE, row.names = FALSE)  
  
  source("plotPost.R")
  par(mfrow = c(1, 3))
  histInfo = plotPost(results$omega, xlab = "Omega", showMode = TRUE)
  histInfo = plotPost(results$p, xlab = "p", showMode = TRUE)
  histInfo = plotPost(results$N, xlab = "N", showMode = TRUE)
  
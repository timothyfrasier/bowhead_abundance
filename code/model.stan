
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
    

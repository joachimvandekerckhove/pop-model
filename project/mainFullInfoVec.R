# This code belongs "to Partially observable predictor models for identifying
# cognitive markers" by Zita Oravecz, Martin Sliwinski, Sharon H. Kim, Lindy
# Williams, Mindy J. Katz, and Joachim Vandekerckhove.
# 
# The data used in this project are not publicly available and are not included
# in this repository.  The code can be read but not run.
# 
# Copyright (C) 2024 Oravecz and Vandekerckhove
# 
# This program comes with ABSOLUTELY NO WARRANTY.
# This is free software, and you are welcome to redistribute it
# under the conditions described in the GNU GPL v3.0 license.
# Visit https://www.gnu.org/licenses/gpl-3.0.en.html for details.
# 
# To read the analysis code, access mainFullInfo.R in this directory.

# This is a vectorized version of the same code.  It is often faster but
# sometimes not as numerically stable.  This code was not used for any of the 
# analyses in the paper and is only provided because it looks more like the 
# equations in the paper.

# Load packages
library(rstan)
library(parallel)
library(lubridate)

# Set seed for reproducibility
set.seed(123)

# Set up parallel computing in Stan
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# clearing memory
rm(list=ls())

# Load data and make into list for Stan
load("EAS316.Rdata")

data <-list(N = totalN, 
            P = nrPersons,
            covar = L,
            nL = nrCovL,
            RT = eas_subset_stan$day_mean_rt/1000,
            personIndex = eas_subset_stan$PID, 
            dayIndex    = eas_subset_stan$studyday_rc,
            nrAssess    = eas_subset_stan$no_asss_day,
            MCIStatus   = MCIStatus)

######### Stan code
modelString = "

data {
	int<lower=1> N;      // total number of datapoints
	int<lower=1> nL;     // total number of covariates for logistic
	int<lower=1> P;      // total number of persons
	vector[N] RT;        // all RT data
	vector[N] dayIndex;  // all assessments nested in persons
	vector[N] nrAssess;  // how many assessments that day
	int personIndex[N];  // which person?
	matrix[P, nL] covar; // logistic covariate matrix 
	int<lower=0,upper=1> MCIStatus[P]; // MCI status
}

parameters {
	matrix<lower=0>[P, 4] Lambda; // Matrix containing [a, g, r, sd] in columns

	real muA; // population mean asymptote
	real muG; // population mean gain
	real muR; // population mean learning
	real muS; // population mean error standard deviation

	real<lower=0> sdA;
	real<lower=0> sdG;
	real<lower=0> sdR;
	real<lower=0> sdS;

	real       beta_0;
	vector[nL] beta_covar;
	vector[4]  beta_lambda;
}

model {
	// Likelihood
	RT ~ normal(Lambda[personIndex, 1] +  
			Lambda[personIndex, 2] .* exp(-Lambda[personIndex, 3].*dayIndex), 
			Lambda[personIndex, 4] ./ sqrt(nrAssess));

	// Predicting MCI status
	MCIStatus ~ bernoulli_logit(beta_0 + Lambda * beta_lambda + covar * beta_covar);

	// Structural priors
	beta_0       ~ normal(0,1);
	beta_covar   ~ normal(0, 1);
	beta_lambda  ~ normal(0, 1);

	// Priors for population-level parameters
	muA ~ normal(0,10);
	muG ~ normal(0,10);
	muR ~ normal(0,10);
	muS ~ normal(0, 1);

	sdA ~ normal(0,10);
	sdG ~ normal(0,10);
	sdR ~ normal(0,10);
	sdS ~ normal(0, 1);

	Lambda[, 1] ~ normal(muA, sdA);
	Lambda[, 2] ~ normal(muG, sdG);
	Lambda[, 3] ~ normal(muR, sdR);
	Lambda[, 4] ~ normal(muS, sdS);
}

" 

stanFile = "StanModelBurst1.vec.stan"
writeLines(modelString, con=stanFile)

expmodel <- stan(stanFile,
                 data = data,
                 pars = c("beta_0", "beta_lambda", "beta_covar",
                          "muA", "muG", "muR", "muS",
                          "sdA", "sdG", "sdR", "sdS" ),
                 control = list(adapt_delta   = 0.99,
                                max_treedepth = 14),
                 chains  =    4 ,
                 warmup  = 2500 ,
                 iter    = 5000 )

save.image(sprintf("./SS_%s.Rdata", Sys.Date()))

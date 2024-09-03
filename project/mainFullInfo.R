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

# Load packages
library(Rmisc)
library(rstan)
library(parallel)
library(coda)

# Set seed for reproducibility
set.seed(123)

# Set up parallel computing in Stan
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# clearing memory
rm(list=ls())

# Load data and make into list for Stan
load("../EAS316.Rdata")

data <-list(N = totalN, 
            P = nrPersons,
            X = X,
            L = L,
            nX = nrCov,
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
	int<lower=1> nX;     // total number of person-predictors
	int<lower=1> nL;     // total number of covariates for logistic
	int<lower=1> P;      // total number of persons
	vector[N] RT;        // all RT data
	vector[N] dayIndex;  // all assessments nested in persons
	int nrAssess[N];     // how many assessments that day
	int personIndex[N];  // which person?
	matrix[P, nL] L;     // logistic covariate matrix 
	int<lower=0,upper=1> MCIStatus[P]; // MCI status
}

parameters {
	// person-specific double negative exponential parameters
	vector<lower=0>[P] rC;    // continuous rate or learning
	vector<lower=0>[P] gC;    // gain between no practice and asymptote
	vector<lower=0>[P] a;     // asymptotic response time
	vector<lower=0>[P] sdE;   // IIV

	real muA;     // population mean asymptote
	real muSdE;   // population mean error standard deviation
	real mugC;    // population mean gain
	real murC;    // population mean learning

	real<lower=0> aSd;
	real<lower=0> sdESd;
	real<lower=0> gCSd;
	real<lower=0> rCSd;

	real MCIIntercept;
	real coeffMCIAsymptote;
	real coeffMCIGain;
	real coeffMCILearning;
	real coeffMCIIIV;
	real coeffMCIAge;
	real coeffMCIGender;
	real coeffMCIEduc;
	real coeffMCIEthnic_Black;
	real coeffMCIEthnic_Hisp;
}

model {
	// Likelihood
	for (n in 1:N) {
		RT[n] ~ normal(a[personIndex[n]] +  
			       gC[personIndex[n]]*exp(-rC[personIndex[n]]*dayIndex[n]), 
			       sdE[personIndex[n]]/sqrt(nrAssess[n]));
	}

	// Predicting MCI status
	for (n in 1:P) {
		MCIStatus[n] ~ bernoulli_logit(MCIIntercept + 
		                    coeffMCIAsymptote    * a[n] + 
		                    coeffMCIGain         * gC[n] + 
		                    coeffMCILearning     * rC[n] +
		                    coeffMCIIIV          * sdE[n] +
		                    coeffMCIAge          * L[n,1] +
		                    coeffMCIGender       * L[n,2] +
		                    coeffMCIEduc         * L[n,3] +
		                    coeffMCIEthnic_Black * L[n,4] +
		                    coeffMCIEthnic_Hisp  * L[n,5]);
	}

	// Priors
	MCIIntercept         ~ normal(0,1);
	coeffMCIAsymptote    ~ normal(0,1);
	coeffMCIGain         ~ normal(0,1);
	coeffMCILearning     ~ normal(0,1);
	coeffMCIIIV          ~ normal(0,1);
	coeffMCIAge          ~ normal(0,1);
	coeffMCIGender       ~ normal(0,1);
	coeffMCIEduc         ~ normal(0,1);
	coeffMCIEthnic_Black ~ normal(0,1);
	coeffMCIEthnic_Hisp  ~ normal(0,1);


	rC  ~ normal(murC , rCSd );
	gC  ~ normal(mugC , gCSd );
	a   ~ normal(muA  , aSd  );
	sdE ~ normal(muSdE, sdESd);

	murC  ~ normal(0,10);
	mugC  ~ normal(0,10);
	muA   ~ normal(0,10);
	muSdE ~ normal(0, 1);

	rCSd  ~ normal(0,10);
	gCSd  ~ normal(0,10);
	aSd   ~ normal(0,10);
	sdESd ~ normal(0, 1);
}

" 

stanFile = "StanModelBurst1.fi.stan"
writeLines(modelString, con=stanFile)

expmodel <- stan(stanFile,
                 data = data,
                 pars = c("a"  , "sdE"  , "gC"  , "rC"  ,
                          "muA", "muSdE", "mugC", "murC",
                          "aSd", "sdESd", "gCSd", "rCSd", 
                          "coeffMCIAsymptote", "coeffMCIIIV",
                          "coeffMCIGain"     , "coeffMCILearning",
                          "coeffMCIAge"      , "coeffMCIGender",
                          "coeffMCIEduc"     , "coeffMCIEthnic_Black",
                          "MCIIntercept"     , "coeffMCIEthnic_Hisp"),
                 control = list(adapt_delta   = 0.99,
                                max_treedepth = 14),
                 chains  =    4 ,
                 warmup  = 2500 ,
                 iter    = 5000 )

save.image(sprintf("./Rdata/SS_%s.Rdata", Sys.Date()))
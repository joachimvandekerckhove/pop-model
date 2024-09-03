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

# Split data into stratified folds
stratify_folds <- function(criterion, num_folds, seed = 42) {
  num_cases <- length(criterion)
  cases <- 1:num_cases
  folds <- vector("list", num_folds)
  
  if (!is.logical(criterion)) {
    stop('I think you did it wrong.  Criterion must be a logical.')
  }
  
  set.seed(seed)  # Set seed for reproducibility
  
  trues = cases[criterion]
  shuffled_cases <- sample(trues)
  nc = length(trues)
  
  fold_sizes <- rep(floor(nc / num_folds), num_folds)
  if ((nc %% num_folds) > 0) {
    fold_sizes[1:(nc %% num_folds)] <- 
      fold_sizes[1:(nc %% num_folds)] + 1
  }
  
  current_index <- 1
  for (i in 1:num_folds) {
    fold_indices <- current_index:(current_index + fold_sizes[i] - 1)
    folds[[i]] <- shuffled_cases[fold_indices]
    current_index <- current_index + fold_sizes[i]
  }
  
  falses = cases[!criterion]
  shuffled_cases <- sample(falses)
  nc = length(falses)
  
  fold_sizes <- rep(floor(nc / num_folds), num_folds)
  if ((nc %% num_folds) > 0) {
    fold_sizes[1:(nc %% num_folds)] <- 
      fold_sizes[1:(nc %% num_folds)] + 1
  }
  
  current_index <- 1
  for (i in 1:num_folds) {
    fold_indices <- current_index:(current_index + fold_sizes[i] - 1)
    folds[[i]] <- sort(c(folds[[i]], shuffled_cases[fold_indices]))
    current_index <- current_index + fold_sizes[i]
  }
  
  if (!(length(unlist(folds)) == length(criterion)) |
      !(length(unique(unlist(folds))) == length(criterion))) {
    stop('I think I did it wrong but I don\'t know what happened :(')
  }
  
  indices_to_logical <- function(indices, length) {
    logical_vector <- rep(TRUE, length)
    logical_vector[indices] <- FALSE
    return(logical_vector)
  }
  
  # Apply the function to each element in the list
  logical_folds <- lapply(folds, indices_to_logical, length = num_cases)
  
  if (!(sum(!unlist(logical_folds)) == length(criterion))) {
    stop('I think I did it wrong at the end :(')
  }
  
  return(logical_folds)
}

nFolds <- 10
folds <- stratify_folds(criterion = (data$MCIStatus == 1),
                        num_folds = nFolds)

# Get fold index from command line
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) {
	stop("One argument must be supplied (fold).", call. = FALSE)
}
fold <- as.numeric(args[1])

cat("\n\n########################\n")
cat(" Starting fold", fold, "of", nFolds, "\n")
cat("########################\n")

data$train <- folds[[fold]]
data$K <- sum(data$train, na.rm = TRUE)

######### Stan code
modelString = "

data {
	int<lower=1> N;      // total number of datapoints
	int<lower=1> nX;     // total number of person-predictors
	int<lower=1> nL;     // total number of covariates for logistic
	int<lower=1> P;      // total number of persons
	int<lower=1> K;      // total number of persons in training set
	vector[N] RT;        // all RT data
	vector[N] dayIndex;  // all assessments nested in persons
	int nrAssess[N];     // how many assessments that day
	int personIndex[N];  // which person?
	matrix[P, nL] L;     // logistic covariate matrix 
	int<lower=0,upper=1> MCIStatus[P]; // MCI status
	int<lower=0,upper=1> train[P];  // Train or test?
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

transformed parameters {
    vector[P] pi;

    for (p in 1:P) {
        pi[p] = MCIIntercept + 
                    coeffMCIAsymptote    * a  [p] +
                		coeffMCIGain         * gC [p] + 
                		coeffMCILearning     * rC [p] +
                		coeffMCIIIV          * sdE[p] +
                		coeffMCIAge          * L[p,1] +
                		coeffMCIGender       * L[p,2] +
                		coeffMCIEduc         * L[p,3] +
                		coeffMCIEthnic_Black * L[p,4] +
                		coeffMCIEthnic_Hisp  * L[p,5];
    }
}

model {
	// Likelihood
	for (n in 1:N) {
		RT[n] ~ normal(a[personIndex[n]] +  
			       gC[personIndex[n]]*exp(-rC[personIndex[n]]*dayIndex[n]), 
			       sdE[personIndex[n]]/sqrt(nrAssess[n]));
	}

	// Predicting MCI status
	for (p in 1:P) {
            if (train[p]) {
		MCIStatus[p] ~ bernoulli_logit(pi[p]);
	    }
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

stanFile = "StanModelBurst1.cv.stan"
writeLines(modelString, con=stanFile)

expmodel<- stan(stanFile,
                data = data,
                pars = c("a"  , "sdE"  , "gC"  , "rC"  ,
                         "muA", "muSdE", "mugC", "murC",
                         "aSd", "sdESd", "gCSd", "rCSd", 
                         "coeffMCIAsymptote", "coeffMCIIIV",
                         "coeffMCIGain"     , "coeffMCILearning",
                         "coeffMCIAge"      , "coeffMCIGender",
                         "coeffMCIEduc"     , "coeffMCIEthnic_Black",
                         "MCIIntercept"     , "coeffMCIEthnic_Hisp" , "pi"),
                control = list(adapt_delta   = 0.99,
                               max_treedepth = 14),
                chains  =    3 ,
                warmup  = 4000 ,
                iter    = 6000 )

save.image(sprintf("./Rdata/SS_fold%i_%s.Rdata", fold, Sys.Date()))
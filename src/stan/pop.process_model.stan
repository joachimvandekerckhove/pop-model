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

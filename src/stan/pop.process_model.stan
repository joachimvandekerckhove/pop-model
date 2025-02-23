data {
	int<lower=1> N;      // total number of datapoints
	int<lower=1> P;      // total number of persons
	int<lower=1> K;      // total number of persons in training set
	int<lower=1> nL;     // total number of covariates for logistic
	matrix[P, nL] L;     // logistic covariate matrix 
	vector[N] RT;        // all RT data
	vector[N] dayIndex;  // all assessments nested in persons
	int nrAssess[N];     // how many assessments that day
	int personIndex[N];  // which person?
	int<lower=0,upper=1> MCIStatus[P];  // MCI status
	int<lower=0,upper=1> train[P];      // Train or test?
}

parameters {
	// Asymptote	
	real mu_asymptote;
	real<lower=0> sd_asymptote;
	vector<lower=0>[P] asymptote;

	// IIV
	real mu_iiv;
	real<lower=0> sd_iiv;
	vector<lower=0>[P] iiv;

	// Gain
	real mu_gain;
	real<lower=0> sd_gain;
	vector<lower=0>[P] gain;
	
	// Learning
	real mu_learning;
	real<lower=0> sd_learning;
	vector<lower=0>[P] learning;

	// Regression coefficients
	real intercept_latent;
	real coeff_asymptote;
	real coeff_gain;
	real coeff_learning;
	real coeff_iiv;
	real coeff_age;
	real coeff_gender;
	real coeff_educ;
	real coeff_black;
	real coeff_hisp;
}

transformed parameters {
    vector[P] pi;

    for (p in 1:P) {
        pi[p] = intercept_latent + 
                    coeff_asymptote * asymptote[p] +
                	coeff_gain      * gain[p] + 
                	coeff_learning  * learning[p] +
                	coeff_iiv       * iiv[p] +
                	coeff_age       * L[p,1] +
                	coeff_gender    * L[p,2] +
                	coeff_educ      * L[p,3] +
                	coeff_black     * L[p,4] +
                	coeff_hisp      * L[p,5];
    }
}

model {
	// Projection to RT data
	for (n in 1:N) {
		RT[n] ~ normal(asymptote[personIndex[n]] +  
			       gain[personIndex[n]]*exp(-learning[personIndex[n]]*dayIndex[n]), 
			       iiv[personIndex[n]]/sqrt(nrAssess[n]));
	}

	// Projection to MCI status data
	for (p in 1:P) {
        if (train[p]) {
		    MCIStatus[p] ~ bernoulli_logit(pi[p]);
	    }
	}

	// Hierarchical level
	learning  ~ normal(mu_learning, sd_learning);
	gain      ~ normal(mu_gain, sd_gain);
	asymptote ~ normal(mu_asymptote, sd_asymptote);
	iiv       ~ normal(mu_iiv, sd_iiv);

	// Priors
	intercept_latent ~ normal(0,10);
	coeff_asymptote  ~ normal(0,10);
	coeff_gain       ~ normal(0,10);
	coeff_learning   ~ normal(0,10);
	coeff_iiv        ~ normal(0,10);
	coeff_age        ~ normal(0,10);
	coeff_gender     ~ normal(0,10);
	coeff_educ       ~ normal(0,10);
	coeff_black      ~ normal(0,10);
	coeff_hisp       ~ normal(0,10);

	mu_learning  ~ normal(0,10);
	mu_asymptote ~ normal(0,10);
	mu_gain      ~ normal(0,10);
	mu_iiv       ~ normal(0,10);

	sd_learning  ~ normal(0,10);
	sd_asymptote ~ normal(0,10);
	sd_gain      ~ normal(0,10);
	sd_iiv       ~ normal(0,10);
} 

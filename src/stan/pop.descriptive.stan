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
	// Mean response time
    real mu_mrt;
    real<lower=0> sd_mrt;
    vector<lower=0>[P] mrt;

	// Standard deviation of response time
    real mu_srt;
    real<lower=0> sd_srt;
    vector<lower=0>[P] srt;

    // Regression coefficients
    real intercept_latent;
    real coeff_mrt;
    real coeff_srt;
    real coeff_age;
    real coeff_gender;
    real coeff_educ;
    real coeff_black;
    real coeff_hisp;
}

transformed parameters {
    vector[P] pi;

    for (p in 1:P) {
        pi[p] = intercept_latent
                + coeff_mrt    * mrt[p]
                + coeff_srt    * srt[p]
                + coeff_age    * L[p,1]
                + coeff_gender * L[p,2]
                + coeff_educ   * L[p,3]
                + coeff_black  * L[p,4]
                + coeff_hisp   * L[p,5];
    }
}

model {
	// Projection to RT data
    for (n in 1:N) {
        RT[n] ~ normal(mrt[personIndex[n]],
                   srt[personIndex[n]]);
    }

	// Projection to MCI status data
    for (p in 1:P) {
        if (train[p]) {
            MCIStatus[p] ~ bernoulli_logit(pi[p]);
        }
    }

    // Hierarchical level
    mrt ~ normal(mu_mrt, sd_mrt);
    srt ~ normal(mu_srt, sd_srt);

    // Priors
    intercept_latent ~ normal(0,10);
    coeff_mrt        ~ normal(0,10);
    coeff_srt        ~ normal(0,10);
    coeff_age        ~ normal(0,10);
    coeff_gender     ~ normal(0,10);
    coeff_educ       ~ normal(0,10);
    coeff_black      ~ normal(0,10);
    coeff_hisp       ~ normal(0,10);

    mu_mrt ~ normal(0,10);
    mu_srt ~ normal(0,10);

    sd_mrt ~ normal(0,10);
    sd_srt ~ normal(0,10);
}

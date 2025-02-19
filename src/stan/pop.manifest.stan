data {
    int<lower=1> P;      // total number of persons
    int<lower=1> K;      // total number of persons in training set
    int<lower=1> nL;     // total number of covariates for logistic
    matrix[P, nL] L;     // logistic covariate matrix

    int<lower=0,upper=1> MCIStatus[P];  // MCI status
    int<lower=0,upper=1> train[P];  // Train or test?
}

parameters {
    real intercept_latent;
    real coeff_age;
    real coeff_gender;
    real coeff_educ;
    real coeff_black;
    real coeff_hisp;
}

transformed parameters {
    vector[P] pi;

    for (p in 1:P) {
        pi[p] = 0
                + intercept_latent
                + coeff_age    * L[p,1]
                + coeff_gender * L[p,2]
                + coeff_educ   * L[p,3]
                + coeff_black  * L[p,4]
                + coeff_hisp   * L[p,5]
                ;
    }
}

model {
    for (p in 1:P) {
        if (train[p]) {
            MCIStatus[p] ~ bernoulli_logit(pi[p]);
        }
    }

    // Priors
    intercept_latent   ~ normal(0,1);

    coeff_age    ~ normal(0,1);
    coeff_gender ~ normal(0,1);
    coeff_educ   ~ normal(0,1);
    coeff_black  ~ normal(0,1);
    coeff_hisp   ~ normal(0,1);
}

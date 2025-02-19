data {
    int<lower=1> N;      // total number of datapoints
    int<lower=1> P;      // total number of persons
    int<lower=1> K;      // total number of persons in training set
    int<lower=1> nX;     // total number of person-predictors
    int<lower=1> nL;     // total number of covariates for logistic
    matrix[P, nX] X;     // person predictor matrix
    matrix[P, nL] L;     // logistic covariate matrix
    int nrAssess[N];     // how many assessments that day
    int personIndex[N];  // which person?
    vector[N] RT;        // all RT data
    vector[N] dayIndex;  // all assessments nested in persons
    int<lower=0,upper=1> MCIStatus[P];  // MCI status
    int<lower=0,upper=1> train[P];  // Train or test?
}

parameters {
    vector<lower=0>[P] ssa;  // mean response time
    vector<lower=0>[P] sss;  // std response time

    vector[nX] beta_ssa;  // regression coefficients for mean response time
    vector[nX] beta_sss;  // regression coefficients for std response time

    real<lower=0> sd_ssa;
    real<lower=0> sd_sss;

    real coeff_ssa;
    real coeff_sss;

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
                + coeff_ssa * ssa[p]
                + coeff_sss * sss[p]
                + coeff_age    * L[p,1]
                + coeff_gender * L[p,2]
                + coeff_educ   * L[p,3]
                + coeff_black  * L[p,4]
                + coeff_hisp   * L[p,5]
                ;
    }
}

model {
    for (n in 1:N) {
        RT[n] ~ normal(ssa[personIndex[n]],
                            sss[personIndex[n]]
                            );
    }

    for (p in 1:P) {
        if (train[p]) {
            MCIStatus[p] ~ bernoulli_logit(pi[p]);
        }
    }

    // Priors
    intercept_latent   ~ normal(0,1);

    coeff_ssa ~ normal(0,1);
    coeff_sss ~ normal(0,1);

    coeff_age    ~ normal(0,1);
    coeff_gender ~ normal(0,1);
    coeff_educ   ~ normal(0,1);
    coeff_black  ~ normal(0,1);
    coeff_hisp   ~ normal(0,1);

    ssa ~ normal(X*beta_ssa, sd_ssa);
    sss ~ normal(X*beta_sss, sd_sss);

    beta_ssa ~ normal(0,10);
    beta_sss ~ normal(0,10);

    sd_ssa   ~ normal(0,10);
    sd_sss   ~ normal(0,10);
}

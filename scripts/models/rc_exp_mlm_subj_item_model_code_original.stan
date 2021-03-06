// This Stan code was generated with the R package 'brms'. 
// We recommend generating the data with the 'make_standata' function. 
functions { 

real dprime_fn(real time, real disabled, real asymptote_unconstrained, real invrate_unconstrained, real intercept)
{
  if (disabled == 0.0 && time >= intercept) {
  real asymptote; 
  real invrate; 
  
  asymptote = 10 * inv_logit(asymptote_unconstrained);
  invrate = exp(invrate_unconstrained) + 10;
  //asymptote = asymptote_unconstrained;
  //invrate = invrate_unconstrained;
  
  return asymptote * (1 - exp(-1/invrate * (time - intercept)) );
  }
  return 0;
}

real criterion_fn(real time, real init_unconstrained, real asymptote_unconstrained, real invrate_unconstrained, real intercept)
{
  real criterion; 
  real init; 
  
  init = 10 * (inv_logit(init_unconstrained) - .5);
  //init = init_unconstrained;
  criterion = init;
  
  if (time >= intercept) {
  real asymptote; 
  real invrate; 
  
  asymptote = 10 * (inv_logit(asymptote_unconstrained) - .5) - init;
  invrate = exp(invrate_unconstrained) + 10;
  //asymptote = asymptote_unconstrained - init;
  //invrate = invrate_unconstrained;
  
  criterion = criterion + (asymptote-init) * (1 - exp(-1/invrate * (time - intercept)) );
  }
  return criterion;
}
} 
data { 
  int<lower=1> N;  // total number of observations 
  int Y[N];  // response variable 
  // data for non-linear effects of asymptote
  int<lower=1> K_asymptote;  // number of population-level effects 
  matrix[N, K_asymptote] X_asymptote;  //  population-level design matrix 
  // data for group-specific effects of item 
  int<lower=1> J_asymptote_1[N]; 
  int<lower=1> N_asymptote_1; 
  int<lower=1> K_asymptote_1; 
  vector[N] Z_asymptote_1_1;  
  vector[N] Z_asymptote_1_2;  
  vector[N] Z_asymptote_1_3;  
  int<lower=1> NC_asymptote_1; 
  // data for group-specific effects of subject 
  int<lower=1> J_asymptote_2[N]; 
  int<lower=1> N_asymptote_2; 
  int<lower=1> K_asymptote_2; 
  vector[N] Z_asymptote_2_1;  
  vector[N] Z_asymptote_2_2;  
  vector[N] Z_asymptote_2_3;  
  int<lower=1> NC_asymptote_2; 
  // data for non-linear effects of invrate
  int<lower=1> K_invrate;  // number of population-level effects 
  matrix[N, K_invrate] X_invrate;  //  population-level design matrix 
  // data for group-specific effects of item 
  int<lower=1> J_invrate_1[N]; 
  int<lower=1> N_invrate_1; 
  int<lower=1> K_invrate_1; 
  vector[N] Z_invrate_1_1;  
  vector[N] Z_invrate_1_2;  
  vector[N] Z_invrate_1_3;  
  int<lower=1> NC_invrate_1; 
  // data for group-specific effects of subject 
  int<lower=1> J_invrate_2[N]; 
  int<lower=1> N_invrate_2; 
  int<lower=1> K_invrate_2; 
  vector[N] Z_invrate_2_1;  
  vector[N] Z_invrate_2_2;  
  vector[N] Z_invrate_2_3;  
  int<lower=1> NC_invrate_2; 
  // data for non-linear effects of intercept
  int<lower=1> K_intercept;  // number of population-level effects 
  matrix[N, K_intercept] X_intercept;  //  population-level design matrix 
  // data for group-specific effects of item 
  int<lower=1> J_intercept_1[N]; 
  int<lower=1> N_intercept_1; 
  int<lower=1> K_intercept_1; 
  vector[N] Z_intercept_1_1;  
  vector[N] Z_intercept_1_2;  
  vector[N] Z_intercept_1_3;  
  int<lower=1> NC_intercept_1; 
  // data for group-specific effects of subject 
  int<lower=1> J_intercept_2[N]; 
  int<lower=1> N_intercept_2; 
  int<lower=1> K_intercept_2; 
  vector[N] Z_intercept_2_1;  
  vector[N] Z_intercept_2_2;  
  vector[N] Z_intercept_2_3;  
  int<lower=1> NC_intercept_2; 
  // data for non-linear effects of critasymptote
  int<lower=1> K_critasymptote;  // number of population-level effects 
  matrix[N, K_critasymptote] X_critasymptote;  //  population-level design matrix 
  // data for group-specific effects of item 
  int<lower=1> J_critasymptote_1[N]; 
  int<lower=1> N_critasymptote_1; 
  int<lower=1> K_critasymptote_1; 
  vector[N] Z_critasymptote_1; 
  // data for group-specific effects of subject 
  int<lower=1> J_critasymptote_2[N]; 
  int<lower=1> N_critasymptote_2; 
  int<lower=1> K_critasymptote_2; 
  vector[N] Z_critasymptote_2; 
  // data for non-linear effects of critintercept
  int<lower=1> K_critintercept;  // number of population-level effects 
  matrix[N, K_critintercept] X_critintercept;  //  population-level design matrix 
  // data for group-specific effects of item 
  int<lower=1> J_critintercept_1[N]; 
  int<lower=1> N_critintercept_1; 
  int<lower=1> K_critintercept_1; 
  vector[N] Z_critintercept_1; 
  // data for group-specific effects of subject 
  int<lower=1> J_critintercept_2[N]; 
  int<lower=1> N_critintercept_2; 
  int<lower=1> K_critintercept_2; 
  vector[N] Z_critintercept_2; 
  // data for non-linear effects of critinvrate
  int<lower=1> K_critinvrate;  // number of population-level effects 
  matrix[N, K_critinvrate] X_critinvrate;  //  population-level design matrix 
  // data for group-specific effects of item 
  int<lower=1> J_critinvrate_1[N]; 
  int<lower=1> N_critinvrate_1; 
  int<lower=1> K_critinvrate_1; 
  vector[N] Z_critinvrate_1; 
  // data for group-specific effects of subject 
  int<lower=1> J_critinvrate_2[N]; 
  int<lower=1> N_critinvrate_2; 
  int<lower=1> K_critinvrate_2; 
  vector[N] Z_critinvrate_2; 
  // data for non-linear effects of critinit
  int<lower=1> K_critinit;  // number of population-level effects 
  matrix[N, K_critinit] X_critinit;  //  population-level design matrix 
  // data for group-specific effects of item 
  int<lower=1> J_critinit_1[N]; 
  int<lower=1> N_critinit_1; 
  int<lower=1> K_critinit_1; 
  vector[N] Z_critinit_1; 
  // data for group-specific effects of subject 
  int<lower=1> J_critinit_2[N]; 
  int<lower=1> N_critinit_2; 
  int<lower=1> K_critinit_2; 
  vector[N] Z_critinit_2; 
  int<lower=1> KC;  // number of covariates 
  matrix[N, KC] C;  // covariate matrix 
  int prior_only;  // should the likelihood be ignored? 
} 
transformed data { 
} 
parameters { 
  // non-linear effects of asymptote
  vector[K_asymptote] b_asymptote;  // population-level effects 
  vector<lower=0>[K_asymptote_1] sd_asymptote_1;  // group-specific standard deviations 
  matrix[K_asymptote_1, N_asymptote_1] z_asymptote_1;  // unscaled group-specific effects 
  // cholesky factor of correlation matrix 
  cholesky_factor_corr[K_asymptote_1] L_asymptote_1; 
  vector<lower=0>[K_asymptote_2] sd_asymptote_2;  // group-specific standard deviations 
  matrix[K_asymptote_2, N_asymptote_2] z_asymptote_2;  // unscaled group-specific effects 
  // cholesky factor of correlation matrix 
  cholesky_factor_corr[K_asymptote_2] L_asymptote_2; 
  // non-linear effects of invrate
  vector[K_invrate] b_invrate;  // population-level effects 
  vector<lower=0>[K_invrate_1] sd_invrate_1;  // group-specific standard deviations 
  matrix[K_invrate_1, N_invrate_1] z_invrate_1;  // unscaled group-specific effects 
  // cholesky factor of correlation matrix 
  cholesky_factor_corr[K_invrate_1] L_invrate_1; 
  vector<lower=0>[K_invrate_2] sd_invrate_2;  // group-specific standard deviations 
  matrix[K_invrate_2, N_invrate_2] z_invrate_2;  // unscaled group-specific effects 
  // cholesky factor of correlation matrix 
  cholesky_factor_corr[K_invrate_2] L_invrate_2; 
  // non-linear effects of intercept
  vector<lower=10,upper=5000>[K_intercept] b_intercept;  // population-level effects 
  vector<lower=0>[K_intercept_1] sd_intercept_1;  // group-specific standard deviations 
  matrix[K_intercept_1, N_intercept_1] z_intercept_1;  // unscaled group-specific effects 
  // cholesky factor of correlation matrix 
  cholesky_factor_corr[K_intercept_1] L_intercept_1; 
  vector<lower=0>[K_intercept_2] sd_intercept_2;  // group-specific standard deviations 
  matrix[K_intercept_2, N_intercept_2] z_intercept_2;  // unscaled group-specific effects 
  // cholesky factor of correlation matrix 
  cholesky_factor_corr[K_intercept_2] L_intercept_2; 
  // non-linear effects of critasymptote
  vector[K_critasymptote] b_critasymptote;  // population-level effects 
  real<lower=0> sd_critasymptote_1;  // group-specific standard deviation 
  vector[N_critasymptote_1] z_critasymptote_1;  // unscaled group-specific effects 
  real<lower=0> sd_critasymptote_2;  // group-specific standard deviation 
  vector[N_critasymptote_2] z_critasymptote_2;  // unscaled group-specific effects 
  // non-linear effects of critintercept
  vector<lower=10,upper=5000>[K_critintercept] b_critintercept;  // population-level effects 
  real<lower=0> sd_critintercept_1;  // group-specific standard deviation 
  vector[N_critintercept_1] z_critintercept_1;  // unscaled group-specific effects 
  real<lower=0> sd_critintercept_2;  // group-specific standard deviation 
  vector[N_critintercept_2] z_critintercept_2;  // unscaled group-specific effects 
  // non-linear effects of critinvrate
  vector[K_critinvrate] b_critinvrate;  // population-level effects 
  real<lower=0> sd_critinvrate_1;  // group-specific standard deviation 
  vector[N_critinvrate_1] z_critinvrate_1;  // unscaled group-specific effects 
  real<lower=0> sd_critinvrate_2;  // group-specific standard deviation 
  vector[N_critinvrate_2] z_critinvrate_2;  // unscaled group-specific effects 
  // non-linear effects of critinit
  vector[K_critinit] b_critinit;  // population-level effects 
  real<lower=0> sd_critinit_1;  // group-specific standard deviation 
  vector[N_critinit_1] z_critinit_1;  // unscaled group-specific effects 
  real<lower=0> sd_critinit_2;  // group-specific standard deviation 
  vector[N_critinit_2] z_critinit_2;  // unscaled group-specific effects 
} 
transformed parameters { 
  vector[N] eta_asymptote; 
  // group-specific effects 
  matrix[N_asymptote_1, K_asymptote_1] r_asymptote_1; 
  vector[N_asymptote_1] r_asymptote_1_1; 
  vector[N_asymptote_1] r_asymptote_1_2; 
  vector[N_asymptote_1] r_asymptote_1_3; 
  // group-specific effects 
  matrix[N_asymptote_2, K_asymptote_2] r_asymptote_2; 
  vector[N_asymptote_2] r_asymptote_2_1; 
  vector[N_asymptote_2] r_asymptote_2_2; 
  vector[N_asymptote_2] r_asymptote_2_3; 
  vector[N] eta_invrate; 
  // group-specific effects 
  matrix[N_invrate_1, K_invrate_1] r_invrate_1; 
  vector[N_invrate_1] r_invrate_1_1; 
  vector[N_invrate_1] r_invrate_1_2; 
  vector[N_invrate_1] r_invrate_1_3; 
  // group-specific effects 
  matrix[N_invrate_2, K_invrate_2] r_invrate_2; 
  vector[N_invrate_2] r_invrate_2_1; 
  vector[N_invrate_2] r_invrate_2_2; 
  vector[N_invrate_2] r_invrate_2_3; 
  vector[N] eta_intercept; 
  // group-specific effects 
  matrix[N_intercept_1, K_intercept_1] r_intercept_1; 
  vector[N_intercept_1] r_intercept_1_1; 
  vector[N_intercept_1] r_intercept_1_2; 
  vector[N_intercept_1] r_intercept_1_3; 
  // group-specific effects 
  matrix[N_intercept_2, K_intercept_2] r_intercept_2; 
  vector[N_intercept_2] r_intercept_2_1; 
  vector[N_intercept_2] r_intercept_2_2; 
  vector[N_intercept_2] r_intercept_2_3; 
  vector[N] eta_critasymptote; 
  // group-specific effects 
  vector[N_critasymptote_1] r_critasymptote_1; 
  // group-specific effects 
  vector[N_critasymptote_2] r_critasymptote_2; 
  vector[N] eta_critintercept; 
  // group-specific effects 
  vector[N_critintercept_1] r_critintercept_1; 
  // group-specific effects 
  vector[N_critintercept_2] r_critintercept_2; 
  vector[N] eta_critinvrate; 
  // group-specific effects 
  vector[N_critinvrate_1] r_critinvrate_1; 
  // group-specific effects 
  vector[N_critinvrate_2] r_critinvrate_2; 
  vector[N] eta_critinit; 
  // group-specific effects 
  vector[N_critinit_1] r_critinit_1; 
  // group-specific effects 
  vector[N_critinit_2] r_critinit_2; 
  vector[N] eta; 
  eta_asymptote = X_asymptote * b_asymptote; 
  r_asymptote_1 = (diag_pre_multiply(sd_asymptote_1, L_asymptote_1) * z_asymptote_1)'; 
  r_asymptote_1_1 = r_asymptote_1[, 1];  
  r_asymptote_1_2 = r_asymptote_1[, 2];  
  r_asymptote_1_3 = r_asymptote_1[, 3];  
  r_asymptote_2 = (diag_pre_multiply(sd_asymptote_2, L_asymptote_2) * z_asymptote_2)'; 
  r_asymptote_2_1 = r_asymptote_2[, 1];  
  r_asymptote_2_2 = r_asymptote_2[, 2];  
  r_asymptote_2_3 = r_asymptote_2[, 3];  
  eta_invrate = X_invrate * b_invrate; 
  r_invrate_1 = (diag_pre_multiply(sd_invrate_1, L_invrate_1) * z_invrate_1)'; 
  r_invrate_1_1 = r_invrate_1[, 1];  
  r_invrate_1_2 = r_invrate_1[, 2];  
  r_invrate_1_3 = r_invrate_1[, 3];  
  r_invrate_2 = (diag_pre_multiply(sd_invrate_2, L_invrate_2) * z_invrate_2)'; 
  r_invrate_2_1 = r_invrate_2[, 1];  
  r_invrate_2_2 = r_invrate_2[, 2];  
  r_invrate_2_3 = r_invrate_2[, 3];  
  eta_intercept = X_intercept * b_intercept; 
  r_intercept_1 = (diag_pre_multiply(sd_intercept_1, L_intercept_1) * z_intercept_1)'; 
  r_intercept_1_1 = r_intercept_1[, 1];  
  r_intercept_1_2 = r_intercept_1[, 2];  
  r_intercept_1_3 = r_intercept_1[, 3];  
  r_intercept_2 = (diag_pre_multiply(sd_intercept_2, L_intercept_2) * z_intercept_2)'; 
  r_intercept_2_1 = r_intercept_2[, 1];  
  r_intercept_2_2 = r_intercept_2[, 2];  
  r_intercept_2_3 = r_intercept_2[, 3];  
  eta_critasymptote = X_critasymptote * b_critasymptote; 
  r_critasymptote_1 = sd_critasymptote_1 * (z_critasymptote_1);
  r_critasymptote_2 = sd_critasymptote_2 * (z_critasymptote_2);
  eta_critintercept = X_critintercept * b_critintercept; 
  r_critintercept_1 = sd_critintercept_1 * (z_critintercept_1);
  r_critintercept_2 = sd_critintercept_2 * (z_critintercept_2);
  eta_critinvrate = X_critinvrate * b_critinvrate; 
  r_critinvrate_1 = sd_critinvrate_1 * (z_critinvrate_1);
  r_critinvrate_2 = sd_critinvrate_2 * (z_critinvrate_2);
  eta_critinit = X_critinit * b_critinit; 
  r_critinit_1 = sd_critinit_1 * (z_critinit_1);
  r_critinit_2 = sd_critinit_2 * (z_critinit_2);
  for (n in 1:N) { 
    eta_asymptote[n] = eta_asymptote[n] + r_asymptote_1_1[J_asymptote_1[n]] * Z_asymptote_1_1[n] + r_asymptote_1_2[J_asymptote_1[n]] * Z_asymptote_1_2[n] + r_asymptote_1_3[J_asymptote_1[n]] * Z_asymptote_1_3[n] + r_asymptote_2_1[J_asymptote_2[n]] * Z_asymptote_2_1[n] + r_asymptote_2_2[J_asymptote_2[n]] * Z_asymptote_2_2[n] + r_asymptote_2_3[J_asymptote_2[n]] * Z_asymptote_2_3[n]; 
    eta_invrate[n] = eta_invrate[n] + r_invrate_1_1[J_invrate_1[n]] * Z_invrate_1_1[n] + r_invrate_1_2[J_invrate_1[n]] * Z_invrate_1_2[n] + r_invrate_1_3[J_invrate_1[n]] * Z_invrate_1_3[n] + r_invrate_2_1[J_invrate_2[n]] * Z_invrate_2_1[n] + r_invrate_2_2[J_invrate_2[n]] * Z_invrate_2_2[n] + r_invrate_2_3[J_invrate_2[n]] * Z_invrate_2_3[n]; 
    eta_intercept[n] = eta_intercept[n] + r_intercept_1_1[J_intercept_1[n]] * Z_intercept_1_1[n] + r_intercept_1_2[J_intercept_1[n]] * Z_intercept_1_2[n] + r_intercept_1_3[J_intercept_1[n]] * Z_intercept_1_3[n] + r_intercept_2_1[J_intercept_2[n]] * Z_intercept_2_1[n] + r_intercept_2_2[J_intercept_2[n]] * Z_intercept_2_2[n] + r_intercept_2_3[J_intercept_2[n]] * Z_intercept_2_3[n]; 
    eta_critasymptote[n] = eta_critasymptote[n] + r_critasymptote_1[J_critasymptote_1[n]] * Z_critasymptote_1[n] + r_critasymptote_2[J_critasymptote_2[n]] * Z_critasymptote_2[n]; 
    eta_critintercept[n] = eta_critintercept[n] + r_critintercept_1[J_critintercept_1[n]] * Z_critintercept_1[n] + r_critintercept_2[J_critintercept_2[n]] * Z_critintercept_2[n]; 
    eta_critinvrate[n] = eta_critinvrate[n] + r_critinvrate_1[J_critinvrate_1[n]] * Z_critinvrate_1[n] + r_critinvrate_2[J_critinvrate_2[n]] * Z_critinvrate_2[n]; 
    eta_critinit[n] = eta_critinit[n] + r_critinit_1[J_critinit_1[n]] * Z_critinit_1[n] + r_critinit_2[J_critinit_2[n]] * Z_critinit_2[n]; 
    // compute non-linear predictor 
    eta[n] = criterion_fn(C[n, 1] , eta_critinit[n] , eta_critasymptote[n] , eta_critinvrate[n] , eta_critintercept[n]) + dprime_fn(C[n, 1] , C[n, 2] , eta_asymptote[n] , eta_invrate[n] , eta_intercept[n]); 
  } 
} 
model { 
  // prior specifications 
  b_asymptote ~ normal(-1, 2); 
  sd_asymptote_1 ~ student_t(3, 0, 10); 
  L_asymptote_1 ~ lkj_corr_cholesky(1); 
  to_vector(z_asymptote_1) ~ normal(0, 1); 
  sd_asymptote_2 ~ student_t(3, 0, 10); 
  L_asymptote_2 ~ lkj_corr_cholesky(1); 
  to_vector(z_asymptote_2) ~ normal(0, 1); 
  b_invrate ~ normal(6.5, .5); 
  sd_invrate_1 ~ student_t(3, 0, 10); 
  L_invrate_1 ~ lkj_corr_cholesky(1); 
  to_vector(z_invrate_1) ~ normal(0, 1); 
  sd_invrate_2 ~ student_t(3, 0, 10); 
  L_invrate_2 ~ lkj_corr_cholesky(1); 
  to_vector(z_invrate_2) ~ normal(0, 1); 
  b_intercept ~ normal(500, 500); 
  sd_intercept_1 ~ student_t(3, 0, 10); 
  L_intercept_1 ~ lkj_corr_cholesky(1); 
  to_vector(z_intercept_1) ~ normal(0, 1); 
  sd_intercept_2 ~ student_t(3, 0, 10); 
  L_intercept_2 ~ lkj_corr_cholesky(1); 
  to_vector(z_intercept_2) ~ normal(0, 1); 
  b_critasymptote ~ normal(-1, 2); 
  sd_critasymptote_1 ~ student_t(3, 0, 10); 
  z_critasymptote_1 ~ normal(0, 1); 
  sd_critasymptote_2 ~ student_t(3, 0, 10); 
  z_critasymptote_2 ~ normal(0, 1); 
  b_critintercept ~ normal(500, 500); 
  sd_critintercept_1 ~ student_t(3, 0, 10); 
  z_critintercept_1 ~ normal(0, 1); 
  sd_critintercept_2 ~ student_t(3, 0, 10); 
  z_critintercept_2 ~ normal(0, 1); 
  b_critinvrate ~ normal(6.5, .5); 
  sd_critinvrate_1 ~ student_t(3, 0, 10); 
  z_critinvrate_1 ~ normal(0, 1); 
  sd_critinvrate_2 ~ student_t(3, 0, 10); 
  z_critinvrate_2 ~ normal(0, 1); 
  b_critinit ~ normal(-1, 2); 
  sd_critinit_1 ~ student_t(3, 0, 10); 
  z_critinit_1 ~ normal(0, 1); 
  sd_critinit_2 ~ student_t(3, 0, 10); 
  z_critinit_2 ~ normal(0, 1); 
  // likelihood contribution 
  if (!prior_only) { 
    Y ~ bernoulli_logit(eta); 
  } 
} 
generated quantities { 
  corr_matrix[K_asymptote_1] Cor_asymptote_1; 
  vector<lower=-1,upper=1>[NC_asymptote_1] cor_asymptote_1; 
  corr_matrix[K_asymptote_2] Cor_asymptote_2; 
  vector<lower=-1,upper=1>[NC_asymptote_2] cor_asymptote_2; 
  corr_matrix[K_invrate_1] Cor_invrate_1; 
  vector<lower=-1,upper=1>[NC_invrate_1] cor_invrate_1; 
  corr_matrix[K_invrate_2] Cor_invrate_2; 
  vector<lower=-1,upper=1>[NC_invrate_2] cor_invrate_2; 
  corr_matrix[K_intercept_1] Cor_intercept_1; 
  vector<lower=-1,upper=1>[NC_intercept_1] cor_intercept_1; 
  corr_matrix[K_intercept_2] Cor_intercept_2; 
  vector<lower=-1,upper=1>[NC_intercept_2] cor_intercept_2; 
  // take only relevant parts of correlation matrix 
  Cor_asymptote_1 = multiply_lower_tri_self_transpose(L_asymptote_1); 
  cor_asymptote_1[1] = Cor_asymptote_1[1,2]; 
  cor_asymptote_1[2] = Cor_asymptote_1[1,3]; 
  cor_asymptote_1[3] = Cor_asymptote_1[2,3]; 
  // take only relevant parts of correlation matrix 
  Cor_asymptote_2 = multiply_lower_tri_self_transpose(L_asymptote_2); 
  cor_asymptote_2[1] = Cor_asymptote_2[1,2]; 
  cor_asymptote_2[2] = Cor_asymptote_2[1,3]; 
  cor_asymptote_2[3] = Cor_asymptote_2[2,3]; 
  // take only relevant parts of correlation matrix 
  Cor_invrate_1 = multiply_lower_tri_self_transpose(L_invrate_1); 
  cor_invrate_1[1] = Cor_invrate_1[1,2]; 
  cor_invrate_1[2] = Cor_invrate_1[1,3]; 
  cor_invrate_1[3] = Cor_invrate_1[2,3]; 
  // take only relevant parts of correlation matrix 
  Cor_invrate_2 = multiply_lower_tri_self_transpose(L_invrate_2); 
  cor_invrate_2[1] = Cor_invrate_2[1,2]; 
  cor_invrate_2[2] = Cor_invrate_2[1,3]; 
  cor_invrate_2[3] = Cor_invrate_2[2,3]; 
  // take only relevant parts of correlation matrix 
  Cor_intercept_1 = multiply_lower_tri_self_transpose(L_intercept_1); 
  cor_intercept_1[1] = Cor_intercept_1[1,2]; 
  cor_intercept_1[2] = Cor_intercept_1[1,3]; 
  cor_intercept_1[3] = Cor_intercept_1[2,3]; 
  // take only relevant parts of correlation matrix 
  Cor_intercept_2 = multiply_lower_tri_self_transpose(L_intercept_2); 
  cor_intercept_2[1] = Cor_intercept_2[1,2]; 
  cor_intercept_2[2] = Cor_intercept_2[1,3]; 
  cor_intercept_2[3] = Cor_intercept_2[2,3]; 
} 
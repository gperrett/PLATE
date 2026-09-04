data{
  int<lower=0> N; // total plate appearances
  int<lower=0> n_pitchers; // # pitchers
  int<lower=0> n_batters; // # batters
  int<lower=0> M; // # of plate appearances that did not end in a strikeout
  int<lower=0> O; // # of in play outs 
  int<lower=0> P; // # of non-outs
  int<lower=0> Q; // # of hits
  int<lower=0> R; // # of power hits
  int<lower=2> K_rbi; // # of RBI categories: 4 = {0, 1, 2, 3+}

  array[N] int<lower=1,upper=n_batters> batter; // batter id
  array[N] int<lower=1,upper=n_pitchers> pitcher; // pitcher id

  vector[N] pitch_count; // pitch count

  array[M] int<lower=1,upper=N> index2; // row in 1:N for each node 2 observation
  array[O] int<lower=1,upper=N> index3; // row in 1:N for each node 3 observation
  array[P] int<lower=1,upper=N> index4; // row in 1:N for each node 4 observation
  array[Q] int<lower=1,upper=N> index5; // row in 1:N for each node 5 observation
  array[R] int<lower=1,upper=N> index6; // row in 1:N for each node 6 observation

  array[N] int<lower=0,upper=1> outcome1; // strike out (1) or not (0)
  array[M] int<lower=0,upper=1> outcome2; // out (1) or not (0)
  array[O] int<lower=0,upper=1> outcome3; // no rbi (1), rbi (0) given an out
  array[P] int<lower=0,upper=1> outcome4; // single/walk (1) or power (0)
  array[Q] int<lower=0,upper=1> outcome5; // no rbi (0) or rbi (1) given a single/walk
  array[R] int<lower=1,upper=K_rbi> outcome6; // RBI category given a power hit
}
parameters{
  sum_to_zero_vector[n_pitchers] theta; // pitcher ability, constrained to sum to 0
  real pct_so; // coefs for pitch count 
  real pct_m; // coefs for pitch count 
  real pct_o; // coefs for pitch count 
  real pct_p; // coefs for pitch count 
  real pct_q; // coefs for pitch count 
  real pct_r; // coefs for pitch count

  // slope and intercepts non-centered
  // node 1 (strikeout)
  vector[n_batters] alpha_so_z;
  vector[n_batters] gamma_so_z;
  real mu_alpha_so;
  real mu_gamma_so;
  real<lower=0> sigma_alpha_so;
  real<lower=0> sigma_gamma_so;

  // node 2 (out vs not, given not-K)
  vector[n_batters] alpha_m_z;
  vector[n_batters] gamma_m_z;
  real mu_alpha_m;
  real mu_gamma_m;
  real<lower=0> sigma_alpha_m;
  real<lower=0> sigma_gamma_m;

  // node 3 (no-rbi vs rbi, given an out)
  vector[n_batters] alpha_o_z;
  vector[n_batters] gamma_o_z;
  real mu_alpha_o;
  real mu_gamma_o;
  real<lower=0> sigma_alpha_o;
  real<lower=0> sigma_gamma_o;

  // node 4 (single/walk vs power)
  vector[n_batters] alpha_p_z;
  vector[n_batters] gamma_p_z;
  real mu_alpha_p;
  real mu_gamma_p;
  real<lower=0> sigma_alpha_p;
  real<lower=0> sigma_gamma_p;

  // node 5 (no-rbi vs rbi, given single/walk)
  vector[n_batters] alpha_q_z;
  vector[n_batters] gamma_q_z;
  real mu_alpha_q;
  real mu_gamma_q;
  real<lower=0> sigma_alpha_q;
  real<lower=0> sigma_gamma_q;

  // node 6 (GRM) — gamma_r kept zero-mean, matching original zero-mean beta_r
  vector[n_batters] alpha_r_z;
  vector[n_batters] gamma_r_z;
  real mu_alpha_r;
  real<lower=0> sigma_alpha_r;
  real<lower=0> sigma_gamma_r;
  ordered[K_rbi - 1] kappa_r; // one RBI category is implicit
}
transformed parameters {

  vector<lower=0>[n_batters] alpha_so = exp(mu_alpha_so + sigma_alpha_so * alpha_so_z);
  vector[n_batters] gamma_so = mu_gamma_so + sigma_gamma_so * gamma_so_z;

  vector<lower=0>[n_batters] alpha_m = exp(mu_alpha_m + sigma_alpha_m * alpha_m_z);
  vector[n_batters] gamma_m = mu_gamma_m + sigma_gamma_m * gamma_m_z;

  vector<lower=0>[n_batters] alpha_o = exp(mu_alpha_o + sigma_alpha_o * alpha_o_z);
  vector[n_batters] gamma_o = mu_gamma_o + sigma_gamma_o * gamma_o_z;

  vector<lower=0>[n_batters] alpha_p = exp(mu_alpha_p + sigma_alpha_p * alpha_p_z);
  vector[n_batters] gamma_p = mu_gamma_p + sigma_gamma_p * gamma_p_z;

  vector<lower=0>[n_batters] alpha_q = exp(mu_alpha_q + sigma_alpha_q * alpha_q_z);
  vector[n_batters] gamma_q = mu_gamma_q + sigma_gamma_q * gamma_q_z;

  vector<lower=0>[n_batters] alpha_r = exp(mu_alpha_r + sigma_alpha_r * alpha_r_z);
  vector[n_batters] gamma_r = sigma_gamma_r * gamma_r_z; // zero-mean, no mu_gamma_r (matches original zero-mean beta_r)
}
model{
  // prior for theta (pitcher ability)
  theta ~ normal(0, 1);
  // fixed effect priors
  pct_so ~ normal(0, 1);
  pct_m ~ normal(0, 1);
  pct_o ~ normal(0, 1);
  pct_p ~ normal(0, 1);
  pct_q ~ normal(0, 1);
  pct_r ~ normal(0, 1);


  // node 1
  mu_alpha_so ~ normal(0, .2);
  mu_gamma_so  ~ normal(0, .5);
  sigma_alpha_so ~ exponential(1);
  sigma_gamma_so  ~ exponential(1);
  alpha_so_z ~ std_normal();
  gamma_so_z  ~ std_normal();
  outcome1 ~ bernoulli_logit(
    pct_so*pitch_count
    + alpha_so[batter] .* theta[pitcher] + gamma_so[batter]
  );

  // node 2
  mu_alpha_m ~ normal(0, .2);
  mu_gamma_m  ~ normal(0, .5);
  sigma_alpha_m ~ exponential(1);
  sigma_gamma_m  ~ exponential(1);
  alpha_m_z ~ std_normal();
  gamma_m_z  ~ std_normal();
  outcome2 ~ bernoulli_logit(
    pct_m*pitch_count[index2]
    + alpha_m[batter[index2]] .* theta[pitcher[index2]] + gamma_m[batter[index2]]
  );

  // node 3
  mu_alpha_o ~ normal(0, .2);
  mu_gamma_o  ~ normal(0, .5);
  sigma_alpha_o ~ exponential(1);
  sigma_gamma_o  ~ exponential(1);
  alpha_o_z ~ std_normal();
  gamma_o_z  ~ std_normal();
  outcome3 ~ bernoulli_logit(
    pct_o*pitch_count[index3]
    + alpha_o[batter[index3]] .* theta[pitcher[index3]] + gamma_o[batter[index3]]
  );

  // node 4
  mu_alpha_p ~ normal(0, .2);
  mu_gamma_p  ~ normal(0, .5);
  sigma_alpha_p ~ exponential(1);
  sigma_gamma_p  ~ exponential(1);
  alpha_p_z ~ std_normal();
  gamma_p_z  ~ std_normal();
  outcome4 ~ bernoulli_logit(
    pct_p*pitch_count[index4]
    + alpha_p[batter[index4]] .* theta[pitcher[index4]] + gamma_p[batter[index4]]
  );

  // node 5
  mu_alpha_q ~ normal(0, .2);
  mu_gamma_q  ~ normal(0, .5);
  sigma_alpha_q ~ exponential(1);
  sigma_gamma_q  ~ exponential(1);
  alpha_q_z ~ std_normal();
  gamma_q_z  ~ std_normal();
  outcome5 ~ bernoulli_logit(
    pct_q*pitch_count[index5]
    + alpha_q[batter[index5]] .* theta[pitcher[index5]] + gamma_q[batter[index5]]
  );

  // node 6 — graded response model for RBI category
  mu_alpha_r ~ normal(0, .2);
  sigma_alpha_r ~ exponential(1);
  sigma_gamma_r  ~ exponential(1);
  alpha_r_z ~ std_normal();
  gamma_r_z  ~ std_normal();
  kappa_r ~ normal(0, 1);

  {
    vector[R] eta6 =
        pct_r*pitch_count[index6]
      + alpha_r[batter[index6]] .* theta[pitcher[index6]] + gamma_r[batter[index6]];
    outcome6 ~ ordered_logistic(eta6, kappa_r);
  }
}
generated quantities {
  // recover the interpretable strength parameters from the slope/intercept fit
  vector[n_batters] beta_so = -gamma_so ./ alpha_so;
  vector[n_batters] beta_m  = -gamma_m  ./ alpha_m;
  vector[n_batters] beta_o  = -gamma_o  ./ alpha_o;
  vector[n_batters] beta_p  = -gamma_p  ./ alpha_p;
  vector[n_batters] beta_q  = -gamma_q  ./ alpha_q;
  vector[n_batters] beta_r  = -gamma_r  ./ alpha_r;
}

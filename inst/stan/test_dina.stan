data {
  int<lower=1> I;                      // number of items
  int<lower=1> R;                      // number of respondents
  int<lower=1> N;                      // number of observations
  int<lower=1> C;                      // number of classes
  array[N] int<lower=1,upper=I> ii;    // item for observation n
  array[N] int<lower=1,upper=R> rr;    // respondent for observation n
  array[N] int<lower=0,upper=1> y;     // score for observation n
  array[R] int<lower=1,upper=N> start; // starting row for respondent R
  array[R] int<lower=1,upper=I> num;   // number items for respondent R
  matrix[I,C] Xi;                      // class attribute indicator
}
parameters {
  simplex[C] Vc;                  // base rates of class membership

  ////////////////////////////////// item parameters
  array[I] real<lower=0,upper=1> slip;
  array[I] real<lower=0,upper=1> guess;
}
transformed parameters {
  vector[C] log_Vc = log(Vc);
  matrix[I,C] pi;

  for (i in 1:I) {
    for (c in 1:C) {
      pi[i,c] = ((1 - slip[i]) ^ Xi[i,c]) * (guess[i] ^ (1 - Xi[i,c]));
    }
  }
}
model {

  ////////////////////////////////// priors
  Vc ~ dirichlet(rep_vector(1, C));
  guess[1] ~ beta(5, 25);
  slip[1] ~ beta(5, 25);
  guess[2] ~ beta(5, 25);
  slip[2] ~ beta(5, 25);
  guess[3] ~ beta(5, 25);
  slip[3] ~ beta(5, 25);
  guess[4] ~ beta(5, 25);
  slip[4] ~ beta(5, 25);
  guess[5] ~ beta(5, 25);
  slip[5] ~ beta(5, 25);
  guess[6] ~ beta(5, 25);
  slip[6] ~ beta(5, 25);
  guess[7] ~ beta(5, 25);
  slip[7] ~ beta(5, 25);
  guess[8] ~ beta(5, 25);
  slip[8] ~ beta(5, 25);
  guess[9] ~ beta(5, 25);
  slip[9] ~ beta(5, 25);
  guess[10] ~ beta(5, 25);
  slip[10] ~ beta(5, 25);
  guess[11] ~ beta(5, 25);
  slip[11] ~ beta(5, 25);
  guess[12] ~ beta(5, 25);
  slip[12] ~ beta(5, 25);
  guess[13] ~ beta(5, 25);
  slip[13] ~ beta(5, 25);
  guess[14] ~ beta(5, 25);
  slip[14] ~ beta(5, 25);
  guess[15] ~ beta(5, 25);
  slip[15] ~ beta(5, 25);
  guess[16] ~ beta(5, 25);
  slip[16] ~ beta(5, 25);
  guess[17] ~ beta(5, 25);
  slip[17] ~ beta(5, 25);
  guess[18] ~ beta(5, 25);
  slip[18] ~ beta(5, 25);
  guess[19] ~ beta(5, 25);
  slip[19] ~ beta(5, 25);
  guess[20] ~ beta(5, 25);
  slip[20] ~ beta(5, 25);
  guess[21] ~ beta(5, 25);
  slip[21] ~ beta(5, 25);
  guess[22] ~ beta(5, 25);
  slip[22] ~ beta(5, 25);
  guess[23] ~ beta(5, 25);
  slip[23] ~ beta(5, 25);
  guess[24] ~ beta(5, 25);
  slip[24] ~ beta(5, 25);
  guess[25] ~ beta(5, 25);
  slip[25] ~ beta(5, 25);
  guess[26] ~ beta(5, 25);
  slip[26] ~ beta(5, 25);
  guess[27] ~ beta(5, 25);
  slip[27] ~ beta(5, 25);
  guess[28] ~ beta(5, 25);
  slip[28] ~ beta(5, 25);
  guess[29] ~ beta(5, 25);
  slip[29] ~ beta(5, 25);
  guess[30] ~ beta(5, 25);
  slip[30] ~ beta(5, 25);
  guess[31] ~ beta(5, 25);
  slip[31] ~ beta(5, 25);
  guess[32] ~ beta(5, 25);
  slip[32] ~ beta(5, 25);
  guess[33] ~ beta(5, 25);
  slip[33] ~ beta(5, 25);
  guess[34] ~ beta(5, 25);
  slip[34] ~ beta(5, 25);
  guess[35] ~ beta(5, 25);
  slip[35] ~ beta(5, 25);

  ////////////////////////////////// likelihood
  for (r in 1:R) {
    row_vector[C] ps;
    for (c in 1:C) {
      array[num[r]] real log_items;
      for (m in 1:num[r]) {
        int i = ii[start[r] + m - 1];
        log_items[m] = y[start[r] + m - 1] * log(pi[i,c]) +
                       (1 - y[start[r] + m - 1]) * log(1 - pi[i,c]);
      }
      ps[c] = log_Vc[c] + sum(log_items);
    }
    target += log_sum_exp(ps);
  }
}

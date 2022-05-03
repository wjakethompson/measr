# dina model works

    Code
      dina$stancode
    Output
      data {
        int<lower=1> I;                 // number of items
        int<lower=1> R;                 // number of respondents
        int<lower=1> N;                 // number of observations
        int<lower=1> C;                 // number of classes
        int<lower=1> A;                 // number of attributes
        int<lower=1,upper=I> ii[N];     // item for observation n
        int<lower=1,upper=R> rr[N];     // respondent for observation n
        int<lower=0,upper=1> y[N];      // score for observation n
        int<lower=1,upper=N> start[R];  // starting row for respondent R
        int<lower=1,upper=I> num[R];    // number of rows (items) for respondent R
        matrix[C,A] Alpha;              // attribute pattern for each class
        matrix[I,C] Xi;                 // class attribute mastery indicator
      }
      parameters {
        simplex[C] Vc;                  // base rates of class membership
      
        ////////////////////////////////// item parameters
        real<lower=0,upper=1> slip[I];
        real<lower=0,upper=1> guess[I];
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
        real ps[C];
      
        ////////////////////////////////// priors
        slip[1] ~ beta(5, 25);
        guess[1] ~ beta(5, 25);
        slip[2] ~ beta(5, 25);
        guess[2] ~ beta(5, 25);
        slip[3] ~ beta(5, 25);
        guess[3] ~ beta(5, 25);
        slip[4] ~ beta(5, 25);
        guess[4] ~ beta(5, 25);
        slip[5] ~ beta(5, 25);
        guess[5] ~ beta(5, 25);
        slip[6] ~ beta(5, 25);
        guess[6] ~ beta(5, 25);
        slip[7] ~ beta(5, 25);
        guess[7] ~ beta(5, 25);
        slip[8] ~ beta(5, 25);
        guess[8] ~ beta(5, 25);
        slip[9] ~ beta(5, 25);
        guess[9] ~ beta(5, 25);
        slip[10] ~ beta(5, 25);
        guess[10] ~ beta(5, 25);
        slip[11] ~ beta(5, 25);
        guess[11] ~ beta(5, 25);
        slip[12] ~ beta(5, 25);
        guess[12] ~ beta(5, 25);
        slip[13] ~ beta(5, 25);
        guess[13] ~ beta(5, 25);
        slip[14] ~ beta(5, 25);
        guess[14] ~ beta(5, 25);
        slip[15] ~ beta(5, 25);
        guess[15] ~ beta(5, 25);
        slip[16] ~ beta(5, 25);
        guess[16] ~ beta(5, 25);
        slip[17] ~ beta(5, 25);
        guess[17] ~ beta(5, 25);
        slip[18] ~ beta(5, 25);
        guess[18] ~ beta(5, 25);
        slip[19] ~ beta(5, 25);
        guess[19] ~ beta(5, 25);
        slip[20] ~ beta(5, 25);
        guess[20] ~ beta(5, 25);
      
        ////////////////////////////////// likelihood
        for (r in 1:R) {
          for (c in 1:C) {
            real log_items[num[r]];
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

---

    Code
      dina_mcmc$stancode
    Output
      data {
        int<lower=1> I;                 // number of items
        int<lower=1> R;                 // number of respondents
        int<lower=1> N;                 // number of observations
        int<lower=1> C;                 // number of classes
        int<lower=1> A;                 // number of attributes
        int<lower=1,upper=I> ii[N];     // item for observation n
        int<lower=1,upper=R> rr[N];     // respondent for observation n
        int<lower=0,upper=1> y[N];      // score for observation n
        int<lower=1,upper=N> start[R];  // starting row for respondent R
        int<lower=1,upper=I> num[R];    // number of rows (items) for respondent R
        matrix[C,A] Alpha;              // attribute pattern for each class
        matrix[I,C] Xi;                 // class attribute mastery indicator
      }
      parameters {
        simplex[C] Vc;                  // base rates of class membership
      
        ////////////////////////////////// item parameters
        real<lower=0,upper=1> slip[I];
        real<lower=0,upper=1> guess[I];
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
        real ps[C];
      
        ////////////////////////////////// priors
        slip[1] ~ beta(5, 25);
        guess[1] ~ beta(5, 25);
        slip[2] ~ beta(5, 25);
        guess[2] ~ beta(5, 25);
        slip[3] ~ beta(5, 25);
        guess[3] ~ beta(5, 25);
        slip[4] ~ beta(5, 25);
        guess[4] ~ beta(5, 25);
        slip[5] ~ beta(5, 25);
        guess[5] ~ beta(5, 25);
        slip[6] ~ beta(5, 25);
        guess[6] ~ beta(5, 25);
        slip[7] ~ beta(5, 25);
        guess[7] ~ beta(5, 25);
        slip[8] ~ beta(5, 25);
        guess[8] ~ beta(5, 25);
        slip[9] ~ beta(5, 25);
        guess[9] ~ beta(5, 25);
        slip[10] ~ beta(5, 25);
        guess[10] ~ beta(5, 25);
        slip[11] ~ beta(5, 25);
        guess[11] ~ beta(5, 25);
        slip[12] ~ beta(5, 25);
        guess[12] ~ beta(5, 25);
        slip[13] ~ beta(5, 25);
        guess[13] ~ beta(5, 25);
        slip[14] ~ beta(5, 25);
        guess[14] ~ beta(5, 25);
        slip[15] ~ beta(5, 25);
        guess[15] ~ beta(5, 25);
        slip[16] ~ beta(5, 25);
        guess[16] ~ beta(5, 25);
        slip[17] ~ beta(5, 25);
        guess[17] ~ beta(5, 25);
        slip[18] ~ beta(5, 25);
        guess[18] ~ beta(5, 25);
        slip[19] ~ beta(5, 25);
        guess[19] ~ beta(5, 25);
        slip[20] ~ beta(5, 25);
        guess[20] ~ beta(5, 25);
      
        ////////////////////////////////// likelihood
        for (r in 1:R) {
          for (c in 1:C) {
            real log_items[num[r]];
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


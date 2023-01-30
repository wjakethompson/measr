# lcdm model works for mdm

    Code
      rstn_mdm_lcdm$stancode
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
      
        ////////////////////////////////// item intercepts
        real l1_0;
        real l2_0;
        real l3_0;
        real l4_0;
      
        ////////////////////////////////// item main effects
        real<lower=0> l1_11;
        real<lower=0> l2_11;
        real<lower=0> l3_11;
        real<lower=0> l4_11;
      }
      transformed parameters {
        vector[C] log_Vc = log(Vc);
        matrix[I,C] pi;
        
        ////////////////////////////////// probability of correct response
        pi[1,1] = inv_logit(l1_0);
        pi[1,2] = inv_logit(l1_0+l1_11);
        pi[2,1] = inv_logit(l2_0);
        pi[2,2] = inv_logit(l2_0+l2_11);
        pi[3,1] = inv_logit(l3_0);
        pi[3,2] = inv_logit(l3_0+l3_11);
        pi[4,1] = inv_logit(l4_0);
        pi[4,2] = inv_logit(l4_0+l4_11);
      }
      model {
        real ps[C];
      
        ////////////////////////////////// priors
        l1_0 ~ uniform(-15, 15);
        l1_11 ~ uniform(0, 15);
        l2_0 ~ uniform(-15, 15);
        l2_11 ~ uniform(0, 15);
        l3_0 ~ uniform(-15, 15);
        l3_11 ~ uniform(0, 15);
        l4_0 ~ uniform(-15, 15);
        l4_11 ~ uniform(0, 15);
      
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


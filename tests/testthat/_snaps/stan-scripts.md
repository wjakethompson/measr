# stan generated quantities script works

    Code
      gqs_script()
    Output
      $stancode
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
        vector[C] log_Vc;
        matrix[I,C] pi;
      }
      generated quantities {
        matrix[R,C] prob_resp_class;   // post prob of respondent R in class C
        matrix[R,A] prob_resp_attr;    // post prob of respondent R master A
        
        for (r in 1:R) {
          row_vector[C] prob_joint;
          for (c in 1:C) {
            real log_items[num[r]];
            for (m in 1:num[r]) {
              int i = ii[start[r] + m - 1];
              log_items[m] = y[start[r] + m - 1] * log(pi[i,c]) +
                             (1 - y[start[r] + m - 1]) * log(1 - pi[i,c]);
            }
            prob_joint[c] = log_Vc[c] + sum(log_items);
          }
          prob_resp_class[r] = exp(prob_joint) / exp(log_sum_exp(prob_joint));
        }
      
        for (r in 1:R) {
          for (a in 1:A) {
            row_vector[C] prob_attr_class;
            for (c in 1:C) {
              prob_attr_class[c] = prob_resp_class[r,c] * Alpha[c,a];
            }
            prob_resp_attr[r,a] = sum(prob_attr_class);
          }
        } 
      }
      

---

    Code
      gqs_script(full_data = TRUE)
    Output
      $stancode
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
        vector[C] log_Vc;
        matrix[I,C] pi;
      }
      generated quantities {
        matrix[R,C] prob_resp_class;   // post prob of respondent R in class C
        matrix[R,A] prob_resp_attr;    // post prob of respondent R master A
        int y_rep[N];
        int r_class[R];
      
        for (r in 1:R) {
          row_vector[C] prob_joint;
          for (c in 1:C) {
            real log_items[num[r]];
            for (m in 1:num[r]) {
              int i = ii[start[r] + m - 1];
              log_items[m] = y[start[r] + m - 1] * log(pi[i,c]) +
                             (1 - y[start[r] + m - 1]) * log(1 - pi[i,c]);
            }
            prob_joint[c] = log_Vc[c] + sum(log_items);
          }
          prob_resp_class[r] = exp(prob_joint) / exp(log_sum_exp(prob_joint));
        }
      
        for (r in 1:R) {
          for (a in 1:A) {
            row_vector[C] prob_attr_class;
            for (c in 1:C) {
              prob_attr_class[c] = prob_resp_class[r,c] * Alpha[c,a];
            }
            prob_resp_attr[r,a] = sum(prob_attr_class);
          }
        } 
      
        for (r in 1:R) {
          vector[C] r_probs = exp(log_Vc) / exp(log_sum_exp(log_Vc));
          r_class[r] = categorical_rng(r_probs);
          for (m in 1:num[r]) {
            int i = ii[start[r] + m - 1];
            y_rep[start[r] + m - 1] = bernoulli_rng(pi[i, r_class[r]]);
          }
        }
      }
      

# stan log_lik script works

    Code
      loglik_script()
    Output
      $stancode
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
        vector[C] log_Vc;
        matrix[I,C] pi;
      }
      generated quantities {
        vector[R] log_lik;
      
        for (r in 1:R) {
          row_vector[C] prob_joint;
          for (c in 1:C) {
            real log_items[num[r]];
            for (m in 1:num[r]) {
              int i = ii[start[r] + m - 1];
              log_items[m] = y[start[r] + m - 1] * log(pi[i,c]) +
                             (1 - y[start[r] + m - 1]) * log(1 - pi[i,c]);
            }
            prob_joint[c] = log_Vc[c] + sum(log_items);
          }
          log_lik[r] = log_sum_exp(prob_joint);
        }
      }
      

# lcdm script works

    Code
      lcdm_script(ecpe_q)
    Output
      $stancode
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
        real l5_0;
        real l6_0;
        real l7_0;
        real l8_0;
        real l9_0;
        real l10_0;
        real l11_0;
        real l12_0;
        real l13_0;
        real l14_0;
        real l15_0;
        real l16_0;
        real l17_0;
        real l18_0;
        real l19_0;
        real l20_0;
        real l21_0;
        real l22_0;
        real l23_0;
        real l24_0;
        real l25_0;
        real l26_0;
        real l27_0;
        real l28_0;
      
        ////////////////////////////////// item main effects
        real<lower=0> l1_11;
        real<lower=0> l1_12;
        real<lower=0> l2_12;
        real<lower=0> l3_11;
        real<lower=0> l3_13;
        real<lower=0> l4_13;
        real<lower=0> l5_13;
        real<lower=0> l6_13;
        real<lower=0> l7_11;
        real<lower=0> l7_13;
        real<lower=0> l8_12;
        real<lower=0> l9_13;
        real<lower=0> l10_11;
        real<lower=0> l11_11;
        real<lower=0> l11_13;
        real<lower=0> l12_11;
        real<lower=0> l12_13;
        real<lower=0> l13_11;
        real<lower=0> l14_11;
        real<lower=0> l15_13;
        real<lower=0> l16_11;
        real<lower=0> l16_13;
        real<lower=0> l17_12;
        real<lower=0> l17_13;
        real<lower=0> l18_13;
        real<lower=0> l19_13;
        real<lower=0> l20_11;
        real<lower=0> l20_13;
        real<lower=0> l21_11;
        real<lower=0> l21_13;
        real<lower=0> l22_13;
        real<lower=0> l23_12;
        real<lower=0> l24_12;
        real<lower=0> l25_11;
        real<lower=0> l26_13;
        real<lower=0> l27_11;
        real<lower=0> l28_13;
      
        ////////////////////////////////// item interactions
        real<lower=-1 * min([l1_11,l1_12])> l1_212;
        real<lower=-1 * min([l3_11,l3_13])> l3_213;
        real<lower=-1 * min([l7_11,l7_13])> l7_213;
        real<lower=-1 * min([l11_11,l11_13])> l11_213;
        real<lower=-1 * min([l12_11,l12_13])> l12_213;
        real<lower=-1 * min([l16_11,l16_13])> l16_213;
        real<lower=-1 * min([l17_12,l17_13])> l17_223;
        real<lower=-1 * min([l20_11,l20_13])> l20_213;
        real<lower=-1 * min([l21_11,l21_13])> l21_213;
      }
      transformed parameters {
        vector[C] log_Vc = log(Vc);
        matrix[I,C] pi;
      
        ////////////////////////////////// probability of correct response
        pi[1,1] = inv_logit(l1_0);
        pi[1,2] = inv_logit(l1_0+l1_11);
        pi[1,3] = inv_logit(l1_0+l1_12);
        pi[1,4] = inv_logit(l1_0);
        pi[1,5] = inv_logit(l1_0+l1_11+l1_12+l1_212);
        pi[1,6] = inv_logit(l1_0+l1_11);
        pi[1,7] = inv_logit(l1_0+l1_12);
        pi[1,8] = inv_logit(l1_0+l1_11+l1_12+l1_212);
        pi[2,1] = inv_logit(l2_0);
        pi[2,2] = inv_logit(l2_0);
        pi[2,3] = inv_logit(l2_0+l2_12);
        pi[2,4] = inv_logit(l2_0);
        pi[2,5] = inv_logit(l2_0+l2_12);
        pi[2,6] = inv_logit(l2_0);
        pi[2,7] = inv_logit(l2_0+l2_12);
        pi[2,8] = inv_logit(l2_0+l2_12);
        pi[3,1] = inv_logit(l3_0);
        pi[3,2] = inv_logit(l3_0+l3_11);
        pi[3,3] = inv_logit(l3_0);
        pi[3,4] = inv_logit(l3_0+l3_13);
        pi[3,5] = inv_logit(l3_0+l3_11);
        pi[3,6] = inv_logit(l3_0+l3_11+l3_13+l3_213);
        pi[3,7] = inv_logit(l3_0+l3_13);
        pi[3,8] = inv_logit(l3_0+l3_11+l3_13+l3_213);
        pi[4,1] = inv_logit(l4_0);
        pi[4,2] = inv_logit(l4_0);
        pi[4,3] = inv_logit(l4_0);
        pi[4,4] = inv_logit(l4_0+l4_13);
        pi[4,5] = inv_logit(l4_0);
        pi[4,6] = inv_logit(l4_0+l4_13);
        pi[4,7] = inv_logit(l4_0+l4_13);
        pi[4,8] = inv_logit(l4_0+l4_13);
        pi[5,1] = inv_logit(l5_0);
        pi[5,2] = inv_logit(l5_0);
        pi[5,3] = inv_logit(l5_0);
        pi[5,4] = inv_logit(l5_0+l5_13);
        pi[5,5] = inv_logit(l5_0);
        pi[5,6] = inv_logit(l5_0+l5_13);
        pi[5,7] = inv_logit(l5_0+l5_13);
        pi[5,8] = inv_logit(l5_0+l5_13);
        pi[6,1] = inv_logit(l6_0);
        pi[6,2] = inv_logit(l6_0);
        pi[6,3] = inv_logit(l6_0);
        pi[6,4] = inv_logit(l6_0+l6_13);
        pi[6,5] = inv_logit(l6_0);
        pi[6,6] = inv_logit(l6_0+l6_13);
        pi[6,7] = inv_logit(l6_0+l6_13);
        pi[6,8] = inv_logit(l6_0+l6_13);
        pi[7,1] = inv_logit(l7_0);
        pi[7,2] = inv_logit(l7_0+l7_11);
        pi[7,3] = inv_logit(l7_0);
        pi[7,4] = inv_logit(l7_0+l7_13);
        pi[7,5] = inv_logit(l7_0+l7_11);
        pi[7,6] = inv_logit(l7_0+l7_11+l7_13+l7_213);
        pi[7,7] = inv_logit(l7_0+l7_13);
        pi[7,8] = inv_logit(l7_0+l7_11+l7_13+l7_213);
        pi[8,1] = inv_logit(l8_0);
        pi[8,2] = inv_logit(l8_0);
        pi[8,3] = inv_logit(l8_0+l8_12);
        pi[8,4] = inv_logit(l8_0);
        pi[8,5] = inv_logit(l8_0+l8_12);
        pi[8,6] = inv_logit(l8_0);
        pi[8,7] = inv_logit(l8_0+l8_12);
        pi[8,8] = inv_logit(l8_0+l8_12);
        pi[9,1] = inv_logit(l9_0);
        pi[9,2] = inv_logit(l9_0);
        pi[9,3] = inv_logit(l9_0);
        pi[9,4] = inv_logit(l9_0+l9_13);
        pi[9,5] = inv_logit(l9_0);
        pi[9,6] = inv_logit(l9_0+l9_13);
        pi[9,7] = inv_logit(l9_0+l9_13);
        pi[9,8] = inv_logit(l9_0+l9_13);
        pi[10,1] = inv_logit(l10_0);
        pi[10,2] = inv_logit(l10_0+l10_11);
        pi[10,3] = inv_logit(l10_0);
        pi[10,4] = inv_logit(l10_0);
        pi[10,5] = inv_logit(l10_0+l10_11);
        pi[10,6] = inv_logit(l10_0+l10_11);
        pi[10,7] = inv_logit(l10_0);
        pi[10,8] = inv_logit(l10_0+l10_11);
        pi[11,1] = inv_logit(l11_0);
        pi[11,2] = inv_logit(l11_0+l11_11);
        pi[11,3] = inv_logit(l11_0);
        pi[11,4] = inv_logit(l11_0+l11_13);
        pi[11,5] = inv_logit(l11_0+l11_11);
        pi[11,6] = inv_logit(l11_0+l11_11+l11_13+l11_213);
        pi[11,7] = inv_logit(l11_0+l11_13);
        pi[11,8] = inv_logit(l11_0+l11_11+l11_13+l11_213);
        pi[12,1] = inv_logit(l12_0);
        pi[12,2] = inv_logit(l12_0+l12_11);
        pi[12,3] = inv_logit(l12_0);
        pi[12,4] = inv_logit(l12_0+l12_13);
        pi[12,5] = inv_logit(l12_0+l12_11);
        pi[12,6] = inv_logit(l12_0+l12_11+l12_13+l12_213);
        pi[12,7] = inv_logit(l12_0+l12_13);
        pi[12,8] = inv_logit(l12_0+l12_11+l12_13+l12_213);
        pi[13,1] = inv_logit(l13_0);
        pi[13,2] = inv_logit(l13_0+l13_11);
        pi[13,3] = inv_logit(l13_0);
        pi[13,4] = inv_logit(l13_0);
        pi[13,5] = inv_logit(l13_0+l13_11);
        pi[13,6] = inv_logit(l13_0+l13_11);
        pi[13,7] = inv_logit(l13_0);
        pi[13,8] = inv_logit(l13_0+l13_11);
        pi[14,1] = inv_logit(l14_0);
        pi[14,2] = inv_logit(l14_0+l14_11);
        pi[14,3] = inv_logit(l14_0);
        pi[14,4] = inv_logit(l14_0);
        pi[14,5] = inv_logit(l14_0+l14_11);
        pi[14,6] = inv_logit(l14_0+l14_11);
        pi[14,7] = inv_logit(l14_0);
        pi[14,8] = inv_logit(l14_0+l14_11);
        pi[15,1] = inv_logit(l15_0);
        pi[15,2] = inv_logit(l15_0);
        pi[15,3] = inv_logit(l15_0);
        pi[15,4] = inv_logit(l15_0+l15_13);
        pi[15,5] = inv_logit(l15_0);
        pi[15,6] = inv_logit(l15_0+l15_13);
        pi[15,7] = inv_logit(l15_0+l15_13);
        pi[15,8] = inv_logit(l15_0+l15_13);
        pi[16,1] = inv_logit(l16_0);
        pi[16,2] = inv_logit(l16_0+l16_11);
        pi[16,3] = inv_logit(l16_0);
        pi[16,4] = inv_logit(l16_0+l16_13);
        pi[16,5] = inv_logit(l16_0+l16_11);
        pi[16,6] = inv_logit(l16_0+l16_11+l16_13+l16_213);
        pi[16,7] = inv_logit(l16_0+l16_13);
        pi[16,8] = inv_logit(l16_0+l16_11+l16_13+l16_213);
        pi[17,1] = inv_logit(l17_0);
        pi[17,2] = inv_logit(l17_0);
        pi[17,3] = inv_logit(l17_0+l17_12);
        pi[17,4] = inv_logit(l17_0+l17_13);
        pi[17,5] = inv_logit(l17_0+l17_12);
        pi[17,6] = inv_logit(l17_0+l17_13);
        pi[17,7] = inv_logit(l17_0+l17_12+l17_13+l17_223);
        pi[17,8] = inv_logit(l17_0+l17_12+l17_13+l17_223);
        pi[18,1] = inv_logit(l18_0);
        pi[18,2] = inv_logit(l18_0);
        pi[18,3] = inv_logit(l18_0);
        pi[18,4] = inv_logit(l18_0+l18_13);
        pi[18,5] = inv_logit(l18_0);
        pi[18,6] = inv_logit(l18_0+l18_13);
        pi[18,7] = inv_logit(l18_0+l18_13);
        pi[18,8] = inv_logit(l18_0+l18_13);
        pi[19,1] = inv_logit(l19_0);
        pi[19,2] = inv_logit(l19_0);
        pi[19,3] = inv_logit(l19_0);
        pi[19,4] = inv_logit(l19_0+l19_13);
        pi[19,5] = inv_logit(l19_0);
        pi[19,6] = inv_logit(l19_0+l19_13);
        pi[19,7] = inv_logit(l19_0+l19_13);
        pi[19,8] = inv_logit(l19_0+l19_13);
        pi[20,1] = inv_logit(l20_0);
        pi[20,2] = inv_logit(l20_0+l20_11);
        pi[20,3] = inv_logit(l20_0);
        pi[20,4] = inv_logit(l20_0+l20_13);
        pi[20,5] = inv_logit(l20_0+l20_11);
        pi[20,6] = inv_logit(l20_0+l20_11+l20_13+l20_213);
        pi[20,7] = inv_logit(l20_0+l20_13);
        pi[20,8] = inv_logit(l20_0+l20_11+l20_13+l20_213);
        pi[21,1] = inv_logit(l21_0);
        pi[21,2] = inv_logit(l21_0+l21_11);
        pi[21,3] = inv_logit(l21_0);
        pi[21,4] = inv_logit(l21_0+l21_13);
        pi[21,5] = inv_logit(l21_0+l21_11);
        pi[21,6] = inv_logit(l21_0+l21_11+l21_13+l21_213);
        pi[21,7] = inv_logit(l21_0+l21_13);
        pi[21,8] = inv_logit(l21_0+l21_11+l21_13+l21_213);
        pi[22,1] = inv_logit(l22_0);
        pi[22,2] = inv_logit(l22_0);
        pi[22,3] = inv_logit(l22_0);
        pi[22,4] = inv_logit(l22_0+l22_13);
        pi[22,5] = inv_logit(l22_0);
        pi[22,6] = inv_logit(l22_0+l22_13);
        pi[22,7] = inv_logit(l22_0+l22_13);
        pi[22,8] = inv_logit(l22_0+l22_13);
        pi[23,1] = inv_logit(l23_0);
        pi[23,2] = inv_logit(l23_0);
        pi[23,3] = inv_logit(l23_0+l23_12);
        pi[23,4] = inv_logit(l23_0);
        pi[23,5] = inv_logit(l23_0+l23_12);
        pi[23,6] = inv_logit(l23_0);
        pi[23,7] = inv_logit(l23_0+l23_12);
        pi[23,8] = inv_logit(l23_0+l23_12);
        pi[24,1] = inv_logit(l24_0);
        pi[24,2] = inv_logit(l24_0);
        pi[24,3] = inv_logit(l24_0+l24_12);
        pi[24,4] = inv_logit(l24_0);
        pi[24,5] = inv_logit(l24_0+l24_12);
        pi[24,6] = inv_logit(l24_0);
        pi[24,7] = inv_logit(l24_0+l24_12);
        pi[24,8] = inv_logit(l24_0+l24_12);
        pi[25,1] = inv_logit(l25_0);
        pi[25,2] = inv_logit(l25_0+l25_11);
        pi[25,3] = inv_logit(l25_0);
        pi[25,4] = inv_logit(l25_0);
        pi[25,5] = inv_logit(l25_0+l25_11);
        pi[25,6] = inv_logit(l25_0+l25_11);
        pi[25,7] = inv_logit(l25_0);
        pi[25,8] = inv_logit(l25_0+l25_11);
        pi[26,1] = inv_logit(l26_0);
        pi[26,2] = inv_logit(l26_0);
        pi[26,3] = inv_logit(l26_0);
        pi[26,4] = inv_logit(l26_0+l26_13);
        pi[26,5] = inv_logit(l26_0);
        pi[26,6] = inv_logit(l26_0+l26_13);
        pi[26,7] = inv_logit(l26_0+l26_13);
        pi[26,8] = inv_logit(l26_0+l26_13);
        pi[27,1] = inv_logit(l27_0);
        pi[27,2] = inv_logit(l27_0+l27_11);
        pi[27,3] = inv_logit(l27_0);
        pi[27,4] = inv_logit(l27_0);
        pi[27,5] = inv_logit(l27_0+l27_11);
        pi[27,6] = inv_logit(l27_0+l27_11);
        pi[27,7] = inv_logit(l27_0);
        pi[27,8] = inv_logit(l27_0+l27_11);
        pi[28,1] = inv_logit(l28_0);
        pi[28,2] = inv_logit(l28_0);
        pi[28,3] = inv_logit(l28_0);
        pi[28,4] = inv_logit(l28_0+l28_13);
        pi[28,5] = inv_logit(l28_0);
        pi[28,6] = inv_logit(l28_0+l28_13);
        pi[28,7] = inv_logit(l28_0+l28_13);
        pi[28,8] = inv_logit(l28_0+l28_13);
      }
      model {
        real ps[C];
      
        ////////////////////////////////// priors
        Vc ~ dirichlet(rep_vector(1, C));
        l1_0 ~ normal(0, 2);
        l1_11 ~ lognormal(0, 1);
        l1_12 ~ lognormal(0, 1);
        l1_212 ~ normal(0, 2);
        l2_0 ~ normal(0, 2);
        l2_12 ~ lognormal(0, 1);
        l3_0 ~ normal(0, 2);
        l3_11 ~ lognormal(0, 1);
        l3_13 ~ lognormal(0, 1);
        l3_213 ~ normal(0, 2);
        l4_0 ~ normal(0, 2);
        l4_13 ~ lognormal(0, 1);
        l5_0 ~ normal(0, 2);
        l5_13 ~ lognormal(0, 1);
        l6_0 ~ normal(0, 2);
        l6_13 ~ lognormal(0, 1);
        l7_0 ~ normal(0, 2);
        l7_11 ~ lognormal(0, 1);
        l7_13 ~ lognormal(0, 1);
        l7_213 ~ normal(0, 2);
        l8_0 ~ normal(0, 2);
        l8_12 ~ lognormal(0, 1);
        l9_0 ~ normal(0, 2);
        l9_13 ~ lognormal(0, 1);
        l10_0 ~ normal(0, 2);
        l10_11 ~ lognormal(0, 1);
        l11_0 ~ normal(0, 2);
        l11_11 ~ lognormal(0, 1);
        l11_13 ~ lognormal(0, 1);
        l11_213 ~ normal(0, 2);
        l12_0 ~ normal(0, 2);
        l12_11 ~ lognormal(0, 1);
        l12_13 ~ lognormal(0, 1);
        l12_213 ~ normal(0, 2);
        l13_0 ~ normal(0, 2);
        l13_11 ~ lognormal(0, 1);
        l14_0 ~ normal(0, 2);
        l14_11 ~ lognormal(0, 1);
        l15_0 ~ normal(0, 2);
        l15_13 ~ lognormal(0, 1);
        l16_0 ~ normal(0, 2);
        l16_11 ~ lognormal(0, 1);
        l16_13 ~ lognormal(0, 1);
        l16_213 ~ normal(0, 2);
        l17_0 ~ normal(0, 2);
        l17_12 ~ lognormal(0, 1);
        l17_13 ~ lognormal(0, 1);
        l17_223 ~ normal(0, 2);
        l18_0 ~ normal(0, 2);
        l18_13 ~ lognormal(0, 1);
        l19_0 ~ normal(0, 2);
        l19_13 ~ lognormal(0, 1);
        l20_0 ~ normal(0, 2);
        l20_11 ~ lognormal(0, 1);
        l20_13 ~ lognormal(0, 1);
        l20_213 ~ normal(0, 2);
        l21_0 ~ normal(0, 2);
        l21_11 ~ lognormal(0, 1);
        l21_13 ~ lognormal(0, 1);
        l21_213 ~ normal(0, 2);
        l22_0 ~ normal(0, 2);
        l22_13 ~ lognormal(0, 1);
        l23_0 ~ normal(0, 2);
        l23_12 ~ lognormal(0, 1);
        l24_0 ~ normal(0, 2);
        l24_12 ~ lognormal(0, 1);
        l25_0 ~ normal(0, 2);
        l25_11 ~ lognormal(0, 1);
        l26_0 ~ normal(0, 2);
        l26_13 ~ lognormal(0, 1);
        l27_0 ~ normal(0, 2);
        l27_11 ~ lognormal(0, 1);
        l28_0 ~ normal(0, 2);
        l28_13 ~ lognormal(0, 1);
      
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
      
      $prior
      # A tibble: 4 x 3
        class       coef  prior_def                  
        <chr>       <chr> <chr>                      
      1 intercept   <NA>  normal(0, 2)               
      2 maineffect  <NA>  lognormal(0, 1)            
      3 interaction <NA>  normal(0, 2)               
      4 structural  Vc    dirichlet(rep_vector(1, C))
      

---

    Code
      lcdm_script(mdm_q)
    Output
      $stancode
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
        Vc ~ dirichlet(rep_vector(1, C));
        l1_0 ~ normal(0, 2);
        l1_11 ~ lognormal(0, 1);
        l2_0 ~ normal(0, 2);
        l2_11 ~ lognormal(0, 1);
        l3_0 ~ normal(0, 2);
        l3_11 ~ lognormal(0, 1);
        l4_0 ~ normal(0, 2);
        l4_11 ~ lognormal(0, 1);
      
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
      
      $prior
      # A tibble: 4 x 3
        class       coef  prior_def                  
        <chr>       <chr> <chr>                      
      1 intercept   <NA>  normal(0, 2)               
      2 maineffect  <NA>  lognormal(0, 1)            
      3 interaction <NA>  normal(0, 2)               
      4 structural  Vc    dirichlet(rep_vector(1, C))
      

---

    Code
      lcdm_script(dtmr_q)
    Output
      $stancode
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
        real l5_0;
        real l6_0;
        real l7_0;
        real l8_0;
        real l9_0;
        real l10_0;
        real l11_0;
        real l12_0;
        real l13_0;
        real l14_0;
        real l15_0;
        real l16_0;
        real l17_0;
        real l18_0;
        real l19_0;
        real l20_0;
        real l21_0;
        real l22_0;
        real l23_0;
        real l24_0;
        real l25_0;
        real l26_0;
        real l27_0;
      
        ////////////////////////////////// item main effects
        real<lower=0> l1_11;
        real<lower=0> l2_13;
        real<lower=0> l3_12;
        real<lower=0> l4_11;
        real<lower=0> l5_11;
        real<lower=0> l6_12;
        real<lower=0> l7_11;
        real<lower=0> l8_13;
        real<lower=0> l9_13;
        real<lower=0> l10_13;
        real<lower=0> l11_13;
        real<lower=0> l12_11;
        real<lower=0> l13_14;
        real<lower=0> l14_11;
        real<lower=0> l14_14;
        real<lower=0> l15_11;
        real<lower=0> l15_14;
        real<lower=0> l16_11;
        real<lower=0> l17_11;
        real<lower=0> l18_12;
        real<lower=0> l18_14;
        real<lower=0> l19_11;
        real<lower=0> l19_12;
        real<lower=0> l20_12;
        real<lower=0> l20_14;
        real<lower=0> l21_12;
        real<lower=0> l22_12;
        real<lower=0> l23_11;
        real<lower=0> l24_11;
        real<lower=0> l24_12;
        real<lower=0> l25_11;
        real<lower=0> l25_12;
        real<lower=0> l26_11;
        real<lower=0> l27_11;
        real<lower=0> l27_12;
      
        ////////////////////////////////// item interactions
        real<lower=-1 * min([l14_11,l14_14])> l14_214;
        real<lower=-1 * min([l15_11,l15_14])> l15_214;
        real<lower=-1 * min([l18_12,l18_14])> l18_224;
        real<lower=-1 * min([l19_11,l19_12])> l19_212;
        real<lower=-1 * min([l20_12,l20_14])> l20_224;
        real<lower=-1 * min([l24_11,l24_12])> l24_212;
        real<lower=-1 * min([l25_11,l25_12])> l25_212;
        real<lower=-1 * min([l27_11,l27_12])> l27_212;
      }
      transformed parameters {
        vector[C] log_Vc = log(Vc);
        matrix[I,C] pi;
      
        ////////////////////////////////// probability of correct response
        pi[1,1] = inv_logit(l1_0);
        pi[1,2] = inv_logit(l1_0+l1_11);
        pi[1,3] = inv_logit(l1_0);
        pi[1,4] = inv_logit(l1_0);
        pi[1,5] = inv_logit(l1_0);
        pi[1,6] = inv_logit(l1_0+l1_11);
        pi[1,7] = inv_logit(l1_0+l1_11);
        pi[1,8] = inv_logit(l1_0+l1_11);
        pi[1,9] = inv_logit(l1_0);
        pi[1,10] = inv_logit(l1_0);
        pi[1,11] = inv_logit(l1_0);
        pi[1,12] = inv_logit(l1_0+l1_11);
        pi[1,13] = inv_logit(l1_0+l1_11);
        pi[1,14] = inv_logit(l1_0+l1_11);
        pi[1,15] = inv_logit(l1_0);
        pi[1,16] = inv_logit(l1_0+l1_11);
        pi[2,1] = inv_logit(l2_0);
        pi[2,2] = inv_logit(l2_0);
        pi[2,3] = inv_logit(l2_0);
        pi[2,4] = inv_logit(l2_0+l2_13);
        pi[2,5] = inv_logit(l2_0);
        pi[2,6] = inv_logit(l2_0);
        pi[2,7] = inv_logit(l2_0+l2_13);
        pi[2,8] = inv_logit(l2_0);
        pi[2,9] = inv_logit(l2_0+l2_13);
        pi[2,10] = inv_logit(l2_0);
        pi[2,11] = inv_logit(l2_0+l2_13);
        pi[2,12] = inv_logit(l2_0+l2_13);
        pi[2,13] = inv_logit(l2_0);
        pi[2,14] = inv_logit(l2_0+l2_13);
        pi[2,15] = inv_logit(l2_0+l2_13);
        pi[2,16] = inv_logit(l2_0+l2_13);
        pi[3,1] = inv_logit(l3_0);
        pi[3,2] = inv_logit(l3_0);
        pi[3,3] = inv_logit(l3_0+l3_12);
        pi[3,4] = inv_logit(l3_0);
        pi[3,5] = inv_logit(l3_0);
        pi[3,6] = inv_logit(l3_0+l3_12);
        pi[3,7] = inv_logit(l3_0);
        pi[3,8] = inv_logit(l3_0);
        pi[3,9] = inv_logit(l3_0+l3_12);
        pi[3,10] = inv_logit(l3_0+l3_12);
        pi[3,11] = inv_logit(l3_0);
        pi[3,12] = inv_logit(l3_0+l3_12);
        pi[3,13] = inv_logit(l3_0+l3_12);
        pi[3,14] = inv_logit(l3_0);
        pi[3,15] = inv_logit(l3_0+l3_12);
        pi[3,16] = inv_logit(l3_0+l3_12);
        pi[4,1] = inv_logit(l4_0);
        pi[4,2] = inv_logit(l4_0+l4_11);
        pi[4,3] = inv_logit(l4_0);
        pi[4,4] = inv_logit(l4_0);
        pi[4,5] = inv_logit(l4_0);
        pi[4,6] = inv_logit(l4_0+l4_11);
        pi[4,7] = inv_logit(l4_0+l4_11);
        pi[4,8] = inv_logit(l4_0+l4_11);
        pi[4,9] = inv_logit(l4_0);
        pi[4,10] = inv_logit(l4_0);
        pi[4,11] = inv_logit(l4_0);
        pi[4,12] = inv_logit(l4_0+l4_11);
        pi[4,13] = inv_logit(l4_0+l4_11);
        pi[4,14] = inv_logit(l4_0+l4_11);
        pi[4,15] = inv_logit(l4_0);
        pi[4,16] = inv_logit(l4_0+l4_11);
        pi[5,1] = inv_logit(l5_0);
        pi[5,2] = inv_logit(l5_0+l5_11);
        pi[5,3] = inv_logit(l5_0);
        pi[5,4] = inv_logit(l5_0);
        pi[5,5] = inv_logit(l5_0);
        pi[5,6] = inv_logit(l5_0+l5_11);
        pi[5,7] = inv_logit(l5_0+l5_11);
        pi[5,8] = inv_logit(l5_0+l5_11);
        pi[5,9] = inv_logit(l5_0);
        pi[5,10] = inv_logit(l5_0);
        pi[5,11] = inv_logit(l5_0);
        pi[5,12] = inv_logit(l5_0+l5_11);
        pi[5,13] = inv_logit(l5_0+l5_11);
        pi[5,14] = inv_logit(l5_0+l5_11);
        pi[5,15] = inv_logit(l5_0);
        pi[5,16] = inv_logit(l5_0+l5_11);
        pi[6,1] = inv_logit(l6_0);
        pi[6,2] = inv_logit(l6_0);
        pi[6,3] = inv_logit(l6_0+l6_12);
        pi[6,4] = inv_logit(l6_0);
        pi[6,5] = inv_logit(l6_0);
        pi[6,6] = inv_logit(l6_0+l6_12);
        pi[6,7] = inv_logit(l6_0);
        pi[6,8] = inv_logit(l6_0);
        pi[6,9] = inv_logit(l6_0+l6_12);
        pi[6,10] = inv_logit(l6_0+l6_12);
        pi[6,11] = inv_logit(l6_0);
        pi[6,12] = inv_logit(l6_0+l6_12);
        pi[6,13] = inv_logit(l6_0+l6_12);
        pi[6,14] = inv_logit(l6_0);
        pi[6,15] = inv_logit(l6_0+l6_12);
        pi[6,16] = inv_logit(l6_0+l6_12);
        pi[7,1] = inv_logit(l7_0);
        pi[7,2] = inv_logit(l7_0+l7_11);
        pi[7,3] = inv_logit(l7_0);
        pi[7,4] = inv_logit(l7_0);
        pi[7,5] = inv_logit(l7_0);
        pi[7,6] = inv_logit(l7_0+l7_11);
        pi[7,7] = inv_logit(l7_0+l7_11);
        pi[7,8] = inv_logit(l7_0+l7_11);
        pi[7,9] = inv_logit(l7_0);
        pi[7,10] = inv_logit(l7_0);
        pi[7,11] = inv_logit(l7_0);
        pi[7,12] = inv_logit(l7_0+l7_11);
        pi[7,13] = inv_logit(l7_0+l7_11);
        pi[7,14] = inv_logit(l7_0+l7_11);
        pi[7,15] = inv_logit(l7_0);
        pi[7,16] = inv_logit(l7_0+l7_11);
        pi[8,1] = inv_logit(l8_0);
        pi[8,2] = inv_logit(l8_0);
        pi[8,3] = inv_logit(l8_0);
        pi[8,4] = inv_logit(l8_0+l8_13);
        pi[8,5] = inv_logit(l8_0);
        pi[8,6] = inv_logit(l8_0);
        pi[8,7] = inv_logit(l8_0+l8_13);
        pi[8,8] = inv_logit(l8_0);
        pi[8,9] = inv_logit(l8_0+l8_13);
        pi[8,10] = inv_logit(l8_0);
        pi[8,11] = inv_logit(l8_0+l8_13);
        pi[8,12] = inv_logit(l8_0+l8_13);
        pi[8,13] = inv_logit(l8_0);
        pi[8,14] = inv_logit(l8_0+l8_13);
        pi[8,15] = inv_logit(l8_0+l8_13);
        pi[8,16] = inv_logit(l8_0+l8_13);
        pi[9,1] = inv_logit(l9_0);
        pi[9,2] = inv_logit(l9_0);
        pi[9,3] = inv_logit(l9_0);
        pi[9,4] = inv_logit(l9_0+l9_13);
        pi[9,5] = inv_logit(l9_0);
        pi[9,6] = inv_logit(l9_0);
        pi[9,7] = inv_logit(l9_0+l9_13);
        pi[9,8] = inv_logit(l9_0);
        pi[9,9] = inv_logit(l9_0+l9_13);
        pi[9,10] = inv_logit(l9_0);
        pi[9,11] = inv_logit(l9_0+l9_13);
        pi[9,12] = inv_logit(l9_0+l9_13);
        pi[9,13] = inv_logit(l9_0);
        pi[9,14] = inv_logit(l9_0+l9_13);
        pi[9,15] = inv_logit(l9_0+l9_13);
        pi[9,16] = inv_logit(l9_0+l9_13);
        pi[10,1] = inv_logit(l10_0);
        pi[10,2] = inv_logit(l10_0);
        pi[10,3] = inv_logit(l10_0);
        pi[10,4] = inv_logit(l10_0+l10_13);
        pi[10,5] = inv_logit(l10_0);
        pi[10,6] = inv_logit(l10_0);
        pi[10,7] = inv_logit(l10_0+l10_13);
        pi[10,8] = inv_logit(l10_0);
        pi[10,9] = inv_logit(l10_0+l10_13);
        pi[10,10] = inv_logit(l10_0);
        pi[10,11] = inv_logit(l10_0+l10_13);
        pi[10,12] = inv_logit(l10_0+l10_13);
        pi[10,13] = inv_logit(l10_0);
        pi[10,14] = inv_logit(l10_0+l10_13);
        pi[10,15] = inv_logit(l10_0+l10_13);
        pi[10,16] = inv_logit(l10_0+l10_13);
        pi[11,1] = inv_logit(l11_0);
        pi[11,2] = inv_logit(l11_0);
        pi[11,3] = inv_logit(l11_0);
        pi[11,4] = inv_logit(l11_0+l11_13);
        pi[11,5] = inv_logit(l11_0);
        pi[11,6] = inv_logit(l11_0);
        pi[11,7] = inv_logit(l11_0+l11_13);
        pi[11,8] = inv_logit(l11_0);
        pi[11,9] = inv_logit(l11_0+l11_13);
        pi[11,10] = inv_logit(l11_0);
        pi[11,11] = inv_logit(l11_0+l11_13);
        pi[11,12] = inv_logit(l11_0+l11_13);
        pi[11,13] = inv_logit(l11_0);
        pi[11,14] = inv_logit(l11_0+l11_13);
        pi[11,15] = inv_logit(l11_0+l11_13);
        pi[11,16] = inv_logit(l11_0+l11_13);
        pi[12,1] = inv_logit(l12_0);
        pi[12,2] = inv_logit(l12_0+l12_11);
        pi[12,3] = inv_logit(l12_0);
        pi[12,4] = inv_logit(l12_0);
        pi[12,5] = inv_logit(l12_0);
        pi[12,6] = inv_logit(l12_0+l12_11);
        pi[12,7] = inv_logit(l12_0+l12_11);
        pi[12,8] = inv_logit(l12_0+l12_11);
        pi[12,9] = inv_logit(l12_0);
        pi[12,10] = inv_logit(l12_0);
        pi[12,11] = inv_logit(l12_0);
        pi[12,12] = inv_logit(l12_0+l12_11);
        pi[12,13] = inv_logit(l12_0+l12_11);
        pi[12,14] = inv_logit(l12_0+l12_11);
        pi[12,15] = inv_logit(l12_0);
        pi[12,16] = inv_logit(l12_0+l12_11);
        pi[13,1] = inv_logit(l13_0);
        pi[13,2] = inv_logit(l13_0);
        pi[13,3] = inv_logit(l13_0);
        pi[13,4] = inv_logit(l13_0);
        pi[13,5] = inv_logit(l13_0+l13_14);
        pi[13,6] = inv_logit(l13_0);
        pi[13,7] = inv_logit(l13_0);
        pi[13,8] = inv_logit(l13_0+l13_14);
        pi[13,9] = inv_logit(l13_0);
        pi[13,10] = inv_logit(l13_0+l13_14);
        pi[13,11] = inv_logit(l13_0+l13_14);
        pi[13,12] = inv_logit(l13_0);
        pi[13,13] = inv_logit(l13_0+l13_14);
        pi[13,14] = inv_logit(l13_0+l13_14);
        pi[13,15] = inv_logit(l13_0+l13_14);
        pi[13,16] = inv_logit(l13_0+l13_14);
        pi[14,1] = inv_logit(l14_0);
        pi[14,2] = inv_logit(l14_0+l14_11);
        pi[14,3] = inv_logit(l14_0);
        pi[14,4] = inv_logit(l14_0);
        pi[14,5] = inv_logit(l14_0+l14_14);
        pi[14,6] = inv_logit(l14_0+l14_11);
        pi[14,7] = inv_logit(l14_0+l14_11);
        pi[14,8] = inv_logit(l14_0+l14_11+l14_14+l14_214);
        pi[14,9] = inv_logit(l14_0);
        pi[14,10] = inv_logit(l14_0+l14_14);
        pi[14,11] = inv_logit(l14_0+l14_14);
        pi[14,12] = inv_logit(l14_0+l14_11);
        pi[14,13] = inv_logit(l14_0+l14_11+l14_14+l14_214);
        pi[14,14] = inv_logit(l14_0+l14_11+l14_14+l14_214);
        pi[14,15] = inv_logit(l14_0+l14_14);
        pi[14,16] = inv_logit(l14_0+l14_11+l14_14+l14_214);
        pi[15,1] = inv_logit(l15_0);
        pi[15,2] = inv_logit(l15_0+l15_11);
        pi[15,3] = inv_logit(l15_0);
        pi[15,4] = inv_logit(l15_0);
        pi[15,5] = inv_logit(l15_0+l15_14);
        pi[15,6] = inv_logit(l15_0+l15_11);
        pi[15,7] = inv_logit(l15_0+l15_11);
        pi[15,8] = inv_logit(l15_0+l15_11+l15_14+l15_214);
        pi[15,9] = inv_logit(l15_0);
        pi[15,10] = inv_logit(l15_0+l15_14);
        pi[15,11] = inv_logit(l15_0+l15_14);
        pi[15,12] = inv_logit(l15_0+l15_11);
        pi[15,13] = inv_logit(l15_0+l15_11+l15_14+l15_214);
        pi[15,14] = inv_logit(l15_0+l15_11+l15_14+l15_214);
        pi[15,15] = inv_logit(l15_0+l15_14);
        pi[15,16] = inv_logit(l15_0+l15_11+l15_14+l15_214);
        pi[16,1] = inv_logit(l16_0);
        pi[16,2] = inv_logit(l16_0+l16_11);
        pi[16,3] = inv_logit(l16_0);
        pi[16,4] = inv_logit(l16_0);
        pi[16,5] = inv_logit(l16_0);
        pi[16,6] = inv_logit(l16_0+l16_11);
        pi[16,7] = inv_logit(l16_0+l16_11);
        pi[16,8] = inv_logit(l16_0+l16_11);
        pi[16,9] = inv_logit(l16_0);
        pi[16,10] = inv_logit(l16_0);
        pi[16,11] = inv_logit(l16_0);
        pi[16,12] = inv_logit(l16_0+l16_11);
        pi[16,13] = inv_logit(l16_0+l16_11);
        pi[16,14] = inv_logit(l16_0+l16_11);
        pi[16,15] = inv_logit(l16_0);
        pi[16,16] = inv_logit(l16_0+l16_11);
        pi[17,1] = inv_logit(l17_0);
        pi[17,2] = inv_logit(l17_0+l17_11);
        pi[17,3] = inv_logit(l17_0);
        pi[17,4] = inv_logit(l17_0);
        pi[17,5] = inv_logit(l17_0);
        pi[17,6] = inv_logit(l17_0+l17_11);
        pi[17,7] = inv_logit(l17_0+l17_11);
        pi[17,8] = inv_logit(l17_0+l17_11);
        pi[17,9] = inv_logit(l17_0);
        pi[17,10] = inv_logit(l17_0);
        pi[17,11] = inv_logit(l17_0);
        pi[17,12] = inv_logit(l17_0+l17_11);
        pi[17,13] = inv_logit(l17_0+l17_11);
        pi[17,14] = inv_logit(l17_0+l17_11);
        pi[17,15] = inv_logit(l17_0);
        pi[17,16] = inv_logit(l17_0+l17_11);
        pi[18,1] = inv_logit(l18_0);
        pi[18,2] = inv_logit(l18_0);
        pi[18,3] = inv_logit(l18_0+l18_12);
        pi[18,4] = inv_logit(l18_0);
        pi[18,5] = inv_logit(l18_0+l18_14);
        pi[18,6] = inv_logit(l18_0+l18_12);
        pi[18,7] = inv_logit(l18_0);
        pi[18,8] = inv_logit(l18_0+l18_14);
        pi[18,9] = inv_logit(l18_0+l18_12);
        pi[18,10] = inv_logit(l18_0+l18_12+l18_14+l18_224);
        pi[18,11] = inv_logit(l18_0+l18_14);
        pi[18,12] = inv_logit(l18_0+l18_12);
        pi[18,13] = inv_logit(l18_0+l18_12+l18_14+l18_224);
        pi[18,14] = inv_logit(l18_0+l18_14);
        pi[18,15] = inv_logit(l18_0+l18_12+l18_14+l18_224);
        pi[18,16] = inv_logit(l18_0+l18_12+l18_14+l18_224);
        pi[19,1] = inv_logit(l19_0);
        pi[19,2] = inv_logit(l19_0+l19_11);
        pi[19,3] = inv_logit(l19_0+l19_12);
        pi[19,4] = inv_logit(l19_0);
        pi[19,5] = inv_logit(l19_0);
        pi[19,6] = inv_logit(l19_0+l19_11+l19_12+l19_212);
        pi[19,7] = inv_logit(l19_0+l19_11);
        pi[19,8] = inv_logit(l19_0+l19_11);
        pi[19,9] = inv_logit(l19_0+l19_12);
        pi[19,10] = inv_logit(l19_0+l19_12);
        pi[19,11] = inv_logit(l19_0);
        pi[19,12] = inv_logit(l19_0+l19_11+l19_12+l19_212);
        pi[19,13] = inv_logit(l19_0+l19_11+l19_12+l19_212);
        pi[19,14] = inv_logit(l19_0+l19_11);
        pi[19,15] = inv_logit(l19_0+l19_12);
        pi[19,16] = inv_logit(l19_0+l19_11+l19_12+l19_212);
        pi[20,1] = inv_logit(l20_0);
        pi[20,2] = inv_logit(l20_0);
        pi[20,3] = inv_logit(l20_0+l20_12);
        pi[20,4] = inv_logit(l20_0);
        pi[20,5] = inv_logit(l20_0+l20_14);
        pi[20,6] = inv_logit(l20_0+l20_12);
        pi[20,7] = inv_logit(l20_0);
        pi[20,8] = inv_logit(l20_0+l20_14);
        pi[20,9] = inv_logit(l20_0+l20_12);
        pi[20,10] = inv_logit(l20_0+l20_12+l20_14+l20_224);
        pi[20,11] = inv_logit(l20_0+l20_14);
        pi[20,12] = inv_logit(l20_0+l20_12);
        pi[20,13] = inv_logit(l20_0+l20_12+l20_14+l20_224);
        pi[20,14] = inv_logit(l20_0+l20_14);
        pi[20,15] = inv_logit(l20_0+l20_12+l20_14+l20_224);
        pi[20,16] = inv_logit(l20_0+l20_12+l20_14+l20_224);
        pi[21,1] = inv_logit(l21_0);
        pi[21,2] = inv_logit(l21_0);
        pi[21,3] = inv_logit(l21_0+l21_12);
        pi[21,4] = inv_logit(l21_0);
        pi[21,5] = inv_logit(l21_0);
        pi[21,6] = inv_logit(l21_0+l21_12);
        pi[21,7] = inv_logit(l21_0);
        pi[21,8] = inv_logit(l21_0);
        pi[21,9] = inv_logit(l21_0+l21_12);
        pi[21,10] = inv_logit(l21_0+l21_12);
        pi[21,11] = inv_logit(l21_0);
        pi[21,12] = inv_logit(l21_0+l21_12);
        pi[21,13] = inv_logit(l21_0+l21_12);
        pi[21,14] = inv_logit(l21_0);
        pi[21,15] = inv_logit(l21_0+l21_12);
        pi[21,16] = inv_logit(l21_0+l21_12);
        pi[22,1] = inv_logit(l22_0);
        pi[22,2] = inv_logit(l22_0);
        pi[22,3] = inv_logit(l22_0+l22_12);
        pi[22,4] = inv_logit(l22_0);
        pi[22,5] = inv_logit(l22_0);
        pi[22,6] = inv_logit(l22_0+l22_12);
        pi[22,7] = inv_logit(l22_0);
        pi[22,8] = inv_logit(l22_0);
        pi[22,9] = inv_logit(l22_0+l22_12);
        pi[22,10] = inv_logit(l22_0+l22_12);
        pi[22,11] = inv_logit(l22_0);
        pi[22,12] = inv_logit(l22_0+l22_12);
        pi[22,13] = inv_logit(l22_0+l22_12);
        pi[22,14] = inv_logit(l22_0);
        pi[22,15] = inv_logit(l22_0+l22_12);
        pi[22,16] = inv_logit(l22_0+l22_12);
        pi[23,1] = inv_logit(l23_0);
        pi[23,2] = inv_logit(l23_0+l23_11);
        pi[23,3] = inv_logit(l23_0);
        pi[23,4] = inv_logit(l23_0);
        pi[23,5] = inv_logit(l23_0);
        pi[23,6] = inv_logit(l23_0+l23_11);
        pi[23,7] = inv_logit(l23_0+l23_11);
        pi[23,8] = inv_logit(l23_0+l23_11);
        pi[23,9] = inv_logit(l23_0);
        pi[23,10] = inv_logit(l23_0);
        pi[23,11] = inv_logit(l23_0);
        pi[23,12] = inv_logit(l23_0+l23_11);
        pi[23,13] = inv_logit(l23_0+l23_11);
        pi[23,14] = inv_logit(l23_0+l23_11);
        pi[23,15] = inv_logit(l23_0);
        pi[23,16] = inv_logit(l23_0+l23_11);
        pi[24,1] = inv_logit(l24_0);
        pi[24,2] = inv_logit(l24_0+l24_11);
        pi[24,3] = inv_logit(l24_0+l24_12);
        pi[24,4] = inv_logit(l24_0);
        pi[24,5] = inv_logit(l24_0);
        pi[24,6] = inv_logit(l24_0+l24_11+l24_12+l24_212);
        pi[24,7] = inv_logit(l24_0+l24_11);
        pi[24,8] = inv_logit(l24_0+l24_11);
        pi[24,9] = inv_logit(l24_0+l24_12);
        pi[24,10] = inv_logit(l24_0+l24_12);
        pi[24,11] = inv_logit(l24_0);
        pi[24,12] = inv_logit(l24_0+l24_11+l24_12+l24_212);
        pi[24,13] = inv_logit(l24_0+l24_11+l24_12+l24_212);
        pi[24,14] = inv_logit(l24_0+l24_11);
        pi[24,15] = inv_logit(l24_0+l24_12);
        pi[24,16] = inv_logit(l24_0+l24_11+l24_12+l24_212);
        pi[25,1] = inv_logit(l25_0);
        pi[25,2] = inv_logit(l25_0+l25_11);
        pi[25,3] = inv_logit(l25_0+l25_12);
        pi[25,4] = inv_logit(l25_0);
        pi[25,5] = inv_logit(l25_0);
        pi[25,6] = inv_logit(l25_0+l25_11+l25_12+l25_212);
        pi[25,7] = inv_logit(l25_0+l25_11);
        pi[25,8] = inv_logit(l25_0+l25_11);
        pi[25,9] = inv_logit(l25_0+l25_12);
        pi[25,10] = inv_logit(l25_0+l25_12);
        pi[25,11] = inv_logit(l25_0);
        pi[25,12] = inv_logit(l25_0+l25_11+l25_12+l25_212);
        pi[25,13] = inv_logit(l25_0+l25_11+l25_12+l25_212);
        pi[25,14] = inv_logit(l25_0+l25_11);
        pi[25,15] = inv_logit(l25_0+l25_12);
        pi[25,16] = inv_logit(l25_0+l25_11+l25_12+l25_212);
        pi[26,1] = inv_logit(l26_0);
        pi[26,2] = inv_logit(l26_0+l26_11);
        pi[26,3] = inv_logit(l26_0);
        pi[26,4] = inv_logit(l26_0);
        pi[26,5] = inv_logit(l26_0);
        pi[26,6] = inv_logit(l26_0+l26_11);
        pi[26,7] = inv_logit(l26_0+l26_11);
        pi[26,8] = inv_logit(l26_0+l26_11);
        pi[26,9] = inv_logit(l26_0);
        pi[26,10] = inv_logit(l26_0);
        pi[26,11] = inv_logit(l26_0);
        pi[26,12] = inv_logit(l26_0+l26_11);
        pi[26,13] = inv_logit(l26_0+l26_11);
        pi[26,14] = inv_logit(l26_0+l26_11);
        pi[26,15] = inv_logit(l26_0);
        pi[26,16] = inv_logit(l26_0+l26_11);
        pi[27,1] = inv_logit(l27_0);
        pi[27,2] = inv_logit(l27_0+l27_11);
        pi[27,3] = inv_logit(l27_0+l27_12);
        pi[27,4] = inv_logit(l27_0);
        pi[27,5] = inv_logit(l27_0);
        pi[27,6] = inv_logit(l27_0+l27_11+l27_12+l27_212);
        pi[27,7] = inv_logit(l27_0+l27_11);
        pi[27,8] = inv_logit(l27_0+l27_11);
        pi[27,9] = inv_logit(l27_0+l27_12);
        pi[27,10] = inv_logit(l27_0+l27_12);
        pi[27,11] = inv_logit(l27_0);
        pi[27,12] = inv_logit(l27_0+l27_11+l27_12+l27_212);
        pi[27,13] = inv_logit(l27_0+l27_11+l27_12+l27_212);
        pi[27,14] = inv_logit(l27_0+l27_11);
        pi[27,15] = inv_logit(l27_0+l27_12);
        pi[27,16] = inv_logit(l27_0+l27_11+l27_12+l27_212);
      }
      model {
        real ps[C];
      
        ////////////////////////////////// priors
        Vc ~ dirichlet(rep_vector(1, C));
        l1_0 ~ normal(0, 2);
        l1_11 ~ lognormal(0, 1);
        l2_0 ~ normal(0, 2);
        l2_13 ~ lognormal(0, 1);
        l3_0 ~ normal(0, 2);
        l3_12 ~ lognormal(0, 1);
        l4_0 ~ normal(0, 2);
        l4_11 ~ lognormal(0, 1);
        l5_0 ~ normal(0, 2);
        l5_11 ~ lognormal(0, 1);
        l6_0 ~ normal(0, 2);
        l6_12 ~ lognormal(0, 1);
        l7_0 ~ normal(0, 2);
        l7_11 ~ lognormal(0, 1);
        l8_0 ~ normal(0, 2);
        l8_13 ~ lognormal(0, 1);
        l9_0 ~ normal(0, 2);
        l9_13 ~ lognormal(0, 1);
        l10_0 ~ normal(0, 2);
        l10_13 ~ lognormal(0, 1);
        l11_0 ~ normal(0, 2);
        l11_13 ~ lognormal(0, 1);
        l12_0 ~ normal(0, 2);
        l12_11 ~ lognormal(0, 1);
        l13_0 ~ normal(0, 2);
        l13_14 ~ lognormal(0, 1);
        l14_0 ~ normal(0, 2);
        l14_11 ~ lognormal(0, 1);
        l14_14 ~ lognormal(0, 1);
        l14_214 ~ normal(0, 2);
        l15_0 ~ normal(0, 2);
        l15_11 ~ lognormal(0, 1);
        l15_14 ~ lognormal(0, 1);
        l15_214 ~ normal(0, 2);
        l16_0 ~ normal(0, 2);
        l16_11 ~ lognormal(0, 1);
        l17_0 ~ normal(0, 2);
        l17_11 ~ lognormal(0, 1);
        l18_0 ~ normal(0, 2);
        l18_12 ~ lognormal(0, 1);
        l18_14 ~ lognormal(0, 1);
        l18_224 ~ normal(0, 2);
        l19_0 ~ normal(0, 2);
        l19_11 ~ lognormal(0, 1);
        l19_12 ~ lognormal(0, 1);
        l19_212 ~ normal(0, 2);
        l20_0 ~ normal(0, 2);
        l20_12 ~ lognormal(0, 1);
        l20_14 ~ lognormal(0, 1);
        l20_224 ~ normal(0, 2);
        l21_0 ~ normal(0, 2);
        l21_12 ~ lognormal(0, 1);
        l22_0 ~ normal(0, 2);
        l22_12 ~ lognormal(0, 1);
        l23_0 ~ normal(0, 2);
        l23_11 ~ lognormal(0, 1);
        l24_0 ~ normal(0, 2);
        l24_11 ~ lognormal(0, 1);
        l24_12 ~ lognormal(0, 1);
        l24_212 ~ normal(0, 2);
        l25_0 ~ normal(0, 2);
        l25_11 ~ lognormal(0, 1);
        l25_12 ~ lognormal(0, 1);
        l25_212 ~ normal(0, 2);
        l26_0 ~ normal(0, 2);
        l26_11 ~ lognormal(0, 1);
        l27_0 ~ normal(0, 2);
        l27_11 ~ lognormal(0, 1);
        l27_12 ~ lognormal(0, 1);
        l27_212 ~ normal(0, 2);
      
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
      
      $prior
      # A tibble: 4 x 3
        class       coef  prior_def                  
        <chr>       <chr> <chr>                      
      1 intercept   <NA>  normal(0, 2)               
      2 maineffect  <NA>  lognormal(0, 1)            
      3 interaction <NA>  normal(0, 2)               
      4 structural  Vc    dirichlet(rep_vector(1, C))
      

---

    Code
      lcdm_script(ecpe_q, strc = "independent")
    Output
      $stancode
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
        real<lower=0,upper=1> eta[A];
      
        ////////////////////////////////// item intercepts
        real l1_0;
        real l2_0;
        real l3_0;
        real l4_0;
        real l5_0;
        real l6_0;
        real l7_0;
        real l8_0;
        real l9_0;
        real l10_0;
        real l11_0;
        real l12_0;
        real l13_0;
        real l14_0;
        real l15_0;
        real l16_0;
        real l17_0;
        real l18_0;
        real l19_0;
        real l20_0;
        real l21_0;
        real l22_0;
        real l23_0;
        real l24_0;
        real l25_0;
        real l26_0;
        real l27_0;
        real l28_0;
      
        ////////////////////////////////// item main effects
        real<lower=0> l1_11;
        real<lower=0> l1_12;
        real<lower=0> l2_12;
        real<lower=0> l3_11;
        real<lower=0> l3_13;
        real<lower=0> l4_13;
        real<lower=0> l5_13;
        real<lower=0> l6_13;
        real<lower=0> l7_11;
        real<lower=0> l7_13;
        real<lower=0> l8_12;
        real<lower=0> l9_13;
        real<lower=0> l10_11;
        real<lower=0> l11_11;
        real<lower=0> l11_13;
        real<lower=0> l12_11;
        real<lower=0> l12_13;
        real<lower=0> l13_11;
        real<lower=0> l14_11;
        real<lower=0> l15_13;
        real<lower=0> l16_11;
        real<lower=0> l16_13;
        real<lower=0> l17_12;
        real<lower=0> l17_13;
        real<lower=0> l18_13;
        real<lower=0> l19_13;
        real<lower=0> l20_11;
        real<lower=0> l20_13;
        real<lower=0> l21_11;
        real<lower=0> l21_13;
        real<lower=0> l22_13;
        real<lower=0> l23_12;
        real<lower=0> l24_12;
        real<lower=0> l25_11;
        real<lower=0> l26_13;
        real<lower=0> l27_11;
        real<lower=0> l28_13;
      
        ////////////////////////////////// item interactions
        real<lower=-1 * min([l1_11,l1_12])> l1_212;
        real<lower=-1 * min([l3_11,l3_13])> l3_213;
        real<lower=-1 * min([l7_11,l7_13])> l7_213;
        real<lower=-1 * min([l11_11,l11_13])> l11_213;
        real<lower=-1 * min([l12_11,l12_13])> l12_213;
        real<lower=-1 * min([l16_11,l16_13])> l16_213;
        real<lower=-1 * min([l17_12,l17_13])> l17_223;
        real<lower=-1 * min([l20_11,l20_13])> l20_213;
        real<lower=-1 * min([l21_11,l21_13])> l21_213;
      }
      transformed parameters {
        simplex[C] Vc;
        vector[C] log_Vc;
        for (c in 1:C) {
          Vc[c] = 1;
          for (a in 1:A) {
            Vc[c] = Vc[c] * eta[a]^Alpha[c,a] * 
                    (1 - eta[a]) ^ (1 - Alpha[c,a]);
          }
        }
        log_Vc = log(Vc);
        matrix[I,C] pi;
      
        ////////////////////////////////// probability of correct response
        pi[1,1] = inv_logit(l1_0);
        pi[1,2] = inv_logit(l1_0+l1_11);
        pi[1,3] = inv_logit(l1_0+l1_12);
        pi[1,4] = inv_logit(l1_0);
        pi[1,5] = inv_logit(l1_0+l1_11+l1_12+l1_212);
        pi[1,6] = inv_logit(l1_0+l1_11);
        pi[1,7] = inv_logit(l1_0+l1_12);
        pi[1,8] = inv_logit(l1_0+l1_11+l1_12+l1_212);
        pi[2,1] = inv_logit(l2_0);
        pi[2,2] = inv_logit(l2_0);
        pi[2,3] = inv_logit(l2_0+l2_12);
        pi[2,4] = inv_logit(l2_0);
        pi[2,5] = inv_logit(l2_0+l2_12);
        pi[2,6] = inv_logit(l2_0);
        pi[2,7] = inv_logit(l2_0+l2_12);
        pi[2,8] = inv_logit(l2_0+l2_12);
        pi[3,1] = inv_logit(l3_0);
        pi[3,2] = inv_logit(l3_0+l3_11);
        pi[3,3] = inv_logit(l3_0);
        pi[3,4] = inv_logit(l3_0+l3_13);
        pi[3,5] = inv_logit(l3_0+l3_11);
        pi[3,6] = inv_logit(l3_0+l3_11+l3_13+l3_213);
        pi[3,7] = inv_logit(l3_0+l3_13);
        pi[3,8] = inv_logit(l3_0+l3_11+l3_13+l3_213);
        pi[4,1] = inv_logit(l4_0);
        pi[4,2] = inv_logit(l4_0);
        pi[4,3] = inv_logit(l4_0);
        pi[4,4] = inv_logit(l4_0+l4_13);
        pi[4,5] = inv_logit(l4_0);
        pi[4,6] = inv_logit(l4_0+l4_13);
        pi[4,7] = inv_logit(l4_0+l4_13);
        pi[4,8] = inv_logit(l4_0+l4_13);
        pi[5,1] = inv_logit(l5_0);
        pi[5,2] = inv_logit(l5_0);
        pi[5,3] = inv_logit(l5_0);
        pi[5,4] = inv_logit(l5_0+l5_13);
        pi[5,5] = inv_logit(l5_0);
        pi[5,6] = inv_logit(l5_0+l5_13);
        pi[5,7] = inv_logit(l5_0+l5_13);
        pi[5,8] = inv_logit(l5_0+l5_13);
        pi[6,1] = inv_logit(l6_0);
        pi[6,2] = inv_logit(l6_0);
        pi[6,3] = inv_logit(l6_0);
        pi[6,4] = inv_logit(l6_0+l6_13);
        pi[6,5] = inv_logit(l6_0);
        pi[6,6] = inv_logit(l6_0+l6_13);
        pi[6,7] = inv_logit(l6_0+l6_13);
        pi[6,8] = inv_logit(l6_0+l6_13);
        pi[7,1] = inv_logit(l7_0);
        pi[7,2] = inv_logit(l7_0+l7_11);
        pi[7,3] = inv_logit(l7_0);
        pi[7,4] = inv_logit(l7_0+l7_13);
        pi[7,5] = inv_logit(l7_0+l7_11);
        pi[7,6] = inv_logit(l7_0+l7_11+l7_13+l7_213);
        pi[7,7] = inv_logit(l7_0+l7_13);
        pi[7,8] = inv_logit(l7_0+l7_11+l7_13+l7_213);
        pi[8,1] = inv_logit(l8_0);
        pi[8,2] = inv_logit(l8_0);
        pi[8,3] = inv_logit(l8_0+l8_12);
        pi[8,4] = inv_logit(l8_0);
        pi[8,5] = inv_logit(l8_0+l8_12);
        pi[8,6] = inv_logit(l8_0);
        pi[8,7] = inv_logit(l8_0+l8_12);
        pi[8,8] = inv_logit(l8_0+l8_12);
        pi[9,1] = inv_logit(l9_0);
        pi[9,2] = inv_logit(l9_0);
        pi[9,3] = inv_logit(l9_0);
        pi[9,4] = inv_logit(l9_0+l9_13);
        pi[9,5] = inv_logit(l9_0);
        pi[9,6] = inv_logit(l9_0+l9_13);
        pi[9,7] = inv_logit(l9_0+l9_13);
        pi[9,8] = inv_logit(l9_0+l9_13);
        pi[10,1] = inv_logit(l10_0);
        pi[10,2] = inv_logit(l10_0+l10_11);
        pi[10,3] = inv_logit(l10_0);
        pi[10,4] = inv_logit(l10_0);
        pi[10,5] = inv_logit(l10_0+l10_11);
        pi[10,6] = inv_logit(l10_0+l10_11);
        pi[10,7] = inv_logit(l10_0);
        pi[10,8] = inv_logit(l10_0+l10_11);
        pi[11,1] = inv_logit(l11_0);
        pi[11,2] = inv_logit(l11_0+l11_11);
        pi[11,3] = inv_logit(l11_0);
        pi[11,4] = inv_logit(l11_0+l11_13);
        pi[11,5] = inv_logit(l11_0+l11_11);
        pi[11,6] = inv_logit(l11_0+l11_11+l11_13+l11_213);
        pi[11,7] = inv_logit(l11_0+l11_13);
        pi[11,8] = inv_logit(l11_0+l11_11+l11_13+l11_213);
        pi[12,1] = inv_logit(l12_0);
        pi[12,2] = inv_logit(l12_0+l12_11);
        pi[12,3] = inv_logit(l12_0);
        pi[12,4] = inv_logit(l12_0+l12_13);
        pi[12,5] = inv_logit(l12_0+l12_11);
        pi[12,6] = inv_logit(l12_0+l12_11+l12_13+l12_213);
        pi[12,7] = inv_logit(l12_0+l12_13);
        pi[12,8] = inv_logit(l12_0+l12_11+l12_13+l12_213);
        pi[13,1] = inv_logit(l13_0);
        pi[13,2] = inv_logit(l13_0+l13_11);
        pi[13,3] = inv_logit(l13_0);
        pi[13,4] = inv_logit(l13_0);
        pi[13,5] = inv_logit(l13_0+l13_11);
        pi[13,6] = inv_logit(l13_0+l13_11);
        pi[13,7] = inv_logit(l13_0);
        pi[13,8] = inv_logit(l13_0+l13_11);
        pi[14,1] = inv_logit(l14_0);
        pi[14,2] = inv_logit(l14_0+l14_11);
        pi[14,3] = inv_logit(l14_0);
        pi[14,4] = inv_logit(l14_0);
        pi[14,5] = inv_logit(l14_0+l14_11);
        pi[14,6] = inv_logit(l14_0+l14_11);
        pi[14,7] = inv_logit(l14_0);
        pi[14,8] = inv_logit(l14_0+l14_11);
        pi[15,1] = inv_logit(l15_0);
        pi[15,2] = inv_logit(l15_0);
        pi[15,3] = inv_logit(l15_0);
        pi[15,4] = inv_logit(l15_0+l15_13);
        pi[15,5] = inv_logit(l15_0);
        pi[15,6] = inv_logit(l15_0+l15_13);
        pi[15,7] = inv_logit(l15_0+l15_13);
        pi[15,8] = inv_logit(l15_0+l15_13);
        pi[16,1] = inv_logit(l16_0);
        pi[16,2] = inv_logit(l16_0+l16_11);
        pi[16,3] = inv_logit(l16_0);
        pi[16,4] = inv_logit(l16_0+l16_13);
        pi[16,5] = inv_logit(l16_0+l16_11);
        pi[16,6] = inv_logit(l16_0+l16_11+l16_13+l16_213);
        pi[16,7] = inv_logit(l16_0+l16_13);
        pi[16,8] = inv_logit(l16_0+l16_11+l16_13+l16_213);
        pi[17,1] = inv_logit(l17_0);
        pi[17,2] = inv_logit(l17_0);
        pi[17,3] = inv_logit(l17_0+l17_12);
        pi[17,4] = inv_logit(l17_0+l17_13);
        pi[17,5] = inv_logit(l17_0+l17_12);
        pi[17,6] = inv_logit(l17_0+l17_13);
        pi[17,7] = inv_logit(l17_0+l17_12+l17_13+l17_223);
        pi[17,8] = inv_logit(l17_0+l17_12+l17_13+l17_223);
        pi[18,1] = inv_logit(l18_0);
        pi[18,2] = inv_logit(l18_0);
        pi[18,3] = inv_logit(l18_0);
        pi[18,4] = inv_logit(l18_0+l18_13);
        pi[18,5] = inv_logit(l18_0);
        pi[18,6] = inv_logit(l18_0+l18_13);
        pi[18,7] = inv_logit(l18_0+l18_13);
        pi[18,8] = inv_logit(l18_0+l18_13);
        pi[19,1] = inv_logit(l19_0);
        pi[19,2] = inv_logit(l19_0);
        pi[19,3] = inv_logit(l19_0);
        pi[19,4] = inv_logit(l19_0+l19_13);
        pi[19,5] = inv_logit(l19_0);
        pi[19,6] = inv_logit(l19_0+l19_13);
        pi[19,7] = inv_logit(l19_0+l19_13);
        pi[19,8] = inv_logit(l19_0+l19_13);
        pi[20,1] = inv_logit(l20_0);
        pi[20,2] = inv_logit(l20_0+l20_11);
        pi[20,3] = inv_logit(l20_0);
        pi[20,4] = inv_logit(l20_0+l20_13);
        pi[20,5] = inv_logit(l20_0+l20_11);
        pi[20,6] = inv_logit(l20_0+l20_11+l20_13+l20_213);
        pi[20,7] = inv_logit(l20_0+l20_13);
        pi[20,8] = inv_logit(l20_0+l20_11+l20_13+l20_213);
        pi[21,1] = inv_logit(l21_0);
        pi[21,2] = inv_logit(l21_0+l21_11);
        pi[21,3] = inv_logit(l21_0);
        pi[21,4] = inv_logit(l21_0+l21_13);
        pi[21,5] = inv_logit(l21_0+l21_11);
        pi[21,6] = inv_logit(l21_0+l21_11+l21_13+l21_213);
        pi[21,7] = inv_logit(l21_0+l21_13);
        pi[21,8] = inv_logit(l21_0+l21_11+l21_13+l21_213);
        pi[22,1] = inv_logit(l22_0);
        pi[22,2] = inv_logit(l22_0);
        pi[22,3] = inv_logit(l22_0);
        pi[22,4] = inv_logit(l22_0+l22_13);
        pi[22,5] = inv_logit(l22_0);
        pi[22,6] = inv_logit(l22_0+l22_13);
        pi[22,7] = inv_logit(l22_0+l22_13);
        pi[22,8] = inv_logit(l22_0+l22_13);
        pi[23,1] = inv_logit(l23_0);
        pi[23,2] = inv_logit(l23_0);
        pi[23,3] = inv_logit(l23_0+l23_12);
        pi[23,4] = inv_logit(l23_0);
        pi[23,5] = inv_logit(l23_0+l23_12);
        pi[23,6] = inv_logit(l23_0);
        pi[23,7] = inv_logit(l23_0+l23_12);
        pi[23,8] = inv_logit(l23_0+l23_12);
        pi[24,1] = inv_logit(l24_0);
        pi[24,2] = inv_logit(l24_0);
        pi[24,3] = inv_logit(l24_0+l24_12);
        pi[24,4] = inv_logit(l24_0);
        pi[24,5] = inv_logit(l24_0+l24_12);
        pi[24,6] = inv_logit(l24_0);
        pi[24,7] = inv_logit(l24_0+l24_12);
        pi[24,8] = inv_logit(l24_0+l24_12);
        pi[25,1] = inv_logit(l25_0);
        pi[25,2] = inv_logit(l25_0+l25_11);
        pi[25,3] = inv_logit(l25_0);
        pi[25,4] = inv_logit(l25_0);
        pi[25,5] = inv_logit(l25_0+l25_11);
        pi[25,6] = inv_logit(l25_0+l25_11);
        pi[25,7] = inv_logit(l25_0);
        pi[25,8] = inv_logit(l25_0+l25_11);
        pi[26,1] = inv_logit(l26_0);
        pi[26,2] = inv_logit(l26_0);
        pi[26,3] = inv_logit(l26_0);
        pi[26,4] = inv_logit(l26_0+l26_13);
        pi[26,5] = inv_logit(l26_0);
        pi[26,6] = inv_logit(l26_0+l26_13);
        pi[26,7] = inv_logit(l26_0+l26_13);
        pi[26,8] = inv_logit(l26_0+l26_13);
        pi[27,1] = inv_logit(l27_0);
        pi[27,2] = inv_logit(l27_0+l27_11);
        pi[27,3] = inv_logit(l27_0);
        pi[27,4] = inv_logit(l27_0);
        pi[27,5] = inv_logit(l27_0+l27_11);
        pi[27,6] = inv_logit(l27_0+l27_11);
        pi[27,7] = inv_logit(l27_0);
        pi[27,8] = inv_logit(l27_0+l27_11);
        pi[28,1] = inv_logit(l28_0);
        pi[28,2] = inv_logit(l28_0);
        pi[28,3] = inv_logit(l28_0);
        pi[28,4] = inv_logit(l28_0+l28_13);
        pi[28,5] = inv_logit(l28_0);
        pi[28,6] = inv_logit(l28_0+l28_13);
        pi[28,7] = inv_logit(l28_0+l28_13);
        pi[28,8] = inv_logit(l28_0+l28_13);
      }
      model {
        real ps[C];
      
        ////////////////////////////////// priors
        eta[1] ~ beta(1, 1);
        eta[2] ~ beta(1, 1);
        eta[3] ~ beta(1, 1);
        l1_0 ~ normal(0, 2);
        l1_11 ~ lognormal(0, 1);
        l1_12 ~ lognormal(0, 1);
        l1_212 ~ normal(0, 2);
        l2_0 ~ normal(0, 2);
        l2_12 ~ lognormal(0, 1);
        l3_0 ~ normal(0, 2);
        l3_11 ~ lognormal(0, 1);
        l3_13 ~ lognormal(0, 1);
        l3_213 ~ normal(0, 2);
        l4_0 ~ normal(0, 2);
        l4_13 ~ lognormal(0, 1);
        l5_0 ~ normal(0, 2);
        l5_13 ~ lognormal(0, 1);
        l6_0 ~ normal(0, 2);
        l6_13 ~ lognormal(0, 1);
        l7_0 ~ normal(0, 2);
        l7_11 ~ lognormal(0, 1);
        l7_13 ~ lognormal(0, 1);
        l7_213 ~ normal(0, 2);
        l8_0 ~ normal(0, 2);
        l8_12 ~ lognormal(0, 1);
        l9_0 ~ normal(0, 2);
        l9_13 ~ lognormal(0, 1);
        l10_0 ~ normal(0, 2);
        l10_11 ~ lognormal(0, 1);
        l11_0 ~ normal(0, 2);
        l11_11 ~ lognormal(0, 1);
        l11_13 ~ lognormal(0, 1);
        l11_213 ~ normal(0, 2);
        l12_0 ~ normal(0, 2);
        l12_11 ~ lognormal(0, 1);
        l12_13 ~ lognormal(0, 1);
        l12_213 ~ normal(0, 2);
        l13_0 ~ normal(0, 2);
        l13_11 ~ lognormal(0, 1);
        l14_0 ~ normal(0, 2);
        l14_11 ~ lognormal(0, 1);
        l15_0 ~ normal(0, 2);
        l15_13 ~ lognormal(0, 1);
        l16_0 ~ normal(0, 2);
        l16_11 ~ lognormal(0, 1);
        l16_13 ~ lognormal(0, 1);
        l16_213 ~ normal(0, 2);
        l17_0 ~ normal(0, 2);
        l17_12 ~ lognormal(0, 1);
        l17_13 ~ lognormal(0, 1);
        l17_223 ~ normal(0, 2);
        l18_0 ~ normal(0, 2);
        l18_13 ~ lognormal(0, 1);
        l19_0 ~ normal(0, 2);
        l19_13 ~ lognormal(0, 1);
        l20_0 ~ normal(0, 2);
        l20_11 ~ lognormal(0, 1);
        l20_13 ~ lognormal(0, 1);
        l20_213 ~ normal(0, 2);
        l21_0 ~ normal(0, 2);
        l21_11 ~ lognormal(0, 1);
        l21_13 ~ lognormal(0, 1);
        l21_213 ~ normal(0, 2);
        l22_0 ~ normal(0, 2);
        l22_13 ~ lognormal(0, 1);
        l23_0 ~ normal(0, 2);
        l23_12 ~ lognormal(0, 1);
        l24_0 ~ normal(0, 2);
        l24_12 ~ lognormal(0, 1);
        l25_0 ~ normal(0, 2);
        l25_11 ~ lognormal(0, 1);
        l26_0 ~ normal(0, 2);
        l26_13 ~ lognormal(0, 1);
        l27_0 ~ normal(0, 2);
        l27_11 ~ lognormal(0, 1);
        l28_0 ~ normal(0, 2);
        l28_13 ~ lognormal(0, 1);
      
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
      
      $prior
      # A tibble: 4 x 3
        class       coef  prior_def      
        <chr>       <chr> <chr>          
      1 intercept   <NA>  normal(0, 2)   
      2 maineffect  <NA>  lognormal(0, 1)
      3 interaction <NA>  normal(0, 2)   
      4 structural  <NA>  beta(1, 1)     
      

# crum script works

    Code
      crum_script(ecpe_q)
    Output
      $stancode
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
        real l5_0;
        real l6_0;
        real l7_0;
        real l8_0;
        real l9_0;
        real l10_0;
        real l11_0;
        real l12_0;
        real l13_0;
        real l14_0;
        real l15_0;
        real l16_0;
        real l17_0;
        real l18_0;
        real l19_0;
        real l20_0;
        real l21_0;
        real l22_0;
        real l23_0;
        real l24_0;
        real l25_0;
        real l26_0;
        real l27_0;
        real l28_0;
      
        ////////////////////////////////// item main effects
        real<lower=0> l1_11;
        real<lower=0> l1_12;
        real<lower=0> l2_12;
        real<lower=0> l3_11;
        real<lower=0> l3_13;
        real<lower=0> l4_13;
        real<lower=0> l5_13;
        real<lower=0> l6_13;
        real<lower=0> l7_11;
        real<lower=0> l7_13;
        real<lower=0> l8_12;
        real<lower=0> l9_13;
        real<lower=0> l10_11;
        real<lower=0> l11_11;
        real<lower=0> l11_13;
        real<lower=0> l12_11;
        real<lower=0> l12_13;
        real<lower=0> l13_11;
        real<lower=0> l14_11;
        real<lower=0> l15_13;
        real<lower=0> l16_11;
        real<lower=0> l16_13;
        real<lower=0> l17_12;
        real<lower=0> l17_13;
        real<lower=0> l18_13;
        real<lower=0> l19_13;
        real<lower=0> l20_11;
        real<lower=0> l20_13;
        real<lower=0> l21_11;
        real<lower=0> l21_13;
        real<lower=0> l22_13;
        real<lower=0> l23_12;
        real<lower=0> l24_12;
        real<lower=0> l25_11;
        real<lower=0> l26_13;
        real<lower=0> l27_11;
        real<lower=0> l28_13;
      }
      transformed parameters {
        vector[C] log_Vc = log(Vc);
        matrix[I,C] pi;
      
        ////////////////////////////////// probability of correct response
        pi[1,1] = inv_logit(l1_0);
        pi[1,2] = inv_logit(l1_0+l1_11);
        pi[1,3] = inv_logit(l1_0+l1_12);
        pi[1,4] = inv_logit(l1_0);
        pi[1,5] = inv_logit(l1_0+l1_11+l1_12);
        pi[1,6] = inv_logit(l1_0+l1_11);
        pi[1,7] = inv_logit(l1_0+l1_12);
        pi[1,8] = inv_logit(l1_0+l1_11+l1_12);
        pi[2,1] = inv_logit(l2_0);
        pi[2,2] = inv_logit(l2_0);
        pi[2,3] = inv_logit(l2_0+l2_12);
        pi[2,4] = inv_logit(l2_0);
        pi[2,5] = inv_logit(l2_0+l2_12);
        pi[2,6] = inv_logit(l2_0);
        pi[2,7] = inv_logit(l2_0+l2_12);
        pi[2,8] = inv_logit(l2_0+l2_12);
        pi[3,1] = inv_logit(l3_0);
        pi[3,2] = inv_logit(l3_0+l3_11);
        pi[3,3] = inv_logit(l3_0);
        pi[3,4] = inv_logit(l3_0+l3_13);
        pi[3,5] = inv_logit(l3_0+l3_11);
        pi[3,6] = inv_logit(l3_0+l3_11+l3_13);
        pi[3,7] = inv_logit(l3_0+l3_13);
        pi[3,8] = inv_logit(l3_0+l3_11+l3_13);
        pi[4,1] = inv_logit(l4_0);
        pi[4,2] = inv_logit(l4_0);
        pi[4,3] = inv_logit(l4_0);
        pi[4,4] = inv_logit(l4_0+l4_13);
        pi[4,5] = inv_logit(l4_0);
        pi[4,6] = inv_logit(l4_0+l4_13);
        pi[4,7] = inv_logit(l4_0+l4_13);
        pi[4,8] = inv_logit(l4_0+l4_13);
        pi[5,1] = inv_logit(l5_0);
        pi[5,2] = inv_logit(l5_0);
        pi[5,3] = inv_logit(l5_0);
        pi[5,4] = inv_logit(l5_0+l5_13);
        pi[5,5] = inv_logit(l5_0);
        pi[5,6] = inv_logit(l5_0+l5_13);
        pi[5,7] = inv_logit(l5_0+l5_13);
        pi[5,8] = inv_logit(l5_0+l5_13);
        pi[6,1] = inv_logit(l6_0);
        pi[6,2] = inv_logit(l6_0);
        pi[6,3] = inv_logit(l6_0);
        pi[6,4] = inv_logit(l6_0+l6_13);
        pi[6,5] = inv_logit(l6_0);
        pi[6,6] = inv_logit(l6_0+l6_13);
        pi[6,7] = inv_logit(l6_0+l6_13);
        pi[6,8] = inv_logit(l6_0+l6_13);
        pi[7,1] = inv_logit(l7_0);
        pi[7,2] = inv_logit(l7_0+l7_11);
        pi[7,3] = inv_logit(l7_0);
        pi[7,4] = inv_logit(l7_0+l7_13);
        pi[7,5] = inv_logit(l7_0+l7_11);
        pi[7,6] = inv_logit(l7_0+l7_11+l7_13);
        pi[7,7] = inv_logit(l7_0+l7_13);
        pi[7,8] = inv_logit(l7_0+l7_11+l7_13);
        pi[8,1] = inv_logit(l8_0);
        pi[8,2] = inv_logit(l8_0);
        pi[8,3] = inv_logit(l8_0+l8_12);
        pi[8,4] = inv_logit(l8_0);
        pi[8,5] = inv_logit(l8_0+l8_12);
        pi[8,6] = inv_logit(l8_0);
        pi[8,7] = inv_logit(l8_0+l8_12);
        pi[8,8] = inv_logit(l8_0+l8_12);
        pi[9,1] = inv_logit(l9_0);
        pi[9,2] = inv_logit(l9_0);
        pi[9,3] = inv_logit(l9_0);
        pi[9,4] = inv_logit(l9_0+l9_13);
        pi[9,5] = inv_logit(l9_0);
        pi[9,6] = inv_logit(l9_0+l9_13);
        pi[9,7] = inv_logit(l9_0+l9_13);
        pi[9,8] = inv_logit(l9_0+l9_13);
        pi[10,1] = inv_logit(l10_0);
        pi[10,2] = inv_logit(l10_0+l10_11);
        pi[10,3] = inv_logit(l10_0);
        pi[10,4] = inv_logit(l10_0);
        pi[10,5] = inv_logit(l10_0+l10_11);
        pi[10,6] = inv_logit(l10_0+l10_11);
        pi[10,7] = inv_logit(l10_0);
        pi[10,8] = inv_logit(l10_0+l10_11);
        pi[11,1] = inv_logit(l11_0);
        pi[11,2] = inv_logit(l11_0+l11_11);
        pi[11,3] = inv_logit(l11_0);
        pi[11,4] = inv_logit(l11_0+l11_13);
        pi[11,5] = inv_logit(l11_0+l11_11);
        pi[11,6] = inv_logit(l11_0+l11_11+l11_13);
        pi[11,7] = inv_logit(l11_0+l11_13);
        pi[11,8] = inv_logit(l11_0+l11_11+l11_13);
        pi[12,1] = inv_logit(l12_0);
        pi[12,2] = inv_logit(l12_0+l12_11);
        pi[12,3] = inv_logit(l12_0);
        pi[12,4] = inv_logit(l12_0+l12_13);
        pi[12,5] = inv_logit(l12_0+l12_11);
        pi[12,6] = inv_logit(l12_0+l12_11+l12_13);
        pi[12,7] = inv_logit(l12_0+l12_13);
        pi[12,8] = inv_logit(l12_0+l12_11+l12_13);
        pi[13,1] = inv_logit(l13_0);
        pi[13,2] = inv_logit(l13_0+l13_11);
        pi[13,3] = inv_logit(l13_0);
        pi[13,4] = inv_logit(l13_0);
        pi[13,5] = inv_logit(l13_0+l13_11);
        pi[13,6] = inv_logit(l13_0+l13_11);
        pi[13,7] = inv_logit(l13_0);
        pi[13,8] = inv_logit(l13_0+l13_11);
        pi[14,1] = inv_logit(l14_0);
        pi[14,2] = inv_logit(l14_0+l14_11);
        pi[14,3] = inv_logit(l14_0);
        pi[14,4] = inv_logit(l14_0);
        pi[14,5] = inv_logit(l14_0+l14_11);
        pi[14,6] = inv_logit(l14_0+l14_11);
        pi[14,7] = inv_logit(l14_0);
        pi[14,8] = inv_logit(l14_0+l14_11);
        pi[15,1] = inv_logit(l15_0);
        pi[15,2] = inv_logit(l15_0);
        pi[15,3] = inv_logit(l15_0);
        pi[15,4] = inv_logit(l15_0+l15_13);
        pi[15,5] = inv_logit(l15_0);
        pi[15,6] = inv_logit(l15_0+l15_13);
        pi[15,7] = inv_logit(l15_0+l15_13);
        pi[15,8] = inv_logit(l15_0+l15_13);
        pi[16,1] = inv_logit(l16_0);
        pi[16,2] = inv_logit(l16_0+l16_11);
        pi[16,3] = inv_logit(l16_0);
        pi[16,4] = inv_logit(l16_0+l16_13);
        pi[16,5] = inv_logit(l16_0+l16_11);
        pi[16,6] = inv_logit(l16_0+l16_11+l16_13);
        pi[16,7] = inv_logit(l16_0+l16_13);
        pi[16,8] = inv_logit(l16_0+l16_11+l16_13);
        pi[17,1] = inv_logit(l17_0);
        pi[17,2] = inv_logit(l17_0);
        pi[17,3] = inv_logit(l17_0+l17_12);
        pi[17,4] = inv_logit(l17_0+l17_13);
        pi[17,5] = inv_logit(l17_0+l17_12);
        pi[17,6] = inv_logit(l17_0+l17_13);
        pi[17,7] = inv_logit(l17_0+l17_12+l17_13);
        pi[17,8] = inv_logit(l17_0+l17_12+l17_13);
        pi[18,1] = inv_logit(l18_0);
        pi[18,2] = inv_logit(l18_0);
        pi[18,3] = inv_logit(l18_0);
        pi[18,4] = inv_logit(l18_0+l18_13);
        pi[18,5] = inv_logit(l18_0);
        pi[18,6] = inv_logit(l18_0+l18_13);
        pi[18,7] = inv_logit(l18_0+l18_13);
        pi[18,8] = inv_logit(l18_0+l18_13);
        pi[19,1] = inv_logit(l19_0);
        pi[19,2] = inv_logit(l19_0);
        pi[19,3] = inv_logit(l19_0);
        pi[19,4] = inv_logit(l19_0+l19_13);
        pi[19,5] = inv_logit(l19_0);
        pi[19,6] = inv_logit(l19_0+l19_13);
        pi[19,7] = inv_logit(l19_0+l19_13);
        pi[19,8] = inv_logit(l19_0+l19_13);
        pi[20,1] = inv_logit(l20_0);
        pi[20,2] = inv_logit(l20_0+l20_11);
        pi[20,3] = inv_logit(l20_0);
        pi[20,4] = inv_logit(l20_0+l20_13);
        pi[20,5] = inv_logit(l20_0+l20_11);
        pi[20,6] = inv_logit(l20_0+l20_11+l20_13);
        pi[20,7] = inv_logit(l20_0+l20_13);
        pi[20,8] = inv_logit(l20_0+l20_11+l20_13);
        pi[21,1] = inv_logit(l21_0);
        pi[21,2] = inv_logit(l21_0+l21_11);
        pi[21,3] = inv_logit(l21_0);
        pi[21,4] = inv_logit(l21_0+l21_13);
        pi[21,5] = inv_logit(l21_0+l21_11);
        pi[21,6] = inv_logit(l21_0+l21_11+l21_13);
        pi[21,7] = inv_logit(l21_0+l21_13);
        pi[21,8] = inv_logit(l21_0+l21_11+l21_13);
        pi[22,1] = inv_logit(l22_0);
        pi[22,2] = inv_logit(l22_0);
        pi[22,3] = inv_logit(l22_0);
        pi[22,4] = inv_logit(l22_0+l22_13);
        pi[22,5] = inv_logit(l22_0);
        pi[22,6] = inv_logit(l22_0+l22_13);
        pi[22,7] = inv_logit(l22_0+l22_13);
        pi[22,8] = inv_logit(l22_0+l22_13);
        pi[23,1] = inv_logit(l23_0);
        pi[23,2] = inv_logit(l23_0);
        pi[23,3] = inv_logit(l23_0+l23_12);
        pi[23,4] = inv_logit(l23_0);
        pi[23,5] = inv_logit(l23_0+l23_12);
        pi[23,6] = inv_logit(l23_0);
        pi[23,7] = inv_logit(l23_0+l23_12);
        pi[23,8] = inv_logit(l23_0+l23_12);
        pi[24,1] = inv_logit(l24_0);
        pi[24,2] = inv_logit(l24_0);
        pi[24,3] = inv_logit(l24_0+l24_12);
        pi[24,4] = inv_logit(l24_0);
        pi[24,5] = inv_logit(l24_0+l24_12);
        pi[24,6] = inv_logit(l24_0);
        pi[24,7] = inv_logit(l24_0+l24_12);
        pi[24,8] = inv_logit(l24_0+l24_12);
        pi[25,1] = inv_logit(l25_0);
        pi[25,2] = inv_logit(l25_0+l25_11);
        pi[25,3] = inv_logit(l25_0);
        pi[25,4] = inv_logit(l25_0);
        pi[25,5] = inv_logit(l25_0+l25_11);
        pi[25,6] = inv_logit(l25_0+l25_11);
        pi[25,7] = inv_logit(l25_0);
        pi[25,8] = inv_logit(l25_0+l25_11);
        pi[26,1] = inv_logit(l26_0);
        pi[26,2] = inv_logit(l26_0);
        pi[26,3] = inv_logit(l26_0);
        pi[26,4] = inv_logit(l26_0+l26_13);
        pi[26,5] = inv_logit(l26_0);
        pi[26,6] = inv_logit(l26_0+l26_13);
        pi[26,7] = inv_logit(l26_0+l26_13);
        pi[26,8] = inv_logit(l26_0+l26_13);
        pi[27,1] = inv_logit(l27_0);
        pi[27,2] = inv_logit(l27_0+l27_11);
        pi[27,3] = inv_logit(l27_0);
        pi[27,4] = inv_logit(l27_0);
        pi[27,5] = inv_logit(l27_0+l27_11);
        pi[27,6] = inv_logit(l27_0+l27_11);
        pi[27,7] = inv_logit(l27_0);
        pi[27,8] = inv_logit(l27_0+l27_11);
        pi[28,1] = inv_logit(l28_0);
        pi[28,2] = inv_logit(l28_0);
        pi[28,3] = inv_logit(l28_0);
        pi[28,4] = inv_logit(l28_0+l28_13);
        pi[28,5] = inv_logit(l28_0);
        pi[28,6] = inv_logit(l28_0+l28_13);
        pi[28,7] = inv_logit(l28_0+l28_13);
        pi[28,8] = inv_logit(l28_0+l28_13);
      }
      model {
        real ps[C];
      
        ////////////////////////////////// priors
        Vc ~ dirichlet(rep_vector(1, C));
        l1_0 ~ normal(0, 2);
        l1_11 ~ lognormal(0, 1);
        l1_12 ~ lognormal(0, 1);
        l2_0 ~ normal(0, 2);
        l2_12 ~ lognormal(0, 1);
        l3_0 ~ normal(0, 2);
        l3_11 ~ lognormal(0, 1);
        l3_13 ~ lognormal(0, 1);
        l4_0 ~ normal(0, 2);
        l4_13 ~ lognormal(0, 1);
        l5_0 ~ normal(0, 2);
        l5_13 ~ lognormal(0, 1);
        l6_0 ~ normal(0, 2);
        l6_13 ~ lognormal(0, 1);
        l7_0 ~ normal(0, 2);
        l7_11 ~ lognormal(0, 1);
        l7_13 ~ lognormal(0, 1);
        l8_0 ~ normal(0, 2);
        l8_12 ~ lognormal(0, 1);
        l9_0 ~ normal(0, 2);
        l9_13 ~ lognormal(0, 1);
        l10_0 ~ normal(0, 2);
        l10_11 ~ lognormal(0, 1);
        l11_0 ~ normal(0, 2);
        l11_11 ~ lognormal(0, 1);
        l11_13 ~ lognormal(0, 1);
        l12_0 ~ normal(0, 2);
        l12_11 ~ lognormal(0, 1);
        l12_13 ~ lognormal(0, 1);
        l13_0 ~ normal(0, 2);
        l13_11 ~ lognormal(0, 1);
        l14_0 ~ normal(0, 2);
        l14_11 ~ lognormal(0, 1);
        l15_0 ~ normal(0, 2);
        l15_13 ~ lognormal(0, 1);
        l16_0 ~ normal(0, 2);
        l16_11 ~ lognormal(0, 1);
        l16_13 ~ lognormal(0, 1);
        l17_0 ~ normal(0, 2);
        l17_12 ~ lognormal(0, 1);
        l17_13 ~ lognormal(0, 1);
        l18_0 ~ normal(0, 2);
        l18_13 ~ lognormal(0, 1);
        l19_0 ~ normal(0, 2);
        l19_13 ~ lognormal(0, 1);
        l20_0 ~ normal(0, 2);
        l20_11 ~ lognormal(0, 1);
        l20_13 ~ lognormal(0, 1);
        l21_0 ~ normal(0, 2);
        l21_11 ~ lognormal(0, 1);
        l21_13 ~ lognormal(0, 1);
        l22_0 ~ normal(0, 2);
        l22_13 ~ lognormal(0, 1);
        l23_0 ~ normal(0, 2);
        l23_12 ~ lognormal(0, 1);
        l24_0 ~ normal(0, 2);
        l24_12 ~ lognormal(0, 1);
        l25_0 ~ normal(0, 2);
        l25_11 ~ lognormal(0, 1);
        l26_0 ~ normal(0, 2);
        l26_13 ~ lognormal(0, 1);
        l27_0 ~ normal(0, 2);
        l27_11 ~ lognormal(0, 1);
        l28_0 ~ normal(0, 2);
        l28_13 ~ lognormal(0, 1);
      
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
      
      $prior
      # A tibble: 3 x 3
        class      coef  prior_def                  
        <chr>      <chr> <chr>                      
      1 intercept  <NA>  normal(0, 2)               
      2 maineffect <NA>  lognormal(0, 1)            
      3 structural Vc    dirichlet(rep_vector(1, C))
      

---

    Code
      crum_script(mdm_q)
    Output
      $stancode
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
        Vc ~ dirichlet(rep_vector(1, C));
        l1_0 ~ normal(0, 2);
        l1_11 ~ lognormal(0, 1);
        l2_0 ~ normal(0, 2);
        l2_11 ~ lognormal(0, 1);
        l3_0 ~ normal(0, 2);
        l3_11 ~ lognormal(0, 1);
        l4_0 ~ normal(0, 2);
        l4_11 ~ lognormal(0, 1);
      
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
      
      $prior
      # A tibble: 3 x 3
        class      coef  prior_def                  
        <chr>      <chr> <chr>                      
      1 intercept  <NA>  normal(0, 2)               
      2 maineffect <NA>  lognormal(0, 1)            
      3 structural Vc    dirichlet(rep_vector(1, C))
      

---

    Code
      crum_script(dtmr_q)
    Output
      $stancode
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
        real l5_0;
        real l6_0;
        real l7_0;
        real l8_0;
        real l9_0;
        real l10_0;
        real l11_0;
        real l12_0;
        real l13_0;
        real l14_0;
        real l15_0;
        real l16_0;
        real l17_0;
        real l18_0;
        real l19_0;
        real l20_0;
        real l21_0;
        real l22_0;
        real l23_0;
        real l24_0;
        real l25_0;
        real l26_0;
        real l27_0;
      
        ////////////////////////////////// item main effects
        real<lower=0> l1_11;
        real<lower=0> l2_13;
        real<lower=0> l3_12;
        real<lower=0> l4_11;
        real<lower=0> l5_11;
        real<lower=0> l6_12;
        real<lower=0> l7_11;
        real<lower=0> l8_13;
        real<lower=0> l9_13;
        real<lower=0> l10_13;
        real<lower=0> l11_13;
        real<lower=0> l12_11;
        real<lower=0> l13_14;
        real<lower=0> l14_11;
        real<lower=0> l14_14;
        real<lower=0> l15_11;
        real<lower=0> l15_14;
        real<lower=0> l16_11;
        real<lower=0> l17_11;
        real<lower=0> l18_12;
        real<lower=0> l18_14;
        real<lower=0> l19_11;
        real<lower=0> l19_12;
        real<lower=0> l20_12;
        real<lower=0> l20_14;
        real<lower=0> l21_12;
        real<lower=0> l22_12;
        real<lower=0> l23_11;
        real<lower=0> l24_11;
        real<lower=0> l24_12;
        real<lower=0> l25_11;
        real<lower=0> l25_12;
        real<lower=0> l26_11;
        real<lower=0> l27_11;
        real<lower=0> l27_12;
      }
      transformed parameters {
        vector[C] log_Vc = log(Vc);
        matrix[I,C] pi;
      
        ////////////////////////////////// probability of correct response
        pi[1,1] = inv_logit(l1_0);
        pi[1,2] = inv_logit(l1_0+l1_11);
        pi[1,3] = inv_logit(l1_0);
        pi[1,4] = inv_logit(l1_0);
        pi[1,5] = inv_logit(l1_0);
        pi[1,6] = inv_logit(l1_0+l1_11);
        pi[1,7] = inv_logit(l1_0+l1_11);
        pi[1,8] = inv_logit(l1_0+l1_11);
        pi[1,9] = inv_logit(l1_0);
        pi[1,10] = inv_logit(l1_0);
        pi[1,11] = inv_logit(l1_0);
        pi[1,12] = inv_logit(l1_0+l1_11);
        pi[1,13] = inv_logit(l1_0+l1_11);
        pi[1,14] = inv_logit(l1_0+l1_11);
        pi[1,15] = inv_logit(l1_0);
        pi[1,16] = inv_logit(l1_0+l1_11);
        pi[2,1] = inv_logit(l2_0);
        pi[2,2] = inv_logit(l2_0);
        pi[2,3] = inv_logit(l2_0);
        pi[2,4] = inv_logit(l2_0+l2_13);
        pi[2,5] = inv_logit(l2_0);
        pi[2,6] = inv_logit(l2_0);
        pi[2,7] = inv_logit(l2_0+l2_13);
        pi[2,8] = inv_logit(l2_0);
        pi[2,9] = inv_logit(l2_0+l2_13);
        pi[2,10] = inv_logit(l2_0);
        pi[2,11] = inv_logit(l2_0+l2_13);
        pi[2,12] = inv_logit(l2_0+l2_13);
        pi[2,13] = inv_logit(l2_0);
        pi[2,14] = inv_logit(l2_0+l2_13);
        pi[2,15] = inv_logit(l2_0+l2_13);
        pi[2,16] = inv_logit(l2_0+l2_13);
        pi[3,1] = inv_logit(l3_0);
        pi[3,2] = inv_logit(l3_0);
        pi[3,3] = inv_logit(l3_0+l3_12);
        pi[3,4] = inv_logit(l3_0);
        pi[3,5] = inv_logit(l3_0);
        pi[3,6] = inv_logit(l3_0+l3_12);
        pi[3,7] = inv_logit(l3_0);
        pi[3,8] = inv_logit(l3_0);
        pi[3,9] = inv_logit(l3_0+l3_12);
        pi[3,10] = inv_logit(l3_0+l3_12);
        pi[3,11] = inv_logit(l3_0);
        pi[3,12] = inv_logit(l3_0+l3_12);
        pi[3,13] = inv_logit(l3_0+l3_12);
        pi[3,14] = inv_logit(l3_0);
        pi[3,15] = inv_logit(l3_0+l3_12);
        pi[3,16] = inv_logit(l3_0+l3_12);
        pi[4,1] = inv_logit(l4_0);
        pi[4,2] = inv_logit(l4_0+l4_11);
        pi[4,3] = inv_logit(l4_0);
        pi[4,4] = inv_logit(l4_0);
        pi[4,5] = inv_logit(l4_0);
        pi[4,6] = inv_logit(l4_0+l4_11);
        pi[4,7] = inv_logit(l4_0+l4_11);
        pi[4,8] = inv_logit(l4_0+l4_11);
        pi[4,9] = inv_logit(l4_0);
        pi[4,10] = inv_logit(l4_0);
        pi[4,11] = inv_logit(l4_0);
        pi[4,12] = inv_logit(l4_0+l4_11);
        pi[4,13] = inv_logit(l4_0+l4_11);
        pi[4,14] = inv_logit(l4_0+l4_11);
        pi[4,15] = inv_logit(l4_0);
        pi[4,16] = inv_logit(l4_0+l4_11);
        pi[5,1] = inv_logit(l5_0);
        pi[5,2] = inv_logit(l5_0+l5_11);
        pi[5,3] = inv_logit(l5_0);
        pi[5,4] = inv_logit(l5_0);
        pi[5,5] = inv_logit(l5_0);
        pi[5,6] = inv_logit(l5_0+l5_11);
        pi[5,7] = inv_logit(l5_0+l5_11);
        pi[5,8] = inv_logit(l5_0+l5_11);
        pi[5,9] = inv_logit(l5_0);
        pi[5,10] = inv_logit(l5_0);
        pi[5,11] = inv_logit(l5_0);
        pi[5,12] = inv_logit(l5_0+l5_11);
        pi[5,13] = inv_logit(l5_0+l5_11);
        pi[5,14] = inv_logit(l5_0+l5_11);
        pi[5,15] = inv_logit(l5_0);
        pi[5,16] = inv_logit(l5_0+l5_11);
        pi[6,1] = inv_logit(l6_0);
        pi[6,2] = inv_logit(l6_0);
        pi[6,3] = inv_logit(l6_0+l6_12);
        pi[6,4] = inv_logit(l6_0);
        pi[6,5] = inv_logit(l6_0);
        pi[6,6] = inv_logit(l6_0+l6_12);
        pi[6,7] = inv_logit(l6_0);
        pi[6,8] = inv_logit(l6_0);
        pi[6,9] = inv_logit(l6_0+l6_12);
        pi[6,10] = inv_logit(l6_0+l6_12);
        pi[6,11] = inv_logit(l6_0);
        pi[6,12] = inv_logit(l6_0+l6_12);
        pi[6,13] = inv_logit(l6_0+l6_12);
        pi[6,14] = inv_logit(l6_0);
        pi[6,15] = inv_logit(l6_0+l6_12);
        pi[6,16] = inv_logit(l6_0+l6_12);
        pi[7,1] = inv_logit(l7_0);
        pi[7,2] = inv_logit(l7_0+l7_11);
        pi[7,3] = inv_logit(l7_0);
        pi[7,4] = inv_logit(l7_0);
        pi[7,5] = inv_logit(l7_0);
        pi[7,6] = inv_logit(l7_0+l7_11);
        pi[7,7] = inv_logit(l7_0+l7_11);
        pi[7,8] = inv_logit(l7_0+l7_11);
        pi[7,9] = inv_logit(l7_0);
        pi[7,10] = inv_logit(l7_0);
        pi[7,11] = inv_logit(l7_0);
        pi[7,12] = inv_logit(l7_0+l7_11);
        pi[7,13] = inv_logit(l7_0+l7_11);
        pi[7,14] = inv_logit(l7_0+l7_11);
        pi[7,15] = inv_logit(l7_0);
        pi[7,16] = inv_logit(l7_0+l7_11);
        pi[8,1] = inv_logit(l8_0);
        pi[8,2] = inv_logit(l8_0);
        pi[8,3] = inv_logit(l8_0);
        pi[8,4] = inv_logit(l8_0+l8_13);
        pi[8,5] = inv_logit(l8_0);
        pi[8,6] = inv_logit(l8_0);
        pi[8,7] = inv_logit(l8_0+l8_13);
        pi[8,8] = inv_logit(l8_0);
        pi[8,9] = inv_logit(l8_0+l8_13);
        pi[8,10] = inv_logit(l8_0);
        pi[8,11] = inv_logit(l8_0+l8_13);
        pi[8,12] = inv_logit(l8_0+l8_13);
        pi[8,13] = inv_logit(l8_0);
        pi[8,14] = inv_logit(l8_0+l8_13);
        pi[8,15] = inv_logit(l8_0+l8_13);
        pi[8,16] = inv_logit(l8_0+l8_13);
        pi[9,1] = inv_logit(l9_0);
        pi[9,2] = inv_logit(l9_0);
        pi[9,3] = inv_logit(l9_0);
        pi[9,4] = inv_logit(l9_0+l9_13);
        pi[9,5] = inv_logit(l9_0);
        pi[9,6] = inv_logit(l9_0);
        pi[9,7] = inv_logit(l9_0+l9_13);
        pi[9,8] = inv_logit(l9_0);
        pi[9,9] = inv_logit(l9_0+l9_13);
        pi[9,10] = inv_logit(l9_0);
        pi[9,11] = inv_logit(l9_0+l9_13);
        pi[9,12] = inv_logit(l9_0+l9_13);
        pi[9,13] = inv_logit(l9_0);
        pi[9,14] = inv_logit(l9_0+l9_13);
        pi[9,15] = inv_logit(l9_0+l9_13);
        pi[9,16] = inv_logit(l9_0+l9_13);
        pi[10,1] = inv_logit(l10_0);
        pi[10,2] = inv_logit(l10_0);
        pi[10,3] = inv_logit(l10_0);
        pi[10,4] = inv_logit(l10_0+l10_13);
        pi[10,5] = inv_logit(l10_0);
        pi[10,6] = inv_logit(l10_0);
        pi[10,7] = inv_logit(l10_0+l10_13);
        pi[10,8] = inv_logit(l10_0);
        pi[10,9] = inv_logit(l10_0+l10_13);
        pi[10,10] = inv_logit(l10_0);
        pi[10,11] = inv_logit(l10_0+l10_13);
        pi[10,12] = inv_logit(l10_0+l10_13);
        pi[10,13] = inv_logit(l10_0);
        pi[10,14] = inv_logit(l10_0+l10_13);
        pi[10,15] = inv_logit(l10_0+l10_13);
        pi[10,16] = inv_logit(l10_0+l10_13);
        pi[11,1] = inv_logit(l11_0);
        pi[11,2] = inv_logit(l11_0);
        pi[11,3] = inv_logit(l11_0);
        pi[11,4] = inv_logit(l11_0+l11_13);
        pi[11,5] = inv_logit(l11_0);
        pi[11,6] = inv_logit(l11_0);
        pi[11,7] = inv_logit(l11_0+l11_13);
        pi[11,8] = inv_logit(l11_0);
        pi[11,9] = inv_logit(l11_0+l11_13);
        pi[11,10] = inv_logit(l11_0);
        pi[11,11] = inv_logit(l11_0+l11_13);
        pi[11,12] = inv_logit(l11_0+l11_13);
        pi[11,13] = inv_logit(l11_0);
        pi[11,14] = inv_logit(l11_0+l11_13);
        pi[11,15] = inv_logit(l11_0+l11_13);
        pi[11,16] = inv_logit(l11_0+l11_13);
        pi[12,1] = inv_logit(l12_0);
        pi[12,2] = inv_logit(l12_0+l12_11);
        pi[12,3] = inv_logit(l12_0);
        pi[12,4] = inv_logit(l12_0);
        pi[12,5] = inv_logit(l12_0);
        pi[12,6] = inv_logit(l12_0+l12_11);
        pi[12,7] = inv_logit(l12_0+l12_11);
        pi[12,8] = inv_logit(l12_0+l12_11);
        pi[12,9] = inv_logit(l12_0);
        pi[12,10] = inv_logit(l12_0);
        pi[12,11] = inv_logit(l12_0);
        pi[12,12] = inv_logit(l12_0+l12_11);
        pi[12,13] = inv_logit(l12_0+l12_11);
        pi[12,14] = inv_logit(l12_0+l12_11);
        pi[12,15] = inv_logit(l12_0);
        pi[12,16] = inv_logit(l12_0+l12_11);
        pi[13,1] = inv_logit(l13_0);
        pi[13,2] = inv_logit(l13_0);
        pi[13,3] = inv_logit(l13_0);
        pi[13,4] = inv_logit(l13_0);
        pi[13,5] = inv_logit(l13_0+l13_14);
        pi[13,6] = inv_logit(l13_0);
        pi[13,7] = inv_logit(l13_0);
        pi[13,8] = inv_logit(l13_0+l13_14);
        pi[13,9] = inv_logit(l13_0);
        pi[13,10] = inv_logit(l13_0+l13_14);
        pi[13,11] = inv_logit(l13_0+l13_14);
        pi[13,12] = inv_logit(l13_0);
        pi[13,13] = inv_logit(l13_0+l13_14);
        pi[13,14] = inv_logit(l13_0+l13_14);
        pi[13,15] = inv_logit(l13_0+l13_14);
        pi[13,16] = inv_logit(l13_0+l13_14);
        pi[14,1] = inv_logit(l14_0);
        pi[14,2] = inv_logit(l14_0+l14_11);
        pi[14,3] = inv_logit(l14_0);
        pi[14,4] = inv_logit(l14_0);
        pi[14,5] = inv_logit(l14_0+l14_14);
        pi[14,6] = inv_logit(l14_0+l14_11);
        pi[14,7] = inv_logit(l14_0+l14_11);
        pi[14,8] = inv_logit(l14_0+l14_11+l14_14);
        pi[14,9] = inv_logit(l14_0);
        pi[14,10] = inv_logit(l14_0+l14_14);
        pi[14,11] = inv_logit(l14_0+l14_14);
        pi[14,12] = inv_logit(l14_0+l14_11);
        pi[14,13] = inv_logit(l14_0+l14_11+l14_14);
        pi[14,14] = inv_logit(l14_0+l14_11+l14_14);
        pi[14,15] = inv_logit(l14_0+l14_14);
        pi[14,16] = inv_logit(l14_0+l14_11+l14_14);
        pi[15,1] = inv_logit(l15_0);
        pi[15,2] = inv_logit(l15_0+l15_11);
        pi[15,3] = inv_logit(l15_0);
        pi[15,4] = inv_logit(l15_0);
        pi[15,5] = inv_logit(l15_0+l15_14);
        pi[15,6] = inv_logit(l15_0+l15_11);
        pi[15,7] = inv_logit(l15_0+l15_11);
        pi[15,8] = inv_logit(l15_0+l15_11+l15_14);
        pi[15,9] = inv_logit(l15_0);
        pi[15,10] = inv_logit(l15_0+l15_14);
        pi[15,11] = inv_logit(l15_0+l15_14);
        pi[15,12] = inv_logit(l15_0+l15_11);
        pi[15,13] = inv_logit(l15_0+l15_11+l15_14);
        pi[15,14] = inv_logit(l15_0+l15_11+l15_14);
        pi[15,15] = inv_logit(l15_0+l15_14);
        pi[15,16] = inv_logit(l15_0+l15_11+l15_14);
        pi[16,1] = inv_logit(l16_0);
        pi[16,2] = inv_logit(l16_0+l16_11);
        pi[16,3] = inv_logit(l16_0);
        pi[16,4] = inv_logit(l16_0);
        pi[16,5] = inv_logit(l16_0);
        pi[16,6] = inv_logit(l16_0+l16_11);
        pi[16,7] = inv_logit(l16_0+l16_11);
        pi[16,8] = inv_logit(l16_0+l16_11);
        pi[16,9] = inv_logit(l16_0);
        pi[16,10] = inv_logit(l16_0);
        pi[16,11] = inv_logit(l16_0);
        pi[16,12] = inv_logit(l16_0+l16_11);
        pi[16,13] = inv_logit(l16_0+l16_11);
        pi[16,14] = inv_logit(l16_0+l16_11);
        pi[16,15] = inv_logit(l16_0);
        pi[16,16] = inv_logit(l16_0+l16_11);
        pi[17,1] = inv_logit(l17_0);
        pi[17,2] = inv_logit(l17_0+l17_11);
        pi[17,3] = inv_logit(l17_0);
        pi[17,4] = inv_logit(l17_0);
        pi[17,5] = inv_logit(l17_0);
        pi[17,6] = inv_logit(l17_0+l17_11);
        pi[17,7] = inv_logit(l17_0+l17_11);
        pi[17,8] = inv_logit(l17_0+l17_11);
        pi[17,9] = inv_logit(l17_0);
        pi[17,10] = inv_logit(l17_0);
        pi[17,11] = inv_logit(l17_0);
        pi[17,12] = inv_logit(l17_0+l17_11);
        pi[17,13] = inv_logit(l17_0+l17_11);
        pi[17,14] = inv_logit(l17_0+l17_11);
        pi[17,15] = inv_logit(l17_0);
        pi[17,16] = inv_logit(l17_0+l17_11);
        pi[18,1] = inv_logit(l18_0);
        pi[18,2] = inv_logit(l18_0);
        pi[18,3] = inv_logit(l18_0+l18_12);
        pi[18,4] = inv_logit(l18_0);
        pi[18,5] = inv_logit(l18_0+l18_14);
        pi[18,6] = inv_logit(l18_0+l18_12);
        pi[18,7] = inv_logit(l18_0);
        pi[18,8] = inv_logit(l18_0+l18_14);
        pi[18,9] = inv_logit(l18_0+l18_12);
        pi[18,10] = inv_logit(l18_0+l18_12+l18_14);
        pi[18,11] = inv_logit(l18_0+l18_14);
        pi[18,12] = inv_logit(l18_0+l18_12);
        pi[18,13] = inv_logit(l18_0+l18_12+l18_14);
        pi[18,14] = inv_logit(l18_0+l18_14);
        pi[18,15] = inv_logit(l18_0+l18_12+l18_14);
        pi[18,16] = inv_logit(l18_0+l18_12+l18_14);
        pi[19,1] = inv_logit(l19_0);
        pi[19,2] = inv_logit(l19_0+l19_11);
        pi[19,3] = inv_logit(l19_0+l19_12);
        pi[19,4] = inv_logit(l19_0);
        pi[19,5] = inv_logit(l19_0);
        pi[19,6] = inv_logit(l19_0+l19_11+l19_12);
        pi[19,7] = inv_logit(l19_0+l19_11);
        pi[19,8] = inv_logit(l19_0+l19_11);
        pi[19,9] = inv_logit(l19_0+l19_12);
        pi[19,10] = inv_logit(l19_0+l19_12);
        pi[19,11] = inv_logit(l19_0);
        pi[19,12] = inv_logit(l19_0+l19_11+l19_12);
        pi[19,13] = inv_logit(l19_0+l19_11+l19_12);
        pi[19,14] = inv_logit(l19_0+l19_11);
        pi[19,15] = inv_logit(l19_0+l19_12);
        pi[19,16] = inv_logit(l19_0+l19_11+l19_12);
        pi[20,1] = inv_logit(l20_0);
        pi[20,2] = inv_logit(l20_0);
        pi[20,3] = inv_logit(l20_0+l20_12);
        pi[20,4] = inv_logit(l20_0);
        pi[20,5] = inv_logit(l20_0+l20_14);
        pi[20,6] = inv_logit(l20_0+l20_12);
        pi[20,7] = inv_logit(l20_0);
        pi[20,8] = inv_logit(l20_0+l20_14);
        pi[20,9] = inv_logit(l20_0+l20_12);
        pi[20,10] = inv_logit(l20_0+l20_12+l20_14);
        pi[20,11] = inv_logit(l20_0+l20_14);
        pi[20,12] = inv_logit(l20_0+l20_12);
        pi[20,13] = inv_logit(l20_0+l20_12+l20_14);
        pi[20,14] = inv_logit(l20_0+l20_14);
        pi[20,15] = inv_logit(l20_0+l20_12+l20_14);
        pi[20,16] = inv_logit(l20_0+l20_12+l20_14);
        pi[21,1] = inv_logit(l21_0);
        pi[21,2] = inv_logit(l21_0);
        pi[21,3] = inv_logit(l21_0+l21_12);
        pi[21,4] = inv_logit(l21_0);
        pi[21,5] = inv_logit(l21_0);
        pi[21,6] = inv_logit(l21_0+l21_12);
        pi[21,7] = inv_logit(l21_0);
        pi[21,8] = inv_logit(l21_0);
        pi[21,9] = inv_logit(l21_0+l21_12);
        pi[21,10] = inv_logit(l21_0+l21_12);
        pi[21,11] = inv_logit(l21_0);
        pi[21,12] = inv_logit(l21_0+l21_12);
        pi[21,13] = inv_logit(l21_0+l21_12);
        pi[21,14] = inv_logit(l21_0);
        pi[21,15] = inv_logit(l21_0+l21_12);
        pi[21,16] = inv_logit(l21_0+l21_12);
        pi[22,1] = inv_logit(l22_0);
        pi[22,2] = inv_logit(l22_0);
        pi[22,3] = inv_logit(l22_0+l22_12);
        pi[22,4] = inv_logit(l22_0);
        pi[22,5] = inv_logit(l22_0);
        pi[22,6] = inv_logit(l22_0+l22_12);
        pi[22,7] = inv_logit(l22_0);
        pi[22,8] = inv_logit(l22_0);
        pi[22,9] = inv_logit(l22_0+l22_12);
        pi[22,10] = inv_logit(l22_0+l22_12);
        pi[22,11] = inv_logit(l22_0);
        pi[22,12] = inv_logit(l22_0+l22_12);
        pi[22,13] = inv_logit(l22_0+l22_12);
        pi[22,14] = inv_logit(l22_0);
        pi[22,15] = inv_logit(l22_0+l22_12);
        pi[22,16] = inv_logit(l22_0+l22_12);
        pi[23,1] = inv_logit(l23_0);
        pi[23,2] = inv_logit(l23_0+l23_11);
        pi[23,3] = inv_logit(l23_0);
        pi[23,4] = inv_logit(l23_0);
        pi[23,5] = inv_logit(l23_0);
        pi[23,6] = inv_logit(l23_0+l23_11);
        pi[23,7] = inv_logit(l23_0+l23_11);
        pi[23,8] = inv_logit(l23_0+l23_11);
        pi[23,9] = inv_logit(l23_0);
        pi[23,10] = inv_logit(l23_0);
        pi[23,11] = inv_logit(l23_0);
        pi[23,12] = inv_logit(l23_0+l23_11);
        pi[23,13] = inv_logit(l23_0+l23_11);
        pi[23,14] = inv_logit(l23_0+l23_11);
        pi[23,15] = inv_logit(l23_0);
        pi[23,16] = inv_logit(l23_0+l23_11);
        pi[24,1] = inv_logit(l24_0);
        pi[24,2] = inv_logit(l24_0+l24_11);
        pi[24,3] = inv_logit(l24_0+l24_12);
        pi[24,4] = inv_logit(l24_0);
        pi[24,5] = inv_logit(l24_0);
        pi[24,6] = inv_logit(l24_0+l24_11+l24_12);
        pi[24,7] = inv_logit(l24_0+l24_11);
        pi[24,8] = inv_logit(l24_0+l24_11);
        pi[24,9] = inv_logit(l24_0+l24_12);
        pi[24,10] = inv_logit(l24_0+l24_12);
        pi[24,11] = inv_logit(l24_0);
        pi[24,12] = inv_logit(l24_0+l24_11+l24_12);
        pi[24,13] = inv_logit(l24_0+l24_11+l24_12);
        pi[24,14] = inv_logit(l24_0+l24_11);
        pi[24,15] = inv_logit(l24_0+l24_12);
        pi[24,16] = inv_logit(l24_0+l24_11+l24_12);
        pi[25,1] = inv_logit(l25_0);
        pi[25,2] = inv_logit(l25_0+l25_11);
        pi[25,3] = inv_logit(l25_0+l25_12);
        pi[25,4] = inv_logit(l25_0);
        pi[25,5] = inv_logit(l25_0);
        pi[25,6] = inv_logit(l25_0+l25_11+l25_12);
        pi[25,7] = inv_logit(l25_0+l25_11);
        pi[25,8] = inv_logit(l25_0+l25_11);
        pi[25,9] = inv_logit(l25_0+l25_12);
        pi[25,10] = inv_logit(l25_0+l25_12);
        pi[25,11] = inv_logit(l25_0);
        pi[25,12] = inv_logit(l25_0+l25_11+l25_12);
        pi[25,13] = inv_logit(l25_0+l25_11+l25_12);
        pi[25,14] = inv_logit(l25_0+l25_11);
        pi[25,15] = inv_logit(l25_0+l25_12);
        pi[25,16] = inv_logit(l25_0+l25_11+l25_12);
        pi[26,1] = inv_logit(l26_0);
        pi[26,2] = inv_logit(l26_0+l26_11);
        pi[26,3] = inv_logit(l26_0);
        pi[26,4] = inv_logit(l26_0);
        pi[26,5] = inv_logit(l26_0);
        pi[26,6] = inv_logit(l26_0+l26_11);
        pi[26,7] = inv_logit(l26_0+l26_11);
        pi[26,8] = inv_logit(l26_0+l26_11);
        pi[26,9] = inv_logit(l26_0);
        pi[26,10] = inv_logit(l26_0);
        pi[26,11] = inv_logit(l26_0);
        pi[26,12] = inv_logit(l26_0+l26_11);
        pi[26,13] = inv_logit(l26_0+l26_11);
        pi[26,14] = inv_logit(l26_0+l26_11);
        pi[26,15] = inv_logit(l26_0);
        pi[26,16] = inv_logit(l26_0+l26_11);
        pi[27,1] = inv_logit(l27_0);
        pi[27,2] = inv_logit(l27_0+l27_11);
        pi[27,3] = inv_logit(l27_0+l27_12);
        pi[27,4] = inv_logit(l27_0);
        pi[27,5] = inv_logit(l27_0);
        pi[27,6] = inv_logit(l27_0+l27_11+l27_12);
        pi[27,7] = inv_logit(l27_0+l27_11);
        pi[27,8] = inv_logit(l27_0+l27_11);
        pi[27,9] = inv_logit(l27_0+l27_12);
        pi[27,10] = inv_logit(l27_0+l27_12);
        pi[27,11] = inv_logit(l27_0);
        pi[27,12] = inv_logit(l27_0+l27_11+l27_12);
        pi[27,13] = inv_logit(l27_0+l27_11+l27_12);
        pi[27,14] = inv_logit(l27_0+l27_11);
        pi[27,15] = inv_logit(l27_0+l27_12);
        pi[27,16] = inv_logit(l27_0+l27_11+l27_12);
      }
      model {
        real ps[C];
      
        ////////////////////////////////// priors
        Vc ~ dirichlet(rep_vector(1, C));
        l1_0 ~ normal(0, 2);
        l1_11 ~ lognormal(0, 1);
        l2_0 ~ normal(0, 2);
        l2_13 ~ lognormal(0, 1);
        l3_0 ~ normal(0, 2);
        l3_12 ~ lognormal(0, 1);
        l4_0 ~ normal(0, 2);
        l4_11 ~ lognormal(0, 1);
        l5_0 ~ normal(0, 2);
        l5_11 ~ lognormal(0, 1);
        l6_0 ~ normal(0, 2);
        l6_12 ~ lognormal(0, 1);
        l7_0 ~ normal(0, 2);
        l7_11 ~ lognormal(0, 1);
        l8_0 ~ normal(0, 2);
        l8_13 ~ lognormal(0, 1);
        l9_0 ~ normal(0, 2);
        l9_13 ~ lognormal(0, 1);
        l10_0 ~ normal(0, 2);
        l10_13 ~ lognormal(0, 1);
        l11_0 ~ normal(0, 2);
        l11_13 ~ lognormal(0, 1);
        l12_0 ~ normal(0, 2);
        l12_11 ~ lognormal(0, 1);
        l13_0 ~ normal(0, 2);
        l13_14 ~ lognormal(0, 1);
        l14_0 ~ normal(0, 2);
        l14_11 ~ lognormal(0, 1);
        l14_14 ~ lognormal(0, 1);
        l15_0 ~ normal(0, 2);
        l15_11 ~ lognormal(0, 1);
        l15_14 ~ lognormal(0, 1);
        l16_0 ~ normal(0, 2);
        l16_11 ~ lognormal(0, 1);
        l17_0 ~ normal(0, 2);
        l17_11 ~ lognormal(0, 1);
        l18_0 ~ normal(0, 2);
        l18_12 ~ lognormal(0, 1);
        l18_14 ~ lognormal(0, 1);
        l19_0 ~ normal(0, 2);
        l19_11 ~ lognormal(0, 1);
        l19_12 ~ lognormal(0, 1);
        l20_0 ~ normal(0, 2);
        l20_12 ~ lognormal(0, 1);
        l20_14 ~ lognormal(0, 1);
        l21_0 ~ normal(0, 2);
        l21_12 ~ lognormal(0, 1);
        l22_0 ~ normal(0, 2);
        l22_12 ~ lognormal(0, 1);
        l23_0 ~ normal(0, 2);
        l23_11 ~ lognormal(0, 1);
        l24_0 ~ normal(0, 2);
        l24_11 ~ lognormal(0, 1);
        l24_12 ~ lognormal(0, 1);
        l25_0 ~ normal(0, 2);
        l25_11 ~ lognormal(0, 1);
        l25_12 ~ lognormal(0, 1);
        l26_0 ~ normal(0, 2);
        l26_11 ~ lognormal(0, 1);
        l27_0 ~ normal(0, 2);
        l27_11 ~ lognormal(0, 1);
        l27_12 ~ lognormal(0, 1);
      
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
      
      $prior
      # A tibble: 3 x 3
        class      coef  prior_def                  
        <chr>      <chr> <chr>                      
      1 intercept  <NA>  normal(0, 2)               
      2 maineffect <NA>  lognormal(0, 1)            
      3 structural Vc    dirichlet(rep_vector(1, C))
      

---

    Code
      crum_script(mdm_q, strc = "independent")
    Output
      $stancode
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
        real<lower=0,upper=1> eta[A];
      
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
        simplex[C] Vc;
        vector[C] log_Vc;
        for (c in 1:C) {
          Vc[c] = 1;
          for (a in 1:A) {
            Vc[c] = Vc[c] * eta[a]^Alpha[c,a] * 
                    (1 - eta[a]) ^ (1 - Alpha[c,a]);
          }
        }
        log_Vc = log(Vc);
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
        eta[1] ~ beta(1, 1);
        l1_0 ~ normal(0, 2);
        l1_11 ~ lognormal(0, 1);
        l2_0 ~ normal(0, 2);
        l2_11 ~ lognormal(0, 1);
        l3_0 ~ normal(0, 2);
        l3_11 ~ lognormal(0, 1);
        l4_0 ~ normal(0, 2);
        l4_11 ~ lognormal(0, 1);
      
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
      
      $prior
      # A tibble: 3 x 3
        class      coef  prior_def      
        <chr>      <chr> <chr>          
      1 intercept  <NA>  normal(0, 2)   
      2 maineffect <NA>  lognormal(0, 1)
      3 structural <NA>  beta(1, 1)     
      

# dina and dino script works

    Code
      dina_script(ecpe_q)
    Output
      $stancode
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
        Vc ~ dirichlet(rep_vector(1, C));
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
        slip[21] ~ beta(5, 25);
        guess[21] ~ beta(5, 25);
        slip[22] ~ beta(5, 25);
        guess[22] ~ beta(5, 25);
        slip[23] ~ beta(5, 25);
        guess[23] ~ beta(5, 25);
        slip[24] ~ beta(5, 25);
        guess[24] ~ beta(5, 25);
        slip[25] ~ beta(5, 25);
        guess[25] ~ beta(5, 25);
        slip[26] ~ beta(5, 25);
        guess[26] ~ beta(5, 25);
        slip[27] ~ beta(5, 25);
        guess[27] ~ beta(5, 25);
        slip[28] ~ beta(5, 25);
        guess[28] ~ beta(5, 25);
      
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
      
      $prior
      # A tibble: 3 x 3
        class      coef  prior_def                  
        <chr>      <chr> <chr>                      
      1 slip       <NA>  beta(5, 25)                
      2 guess      <NA>  beta(5, 25)                
      3 structural Vc    dirichlet(rep_vector(1, C))
      

---

    Code
      dino_script(mdm_q)
    Output
      $stancode
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
        Vc ~ dirichlet(rep_vector(1, C));
        slip[1] ~ beta(5, 25);
        guess[1] ~ beta(5, 25);
        slip[2] ~ beta(5, 25);
        guess[2] ~ beta(5, 25);
        slip[3] ~ beta(5, 25);
        guess[3] ~ beta(5, 25);
        slip[4] ~ beta(5, 25);
        guess[4] ~ beta(5, 25);
      
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
      
      $prior
      # A tibble: 3 x 3
        class      coef  prior_def                  
        <chr>      <chr> <chr>                      
      1 slip       <NA>  beta(5, 25)                
      2 guess      <NA>  beta(5, 25)                
      3 structural Vc    dirichlet(rep_vector(1, C))
      

---

    Code
      dina_script(dtmr_q)
    Output
      $stancode
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
        Vc ~ dirichlet(rep_vector(1, C));
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
        slip[21] ~ beta(5, 25);
        guess[21] ~ beta(5, 25);
        slip[22] ~ beta(5, 25);
        guess[22] ~ beta(5, 25);
        slip[23] ~ beta(5, 25);
        guess[23] ~ beta(5, 25);
        slip[24] ~ beta(5, 25);
        guess[24] ~ beta(5, 25);
        slip[25] ~ beta(5, 25);
        guess[25] ~ beta(5, 25);
        slip[26] ~ beta(5, 25);
        guess[26] ~ beta(5, 25);
        slip[27] ~ beta(5, 25);
        guess[27] ~ beta(5, 25);
      
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
      
      $prior
      # A tibble: 3 x 3
        class      coef  prior_def                  
        <chr>      <chr> <chr>                      
      1 slip       <NA>  beta(5, 25)                
      2 guess      <NA>  beta(5, 25)                
      3 structural Vc    dirichlet(rep_vector(1, C))
      

---

    Code
      dino_script(dtmr_q, strc = "independent")
    Output
      $stancode
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
        real<lower=0,upper=1> eta[A];
      
        ////////////////////////////////// item parameters
        real<lower=0,upper=1> slip[I];
        real<lower=0,upper=1> guess[I];
      }
      transformed parameters {
        simplex[C] Vc;
        vector[C] log_Vc;
        for (c in 1:C) {
          Vc[c] = 1;
          for (a in 1:A) {
            Vc[c] = Vc[c] * eta[a]^Alpha[c,a] * 
                    (1 - eta[a]) ^ (1 - Alpha[c,a]);
          }
        }
        log_Vc = log(Vc);
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
        Vc ~ beta(1, 1);
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
        slip[21] ~ beta(5, 25);
        guess[21] ~ beta(5, 25);
        slip[22] ~ beta(5, 25);
        guess[22] ~ beta(5, 25);
        slip[23] ~ beta(5, 25);
        guess[23] ~ beta(5, 25);
        slip[24] ~ beta(5, 25);
        guess[24] ~ beta(5, 25);
        slip[25] ~ beta(5, 25);
        guess[25] ~ beta(5, 25);
        slip[26] ~ beta(5, 25);
        guess[26] ~ beta(5, 25);
        slip[27] ~ beta(5, 25);
        guess[27] ~ beta(5, 25);
      
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
      
      $prior
      # A tibble: 3 x 3
        class      coef  prior_def  
        <chr>      <chr> <chr>      
      1 slip       <NA>  beta(5, 25)
      2 guess      <NA>  beta(5, 25)
      3 structural <NA>  beta(1, 1) 
      


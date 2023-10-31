# lcdm model works for ecpe

    Code
      cmds_ecpe_lcdm$stancode
    Output
      data {
        int<lower=1> I;                      // number of items
        int<lower=1> R;                      // number of respondents
        int<lower=1> N;                      // number of observations
        int<lower=1> C;                      // number of classes
        int<lower=1> A;                      // number of attributes
        array[N] int<lower=1,upper=I> ii;    // item for observation n
        array[N] int<lower=1,upper=R> rr;    // respondent for observation n
        array[N] int<lower=0,upper=1> y;     // score for observation n
        array[R] int<lower=1,upper=N> start; // starting row for respondent R
        array[R] int<lower=1,upper=I> num;   // number of items for respondent R
        matrix[C,A] Alpha;                   // attribute pattern for each class
        matrix[I,C] Xi;                      // class attribute mastery indicator
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
      
        ////////////////////////////////// priors
        Vc ~ dirichlet(rep_vector(1, C));
        l1_0 ~ uniform(-15, 15);
        l1_11 ~ uniform(0, 15);
        l1_12 ~ uniform(0, 15);
        l1_212 ~ uniform(-15, 15);
        l2_0 ~ uniform(-15, 15);
        l2_12 ~ uniform(0, 15);
        l3_0 ~ uniform(-15, 15);
        l3_11 ~ uniform(0, 15);
        l3_13 ~ uniform(0, 15);
        l3_213 ~ uniform(-15, 15);
        l4_0 ~ uniform(-15, 15);
        l4_13 ~ uniform(0, 15);
        l5_0 ~ uniform(-15, 15);
        l5_13 ~ uniform(0, 15);
        l6_0 ~ uniform(-15, 15);
        l6_13 ~ uniform(0, 15);
        l7_0 ~ uniform(-15, 15);
        l7_11 ~ uniform(0, 15);
        l7_13 ~ uniform(0, 15);
        l7_213 ~ uniform(-15, 15);
        l8_0 ~ uniform(-15, 15);
        l8_12 ~ uniform(0, 15);
        l9_0 ~ uniform(-15, 15);
        l9_13 ~ uniform(0, 15);
        l10_0 ~ uniform(-15, 15);
        l10_11 ~ uniform(0, 15);
        l11_0 ~ uniform(-15, 15);
        l11_11 ~ uniform(0, 15);
        l11_13 ~ uniform(0, 15);
        l11_213 ~ uniform(-15, 15);
        l12_0 ~ uniform(-15, 15);
        l12_11 ~ uniform(0, 15);
        l12_13 ~ uniform(0, 15);
        l12_213 ~ uniform(-15, 15);
        l13_0 ~ uniform(-15, 15);
        l13_11 ~ uniform(0, 15);
        l14_0 ~ uniform(-15, 15);
        l14_11 ~ uniform(0, 15);
        l15_0 ~ uniform(-15, 15);
        l15_13 ~ uniform(0, 15);
        l16_0 ~ uniform(-15, 15);
        l16_11 ~ uniform(0, 15);
        l16_13 ~ uniform(0, 15);
        l16_213 ~ uniform(-15, 15);
        l17_0 ~ uniform(-15, 15);
        l17_12 ~ uniform(0, 15);
        l17_13 ~ uniform(0, 15);
        l17_223 ~ uniform(-15, 15);
        l18_0 ~ uniform(-15, 15);
        l18_13 ~ uniform(0, 15);
        l19_0 ~ uniform(-15, 15);
        l19_13 ~ uniform(0, 15);
        l20_0 ~ uniform(-15, 15);
        l20_11 ~ uniform(0, 15);
        l20_13 ~ uniform(0, 15);
        l20_213 ~ uniform(-15, 15);
        l21_0 ~ uniform(-15, 15);
        l21_11 ~ uniform(0, 15);
        l21_13 ~ uniform(0, 15);
        l21_213 ~ uniform(-15, 15);
        l22_0 ~ uniform(-15, 15);
        l22_13 ~ uniform(0, 15);
        l23_0 ~ uniform(-15, 15);
        l23_12 ~ uniform(0, 15);
        l24_0 ~ uniform(-15, 15);
        l24_12 ~ uniform(0, 15);
        l25_0 ~ uniform(-15, 15);
        l25_11 ~ uniform(0, 15);
        l26_0 ~ uniform(-15, 15);
        l26_13 ~ uniform(0, 15);
        l27_0 ~ uniform(-15, 15);
        l27_11 ~ uniform(0, 15);
        l28_0 ~ uniform(-15, 15);
        l28_13 ~ uniform(0, 15);
      
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


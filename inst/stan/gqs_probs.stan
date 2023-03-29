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

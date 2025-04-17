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
  array[R] int<lower=1,upper=I> num;   // number items for respondent R
  matrix[C,A] Alpha;                   // attribute patterns for classes
}
parameters {
  vector[C] log_Vc;
  matrix[I,C] pi;
}
generated quantities {
  array[N] int y_rep;
  array[R] int r_class;

  for (r in 1:R) {
    vector[C] r_probs = exp(log_Vc) / exp(log_sum_exp(log_Vc));
    r_class[r] = categorical_rng(r_probs);
    for (m in 1:num[r]) {
      int i = ii[start[r] + m - 1];
      y_rep[start[r] + m - 1] = bernoulli_rng(pi[i, r_class[r]]);
    }
  }
}

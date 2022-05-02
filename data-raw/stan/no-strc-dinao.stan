data{
    int<lower=1> I;         // # of items
    int<lower=1> J;         // # of respondents
    int<lower=1> K;         // # of attributes
    int<lower=1> C;         // # of attribute profiles (latent classes)
    matrix[J,I] y;          // response matrix
    matrix[C,K] alpha;      // attribute profile matrix
    matrix[I,C] xi;         // the global attribute mastery indicator (product of alpha^q-element)
}

parameters{
    simplex[C] nu;                      // probabilities of latent class membership
    real<lower=0,upper=1> slip[I];      // slip parameter
    real<lower=0,upper=1> guess[I];     // guess parameter
}

transformed parameters{
    vector[C] log_nu;
    log_nu = log(nu);
}

model{
    real ps[C];             // temp for log component densities
    real pi;
    real log_items[I];
    slip ~ beta(5,25);
    guess ~ beta(5,25);
    for (j in 1:J){
        for (c in 1:C){
            for (i in 1:I){
                pi = (1 - slip[i])^xi[i,c] * guess[i]^(1 - xi[i,c]);
                log_items[i] = y[j,i] * log(pi)
                        + (1 - y[j,i]) * log(1 - pi);
            }
            ps[c] = log_nu[c] + sum(log_items);
        }
        target += log_sum_exp(ps);
    }
}

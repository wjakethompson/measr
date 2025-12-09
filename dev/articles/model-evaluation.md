# Evaluating diagnostic classification models

In this article, we will describe the different options for evaluating
diagnostic classification models (DCMs; also known as cognitive
diagnostic models \[CDMs\]) using measr. We start with the data to
analyze, estimate our DCM, and learn how to evaluate different aspects
of the model such as model fit and reliability.

To use the code in this article, you will need to install and load the
measr package.

``` r
library(measr)
#> 
#> Attaching package: 'measr'
#> The following object is masked from 'package:stats':
#> 
#>     optim
```

## Example Data

To demonstrate the model fit functionality of measr, we’ll use the same
simulated data set that was used to illustrate [model estimation
functionality](https://measr.info/dev/articles/model-estimation.md).
This data set contains 2,000 respondents and 20 items that measure a
total of 4 attributes, but no item measures more than 2 attributes. The
data was generated from the loglinear cognitive diagnostic model (LCDM),
which is a general model that subsumes many other DCM subtypes ([Henson
et al., 2009](#ref-lcdm)). To demonstrate model fit functionality, we’ll
first fit an LCDM and a deterministic-input, noisy “and” gate (DINA)
model ([de la Torre & Douglas, 2004](#ref-dina)) to the data set in
order to compare the fit indices. Because the LCDM was used to generate
our fake data, we expect our estimated LCDM model to perform well. On
the other hand, the DINA model places heavy constraints on the LCDM, and
therefore we expect worse performance from the DINA model. For details
on model estimation, see [Estimating diagnostic classification
models](https://measr.info/dev/articles/model-estimation.md).

``` r
library(tidyverse)

sim_data <- read_rds("data/simulated-data.rds")

lcdm <- dcm_estimate(
  dcm_spec = dcm_specify(qmatrix = sim_data$q_matrix, identifier = "item_id",
                         measurement_model = lcdm()),
  data = sim_data$data, identifier = "resp_id",
  method = "mcmc", backend = "cmdstanr",
  iter_warmup = 1000, iter_sampling = 500, chains = 4, parallel_chains = 4,
  file = "fits/sim-lcdm"
)

dina <- dcm_estimate(
  dcm_spec = dcm_specify(qmatrix = sim_data$q_matrix, identifier = "item_id",
                         measurement_model = dina()),
  data = sim_data$data, identifier = "resp_id",
  method = "mcmc", backend = "cmdstanr",
  iter_warmup = 1000, iter_sampling = 500, chains = 4, parallel_chains = 4,
  file = "fits/sim-dina"
)
```

## Model Evaluation

There are three major types of evaluations that are supported by measr.

- Absolute model fit
- Relative model fit (i.e., model comparisons)
- Reliability

Each of these are discussed in turn.

### Absolute Model Fit

Absolute model fit measures how well the estimated model fits the
observed data. One of the more popular methods for evaluating absolute
model fit for DCMs is the M₂ statistic ([Hansen et al.,
2016](#ref-hansen2016); [Liu et al., 2016](#ref-liu2016)). We can
calculate the M₂ statistics with the
[`fit_m2()`](https://rdrr.io/pkg/dcm2/man/fit_m2.html) function.

``` r
fit_m2(lcdm)
#> # A tibble: 1 × 8
#>      m2    df  pval rmsea ci_lower ci_upper `90% CI`     srmsr
#>   <dbl> <int> <dbl> <dbl>    <dbl>    <dbl> <chr>        <dbl>
#> 1  121.   129 0.672     0        0   0.0091 [0, 0.0091] 0.0166
```

This function returns a data frame with the M₂ statistic (`m2`), and the
degrees of freedom and *p*-value associated with the M₂ (`df` and
`pval`, respectively). A *p*-value less than .05 would typically
indicate poor model fit. As expected in this example, the estimated LCDM
shows adequate model fit to our data (*p* \> .05). The
[`fit_m2()`](https://rdrr.io/pkg/dcm2/man/fit_m2.html) also return
information on the RMSEA and SRMSR fit statistics. The M₂, RMSEA, and
SRMSR are all considered limited-information fit indices. For example,
the M₂ is based off of univariate item statistics and the bivariate
relationships between items. Thus, it cannot capture aspects of model
fit from higher-order relationships (e.g., triplets of items).

Because we used a fully Bayesian estimation of our models, we can use
the posterior distributions to provide another evaluation of model fit
that can incorporate more information. This method is known as a
posterior predictive model check (PPMC). The general process for PPMCs
is as follows:

1.  For each draw of the posterior distribution, generate a synthetic
    data set using the parameters values for that draw.
2.  For each synthetic data set calculate a summary of the data. This
    process creates a posterior distribution of the summary. Because the
    synthetic data sets are generated from the parameter values in the
    posterior distribution, the distribution of the data summary
    represents what we would expect the summary to look like, if the
    estimated model is correct or true.
3.  Calculate the same data summary for the observed data set.
4.  Compare the summary from the observed data to the posterior
    distribution.

If the observed value falls within the posterior distribution of the
summary, this is evidence that our estimated model is consistent with
the observed data. On the other hand, discrepancies between the observed
value and posteriors indicates inconsistencies.

The PPMCs can be used to evaluate both model- and item-level fit. At the
model level, fit is evaluated by the expected raw score distribution, as
described by Park et al. ([2015](#ref-park2015)) and Thompson
([2019](#ref-thompson2019)). At the item level, we can evaluate fit by
examining the expected conditional probabilities of members of each
class providing a correct response ([Sinharay & Almond,
2007](#ref-sinharay2007); [Thompson, 2019](#ref-thompson2019)), as well
as the expected odds ratio between each pair of items ([Park et al.,
2015](#ref-park2015); [Sinharay et al., 2006](#ref-sinharay2006)).

With measr, PPMCs can be calculated with
[`fit_ppmc()`](https://measr.info/dev/reference/fit_ppmc.md). Using
[`fit_ppmc()`](https://measr.info/dev/reference/fit_ppmc.md) can specify
which PPMCs to calculate. For example, here we estimate just the
model-level raw score check by setting `item_fit = NULL`.

``` r
fit_ppmc(lcdm, model_fit = "raw_score")
#> $ppmc_raw_score
#> # A tibble: 1 × 5
#>   obs_chisq ppmc_mean `2.5%` `97.5%`   ppp
#>       <dbl>     <dbl>  <dbl>   <dbl> <dbl>
#> 1      24.0      23.7   10.9    40.9 0.452
```

[`fit_ppmc()`](https://measr.info/dev/reference/fit_ppmc.md) returns a
list, where each element is a different PPMC. Here, we only specified
one PPMC, so we only get the `raw_score` element back. The `obs_chisq`
is the raw score χ² values from our observed data. We then see the mean
of the posterior distribution for the χ², quantiles of the posterior
distribution, and the posterior predictive *p*-value (*ppp*). The *ppp*
is the proportion of posterior draws that are greater than the observed
value. Values close to 0 indicate poor fit. Values close to 1 may
indicate overfitting. Because the LCDM was used to generate this data,
it’s not surprising that the *ppp* is approaching close to 0.5 for our
estimated model, as the model is perfectly capturing the data generating
process (i.e., our observed data is right in the middle of what the
estimated model would expect).

We can also specify the posterior quantiles that are returned. For
example, we can calculate the item-level odds ratios and request
quantiles that will result in a 90% credible interval.

``` r
fit_ppmc(lcdm, model_fit = NULL, item_fit = "odds_ratio",
         probs = c(0.05, 0.95))
#> $ppmc_odds_ratio
#> # A tibble: 190 × 7
#>    item_1 item_2 obs_or ppmc_mean  `5%` `95%`   ppp
#>    <chr>  <chr>   <dbl>     <dbl> <dbl> <dbl> <dbl>
#>  1 A1     A2      2.61       2.76 2.22   3.37 0.644
#>  2 A1     A3      2.04       1.96 1.60   2.37 0.344
#>  3 A1     A4      0.904      1.06 0.870  1.28 0.910
#>  4 A1     A5      1.99       2.10 1.69   2.57 0.628
#>  5 A1     A6      3.27       3.24 2.57   4.03 0.428
#>  6 A1     A7      0.966      1.05 0.864  1.25 0.747
#>  7 A1     A8      6.78       6.56 5.06   8.38 0.376
#>  8 A1     A9     10.1        9.76 7.56  12.4  0.359
#>  9 A1     A10     2.98       2.85 2.21   3.68 0.352
#> 10 A1     A11     2.98       3.06 2.50   3.71 0.550
#> # ℹ 180 more rows
```

We see similar output for the item-level indices: the observed odds
ratio for each item pair (`obs_or`), the mean of the posterior
distribution for each item pair (`ppmc_mean`), the quantiles of the
posterior that we specified, and the *ppp*. As with the raw score
distribution, here the *ppp* represents the proportion of posterior
draws where the odds ratio is greater than the observed value.

### Relative Model Fit

Relative fit measures are used to compare multiple competing models. In
this case, we have estimate an LCDM and a DINA model and want to compare
which model performs better. We should first note that “better” does not
necessarily mean “good.” Evidence that one model performs better than
another is not evidence of “good” fit in an absolute sense. Only
absolute model fit indices can provide evidence of adequate fit to the
data. However, if multiple models show adequate absolute fit, relative
fit indices can be used, along with our understanding of the constructs,
to select a preferred model.

Currently, the *Stan* ecosystem supports two information criteria
through the [loo](https://mc-stan.org/loo/) package that can be used as
relative fit indices: leave-one-out (LOO) cross validation with
Pareto-smoothed importance sampling ([Vehtari et al.,
2017](#ref-loo-waic), [2022](#ref-psis)) and the widely applicable
information criterion (WAIC) described by Watanabe ([2010](#ref-waic)).
The information criteria can be calculated using the associated
functions from the loo package (i.e.,
[`loo()`](https://mc-stan.org/loo/reference/loo.html) and
[`waic()`](https://mc-stan.org/loo/reference/waic.html)). Here, we
calculate the LOO for both the LCDM and DINA models.

``` r
lcdm_loo <- loo(lcdm)
lcdm_loo
#> 
#> Computed from 2000 by 2000 log-likelihood matrix.
#> 
#>          Estimate    SE
#> elpd_loo -21731.5 102.7
#> p_loo        77.8   1.2
#> looic     43463.0 205.4
#> ------
#> MCSE of elpd_loo is 0.2.
#> MCSE and ESS estimates assume independent draws (r_eff=1).
#> 
#> All Pareto k estimates are good (k < 0.7).
#> See help('pareto-k-diagnostic') for details.

dina_loo <- loo(dina)
dina_loo
#> 
#> Computed from 2000 by 2000 log-likelihood matrix.
#> 
#>          Estimate    SE
#> elpd_loo -23539.8  79.1
#> p_loo       886.9  18.9
#> looic     47079.5 158.3
#> ------
#> MCSE of elpd_loo is 0.6.
#> MCSE and ESS estimates assume independent draws (r_eff=1).
#> 
#> All Pareto k estimates are good (k < 0.7).
#> See help('pareto-k-diagnostic') for details.
```

In isolation, this output is not very useful, as these indices are meant
to facilitate model comparisons. We can conduct model comparisons using
[`loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html),
which is used for comparing both LOO and WAIC estimates. In the output,
the model in the first row is the preferred model. In subsequent rows,
the `epld_diff` column reports the difference in the information
criteria (in this case the LOO) between the model in that row and the
preferred model. The `se_diff` column is the standard error of the
difference between that model and the preferred model.

``` r
loo_compare(list(lcdm = lcdm_loo, dina = dina_loo))
#>      elpd_diff se_diff
#> lcdm     0.0       0.0
#> dina -1808.3      45.1
```

Bengio & Grandvalet ([2004](#ref-bengio2004)) have recommended a cutoff
of 2.5 standard errors for identifying the preferred model. For example,
because the absolute value of the `elpd_diff` is greater than 45.1 × 2.5
= 112.8, we would conclude that the LCDM fits significantly better than
the DINA model. This is our expected outcome in this case, given the
data generation process and the results of the absolute model fit
analysis. If the difference were less than 2.5 standard errors, we would
conclude that the models fit equally well.

### Reliability

We can also evaluate DCMs through their reliability. That is, it’s
important to understand the accuracy and consistency of the
classifications that are made by the model. For models estimated with
measr, estimates of reliability can be calculated using
[`reliability()`](https://measr.info/dev/reference/reliability.md).

``` r
reliability(lcdm)
#> $pattern_reliability
#>       p_a       p_c 
#> 0.7444377 0.6093438 
#> 
#> $map_reliability
#> $map_reliability$accuracy
#> # A tibble: 4 × 8
#>   attribute   acc lambda_a kappa_a youden_a tetra_a  tp_a  tn_a
#>   <chr>     <dbl>    <dbl>   <dbl>    <dbl>   <dbl> <dbl> <dbl>
#> 1 att1      0.931    0.857   0.244    0.862   0.977 0.929 0.933
#> 2 att2      0.980    0.955   0.944    0.960   0.998 0.981 0.979
#> 3 att3      0.835    0.633   0.650    0.659   0.869 0.882 0.777
#> 4 att4      0.966    0.931   0.395    0.932   0.994 0.968 0.964
#> 
#> $map_reliability$consistency
#> # A tibble: 4 × 10
#>   attribute consist lambda_c kappa_c youden_c tetra_c  tp_c  tn_c gammak
#>   <chr>       <dbl>    <dbl>   <dbl>    <dbl>   <dbl> <dbl> <dbl>  <dbl>
#> 1 att1        0.873    0.737   0.860    0.745   0.921 0.868 0.877  0.898
#> 2 att2        0.960    0.909   0.935    0.919   0.992 0.955 0.964  0.970
#> 3 att3        0.761    0.423   0.623    0.507   0.718 0.796 0.711  0.771
#> 4 att4        0.936    0.870   0.932    0.871   0.980 0.935 0.936  0.948
#> # ℹ 1 more variable: pc_prime <dbl>
#> 
#> 
#> $eap_reliability
#> # A tibble: 4 × 5
#>   attribute rho_pf rho_bs rho_i rho_tb
#>   <chr>      <dbl>  <dbl> <dbl>  <dbl>
#> 1 att1       0.799  0.797 0.647  0.949
#> 2 att2       0.937  0.938 0.717  0.995
#> 3 att3       0.619  0.537 0.480  0.748
#> 4 att4       0.902  0.897 0.700  0.987
```

There are several types of reliability evidence that are provided in the
output. For all indices reported, values can range from 0 to 1, where 1
represents perfect accuracy or consistency. The first type reliability
in the output is pattern-level reliability, as described by Cui et al.
([2012](#ref-cui2012)). This reliability describes the accuracy (`p_a`)
and consistency (`p_c`) of the classification of respondents into an
overall profile of proficiency on the on assessed skills. For example,
in a 3-attribute assessment there are 8 possible profiles: \[0,0,0\],
\[1,0,0\], \[0,1,0\], \[0,0,1\], \[1,1,0\], \[1,0,1\], \[0,1,1\],
\[1,1,1\]. One option for reporting results from a DCM-based assessment
is to select the profile that is most likely for each respondent. These
pattern-level reliability metrics provide a measure of the accuracy and
consistency for this type of classification.

On the other hand, rather than reporting results based on the overall
most likely profile, we can assign proficiency for each individual
attribute, and build up a overall profile from the attribute-level
decisions. This type of reporting may or may not result in the same
profile as the overall most likely profile. Because in this scenario
classifications are made at the attribute level, we need to examine the
classification accuracy and consistency for each individual attribute.
For models estimated with measr, this is referred to *maximum a
posteriori* (MAP) reliability, because classifications are based on the
most likely category for each attribute for each respondent. The MAP
values reported in the output are those described by Johnson & Sinharay
([2018](#ref-johnson2018)). The `acc` and `consist` variables in
`map_reliability$accuracy` and `map_reliability_consistency` tables,
respectively, are the classification accuracy and consistency metrics
described by Johnson & Sinharay ([2018](#ref-johnson2018)). In their
paper, they also compared these metrics to other measures of agreement
(e.g., Cohen’s κ, Goodman and Kruskal’s λ), which are also included in
the output. Johnson & Sinharay ([2018](#ref-johnson2018)) also provide
recommendations for threshold values for each metric that represent
poor, fair, good, very good, or excellent reliability.

Finally, rather than reporting results as classifications (e.g.,
proficient/not proficient), results can also be reported simply as the
probability that each respondent is proficient on each attribute. Thus,
rather than reporting the accuracy and consistency of a classification,
we should report the precision of the reported probability. This is
referred to as *expected a posteriori* (EAP) reliability, because the
probability represents the expected value of each attribute for each
respondent. The EAP values reported in the output (`eap_reliability`)
are those described by Johnson & Sinharay ([2020](#ref-johnson2020)) and
Templin & Bradshaw ([2013](#ref-templin2013)). Templin & Bradshaw
([2013](#ref-templin2013)) describe a reliability index based on a
restrictive assumption of parallel forms (`rho_tb`). Johnson & Sinharay
([2020](#ref-johnson2020)) described a more generalized parallel forms
reliability (`rho_pf`), along with additional biserial (`rho_bs`), and
informational (`rho_i`) reliability indices. Because proficiency
probabilities are provided at the attribute level, the EAP reliability
estimates are also provided for each attribute measured by the
assessment. As with the classification reliability indices, Johnson &
Sinharay ([2020](#ref-johnson2020)) provide recommendations for
thresholds representing poor, fair, good, very good, and excellent
reliability for all four EAP reliability indices.

## Storing Model Evaluations

If you have followed along with running the code in this vignette, you
will have noticed that some of the model evaluations take a significant
amount of computation time. This means repeating the calculations (e.g.,
didn’t assign the output new a new object, opened a new R session) can
be a very time-consuming process. To make you analysis more efficient,
measr offers several functions that can be used to add the model
evaluation metrics described in this vignette directly to the model
object. If you specified a `file` when the model was estimated, the
updated model object with the model evaluation components will
automatically resave, ensuring that you don’t have to rerun
computationally intensive tasks.

There are three functions for adding model evaluation components to a
model object, which correspond to the three types of evaluation
described in this vignette:

- [`add_fit()`](https://measr.info/dev/reference/model_evaluation.md):
  Adds absolute model fit indices (i.e., M₂, PPMCs)
- [`add_criterion()`](https://measr.info/dev/reference/model_evaluation.md):
  Adds relative model fit indices (i.e., LOO, WAIC)
- [`add_reliability()`](https://measr.info/dev/reference/model_evaluation.md):
  Adds reliability metrics

All three functions have several arguments in common.

- `x`: The model to add evaluation components to.
- `save`: Whether to resave the model object if `file` was specified
  when estimating the model. The default is `TRUE`.
- `overwrite`: Whether to overwrite existing evaluations. For example,
  if you attempt to add reliability metrics with
  [`add_reliability()`](https://measr.info/dev/reference/model_evaluation.md),
  but those metrics have already been added, should the reliability
  metrics be recalculated and overwrite the existing metrics? The
  default is `FALSE`.

Additionally, all three functions have a `...` argument for passing
additional arguments along to the relevant functions. For example, if we
want to add PPMC absolute model fit indices, we can specify the types of
model- and item-level fit indices to calculate, just as we did when
using [`fit_ppmc()`](https://measr.info/dev/reference/fit_ppmc.md).

``` r
lcdm <- add_fit(lcdm, method = "ppmc",
                model_fit = "raw_score",
                item_fit = "odds_ratio")
#> Warning in file.remove(current_files[!current_files %in% new_paths]):
#> cannot remove file
#> '/Users/jakethompson/Documents/GIT/packages/measr/vignettes/articles/fits/sim-lcdm-1.csv',
#> reason 'No such file or directory'
#> Warning in file.remove(current_files[!current_files %in% new_paths]):
#> cannot remove file
#> '/Users/jakethompson/Documents/GIT/packages/measr/vignettes/articles/fits/sim-lcdm-2.csv',
#> reason 'No such file or directory'
#> Warning in file.remove(current_files[!current_files %in% new_paths]):
#> cannot remove file
#> '/Users/jakethompson/Documents/GIT/packages/measr/vignettes/articles/fits/sim-lcdm-3.csv',
#> reason 'No such file or directory'
#> Warning in file.remove(current_files[!current_files %in% new_paths]):
#> cannot remove file
#> '/Users/jakethompson/Documents/GIT/packages/measr/vignettes/articles/fits/sim-lcdm-4.csv',
#> reason 'No such file or directory'
#> Moved 4 files and set internal paths to new locations:
#> - /home/runner/work/measr/measr/vignettes/articles/NA
#> - /home/runner/work/measr/measr/vignettes/articles/NA
#> - /home/runner/work/measr/measr/vignettes/articles/NA
#> - /home/runner/work/measr/measr/vignettes/articles/NA
```

Once components have been added to the model, a helper function,
[`measr_extract()`](https://measr.info/dev/reference/measr_extract.md),
can be used to pull out relevant pieces of output. For example, we can
extract the PPMC raw score results.

``` r
measr_extract(lcdm, what = "ppmc_raw_score")
#> # A tibble: 1 × 5
#>   obs_chisq ppmc_mean `2.5%` `97.5%`   ppp
#>       <dbl>     <dbl>  <dbl>   <dbl> <dbl>
#> 1      24.0      23.7   10.9    40.9 0.452
```

In addition, we can also extract elements of the model, such as the
priors that were used during estimation, or estimated parameters like
the base rate of membership in each class.

``` r
measr_extract(lcdm, what = "prior")
#> # A tibble: 4 × 3
#>   type        coefficient prior                      
#>   <chr>       <chr>       <chr>                      
#> 1 intercept   NA          normal(0, 2)               
#> 2 maineffect  NA          lognormal(0, 1)            
#> 3 interaction NA          normal(0, 2)               
#> 4 structural  Vc          dirichlet(rep_vector(1, C))

measr_extract(lcdm, what = "strc_param")
#> # A tibble: 16 × 2
#>    class            estimate
#>    <chr>          <rvar[1d]>
#>  1 [0,0,0,0]  0.059 ± 0.0069
#>  2 [1,0,0,0]  0.080 ± 0.0075
#>  3 [0,1,0,0]  0.063 ± 0.0065
#>  4 [0,0,1,0]  0.083 ± 0.0078
#>  5 [0,0,0,1]  0.052 ± 0.0086
#>  6 [1,1,0,0]  0.044 ± 0.0059
#>  7 [1,0,1,0]  0.051 ± 0.0066
#>  8 [1,0,0,1]  0.060 ± 0.0110
#>  9 [0,1,1,0]  0.083 ± 0.0078
#> 10 [0,1,0,1]  0.049 ± 0.0079
#> 11 [0,0,1,1]  0.081 ± 0.0100
#> 12 [1,1,1,0]  0.043 ± 0.0066
#> 13 [1,1,0,1]  0.044 ± 0.0095
#> 14 [1,0,1,1]  0.092 ± 0.0111
#> 15 [0,1,1,1]  0.047 ± 0.0085
#> 16 [1,1,1,1]  0.068 ± 0.0099
```

For a complete list of what can be extract with
[`measr_extract()`](https://measr.info/dev/reference/measr_extract.md),
see
[`?measr_extract`](https://measr.info/dev/reference/measr_extract.md).

## References

Bengio, Y., & Grandvalet, Y. (2004). No unbiased estimator of the
variance of k-fold cross-validation. *Journal of Machine Learning*, *5*,
1089–1105.
[http://www.jmlr.org/papers/v5/grandvalet04a.html](http://www.jmlr.org/papers/v5/grandvalet04a.md)

Cui, Y., Gierl, M. J., & Chang, H.-H. (2012). Estimating classification
consistency and accuracy for cognitive diagnostic assessment. *Journal
of Educational Measurement*, *49*(1), 19–38.
<https://doi.org/10.1111/j.1745-3984.2011.00158.x>

de la Torre, J., & Douglas, J. A. (2004). Higher-order latent trait
models for cognitive diagnosis. *Psychometrika*, *69*(3), 333–353.
<https://doi.org/10.1007/BF02295640>

Hansen, M., Cai, L., Monroe, S., & Li, Z. (2016). Limited-information
goodness-of-fit testing of diagnostic classification item response
models. *British Journal of Mahtematical and Statistical Psychology*,
*69*(3), 225–252. <https://doi.org/10.1111/bmsp.12074>

Henson, R., Templin, J., & Willse, J. T. (2009). Defining a family of
cognitive diagnosis models using log-linear models with latent
variables. *Psychometrika*, *74*(2), 191–210.
<https://doi.org/10.1007/s11336-008-9089-5>

Johnson, M. S., & Sinharay, S. (2018). Measures of agreement to assess
attribute-level classification accuracy and consistency for cognitive
diagnostic assessments. *Journal of Educational Measurement*, *55*(4),
635–664. <https://doi.org/10.1111/jedm.12196>

Johnson, M. S., & Sinharay, S. (2020). The reliability of the posterior
probability of skill attainment in diagnostic classification models.
*Journal of Educational and Behavioral Statistics*, *45*(1), 5–31.
<https://doi.org/10.3102/1076998619864550>

Liu, Y., Tian, W., & Xin, T. (2016). An application of M_2 statistic to
evaluate the fit of cognitive diagnostic models. *Journal of Educational
and Behavioral Statistics*, *41*(1), 3–26.
<https://doi.org/10.3102/1076998615621293>

Park, J. Y., Johnson, M. S., & Lee, Y.-S. (2015). Posterior predictive
model checks for cognitive diagnostic models. *International Journal of
Quantitative Research in Education*, *2*(3–4), 244–264.
<https://doi.org/10.1504/IJQRE.2015.071738>

Sinharay, S., & Almond, R. G. (2007). Assessing fit of cognitive
diagnostic models. *Educational and Psychological Measurement*, *67*(2),
239–257. <https://doi.org/10.1177/0013164406292025>

Sinharay, S., Johnson, M. S., & Stern, H. S. (2006). Posterior
predictive assessment of item response theory models. *Applied
Psychological Measurement*, *30*(4), 298–321.
<https://doi.org/10.1177/0146621605285517>

Templin, J., & Bradshaw, L. (2013). Measuring the reliability of
diagnostic classification model examinee estimates. *Journal of
Classification*, *30*(2), 251–275.
<https://doi.org/10.1007/s00357-013-9129-4>

Thompson, W. J. (2019). *Bayesian psychometrics for diagnostic
assessments: A proof of concept* (Research Report No. 19-01). University
of Kansas; Accessible Teaching, Learning, and Assessment Systems.
<https://doi.org/10.35542/osf.io/jzqs8>

Vehtari, A., Gelman, A., & Gabry, J. (2017). Practical Bayesian model
evaluation using leave-one-out cross-validation and WAIC. *Statistics
and Computing*, *27*, 1413–1432.
<https://doi.org/10.1007/s11222-016-9696-4>

Vehtari, A., Simpson, D., Gelman, A., Yao, Y., & Gabry, J. (2022).
*Pareto smoothed importance sampling*.
<https://doi.org/10.48550/arXiv.1507.02646>

Watanabe, S. (2010). Asymptotic equivalence of Bayes cross validation
and widely applicable information criterion in singular learning theory.
*Journal of Machine Learning Research*, *11*, 3571–3594.
[http://www.jmlr.org/papers/v11/watanabe10a.html](http://www.jmlr.org/papers/v11/watanabe10a.md)

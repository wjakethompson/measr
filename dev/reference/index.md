# Package index

## Model estimation

### Specification

Specify a diagnostic model, including measurement model, structural
model, and prior distributions.

- [`dcm_specify()`](https://dcmstan.r-dcm.org/reference/dcm_specify.html)
  : Specify a diagnostic classification model (from dcmstan)
- [`get_parameters()`](https://dcmstan.r-dcm.org/reference/get_parameters.html)
  : Identify parameters included in a diagnostic classification model
  (from dcmstan)
- [`prior()`](https://dcmstan.r-dcm.org/reference/prior.html)
  [`prior_string()`](https://dcmstan.r-dcm.org/reference/prior.html) :
  Prior definitions for diagnostic classification models (from dcmstan)
- [`default_dcm_priors()`](https://dcmstan.r-dcm.org/reference/default_dcm_priors.html)
  : Default priors for diagnostic classification models (from dcmstan)

### Estimation

Estimate the model using Markov chain Monte Carlo or Stan’s optimizer.

- [`dcm_estimate()`](https://measr.info/dev/reference/dcm_estimate.md) :
  Fit Bayesian diagnostic classification models

## Model evaluation

### Reliability

Estimate the pattern- or attribute-level classification accuracy and
consistency.

- [`reliability()`](https://measr.info/dev/reference/reliability.md) :
  Estimate the reliability of a diagnostic classification model
- [`cdi()`](https://measr.info/dev/reference/cdi.md) : Item, attribute,
  and test-level discrimination indices

### Model fit

Evaluate the fit of the estimated model to the observed data.

- [`fit_m2(`*`<measr::measrdcm>`*`)`](https://measr.info/dev/reference/m2.md)
  :

  Estimate the M₂ fit statistic for diagnostic classification models

- [`fit_ppmc()`](https://measr.info/dev/reference/fit_ppmc.md) :
  Posterior predictive model checks for assessing model fit

### Model assumptions

Evaluate assumptions of the estimated model

- [`qmatrix_validation()`](https://measr.info/dev/reference/qmatrix_validation.md)
  : Q-matrix validation
- [`yens_q3()`](https://measr.info/dev/reference/yens_q3.md) : Yen's
  \\Q_3\\ statistic for local item dependence

### Model comparisons

Assess the relative fit of two competing models.

- [`aic()`](https://measr.info/dev/reference/aic-bic.md)
  [`bic()`](https://measr.info/dev/reference/aic-bic.md) : Maximum
  likelihood based information criteria
- [`loo(`*`<measr::measrdcm>`*`)`](https://measr.info/dev/reference/loo-waic.md)
  [`waic(`*`<measr::measrdcm>`*`)`](https://measr.info/dev/reference/loo-waic.md)
  [`loo_compare(`*`<measr::measrdcm>`*`)`](https://measr.info/dev/reference/loo-waic.md)
  : Relative fit for Bayesian models
- [`loglik_array()`](https://measr.info/dev/reference/loglik_array.md) :
  Extract the log-likelihood of an estimated model
- [`log_mll()`](https://measr.info/dev/reference/log_mll.md) : Log
  marginal likelihood calculation
- [`bayes_factor()`](https://measr.info/dev/reference/bayes_factor.md) :
  Bayes factor for model comparisons

### Add evaluations to model objects

Add reliability, model fit, and model comparison information to an
estimated model object.

- [`add_criterion()`](https://measr.info/dev/reference/model_evaluation.md)
  [`add_reliability()`](https://measr.info/dev/reference/model_evaluation.md)
  [`add_fit()`](https://measr.info/dev/reference/model_evaluation.md)
  [`add_respondent_estimates()`](https://measr.info/dev/reference/model_evaluation.md)
  : Add model evaluation metrics model objects

## Model applications

View and use an estimated model.

- [`measr_extract()`](https://measr.info/dev/reference/measr_extract.md)
  :

  Extract components of a `measrfit` object

- [`score()`](https://measr.info/dev/reference/score.md) : Posterior
  draws of respondent proficiency

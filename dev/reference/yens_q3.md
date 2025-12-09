# Yen's \\Q_3\\ statistic for local item dependence

Calculate the \\Q_3\\ statistic to evaluate the assumption of
independent items.

## Usage

``` r
yens_q3(x, ..., crit_value = 0.2, summary = NULL)
```

## Arguments

- x:

  A [measrdcm](https://measr.info/dev/reference/dcm_estimate.md) object.

- ...:

  Unused.

- crit_value:

  The critical value threshold for flagging the residual correlation of
  a given item pair. The default is 0.2, as described by Chen and
  Thissen (1997).

- summary:

  A summary statistic to be returned. Must be one of `"q3max"` or
  `"q3star"` (see Details). If `NULL` (the default), no summary
  statistic is return, and all residual correlations are returned.

## Value

If `summary = NULL`, a tibble with the residual correlation and flags
for all item pairs. Otherwise, a numeric value representing the
requested summary statistic.

## Details

Psychometric models assume that items are independent of each other,
conditional on the latent trait. The \\Q_3\\ statistic (Yen, 1984) is
used to evaluate this assumption. For each observed item response, we
calculate the residual between the model predicted score and the
observed score and then estimate correlations between the residuals
across items. Each residual correlation is a \\Q_3\\ statistic.

Often, a critical values is used to flag a residual correlation above a
given threshold (e.g., Chen & Thissen, 1997). Alternatively, we may use
a summary statistic such as the maximum \\Q_3\\ statistic
(\\Q\_{3,max}\\; Christensen et al., 2017), defined as

\$\$Q\_{3,max} = \text{max}\_{i\>j}\left\|Q\_{3,ij}\right\|\$\$

Or the mean-adjusted maximum \\Q_3\\ statistic (\\Q\_{3,\*}\\; Marais,
2013), defined as

\$\$ \overline{Q}\_3 = \begin{pmatrix} I\\\\ 2\end{pmatrix}^{-1}
\displaystyle\sum\_{i\>j}Q\_{3,ij} \\ Q\_{3,\*} = Q\_{3,max} -
\overline{Q}\_3 \$\$

## References

Chen, W.-H., & Thissen, D. (1997). Local dependence indexes for item
pairs using item response theory. *Journal of Educational and Behavioral
Statistics, 22*(3), 265-389.
[doi:10.3102/10769986022003265](https://doi.org/10.3102/10769986022003265)

Christensen, K. B., Makransky, G., & Horton, M. (2017). Critical values
for Yen's Q3: Identification of local dependence in the Rasch model
using residual correlations. *Applied Psychological Measurement, 41*(3),
178-194.
[doi:10.1177/0146621616677520](https://doi.org/10.1177/0146621616677520)

Marais, I. (2013). Local dependence. In K. B. Christensen, S. Kreiner, &
M. Mesbah (Eds.), *Rasch models in health* (pp. 111-130). Wiley.

Yen, W. M. (1984). Effects of local item dependence on the fit and
equating performance of the three-parameter logistic model. *Applied
Psychological Measurement, 8*(2), 125-145.
[doi:10.1177/014662168400800201](https://doi.org/10.1177/014662168400800201)

## Examples

``` r
model_spec <- dcm_specify(qmatrix = dcmdata::mdm_qmatrix,
                          identifier = "item")
model <- dcm_estimate(dcm_spec = model_spec, data = dcmdata::mdm_data,
                      identifier = "respondent", method = "optim",
                      seed = 63277)

yens_q3(model)
#> # A tibble: 6 × 4
#>   item_1 item_2 resid_corr flag 
#>   <chr>  <chr>       <dbl> <lgl>
#> 1 mdm1   mdm2      -0.132  FALSE
#> 2 mdm1   mdm3      -0.0910 FALSE
#> 3 mdm1   mdm4      -0.123  FALSE
#> 4 mdm2   mdm3      -0.110  FALSE
#> 5 mdm2   mdm4      -0.249  TRUE 
#> 6 mdm3   mdm4      -0.0988 FALSE
```


<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyfit <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->
<!-- badges: end -->

`tidyfit` is an `R`-package that facilitates and automates linear
regression and classification modeling in a tidy environment. The
package includes several methods, such as Lasso, PLS and ElasticNet
regressions. `tidyfit` builds on the `tidymodels` suite, but emphasizes
automated modeling with a focus on grouped data, model comparisons, and
high-volume analytics with standardized input/output interfaces. The
objective is to make model fitting, cross validation and model output
very simple and standardized across all methods, with any necessary
method-specific transformations handled in the background.

## Installation

You can install the development version of tidyfit from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jpfitzinger/tidyfit")
library(tidyfit)
```

## Overview and usage

`tidyfit` includes 3 deceptively simple functions:

-   `regress()`
-   `classify()`
-   `m()`

All 3 of these functions return a `tidyfit.models` frame, which is a
tibble containing information about fitted regression and classification
models. `regress` and `classify` perform regression and classification
on tidy data. The functions ingest a tibble, prepare input data for the
models by splitting groups, partitioning cross validation slices and
handling any necessary adjustments and transformations. The data is
ultimately passed to the model wrapper `m()` which fits the models. The
basic usage is as follows:

``` r
regress(
  .data, 
  formula = y ~ x1 + x2, 
  mod1 = m(<args for underlying method>), mod2 = m(), ...,    # Pass multiple model wrappers
  .cv = "vfold", .cv_args = list(), .weights = "weight_col",  # Additional settings
  <further arguments>
)
```

The syntax is identical for `classify`.

`m` is a powerful wrapper for many different regression and
classification techniques that can be used with `regress` and
`classify`, or as a stand-alone function:

``` r
m(
  <method>,           # e.g. "lm" or "lasso"
  formula, data,      # not passed when used within regress or classify
  ...                 # Args passed to underlying method, e.g. to stats::lm for OLS regression
)
```

An important feature of `m()` is that all arguments can be passed as
vectors, allowing generalized hyperparameter tuning or scenario analysis
for any method:

-   Passing a hyperparameter grid:
    `m("lasso", lambda = seq(0, 1, by = 0.1))`
-   Different algorithms for robust regression:
    `m("robust", method = c("M", "MM"))`
-   Forward vs. backward selection:
    `m("subset", method = c("forward", "backward"))`
-   Logit and Probit models:
    `m("glm", family = list(binomial(link="logit"), binomial(link="probit")))`

Arguments that are meant to be vectors (e.g. weights) are recognized by
the function and not interpreted as grids.

## Methods implemented in `tidyfit`

`tidyfit` currently implements the following methods:

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Method
</th>
<th style="text-align:center;">
Name
</th>
<th style="text-align:center;">
Package
</th>
<th style="text-align:center;">
Regression
</th>
<th style="text-align:center;">
Classification
</th>
</tr>
</thead>
<tbody>
<tr grouplength="4">
<td colspan="5" style="border-bottom: 1px solid;">
<strong>Linear (generalized) regression or classification</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
OLS
</td>
<td style="text-align:center;">
lm
</td>
<td style="text-align:center;">
`stats::lm`
</td>
<td style="text-align:center;">
yes
</td>
<td style="text-align:center;">
no
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
Generalized least squares
</td>
<td style="text-align:center;">
glm
</td>
<td style="text-align:center;">
`stats::glm`
</td>
<td style="text-align:center;">
yes
</td>
<td style="text-align:center;">
yes
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
Robust regression (e.g. Huber loss)
</td>
<td style="text-align:center;">
robust
</td>
<td style="text-align:center;">
`MASS::rlm`
</td>
<td style="text-align:center;">
yes
</td>
<td style="text-align:center;">
no
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
Quantile regression
</td>
<td style="text-align:center;">
quantile
</td>
<td style="text-align:center;">
`quantreg`
</td>
<td style="text-align:center;">
yes
</td>
<td style="text-align:center;">
no
</td>
</tr>
<tr grouplength="4">
<td colspan="5" style="border-bottom: 1px solid;">
<strong>Regression and classification with L1 and L2 penalties</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
LASSO
</td>
<td style="text-align:center;">
lasso
</td>
<td style="text-align:center;">
`glmnet`
</td>
<td style="text-align:center;">
yes
</td>
<td style="text-align:center;">
yes
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
Ridge
</td>
<td style="text-align:center;">
ridge
</td>
<td style="text-align:center;">
`glmnet`
</td>
<td style="text-align:center;">
yes
</td>
<td style="text-align:center;">
yes
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
Adaptive LASSO
</td>
<td style="text-align:center;">
adalasso
</td>
<td style="text-align:center;">
`glmnet`
</td>
<td style="text-align:center;">
yes
</td>
<td style="text-align:center;">
yes
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
ElasticNet
</td>
<td style="text-align:center;">
enet
</td>
<td style="text-align:center;">
`glmnet`
</td>
<td style="text-align:center;">
yes
</td>
<td style="text-align:center;">
yes
</td>
</tr>
<tr grouplength="1">
<td colspan="5" style="border-bottom: 1px solid;">
<strong>Gradient boosting</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
Gradient boosting regression
</td>
<td style="text-align:center;">
boost
</td>
<td style="text-align:center;">
`mboost`
</td>
<td style="text-align:center;">
yes
</td>
<td style="text-align:center;">
yes
</td>
</tr>
<tr grouplength="3">
<td colspan="5" style="border-bottom: 1px solid;">
<strong>Factor regressions</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
Principal components regression
</td>
<td style="text-align:center;">
pcr
</td>
<td style="text-align:center;">
`pls`
</td>
<td style="text-align:center;">
yes
</td>
<td style="text-align:center;">
no
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
Partial least squares
</td>
<td style="text-align:center;">
plsr
</td>
<td style="text-align:center;">
`pls`
</td>
<td style="text-align:center;">
yes
</td>
<td style="text-align:center;">
no
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
Hierarchical feature regression
</td>
<td style="text-align:center;">
hfr
</td>
<td style="text-align:center;">
`hfr`
</td>
<td style="text-align:center;">
yes
</td>
<td style="text-align:center;">
no
</td>
</tr>
<tr grouplength="1">
<td colspan="5" style="border-bottom: 1px solid;">
<strong>Best subset selection</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
Best subset selection
</td>
<td style="text-align:center;">
subset
</td>
<td style="text-align:center;">
`bestglm`
</td>
<td style="text-align:center;">
yes
</td>
<td style="text-align:center;">
yes
</td>
</tr>
<tr grouplength="2">
<td colspan="5" style="border-bottom: 1px solid;">
<strong>Bayesian regression</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
Bayesian regression
</td>
<td style="text-align:center;">
bayes
</td>
<td style="text-align:center;">
`arm`
</td>
<td style="text-align:center;">
yes
</td>
<td style="text-align:center;">
yes
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
Bayesian time-varying parameters regression
</td>
<td style="text-align:center;">
tvp
</td>
<td style="text-align:center;">
`shrinkTVP`
</td>
<td style="text-align:center;">
yes
</td>
<td style="text-align:center;">
no
</td>
</tr>
<tr grouplength="1">
<td colspan="5" style="border-bottom: 1px solid;">
<strong>Mixed-effects modeling</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
Generalized mixed-effects regression
</td>
<td style="text-align:center;">
glmm
</td>
<td style="text-align:center;">
`lme4::glmer`
</td>
<td style="text-align:center;">
yes
</td>
<td style="text-align:center;">
yes
</td>
</tr>
</tbody>
</table>

See `?m` for additional information.

It is important to note that the above list is not complete, since some
of the methods encompass multiple algorithms. For instance, “subset” can
be used to perform forward, backward or exhaustive search selection
using `leaps`. Similarly, “lasso” includes certain grouped LASSO
implementations that can be fitted with `glmnet`.

## A minimal workflow

In this section, a minimal workflow is used to demonstrate how the
package works. For more detailed guides of specialized topics, or simply
for further reading, follow these links:

-   [Accessing fitted
    models](https://tidyfit.unchartedml.com/articles/Accessing_Fitted_Model_Objects.html)
-   [Regularized
    regression](https://tidyfit.unchartedml.com/articles/Predicting_Boston_House_Prices.html)
    (Boston house price data)
-   [Multinomial
    classification](https://tidyfit.unchartedml.com/articles/Multinomial_Classification.html)
    (iris data)
-   [Rolling window regression for time
    series](https://tidyfit.unchartedml.com/articles/Rolling_Window_Time_Series_Regression.html)
    (factor data)
-   [Time-varying
    parameters](https://tidyfit.unchartedml.com/articles/Time-varying_parameters_vs_rolling_windows.html)
    (factor data)
-   [Bootstrap confidence
    intervals](https://tidyfit.unchartedml.com/articles/Bootstrapping_Confidence_Intervals.html)
-   \[coming soon\] Fixed and Random effects
-   \[coming soon\] Quantile regression
-   \[coming soon\] Custom models

`tidyfit` includes a data set of financial Fama-French factor returns
freely available
[here](https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html).
The data set includes monthly industry returns for 10 industries, as
well as monthly factor returns for 5 factors:

``` r
data <- tidyfit::Factor_Industry_Returns
data
#> # A tibble: 7,080 × 9
#>      Date Industry Return `Mkt-RF`   SMB   HML   RMW   CMA    RF
#>     <dbl> <chr>     <dbl>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 196307 NoDur     -0.49    -0.39 -0.44 -0.89  0.68 -1.23  0.27
#>  2 196308 NoDur      4.89     5.07 -0.75  1.68  0.36 -0.34  0.25
#>  3 196309 NoDur     -1.69    -1.57 -0.55  0.08 -0.71  0.29  0.27
#>  4 196310 NoDur      2.65     2.53 -1.37 -0.14  2.8  -2.02  0.29
#>  5 196311 NoDur     -1.13    -0.85 -0.89  1.81 -0.51  2.31  0.27
#>  6 196312 NoDur      2.81     1.83 -2.07 -0.08  0.03 -0.04  0.29
#>  7 196401 NoDur      0.79     2.24  0.11  1.47  0.17  1.51  0.3 
#>  8 196402 NoDur      1.87     1.54  0.3   2.74 -0.05  0.9   0.26
#>  9 196403 NoDur      3.08     1.41  1.36  3.36 -2.21  3.19  0.31
#> 10 196404 NoDur     -0.48     0.1  -1.59 -0.58 -1.27 -1.04  0.29
#> # … with 7,070 more rows
```

We will only use a subset of the data to keep things simple:

``` r
df_train <- data %>% 
  filter(Date > 201800 & Date < 202000)
df_test <- data %>% 
  filter(Date >= 202000)
```

For purposes of this demonstration, the objective will be to fit an
ElasticNet regression for each industry group, and compare results to a
robust least squares regression. This can be done with `regress` after
grouping the data. For grouped data, the functions `regress` and
`classify` estimate models for each group independently:

``` r
model_frame <- df_train %>% 
  group_by(Industry) %>% 
  regress(Return ~ ., 
          m("enet"), 
          m("robust", method = "MM", psi = MASS::psi.huber), 
          .cv = "initial_time_split", .mask = "Date")
```

Note that the penalty and mixing parameters are chosen automatically
using a time series train/test split (`rsample::initial_time_split`),
with a hyperparameter grid set by the package `dials`. See `?regress`
for additional CV methods. A custom grid can easily be passed using
`lambda = c(...)` and/or `alpha = c(...)` in the `m("enet")` wrapper.

The resulting `tidyfit.models` frame consists of 3 components:

1.  A group of identifying columns. This includes the `Industry` column,
    the model name, and grid ID (the ID for the model settings used).
2.  A `handler` column. This column contains a partialised function
    (created with `purrr::partial`) that contains the model object as
    well as any additional information needed to perform predictions or
    access coefficients (e.g. the sample mean and s.d. for rescaling
    coefficients)
3.  `settings`, as well as (if applicable) `messages` and `warnings`.

``` r
subset_mod_frame <- model_frame %>% 
  filter(Industry %in% unique(Industry)[1:2])
subset_mod_frame
#> # A tibble: 4 × 7
#>   Industry model  estimator      size          grid_id  handler    settings
#>   <chr>    <chr>  <chr>          <objct_sz>    <chr>    <list>     <list>  
#> 1 Durbl    enet   glmnet::glmnet 1208536 bytes #005.002 <prrr_fn_> <tibble>
#> 2 Enrgy    enet   glmnet::glmnet 1208872 bytes #001.002 <prrr_fn_> <tibble>
#> 3 Durbl    robust MASS::rlm      64728 bytes   #0010000 <prrr_fn_> <tibble>
#> 4 Enrgy    robust MASS::rlm      64816 bytes   #0010000 <prrr_fn_> <tibble>
```

Let’s unnest the settings columns:

``` r
subset_mod_frame %>% 
  tidyr::unnest(settings, keep_empty = TRUE)
#> # A tibble: 4 × 11
#>   Industry model  estimator  size  grid_id handler    family lambda alpha method
#>   <chr>    <chr>  <chr>      <obj> <chr>   <list>     <chr>   <dbl> <dbl> <chr> 
#> 1 Durbl    enet   glmnet::g… 1208… #005.0… <prrr_fn_> gauss…  0.792     1 <NA>  
#> 2 Enrgy    enet   glmnet::g… 1208… #001.0… <prrr_fn_> gauss…  0.792     0 <NA>  
#> 3 Durbl    robust MASS::rlm  6472… #00100… <prrr_fn_> <NA>   NA        NA MM    
#> 4 Enrgy    robust MASS::rlm  6481… #00100… <prrr_fn_> <NA>   NA        NA MM    
#> # … with 1 more variable: psi <list>
```

The `tidyfit.models` frame can be used to access additional information.
Specifically, we can do 3 things:

1.  Access the fitted model
2.  Predict
3.  Access a tibble of estimated parameters

To **access the fitted models**, we need to call the handler function
without any arguments (see
[here](https://tidyfit.unchartedml.com/articles/Accessing_Fitted_Model_Objects.html)
for another example):

``` r
subset_mod_frame %>% 
  mutate(fitted_model = map(handler, ~.()))
#> # A tibble: 4 × 8
#>   Industry model  estimator      size        grid_id handler    settings fitte…¹
#>   <chr>    <chr>  <chr>          <objct_sz>  <chr>   <list>     <list>   <list> 
#> 1 Durbl    enet   glmnet::glmnet 1208536 by… #005.0… <prrr_fn_> <tibble> <elnet>
#> 2 Enrgy    enet   glmnet::glmnet 1208872 by… #001.0… <prrr_fn_> <tibble> <elnet>
#> 3 Durbl    robust MASS::rlm      64728 bytes #00100… <prrr_fn_> <tibble> <rlm>  
#> 4 Enrgy    robust MASS::rlm      64816 bytes #00100… <prrr_fn_> <tibble> <rlm>  
#> # … with abbreviated variable name ¹​fitted_model
```

To **predict**, we need data with the same columns as the input data and
simply use the generic `predict` function. Groups are respected and if
the response variable is in the data, it is included as a `truth` column
in the resulting object:

``` r
predict(subset_mod_frame, data)
#> # A tibble: 2,832 × 5
#> # Groups:   Industry, model [4]
#>    Industry model prediction truth size         
#>    <chr>    <chr>      <dbl> <dbl> <objct_sz>   
#>  1 Durbl    enet      -0.897 -0.22 1208536 bytes
#>  2 Durbl    enet       5.11   6.55 1208536 bytes
#>  3 Durbl    enet      -1.85  -0.24 1208536 bytes
#>  4 Durbl    enet       2.16   9.72 1208536 bytes
#>  5 Durbl    enet      -0.739 -4.84 1208536 bytes
#>  6 Durbl    enet       1.48   0.27 1208536 bytes
#>  7 Durbl    enet       2.26   1.19 1208536 bytes
#>  8 Durbl    enet       1.86   2.14 1208536 bytes
#>  9 Durbl    enet       1.88   0.93 1208536 bytes
#> 10 Durbl    enet      -0.351  1.93 1208536 bytes
#> # … with 2,822 more rows
```

Finally, we can obtain a **tidy frame of the coefficients** using the
generic `coef` function:

``` r
estimates <- coef(subset_mod_frame)
estimates
#> # A tibble: 25 × 5
#> # Groups:   Industry, model [4]
#>    Industry model term        estimate model_info      
#>    <chr>    <chr> <chr>          <dbl> <list>          
#>  1 Durbl    enet  (Intercept) -0.302   <tibble [1 × 5]>
#>  2 Durbl    enet  `Mkt-RF`     0.992   <tibble [1 × 5]>
#>  3 Durbl    enet  SMB          0.00978 <tibble [1 × 5]>
#>  4 Durbl    enet  HML          0.229   <tibble [1 × 5]>
#>  5 Enrgy    enet  (Intercept)  1.47    <tibble [1 × 5]>
#>  6 Enrgy    enet  `Mkt-RF`     1.13    <tibble [1 × 5]>
#>  7 Enrgy    enet  SMB          0.649   <tibble [1 × 5]>
#>  8 Enrgy    enet  HML          0.0703  <tibble [1 × 5]>
#>  9 Enrgy    enet  RMW         -0.552   <tibble [1 × 5]>
#> 10 Enrgy    enet  CMA          1.16    <tibble [1 × 5]>
#> # … with 15 more rows
```

The estimates contain additional method-specific information that is
nested in `model_info`. This can include standard errors, t-values and
similar information:

``` r
tidyr::unnest(estimates, model_info)
#> # A tibble: 25 × 9
#> # Groups:   Industry, model [4]
#>    Industry model term        estimate size       lambda dev.r…¹ std.e…² stati…³
#>    <chr>    <chr> <chr>          <dbl> <objct_sz>  <dbl>   <dbl>   <dbl>   <dbl>
#>  1 Durbl    enet  (Intercept) -0.302   1208536 b…  0.792   0.735      NA      NA
#>  2 Durbl    enet  `Mkt-RF`     0.992   1208536 b…  0.792   0.735      NA      NA
#>  3 Durbl    enet  SMB          0.00978 1208536 b…  0.792   0.735      NA      NA
#>  4 Durbl    enet  HML          0.229   1208536 b…  0.792   0.735      NA      NA
#>  5 Enrgy    enet  (Intercept)  1.47    1208872 b…  0.792   0.807      NA      NA
#>  6 Enrgy    enet  `Mkt-RF`     1.13    1208872 b…  0.792   0.807      NA      NA
#>  7 Enrgy    enet  SMB          0.649   1208872 b…  0.792   0.807      NA      NA
#>  8 Enrgy    enet  HML          0.0703  1208872 b…  0.792   0.807      NA      NA
#>  9 Enrgy    enet  RMW         -0.552   1208872 b…  0.792   0.807      NA      NA
#> 10 Enrgy    enet  CMA          1.16    1208872 b…  0.792   0.807      NA      NA
#> # … with 15 more rows, and abbreviated variable names ¹​dev.ratio, ²​std.error,
#> #   ³​statistic
```

Suppose we would like to evaluate the relative performance of the two
methods. This becomes exceedingly simple using the `yardstick` package:

``` r
model_frame %>% 
  # Generate predictions
  predict(df_test) %>% 
  # Calculate RMSE
  yardstick::rmse(truth, prediction) %>% 
  # Plot
  ggplot(aes(Industry, .estimate)) +
  geom_col(aes(fill = model), position = position_dodge()) +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-14-1.png" width="100%" style="display: block; margin: auto;" />

The ElasticNet performs a little better (unsurprising really, given the
small data set).

A **more detailed regression analysis of Boston house price data** using
a panel of regularized regression estimators can be found
[here](https://tidyfit.unchartedml.com/articles/Predicting_Boston_House_Prices.html).

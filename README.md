
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
`quantreg::rq`
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
`glmnet::glmnet`
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
`glmnet::glmnet`
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
`glmnet::glmnet`
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
`glmnet::glmnet`
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
`mboost::glmboost`
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
`pls::plsr`
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
`pls::pcr`
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
`hfr::hfr`
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
`bestglm::bestglm`
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
`arm::bayesglm`
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
`shrinkTVP::shrinkTVP`
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
<tr grouplength="1">
<td colspan="5" style="border-bottom: 1px solid;">
<strong>Specialized time series methods</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
Markov-switching regression
</td>
<td style="text-align:center;">
mslm
</td>
<td style="text-align:center;">
`MSwM::msmFit`
</td>
<td style="text-align:center;">
yes
</td>
<td style="text-align:center;">
no
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

Before beginning with the estimation, we activate the progress bar
visualization. This allows us to gauge estimation progress along the
way. `tidyfit` uses the `progressr`-package internally to generate a
progress bar:

``` r
progressr::handlers(global=TRUE)
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
#>   Industry model  estimator      `size (MB)` grid_id  model_object settings
#>   <chr>    <chr>  <chr>                <dbl> <chr>    <list>       <list>  
#> 1 Enrgy    enet   glmnet::glmnet      1.21   #001|002 <tidyFit>    <tibble>
#> 2 Utils    enet   glmnet::glmnet      1.21   #001|001 <tidyFit>    <tibble>
#> 3 Enrgy    robust MASS::rlm           0.0648 #0010000 <tidyFit>    <tibble>
#> 4 Utils    robust MASS::rlm           0.0648 #0010000 <tidyFit>    <tibble>
```

Let’s unnest the settings columns:

``` r
subset_mod_frame %>% 
  tidyr::unnest(settings, keep_empty = TRUE)
#> # A tibble: 4 × 11
#>   Industry model  estimator size …¹ grid_id model_o…² alpha family lambda method
#>   <chr>    <chr>  <chr>       <dbl> <chr>   <list>    <dbl> <chr>   <dbl> <chr> 
#> 1 Enrgy    enet   glmnet::…  1.21   #001|0… <tidyFit>     0 gauss…  0.792 <NA>  
#> 2 Utils    enet   glmnet::…  1.21   #001|0… <tidyFit>     0 gauss…  1     <NA>  
#> 3 Enrgy    robust MASS::rlm  0.0648 #00100… <tidyFit>    NA <NA>   NA     MM    
#> 4 Utils    robust MASS::rlm  0.0648 #00100… <tidyFit>    NA <NA>   NA     MM    
#> # … with 1 more variable: psi <list>, and abbreviated variable names
#> #   ¹​`size (MB)`, ²​model_object
```

The `tidyfit.models` frame can be used to access additional information.
Specifically, we can do 3 things:

1.  Access the fitted model
2.  Predict
3.  Access a tibble of estimated parameters

The **fitted tidyFit models** are stored as an `R6` class in the
`model_object` column and can be addressed directly with generics such
as `coef` or `summary`. The underlying object (e.g. an `lm` class fitted
model) is given in `...$object` (see
[here](https://tidyfit.unchartedml.com/articles/Accessing_Fitted_Model_Objects.html)
for another example):

``` r
subset_mod_frame %>% 
  mutate(fitted_model = map(model_object, ~.$object))
#> # A tibble: 4 × 8
#>   Industry model  estimator      `size (MB)` grid_id  model_o…¹ settings fitte…²
#>   <chr>    <chr>  <chr>                <dbl> <chr>    <list>    <list>   <list> 
#> 1 Enrgy    enet   glmnet::glmnet      1.21   #001|002 <tidyFit> <tibble> <elnet>
#> 2 Utils    enet   glmnet::glmnet      1.21   #001|001 <tidyFit> <tibble> <elnet>
#> 3 Enrgy    robust MASS::rlm           0.0648 #0010000 <tidyFit> <tibble> <rlm>  
#> 4 Utils    robust MASS::rlm           0.0648 #0010000 <tidyFit> <tibble> <rlm>  
#> # … with abbreviated variable names ¹​model_object, ²​fitted_model
```

To **predict**, we need data with the same columns as the input data and
simply use the generic `predict` function. Groups are respected and if
the response variable is in the data, it is included as a `truth` column
in the resulting object:

``` r
predict(subset_mod_frame, data)
#> # A tibble: 2,832 × 4
#> # Groups:   Industry, model [4]
#>    Industry model prediction truth
#>    <chr>    <chr>      <dbl> <dbl>
#>  1 Enrgy    enet      -4.75   2.29
#>  2 Enrgy    enet       2.90   3.94
#>  3 Enrgy    enet      -3.56  -3.64
#>  4 Enrgy    enet      -4.34  -0.32
#>  5 Enrgy    enet      -0.613 -1.16
#>  6 Enrgy    enet      -1.76   4.65
#>  7 Enrgy    enet       1.81   4.84
#>  8 Enrgy    enet       1.18   1.06
#>  9 Enrgy    enet       4.94   1.4 
#> 10 Enrgy    enet      -3.89   4.03
#> # … with 2,822 more rows
```

Finally, we can obtain a **tidy frame of the coefficients** using the
generic `coef` function:

``` r
estimates <- coef(subset_mod_frame)
estimates
#> # A tibble: 28 × 5
#> # Groups:   Industry, model [4]
#>    Industry model term        estimate model_info      
#>    <chr>    <chr> <chr>          <dbl> <list>          
#>  1 Enrgy    enet  (Intercept)   1.47   <tibble [1 × 4]>
#>  2 Enrgy    enet  Mkt-RF        1.13   <tibble [1 × 4]>
#>  3 Enrgy    enet  SMB           0.649  <tibble [1 × 4]>
#>  4 Enrgy    enet  HML           0.0703 <tibble [1 × 4]>
#>  5 Enrgy    enet  RMW          -0.552  <tibble [1 × 4]>
#>  6 Enrgy    enet  CMA           1.16   <tibble [1 × 4]>
#>  7 Enrgy    enet  RF          -13.5    <tibble [1 × 4]>
#>  8 Utils    enet  (Intercept)  -1.50   <tibble [1 × 4]>
#>  9 Utils    enet  Mkt-RF        0.229  <tibble [1 × 4]>
#> 10 Utils    enet  SMB           0.343  <tibble [1 × 4]>
#> # … with 18 more rows
```

The estimates contain additional method-specific information that is
nested in `model_info`. This can include standard errors, t-values and
similar information:

``` r
tidyr::unnest(estimates, model_info)
#> # A tibble: 28 × 8
#> # Groups:   Industry, model [4]
#>    Industry model term        estimate lambda dev.ratio std.error statistic
#>    <chr>    <chr> <chr>          <dbl>  <dbl>     <dbl>     <dbl>     <dbl>
#>  1 Enrgy    enet  (Intercept)   1.47    0.792     0.807        NA        NA
#>  2 Enrgy    enet  Mkt-RF        1.13    0.792     0.807        NA        NA
#>  3 Enrgy    enet  SMB           0.649   0.792     0.807        NA        NA
#>  4 Enrgy    enet  HML           0.0703  0.792     0.807        NA        NA
#>  5 Enrgy    enet  RMW          -0.552   0.792     0.807        NA        NA
#>  6 Enrgy    enet  CMA           1.16    0.792     0.807        NA        NA
#>  7 Enrgy    enet  RF          -13.5     0.792     0.807        NA        NA
#>  8 Utils    enet  (Intercept)  -1.50    1         0.445        NA        NA
#>  9 Utils    enet  Mkt-RF        0.229   1         0.445        NA        NA
#> 10 Utils    enet  SMB           0.343   1         0.445        NA        NA
#> # … with 18 more rows
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

<img src="man/figures/README-unnamed-chunk-15-1.png" width="100%" style="display: block; margin: auto;" />

The ElasticNet performs a little better (unsurprising really, given the
small data set).

A **more detailed regression analysis of Boston house price data** using
a panel of regularized regression estimators can be found
[here](https://tidyfit.unchartedml.com/articles/Predicting_Boston_House_Prices.html).


<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyfit

<!-- badges: start -->
<!-- badges: end -->

<img src="./man/figures/logo_nobackground.png" style="width:30.0%" />

`tidyfit` is an `R`-package that facilitates and automates linear
regression and classification modeling in a tidy environment. The
package includes several methods, such as Lasso, PLS and ElasticNet
regressions. `tidyfit` builds on the `tidymodels` suite, but emphasizes
automated modeling with a focus on the linear regression and
classification coefficients, which are the primary output of `tidyfit`.
The primary objective is to make model fitting, cross validation and
model output very simple and standardized across all methods, with
method-specific transformations handled in the background.

## Installation

You can install the development version of tidyfit from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jpfitzinger/tidyfit")
```

## Example

### Fama-French factor and industry data

`tidyfit` includes a data set of financial factor returns freely
available
[here](https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html).
The data set includes monthly industry returns for 10 industries, as
well as monthly factor returns for 5 factors:

``` r
library(tidyfit)
data <- tidyfit::Factor_Industry_Returns
```

### Fitting a linear regression

Models are fitted using `tidyfit::regress` for regression or
`tidyfit::classify` for binomial classification problems. Below a linear
regression is fitted using the `tidyfit::m` model wrapper, which
standardizes a large number of regression and classification models. The
date columns is masked and the industry column is one-hot encoded:

``` r
fit <- data %>% 
  regress(Return ~ ., lin_reg = m("lm"), .mask = "Date")
fit
#> # A tibble: 16 × 4
#>    variable          beta model   model_info      
#>    <chr>            <dbl> <chr>   <list>          
#>  1 (Intercept)   -0.00408 lin_reg <tibble [1 × 5]>
#>  2 IndustryEnrgy -0.00409 lin_reg <tibble [1 × 5]>
#>  3 IndustryHiTec  0.0559  lin_reg <tibble [1 × 5]>
#>  4 IndustryHlth   0.0506  lin_reg <tibble [1 × 5]>
#>  5 IndustryManuf -0.0469  lin_reg <tibble [1 × 5]>
#>  6 IndustryNoDur  0.0171  lin_reg <tibble [1 × 5]>
#>  7 IndustryOther -0.0707  lin_reg <tibble [1 × 5]>
#>  8 IndustryShops  0.0405  lin_reg <tibble [1 × 5]>
#>  9 IndustryTelcm -0.184   lin_reg <tibble [1 × 5]>
#> 10 IndustryUtils -0.181   lin_reg <tibble [1 × 5]>
#> 11 CMA            0.117   lin_reg <tibble [1 × 5]>
#> 12 HML            0.0601  lin_reg <tibble [1 × 5]>
#> 13 `Mkt-RF`       0.977   lin_reg <tibble [1 × 5]>
#> 14 RF             1.01    lin_reg <tibble [1 × 5]>
#> 15 RMW            0.164   lin_reg <tibble [1 × 5]>
#> 16 SMB            0.0178  lin_reg <tibble [1 × 5]>
```

Detailed model and hyperparameter information is nested and can be
expanded:

``` r
fit %>% 
  unnest(model_info)
#> # A tibble: 16 × 8
#>    variable          beta model   family      s.e. `t value` `p value` Adj. R-…¹
#>    <chr>            <dbl> <chr>   <list>     <dbl>     <dbl>     <dbl>     <dbl>
#>  1 (Intercept)   -0.00408 lin_reg <family> 0.133     -0.0306  9.76e- 1     0.625
#>  2 IndustryEnrgy -0.00409 lin_reg <family> 0.172     -0.0237  9.81e- 1     0.625
#>  3 IndustryHiTec  0.0559  lin_reg <family> 0.172      0.325   7.45e- 1     0.625
#>  4 IndustryHlth   0.0506  lin_reg <family> 0.172      0.294   7.69e- 1     0.625
#>  5 IndustryManuf -0.0469  lin_reg <family> 0.172     -0.272   7.85e- 1     0.625
#>  6 IndustryNoDur  0.0171  lin_reg <family> 0.172      0.0994  9.21e- 1     0.625
#>  7 IndustryOther -0.0707  lin_reg <family> 0.172     -0.411   6.81e- 1     0.625
#>  8 IndustryShops  0.0405  lin_reg <family> 0.172      0.235   8.14e- 1     0.625
#>  9 IndustryTelcm -0.184   lin_reg <family> 0.172     -1.07    2.85e- 1     0.625
#> 10 IndustryUtils -0.181   lin_reg <family> 0.172     -1.05    2.93e- 1     0.625
#> 11 CMA            0.117   lin_reg <family> 0.0281     4.18    2.94e- 5     0.625
#> 12 HML            0.0601  lin_reg <family> 0.0182     3.31    9.30e- 4     0.625
#> 13 `Mkt-RF`       0.977   lin_reg <family> 0.00985   99.3     0            0.625
#> 14 RF             1.01    lin_reg <family> 0.145      6.99    2.91e-12     0.625
#> 15 RMW            0.164   lin_reg <family> 0.0191     8.56    1.41e-17     0.625
#> 16 SMB            0.0178  lin_reg <family> 0.0140     1.27    2.03e- 1     0.625
#> # … with abbreviated variable name ¹​`Adj. R-squared`
```

Now, instead of fitting a single regression, we need to fit a regression
per industry. This is achieved simply by grouping:

``` r
fit <- data %>% 
  group_by(Industry) %>% 
  regress(Return ~ ., lin_reg = m("lm"), .mask = "Date")
```

Let’s plot the factor loadings in a heatmap:

``` r
fit %>% 
  ggplot(aes(variable, Industry)) +
  geom_tile(aes(fill = beta)) +
  scale_fill_gradient2(low = "firebrick", high = "forestgreen")
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" style="display: block; margin: auto;" />

### Fitting a Lasso regression

Fitting a Lasso regression requires hyperparameter tuning for the
penalty `lambda`. This can be done by passing values to `.cv` and
`.cv_args`. Cross validation is performed using `rsample`. See
`?rsample::vfold_cv`, `?rsample::loo_cv`, `?rsample::initial_split`,
`?rsample::initial_time_split` or `?rsample::rolling_origin` to see
optional arguments that can be passed to `.cv_args`. A reasonable
hyperparameter grid is determined using the `dials` package.

``` r
fit <- data %>% 
  group_by(Industry) %>% 
  regress(Return ~ ., lasso_reg = m("lasso"), .mask = "Date", 
          .cv = "vfold", .cv_args = list(v = 5))

fit %>% 
  ggplot(aes(variable, Industry)) +
  geom_tile(aes(fill = beta)) +
  scale_fill_gradient2(low = "firebrick", high = "forestgreen")
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" style="display: block; margin: auto;" />

The results do not appear to be different from a linear regression. To
compare methods, simply pass multiple models:

``` r
fit <- data %>% 
  group_by(Industry) %>% 
  regress(Return ~ ., lasso_reg = m("lasso"), lin_reg = m("lm"), .mask = "Date", 
          .cv = "vfold", .cv_args = list(v = 5))
```

Of course, a v-fold cross validation is not valid for ordered data.
Instead simply set a rolling cross validation. In addition, we can pass
a custom grid for `lambda` by adding the argument to `m`. Note also that
it is not necessary to specify a model name:

``` r
fit <- data %>% 
  group_by(Industry) %>% 
  regress(Return ~ ., m("lasso", lambda = seq(0, 0.4, by = 0.05)), .mask = "Date", 
          .cv = "rolling_origin", .cv_args = list(initial = 60, assess = 24, skip = 24, cumulative = FALSE))

fit %>% 
  ggplot(aes(variable, Industry)) +
  geom_tile(aes(fill = beta)) +
  scale_fill_gradient2(low = "firebrick", high = "forestgreen")
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" style="display: block; margin: auto;" />

### Predicting with an ElasticNet classifier

Let’s predict out-of-sample return probabilities:

``` r
data_train <- data %>% 
  mutate(Return = ifelse(Return > 0, 1, 0)) %>% 
  filter(Date <= 202112)

data_test <- data %>% 
  mutate(Return = ifelse(Return > 0, 1, 0)) %>% 
  filter(Date > 202112)
```

Classification is possible with `tidyfit` using the `classify` function
instead of `regress`. This passes a `family = "binomial"` argument to
the underlying model functions. Note that additional arguments can be
specified in the model function that are passed on to the underlying
estimator (in this case `glmnet::glmnet`):

``` r
fit <- data_train %>% 
  mutate(Return = ifelse(Return > 0, 1, 0)) %>% 
  group_by(Industry) %>% 
  classify(Return ~ ., enet_clf = m("enet", maxit = 1e+06), .mask = "Date", 
          .cv = "rolling_origin", .cv_args = list(initial = 60, assess = 24, skip = 24, cumulative = FALSE))
```

Predictions can be made for all models using `cross_prod`. As the name
indicates, this generates predictions by multiplying data and
coefficients (and passing through the respective link function). No
model-specific predict methods are used. Predictions automatically apply
along the same groups as in the fitted object, and use the response
family specified during fitting:

``` r
pred <- fit %>% 
  cross_prod(data_test) %>% 
  mutate(Predicted = ifelse(pred > 0.5, 1, 0)) %>% 
  rename(Truth = Return)

# Print a confusion matrix
table(pred$Truth, pred$Predicted)
#>    
#>      0  1
#>   0 15  3
#>   1  3  9
```

### Parallel computation

`tidyfit` parallelizes cross-validation computations using the `future`
package in conjunction with `furrr`. Parallel computation can therefore
be activated by setting an appropriate plan:

``` r
library(furrr)
#> Loading required package: future
plan(multisession(workers = 4))
fit <- data %>% 
  group_by(Industry) %>% 
  regress(Return ~ ., lasso_reg = m("lasso"), .mask = "Date", 
          .cv = "vfold", .cv_args = list(v = 5))
```

### Additional functionality

`tidyfit` makes a few things easier:

-   Methods returned statistically comparable outputs. For instance, all
    covariates are standardized and the coefficients are
    back-transformed to the original scale. This is not done by all
    underlying packages (e.g. `pls`, which is used for the PCR and PLSR
    methods).
-   Hyperparameter grids are set to reasonable starting values. Custom
    grids can be passed to the model wrappers
    (e.g. `mod_lasso(lambda = seq(0, 1, by = 0.1))`).
-   Hyperparameter can be tuned across all groups or separately within
    each group by setting the `.tune_each_group` flag.
-   Results for the individual slices can be returned using the
    `.return_slices` flag. This is particularly useful for rolling
    window estimation, which can be done by returning the results of a
    rolling cross validation.

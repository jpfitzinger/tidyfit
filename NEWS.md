## tidyfit 0.7.3

This version includes two minor changes:

 - Minor bugfixes
 - Improves handling of groups for 'group_lasso' (for instance, missing groups, empty groups and ungrouped variables)
 
## tidyfit 0.7.2

This version adds new methods and features:

 - New methods:
  - 'group_lasso' for grouped Lasso estimation with gglasso
 - Fix ordering of 'tau' arguments in 'quantile_rf'
 - Allow columns containing NA values (these will be dropped before fitting)
 - Minor bugfixes

## tidyfit 0.7.1

Minor bugfix for non-syntactic name handling in 'rf' and 'quantile_rf' methods.

## tidyfit 0.7.0

This version adds several new features and methods:

- More generic handling of non-syntactic names
- New methods:
  - 'anova' for analysis of variance on glm objects
  - 'nnet' for single-layer neural networks
- An explain() generic which provides a convenience wrapper for methods from several variable importance packages
- Several bugfixes and improved error handling

## tidyfit 0.6.5

This version adds a new regression method:

- Quantile Random Forest regression ('quantile_rf')

In addition, there a few additional features & fixes:

- Add a new arugment '.return_grid' (default \code{FALSE}) to \code{regress} and \code{classify} methods to permit returning entire hyperparameter grid instead of only optimal setting
- Handling of syntactically invalid names is now down generically and not by the individual methods
- Add observation weights in 'genetic'
- Bugfixes in 'glmm' classification

## tidyfit 0.6.4

This version adds two new regression and classification methods:

- Spike & Slab regression and classification ('spikeslab')
- Genetic algorithm for variable selection in regression ('genetic')

In addition this version fixes a bug with 'adalasso' in conjunction with the 'dfmax' and 'pmax' arguments.
Finally, the internal '.model' generic is renamed to '.fit'.

## tidyfit 0.6.3

- Update generic methods for changes in 'broom' package

## tidyfit 0.6.2

This version adds new regression methods: Bayesian ridge and Bayesian lasso (using 'monomvn'-package). In addition, a number of improvements are made to the internal functions:

- Bugfix: add 'index' and 'group' columns to the 'mask' vector for 'sliding_index' CV and 'group_*' CV methods. This ensures that the columns are automatically removed from the regression.
- Add a resid() method for BMA regression.
- Minor adjustments in response to upstream package deprecation warnings.
- Unit testing with testthat.
- Improved error handling and CV efficiency.

## tidyfit 0.6.1

- Change method (.model.hfr) for compatibility with upstream package updates
- Bugfix: unnest.tidyfit.models missing struc
- Minor adjustments in response to upstream package deprecation warnings

## tidyfit 0.6.0

This version adds several new methods and enhances functionality & documentation:

- Add new regression methods: BMA, SVM, GETS, Random Forest
- Add new feature selection methods: MRMR, ReliefF, Correlation, Chi-Squared Test
- Add a vignette for feature selection
- Add jack-knife results to coef() of PCR and PLSR and improve grid handling
- Add a 'lambda' parameter for 1st-stage weighting regression in AdaLasso
- Minor bug-fixes and performance enhancements
- Add 'unnest' method for tidyfit.models frame

## tidyfit 0.5.1

- Add 'fitted' and 'resid' methods for tidyfit.models frame

## tidyfit 0.5.0

- This version introduces R6 classes for background handling of models. This generally makes the workflow more efficient and provides an easy method to store fitting information that is required at a later stage (e.g. to obtain coefficients or predictions).
- A progress bar is introduced using 'progressr'

## tidyfit 0.4.0

- This versions add the concept of a 'tidyfit.models' frame. Instead of producing coefficients directly, the models objects are stored and are accessed to obtain coefficients or predictions. This approach allows vastly more flexibility in the types of methods that can be included.
- Several additional cross validation methods such as bootstrap and sliding window methods
- Several new vignettes to illustrate how to use CV methods
- The version also adds a new method: the TVP method, which uses shrinkTVP to estimate a Bayesian time-varying parameter model.

## tidyfit 0.3.0

- This version adds the concept of an index which facilitates the addition of methods with heterogeneous coefficients (e.g. mixed-effects model)
- The backend handling of predictions has been adapted to allow coefficients to vary over one or more index columns

## tidyfit 0.2.1

- Refactoring of internal functions, no change to the functionality of the package

## tidyfit 0.2.0

- The release adds multinomial classification to the package:
  - Automatically detect classes, check if method can handle multinomial classification and fit appropriately
  - Coefficients returned for each class
  - Prediction and cross validation handle multi-class results
- More efficient and flexible handling of prediction and performance evaluation for cross validation

## tidyfit 0.1.0

- Note that this starts from version `tidyfit 0.1.0`.







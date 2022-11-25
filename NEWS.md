## tidyfit 0.1.0

- Note that this starts from version `tidyfit 0.1.0`.

## tidyfit 0.2.0

- The release adds multinomial classification to the package:
  - Automatic detect classes, check if method can handle multinomial classification and fit appropriately
  - Coefficients returned for each class
  - Prediction and cross validation handle multi-class results
- More efficient and flexible handling of prediction and performance evaluation for cross validation

## tidyfit 0.2.1

- Refactoring of internal functions, no change to the functionality of the package

## tidyfit 0.3.0

- This version adds the concept of an index which facilitates the addition of methods with heterogeneous coefficients (e.g. mixed-effects model)
- The backend handling of predictions has been adapted to allow coefficients to vary over one or more index columns

## tidyfit 0.4.0

- This versions add the concept of a 'tidyfit.models' frame. Instead of producing coefficients directly, the models objects are stored and are accessed to obtain coefficients or predictions. This approach allows vastly more flexibility in the types of methods that can be included.
- Several additional cross validation methods such as bootstrap and sliding window methods
- Several new vignettes to illustrate how to use CV methods
- The version also adds a new method: the TVP method, which uses shrinkTVP to estimate a Bayesian time-varying parameter model.

## tidyfit 0.5.0

- This version introduces R6 classes for background handling of models. This generally makes the workflow more efficient and provides an easy method to store fitting information that is required at a later stage (e.g. to obtain coefficients or predictions).
- A progress bar is introduced using 'progressr'

## tidyfit 0.5.1

- Add 'fitted' and 'resid' methods for tidyfit.models frame

## tidyfit 0.6.0

This version adds several new methods and enhances functionality & documentation:

- Add new regression methods: BMA, SVM, GETS, Random Forest
- Add new feature selection methods: MRMR, ReliefF, Correlation, Chi-Squared Test
- Add a vignette for feature selection
- Add jack-knife results to coef() of PCR and PLSR and improve grid handling
- Add a 'lambda' parameter for 1st-stage weighting regression in AdaLasso
- Minor bug-fixes and performance enhancements
- Add 'unnest' method for tidyfit.models frame

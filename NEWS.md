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

- This version adds the concept of an index which facilitates the addition of methods with heterogenous coefficients (e.g. mixed-effects model)
- The backend handling of predictions has been adapted to allow coefficients to vary over one or more index columns

## Summary of changes

This version adds a new regression method:

- Quantile Random Forest regression ('quantile_rf')

In addition, there a few additional features & fixes:

- Add a new arugment '.return_grid' (default \code{FALSE}) to \code{regress} and \code{classify} methods to permit returning entire hyperparameter grid instead of only optimal setting
- Handling of syntactically invalid names is now down generically and not by the individual methods
- Add observation weights in 'genetic'
- Bugfixes in 'glmm' classification

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs. 

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Summary of changes

This version adds new regression methods: Bayesian ridge and Bayesian lasso (using 'monomvn'-package). In addition, a number of improvements are made to the internal functions:

- Bugfix: add 'index' and 'group' columns to the 'mask' vector for 'sliding_index' CV and 'group_*' CV methods. This ensures that the columns are automatically removed from the regression.
- Add a resid() method for BMA regression.
- Minor adjustments in response to upstream package deprecation warnings.
- Unit testing with testthat.
- Improved error handling and CV efficiency.

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs. 

## Downstream dependencies

There are currently no downstream dependencies for this package.

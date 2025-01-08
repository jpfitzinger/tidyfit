## RESUBMISSION

  - **Resolved:** CRAN test returned a NOTE for long-running examples: I reduced the running time of the relevant examples
  - **Resolved:** CRAN test commented on ASAN errors from previous version: I have changed usage of the problematic dependency, resolving this issue (verified by running ASAN locally). Please re-run the test.

## Summary of changes

This version includes two minor changes:

 - Resolves asan issues with dependencies
 - Improves handling of groups for 'group_lasso' (for instance, missing groups, empty groups and ungrouped variables)

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs. 

## Downstream dependencies

There are currently no downstream dependencies for this package.

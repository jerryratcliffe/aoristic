CRAN-comments
================

## Update information

Martin Maechler flagged up three weeks ago that with the development
version of R, called “R-devel”, since svn revision r82904 (2022-09-24
19:32:52) changes to “POSIXt” were throwing an error in my aoristic
package. This update seeks to fix the errors and warnings that affected
‘aoristic’

My email address for maintainer purposes is: <jhr@temple.edu>

## Changes

Modified code in ‘aoristic.datacheck’. Modified code in ‘aoristic.df’.

## Submission version

Version 1.1.1.

## R CMD check results

There were no ERRORs or WARNINGs. The only notes relate to absense of
global definition functions for device management. Complying with the
suggestions in the notes, does not solve the issue, but neither do the
issues affect any program management.

## Test environments

##### Tested with TRAVIS

os: - linux - osx

r: - oldrel - release - devel

## Dependencies

I ran devtools::revdep() on the package and it returned character(0).

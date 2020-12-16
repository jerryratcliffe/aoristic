CRAN-comments
================

## Update information

This is minor update that improves production of one output table. Also
added one function that simplifies a plot that the user might find
helpful.

My email address for maintainer purposes is: <jhr@temple.edu>

## Changes

Modified code in ‘aoristic.summary’.  
Created new function ‘aoristic.plot’

## Submission version

Version 1.1.0.

## R CMD check results

There were no ERRORs or WARNINGs. The only notes relate to absense of
global defintion functions for device management. Complying with the
suggestions in the notes, does not solve the issue, but neither do the
issues affect any program management.

## Test environments

##### Tested with TRAVIS

os: - linux - osx

r: - oldrel - release - devel

## Dependencies

I ran devtools::revdep() on the package and it returned character(0).

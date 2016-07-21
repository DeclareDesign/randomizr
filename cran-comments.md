## Update randomizr to version 0.4.0
* harmonized function arguments
* added experimental load balancing
* documentation, examples, and vignette improvements

## Response to Prof Ripley's email

Prof Ripley writes:
"Things such as checking that probabilities numerically sum to one are silly: numerical tests should always have a tolerance. That the tests on your platform achieve a particular tolerance says little about other platforms. yet you do if (sum(prob_each) != 1) {}... Please correct ASAP." 

This line has been amended to if (!isTRUE(all.equal(sum(prob_each), 1))) {}.  Many thanks for the correction.

## Test environments
* local OS X install, 3.3.1 (2016-06-21) -- "Bug in Your Hair"
* R-devel with win-builder.r-project.org.

## R CMD check results
There were no ERRORs, WARNINGs. There was one NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Alexander Coppock <acoppock@gmail.com>'

New maintainer:
  Alexander Coppock <acoppock@gmail.com>
Old maintainer(s):
  Alexander Coppock <ac3242@columbia.edu>

I sent confirmation emails regarding this change on June 26th, 2016.

## Downstream dependencies
There are currently no downstream dependencies for this package.


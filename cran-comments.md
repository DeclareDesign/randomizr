## Test environments
* local OS X install, R 3.1.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, or WARNINGs.

There were 2 NOTES:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘"Alexander Coppock" <ac3242@columbia.edu>’
  New submission

  This is indeed my first submission.

* checking package dependencies ... NOTE
  No repository set, so cyclic dependency check skipped

  This NOTE does not appear on win-builder devel or release, but only on my local OS X install.

  I took steps to address this NOTE, but was unable to fix. In particular, I added the line
  options(repos = c(CRAN="http://cran.r-project.org"))
  to ~/.Rprofile, as recommended in the manual.

  I verified that the option was indeed set with options()$repos in an R session, so it is not a failure to set a repository on my system.

  Further, this package does not depend on any packages.

## Downstream dependencies
There are currently no downstream dependencies for this package.

## Update randomizr to version 0.22.0
* Remove suggests dependency of blockTools per Prof. Ripley email (the package was removed from CRAN)
* Add Graeme Blair as a contributor
* Small bug fixes
* Added permutation support for random sampling functions

## Test environments
R version 4.1.2
* R-devel with win-builder.r-project.org
* R-release with win-builder.r-project.org.
* R-oldrelease with win-builder.r-project.org.
* Linux, Mac OS via Github Actions

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies

There is a change to worse in the reverse dependency DeclareDesign, but we have a patch prepared once this is accepted that will address that test failure. Thank you for your understanding. There are no other changes to worse. 


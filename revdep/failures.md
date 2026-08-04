# estimatr (1.0.6)

* GitHub: <https://github.com/DeclareDesign/estimatr>
* Email: <mailto:graeme.blair@gmail.com>
* GitHub mirror: <https://github.com/cran/estimatr>

Run `revdepcheck::revdep_details(, "estimatr")` for more info

## In both

*   checking whether package ‘estimatr’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/private/tmp/claude-501/-Users-alexandercoppock-Library-CloudStorage-Dropbox-claude-control/d9a22e33-47dd-4682-b584-a5df0e8aabe0/scratchpad/rz20/revdep/checks.noindex/estimatr/new/estimatr.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘estimatr’ ...
** this is package ‘estimatr’ version ‘1.0.6’
** package ‘estimatr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 21.0.0 (clang-2100.1.1.101)’
using SDK: ‘MacOSX26.5.sdk’
clang++ -arch arm64 -std=gnu++20 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/private/tmp/claude-501/-Users-alexandercoppock-Library-CloudStorage-Dropbox-claude-control/d9a22e33-47dd-4682-b584-a5df0e8aabe0/scratchpad/rz20/revdep/library.noindex/randomizr/new/Rcpp/include' -I'/private/tmp/claude-501/-Users-alexandercoppock-Library-CloudStorage-Dropbox-claude-control/d9a22e33-47dd-4682-b584-a5df0e8aabe0/scratchpad/rz20/revdep/library.noindex/estimatr/RcppEigen/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c RcppExports.cpp -o RcppExports.o
In file included from RcppExports.cpp:4:
In file included from /private/tmp/claude-501/-Users-alexandercoppock-Library-CloudStorage-Dropbox-claude-control/d9a22e33-47dd-4682-b584-a5df0e8aabe0/scratchpad/rz20/revdep/library.noindex/estimatr/RcppEigen/include/RcppEigen.h:25:
...
      |       ^
5 warnings generated.
clang++ -arch arm64 -std=gnu++20 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -o estimatr.so RcppExports.o horvitz_thompson_variance.o lm_robust_helper.o naomit.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0 -L/opt/gfortran/lib -lemutls_w -lheapt_w -lgfortran -lquadmath -F/Library/Frameworks/R.framework/.. -framework R
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0' not found
ld: warning: search path '/opt/gfortran/lib' not found
ld: library 'emutls_w' not found
clang++: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [estimatr.so] Error 1
ERROR: compilation failed for package ‘estimatr’
* removing ‘/private/tmp/claude-501/-Users-alexandercoppock-Library-CloudStorage-Dropbox-claude-control/d9a22e33-47dd-4682-b584-a5df0e8aabe0/scratchpad/rz20/revdep/checks.noindex/estimatr/new/estimatr.Rcheck/estimatr’


```
### CRAN

```
* installing *source* package ‘estimatr’ ...
** this is package ‘estimatr’ version ‘1.0.6’
** package ‘estimatr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 21.0.0 (clang-2100.1.1.101)’
using SDK: ‘MacOSX26.5.sdk’
clang++ -arch arm64 -std=gnu++20 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/private/tmp/claude-501/-Users-alexandercoppock-Library-CloudStorage-Dropbox-claude-control/d9a22e33-47dd-4682-b584-a5df0e8aabe0/scratchpad/rz20/revdep/library.noindex/estimatr/Rcpp/include' -I'/private/tmp/claude-501/-Users-alexandercoppock-Library-CloudStorage-Dropbox-claude-control/d9a22e33-47dd-4682-b584-a5df0e8aabe0/scratchpad/rz20/revdep/library.noindex/estimatr/RcppEigen/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c RcppExports.cpp -o RcppExports.o
In file included from RcppExports.cpp:4:
In file included from /private/tmp/claude-501/-Users-alexandercoppock-Library-CloudStorage-Dropbox-claude-control/d9a22e33-47dd-4682-b584-a5df0e8aabe0/scratchpad/rz20/revdep/library.noindex/estimatr/RcppEigen/include/RcppEigen.h:25:
...
      |       ^
5 warnings generated.
clang++ -arch arm64 -std=gnu++20 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -o estimatr.so RcppExports.o horvitz_thompson_variance.o lm_robust_helper.o naomit.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0 -L/opt/gfortran/lib -lemutls_w -lheapt_w -lgfortran -lquadmath -F/Library/Frameworks/R.framework/.. -framework R
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/14.2.0' not found
ld: warning: search path '/opt/gfortran/lib' not found
ld: library 'emutls_w' not found
clang++: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [estimatr.so] Error 1
ERROR: compilation failed for package ‘estimatr’
* removing ‘/private/tmp/claude-501/-Users-alexandercoppock-Library-CloudStorage-Dropbox-claude-control/d9a22e33-47dd-4682-b584-a5df0e8aabe0/scratchpad/rz20/revdep/checks.noindex/estimatr/old/estimatr.Rcheck/estimatr’


```

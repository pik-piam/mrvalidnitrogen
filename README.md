# madrat data preparation for validation purposes of nitrogen budgets

R package **mrvalidnitrogen**, version **1.6.6**

[![CRAN status](https://www.r-pkg.org/badges/version/mrvalidnitrogen)](https://cran.r-project.org/package=mrvalidnitrogen)  [![R build status](https://github.com/pik-piam/mrvalidnitrogen/workflows/check/badge.svg)](https://github.com/pik-piam/mrvalidnitrogen/actions) [![codecov](https://codecov.io/gh/pik-piam/mrvalidnitrogen/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/mrvalidnitrogen) [![r-universe](https://pik-piam.r-universe.dev/badges/mrvalidnitrogen)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

Package contains routines to prepare data for validation exercises.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("mrvalidnitrogen")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Benjamin Leon Bodirsky <bodirsky@pik-potsdam.de>.

## Citation

To cite package **mrvalidnitrogen** in publications use:

Bodirsky B (2024). _mrvalidnitrogen: madrat data preparation for validation purposes of nitrogen budgets_. R package version 1.6.6, <https://github.com/pik-piam/mrvalidnitrogen>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {mrvalidnitrogen: madrat data preparation for validation purposes of nitrogen budgets},
  author = {Benjamin Leon Bodirsky},
  year = {2024},
  note = {R package version 1.6.6},
  url = {https://github.com/pik-piam/mrvalidnitrogen},
}
```

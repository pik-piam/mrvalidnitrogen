# madrat data preparation for validation purposes of nitrogen budgets

R package **mrvalidnitrogen**, version **1.1.2**

[![CRAN status](https://www.r-pkg.org/badges/version/mrvalidnitrogen)](https://cran.r-project.org/package=mrvalidnitrogen)    

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

Bodirsky B (2021). _mrvalidnitrogen: madrat data preparation for validation purposes of
nitrogen budgets_. R package version 1.1.2, <URL:
https://github.com/pik-piam/mrvalidnitrogen>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {mrvalidnitrogen: madrat data preparation for validation purposes of nitrogen budgets},
  author = {Benjamin Leon Bodirsky},
  year = {2021},
  note = {R package version 1.1.2},
  url = {https://github.com/pik-piam/mrvalidnitrogen},
}
```


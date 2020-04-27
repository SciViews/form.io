
# form.io

<!-- badges: start -->
[![Linux build status](https://travis-ci.com/SciViews/form.io.svg?branch=master)](https://travis-ci.com/SciViews/form.io)
[![Win build status](https://ci.appveyor.com/api/projects/status/github/SciViews/form.io?branch=master&svg=true)](https://ci.appveyor.com/project/phgrosjean/form.io)
[![Coverage status](https://img.shields.io/codecov/c/github/SciViews/form.io/master.svg)
](https://codecov.io/github/SciViews/form.io?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/form.io)](https://CRAN.R-project.org/package=form.io)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

'form.io' formats textual data for R Markdown / R Notebook documents and provides figures, tables and equation numbering and crossreferencing for all types of R Markdown documents.

## Installation

You can install the released version of 'form.io' from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("form.io")
```

You can also install the latest developement version. Make sure you have the 'devtools' R package installed:

```r
install.packages("devtools")
```

Use `install_github()` to install the 'form.io' package from Github (source from **master** branch will be recompiled on your machine):

```r
devtools::install_github("SciViews/form.io")
```

R should install all required dependencies automatically, and then it should compile and install 'form.io'.

Latest devel version of 'form.io' in the "master" branch (source + Windows binaires for the latest stable version of R at the time of compilation) is also available from [appveyor](https://ci.appveyor.com/project/phgrosjean/form.io/build/artifacts).

## Further explore 'form.io'

You can get further help about this package this way: Make the 'form.io' package available in your R session:

```r
library("form.io")
```

Get help about this package:

```r
library(help = "form.io")
pckage?form.io
vignette("form.io") # None is installed with install_github()
```

For further instructions, please, refer to the related web site at https://www.sciviews.org/form.io/.

## Code of Conduct

Please note that the 'svSweave'form.io' package is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.

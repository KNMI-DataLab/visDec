
<!-- README.md is generated from README.Rmd. Please edit that file -->
Visibility / Fog Detection
==========================

[![Travis-CI Build Status](https://travis-ci.org/KNMI-DataLab/visDec.svg?branch=master)](https://travis-ci.org/KNMI-DataLab/visDec) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/KNMI-DataLab/visDec?branch=master&svg=true)](https://ci.appveyor.com/project/KNMI-DataLab/visDec) [![Coverage Status](https://img.shields.io/codecov/c/github/KNMI-DataLab/visDec/master.svg)](https://codecov.io/github/KNMI-DataLab/visDec?branch=master) [![Gitter](https://badges.gitter.im/KNMI-DataLab/visDec.svg)](https://gitter.im/KNMI-DataLab/visDec?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

Extract features from images for the purpose of fog detection / visibility estimation. The package can be installed via

``` r
devtools::install_github("KNMI-DataLab/visdec")
```

The image processing is done via the package [imager](https://cran.r-project.org/web/packages/imager/index.html), which is a port to [cimg](http://cimg.eu/).

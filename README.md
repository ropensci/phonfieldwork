# phonfieldwork

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![CRAN version](http://www.r-pkg.org/badges/version/phonfieldwork)](https://cran.r-project.org/package=phonfieldwork)
[![](http://cranlogs.r-pkg.org/badges/grand-total/phonfieldwork)](https://CRAN.R-project.org/package=phonfieldwork)
[![](https://travis-ci.org/agricolamz/phonfieldwork.svg?branch=master)](https://travis-ci.org/github/agricolamz/phonfieldwork)
[![Coverage Status](https://img.shields.io/codecov/c/github/agricolamz/phonfieldwork/master.svg)](https://codecov.io/github/agricolamz/phonfieldwork?branch=master)
[![DOI](https://zenodo.org/badge/194053227.svg)](https://zenodo.org/badge/latestdoi/194053227)

`phonfieldwork` is a package for phonetic fieldwork research and experiments. This package make it easier:

- creating a html/pptx presentation from stimuli-translation list, 
- renaming soundfiles according to the list of stimuli, 
- concatenating multiple soundfiles and create a Praat TextGrid whose interval labels are the original names of the sound
- extracting sounds according to annotation
- extracting annotation from multiple linguistic formats (Praat `.TextGrid`, ELAN `.eaf`, EXMARaLDA `.exb`, Audacity `.txt` and subtitles `.srt`)
- visualising an oscilogram, a spectrogram and an annotation
- creating an html viewer [like this](https://agricolamz.github.io/phonfieldwork/s1/stimuli_viewer.html), ethical problems of this kind of viewer in linguistic research are covered in the vignette `vignette("ethical_research_with_phonfieldwork")`.

For more ditails see [tutorial](https://agricolamz.github.io/phonfieldwork/).

The main goal of the `phonfieldwork` package is to make a full research workflow from data collection to data extraction and data representation easier for people that are not familiar with programming. Hovewer most of the `phonfieldwork` funnctionality can be found in other software and packages:

* stimuli presentation creation could be done with any programming language and probably without them
* automatic file renaming and automatic merge could be done with any programming language
* Praat `.TextGrid` manipulation could be done with Praat, R packages [`rPraat`](https://cran.r-project.org/package=rPraat) and [`textgRid`](https://cran.r-project.org/package=textgRid), the Python package ['pympi'](https://dopefishh.github.io/pympi/index.html))
* ELAN `.eaf` manipulationcould be done with ELAN, the R package [`FRelan`](https://github.com/langdoc/FRelan) and the Python package ['pympi'](https://dopefishh.github.io/pympi/index.html)
* cut sounds according to annotation could be done with Praat and the R package`tuneR`
* spectrogram visualisation could be done with multiple R packages [`signal`](https://cran.r-project.org/package=signal), [`tuneR`](https://cran.r-project.org/package=tuneR), [`seewave`](https://cran.r-project.org/package=seewave), [`phonTools`](https://cran.r-project.org/package=phonTools), [`monitor`](https://cran.r-project.org/package=monitor), [`warbleR`](https://cran.r-project.org/package=warbleR), [`soundgen`](https://cran.r-project.org/package=soundgen) and many others

## Installation

Install from CRAN:

```
install.packages("phonfieldwork")
```

Get the development version from GitHub:

```
install.packages("devtools")
devtools::install_github("agricolamz/phonfieldwork")
```
Load a library:
```
library(phonfieldwork)
```

## To do:

* export to ELAN and EXMARALDA files
* use ELAN and EXMARALDA files in the whole pipline discribed in docs

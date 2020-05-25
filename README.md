# phonfieldwork

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![CRAN version](http://www.r-pkg.org/badges/version/phonfieldwork)](https://cran.r-project.org/package=phonfieldwork)
[![](http://cranlogs.r-pkg.org/badges/grand-total/phonfieldwork)](https://CRAN.R-project.org/package=phonfieldwork)
[![DOI](https://zenodo.org/badge/194053227.svg)](https://zenodo.org/badge/latestdoi/194053227)

`phonfieldwork` is a package for phonetic fieldwork. This package make it easier sound file manipilations like

- creating a html/pptx presentation from stimuli-translation list, 
- renaming soundfiles according to the list of stimuli, 
- concatenating multiple soundfiles and create a Praat TextGrid whose interval labels are the original names of the sound
- extracting sounds from annotation
- creating an oscilogram and a spectrogram from a sound
- creating an html viewer [like this](https://agricolamz.github.io/phonfieldwork/s1/stimuli_viewer.html)

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

* import EXMARALDA files
* export to ELAN, EXMARALDA files
* add textgrid visualisation

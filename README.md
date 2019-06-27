# lingfieldwork

`lingfieldwork` is a package for phonetic fieldwork. This package make it easier sound file manipilation:

* `create_presentation` -- creates a html/pptx presentation from stimuli-translation list
* `rename_soundfiles` -- renames soundfiles according to the list of stimuli
* `concatenate_soundfiles` -- concatenates multiple soundfiles and create a Praat TextGrid whose interval labels are the original names of the sound

## Installation

Get the development version from GitHub:
```R
install.packages("devtools")
devtools::install_github("agricolamz/lingfieldwork")
```
Load a library:
```R
library(lingfieldwork)
```

For a detailed tutorial see [GitHub pages](https://agricolamz.github.io/lingfieldwork/).

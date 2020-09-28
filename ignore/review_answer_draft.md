# For  @jonkeane

## Think about making classes

I will keep it in mind, but for the time being I created a function `read_textgrid()` that is more general then previous, since it deals with encodings.

## String manipulation and {glue}

## srt_to_df()

> On lines 44, 55, and 56, I see use of integers (e.g. c(3:4)) for indexing. This is probalby ok since you define the result that is being indexed, but it might be nice for readability to use names here.

I'm not sure what kind of names could be used here, since the common 

## Tests

> If your `test-*.R` files are named the same (after the `test-` bit) as your `*.R` source files, you can run them individually with `cmd+shift+i  (most of yours are, but there are a few exceptions)

For some reason it doesn't work for me (I checked both with `_` and `-` signs in function names). So I found and fixed only one mismatch.

# For  @nikopartanen

## Creating stimuli and organizing the files
I added `autonumber` and `loging` arguments to `rename_soundfiles()` function. They both can be switched off. The `autonumber` argument add prefix with number (and appropriate zero padding). The `logging` arguments creates a `logging.csv` file with the change correspondencies.

I added SpeechRecorder (for some reason, I can't install it on my PC) to the vignette. It would be nice to add once an R function that will create a recording script for them.

I added `missing` argument -- this is really nice thought.

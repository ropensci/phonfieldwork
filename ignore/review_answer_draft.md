Dear @jonkeane and @nikopartanen, I've made a lot of changes. Thank you again for your comments.

I'm mostly finished. The only big change that left is to create an additional functions instead of `..._from_folder` arguments -- I will do it during this week.

# For  @jonkeane

## pandoc-citeproc

> I needed to install pandoc-citeproc in order to get started, this should probably be listed in the SystemRequirements field of the DESCRIPTION + it would be nice to include it in the readme.

It looks like that pandoc-citeproc is [a dependency of `rmarkdown`](https://github.com/rstudio/rmarkdown/blob/5b0df0ee2212aa3eaf34a3f8be05915cbd867c8c/vignettes/pandoc.Rmd) and not of `phonfieldwork`. So I'm not sure that it should be in the `phonfieldwork` DESCRIPTION, but I added it to Introduction section.

## Think about making classes

I will keep it in mind, but for the time being I created a function `read_textgrid()` that is more general then previous, since it deals with encodings.

## Function restructuring

## Documentation improvements

I added sections to `pkgdown` refference page

## String manipulation and {glue}


## srt_to_df()

> On lines 44, 55, and 56, I see use of integers (e.g. c(3:4)) for indexing. This is probalby ok since you define the result that is being indexed, but it might be nice for readability to use names here.

I'm not sure what kind of names could be used here, since the common 

## Tests

> If your `test-*.R` files are named the same (after the `test-` bit) as your `*.R` source files, you can run them individually with `cmd+shift+i  (most of yours are, but there are a few exceptions)

For some reason it doesn't work for me (I checked both with `_` and `-` signs in function names). So I found and fixed only one mismatch.

## Comments on specific functions/files

### `create_glossed_document()`
> You might consider using a flextext class here (if you're using S3, you can make it so that are also data.frames, if you're using S4 or R6 or reference classes (all of which are probably overkill here) you could make this class inherit from data.frame). This way you could test that the object was of that class and write one method that checks that it has the column you have specified there.

I'm not so big fun of classes, but I will keep it in mind.


### `create_viewer()`
> Would it be possible to create a report where some sounds have images, but others don't? (Or even one where it's just sounds and no pictures, or just pictures and no sounds?)

Not in the current version, but I will consider to make it possible [in the future](https://github.com/agricolamz/phonfieldwork/issues/30).

> Is it possible to have mp3 audio files in the viewer?

> I'm not sure if this is using Safari, or something about the images, but the images never seemed to show up (or would show up on the map only in the example) when clicking on the eye. I didn't dig too much to see what was going on here, but you might want to try it out in a few different browsers / settings.

It is a known [problem](https://github.com/agricolamz/phonfieldwork/issues/19). I will fix it soon.

### textgrid_to_df()

> I had at least one textgrid that had nuls in it such that I had to add , skipNul = TRUE to the readLines() call on line 33 to let this work (the warning I got was line 50 appears to contain an embedded nul, though that will result in all of the lines being "". I wonder if you should pass all of the options (maybe via ... from textgrid_to_df() through to readLines).

May I look at it? I've implemented some encoding cracker since this version, may be now it is work.

> You might also want to add a check that the lines in tg aren't all "" (like with the above error) and error proactively there. Otherwise, the error happens later on line 53 where split_text doesn't exist which is harder to debug that this was an encoding/input problem.

I'm not sure about it. Since I think that I solved the possible encoding problems, I think this is not so important. But I think you are right, I need to consider all weird possibilities.

# For  @nikopartanen

## Creating stimuli and organizing the files
I added `autonumber` and `loging` arguments to `rename_soundfiles()` function. They both can be switched off. The `autonumber` argument add prefix with number (and appropriate zero padding). The `logging` arguments creates a `logging.csv` file with the change correspondencies.

I added SpeechRecorder (for some reason, I can't install it on my PC) to the vignette. It would be nice to add once an R function that will create a recording script for them.

I added `missing` argument -- this is really nice thought.


## Processing data

I added a function `create_empty_textgrid()`.

## Reading data into R

I fixed `exb_to_df()`, so your example now parsed into table. 

I add an error for those flextexts that doesn't have a word-level annotations. It looks like kind of a bigger work, so I will fix it [in the future](https://github.com/agricolamz/phonfieldwork/issues/28).

I added a new vignette about manipulating with tables with `tidyvers`. Grammar and spelling will be corrected soon.

## Visualizations
I changed documentation little bit, in order to say explicitly about df as annotation source.

> As a technical note, using a tibble instead a data.frame seems to give an error here. I think it would be important that the package would not make a difference between data.frame and tbl_df classes.

Thank you for this spot! Fixed it.

## Further comments

> I think create_glossed_document is a very useful function, but I'm not entirely sure if this package is ultimately the best place for it. It's a bit aside from the current pipeline the package provides, although it is a really important tool. One way to connect it better would be to be able to give it instead of a FlexText file a data frame with specific columns. Then it would link more clearly to other capablities of the package: reading the corpus, manipulating and analyzing it in R, and then converting some specific parts to glossed documents that can be used elsewhere.

It is really nice thought, I added it as [an issue](https://github.com/agricolamz/phonfieldwork/issues/26).

About Leipzig.js -- it is nice, but it has its own issues that we [started discussing long ago](https://github.com/bdchauvette/leipzig.js/issues/1).

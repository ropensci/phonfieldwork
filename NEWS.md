# phonfieldwork 0.0.8

- add `intensity`, `picth` and `pitch_range` arguments to the `draw_sound()` function
- add the `picth_to_df()` function
- add the `intensity_to_df()` function
- add an argument `external` to the `create_presentation` function in order to mark external images or gifs

# phonfieldwork 0.0.7

- add a vigniettes about ethical research and introduction to work with phonfieldwork
- add an argument `textgrids_from_folder` to the `textgrid_to_df()` function
- add an argument `exbs_from_folder` to the `exb_to_df()` function
- add an argument `eafs_from_folder` to the `eaf_to_df()` function
- add Raven style annotations8
- replace freqmax with frequency_range argument
- replace example_textgrid with systemfile() call
- add window annotation to spectrograms
- add bridge to lingtypology package: `map` argument in the `create_viewer()` function
- make it possible to visualise all types of annotations with the `draw_sound()` function
- add the `source` column to all `..._to_df()` functions
- add the `audacity_to_df` function
- add the `srt_to_df` function
- chage textgrid related functions' output from `start`, `end`, `annotation` to `time_start`, `time_end`, `content`
- correct point tier visualization

# phonfieldwork 0.0.6

- add `encoding` arguments to functions for working with TextGrids
- add `textgrid` argument to the `draw_sound()` function
- change subgraphs alignment in the `draw_sound()` function including textgrid annotation
- add `from` and `to` arguments to the `draw_sound()` function
- add .mp3 format reading options to all functions that work with sounds
- add `text_size` argument to the `draw_sound()` function
- add `zoom`` argument to the `draw_sound()` function
- add the `get_sound_duration()` function
- fix ploting of multiple sounds with multiple .TextGrids
- add an argument `title_as_filename` to the `draw_sound()` function
- change .TextGrid associated arguments of the `create_viewer()` function to `table` argument; as a result users now need to provide a table for the annotation viewer and not a .TextGrid

# phonfieldwork 0.0.5

- add `textgrid_to_df()` function for reading Praat files
- add `create_glossed_document()` function for converting .flextext files into a glossed document
- add `flextext_to_df()` function for reading FLEx files
- add `eaf_to_df()` function for reading ELAN files
- add `exb_to_df()` function for reading EXMARaLDA files
- rename `textgrid` argument into `annotation` argument in `concatenate_soundfiles()` function adding new possible values

# phonfieldwork 0.0.4

- add `create_subannotation()` function

# phonfieldwork 0.0.3

- vertically and horisontally center text in presentations created by `create_presentation()`; thx @Pandaklez #1
- add the `font_size` argument to the `create_presentation()` function
- add `rename_videofiles()` function
- rebuild html viewer for sounds with JavaScript with the help of new functions `create_image_look_up()` and `create_sound_play()`.

# phonfieldwork 0.0.2

- make the `create_presentation()` function render silently
- add a new function `draw_sound()` for creating spectrogram and oscilogram
- add a new function `create_viewer()` for creating an html viewer with sound and spectrograms
- correct work of `autonumbering` function in `extract_intervals()` function
- add new functions  `get_textgrid_names()` and `set_textgrid_names()`
- finish tutorial <https://agricolamz.github.io/phonfieldwork/>

# phonfieldwork 0.0.1

- initial release

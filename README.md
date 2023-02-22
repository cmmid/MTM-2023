# Modern Techniques in Modelling (MTM)

This package contains materials to support the Modern Techniques in Modelling (MTM) course offered by LSHTM.

## Installation & Local Use

The MTM package is not available on CRAN at this time. You may install manually from this repository, but will likely find using the [remotes](https://cran.r-project.org/package=remotes) package the easier option. With `remotes` installed, from an `R` prompt: `remotes::install_github('cmmid/MTM')`.

To create a local copy of the course exercises, you can then use `MTM::scripts()` - the default options copy the scripts to `~/Downloads/MTM`, organized into sub folders by session topic.

## Content

Those materials are provided in three ways:
 - *as typical package functions:* The package functions provide support code for use by course participants, covering capabilities needed to do the various learning activities. These are documented and written with an emphasis on learning the material, so are not necessarily the most efficient approaches. We intend participants using the functions to read the documentation (e.g. via `?somefunction` at the prompt) and to inspect their internals (e.g. via `somefunction` at the the prompt - note lack of parens and that internal comments are included in output) particularly in the script-based activities outlined below. 
 - *as interactive tutorial applications:* We provide an interactive application for some of the learning activities in the `learnr` framework. Participants can access this by having the `learnr` package installed, and using the `MTM::launch()` function. Aside from the application framework code, we use the function code of this package to implement the various simulations, analyses, and plots. There are several compartmentalized coding exercises where participants can use these functions as well.
 - *as editable scripts:* We provide a series of scripts for participants to edit directly; they can export a copy of these scripts using the `MTM::scripts()` function. In general, these scripts reflect the same concepts as covered in the interactive tutorials, but instead participants are running the code at an R prompt rather than via the tutorial application.

## Development

See the `.github/CONTRIBUTING.md` file for coding guidelines.

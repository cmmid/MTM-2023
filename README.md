# Modern Techniques in Modelling (MTM)

This package contains materials to support the Modern Techniques in Modelling (MTM) course offered by LSHTM.

## Content

Those materials are provided in three ways:
 - *as typical package functions:* The package functions provide support code for use by course participants, covering capabilities needed to do the various learning activities. These are documented and written with an emphasis on learning the material, so are not necessarily the most efficient approaches. We intend participants using the functions to inspect their internals (particularly in the script-based activities outlined below). 
 - *as interactive tutorial applications:* We provide an interactive application for some of the learning activities in the `learnr` framework. Participants can access this by having the `learnr` package installed, and using the `MTM::launch()` function. Aside from the application framework code, we use the function code of this package to implement the various simulations, analyses, and plots. There are several compartmentalized coding exercises where participants can use these functions as well.
 - *as editable scripts:* We provide a series of scripts for participants to edit directly; they can export a copy of these scripts using the `MTM::scripts()` function. In general, these scripts reflect the same concepts as covered in the interactive tutorials, but instead participants are running the code at an R prompt rather than via the tutorial application.

## Development

The package files are organized according to _shorthand_ names. For example, a _Network Models_ session would have as shorthand a _network_ prefix. Any functions specific to that session should start with `network_...`. In the `R/` source folder, scripts defining those functions should start with `network_...`. In the scripts and tutorials, the folders for associated session content should be `network`.

_TODO: for instructor convenience, we should provide an add/rename session functions, which will skeleton out (add) elements for the new session or do all the renaming._

We use the `inst/` directory to distribute the interactive tutorial (`inst/tutorials`) and scripts (`inst/scripts`).

### Package Functions

Package functions should be prefixed with their session shorthand. Doing means that participants will be able to `?shorthand_` and have code completion show them all the relevant functions to the session.

Most of the sessions have some overlapping capabilities, since the course highlights different modelling approaches to addressing the same meta-problem of modelling. Where possible, package functions that are "doing the same thing" (in that meta-problem sense), they should have the same suffix and (again, where possible) return the same type of R object. Obviously, the latter will not be always strictly possible.

_TODO: overview of likely suffixes, rough descriptions_

For some course-related capabilities, particularly plotting, we have developed approaches for use *across* exercises. That choice is for pedagogical emphasis, to enable participants to focus on model comparison across underlying implementation or phenomena differences, rather than be distracted by different axis labels, color schemes, etc. These functions should be located in the `aaa_functions.R` script, and prefixed with `general_` except where they are custom functions for another package's capabilties. For example, a consistent `ggplot2` x-axis for simulation time for re-use would be `scale_x_simtime()` (mimicking the `ggplot2` idiom), not `general_scale_x_simtime()`.

_TODO: highlight important general functions._

_TODO: figure out shared plots once we have multiple sessions in here._

### Tutorials

_TODO: overview of how to write learnr tutorials in the MTM way_

### Scripts

_TODO: overview of how to write scripts in the MTM way_
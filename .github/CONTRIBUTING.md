# Contributing

## Organizing principles

 - *Pedagogy First*: Code should be written to be read by beginners. This approach
 overlaps with "well engineered", but sometimes the better engineering
 abstractions are in tension with comprehensibility for novices

## Organization

 - *Package Functions*: as normal, in the `R/` folder; see below for naming conventions. In general, session-specific content should be in filename(s) prefixed indicating the association. Non-specific, utility / support / cross-section etc objections/function should be grouped logically according role; we do *not* want one-function-per-file.
 - *Tutorials*: Tutorials should be edited in the `/inst/tutorials` folder. In general, they can be written as Rmarkdown documents, using `learnr` features to create questions / answers.
 - *Scripts*: Scripts should be edited in the `/inst/scripts/solutions` folder, with guidance in comment blocks starting with `#'` (n.b. the roxygen style hash). Questions / answers / etc should be marked with `@question`, `@answer` etc tags. This enables automated conversion of the script with answers into a version with the answers stripped out. FUTURE: and rough translation to/from tutorial format.
 Do *not* edit the scripts in outside the `.../solutions` hierarchy - those should only be generated automatically. However, after (re-)generation of the non-solution version, you should check that they turned out correctly.

## Naming Conventions

The package files are organized according to _shorthand_ names. For example, a "Network Models" session would have as shorthand a _network_ prefix. Any functions specific to that session should start with `network_...`. In the `R/` source folder, scripts defining those functions should start with `network_...`. In the scripts and tutorials, the folders for associated session content should be `network`.

_TODO: for instructor convenience, we should provide an add/rename session functions, which will skeleton out (add) elements for the new session or do all the renaming._

We use the `inst/` directory to distribute the interactive tutorial (`inst/tutorials`) and scripts (`inst/scripts`). So the scripts for the `network` session would be developed in `inst/scripts/solutions/network`. The scripts themselves should be named `00_...R`, `01_...R`, etc, reflecting the order they should be attempted. The rest of the script name should be a shorthand for what's being covered, e.g. `00_warmup.R` for a pre-content exercise. It is *not* necessary to repeat the repeat the overall session shorthand in the file name (as this is already present in the path).

## Package Functions

Session-related functions should generally cover a "step" - as in: if we were to give students a pseudo-code description of some model implementation or algorithm (i.e. a numbered list roughly spanning a slide / diagram), those steps could be captured as a function each (or if at a low enough level, roughly a line in a function, with the whole function definition not exceeding a screen). The session-related functions can then be used in scripts / tutorials to make engagement with the material at that pseudo code level, and can be inspected by the students if they want to see the detailed implementations.

In general, functions should respond well to "abuse" - as in, being given garbage arguments. Typically, this means failing-fast with a clear error message. The shared function files in `checks.R` have a variety of generic input validations. If a new one is needed, consider whether it is generic (might be used in other sessions) or specific to your session; if just your session (e.g. checking something is a `igraph` with the right properties for the networks session), it can go in a session specific file, but if its generic, feel free to add to `checks.R`. Check functions should be named `check_...` and generally deal with a single variable. Check functions should return their argument variable invisibly, so that they can potentially be applied in sequence (when multiple checks required). Check functions should be used as

```
variable |> check_...(...any other necessary args...) # other |> check_... as necessary 
```

Any exported session functions should be documented, ideally with arguments matching any session presentation / discussion content. We encourage students to inspect the function internals, so pay particular attention to internal function comments (which will be available for inspection of that function) and how the internal code is written. General guidance: choose clear over clever, unless clever is _necessary_ for function performance or the particular learning point.

Package functions should be prefixed with their session shorthand. Doing means that participants will be able to `?shorthand_` and have code completion show them all the relevant functions to the session.

Most of the sessions have some overlapping capabilities, since the course highlights different modelling approaches to addressing the same meta-problem of modelling. Where possible, package functions that are "doing the same thing" (in that meta-problem sense) should have the same suffix and (again, where possible) return the same type of R object. Obviously, the latter will not be always strictly possible.

_TODO: overview of likely suffixes, rough descriptions_

For some course-related capabilities, particularly plotting, we have developed approaches for use *across* exercises. That choice is for pedagogical emphasis, to enable participants to focus on model comparison across underlying implementation or phenomena differences, rather than be distracted by different axis labels, color schemes, etc. These functions should be located in the `aaa_functions.R` script, and prefixed with `general_` except where they are custom functions for another package's capabilities. For example, a consistent `ggplot2` x-axis for simulation time for re-use would be `scale_x_simtime()` (mimicking the `ggplot2` idiom), not `general_scale_x_simtime()`.

_TODO: highlight important general functions._

_TODO: figure out shared plots once we have multiple sessions in here._

## Tutorials

_TODO: overview of how to write learnr tutorials in the MTM way_

## Scripts

_TODO: overview of how to write scripts in the MTM way_

## Temporary parking here from external README (20230413)
## Content

Those materials are provided in three ways:
 - *as typical package functions:* The package functions provide support code for use by course participants, covering capabilities needed to do the various learning activities. These are documented and written with an emphasis on learning the material, so are not necessarily the most efficient approaches. We intend participants using the functions to read the documentation (e.g. via `?somefunction` at the prompt) and to inspect their internals (e.g. via `somefunction` at the the prompt - note lack of parens and that internal comments are included in output) particularly in the script-based activities outlined below. 
 - *as interactive tutorial applications:* We provide an interactive application for some of the learning activities in the `learnr` framework. Participants can access this by having the `learnr` package installed, and using the `MTM::launch()` function. Aside from the application framework code, we use the function code of this package to implement the various simulations, analyses, and plots. There are several compartmentalized coding exercises where participants can use these functions as well.
 - *as editable scripts:* We provide a series of scripts for participants to edit directly; they can export a copy of these scripts using the `MTM::scripts()` function. In general, these scripts reflect the same concepts as covered in the interactive tutorials, but instead participants are running the code at an R prompt rather than via the tutorial application.
 
## Development

See the `.github/CONTRIBUTING.md` file for coding guidelines.


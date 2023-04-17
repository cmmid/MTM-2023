# Modern Techniques in Modelling (MTM)

This package contains materials to support the Modern Techniques in Modelling (MTM) course offered by the LSHTM. The content presented in this repository is intended for the short course participants only and may not be redistributed beyond the capacity of this short course. 

## Quickstart

Copy-and-run this at an R prompt; you will be prompted about creating a directory - you must answer `y` to proceed.

```r
# try to load or if necessary install-load "remotes" package
if (!require(remotes)) { install.packages("remotes"); stopifnot(require("remotes")) } 
remotes::install_github("cmmid/MTM") # install MTM package
setwd(MTM::scripts()) # download MTM exercises
```

_n.b._: there's a convenient copy-all-this at the top right corner of the code block

## Detailed Installation 

The MTM package is not available on CRAN at this time. You may install manually from this repository, but will likely find using the [remotes](https://cran.r-project.org/package=remotes) package the easier option. With `remotes` installed, from an `R` prompt: `remotes::install_github('cmmid/MTM')`. Running this initial command to install `MTM` will also install all package dependencies that you would need for this short course.

If you have a company- or organization-managed computer, you might not be able to install packages or update `R`. We generally recommend that you use a personal computer for this short course. If you cannot, we recommend that you use the [web-based R-Studio](https://posit.cloud/). You will need to create an account, but the free level is sufficient for this course. Once you have an account, create a new project, and consistently use that project throughout the short course.

## Installation Troubleshooting

### I can't update R!

If you don't have admin permissions for your machine, you'll have to use the [web-based R-Studio](https://posit.cloud/) route.

On recent Mac versions, there seems to be an issue with where you install the `.pkg` *from*; see the [Ventura note](https://cran.r-project.org/bin/macosx/).

You may need to manually set *which* version of R that Rstudio uses. How to do so varies by [operating system](https://support.posit.co/hc/en-us/articles/200486138-Changing-R-versions-for-the-RStudio-Desktop-IDE).

### I can't install packages!

Again, if you don't have admin rights, you will have to use web-based version.

First, is your R version sufficiently recent? (see previous item)

Second, try installing `remotes` and/or `MTM` more than once - there may have been a hiccup in your internet connection.

When prompted to update other package dependencies, please indicate "1" (update All) - otherwise, `MTM` may not be able to install.

### I can't install the scripts!

You may need permissions to write the target folder; if you're working on the web-based Rstudio, your folder will be created *on that system*, not your local system.

When prompted to agree to creating the directory, you must respond with `y`+enter to allow it to create the folder.

On Windows, it seems somewhat common that people have their home directory (i.e. what the special directory `~` refers to) set to their documents, rather than the expected place. In that case, the default installation directs to `Documents/Downloads/MTM/scripts` - feel free to move it from there, leave it there, etc. Should just work!

## Local Use

To create a local copy of the course exercises, you can then use `MTM::scripts()` - the default options copy the scripts to `~/Downloads/MTM/scripts` (see below), organized into sub folders by session topic. This function also creates an Rstudio project, which you can open via Rstudio or your typical operating system interface (_e.g._ double-clicking it, `open ~/Downloads/MTM/scripts/scripts.Rproj` at the command prompt). If you use this project, you'll have the correct working directory for any file paths used in the exercises; otherwise, you may need to manually adjust your working directory or file paths when reading in files in various exercises.

When you run `MTM::scripts()`, this function downloads a local copy of MTM content using three arguments:  
  
*  `path`: accepts a string that point to the location where you would like to save MTM contents. By default, this path is set to `~/Downloads/MTM`.    
*  `overwrite`: default set to `FALSE` but can be set to `TRUE` as needed. When set to TRUE, the function will try and re-download the script files to overwrite what already exists.   
*  `what`: default set to `"scripts"`, which allows you to create a local copy of the course Practicals. This argument may be set to `"solutions"` as needed.

If contents have already been downloaded to the target path, and `overwrite` is set to `FALSE`, running this function would not make any changes - it will only remind you where the target contents have been saved. 

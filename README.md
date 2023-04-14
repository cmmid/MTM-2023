# Modern Techniques in Modelling (MTM)

This package contains materials to support the Modern Techniques in Modelling (MTM) course offered by the LSHTM. The content presented in this repository is intended for the short course participants only and may not be redistributed beyond the capacity of this short course. 

## Quickstart

```r
# try to load or if necessary install-load "remotes" package
if (!require(remotes)) { install.packages("remotes"); stopifnot(require("remotes")) } 
remotes::install_github("cmmid/MTM") # install MTM package
setwd(MTM::scripts()) # download MTM exercises
```

## Installation 

The MTM package is not available on CRAN at this time. You may install manually from this repository, but will likely find using the [remotes](https://cran.r-project.org/package=remotes) package the easier option. With `remotes` installed, from an `R` prompt: `remotes::install_github('cmmid/MTM')`. Running this initial command to install `MTM` will also install all package dependencies that you would need for this short course.

If you have a company- or organization-managed computer, you might not be able to install packages or update `R`. We generally recommend that you use a personal computer for this short course. If you cannot, we recommend that you use the [web-based R-Studio](https://posit.cloud/). You will need to create an account, but the free level is sufficient for this course. Once you have an account, create a new project, and consistently use that project throughout the short course.

## Local Use

To create a local copy of the course exercises, you can then use `MTM::scripts()` - the default options copy the scripts to `~/Downloads/MTM/scripts` (see below), organized into sub folders by session topic. This function also creates an Rstudio project, which you can open via Rstudio or your typical operating system interface (_e.g._ double-clicking it, `open ~/Downloads/MTM/scripts/scripts.Rproj` at the command prompt). If you use this project, you'll have the correct working directory for any file paths used in the exercises; otherwise, you may need to manually adjust your working directory or file paths when reading in files in various exercises.

When you run `MTM::scripts()`, this function downloads a local copy of MTM content using three arguments:  
  
*  `path`: accepts a string that point to the location where you would like to save MTM contents. By default, this path is set to `~/Downloads/MTM`.    
*  `overwrite`: default set to `FALSE` but can be set to `TRUE` as needed. When set to TRUE, the function will try and re-download the script files to overwrite what already exists.   
*  `what`: default set to `"scripts"`, which allows you to create a local copy of the course Practicals. This argument may be set to `"solutions"` as needed.

If contents have already been downloaded to the target path, and `overwrite` is set to `FALSE`, running this function would not make any changes - it will only remind you where the target contents have been saved. 

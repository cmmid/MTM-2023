# Modern Techniques in Modelling (MTM)

This package contains materials to support the Modern Techniques in Modelling (MTM) course offered by the LSHTM. The content presented in this repository is intended for the short course participants only and may not be redistributed beyond the capacity of this short course. 

## Installation 

The MTM package is not available on CRAN at this time. You may install manually from this repository, but will likely find using the [remotes](https://cran.r-project.org/package=remotes) package the easier option. With `remotes` installed, from an `R` prompt: `remotes::install_github('cmmid/MTM')`. Running this initial command to install `MTM` will also install all package dependencies that you would need for this short course.

At times, organisations may prevent users from freely installing `R` packages as they wish. In those cases, we recommend that you use personal computer for this short course, to which you have admin rights. When this is not possible, we recommend that you use [web-based R-Studio](https://posit.cloud/). You would need to create an account with them. We recommend that you create/ use only one project throughout this short course with this site.

## Local Use

The first function that you are likely to run from the package `MTM` is likely `scripts()`. This function helps you download local copy of MTM content using three arguments:  
  
*  `path`: accepts a string that point to the location where you would like to save MTM contents. By default, this path is set to `~/Downloads/MTM`.    
*  `overwrite`: default set to `FALSE` but can be set to `TRUE` as needed. When set to TRUE, the function will try and re-download the script files to overwrite what already exists.   
*  `what`: default set to `"scripts"`, which allows you to create a local copy of the course Practicals. This argument may be set to `"solutions"` as needed.

If contents have already been downloaded to the target path, and `overwrite` is set to `FALSE`, running this function would not make any changes - it will only remind you where the target contents have been saved. 



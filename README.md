## reactome4r: an R interface for Reactome Content Service

[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Build Status](https://travis-ci.com/reactome/reactome4r.svg?token=qrkG3qhYuEKpFXjXW1Fp&branch=develop)](https://travis-ci.com/reactome/reactome4r)

The `reactome4r` package provides a wrapper for the Reactome [Content Service](https://reactome.org/ContentService/).


# Installation



# Documentation

We have a __vignette__ for showing how to fetch instances in Reactome knowledgebase: (link)


If you are using R 4.x and RStudio on macOS, and get the error message like:
```
Error in makePSOCKcluster(names = spec, ...) : 
  Cluster setup failed. 2 of 2 workers failed to connect.
```
You can try to add the following to `~/.Rprofile` until it's officially fixed in RStudio:
```
## Copied from https://github.com/rstudio/rstudio/issues/6692#issuecomment-619645114
## Revert to 'sequential' setup of PSOCK cluster in RStudio Console on macOS and R 4.0.0

if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) && 
    Sys.info()["sysname"] == "Darwin" && getRversion() == "4.0.0") {
  parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}
```


# Feedback

Your feedback is welcome! Feel free to open an [issue](https://github.com/reactome/reactome4r/issues) on GitHub.

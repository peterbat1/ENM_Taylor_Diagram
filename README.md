# ENM Taylor Diagram

## Application of Taylor Diagrams to Ecological Niche Models/Species Distribution Models

Taylor (2001) published a paper which introduced an innovative plot or diagram which summarised three attributes of spatial model output in a 2-D plot, and was designed to contrast these attributes between a "reference" model and one or more alternate models. The reference model may be a baseline model output, or observations of a complex system. The alternate models may be alternate observations, observations at a different time, or the output of predictive models.  

Originally developed for applications in the atmospheric sciences (e.g. meteorology, climatology), Taylor Diagrams have been applied in other disciplines including hydrology.

Curiously, they have not been adopted by developers of ecological niche models (also referred to as species distribution models). This repository is my humble attempt to provide a useful script to allow ENM developers to use Taylor Diagrams.

This script is inspired by the source code for the function _taylor.diagram()_ from the _**R**_-package _plotrix_ written by Olivier Eterradossi and Jim Lemon.

The original source code is available [here](https://CRAN.R-project.org/package=plotrix).

Some aspects of the original data flow and some methods/code fragments have been retained in the version available in this repository. Although there are some borrowings from the original function, this version is a major re-write using the features and functions of the _**R**_-packages _ggplot2_ and _ggrepel_.

## Downloading and using the function:

Click on the green **Code** button Itop right of screen) to download a zip file containing the R-script.

Unzip the file in a folder of your chooosing.

To use the function in your R-scripts, using the source() funciton to load the script into your _**R**_ session:

```
source("path/to/taylor_diagram.R")
```

## Dependencies:

This function requires the _**R**_ libraries _ggplot2_ and _ggrepel_.


## Alternate inplementations of Taylor Diagrams in _**R**_

Other implementations of Taylor Diagrams in _**R**_ include:

_taylor.diagram()_ in the _**R**_-package _plotrix_ (of course);

_**R**_-package _openair_ (available from the CRAN repository) which has a function _TaylorDiagram()_; and,

An implementation using _ggplot2_ in the GitHub Repository [https://github.com/shirazipooya/Taylor.Diagram](https://github.com/shirazipooya/Taylor.Diagram) which appears to be incomplete and unchanged for a number of years.


## To do:

* Make the parameter _normalise_ work; at present it is ignored.
* Add an optional legend.
* Give greater control over plot symbols, colours and labels.


## References

Taylor, K. E. 2001. Summarizing multiple aspects of model performance in a single diagram. Journal of Geophysical Research: Atmospheres 106:7183–7192.


# ENM Taylor Diagram

## Application of Taylor Diagrams to Ecological Niche Models/Species Distribution Models

Taylor (2001) published a paper which introduced an innovative plot or diagram which summarised three attributes of spatial model output in a 2-D plot, and was designed to contrast these attributes between a "reference" model and one or more alternate models. The reference model may be a baseline model output, or observations of a complex system. The alternate models may be alternate observations, observations at a different time, or the output of predictive models.  

Originally developed for applications in the atmospheric sciences (e.g. meteorology, climatology), Taylor Diagrams have been applied other disciplines including hydrology.

Curiously, they have not been adopted by developers of ecological niche models (also referred to as species distribution models). This repository is my humble attempt to provide a useful script to allow ENM developers to use Taylor Diagrams.

This script is derived from the source code for the function _taylor.diagram()_ from the _**R**_-package _plotrix_ written by Olivier Eterradossi and Jim Lemon.

The original source code is available [here](https://CRAN.R-project.org/package=plotrix).

Changes made to the original _taylor.diagram()_ function include:

1. Significant streamlining and re-organisation of the original source code;
3. Ensuring that the visual style or aesthetic for the two variants are the same - the _plotrix_ function produces **very** different plots for each variant; and,
4. Changing some parameters to enhance the ability of users to adjust aspects of the plots.

## Dependencies:

This function requires the _**R**_ library _plotrix_ to access the function _boxed.labels()._


## Alternate inplementations of Taylor Diagrams in _**R**_

Other implementations of Taylor Diagrams in _**R**_ include:

_**R**_-package _openair_ (available from the CRAN repository) which has a function _TaylorDiagram()_; and,

An implementation using _ggplot2_ in the GitHub Repository [https://github.com/shirazipooya/Taylor.Diagram](https://github.com/shirazipooya/Taylor.Diagram)


## References

Taylor, K. E. 2001. Summarizing multiple aspects of model performance in a single diagram. Journal of Geophysical Research: Atmospheres 106:7183–7192.


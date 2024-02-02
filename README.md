# NotAllOilTypesAreAlike
Replication package for Güntner, Irlacher, Öhlinger (2024) Not all oil types are alike.

This repository is organized as an R package, facilitating convenient reuse. All data utilized in our paper is encapsulated within the package, ensuring straightforward accessibility post-installation. Additionally, all functions are thoroughly documented and accompanied by an R help page. A comprehensive package manual in PDF format is also provided. Furthermore, two vignettes are available—one offering a general overview on package usage and the other guiding the replication of all figures presented in the paper.

## For Economists

Recognizing that many economists may not be familiar with R's package framework, I've included the code in a format standard in our profession. In the "work" folder, a master file is available, executing all subsequent files and generating the results reported in the paper.

## How to install

Just run `devtools::install_github("OehlingerP/NotAllOilTypesAreAlike")`. If `devtools` is not yet install you need to run `install.packages("devtools")` before. Typically installed in less than one minute (on a "normal" desktop computer).

## Demo

Demos can be found in the `vignettes` folder. There is one demo for replicating all figures and there is one demo explaining the usage of the main function. Also included in this folder are the final HTML files. So no R-installation is needed to look at the demos. If the .Rmd files are executed, the run time on a "normal" desktop computer is less than one min.

## Package Manual

The package manual is stored in the vignettes folder and contains a description of all function used to replicate the tables and figures shown in the paper. Furthermore, all data used within this package is briefly explained as well. 



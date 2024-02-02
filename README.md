# NotAllOilTypesAreAlike
Replication package for Güntner, Irlacher, Öhlinger (2024) Not all oil types are alike.

This repository is organized as an R package, facilitating convenient reuse. All data utilized in our paper is encapsulated within the package, ensuring straightforward accessibility post-installation. Additionally, all functions are thoroughly documented and accompanied by an R help page. A comprehensive package manual in PDF format is also provided. Furthermore, two vignettes are available—one offering a general overview on package usage and the other guiding the replication of all figures presented in the paper.

## For Economists

Recognizing that many economists may not be familiar with R's package framework, I've included the code in a format standard in our profession. In the "work" folder, a master file is available, executing all subsequent files and generating the results reported in the paper.

## How to install

Just run `devtools::install_github("OehlingerP/NotAllOilTypesAreAlike")`. If `devtools` is not yet install you need to run `install.packages("devtools")` before. Typically installed in less than 1 minute.

#' As this is the standard way replication packages are structured in economics
#' I also provide a master file. Running this file just calls other files and
#' will create all outputs shown in the paper, though, in a different format.
#' Tables will be stored in Excel files.

#' PLEASE CHANGE THE DIRECTORY OF WHERE YOU WANT YOUR OUTPUT TO BE STORED!
FILE_OUTPUT <- "full/path/of/your/directory"

#' INSTALL PACKAGE IF NOT DONE ALREADY
# devtools::install_github("OehlingerP/NotAllOilTypesAreAlike")

# DO NOT CHANGE SCRIPT FROM HERE
library(NotAllOilTypesAreAlike)

# Get the directory of the current script or file
current_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the current directory
setwd(current_dir)

# figure 2
source("figure_2_aggregation_methods.R")

# figure 3
source("figure_3_distribution_of_ea_imports.R")

# figure 4
source("figure_4_us_and_ru_imports.R")

# table 1
source("table_1_hedonic_pricing_model.R")

# table 2
source("table_2_sigma_estimation.R")

# table 3
source("table_3_welfare_estimation.R")

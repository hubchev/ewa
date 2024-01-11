# Set the base directory, i.e., where the ewa folder is:
base_directory <- "~/Dropbox/hsf/23-ws/ewa/ewa_all/"
setwd(base_directory)
# Make sure you download the data folder. This folder is not on Github as it 
# contains confidential data, see: https://doi.org/10.4232/1.13978

if (!require(pacman)) install.packages("pacman")
pacman::p_load(conflicted, tidyverse, MASS)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

###################################################
# Read in gesis data
source("scripts/readin_GESIS.R")

###################################################
# Read in gesis data
source("scripts/desc_age_perception.R")

###################################################
## rmd_desc: "Descriptive Statistics of the NRW80+ Dataset"
setwd(paste0(base_directory, "rmd_desc"))
rmarkdown::render("desc_NRW80.Rmd", "all")

# clear folder
# List all files and folders in the working directory
files <- list.files()
valid_extensions <- c("Rmd", "R", "pdf", "bib")
for (file in files) {
  if (file != "data" && file.info(file)$isdir) {
    unlink(file, recursive = TRUE)
  } else {
    file_extension <- tools::file_ext(file)
    if (!(file_extension %in% valid_extensions) && file != "data") {
      file.remove(file)
    }
  }
}
cat("Files and folders deleted successfully except for data folder and specified extensions.\n")

# Convert to a R-script:
knitr::purl("desc_NRW80.Rmd")
cat("Conversion of the rmd to a script was successfull.\n")


###################################################
## desc_aov: "ANOVA Lecture Material"
setwd(paste0(base_directory, "rmd_aov"))
rmarkdown::render("desc_aov.Rmd", "all")

# clear folder
# List all files and folders in the working directory
files <- list.files()
valid_extensions <- c("Rmd", "R", "pdf", "bib")
for (file in files) {
  if (file != "data" && file.info(file)$isdir) {
    unlink(file, recursive = TRUE)
  } else {
    file_extension <- tools::file_ext(file)
    if (!(file_extension %in% valid_extensions) && file != "data") {
      file.remove(file)
    }
  }
}
cat("Files and folders deleted successfully except for data folder and specified extensions.\n")

# Convert to a R-script:
knitr::purl("desc_aov.Rmd")
cat("Conversion of the rmd to a script was successfull.\n")


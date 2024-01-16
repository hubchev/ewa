# Set the working directory, i.e., where the ewa folder is:
setwd("~/Dropbox/hsf/courses/ewa/ewa_all/")

# Make sure you download the data folder. This folder is not on Github as it 
# contains confidential data, see: https://doi.org/10.4232/1.13978

# Load packages / unload packages to avoid conflicts
if (!require(pacman)) install.packages("pacman")
pacman::p_unload(all)

###################################################
# Read in gesis data
source("scripts/readin_GESIS.R")

###################################################
# Read in gesis data
source("scripts/desc_age_perception.R")

###################################################
# E
rmarkdown::render("rmd_weight/explain_weighted_average.Rmd", "all")

###################################################
## rmd_desc: "Descriptive Statistics of the NRW80+ Dataset"
rmarkdown::render("rmd_desc/desc_NRW80.Rmd", "all")

dir.exists("scripts/from_Rmd")
dir.create("scripts/from_Rmd", recursive = TRUE, showWarnings = FALSE)

# Convert to a R-script:
knitr::purl("rmd_desc/desc_NRW80.Rmd", output = "scripts/from_Rmd/desc_NRW80.R")
cat("Conversion of the rmd to a script was successfull.\n")


###################################################
## desc_aov: "ANOVA Lecture Material"
rmarkdown::render("rmd_aov/desc_aov.Rmd", "all")

# Convert to a R-script:
knitr::purl("rmd_aov/desc_aov.Rmd", output = "scripts/from_Rmd/desc_aov.R")
cat("Conversion of the rmd to a script was successfull.\n")

# Clear the session
pacman::p_unload(all)
rm(list = ls())
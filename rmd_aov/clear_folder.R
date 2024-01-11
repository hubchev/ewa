# clear folder
setwd("~/Dropbox/hsf/github/courses/rmd/rmd_aov")

# List all files and folders in the working directory
files <- list.files()

# Specify the extensions to keep
valid_extensions <- c("Rproj", "Rmd", "R", "pdf", "bib")

# Loop through each file and folder
for (file in files) {
  # Check if it is a folder and not named "data"
  if (file != "data" && file.info(file)$isdir) {
    # Delete the folder and its contents
    unlink(file, recursive = TRUE)
  } else {
    # Check if the file has a valid extension
    file_extension <- tools::file_ext(file)
    if (!(file_extension %in% valid_extensions) && file != "data") {
      # Delete the file
      file.remove(file)
    }
  }
}

cat("Files and folders deleted successfully except for data folder and specified extensions.\n")



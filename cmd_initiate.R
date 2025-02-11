## rm(list = ls())
packages <- c("readxl",
              "haven",
              "tidyverse")

## Now load or install&load all
package.check <- lapply(
    packages,
    FUN = function(x) {
        if (!require(x, character.only = TRUE)) {
            install.packages(x, dependencies = TRUE)
            library(x, character.only = TRUE)
        }
    }
)

dirs <- c("input_data",
          "output_data",
          "documentation")
tmp <- lapply(dirs, function(x) if (!dir.exists(x)) dir.create(x))

## source("functions.R")

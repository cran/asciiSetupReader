## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
# Using the example .sps setup file included with the package
sps_name <- system.file("extdata", "example_setup.sps",
                          package = "asciiSetupReader")
example <- asciiSetupReader::parse_setup(sps_name)

## ------------------------------------------------------------------------
head(example$setup)

## ------------------------------------------------------------------------
example$value_labels[1:3]

## ------------------------------------------------------------------------
length(example$value_labels)

## ------------------------------------------------------------------------
head(example$missing)


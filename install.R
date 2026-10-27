# Install packages required by the monetary policy analysis
required_packages <- c(
  "tidyverse",
  "lubridate",
  "zoo",
  "readr",
  "lmtest",
  "sandwich",
  "car",
  "strucchange",
  "AER"
)

# Install only missing packages
missing_packages <- setdiff(
  required_packages,
  rownames(installed.packages())
)

if (length(missing_packages) > 0) {
  install.packages(
    missing_packages,
    repos = "https://cloud.r-project.org"
  )
}

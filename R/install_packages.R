required_packages <- c(
  "tidyverse",
  "DBI",
  "RSQLite",
  "caret",
  "e1071",
  "arules",
  "cluster",
  "tidytext",
  "tm",
  "ggplot2",
  "plotly",
  "lubridate"
)

installed <- rownames(installed.packages())

to_install <- setdiff(required_packages, installed)

if (length(to_install) > 0) {
  install.packages(to_install, dependencies = TRUE)
} else {
  message("All required packages are already installed.")
}


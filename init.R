# init.R
#
# Example R code to install packages if not already installed

my_packages = c("shiny",
                "shinyjs",
                "shinythemes",
                "bslib",
                "dbplyr",
                "lubridate",
                "plotly",
                "bsicons",
                "reactable",
                "echarts4r",
                "DBI",
                "stringr",
                "scales",
                "readr",
                "shinyWidgets",
                "shinymanager",
                "htmltools",
                "htmlwidgets",
                "shiny.fluent",
                "RMySQL",
                "janitor",
                "heatmaply",
                "mapboxer",
                "bcrypt",
                "dplyr",
                "shinyauthr",
                "DT",
                "xlsx"
)


install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
install.packages("bslib")
install.packages("bsicons")
install.packages("shiny")
install.packages("shinyjs")
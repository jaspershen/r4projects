## -----------------------------------------------------------------------
#' @title get_os
#' @description get os information.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @export
#' @return os name
#' @examples
#' get_os()
get_os <- function() {
  sysinf <- Sys.info()
  if (!is.null(sysinf)) {
    os <- sysinf["sysname"]
    if (os == "Darwin") {
      os <- "osx"
    }
  } else {
    ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os)) {
      os <- "osx"
    }
    if (grepl("linux-gnu", R.version$os)) {
      os <- "linux"
    }
  }
  tolower(os)
}


## -----------------------------------------------------------------------
#' @title create_project_organization
#' @description Create project organization.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @export
#' @return some folders
create_project_organization <-
  function() {
    current_wd <-
      getwd()
    ####code
    dir.create(file.path(current_wd, "1-code"),
               showWarnings = FALSE,
               recursive = TRUE)

    if (!file.exists(file.path(current_wd, "1-code/100-tools.R"))) {
      file.create(file.path(current_wd, "1-code/100-tools.R"),
                  showWarnings = FALSE)
      writeLines(
        "library(tidyverse)\nlibrary(ggplot2)",
        file.path(current_wd, "1-code/100-tools.R")
      )
    } else {
      warning("'1-code/100-tools.R' already exists. Not overwriting.\n")
    }

    file.create(file.path(current_wd, "1-code/101-demo_code.R"),
                showWarnings = FALSE)

    writeLines(
      "library(r4projects)\nsetwd(get_project_wd())\nrm(list = ls())\nsource('1-code/100-tools.R')",
      file.path(current_wd, "1-code/101-demo_code.R")
    )

    #####data
    dir.create(file.path(current_wd, "2-data"),
               showWarnings = FALSE,
               recursive = TRUE)

    ###data_analysis
    dir.create(
      file.path(current_wd, "3-data_analysis"),
      showWarnings = FALSE,
      recursive = TRUE
    )

    ###manuscript
    dir.create(
      file.path(current_wd, "4-manuscript"),
      showWarnings = FALSE,
      recursive = TRUE
    )

    dir.create(
      file.path(current_wd, "4-manuscript/Figures"),
      showWarnings = FALSE,
      recursive = TRUE
    )

    dir.create(
      file.path(current_wd, "4-manuscript/Supplementary_data"),
      showWarnings = FALSE,
      recursive = TRUE
    )

    dir.create(
      file.path(current_wd, "4-manuscript/Supplementary_figures"),
      showWarnings = FALSE,
      recursive = TRUE
    )

    ###summary
    dir.create(
      file.path(current_wd, "5-summary"),
      showWarnings = FALSE,
      recursive = TRUE
    )

  }

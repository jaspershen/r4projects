
#' List all packages in the r4projects
#'
#' @param include_self Include r4projects in the list?
#' @export
#' @return r4projects packages
#' @examples
#' r4projects_packages()
r4projects_packages <- function(include_self = TRUE) {
  raw <- utils::packageDescription("r4projects")$Imports
  imports <- strsplit(raw, ",")[[1]]
  parsed <- gsub("^\\s+|\\s+$", "", imports)
  names <-
    vapply(strsplit(parsed, "\\s+"), "[[", 1, FUN.VALUE = character(1))

  if (include_self) {
    names <- c(names, "r4projects")
  }

  names
}

invert <- function(x) {
  if (length(x) == 0) {
    return()
  }
  stacked <- utils::stack(x)
  tapply(as.character(stacked$ind), stacked$values, list)
}


style_grey <- function(level, ...) {
  crayon::style(paste0(...),
                crayon::make_style(grDevices::grey(level), grey = TRUE))
}



















#' @title show_progresser
#' @description show_progresser
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param index index for loop
#' @param progresser progresser
#' @return A data.frame
#' @importFrom magrittr %>%
#' @importFrom crayon green
#' @export

show_progresser <-
  function(index = 1:1000,
           progresser = c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) {
    idx <-
      seq(
        from = 1,
        to = max(index),
        length.out = length(progresser)
      ) %>%
      round()

    data.frame(idx = idx,
               progresser = paste0(progresser, "%"))
  }


#' @title install_fastgit
#' @description install packages from fastgit. Credit to Shixiang Wang
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param pkg pkg name from github, gitlab or gitee, "name/repo" format
#' @param from gitlab, github or gitee.
#' @param ... Other parameters for install_git
#' @return NULL
#' @export

install_fastgit <-
  function(pkg,
           from = c("gitee", "gitlab", "github"),
           ...) {
    from <- match.arg(from)

    if (from == "gitee") {
      if (!grepl("/", pkg)) {
        stop("Invalid package name, should in 'name/repo' format.")
      }
      remotes::install_git(paste0("https://gitee.com/", pkg), ...)
    } else {
      if (any(grepl(":", pkg))) {
        remotes::install_git(pkg, ...)
      } else {
        if (any(grepl("/", pkg))) {
          tryCatch(
            remotes::install_git(paste0("https://hub.fastgit.org/", pkg)),
            error = function(e) {
              message("Install error when use GitHub mirror, roll back to official GitHub.")
              remotes::install_github(pkg)
            }
          )
        }
      }
    }
  }

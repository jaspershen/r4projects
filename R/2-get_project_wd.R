#' @title get_project_wd
#' @description Get the working directory where Rproj object is in.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @importFrom magrittr %>%
#' @importFrom stringr str_split
#' @importFrom utils head
#' @importFrom utils tail
#' @return A working directory.
#' @export

get_project_wd <-
  function() {
    current_wd <-
      getwd()
    candidate_wd <-
      unlist(stringr::str_split(current_wd, "/"))

    if (length(candidate_wd) == 1) {
      candidate_wd <- current_wd
    } else {
      candidate_wd <-
        lapply(2:length(candidate_wd), function(i) {
          paste(candidate_wd[seq_len(i)], collapse = "/")
        })
    }

    candidate_wd <-
      rev(candidate_wd)

    for (i in seq_along(candidate_wd)) {
      wd <- candidate_wd[[i]]
      file_name <-
        list.files(wd,
                   recursive = ifelse(wd == current_wd, TRUE, FALSE),
                   full.names = TRUE)
      project_index <-
        grep(".Rproj", file_name)

      if (length(project_index) != 0) {
        project_wd <-
          file_name[project_index[1]] %>%
          stringr::str_split("/") %>%
          unlist() %>%
          head(-1) %>%
          paste(collapse = "/")
        message(
          "The project name is:",
          file_name[project_index[1]] %>%
            stringr::str_split("/") %>%
            unlist() %>%
            tail(1)
        )
        return(project_wd)
        break()
      }
    }
    if (length(project_index) == 0) {
      message("There are no .Rproj in your file. No change for wd.")
    }
  }

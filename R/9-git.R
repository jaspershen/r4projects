#' @title install_fastgit
#' @description install packages from fastgit. Credit to Shixiang Wang
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param pkg pkg name from github, gitlab or gitee, "name/repo" format
#' @param from gitlab, github or gitee.
#' @param ... Other parameters for install_git
#' @importFrom remotes install_git
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




#' Add Large Files to .gitignore
#'
#' This function scans a specified folder for files larger than a given size limit
#' and appends their paths to the '.gitignore' file to prevent them from being
#' committed to a Git repository.
#'
#' @param path Character string specifying the path to the folder to be scanned.
#' @param size_limit Numeric value indicating the size limit in megabytes (MB).
#'        Files larger than this limit will be added to '.gitignore'. Default is 200 MB.
#'
#' @return A message indicating the number of large files added to '.gitignore',
#'         or a message stating that no large files were found.
#' @export
ignore_large_files <-
  function(path,
           size_limit = 200) {
    # List all files in the folder recursively
    all_files <-
      list.files(path, recursive = TRUE, full.names = TRUE)

    # Initialize a vector to store large files
    large_files <- c()

    # Loop through each file to check its size
    for (file in all_files) {
      file_size_MB <-
        file.info(file)$size / (1024 * 1024)  # Convert size to MB
      if (file_size_MB > size_limit) {
        large_files <- c(large_files, file)
      }
    }

    # If there are large files, append them to .gitignore
    if (length(large_files) > 0) {
      # Make the paths relative to the folder where .gitignore is located
      large_files_relative <-
        gsub(paste0(path, "/"), "", large_files)

      # Open .gitignore file for appending
      con <- file(".gitignore", "a")

      # Write each large file path to .gitignore
      for (file in large_files_relative) {
        writeLines(paste0(file), con)
      }

      # Close the connection
      close(con)

      message(paste0("Added ", length(large_files), " large files to .gitignore"))
    } else {
      message("No large files found.")
    }
  }

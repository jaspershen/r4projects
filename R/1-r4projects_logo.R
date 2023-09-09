#' @title r4projects_logo
#' @description Get the detailed of metPath package.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @importFrom utils packageDescription head tail download.file
#' @importFrom cli rule symbol
#' @importFrom crayon green blue col_align
#' @importFrom crayon red black white style make_style num_colors
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer
#' @export
#' @return r4projects logo
#' @examples
#' r4projects_logo()
r4projects_logo <-
  function() {
    message("Thank you for using r4projects!")
    message("Version ", r4projects_version, " (", update_date, ')')
    cat(
      c("           d8888                            d8b                   888             ",
        "          d8P888                            Y8P                   888             ",
        "         d8P 888                                                  888             ",
        "888d888 d8P  888  88888b.  888d888 .d88b.  8888  .d88b.   .d8888b 888888 .d8888b  ",
        "888P\"  d88   888  888 \"88b 888P\"  d88\"\"88b \"888 d8P  Y8b d88P\"    888    88K      ",
        "888    8888888888 888  888 888    888  888  888 88888888 888      888    \"Y8888b. ",
        "888          888  888 d88P 888    Y88..88P  888 Y8b.     Y88b.    Y88b.       X88 ",
        "888          888  88888P\"  888     \"Y88P\"   888  \"Y8888   \"Y8888P  \"Y888  88888P' ",
        "                  888                       888                                   ",
        "                  888                      d88P                                   ",
        "                  888                    888P\"                                    "
      ),
      sep = "\n"
    )
  }

r4projects_version <-
  as.character(utils::packageVersion(pkg = "r4projects"))
update_date <- as.character(Sys.time())

# library(cowsay)
# # https://onlineasciitools.com/convert-text-to-ascii-art
# # writeLines(capture.output(say("Hello"), type = "message"), con = "ascii_art.txt")
# art <- readLines("logo.txt")
# dput(art)
# metid_logo <-
#   c("           d8888                            d8b                   888             ",
#     "          d8P888                            Y8P                   888             ",
#     "         d8P 888                                                  888             ",
#     "888d888 d8P  888  88888b.  888d888 .d88b.  8888  .d88b.   .d8888b 888888 .d8888b  ",
#     "888P\"  d88   888  888 \"88b 888P\"  d88\"\"88b \"888 d8P  Y8b d88P\"    888    88K      ",
#     "888    8888888888 888  888 888    888  888  888 88888888 888      888    \"Y8888b. ",
#     "888          888  888 d88P 888    Y88..88P  888 Y8b.     Y88b.    Y88b.       X88 ",
#     "888          888  88888P\"  888     \"Y88P\"   888  \"Y8888   \"Y8888P  \"Y888  88888P' ",
#     "                  888                       888                                   ",
#     "                  888                      d88P                                   ",
#     "                  888                    888P\"                                    "
#   )
# cat(metid_logo, sep = "\n")

.onAttach <- function(...) {
  # needed <- core[!is_attached(core)]
  # if (length(needed) == 0) {
  #       return()
  #   }
  #
  crayon::num_colors(TRUE)
 r4projects_attach()

  # if (!"package:conflicted" %in% search()) {
  #     x <-r4projects_conflicts()
  #     msg(r4projects_conflict_message(x), startup = TRUE)
  # }
  packageStartupMessage(paste0("r4projects ",r4projects_version, " (", update_date, ')'))
}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}

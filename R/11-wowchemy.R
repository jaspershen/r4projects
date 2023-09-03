#' @title update_packages_tidymass_org
#' @description Update packages in tidymass.org. Please set the path to "/Users/xiaotaoshen/tidymass" first.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param path_tidymass.org path_tidymass.org
#' @param path_packages path_packages
#' @importFrom magrittr %>%
#' @importFrom rvest read_html html_node html_text html_text2 html_elements
#' @importFrom stringr str_sort str_replace_all str_extract
#' @importFrom dplyr mutate group_by slice_tail ungroup left_join
#' @importFrom purrr walk
#' @importFrom readr write_csv
#' @return Profile.
#' @export
update_packages_tidymass_org <-
  function(path_tidymass.org = "tidymass-website/static/tidymass-packages",
           path_packages = ".") {
    file1 <-
      dir(path_tidymass.org,
          pattern = "tar.gz") %>%
      stringr::str_sort(numeric = TRUE) %>%
      data.frame(file_name = .) %>%
      dplyr::mutate(
        package = stringr::str_replace_all(file_name, "\\_[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{1,2}", "")
      ) %>%
      dplyr::mutate(package = stringr::str_replace_all(package, "\\.tar\\.gz", "")) %>%
      dplyr::mutate(version = stringr::str_extract(file_name, "[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{1,2}")) %>%
      dplyr::group_by(package) %>%
      dplyr::slice_tail(n = 1) %>%
      dplyr::ungroup()

    file2 <- dir(path_packages, pattern = "tar.gz") %>%
      stringr::str_sort(numeric = TRUE) %>%
      data.frame(file_name = .) %>%
      dplyr::mutate(
        package = stringr::str_replace_all(file_name, "\\_[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{1,2}", "")
      ) %>%
      dplyr::mutate(package = stringr::str_replace_all(package, "\\.tar\\.gz", "")) %>%
      dplyr::mutate(version = stringr::str_extract(file_name, "[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{1,2}")) %>%
      dplyr::group_by(package) %>%
      dplyr::slice_tail(n = 1) %>%
      dplyr::ungroup()

    file <-
      file1 %>%
      dplyr::left_join(file2, by = "package")

    ###move new package
    seq_len(nrow(file)) %>%
      purrr::walk(function(i) {
        message(i)
        if (file$version.x[i] != file$version.y[i]) {
          unlink(file.path(path_tidymass.org, file$file_name.x[i]))
          file.copy(from = file.path(path_packages,  file$file_name.y[i]),
                    to = path_tidymass.org)
          file.copy(
            from = file.path(path_packages,  file$package[i], "/Description"),
            to = paste0(path_tidymass.org,
                        file$package[i],
                        "_Description.txt")
          )
        }
      })
    readr::write_csv(file, file = file.path(path_tidymass.org, "file.csv"))
  }


# generate_publication4wowchemy <-
#   function(path = ".",
#            pmid = "37468756",
#            featured = "false",
#            publication_types = c("1", "2", "3")) {
#     publication_info <-
#       request_pubmed_publication_info(pmid = pmid)
#   }

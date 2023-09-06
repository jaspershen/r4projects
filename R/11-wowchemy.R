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


generate_publication4wowchemy <-
  function(path = ".",
           user_id = "3TK9yz8AAAAJ",
           force = FALSE,
           interval = 7,
           admin = "Xiaotao Shen",
           featured = "false",
           publication_types = c("1", "2", "3")) {
    publication_types <- match.arg(publication_types)
    publications <-
      request_publications(user_id = user_id,
                           force = force,
                           interval = interval)

    seq_len(nrow(publications)) %>%
      purrr::walk(function(i) {
        cat(i, " ")
        pub_path <-
          file.path(path, publications$publication_title[i])
        dir.create(pub_path,
                   recursive = TRUE,
                   showWarnings = FALSE)

        file <-
          file.path(pub_path, "index.txt")

        writeLines("---", file)

        if (is.na(publications$publication_title[i])) {
          publication_title <- ""
        } else{
          publication_title <-
            publications$publication_title[i]
        }
        write(paste0("title: ", publication_title),
              file,
              append = TRUE)
        write("", file, append = TRUE)


        if (is.na(publications$Abstract[i])) {
          Abstract <- ""
        } else{
          Abstract <-
            publications$Abstract[i]
        }
        write(paste0("abstract: ", Abstract),
              file,
              append = TRUE)

        write("", file, append = TRUE)

        write("authors:", file, append = TRUE)

        if (is.na(publications$Authors[i])) {
          Authors <- ""
        } else{
          Authors <-
            publications$Authors[i]
        }

        if (Authors == "") {
          write(paste0("- ", "admin"),
                file,
                append = TRUE)
        } else{
          publications$Authors[i] %>%
            stringr::str_split(",") %>%
            `[[`(1) %>%
            stringr::str_trim(side = "both") %>%
            lapply(function(author) {
              if (author == admin) {
                author <- "admin"
              }
              write(paste0("- ", author),
                    file,
                    append = TRUE)
            })
        }

        write("", file, append = TRUE)
        write(paste0("featured: ", featured),
              file,
              append = TRUE)

        if (is.na(publications$Journal[i])) {
          Journal <- ""
        } else{
          Journal <-
            publications$Journal[i]
        }

        write(paste0("publication: ", Journal),
              file,
              append = TRUE)

        write("publication_types:",
              file,
              append = TRUE)

        write(paste0("- ", publication_types),
              file,
              append = TRUE)

        publishDate <-
          publications$Publication_date[i]

        if (is.na(publishDate)) {
          publishDate <-
            as.character(Sys.Date())
        } else{
          publishDate <-
            tryCatch(
              as.character(as.Date(publishDate)),
              error = function(e) {
                return(as.character(Sys.Date()))
              }
            )
        }

        write(paste0("publishDate: ", publishDate),
              file,
              append = TRUE)

        write(paste0("summary: ", Abstract),
              file,
              append = TRUE)

        write("links:",
              file,
              append = TRUE)
        write("- icon: link",
              file,
              append = TRUE)
        write("  icon_pack: fas",
              file,
              append = TRUE)
        write("  name: Link",
              file,
              append = TRUE)

        if (is.na(publications$publication_link[i])) {
          publication_link <- ""
        } else{
          publication_link <-
            publications$publication_link[i]
        }

        write(paste0("  url: ", publication_link),
              file,
              append = TRUE)
        write("- icon: file-pdf",
              file,
              append = TRUE)
        write("  icon_pack: fas",
              file,
              append = TRUE)
        write("  name: PDF",
              file,
              append = TRUE)
        write(paste0("  url: "),
              file,
              append = TRUE)
        write("---",
              file,
              append = TRUE)
        file.rename(file, file.path(pub_path, "index.md"))
      })
  }




#' @title generate_collaborator_map
#' @description Generate map of collaborators
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param gs_user_id user google scholar ID
#' @param force Force read webpage?
#' @param interval If there is old data on the local machine,
#' do you want to use it and the interval (day)?
#' @param map_color color of map
#' @param point_color color of point
#' @importFrom magrittr %>%
#' @importFrom rvest read_html html_node html_text html_text2 html_elements
#' @importFrom stringr str_sort str_replace_all str_extract
#' @importFrom dplyr mutate group_by slice_tail ungroup left_join desc
#' @importFrom purrr walk
#' @importFrom readr write_csv
#' @importFrom tmaptools geocode_OSM
#' @import ggplot2
#' @return Profile.
#' @export
generate_collaborator_map <-
  function(gs_user_id = "3TK9yz8AAAAJ",
           force = FALSE,
           interval = 7,
           map_color = "#76b5c5",
           point_color = "#873e23") {
    publications <-
      request_publications(user_id = gs_user_id,
                           force = force,
                           interval = interval)

    all_affiliations <-
      publications$publication_id %>%
      lapply(function(id) {
        cat(id, " ")
        pmid <-
          convert_gs_pub_id2pmid(
            gs_user_id = gs_user_id,
            gs_pub_id = id,
            interval = interval,
            force = force
          )
        if (is.na(pmid)) {
          return(NA)
        } else{
          Sys.sleep(3)
          result <-
            request_pubmed_publication_info(pmid = pmid)
          affiliations <-
            result$affiliations
          return(affiliations)
        }
      })

    all_affiliations <-
      unlist(all_affiliations)

    all_affiliations <-
      all_affiliations[!is.na(all_affiliations)]

    all_affiliations <-
      all_affiliations %>%
      stringr::str_replace("^[0-9]{1,3}", "") %>%
      stringr::str_replace("\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,4}\\b",
                           "") %>%
      stringr::str_replace("Electronic address", "") %>%
      stringr::str_replace_all("\\.", "") %>%
      stringr::str_replace_all("\\:", "") %>%
      stringr::str_replace_all("\\;", ",") %>%
      stringr::str_trim(side = "both")

    all_affiliations <-
      all_affiliations[all_affiliations != ""]

    all_locations <-
      all_affiliations %>%
      lapply(function(x) {
        # cat(x, "\n")
        x <-
          stringr::str_split(x, ",")[[1]] %>%
          stringr::str_trim(side = "both")

        if (length(x) > 3) {
          x1 <- tail(x, 3) %>%
            paste(collapse = ", ")
        } else{
          x1 <- x %>%
            paste(collapse = ", ")
        }

        if (length(x) > 2) {
          x2 <- tail(x, 2) %>%
            paste(collapse = ", ")
        } else{
          x2 <- x %>%
            paste(collapse = ", ")
        }

        result <-
          tryCatch(
            tmaptools::geocode_OSM(x1),
            error = function(e) {
              return(NULL)
            }
          )

        if (is.null(result)) {
          result <-
            tryCatch(
              tmaptools::geocode_OSM(x2),
              error = function(e) {
                return(NULL)
              }
            )
        }

        if (is.null(result)) {
          return(NULL)
        } else{
          return(result$coords)
        }
      })

    all_locations <-
      all_locations %>%
      do.call(rbind, .) %>%
      as.data.frame()

    all_locations$id <-
      paste(all_locations$x,
            all_locations$y, sep = "_")

    location_count <-
      all_locations %>%
      dplyr::count(id)

    all_locations <-
      all_locations %>%
      dplyr::distinct(id, .keep_all = TRUE) %>%
      dplyr::left_join(location_count, by = "id") %>%
      dplyr::arrange(dplyr::desc(n))

    ###map to show it
    # library(ggmap)
    # library(mapdata)
    # library(maps)
    world <- ggplot2::map_data("world")

    world <- world %>%
      dplyr::filter(region != "Antarctica" & region != "Greenland")

    gg1 <- ggplot(data = world, aes(x = long, y = lat)) +
      geom_polygon(aes(group = group),
                   fill = map_color) +
      theme_bw() +
      labs(x = "", y = "") +
      theme(
        legend.position = "none",
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank()
      )

    gg1 <-
      gg1 +
      geom_point(
        data = all_locations,
        aes(x = x, y = y, size = n),
        shape = 16,
        color = point_color
      ) +
      scale_size_continuous(range = c(1, 4))

    gg1
  }





















# htmlwidgets::saveWidget(plot, "map.html")

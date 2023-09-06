#' @title craw_or_load_page
#' @description Craw or load webpage data.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param user_id User google scholar ID
#' @param force Force read webpage?
#' @param interval If there is old data on the local machine,
#' do you want to use it and the interval (day)?
#' @importFrom magrittr %>%
#' @importFrom rvest read_html html_node html_text html_text2 html_elements
#' @importFrom rvest html_element html_attr html_table html_nodes
#' @importFrom tibble as_tibble
#' @importFrom xml2 read_html write_html
#' @return page data
#' @export

craw_or_load_page <-
  function(user_id = "3TK9yz8AAAAJ",
           force = FALSE,
           interval = 7) {
    url <-
      paste0(
        "https://scholar.google.com/citations?hl=en&user=",
        user_id,
        "&pagesize=80&view_op=list_works&sortby=pubdate"
      )

    package_path <-
      system.file(package = "r4projects")

    user_path <-
      file.path(package_path, paste0("cache/", user_id))

    dir.create(user_path,
               showWarnings = FALSE,
               recursive = TRUE)

    if (force) {
      Sys.sleep(time = 3)
      page_data <-
        rvest::read_html(x = url)
      xml2::write_html(page_data, file = paste0(user_path, "/page_data.html"))
    } else{
      if (any(dir(user_path) == "page_data.html")) {
        file_info <- file.info(file.path(user_path, "page_data.html"))
        diff_time <-
          difftime(Sys.time(), file_info$ctime, units = "days") %>%
          as.numeric()
        if (diff_time >= interval) {
          unlink(file.path(user_path, "page_data.html"))
        }
      }

      if (any(dir(user_path) == "page_data.html")) {
        page_data <-
          xml2::read_html(paste0(user_path, "/page_data.html"))
        message("Previouse webpage data is in ", user_path)
        message(
          "Use previouse webpage data less than ",
          interval,
          " days, if you want to use latest data, set force as TRUE"
        )
      } else{
        Sys.sleep(time = 3)
        page_data <-
          rvest::read_html(x = url)
        xml2::write_html(page_data, file = paste0(user_path, "/page_data.html"))
      }
    }
    page_data
  }


#' @title craw_or_load_publication
#' @description Craw or load webpage data of one publication.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param user_id User google scholar ID
#' @param pub_id publication ID
#' @param force Force read webpage?
#' @param interval If there is old data on the local machine,
#' do you want to use it and the interval (day)?
#' @importFrom magrittr %>%
#' @importFrom rvest read_html html_node html_text html_text2 html_elements
#' @importFrom rvest html_element html_attr html_table html_nodes
#' @importFrom tibble as_tibble
#' @importFrom xml2 read_html write_html
#' @return page data
#' @export

craw_or_load_publication <-
  function(user_id = "3TK9yz8AAAAJ",
           pub_id = "2osOgNQ5qMEC",
           force = FALSE,
           interval = 7) {
    pub_url <-
      paste0(
        "https://scholar.google.com/citations?view_op=view_citation&hl=en&user=",
        user_id,
        "&pagesize=80&sortby=pubdate&citation_for_view=",
        user_id,
        ":",
        pub_id
      )

    package_path <-
      system.file(package = "r4projects")

    user_path <-
      file.path(package_path, paste0("cache/", user_id))

    pub_path <- file.path(user_path, pub_id)

    dir.create(pub_path,
               showWarnings = FALSE,
               recursive = TRUE)


    if (force) {
      Sys.sleep(time = 3)
      pub_data <-
        rvest::read_html(x = pub_url)
      xml2::write_html(pub_data, file = paste0(pub_path, "/pub_data.html"))
    } else{
      if (any(dir(pub_path) == "pub_data.html")) {
        file_info <- file.info(file.path(pub_path, "pub_data.html"))
        diff_time <-
          difftime(Sys.time(), file_info$ctime, units = "days") %>%
          as.numeric()
        if (diff_time >= interval) {
          unlink(file.path(pub_path, "pub_data.html"))
        }
      }

      if (any(dir(pub_path) == "pub_data.html")) {
        pub_data <-
          xml2::read_html(paste0(pub_path, "/pub_data.html"))
        message("Previouse webpage data is in ", user_path)
        message(
          "Use previouse webpage data less than ",
          interval,
          " days, if you want to use latest data, set force as TRUE"
        )
      } else{
        Sys.sleep(time = 3)
        pub_data <-
          rvest::read_html(x = pub_url)
        xml2::write_html(pub_data, file = paste0(pub_path, "/pub_data.html"))
      }
    }

    pub_data
  }


#' @title request_profile
#' @description Request the profile.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param user_id User google scholar ID
#' @param force Force read webpage?
#' @param interval If there is old data on the local machine,
#' do you want to use it and the interval (day)?
#' @importFrom magrittr %>%
#' @importFrom rvest read_html html_node html_text html_text2 html_elements
#' @importFrom rvest html_element html_attr html_table html_nodes
#' @importFrom tibble as_tibble
#' @importFrom xml2 read_html write_html
#' @return Profile.
#' @export

request_profile <-
  function(user_id = "3TK9yz8AAAAJ",
           force = FALSE,
           interval = 7) {
    url <-
      paste0(
        "https://scholar.google.com/citations?user=",
        user_id,
        "&cstart=0&pagesize=100"
      )

    page_data <-
      craw_or_load_page(user_id = user_id,
                        force = force,
                        interval = interval)

    user_name <-
      page_data %>%
      rvest::html_node("#gsc_prf_in") %>%
      rvest::html_text()

    affiliation <-
      page_data %>%
      rvest::html_node(".gsc_prf_il") %>%
      rvest::html_text()

    homepage <-
      page_data %>%
      rvest::html_node("#gsc_prf_ivh") %>%
      rvest::html_elements("a") %>%
      rvest::html_attr("href")

    areas_of_interest <-
      page_data %>%
      rvest::html_node("#gsc_prf_int") %>%
      rvest::html_elements("a") %>%
      rvest::html_text()

    other_names <-
      page_data %>%
      rvest::html_node("#gsc_prf_ion") %>%
      rvest::html_elements("span") %>%
      rvest::html_text()

    other_names <-
      other_names[other_names != "Other names"]

    list(
      user_name = user_name,
      affiliation = affiliation,
      homepage = homepage,
      areas_of_interest = areas_of_interest,
      other_names = other_names
    )
  }




#' @title request_citation
#' @description Request citation information.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param user_id User google scholar ID
#' @param force Force read webpage?
#' @param interval If there is old data on the local machine,
#' do you want to use it and the interval (day)?
#' @importFrom magrittr %>%
#' @importFrom rvest read_html html_node html_text html_text2 html_elements
#' @importFrom rvest html_element html_attr html_table
#' @return Profile.
#' @export
request_citation <-
  function(user_id = "3TK9yz8AAAAJ",
           force = FALSE,
           interval = 7) {
    url <-
      paste0(
        "https://scholar.google.com/citations?user=",
        user_id,
        "&cstart=0&pagesize=100"
      )

    page_data <-
      craw_or_load_page(user_id = user_id,
                        force = force,
                        interval = interval)

    citation_summary <-
      page_data %>%
      rvest::html_node("#gsc_rsb_st") %>%
      html_table() %>%
      as.data.frame()

    year <-
      page_data %>%
      html_nodes(".gsc_g_t") %>%
      html_text() %>%
      as.numeric()

    citation <-
      page_data %>%
      html_nodes(".gsc_g_a") %>%
      html_text() %>%
      as.numeric()

    citation_history <-
      data.frame(year = year,
                 citation = citation)

    list(citation_summary = citation_summary,
         citation_history = citation_history)
  }


#' @title draw_citation_history
#' @description Draw citation history plot.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param user_id User google scholar ID
#' @param force Force read webpage?
#' @param interval If there is old data on the local machine,
#' do you want to use it and the interval (day)?
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_bar theme_bw labs
#' @return Profile.
#' @export

draw_citation_history <-
  function(user_id = "3TK9yz8AAAAJ",
           force = FALSE,
           interval = 7) {
    url <-
      paste0(
        "https://scholar.google.com/citations?user=",
        user_id,
        "&cstart=0&pagesize=100"
      )

    page_data <-
      craw_or_load_page(user_id = user_id,
                        force = force,
                        interval = interval)

    year <-
      page_data %>%
      html_nodes(".gsc_g_t") %>%
      html_text() %>%
      as.numeric()

    citation <-
      page_data %>%
      html_nodes(".gsc_g_a") %>%
      html_text() %>%
      as.numeric()

    citation_history <-
      data.frame(year = year,
                 citation = citation)

    citation_history %>%
      ggplot(aes(year, citation)) +
      geom_bar(stat = "identity") +
      theme_bw() +
      labs(x = "Year", y = "Citation number")

  }



#' @title request_coauthors
#' @description Request coauthor information
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param user_id User google scholar ID
#' @param force Force read webpage?
#' @param interval If there is old data on the local machine,
#' do you want to use it and the interval (day)?
#' @importFrom magrittr %>%
#' @importFrom stringr str_extract str_replace str_detect
#' @return Information of coauthors
#' @export
request_coauthors <-
  function(user_id = "3TK9yz8AAAAJ",
           force = FALSE,
           interval = 7) {
    url <-
      paste0(
        "https://scholar.google.com/citations?user=",
        user_id,
        "&cstart=0&pagesize=100"
      )

    page_data <-
      craw_or_load_page(user_id = user_id,
                        force = force,
                        interval = interval)

    coauthor_names <-
      page_data %>%
      rvest::html_nodes(".gsc_rsb_a_desc") %>%
      html_nodes("a") %>%
      html_text()

    coauthor_user_ids <-
      page_data %>%
      rvest::html_nodes(".gsc_rsb_a_desc") %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr(name = "href") %>%
      stringr::str_extract("\\=[a-zA-Z0-9-_]{10,20}\\&") %>%
      stringr::str_replace("\\=", "") %>%
      stringr::str_replace("\\&", "")

    coauthor_affliations <-
      page_data %>%
      rvest::html_nodes(".gsc_rsb_a_ext") %>%
      rvest::html_text()

    coauthor_location <-
      coauthor_affliations[stringr::str_detect(coauthor_affliations, "Verified")] %>%
      stringr::str_replace("Verified email at ", "")

    coauthor_affliations <-
      coauthor_affliations[!stringr::str_detect(coauthor_affliations, "Verified")]

    data.frame(
      names = coauthor_names,
      user_ids = coauthor_user_ids,
      affliations = coauthor_affliations,
      location = coauthor_location
    ) %>%
      tibble::as_tibble()

  }


#' @title request_publications
#' @description Request all publications
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param user_id User google scholar ID
#' @param force Force read webpage?
#' @param interval If there is old data on the local machine,
#' do you want to use it and the interval (day)?
#' @importFrom magrittr %>%
#' @importFrom stringr str_extract str_replace str_detect
#' @importFrom purrr map
#' @return Information of publications
#' @export

request_publications <-
  function(user_id = "3TK9yz8AAAAJ",
           force = FALSE,
           interval = 7) {
    url <-
      paste0(
        "https://scholar.google.com/citations?user=",
        user_id,
        "&cstart=0&pagesize=100"
      )

    page_data <-
      craw_or_load_page(user_id = user_id,
                        force = force,
                        interval = interval)

    publications <- page_data %>%
      html_nodes(".gsc_a_tr")

    publication_id <- publications %>%
      html_nodes(".gsc_a_at") %>%
      html_attr("href") %>%
      str_extract(":.*$") %>%
      stringr::str_replace(pattern = "\\:", "")

    publication_id %>%
      purrr::map(function(id) {
        cat(id, " ")
        pub_url <-
          paste0(
            "https://scholar.google.com/citations?view_op=view_citation&hl=en&user=",
            user_id,
            "&citation_for_view=",
            user_id,
            ":",
            id
          )

        page <-
          craw_or_load_publication(
            user_id = user_id,
            pub_id = id,
            force = force,
            interval = interval
          )

        publication_title <-
          page %>%
          html_node("#gsc_oci_title") %>%
          html_text()

        publication_link <-
          page %>%
          html_node("#gsc_oci_title") %>%
          html_node("a") %>%
          html_attr("href")

        labels <-
          page %>%
          html_nodes(".gs_scl") %>%
          html_nodes("div.gsc_oci_field") %>%
          html_text2()

        values <-
          page %>%
          html_nodes(".gs_scl") %>%
          html_nodes("div.gsc_oci_value") %>%
          html_text2()

        Authors = values[labels == "Authors"]
        if (length(Authors) == 0) {
          Authors <- NA
        }
        Publication_date = values[labels == "Publication date"]
        if (length(Publication_date) == 0) {
          Publication_date <- NA
        }
        Journal = values[labels == "Journal"]
        if (length(Journal) == 0) {
          Journal <- NA
        }
        Volume = values[labels == "Volume"]
        if (length(Volume) == 0) {
          Volume <- NA
        }
        Issue = values[labels == "Issue"]
        if (length(Issue) == 0) {
          Issue <- NA
        }
        Pages = values[labels == "Pages"]
        if (length(Pages) == 0) {
          Pages <- NA
        }
        Publisher = values[labels == "Publisher"]
        if (length(Publisher) == 0) {
          Publisher <- NA
        }
        Total_citations = values[labels == "Total citations"] %>%
          stringr::str_extract("Cited by [0-9]{1,5}") %>%
          stringr::str_replace("Cited by ", "") %>%
          as.numeric()
        if (length(Total_citations) == 0) {
          Total_citations <- NA
        }

        Abstract <-
          values[labels == "Description"]

        if (length(Abstract) == 0) {
          Abstract <- NA
        }

        data.frame(
          publication_id = id,
          publication_title = publication_title,
          publication_link = publication_link,
          Authors = Authors,
          Publication_date = Publication_date,
          Journal = Journal,
          Volume = Volume,
          Issue = Issue,
          Pages = Pages,
          Publisher = Publisher,
          Total_citations = Total_citations,
          Abstract = Abstract
        ) %>%
          tibble::as_tibble()
      }) %>%
      do.call(rbind, .) %>%
      tibble::as_tibble()
  }





#' @title request_publication_info
#' @description Request one publication info
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param user_id User google scholar ID
#' @param pub_id Publication google scholar ID
#' @param force Force read webpage?
#' @param interval If there is old data on the local machine,
#' do you want to use it and the interval (day)?
#' @importFrom magrittr %>%
#' @importFrom stringr str_extract str_replace str_detect
#' @importFrom purrr map
#' @return Information of publications
#' @export

request_publication_info <-
  function(user_id = "3TK9yz8AAAAJ",
           pub_id = "0EnyYjriUFMC",
           force = FALSE,
           interval = 7) {
    pub_url <-
      paste0(
        "https://scholar.google.com/citations?view_op=view_citation&hl=en&user=",
        user_id,
        "&citation_for_view=",
        user_id,
        ":",
        pub_id
      )

    page <-
      craw_or_load_publication(
        user_id = user_id,
        pub_id = pub_id,
        force = force,
        interval = interval
      )

    publication_title <-
      page %>%
      html_node("#gsc_oci_title") %>%
      html_text()

    publication_link <-
      page %>%
      html_node("#gsc_oci_title") %>%
      html_node("a") %>%
      html_attr("href")

    labels <-
      page %>%
      html_nodes(".gs_scl") %>%
      html_nodes("div.gsc_oci_field") %>%
      html_text2()

    values <-
      page %>%
      html_nodes(".gs_scl") %>%
      html_nodes("div.gsc_oci_value") %>%
      html_text2()

    Authors = values[labels == "Authors"]
    if (length(Authors) == 0) {
      Authors <- NA
    }
    Publication_date = values[labels == "Publication date"]
    if (length(Publication_date) == 0) {
      Publication_date <- NA
    }
    Journal = values[labels == "Journal"]
    if (length(Journal) == 0) {
      Journal <- NA
    }
    Volume = values[labels == "Volume"]
    if (length(Volume) == 0) {
      Volume <- NA
    }
    Issue = values[labels == "Issue"]
    if (length(Issue) == 0) {
      Issue <- NA
    }
    Pages = values[labels == "Pages"]
    if (length(Pages) == 0) {
      Pages <- NA
    }
    Publisher = values[labels == "Publisher"]
    if (length(Publisher) == 0) {
      Publisher <- NA
    }
    Total_citations = values[labels == "Total citations"] %>%
      stringr::str_extract("Cited by [0-9]{1,5}") %>%
      stringr::str_replace("Cited by ", "") %>%
      as.numeric()
    if (length(Total_citations) == 0) {
      Total_citations <- NA
    }

    Abstract <-
      values[labels == "Description"]

    if (length(Abstract) == 0) {
      Abstract <- NA
    }

    data.frame(
      publication_id = id,
      publication_title = publication_title,
      publication_link = publication_link,
      Authors = Authors,
      Publication_date = Publication_date,
      Journal = Journal,
      Volume = Volume,
      Issue = Issue,
      Pages = Pages,
      Publisher = Publisher,
      Total_citations = Total_citations,
      Abstract = Abstract
    ) %>%
      tibble::as_tibble()
  }

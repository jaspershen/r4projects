#' @title request_profile
#' @description Request the profile.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param user_id User google scholar ID
#' @importFrom magrittr %>%
#' @importFrom rvest read_html html_node html_text html_text2 html_elements
#' @importFrom rvest html_element html_attr html_table html_nodes
#' @importFrom tibble as_tibble
#' @return Profile.
#' @export

request_profile <-
  function(user_id = "3TK9yz8AAAAJ") {
    url <- paste0("https://scholar.google.com/citations?user=", user_id)

    data <-
      rvest::read_html(x = url)

    user_name <-
      data %>%
      rvest::html_node("#gsc_prf_in") %>%
      rvest::html_text()

    affiliation <-
      data %>%
      rvest::html_node(".gsc_prf_il") %>%
      rvest::html_text()

    homepage <-
      data %>%
      rvest::html_node("#gsc_prf_ivh") %>%
      html_elements("a") %>%
      html_attr("href")

    areas_of_interest <-
      data %>%
      rvest::html_node("#gsc_prf_int") %>%
      html_elements("a") %>%
      rvest::html_text()

    other_names <-
      data %>%
      rvest::html_node("#gsc_prf_ion") %>%
      html_elements("span") %>%
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
#' @importFrom magrittr %>%
#' @importFrom rvest read_html html_node html_text html_text2 html_elements
#' @importFrom rvest html_element html_attr html_table
#' @return Profile.
#' @export
request_citation <-
  function(user_id = "3TK9yz8AAAAJ") {
    url <- paste0("https://scholar.google.com/citations?user=", user_id)

    data <-
      rvest::read_html(x = url)

    citation_summary <-
      data %>%
      rvest::html_node("#gsc_rsb_st") %>%
      html_table() %>%
      as.data.frame()

    year <-
      data %>%
      html_nodes(".gsc_g_t") %>%
      html_text() %>%
      as.numeric()

    citation <-
      data %>%
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
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_bar theme_bw labs
#' @return Profile.
#' @export

draw_citation_history <-
  function(user_id = "3TK9yz8AAAAJ") {
    url <- paste0("https://scholar.google.com/citations?user=", user_id)

    data <-
      rvest::read_html(x = url)

    year <-
      data %>%
      html_nodes(".gsc_g_t") %>%
      html_text() %>%
      as.numeric()

    citation <-
      data %>%
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
#' @importFrom magrittr %>%
#' @importFrom stringr str_extract str_replace str_detect
#' @return Information of coauthors
#' @export
request_coauthors <-
  function(user_id = "3TK9yz8AAAAJ") {
    url <- paste0("https://scholar.google.com/citations?user=", user_id)

    data <-
      rvest::read_html(x = url)

    coauthor_names <-
      data %>%
      rvest::html_nodes(".gsc_rsb_a_desc") %>%
      html_nodes("a") %>%
      html_text()

    coauthor_user_ids <-
      data %>%
      rvest::html_nodes(".gsc_rsb_a_desc") %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr(name = "href") %>%
      stringr::str_extract("\\=[a-zA-Z0-9-_]{10,20}\\&") %>%
      stringr::str_replace("\\=", "") %>%
      stringr::str_replace("\\&", "")

    coauthor_affliations <-
      data %>%
      rvest::html_nodes(".gsc_rsb_a_ext") %>%
      rvest::html_text()

    coauthor_affliations <-
      coauthor_affliations[!stringr::str_detect(coauthor_affliations, "Verified")]


    data.frame(names = coauthor_names,
               user_ids = coauthor_user_ids,
               affliations = coauthor_affliations)

  }


#' @title request_publications
#' @description Request all publications
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param user_id User google scholar ID
#' @importFrom magrittr %>%
#' @importFrom stringr str_extract str_replace str_detect
#' @importFrom purrr map
#' @return Information of publications
#' @export

request_publications <-
  function(user_id = "3TK9yz8AAAAJ") {
    url <-
      paste0(
        "https://scholar.google.com/citations?user=",
        user_id,
        "&cstart=0&pagesize=100"
      )

    data <-
      rvest::read_html(x = url)

    publications <- data %>%
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
            "&citation_for_view=3TK9yz8AAAAJ:",
            id
          )
        page <-
          rvest::read_html(x = pub_url)

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
        data.frame(
          publication_title = publication_title,
          publication_link = publication_link,
          Authors = Authors,
          Publication_date = Publication_date,
          Journal = Journal,
          Volume = Volume,
          Issue = Issue,
          Pages = Pages,
          Publisher = Publisher,
          Total_citations = Total_citations
        ) %>%
          tibble::as_tibble()
      }) %>%
      do.call(rbind, .) %>%
      tibble::as_tibble()
  }

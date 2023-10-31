#' Convert Google Scholar Publication ID to PubMed ID
#'
#' This function takes a Google Scholar user ID and a Google Scholar publication ID,
#' retrieves the publication information, and then searches for the corresponding
#' PubMed ID (PMID).
#'
#' @param gs_user_id Character string specifying the Google Scholar user ID. Default is "3TK9yz8AAAAJ".
#' @param gs_pub_id Character string specifying the Google Scholar publication ID. Default is "0EnyYjriUFMC".
#' @param force Logical. Whether to force the request for publication info. Default is FALSE.
#' @param interval Numeric. Time interval in seconds to wait between requests. Default is 7.
#' @importFrom magrittr %>%
#' @importFrom stringr str_extract str_replace str_detect str_split
#' @importFrom purrr map
#' @importFrom rentrez entrez_search
#' @return A character string containing the PubMed ID (PMID) if found, or NA otherwise.
#'         Warnings are issued if the PubMed ID or the publication is not found.
#'
#' @examples
#' \dontrun{
#' convert_gs_pub_id2pmid(gs_user_id = "some_user_id", gs_pub_id = "some_pub_id")
#' }
#'
#' @export

convert_gs_pub_id2pmid <-
  function(gs_user_id = "3TK9yz8AAAAJ",
           gs_pub_id = "0EnyYjriUFMC",
           force = FALSE,
           interval = 7) {
    result <-
      request_publication_info(
        user_id = gs_user_id,
        pub_id = gs_pub_id,
        force = force,
        interval = interval
      )

    if (!is.na(result$publication_title)) {
      result <-
        rentrez::entrez_search(db = "pubmed", term = result$publication_title)

      if (result$count > 0) {
        # Retrieve the first PubMed ID (PMID)
        pmid <- result$ids[1]
        return(pmid)
      } else {
        warning("PubMed ID not found")
        return(NA)
      }
    } else{
      warning("Publication not found on Google Scholar")
      return(NA)
    }
  }



#' @title request_pubmed_publication_info
#' @description Request publication information from pubmed
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param pmid pmid of paper
#' @importFrom magrittr %>%
#' @importFrom stringr str_extract str_replace str_detect str_split
#' @importFrom purrr map
#' @return Information of publication
#' @export

request_pubmed_publication_info <-
  function(pmid = "35902589") {
    url <-
      paste0("https://pubmed.ncbi.nlm.nih.gov/",
             pmid)

    data <-
      rvest::read_html(x = url)

    journal <-
      data %>%
      html_nodes("#full-view-journal-trigger") %>%
      html_text2()

    article_source <-
      data %>%
      html_nodes(".cit") %>%
      html_text2()

    publication_title <-
      data %>%
      html_nodes(".heading-title") %>%
      html_text2()

    publication_title <-
      publication_title[1]

    authors <-
      data %>%
      html_nodes(".authors") %>%
      html_text2()

    authors <-
      authors[1]

    affiliations <-
      data %>%
      html_nodes("#expanded-authors") %>%
      html_text2() %>%
      stringr::str_split(pattern = "\\\n")

    if (length(affiliations) == 0) {
      affiliations <- NA
    } else{
      affiliations <-
        affiliations[[1]]

      affiliations <-
        affiliations[!stringr::str_detect(affiliations, "Affiliation")]
      affiliations <-
        affiliations[!stringr::str_detect(affiliations, "Contributed")]
      affiliations <-
        affiliations[!stringr::str_detect(affiliations, "equally")]

      affiliations <-
        affiliations %>%
        stringr::str_replace("^[0-9]{1,3}", "") %>%
        stringr::str_replace("\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,4}\\b", "") %>%
        stringr::str_replace("Electronic address", "") %>%
        stringr::str_replace_all("\\.", "") %>%
        stringr::str_replace_all("\\:", "") %>%
        stringr::str_replace_all("\\;", ",") %>%
        stringr::str_trim(side = "both")

    }

    identifiers <-
      data %>%
      html_nodes("#full-view-identifiers") %>%
      html_text2() %>%
      stringr::str_split(pattern = "\\\n")

    identifiers <-
      identifiers[[1]]

    abstrct <-
      data %>%
      html_nodes("#abstract") %>%
      html_text2() %>%
      stringr::str_split(pattern = "\\\n")

    abstrct <-
      abstrct[[1]]

    abstrct <-
      abstrct[abstrct != "Abstract"]

    abstrct <-
      abstrct[abstrct != ""]

    abstrct <- abstrct[1]

    list(
      publication_title = publication_title,
      journal = journal,
      authors = authors,
      affiliations = affiliations,
      identifiers = identifiers,
      abstrct = abstrct
    )
  }

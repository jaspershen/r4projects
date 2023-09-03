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

    if(length(affiliations) == 0){
      affiliations <- NA
    }else{
      affiliations <-
        affiliations[[1]]

      affiliations <-
        affiliations[affiliations != "Affiliations"]
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

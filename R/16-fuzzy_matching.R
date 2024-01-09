#' Fuzzy Match Between Two Vectors
#'
#' This function performs a fuzzy match between two character vectors using various distance methods.
#' It calculates the similarity scores based on the selected method and returns a data frame of matches
#' with their corresponding similarity scores. Optionally, the results can be sorted by the similarity scores.
#'
#' @param x A character vector.
#' @param y Another character vector to be matched against `x`.
#' @param method A character string specifying the method to be used for calculating string distance.
#'        Valid options are "osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", and "soundex".
#' @param useBytes Logical; if `TRUE`, the matching is done byte-by-byte rather than character-by-character.
#' @param weight A named numeric vector of weights to be used for the "osa", "lv", and "dl" methods.
#' @param q An integer used for the "qgram" method.
#' @param p A penalty parameter used in the "jw" method.
#' @param bt A breaking tie parameter used in some methods.
#' @param nthread The number of threads to use for computation.
#' @param sorted_by_score Logical; if `TRUE`, the results are sorted by descending similarity scores.
#'
#' @return A `data.frame` containing the original elements of `x` and `y`, their similarity scores, and index positions.
#'         If `sorted_by_score` is `TRUE`, the data frame is sorted by the similarity scores in descending order.
#'
#' @examples
#' x <- "apple"
#' y <- c("ape", "bandana", "berry")
#' fuzzy_match(x, y, method = "jaccard")
#'
#' @importFrom stringdist stringdistmatrix
#' @importFrom dplyr arrange desc
#' @export

fuzzy_match <-
  function(x,
           y,
           method = c("osa",
                      "lv",
                      "dl",
                      "hamming",
                      "lcs",
                      "qgram",
                      "cosine",
                      "jaccard",
                      "jw",
                      "soundex"),
           useBytes = FALSE,
           weight = c(d = 1,
                      i = 1,
                      s = 1,
                      t = 1),
           q = 1,
           p = 0,
           bt = 0,
           nthread = getOption("sd_num_thread"),
           sorted_by_score = TRUE) {
    method <-
      match.arg(method)
    # Calculate distances
    distances <-
      stringdist::stringdistmatrix(
        x,
        y,
        method = method,
        useBytes = useBytes,
        weight = weight,
        q = q,
        p = p,
        bt = bt,
        nthread = nthread
      )

    distances <-
      as.numeric(distances[1, ])

    # Convert distances to similarity scores (optional, depends on your needs)
    similarity_scores <-
      1 - (distances / max(distances))

    result <-
      data.frame(x,
                 y,
                 similarity_scores,
                 index = 1:length(y))

    if (sorted_by_score) {
      result <-
        result %>%
        dplyr::arrange(dplyr::desc(similarity_scores))
    }

    return(result)
  }

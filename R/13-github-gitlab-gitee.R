#' @title require_github_contribution_activity
#' @description Request the contribution activity of github
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param user_name GitHub user name
#' @importFrom magrittr %>%
#' @importFrom rvest read_html html_node html_text html_text2 html_elements
#' @importFrom rvest html_element html_attr html_table html_nodes
#' @importFrom tibble as_tibble
#' @importFrom xml2 read_html write_html
#' @return Profile.
#' @export

require_github_contribution_activity <-
  function(user_name = "jaspershen") {
    url <- paste0("https://github.com/", user_name)
    page_data <-
      rvest::read_html(url)

    # Extract the contribution data
    contribution_data <-
      page_data %>%
      html_nodes(".js-calendar-graph-table") %>%
      html_table() %>%
      `[[`(1) %>%
      as.data.frame()

    row_names <-
      contribution_data$X1 %>%
      stringr::str_split("\\\n") %>%
      lapply(function(x) {
        x[1]
      }) %>%
      unlist()

    rownames(contribution_data) <-
      row_names

    contribution_data <-
      contribution_data[, -1]

    col_names <-
      contribution_data[1, ] %>%
      as.character() %>%
      stringr::str_split("\\\n") %>%
      lapply(function(x) {
        x[1]
      }) %>%
      unlist()

    contribution_data <-
      contribution_data[-1,]

    contribution_value <-
      contribution_data %>%
      purrr::map(function(x) {
        x %>%
          stringr::str_extract("^[0-9]{1,4}") %>%
          as.numeric()
      }) %>%
      do.call(cbind, .) %>%
      as.data.frame()

    contribution_date <-
      contribution_data %>%
      purrr::map(function(x) {
        x %>%
          stringr::str_split(",") %>%
          lapply(function(x) {
            tail(x, 2) %>%
              stringr::str_trim(side = "both") %>%
              paste(collapse = ", ")
          }) %>%
          unlist()
      }) %>%
      do.call(cbind, .) %>%
      as.data.frame()

    rownames(contribution_value) <-
      rownames(contribution_date) <-
      row_names[-1] %>%
      stringr::str_extract("^[A-Za-z]{3}")

    contribution_value[is.na(contribution_value)] <-
      0

    contribution_value <-
      contribution_value %>%
      tibble::rownames_to_column(var = "week") %>%
      tidyr::pivot_longer(cols = -week,
                          names_to = "col_names",
                          values_to = "value")

    contribution_date <-
      contribution_date %>%
      tibble::rownames_to_column(var = "week") %>%
      tidyr::pivot_longer(cols = -week,
                          names_to = "col_names",
                          values_to = "date") %>%
      dplyr::mutate(month = stringr::str_extract(date, "^[A-Za-z]{3}"))


    contribution_data <-
      contribution_date %>%
      dplyr::left_join(contribution_value,
                       by = c("week", "col_names")) %>%
      dplyr::mutate(week = factor(week, levels = c(
        "Sun", "Sat", "Fri", "Thu", "Wed", "Tue", "Mon"
      ))) %>%
      dplyr::mutate(col_names =
                      factor(col_names,
                             levels = stringr::str_sort(unique(col_names), numeric = TRUE)))

    contribution_data <-
      contribution_data %>%
      dplyr::mutate(year = stringr::str_extract(date, "[0-9]{4}") %>%
                      as.numeric())

    temp_data <-
      contribution_data %>%
      dplyr::mutate(temp = paste(year, month, sep = "_"))

    col_values <-
      temp_data %>%
      split(temp_data$temp) %>%
      purrr::map(function(x) {
        x <-
          x[, c("col_names", "month")] %>%
          dplyr::mutate(col_names = as.character(col_names)) %>%
          dplyr::distinct(col_names, .keep_all = TRUE)
        x$month[-1] <- ""
        value <-
          x$month
        names(value) <- as.character(x$col_names)
        value
      })

    names(col_values) <- NULL

    plot <-
      contribution_data %>%
      ggplot(aes(col_names, week)) +
      geom_tile(aes(fill = value),
                color = "white",
                linewidth = 1) +
      labs(x = "", y = "") +
      scale_y_discrete(
        labels = c(
          "Tue" = "",
          "Mon" = "Mon",
          "Wed" = "Wed",
          "Thu" = "",
          "Fri" = "Fri",
          "Sat" = "",
          "Sun" = "Sun"
        )
      ) +
      scale_x_discrete(labels = unlist(col_values),
                       position = 'top') +
      scale_fill_gradientn(colours = c("#EDF8E9", "#BAE4B3", "#74C476", "#238B45")) +
      theme(
        legend.position = "bottom",
        plot.background = element_rect(fill = "transparent"),
        panel.background = element_blank(),
        axis.ticks = element_blank()
      )

    list(contribution_data = contribution_data,
         plot = plot)

  }

#' @title require_github_profile
#' @description Request the profile of GitHub.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param user_name GitHub user name
#' @importFrom magrittr %>%
#' @importFrom rvest read_html html_node html_text html_text2 html_elements
#' @importFrom rvest html_element html_attr html_table html_nodes
#' @importFrom tibble as_tibble
#' @importFrom xml2 read_html write_html
#' @return Profile.
#' @export

require_github_profile <-
  function(user_name = "jaspershen") {
    url <- paste0("https://github.com/", user_name)
    page_data <-
      rvest::read_html(url)

    # Extract the user's name
    name <-
      page_data %>%
      html_nodes(".p-name") %>%
      html_text() %>%
      trimws()

    # Extract the user's bio
    user_bio <-
      page_data %>%
      html_nodes(".p-note") %>%
      html_text() %>%
      trimws()

    # Extract the user's affiliation
    user_affiliation <-
      page_data %>%
      html_nodes(".p-org") %>%
      html_text2()

    # Extract the user's location
    user_location <-
      page_data %>%
      html_nodes(".p-label") %>%
      html_text2() %>%
      trimws() %>%
      `[`(1)

    # Extract the user's email address (if available)
    user_email <-
      page_data %>%
      html_nodes("a[href^='mailto:']") %>%
      html_attr("href") %>%
      str_extract("[^:]+:[^@]+@[^.]+\\..+") %>%
      stringr::str_replace("mailto\\:", "")

    # Extract numbers of followers and following
    user_follow <-
      page_data %>%
      html_nodes(".no-underline.no-wrap") %>%
      html_text2()

    list(
      name = name,
      user_bio = user_bio,
      user_affiliation = user_affiliation,
      user_location = user_location,
      user_email = user_email,
      user_follow = user_follow
    )

  }






#' @title require_package_info
#' @description Request R package information from GitHub, GitLab or Gitee.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param repo_name R package repo name, default is "jaspershen/laggedcor"
#' @param from github, gitlab or gitee.
#' @importFrom magrittr %>%
#' @importFrom rvest read_html html_node html_text html_text2 html_elements
#' @importFrom rvest html_element html_attr html_table html_nodes
#' @importFrom tibble as_tibble
#' @importFrom xml2 read_html write_html
#' @return information
#' @export

require_package_info <-
  function(repo_name = "jaspershen/laggedcor",
           from = c("github", "gitlab", "gitee")) {
    from = match.arg(from)

    ###github
    if (from == "github") {
      url <-
        paste0("https://raw.githubusercontent.com/",
               repo_name,
               "/master/DESCRIPTION")

      x <-
        tryCatch(
          readLines(url),
          error = function(e) {
            NULL
          }
        )

      if (is.null(x)) {
        url <-
          paste0("https://raw.githubusercontent.com/",
                 repo_name,
                 "/main/DESCRIPTION")

        x <-
          tryCatch(
            readLines(url),
            error = function(e) {
              NULL
            }
          )
      }
    }


    ###gitlab
    if (from == "gitlab") {
      url <-
        paste0("https://gitlab.com/",
               repo_name,
               "/raw/master/DESCRIPTION")
      x <-
        tryCatch(
          readLines(url),
          error = function(e) {
            NULL
          }
        )

      if (is.null(x)) {
        url <-
          paste0("https://gitlab.com/",
                 repo_name,
                 "/raw/main/DESCRIPTION")
      }

      x <-
        tryCatch(
          readLines(url),
          error = function(e) {
            NULL
          }
        )
    }

    ###giteee
    if (from == "gitee") {
      url <-
        paste0("https://gitee.com/",
               repo_name,
               "/raw/master/DESCRIPTION")
      x <-
        tryCatch(
          readLines(url),
          error = function(e) {
            NULL
          }
        )

      if (is.null(x)) {
        url <-
          paste0("https://gitee.com/",
                 repo_name,
                 "/raw/main/DESCRIPTION")
      }

      x <-
        tryCatch(
          readLines(url),
          error = function(e) {
            NULL
          }
        )
    }

    if (is.null(x)) {
      stop("can't read information from ", url)
    }

    package_name <-
      x[stringr::str_detect(x, "Package")] %>%
      stringr::str_replace("Package: ", "")

    title <-
      x[stringr::str_detect(x, "Title:")] %>%
      stringr::str_replace("Title: ", "")

    version <-
      x[stringr::str_detect(x, "Version:")] %>%
      stringr::str_replace("Version: ", "")

    list(
      package_name = package_name,
      title = title,
      version = version,
      all_info = x
    )

  }

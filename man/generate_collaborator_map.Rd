% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/11-wowchemy.R
\name{generate_collaborator_map}
\alias{generate_collaborator_map}
\title{generate_collaborator_map}
\usage{
generate_collaborator_map(
  gs_user_id = "3TK9yz8AAAAJ",
  force = FALSE,
  interval = 7,
  map_color = "#76b5c5",
  point_color = "#873e23"
)
}
\arguments{
\item{gs_user_id}{user google scholar ID}

\item{force}{Force read webpage?}

\item{interval}{If there is old data on the local machine,
do you want to use it and the interval (day)?}

\item{map_color}{color of map}

\item{point_color}{color of point}
}
\value{
Profile.
}
\description{
Generate map of collaborators
}
\author{
Xiaotao Shen
\email{shenxt1990@outlook.com}
}

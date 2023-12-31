% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/15-color-palette.R
\name{color_palette}
\alias{color_palette}
\title{Generate Color Palettes}
\usage{
color_palette(palette_number)
}
\arguments{
\item{palette_number}{An integer specifying the palette number between 2 and 9.}
}
\value{
A list of color palettes. The selected palette is returned based on the
        specified palette_number.
}
\description{
This function generates color palettes for use in data visualization.
}
\examples{
# Generate a color palette with 5 colors
my_palette <- color_palette(5)

# Display the first color palette
my_palette[[1]]

# Display the second color palette
my_palette[[2]]

}

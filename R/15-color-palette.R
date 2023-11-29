#' Generate Color Palettes
#'
#' This function generates color palettes for use in data visualization.
#'
#' @param palette_number An integer specifying the palette number between 2 and 9.
#'
#' @return A list of color palettes. The selected palette is returned based on the
#'         specified palette_number.
#'
#' @examples
#' # Generate a color palette with 5 colors
#' my_palette <- color_palette(5)
#'
#' # Display the first color palette
#' my_palette[[1]]
#'
#' # Display the second color palette
#' my_palette[[2]]
#'
#' @export

color_palette <-
  function(palette_number) {
    if(missing(palette_number)){
     stop("You must specify a palette number between 2 and 9")
    }
    color_palette5 <-
      list(
        c("#2C3E50", "#E74C3C", "#ECF0F1", "#3498DB", "#2980B9"),
        c(
          "#E67E22",
          "#F1C40F",
          "#F3FFE2",
          "#ACF0F2",
          "#1695A3",
          "#225378"
        ),
        c("#002F2F", "#046380", "#EFECCA", "#A7A37E", "#E6E2AF"),
        c("#E28B00", "#B64926", "#FFB03B", "#FFD34E", "#468966"),
        c("#FF6138", "#FFFF9D", "#BEEB9F", "#79BD8F", "#00A388"),
        c('#D9ECF2', '#F56A79', '#FF414D', '#1AA687', '#002D40'),
        c('#5AA7A7', '#96D7C6', '#8AC94A', '#E2D368', '#6C8CBF'),
        c('#147C72', '#299D90', '#30C3B1', '#8FE4DC', '#B4F0E8'),
        c('#ED4557', '#B8010B', '#D9569E', '#381B2A', '#FCBC53'),
        c('#93C6BD', '#C1E1DA', '#DBE1EA', '#F2C29F', '#E9A475'),
        c('#86CBCD', '#A8DFE0', '#F9E2AE', '#FBC78D', '#A6D676'),
        c("#8969A5", "#C48ADE", "#B1BEEA", "#8FC4E9", "#8095CF"),
        c("#FD465D", "#FEB396", "#FECCBF", "#AED4D5", "#F8CC88"),
        c("#86E3CE", "#DOE6A5", "#FFDD95", "#FD9385", "#CCABDA"),
        c("#348899", "#F2EBC7", "#979C9C", "#343642", "#FE8B54"),
        c("#F95759", "#FDA099", "#FFFFFF", "#D9F3CB", "#8AC2B0"),
        c("#0A3D64", "#DDB0A7", "#EAAD5A", "#A8B293", "#DF8053"),
        c("#DC3971", "#EC719F", "#F3B3CC", "#ABE5E8", "#34ADAE"),
        c("#FEA88C", "#FFA3A6", "#F583B3", "#CD69A7", "#ED7179"),
        c("#FCF5EF", "#FEA735", "#FE7235", "#00C3FF", "#0077FF"),
        c("#2B3A42", "#95AB63", "#BDD684", "#E2F0D6", "#F6FFE0"),
        c("#E7C54F", "#E5663F", "#C7323F", "#67493A", "#009DAE"),
        c("#264D59", "#43978D", "#F9E07F", "#F9AD6A", "#D46C4E"),
        c("#FCB4C5", "#FB7A8F", "#AAB6FD", "#6096FC", "#112B8C"),
        c("#CAD4BD", "#ACC352", "#8ED4C5", "#FFF16F", "#FFA68D"),
        c("#8F797E", "#FFC2B5", "#FFE3CC", "#646C8F", "#DCC3A1")
      )
    if(palette_number == 5){
      return(color_palette5)
    }
  }

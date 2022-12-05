#' CR´s Karoliska Institutet ggplot theme
#'
#' A minimal theme with axis lines. Available in white, black, grey and offwhite. Uses KIs official font DM Sans (Which you should install separately at https://fonts.google.com/specimen/DM+Sans)
#'
#' @param horizontal \code{logical} Horizontal axis lines (\code{TRUE/FALSE}).
#' @param bgcolor \code{character} Default is \code{white}; white bakground. Options are \code{c("white", "grey", "offwhite", "black")}
#' @param  base_size \code{integer} Adjusts all fonts and spacing etc. Default is \code{8}.
#' @param base_family \code{character} A character vector specifying font. For this KI theme default is \code{"DM Sans"}, but can be substituted.
#' Most standard fonts should installed on your system should work. Recommended are:
#'
#' \itemize{"DM Sans"} (Default)
#' \itemize{"Adobe Caslon Pro} (Must be separately installed, and may be expensive, but oh so beautiful.)
#' \itemize{"Helvetica"}
#' \itemize{"Optima"}
#' \itemize{"CMU Serif"} (Must be installed from web, but looks darn good.)
#'
#' @return An object of class \code{\link[ggplot2]{theme}()}.
#'
#' @export
#' @family themes crgg
#'
#'
#  Create a KI theme with standard KI font
theme_ki_standard <- function(base_size = 8, base_family = "DM Sans",
                              horizontal = TRUE, bgcolor = "white"){
  ret  <- theme_crgg_standard(base_size = base_size, base_family = base_family,
                              horizontal = horizontal, bgcolor = bgcolor)

}

#' CR´s Karoliska Institutet ggplot theme
#'
#' A minimal theme with axis lines. Available in white, black, grey and offwhite. Uses KIs official font DM Sans (Which you should install separately at https://fonts.google.com/specimen/DM+Sans)
#'
#' @param horizontal \code{logical} Horizontal axis lines (\code{TRUE/FALSE}).
#' @param bgcolor \code{character} Default is \code{white}; white bakground. Options are \code{c("white", "grey", "offwhite", "black")}
#' @param base_size \code{integer} Adjusts all fonts and spacing etc. Default is \code{8}.
#' @param base_family \code{character} A character vector specifying font. For this DS theme default is \code{"Arial"}, but can be substituted.
#' Most standard fonts should installed on your system should work. Recommended are:
#'
#' \itemize{"Arial"} (Default sans serif font)
#' \itemize{"Times New Roman"} (Default serif font.)
#'
#' @return An object of class \code{\link[ggplot2]{theme}()}.
#'
#' @export
#' @family themes crgg
#'
#'
#  Create a KI theme with standard KI font
theme_ds_standard <- function(base_size = 8, base_family = "Arial",
                              horizontal = TRUE, bgcolor = "white"){
  ret  <- theme_crgg_standard(base_size = base_size, base_family = base_family,
                              horizontal = horizontal, bgcolor = bgcolor)

}

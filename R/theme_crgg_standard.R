#' CR´s ggplot standard theme
#'
#' A minimal theme with axis lines. Available in white, black, grey and offwhite.
#'
#' @inheritParams ggplot2::theme_classic
#' @param horizontal \code{logical} Horizontal axis lines (\code{TRUE/FALSE}).
#' @param bgcolor \code{character} Default is \code{white}; white bakground. Options are \code{c("white", "grey", "offwhite", "black")}
#' @param  base_size \code{integer} Adjusts all fonts and spacing etc. Default is \code{8}.
#' @param base_family \code{character} A character vector specifying font. Most standard fonts should installed on your system should work. Recommended are:
#'
#' \itemize{"Palatino"} (Default)
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
theme_crgg_standard <- function(base_size = 8, base_family = "Palatino",
                               horizontal = FALSE, bgcolor = "white") {
  ## From measurements
  ## Ticks = 1 / 32 in, with margin about 1.5 / 32
  ## Title = 3 / 32 in (6 pt)
  ## Legend Labels = 2.5 / 32 in (5pt)
  ## Axis Labels = 2
  ## Axis Titles and other text ~ 2
  ## Margins: Top / Bottom = 6 / 32, sides = 5 / 32
  ret <-
    theme_foundation(base_size = base_size, base_family = base_family) +
    theme(line = element_line(colour = "black"),
          rect = element_rect(fill = "white", colour = NA,
                              linetype = 1),
          text = element_text(colour = "black", family = base_family),
          ## Axis
          axis.line = element_line(),
          axis.text = element_text(size = rel(.8)),
          axis.text.x.top = element_text(vjust = 0, margin = margin(b = base_size, unit = "pt")),
          axis.ticks = element_line(),
          axis.title = element_text(size = rel(1)),
          axis.title.x = element_text(margin = margin(t = base_size, unit = "pt")),
          axis.title.y = element_text(angle = 90, margin = margin(r = base_size, unit = "pt")),
          axis.ticks.length = unit( base_size * 0.5, "points"),
          ## Legend
          legend.background = element_rect(linetype = 0),
          legend.spacing = unit(base_size * 1.5, "points"),
          legend.key = element_rect(linetype = 0, size = 0, color = "white"),
          legend.key.size = unit(1, "lines"),
          legend.key.height = NULL,
          legend.key.width = NULL,
          legend.text = element_text(size = rel(.7)),
          legend.text.align = NULL,
          legend.title = element_text(size = rel(1),  vjust = .8 ),
          legend.title.align = NULL,
          legend.position = "bottom",
          legend.direction = NULL,
          legend.justification = "center",
          ## Panel
          panel.background = element_rect(linetype = 0),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.spacing = unit(2, "lines"),
          ## Facet strip (headers)
          strip.background = element_blank(),
          strip.text = element_text(size = rel(1)),
          strip.text.x = element_text(),
          strip.text.y = element_text(angle = -90),
          ## Global plot options
          plot.background = element_blank(),
          plot.title = element_text(size = rel(1.5),
                                    hjust = 0, vjust = ,
                                    face = "bold",
                                    lineheight = .1,
                                    margin = margin(b = base_size * .5, unit = "pt")),
          plot.tag = element_text(face = "bold", size = base_size * 2),
          plot.subtitle = element_text(vjust = 0,
                                       hjust = 0,
                                       margin = margin(b = base_size)),
          plot.margin = margin(l = base_size, t = base_size, r = base_size * 3, b = base_size, unit = "pt"),
          complete = TRUE)
  # Grey theme
  if (bgcolor == "grey") {
    ret <- ret + theme(plot.background = element_rect(fill = "lightgrey"),
                       panel.background = element_rect(fill = "lightgrey"),
                       legend.background = element_rect(fill = "lightgrey"),
                       legend.key = element_rect(linetype = 0, size = 0, color = "lightgrey"))
  }
  # Offwhite theme
  if (bgcolor == "offwhite") {
    ret <- ret + theme(plot.background = element_rect(fill = "#FAF9F6"),
                       panel.background = element_rect(fill = "#FAF9F6"),
                       legend.background = element_rect(fill = "#FAF9F6"),
                       legend.key = element_rect(linetype = 0, size = 0, color = "#FAF9F6"))
  }
  # Black theme
  if (bgcolor == "black") {
    ret <- ret + theme(text = element_text(colour = "white"),
                       axis.line = element_line(color = "white"),
                       plot.background = element_rect(fill = "black"),
                       panel.background = element_rect(fill = "black"),
                       legend.background = element_rect(fill = "black"),
                       axis.ticks = element_line(colour = "white"))
  }
  # Horizontal line parameters
  if (horizontal) {
    if(bgcolor %in% c("offwhite", "white")){
      line_thickness = .4
      line_color = "lightgrey"
    } else {
      line_thickness = .25
      line_color = "white"
    }
    ret <- ret + theme(panel.grid.major.x = element_blank(),
                       panel.grid.major.y = element_line(size = line_thickness, color = line_color),
                       panel.grid.minor.y = element_line(size = line_thickness, color = line_color))
  } else {
    ret <- ret + theme(panel.grid.major.y = element_blank())
  }
  ret
}


#' CR´s Karoliska Institutet ggplot theme
#'
#' A minimal theme with axis lines. Available in white, black, grey and offwhite. Uses KIs official font DM Sans (Which you should install separately at https://fonts.google.com/specimen/DM+Sans)
#'
#' @inheritParams ggplot2::theme_classic
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

#' CR´s ggplot minimal theme
#'
#' A white, minimal theme with serif Palatino font.
#'
#' @inheritParams ggplot2::theme_classic
#' @param horizontal \code{logical} Horizontal axis lines?
#' @param black \code{logical} If \code{TRUE}, use black background, else
#'    use white background. Default is \code{FALSE}.
#'
#' @return An object of class \code{\link[ggplot2]{theme}()}.
#'
#' @export
#' @family themes crgg
#'
#'
theme_crgg_minimal <- function(base_size = 8, base_family = "Palatino",
                            horizontal = FALSE, black = FALSE) {
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
          axis.line = element_blank(),
          axis.text = element_text(size = rel(.8)),
          #axis.text.x = element_text(vjust = 0, margin = margin(t = base_size, unit = "pt")),
          axis.text.x.top = element_text(vjust = 0, margin = margin(b = base_size, unit = "pt")),
          # axis.text.y = element_text(hjust = 0,
          #                            margin = margin(r = base_size,
          #                                            unit = "pt")),
          axis.ticks = element_line(),
          #axis.ticks.y = element_blank(),
          axis.title = element_text(size = rel(1)),
          axis.title.x = element_text(margin = margin(t = base_size, unit = "pt")),
          axis.title.y = element_text(angle = 90, margin = margin(r = base_size, unit = "pt")),
          # axis.ticks.length = unit( -1/32, "in"),
          axis.ticks.length = unit( base_size * 0.5, "points"),
          legend.background = element_rect(linetype = 0),
          legend.spacing = unit(base_size * 1.5, "points"),
          legend.key = element_rect(linetype = 0, size = 0),
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
          ## legend.box = element_rect(fill = palette_economist['bgdk'],
          ## colour=NA, linetype=0),
          ## Economist only uses vertical lines
          panel.background = element_rect(linetype = 0),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.spacing = unit(2, "lines"),
          strip.background = element_blank(),
          strip.text = element_text(size = rel(1)),
          strip.text.x = element_text(),
          strip.text.y = element_text(angle = -90),
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
          plot.margin = unit(c(5,10,10,5)*2, "points"),
          complete = TRUE)
  if (horizontal) {
    ret <- ret + theme(panel.grid.major.x = element_blank())
  } else {
    ret <- ret + theme(panel.grid.major.y = element_blank())
  }
  if (black) {
    ret <- ret + theme(text = element_text(colour = "white"),
                       plot.background = element_rect(fill = "black"),
                       panel.background = element_rect(fill = "black"),
                       legend.background = element_rect(fill = "black"),
                       axis.ticks = element_line(colour = "white"))
  }
  ret
}

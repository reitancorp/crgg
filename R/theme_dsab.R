#' DSAB minimal axis ggplot2 theme
#'
#' A clean theme that keeps only axis lines, axis text, and axis titles.
#' Uses Arial by default. Removes panel grid, background, and other
#' non-essential elements. Conforms to the DSAB graphical profile.
#'
#' @param base_size \code{numeric} Base font size. Default is \code{11}.
#' @param base_family \code{character} Font family. Default is \code{"Arial"}.
#' @param axis_line_colour \code{character} Colour for axis lines. Default is \code{"black"}.
#' @param axis_line_width \code{numeric} Line width for axis lines. Default is \code{0.5}.
#' @param legend \code{logical} Show legend? Default is \code{FALSE}.
#'
#' @return An object of class \code{\link[ggplot2]{theme}()}.
#'
#' @export
#' @family themes crgg
#'
theme_ds_standard <- function(base_size = 11,
                              base_family = "Arial",
                              axis_line_colour = "black",
                              axis_line_width = 0.5,
                              legend = FALSE) {
    th <- ggplot2::theme_void(base_size = base_size, base_family = base_family) +
        ggplot2::theme(
            # Axis lines
            axis.line = ggplot2::element_line(
                colour = axis_line_colour,
                linewidth = axis_line_width,
                lineend = "round"
            ),
            # Axis text & titles
            axis.text = ggplot2::element_text(colour = "black", size = base_size * 0.9),
            axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = base_size * 0.25)),
            axis.text.y = ggplot2::element_text(
                margin = ggplot2::margin(r = base_size * 0.25),
                hjust = 1
            ),
            axis.title = ggplot2::element_text(
                colour = "black", size = base_size, face = "plain"
            ),
            axis.ticks = ggplot2::element_blank(),
            # Plot title
            plot.title = ggplot2::element_text(
                margin = ggplot2::margin(b = base_size * 0.6)
            ),
            # Margins
            plot.margin = ggplot2::margin(5, 5, 5, 5),
            # Panel spacing
            panel.spacing = ggplot2::unit(0.5, "lines")
        )

    if (!legend) {
        th <- th + ggplot2::theme(legend.position = "none")
    }

    attr(th, "complete") <- TRUE
    th
}

#' Set DSAB theme as global default
#'
#' Convenience function that calls \code{\link[ggplot2]{theme_set}()} with
#' \code{\link{theme_ds_standard}()}.
#'
#' @param ... Arguments passed to \code{\link{theme_ds_standard}()}.
#' @return Invisibly returns \code{TRUE}.
#' @export
use_ds_theme <- function(...) {
    ggplot2::theme_set(theme_ds_standard(...))
    invisible(TRUE)
}

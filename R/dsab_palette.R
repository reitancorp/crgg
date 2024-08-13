#' DSAB official color palettes
#'
#' @param  order The ordering of colors. Default is \code{"hue"}. Options are \code{c("color", "hue", "original")}.
#' @param name The palette used. Default is \code{"ds_dark"}. Options are \code{c("ds_standard", "ds_dark", "ds_light")}
#'


ds_palettes = function(palettename = "ds_dark", n, type = c("discrete", "continuous"), order = c("hue", "color", "original")) {
  all_palettes = list(
    ds_standard = c("#017F71", "#000000", "#FFFFFF"),
    ds_dark = c("#017F71", "#A89C94", "#F0C146",'#00425C',  '#C4043F'),
    ds_light = c("#D8D1CA", "#F7E6AD", "#C8D6DF", '#BDD5CC')
  )
  palette = all_palettes[[palettename]]
  if (missing(n)) {
    n = length(palette)
  }
  if (order == "hue" & palettename != "ds_standard") {
    palette = palette[order(c(1,5,2,3,4))]
  }
  if (order == "color" & palettename != "ds_standard") {
    palette = palette[order(c(1,5,3,4,2))]
  }
  if (order == "original" & palettename != "ds_standard") {
    palette = palette[order(c(1,2,3,4,5))]
  }
  type = match.arg(type)
  out = switch(type,
               continuous = grDevices::colorRampPalette(palette)(n),
               discrete = palette[1:n]
  )
  structure(out, palettename = palettename, class = "palette")
}



#' @export
scale_colour_ds_d = function(palettename = "ds_dark", order = "original") {
  ggplot2::scale_colour_manual(values = ds_palettes(palettename,
                                                    type = "discrete",
                                                    order = order))
}
#' @export
scale_fill_ds_d = function(palettename = "ds_dark", order = "original") {
  ggplot2::scale_fill_manual(values = ds_palettes(palettename,
                                                  type = "discrete",
                                                  order = order))
}
#' @export
scale_colour_ds_c = function(palettename = "ds_dark", order = "original") {
  ggplot2::scale_colour_gradientn(colours = ds_palettes(palettename = palettename,
                                                        type = "continuous",
                                                        order = order))
}
#' @export
scale_fill_ds_c = function(palettename = "ds_dark", order = "original") {
  ggplot2::scale_fill_gradient(colours = ds_palettes(palettename = palettename,
                                                      type = "continuous",
                                                      order = order))
}


scale_color_ds_d = scale_colour_ds_d
scale_color_ds_c = scale_colour_ds_c


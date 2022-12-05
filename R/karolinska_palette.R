#' Kaolinska Institutets official color palettes
#'
#' The colour palettes can be found on KI´s webpages https://medarbetare.ki.se/farger-i-kis-grafiska-profil
#'
#' @param  order The ordering of colors. Default is \code{"gradient"}. Options are \code{c("gradient", "hue", "original")}.
#' @param name The palette used. Default is \code{"ki_primary"}. Options are \code{c("ki_primary", "ki_function_1", "ki_function_2", "ki_function_3")}
#'


ki_palettes = function(palettename = "ki_primary", n,type = c("discrete", "continuous"), order = c("gradient", "hue", "color", "original")) {
  all_palettes = list(
    ki_primary = c("#4F0433", "#FF876F", "#FEEEEB", "#EDF4F4", "#870052"),
    ki_function_1 = c("#B84145", "#FF876F", "#FFDDD6",'#C7ECDC',  '#54B986', '#094334'),
    ki_function_2 = c("#002C34", "#4DB5BC", "#CCEBED", '#FFE7C2', '#FFC66D', '#F59A00'),
    ki_function_3 = c("#000000", "#666666", "#F1F1F1",'#EDDBE4', '#870052', '#4F0433')
  )
  palette = all_palettes[[palettename]]
  if (missing(n)) {
    n = length(palette)
  }
  if (order == "hue" & palettename != "ki_primary") {
    palette = palette[order(c(1,3,5,6,4,2))]
  }
  if (order == "color" & palettename != "ki_primary") {
    palette = palette[order(c(1,2,3,6,5,4))]
  }
  if (order == "original" & palettename != "ki_primary") {
    palette = palette[order(c(1,2,3,6,5,4))]
  }
  type = match.arg(type)
  out = switch(type,
               continuous = grDevices::colorRampPalette(palette)(n),
               discrete = palette[1:n]
  )
  structure(out, palettename = palettename, class = "palette")
}



#' @export
scale_colour_ki_d = function(palettename = "ki_primary", order = "gradient") {
  ggplot2::scale_colour_manual(values = ki_palettes(palettename,
                                                    type = "discrete",
                                                    order = order))
}
#' @export
scale_fill_ki_d = function(palettename = "ki_primary", order = "gradient") {
  ggplot2::scale_fill_manual(values = ki_palettes(palettename,
                                                  type = "discrete",
                                                  order = order))
}
#' @export
scale_colour_ki_c = function(palettename = "ki_primary", order = "gradient") {
  ggplot2::scale_colour_gradientn(colours = ki_palettes(palettename = palettename,
                                                        type = "continuous",
                                                        order = order))
}
#' @export
scale_fill_ki_c = function(palettename = "ki_primary", order = "gradient") {
  ggplot2::scale_fill_gradientn(colours = ki_palettes(palettename = palettename,
                                                      type = "continuous",
                                                      order = order))
}


scale_color_ki_d = scale_colour_ki_d
scale_color_ki_c = scale_colour_ki_c


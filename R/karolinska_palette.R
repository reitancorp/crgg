
# KI colours from https://medarbetare.ki.se/farger-i-kis-grafiska-profil
karolinska_colours = list(
  ki_primary = c("#4F0433", "#FF876F", "#FEEEEB", "#EDF4F4", "#870052"),
  ki_function_1 = c("#B84145", "#FF876F", "#FFDDD6",'#C7ECDC',  '#54B986', '#094334'),
  ki_function_2 = c("#002C34", "#4DB5BC", "#CCEBED", '#FFE7C2', '#FFC66D', '#F59A00'),
  ki_function_3 = c("#000000", "#666666", "#F1F1F1",'#EDDBE4', '#870052', '#4F0433')
)

ki_palettes = function(name = "ki_primary", n, all_palettes = karolinska_colours, type = c("discrete", "continuous"), order = NULL) {
  palette = all_palettes[[name]]
  if (missing(n)) {
    n = length(palette)
  }
  if (order == "hue" & name != "ki_primary") {
    palette = palette[order(c(1,3,5,6,4,2))]
  }
  if (order == "color" & name != "ki_primary") {
    palette = palette[order(c(1,2,3,6,5,4))]
  }
  if (order == "original" & name != "ki_primary") {
    palette = palette[order(c(1,2,3,6,5,4))]
  }
  type = match.arg(type)
  out = switch(type,
               continuous = grDevices::colorRampPalette(palette)(n),
               discrete = palette[1:n]
  )
  structure(out, name = name, class = "palette")
}


scale_colour_ki_d = function(name = "ki_primary", order = "gradient") {
  ggplot2::scale_colour_manual(values = ki_palettes(name,
                                                     type = "discrete",
                                                    order = order))
}

scale_fill_ki_d = function(name = "ki_primary", order = "gradient") {
  ggplot2::scale_fill_manual(values = ki_palettes(name,
                                                   type = "discrete",
                                                  order = order))
}

scale_colour_ki_c = function(name = "ki_primary", order = "gradient") {
  ggplot2::scale_colour_gradientn(colours = ki_palettes(name = name,
                                                         type = "continuous",
                                                        order = order))
}

scale_fill_ki_c = function(name = "ki_primary", order = "gradient") {
  ggplot2::scale_fill_gradientn(colours = ki_palettes(name = name,
                                                       type = "continuous",
                                                      order = order))
}


scale_color_ki_d = scale_colour_ki_d
scale_color_ki_c = scale_colour_ki_c


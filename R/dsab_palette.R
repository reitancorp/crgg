# ---------------------------------------------------------------------------
# DSAB brand palettes (solid + light)
# ---------------------------------------------------------------------------
# Solid colours from DSAB graphical profile
.ds_cols_solid <- c(
  teal      = "#017F71", # Pantone 3282  (R1 G127 B113)
  warm_grey = "#A89C94", # Warm Gray 6C  (R168 G156 B148)
  gold      = "#F0C146", # Pantone 129   (R240 G193 B70)
  navy      = "#00425C", # Pantone 2955  (R0 G66 B92)
  red       = "#C4043F" # Pantone 1945  (R196 G4 B63)
)

# Light / tint variants (matching order)
.ds_cols_light <- c(
  teal      = "#BDD5CC", # Pantone 565 (light teal / green)
  warm_grey = "#D8D1CA", # Warm Gray 2C
  gold      = "#F7E6AD", # Pantone 1205
  navy      = "#C8D6DF", # Pantone 277
  red       = "#E2829F" # 50% tint of Pantone 1945 (computed)
)

#' Get DSAB brand colours
#'
#' Returns a named character vector of DSAB brand colours.
#'
#' @param type \code{character} \code{"solid"} (default) or \code{"light"}.
#' @param as_named \code{logical} If \code{FALSE}, return an unnamed vector
#'   (useful for discrete scales). Default is \code{TRUE}.
#' @return A character vector of hex colour codes.
#' @export
ds_palette <- function(type = c("solid", "light"), as_named = TRUE) {
  type <- match.arg(type)
  pal <- switch(type,
    solid = .ds_cols_solid,
    light = .ds_cols_light
  )
  if (!as_named) unname(pal) else pal
}

#' Discrete palette function for DSAB scales
#'
#' Returns a function that maps \code{n} to a vector of \code{n} colours
#' from the DSAB brand palette.
#'
#' @param type \code{character} \code{"solid"} (default) or \code{"light"}.
#' @return A function taking integer \code{n} and returning \code{n} hex colours.
#' @export
ds_pal <- function(type = c("solid", "light")) {
  type <- match.arg(type)
  cols <- ds_palette(type, as_named = FALSE)
  function(n) {
    if (n > length(cols)) {
      warning(
        "Requested ", n, " colours but palette has only ", length(cols),
        "; colours will be repeated."
      )
      rep(cols, length.out = n)
    } else {
      cols[seq_len(n)]
    }
  }
}

#' DSAB brand colour scale
#'
#' Discrete colour scale using DSAB brand colours.
#'
#' @param type \code{character} \code{"solid"} (default) or \code{"light"}.
#' @param ... Additional arguments passed to
#'   \code{\link[ggplot2]{discrete_scale}()}.
#' @return A ggplot2 scale object.
#' @export
scale_colour_ds <- function(type = c("solid", "light"), ...) {
  type <- match.arg(type)
  ggplot2::discrete_scale(
    aesthetics = "colour",
    scale_name = paste0("ds_", type),
    palette    = ds_pal(type),
    ...
  )
}

#' @rdname scale_colour_ds
#' @export
scale_color_ds <- scale_colour_ds

#' DSAB brand fill scale
#'
#' Discrete fill scale using DSAB brand colours.
#'
#' @param type \code{character} \code{"solid"} (default) or \code{"light"}.
#' @param ... Additional arguments passed to
#'   \code{\link[ggplot2]{discrete_scale}()}.
#' @return A ggplot2 scale object.
#' @export
scale_fill_ds <- function(type = c("solid", "light"), ...) {
  type <- match.arg(type)
  ggplot2::discrete_scale(
    aesthetics = "fill",
    scale_name = paste0("ds_", type),
    palette    = ds_pal(type),
    ...
  )
}

#' Preview DSAB brand palette
#'
#' Draws a tile plot showing the colours and hex values for a DSAB palette.
#'
#' @param type \code{character} \code{"solid"} (default) or \code{"light"}.
#' @param labels \code{logical} Show hex labels on tiles? Default is \code{TRUE}.
#' @return A ggplot object.
#' @export
show_ds_palette <- function(type = c("solid", "light"), labels = TRUE) {
  type <- match.arg(type)
  cols <- ds_palette(type)
  df <- data.frame(colour = names(cols), hex = unname(cols))
  p <- ggplot2::ggplot(df, ggplot2::aes(
    x = .data[["colour"]], y = 1,
    fill = .data[["colour"]]
  )) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_manual(values = cols) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(c(0, 0))) +
    ggplot2::labs(x = NULL, y = NULL, title = paste("DSAB palette:", type)) +
    theme_ds_standard(legend = FALSE) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)
    )
  if (labels) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = .data[["hex"]]),
      colour = "white", size = 3
    )
  }
  p
}

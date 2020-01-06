# Functions for saving plot objects

#' Inteligently save plot objects.
#'
#' \code{smart_save} saves plot objects using different methods depending on
#' class, including support for \code{labeled_plot} objects.
#'
#' This is a generic function with methods defined for different classes,
#' dispatching on the \code{p} argument.
#'
#' @param p Plot object to save.
#' @param filename Path to save plot at.
#' @param override Override labels from \code{labeled_plot} with values from
#'     \code{...}.
#' @param ... Additional parameters passed to plot method.
#' @export
smart_save <- function(p, ...){
  UseMethod("smart_save", p)
}

#' @describeIn smart_save Use \code{ggsave} by default
#' @export
smart_save.default <- function(p, filename, ...){
  params <- list(...)

  # Set vaguely sensible default size
  if (is.null(params$units)){
    params$units <- "in" # for consistency only...
  }
  if (is.null(params$width)){
    params$width <- 7
  }
  if (is.null(params$height)){
    params$height <- 5
  }

  # Remove unecessary params
  params$filename <- NULL
  params[!names(params) %in% names(formals(ggplot2::ggsave))] <- NULL

  do.call(ggplot2::ggsave, c(list(filename=filename, plot=p), params))
}

#' @describeIn smart_save Process labels from \code{labeled_plot}
#' @export
smart_save.labeled_plot <- function(p, filename=NULL, override=FALSE, ...){
  params <- attributes(p)$plotlistr_params
  if (override){
    params <- c(list(...), params)
  }

  if (is.null(filename) & "filename" %in% names(params)){
    filename <- params$filename
  }

  params$filename <- NULL
  class(p) <- class(p)[!class(p) == "labeled_plot"]
  do.call(smart_save, c(list(p=p, filename=filename), params))
}

#' @describeIn smart_save Save \code{multipanelfigure} objects
#' @export
smart_save.multipanelfigure <- function(p, filename, ...){
  if (!requireNamespace("multipanelfigure", quietly = TRUE)) {
    stop("Attempted to save a \"multipanelfigure\" object without the \"multipanelfigure\" package installed. Please install it.",
         call. = FALSE)
  }

  params <- list(...)

  # Remove unecessary params
  params$filename <- NULL
  params[!names(params) %in% names(formals(ggplot2::ggsave))] <- NULL

  do.call(multipanelfigure::save_multi_panel_figure, c(list(figure=p, filename=filename), params))
}

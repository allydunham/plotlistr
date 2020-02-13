# Class of plots with attached labels for saving

#' Attach labels to a plot object
#'
#' \code{labeled_plot} creates a plot object with attributes describing how to
#' save it. If \code{p} is already a \code{labeled_plot} the additional labels
#' will be added to it.
#'
#' This is an S3 class interacting with the \code{smart_save} S3 generic.
#'
#' @param p Plot to wrap
#' @param overwrite Overwrite existing files by default.
#' @param filename Name of file to save to, including extension. Will be added
#'     to the path when saving plot lists. Overrides \code{file_format}
#' @param file_format Format of file to save (e.g. pdf, jpg)
#' @param units,height,width Size of plot, in \code{units}
#' @param ... Other parameters passed to saving method (e.g.
#'     see \code{\link[ggplot2]{ggsave}})
#'
#' @export
labeled_plot <- function(p, ...) {
  UseMethod("labeled_plot", p)
}

#' @describeIn labeled_plot Labels to an unlabled plot
#' @export
labeled_plot.default <- function(p, ...) {
  class(p) <- c("labeled_plot", class(p))

  params <- list(...)

  # Set overwrite to false by default
  if (!"overwrite" %in% names(params)) {
    params$overwrite <- FALSE
  }

  # Remove leading . in file format
  if ("file_format" %in% names(params)) {
    params$file_format <- gsub("^\\.", "", params$file_format)
  }

  attributes(p)$plotlistr_params <- params
  return(p)
}

#' @describeIn labeled_plot Add additional labels to a labeled plot
#' @export
labeled_plot.labeled_plot <- function(p, ...) {
  params <- list(...)

  # Remove leading . in file format
  if ("file_format" %in% names(params)) {
    params$file_format <- gsub("^\\.", "", params$file_format)
  }

  # Only keep onld params that aren't being updated
  old_params <- attributes(p)$plotlistr_params
  old_params <- old_params[!names(old_params) %in% names(params)]

  attributes(p)$plotlistr_params <- c(params, old_params)

  return(p)
}

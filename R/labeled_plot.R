# Class of plots with attached labels for saving

#' Attach labels to a plot object
#'
#' \code{labeled_plot} creates a plot object with attributes describing how to
#' save it
#'
#' This is an S3 class interacting with the \code{smart_save} S3 generic.
#'
#' @param p Plot to wrap
#' @param overwrite Overwrite existing files by default.
#' @param filename Name of file to save to, including extension. Will be added
#'     to the path when saving plot lists. Overides \code{file_format}
#' @param file_format Format of file to save (e.g. pdf, jpg)
#' @param units,height,width Size of plot, in \code{units}
#' @param ... Other parameters passed to saving method (e.g.
#'     see \code{\link[ggplot2]{ggsave}})
#'
#' @export
labeled_plot <- function(p, overwrite = FALSE, ...){
  class(p) <- c("labeled_plot", class(p))

  params <- list(overwrite = overwrite, ...)

  # Remove leading . in file format
  if ("file_format" %in% names(params)){
    params$file_format <- gsub("^\\.", "", params$file_format)
  }

  attributes(p)$plotlistr_params <- params
  return(p)
}

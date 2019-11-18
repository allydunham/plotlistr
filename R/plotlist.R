# Functions for managing lists of plots

#' Save lists of plots.
#'
#' Recursively save plots in a (nested) list, allowing you to generate all
#' plots during an analysis and then plot them afterwards. Use
#' \code{\link{labeled_plot}} to add properties so that plots can be easily
#' saved automatically.
#'
#' This function uses the \code{\link{smart_save}} method to save plots,
#' respecting the attributes of \code{\link{labeled_plot}} objects. Nested
#' levels of the list will be saved in subfolders
#'
#' @param plotlist Potentially nested list of plot objects.
#' @param root Root directory to save to.
#' @param overwrite Whether to overwrite existing files, with "plot" respecting
#'     the property stored in a \code{\link{labeled_plot}}.
#' @param verbose Maximum list depth to report saving
#' @param default_format Default file format to save in
#' @export
save_plotlist <- function(plotlist, root, overwrite = c("none", "all", "plot"),
                          verbose = 1, default_format = "pdf"){
  overwrite <- match.arg(overwrite)

  # remove trailing / in path
  root <- gsub("*/$", "", root)

  # Check root  exists
  if (!dir.exists(root)){
    dir.create(root)
  }

  if (verbose > 0){
    message(paste0("Saving plots to ", root, ':'))
  }
  sv_pltlst(plotlist, root, overwrite, verbose, default_format = default_format)
}

#' Internal recursive function for saving plotlists
#'
#' This function performs the recursive saving for \code{\link{save_plotlist}}.
#' It's parameters mostly link to those in \code{save_plotlist}.
#'
#' @param p Plot or list to save.
#' @param root Root directory.
#' @param overwrite Level of overwriting from \code{c("none", "all", "plot")}.
#' @param verbose Maximum depth to report at.
#' @param level Current depth.
#' @param default_format Default file format to save to.
#' @keywords internal
sv_pltlst <- function(p, root, overwrite, verbose, level = 1,
                      default_format = "pdf"){
  for (name in names(p)){
    #### Recursively process sublists ####
    if (class(p[[name]])[1] == "list"){
      if (level <= verbose){
        message(paste0(paste0(rep("  ", level), collapse = ""), name))
      }

      next_dir <- paste0(root, "/", name)
      if (!dir.exists(next_dir)){
        dir.create(next_dir)
      }

      sv_pltlst(p = p[[name]], root = next_dir, overwrite = overwrite,
                verbose = verbose, level = level + 1)

    #### Save labeled_plot objects ####
    } else if ("labeled_plot" %in% class(p[[name]])){
      params <- attributes(p[[name]])$plotlistr_params

      if (!is.null(params$filename)){
        plot_name <- params$filename
      } else {
        plot_name <- paste0(name, ".", ifelse(!is.null(params$file_format),
                                              params$file_format,
                                              default_format))
      }

      plot_path <- paste0(root, "/", plot_name)

      if (level <= verbose){
        message(paste0(paste0(rep("  ", level), collapse = ""), plot_name))
      }

      # Write figure
      if (
        !file.exists(plot_path) |
        overwrite == "all" |
        (params$overwrite & overwrite == "plot")
      ){
        smart_save(p[[name]], plot_path)
      }

    #### Save unlabled plots ####
    } else {
      # Save other plots
      plot_name <- paste0(name, ".", default_format)
      plot_path <- paste0(root, "/", plot_name)

      # Report plot/list being processed
      if (level <= verbose){
        message(paste0(paste0(rep("  ", level), collapse = ""), plot_name))
      }

      # Only write figures that don't exist, delete figs to regenerate
      if (!file.exists(plot_path) | overwrite == "all"){
        smart_save(p[[name]], plot_path)
      }
    }
  }
}

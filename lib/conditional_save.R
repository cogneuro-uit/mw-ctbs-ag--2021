#' Save an `object` based on conditions set in "options()"
#'
#' A universal function to save data, figures, and tables based on project options.
#' Detects the object type and applies the appropriate saving method.
#'
#' @param data The object to save (data, figure/plot, or table)
#' @param name The name for the output file
#' @param ... Additional parameters passed to the respective save functions
#' @param .relative_path Manually set a relative path (overrides project options)
#' @param .suppressMessages Suppress status messages
#' @param .suppress_print Whether the object should be printed or not
#'
#' @return Object (invisiblely if .suppress_print = TRUE)
#'
#' @examples
#' # Save a data object
#' bayesian_model <- brms::brm(mpg ~ cyl, mtcars) 
#' conditional_save(bayesian_model, "bayes_test")
#'
#' # Save a ggplot figure
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' conditional_save(p, "scatter_plot", width = 8, height = 6)
#'
#' # Save a gt table
#' tbl <- gt(mtcars[1:5, 1:5])
#' conditional_save(tbl, "car_data_table")
#'
#' @export
conditional_save <- function(data, name, ...
                             , .relative_path = NULL
                             , .suppressMessages = FALSE
                             , .print = FALSE) {
  
  # Get the name of the input data for data objects
  data_name <- deparse(substitute(data))
  
  # Determine the class of the input object
  obj_class <- class(data)
  
  # Handle relative path - preserving original behavior
  if (!exists("relative_path", inherits = TRUE) && is.null(.relative_path)) {
    relative_path_final <- ""
  } else if (!is.null(.relative_path)) {
    relative_path_final <- .relative_path
  } else {
    relative_path_final <- get("relative_path", inherits = TRUE)
  }
  
  # Initialize datetime suffix
  datetime_suffix <- ""
  
  # Process based on object type
  if (any(obj_class %in% c("gg", "ggplot"))) {
    # Figure/plot object
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      stop("Package 'ggplot2' is required for saving figures but is not available")
    }
    
    obj_type <- "figure"
    save_enabled <- getOption("project_save_figs_to_file", default = FALSE)
    save_location <- getOption("project_save_figs_location", default = "figures/")
    save_formats <- getOption("project_save_figs_formats", default = c(".png"))
    save_with_datetime <- getOption("project_save_figs_with_date_time", default = FALSE)
    
    # Validate formats
    valid_formats <- c(".eps", ".ps", ".tex", ".pdf", ".jpeg", ".tiff", ".png", ".bmp", ".svg")
    if (!any(save_formats %in% valid_formats)) {
      warning("Figure format(s) not valid. Saving as .png")
      save_formats <- ".png"
    }
    
    # Set save function
    save_func <- function(filename, ...) {
      ggplot2::ggsave(filename, plot = data, ...)
    }
    
  } else if (any(obj_class %in% c("gt_tbl"))) {
    # Table object
    if (!requireNamespace("gt", quietly = TRUE)) {
      stop("Package 'gt' is required for saving tables but is not available")
    }
    
    obj_type <- "table"
    save_enabled <- getOption("project_save_tbls_to_file", default = FALSE)
    save_location <- getOption("project_save_tbls_location", default = "tables/")
    save_formats <- getOption("project_save_tbls_formats", default = c(".html"))
    save_with_datetime <- getOption("project_save_tbls_with_date_time", default = FALSE)
    
    # Validate formats
    valid_formats <- c(".html", ".tex", ".ltx", ".rtf", ".docx")
    if (!any(save_formats %in% valid_formats)) {
      warning("Table format not valid, setting to .rtf...")
      save_formats <- ".rtf"
    }
    
    # Set save function
    save_func <- function(filename, ...) {
      gt::gtsave(data, filename, ...)
    }
    
  } else {
    # Assume it's a data object
    obj_type <- "data"
    save_enabled <- getOption("project_save_data_to_file", default = FALSE)
    save_location <- getOption("project_save_data_location", default = "data/")
    save_formats <- getOption("project_save_data_formats", default = c(".RData"))
    save_with_datetime <- getOption("project_bayes_save_with_date_time", default = FALSE)
    
    # Set save function
    save_func <- function(filename, ...) {
      save(list = data_name, file = filename, ...)
    }
  }
  
  # Add datetime suffix if configured
  if (save_with_datetime) {
    datetime_suffix <- getOption("project_date_time", default = format(Sys.time(), "_%Y%m%d_%H%M%S"))
  }
  
  # Process the save operation
  if (save_enabled) {
    if (!.suppressMessages) message("Saving ", obj_type, "...")
    
    # Create base filename
    base_filename <- paste0(relative_path_final, save_location, name, datetime_suffix)
    
    if (!.suppressMessages) message("Saving files to: ", base_filename, "[format]")
    
    # Save across all specified formats
    for (fmt in save_formats) {
      filename <- paste0(base_filename, fmt)
      if (!.suppressMessages) message("Saving ", obj_type, " with format: ", fmt)
      save_func(filename, ...)
    }
    
    if (!.suppressMessages) message("Done!")
  } else {
    if (!.suppressMessages) {
      # Capitalize first letter of object type
      obj_type_cap <- paste0(toupper(substr(obj_type, 1, 1)), 
                             substr(obj_type, 2, nchar(obj_type)))
      message(obj_type_cap, " saving not enabled, skipping...")
    }
  }
  
  # Return the object (invisibly if requested)
  if (.print) {
    data
  } else {
    invisible(data)
  }
}

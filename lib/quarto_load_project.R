# General loader function
loader <- function(path){
  lapply( path,  \(item){
    message("Loading: ", item)
    
    # If "dcf":
    if( grepl("\\.dcf$", item, ignore.case = T)){
      strsplit(
        read.dcf( item )[,"libraries"], 
        ", "
      )[["libraries"]] |>
        lapply( \(library){
          cat("Loading library:", library, "\n")
          library(library, character.only = T)
        })
    }
    
    # If "data": 
    if ( grepl("\\.rdata$", item, ignore.case = TRUE) ){
      load(item, envir = .GlobalEnv)
    }
    
    # If "ccp": 
    if( grepl("\\.cpp$", item,  ignore.case = TRUE) ){
      Rcpp::sourceCpp(item)
    }

    # If ".r"
    if ( grepl("\\.r$", item, ignore.case = TRUE) ){
      source(item)
    }
  })
}

#' Single function to load all
quarto_load_project <- function(relative_path, 
                                load_libraries = TRUE,
                                load_local_library = TRUE,
                                load_caches = TRUE,
                                load_data_data = FALSE,
                                load_data_scripts = FALSE,
                                load_munges = TRUE){
  # Get path
  get_path <- function(path, pattern){
    list.files( paste0(relative_path, path), 
                pattern = pattern, 
                ignore.case = T, 
                full.names = T)
  }
  
  # Load libraries
  #' Specified in the .dcf file for ProjectTemplate file structure.
  if( load_libraries ) loader( get_path("config", "\\.dcf$") )
  
  #' Load local libraries
  if( load_local_library ){
    loader( get_path("lib", ".r$") )
    loader( get_path("lib", "\\.cpp$") )
  }
  
  # Cache
  if( load_caches ) loader( get_path("cache", "\\.rdata$") )
  
  # Data
  if( load_data_data ) loader( get_path("data", "\\.rdata$") )
  # Data scripts
  if( load_data_scripts ) loader (get_path("data", "\\.r$") )
  
  # Munge  
  if( load_munges ) loader( get_path("munge", "\\.r$") )
}

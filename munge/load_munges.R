message("=====     Munging data     =====")


load_list <- list.files(
  paste0(relative_path, "munge"), 
  recursive = T,
  full.names = T)

# Load data
sapply( 
  load_list[!str_detect(load_list, "load_munges")],
  \(file){
    message("Loading: ", file)
    source(file)
})
rm(load_list)

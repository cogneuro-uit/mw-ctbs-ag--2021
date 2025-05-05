fnames <- list.files("data/raw", pattern="*.csv")

raw_data <- map_df(fnames, \(fname){
  d <- read_csv(file.path("data/raw/", fname), comment = "#")
  session <- str_split(fname, "_")[[1]][3]
  d$session=session

  return(d)
})

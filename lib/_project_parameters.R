#'                    **PROJECT PARAMETERS**                              ======
message("Setting Project options...")
options(
  #' Set general project parameters
  #' **Data related settings **
  project_save_data_to_file        = TRUE,
  project_save_data_location       = "data/",
  project_save_data_formats        = c(".RData"),
  project_save_data_with_date_time = FALSE,

  #' **Figure related settings**
  project_save_figs_to_file        = TRUE,
  project_save_figs_location       = "figures/",
  project_save_figs_formats        = c(".svg", ".jpeg"),
  project_save_figs_with_date_time = FALSE,

  #' **Table related settings**
  project_save_tbls_to_file        = TRUE,
  project_save_tbls_location       = "tables/",
  project_save_tbls_formats        = c(".html"),
  project_save_tbls_with_date_time = FALSE,

  #' **Bayesian related settings**
  project_bayes_run_models          = FALSE,
  project_bayes_save_to_file        = FALSE,
  project_bayes_save_with_date_time = TRUE,
  project_bayes_diagnostics         = TRUE,
  mc.cores                          = 6,
  brms.backend                      = "cmdstanr",

  #' **GGplot**
  project_custom_ggplot = TRUE,



  #' Generally, leave the below alone.
  project_custom_colours = list(
    `-r` = "#ffd6d2", r = "#F8766D", R = "#b20b00", `+R` = "#8c0000",
    `-b` = "#d6e9ff", b = "#78ADFF", B = "#00348C", `+B` = "#00246D",
    `-g` = "#c9ffd6", g = "#00BA38", G = "#006400", `+G` = "#004B00",
    `-o` = "#ffe0c2", o = "#FF9E4A", O = "#B25000", `+O` = "#8C3D00",
    `-y` = "#fffbd6", y = "#FFE359", Y = "#D1B000", `+Y` = "#AA8F00",
    `-p` = "#e9d6ff", p = "#B476FF", P = "#7436B3", `+P` = "#5A1A99"
  ),
  # project_custom_colours_rrR      = c("#ffd6d2", "#F8766D", ),
  # project_custom_colours_rgy      = c("#F8766D", "#619CFF", "#f8ea6d"),
  # project_custom_colours_pale_bBrR = c("#4477AA","#114477","#EE6677","#991122"),

  #' Because partial matching exists, warn whenever such an instance occur,
  #' as this can be especially problematic for statistical analysis.
  warnPartialMatchArgs = TRUE,
  warnPartialMatchAttr = TRUE,
  warnPartialMatchDollar = TRUE
)

#' If conditions can take vectors but only evaluate the first on,
#' this will add a warning.
Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_"=1)

message("Complete.")
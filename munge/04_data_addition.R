# Selected demographics
selected_demographics <-
  demographics |>
  pivot_longer(c(S1_Tired, S2_Tired, S1_Snack, S2_Snack)) |>
  separate_wider_delim(name, "_", names_sep="_", names=c("session","type")) |>
  pivot_wider(names_from=name_type, values_from = value)

# Bind demo to data
ag.data.demo <-
  d.pro.stim_ag |>
  left_join(
    selected_demographics
    , by = join_by("subj"=="Code", "session"=="name_session")
  ) |>
  mutate(
    music_exp = if_else(Music_years>0, "yes", "no")
  )

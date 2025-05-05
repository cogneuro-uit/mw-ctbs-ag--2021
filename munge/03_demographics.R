demographics <-
  demographics |>
  # select(Code, Age, Gender,
  #        Music_years, Meditation,
  #        S1_Tired, S2_Tired,
  #        S1_Snack, S2_Snack) |>
  mutate(
    Code = paste0("AG", str_remove_all(Code, " "))
    , across(ends_with("Snack"), ~ case_when(.x=="no"~0, .x=="yes"~1, T~NA))
    , Meditation = case_when(
      Meditation=="no"~0
      , Meditation=="some"~1
      , Meditation=="moderate"~2
      , Meditation=="much"~3
      , T ~ NA
    )
    , across(ends_with("Tired"), ~case_when(
      .x=="fully awake"~10
      , .x=="extremely tired"~1
      , T~as.numeric(.x)))
    # reverse, such that higher scores indicate "tired"
    , across(ends_with("Tired"), ~10-.x)
  )

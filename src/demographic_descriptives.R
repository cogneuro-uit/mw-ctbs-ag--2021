library(ProjectTemplate)
# migrate.project()
load.project()

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#' These toggles makes it easy to return the various aspects your might want
#' and nevertheless run the whole script without necessarily all the other aspects

# Toggles           =====
script_save_figures <- FALSE
#' **TRUE** will save figure
#' **FALSE** will *NOT* save figures

script_save_tables <- FALSE
#' **TRUE** will save tables
#' **FALSE** will *NOT* save tables

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


# Demographics        ======
## Age =====
demographics |> summarize(
  mean = mean(Age), sd=sd(Age), min=min(Age), max=max(Age))

## Gender         ======
demographics |> summarize(
  male = sum(Gender=="Male"),
  female = sum(Gender=="Female"))

## Music          ======
demographics |> summarise(
  min = min(Music_years),
  max = max(Music_years),
  mean = mean(Music_years),
  sd = sd(Music_years),
  sum_music_exp = sum( Music_years > 0 ),
  sum_no_music_exp = sum( Music_years == 0 ) )

demographics |>
  ggplot(aes(Music_years))+
  geom_bar()

## Meditation       ======
demographics |> summarise(
  min = min(Meditation),
  max = max(Meditation),
  mean = mean(Meditation),
  sd = sd(Meditation),
  sum_med_exp = sum(Meditation>1),
  sum_no_med_exp=sum(Meditation==1))

demographics |>
  ggplot(aes(Meditation))+
  geom_bar()

## TMS expectation     =====
stimulation_expectation <-
  demographics |>
  select(Code, contains("_expectation")) |>
  pivot_longer(contains("expectation")) |>
  summarise(
    .by = name,
    `No expectation`   = sum(value=="No, I have no expectations"),
    `Yes, increase`    = sum(value=="Yes, it is going to improve my performance"),
    `Yes, reduce`      = sum(value=="Yes, it is going to reduce my performance"),
    `Yes, but not how` = sum(value=="Yes, but do not know to which extent"),
    `Don't know`       = sum(value=="I do not know")
  ) |>
  pivot_longer(c(everything(), -name), names_to="Expectation") |>
  pivot_wider(names_from = name, values_from = value) |>
  rename(`Session 1` = S1_TMS_expectation, `Session 2` = S2_TMS_expectation ) |>
  mutate(n =  "") |>
  gt() |>
  cols_move(n, after ="Session 1") |>
  cols_label(n = "")

stimulation_expectation
if(script_save_tables){
  gtsave(stimulation_expectation, "tables/Stimulation_Expectation.docx")
}
# see analysis in exploratory analyses

# Guesses     ======
## Stimulation guesses      ======
# Get the data

# Get the stimulation guesses
stim_predictions <-
  demographics |>
  select(Code, contains("FB_stimulation"), contains("researcher"),-contains("conf")) |>
  mutate(across(contains("_stim"), ~if_else(str_starts(.x, "Fake"), "sham", "real"))) |>
  pivot_longer(c(contains("Researcher"), contains("_FB_")), values_to = "guess")  |>
  mutate(
    .before = 2
    , session = ifelse(str_detect(name, "[Ss]1"), "S1", "S2")
    , pers = ifelse(str_detect(name, "Resear"), "Researcher", "Participant")
    , name = NULL
    , guess_n = if_else(guess=="real", 1, 0)
  ) |>
  #' ATTACH the **true stimulation** received
  left_join(
    ag.data |>
      select(subj, session, stimulation) |>
      reframe(
        .by = subj
        , session = unique(session)
        , true_stim = unique(stimulation)
        , true_stim_n = if_else(true_stim=="real", 1, 0))
    , by = join_by("Code"=="subj", "session")
  ) |>
  #' ATTACH the **confidence in the guesses**
  left_join(
    demographics |>
      select(contains("conf"), Code, -contains("_task_")) |>
      mutate(
        across(contains("conf"), ~case_when(
          .x=="Very confident"    ~ 7
          , .x == "Not confident" ~ 1
          , T ~ as.numeric(.x)))
      ) |>
      pivot_longer(contains("_"), values_to = "guess_confidence") |>
      mutate(
        , pers = ifelse(str_detect(name, "[Rr]es"), "Researcher", "Participant"),
        , session = ifelse(str_detect(name, "[Ss]1"), "S1", "S2")
        , name = NULL
      )
    , by = c("Code", "session", "pers")
  ) |>
  left_join(
    demographics |>
      select(Code, S2_FB_change_answer) |>
      mutate(Change = case_when(
        S2_FB_change_answer=="Not sure" ~ 2
        , S2_FB_change_answer=="Yes"    ~ 1
        , S2_FB_change_answer=="No"     ~ 0
      ) )
    , by = "Code"
  )

# summarise
guess_table <-
  stim_predictions |>
  summarise(
    .by = c(session, pers, true_stim),
    real_guess = sum(guess=="real"),
    sham_guess = sum(guess=="sham")
  ) |>
  pivot_wider(names_from=pers, values_from = c(real_guess, sham_guess))

# Create the table
guess_table |>
  add_row(
    guess_table |>
      pivot_longer(contains("_guess")) |>
      summarise(
        .by = c(session, name),
        sum = sum(value)
      ) |>
      pivot_wider(names_from = name, values_from = sum) |>
      mutate(true_stim=c("Total","Total"))
  ) |>
  mutate(
    par_sum = sham_guess_Participant + real_guess_Participant,
    res_sum = sham_guess_Researcher  + real_guess_Researcher,
  ) |>
  mutate(b="") |>
  gt() |>
  tab_spanner("Participants",
              c(ends_with("Participant"), par_sum)) |>
  tab_spanner("Researcher",
              c(ends_with("Researcher"), res_sum)) |>
  cols_label(
    starts_with("real") ~ "Real"
    , starts_with("sham") ~ "Sham"
    , ends_with("_sum") ~ "Total"
    , b = "", session = "Session", true_stim="True Stimulation") |>
  cols_move(b, par_sum) |>
  gtsave("tables/Blinding_Guesses_for_Participants_and_Researchers.docx")

### Test predictions         ======
stim_predictions |>
  summarise(
    .by    = c(session, pers),
    , chisq_s  = chisq.test( guess_n, true_stim_n)$statistic
    , chisq_p  = chisq.test( guess_n, true_stim_n)$p.value
    , fisher_p = fisher.test(guess_n, true_stim_n)$p.value
    , bf = extractBF( contingencyTableBF(
      table(guess, true_stim), sampleType = "jointMulti"
    ) )[["bf"]]
    , bfp = extractBF( contingencyTableBF(
      table(guess, true_stim), sampleType = "poisson"
    ) )[["bf"]]
  )


### Descriptives       =====
# Mean confidence rating across participant and researcher for real and sham.
stim_predictions |>
  summarise(
    .by = c(pers, session) # true_stim
    , m_real   = mean(guess_confidence[true_stim=="real"])
    , sd_real  = sd(  guess_confidence[true_stim=="real"])
    , m_sham   = mean(guess_confidence[true_stim=="sham"])
    , sd_sham  = sd(  guess_confidence[true_stim=="sham"])
    , te_t        = t.test(guess_confidence[true_stim=="sham"],
                        guess_confidence[true_stim=="real"])$statistic
    , te_df        = t.test(guess_confidence[true_stim=="sham"],
                        guess_confidence[true_stim=="real"])$parameter
    , te_p        = t.test(guess_confidence[true_stim=="sham"],
                        guess_confidence[true_stim=="real"])$p.value
    , bf       = extractBF(ttestBF(guess_confidence[true_stim=="sham"],
                                   guess_confidence[true_stim=="real"]))$bf
  ) |>
  mutate(
    across(c(is.numeric, -bf, -te_p), ~fmt_APA_numbers(.x))
    , te_p = fmt_APA_numbers(te_p, .p=T)
    , bf = fmt_APA_numbers(bf, .low_val = T)
    , e =""
  ) |>
  gt(groupname_col = "pers") |>
  tab_spanner("Sham ", ends_with("_sham") ) |>
  tab_spanner("Real ", ends_with("_real") ) |>
  tab_spanner("Statistics", c(starts_with("te"), bf) ) |>
  cols_move(ends_with("sham"), session) |>
  cols_move(e, sd_sham) |>
  cols_label(
    starts_with("m_") ~ md("*M*")
    , starts_with("sd_") ~ md("*SD*")
    , starts_with("e") ~ ""
    , bf = md("BF~10~")
    , session = "Session"
    , te_df = "df"
    , te_p = md("*p*")
    , te_t = md("*t*")
  ) |>
  gtsave("tables/confidence_in_guesses.docx")


## Change Prediction       ======
### Test the changed predictions      ====
stim_predictions |>
  filter(pers=="Participant") |>
  mutate( guess = case_when(
    session=="S1" & guess == "real" & Change==1 ~ "sham",
    session=="S1" & guess == "sham" & Change==1 ~ "real",
    T ~ guess
 )) |>
  summarise(
    f.p    = fisher.test(guess_n, true_stim_n)$p.value
    , chi.t = chisq.test(guess_n, true_stim_n)$statistic
    , chi.p = chisq.test(guess_n, true_stim_n)$p.value
    , bf = extractBF( contingencyTableBF(
        table(guess_n, true_stim_n), sampleType = "jointMulti"
    ) )[["bf"]]
    , bf_p = extractBF( contingencyTableBF(
        table(guess_n, true_stim_n), sampleType = "poisson"
    ) )[["bf"]]
  )


# Feedback + Tired      =====
# prepare data:
demographics |>
  select(Code, S1_FB_task_confidence, S2_FB_task_confidence,
         S1_FB_motivation, S2_FB_motivation,
         S1_FB_subject_tracker, S2_FB_subject_tracker,
         S1_Tired, S2_Tired) |>
  mutate( across( ends_with("confidence"), ~ case_when(
    .x=="Extremely confident" ~ 7
    , .x =="Not confident" ~ 1
    , T ~ as.numeric(.x)
  )), across( ends_with("motivation"), ~ case_when(
    .x=="Very motivated" ~ 7
    , .x =="Not motivated" ~ 1
    , T ~ as.numeric(.x)
  )), across( ends_with("tracker"), ~ case_when(
    .x   == "Very distracted" ~ 7
    , .x == "Not atdistracted at all" ~ 1
    , T ~ as.numeric(.x)
  )) ) |>
  pivot_longer(c(everything(), -Code) ) |>
  mutate(
    session = if_else(str_starts(name, "S1"), "S1", "S2")
    , type = case_when(
      str_ends(name,"dence") ~ "Attention confidence"
      , str_ends(name,"tion") ~ "Randomness motivation"
      , str_ends(name,"red") ~ "Tired"
      , str_ends(name, "cker") ~ "Subject tracker"
    ), name = NULL
  ) |>
  rename(rating = value) |>
  left_join(
    stim_predictions |> filter(pers=="Participant")
    , by = c("Code", "session")
  ) |>
  filter(!is.na(rating)) |> # one missing
  summarise(
    .by = c(type)
    , m_real  = mean(rating[true_stim == "real"], na.rm = T)
    , sd_real = sd(  rating[true_stim == "real"] , na.rm = T)
    , m_sham  = mean(rating[true_stim == "sham"], na.rm = T)
    , sd_sham = sd(  rating[true_stim == "sham"] , na.rm = T)
    , bf      = extractBF( ttestBF(
      rating[true_stim == "sham"], rating[true_stim == "real"]
      ))$bf
  )



#' **   THIS DATA IS MISSING FORM THE FILE ?   **
#' **   THIS DATA IS MISSING FORM THE FILE ?   **
#' **   THIS DATA IS MISSING FORM THE FILE ?   **
#' **   THIS DATA IS MISSING FORM THE FILE ?   **
#' **   THIS DATA IS MISSING FORM THE FILE ?   **
#' **   THIS DATA IS MISSING FORM THE FILE ?   **
if(FALSE){
  #   Adverse effects of the stimulation          ======
  tms_checklist_stim_diff <-
    demographics |>
    select(contains("Checklist"), -S2_Checklist_comments, -S1_Checklist_comments, Code,
           -S1_Checklist_other_specify) |>
    pivot_longer( c(everything(), -Code), ) |>
    mutate( session = str_split(name, "_" ) |> map_chr(1),
            name = str_split( name, "_Checklist_" ) |> map_chr(2)) |>
    select( Code, session, name, value ) |> #-> tms_checklist
    # Add stimulation type
    left_join(demographics |>
                select(Code, true_stim1, true_stim2) |>
                pivot_longer(c(true_stim1, true_stim2),
                             names_to = "session", values_to = "stimulation") |>
                mutate(session = ifelse(session=="true_stim1", "S1", "S2")),
              by = join_by(Code, session)) |>  #-> tms_checklist_stim
    select(-session) |>
    pivot_wider(names_from = stimulation, values_from = value) |>
    # Transforme NA to 1 (assumed)
    # If this is used ????
    mutate(`0` = ifelse(`0` %in% c(NA), 1, `0`),
           `1` = ifelse(`1` %in% c(NA), 1, `1`))

  ## Adverse outcome        =====
  checklist_outcomes <-
    tms_checklist_stim_diff |>
    filter(!str_detect(name, "_TMS")) |>
    summarize(
      .by = name,
      s_m = mean(`0`, na.rm = T),
      s_sd = sd(`0`, na.rm = T),
      r_m = mean(`1`, na.rm = T),
      r_sd = sd(`1`, na.rm = T),
      m_diff = mean(`1` - `0`),
      t  = t.test(`1`, `0`, paired=T)$statistic,
      df = t.test(`1`, `0`, paired=T)$parameter,
      p  = t.test(`1`, `0`, paired=T)$p.value,
      bf01 = 1/extractBF( ttestBF(`1`, `0`, paired=T))$bf,
    ) |>
    mutate(p.adj = p.adjust(p, "bonferroni")) |>
    rename_with(~paste0("Sym_",.x))


  ## Relation to TMS  ======
  checklist_o_related_tms <-
    tms_checklist_stim_diff |>
    filter(str_detect(name, "_TMS")) |>
    summarize(
      .by = name,
      s_m = mean(`0`, na.rm=T),
      s_sd = sd(`0`, na.rm=T),
      r_m = mean(`1`, na.rm=T),
      r_sd = sd(`1`, na.rm=T),
      m_diff = mean(`1` - `0`),
      t  = t.test(`1`, `0`, paired=T)$statistic,
      df = t.test(`1`, `0`, paired=T)$parameter,
      p  = t.test(`1`, `0`, paired=T)$p.value,
      bf01 = 1/extractBF(ttestBF(`1`, `0`, paired=T))$bf,
    ) |>
    mutate(
      p.adj = p.adjust(p, "bonferroni"),
      name = str_split(name, "_TMS") |> map_chr(1)
    ) |>
    rename_with(~paste0("TMS_", .x))


  ## Create table      =====
  tms_adverse_outcome_tbl <-
    checklist_outcomes |>
    left_join(checklist_o_related_tms, by=join_by("Sym_name"=="TMS_name")) |>
    rename(Symptom ="Sym_name") |>
    mutate(
      Symptom = str_replace_all(Symptom, "_", " "),
      empty1="", empty2="", empty3="", empty4="", empty5="",
      across(contains("_p"), ~fmt_APA_numbers(.x, .p=T)),
      across(where(is.double), ~fmt_APA_numbers(.x))
    ) |>
    gt() |>
    opt_table_font(font = c("san-serif", "Times New Roman")) |>
    cols_align("center", -1) |>
    cols_move(empty1, Sym_s_sd) |>
    cols_move(empty2, Sym_r_sd) |>
    cols_move(Sym_p.adj, Sym_p) |>
    cols_move(empty3, Sym_bf01) |>
    cols_move(empty4, TMS_s_sd) |>
    cols_move(empty5, TMS_r_sd) |>
    cols_move(TMS_p.adj, TMS_p) |>
    tab_spanner("Sham", starts_with("Sym_s")) |>
    tab_spanner("Real", starts_with("Sym_r")) |>
    tab_spanner("Sham", id = "Sham2", starts_with("TMS_s")) |>
    tab_spanner("Real", id = "Real2", starts_with("TMS_r")) |>
    tab_spanner("Symptom report", c(starts_with("Sym"), empty1, empty2)) |>
    tab_spanner("Relation to TMS", c(starts_with("TMS_"), empty4, empty5)) |>
    cols_label(
      ends_with("_m") ~ md("*M*"),
      ends_with("_sd") ~ md("*SD*"),
      ends_with("_m_diff") ~ md("*M*~diff~"),
      ends_with("_t") ~ md("*t*"),
      ends_with("_df") ~ md("*df*"),
      ends_with("_p") ~ md("*p*"),
      ends_with("_p.adj") ~ md("*p*~adj~"),
      ends_with("_bf01") ~ md("BF~01~"),
      starts_with("empt") ~ ""
    ) |>
    tab_footnote(md("Note. Symptom reports  are reported using four categories:\
                  (1) absent, (2) mild, (3) moderate, and (4) severe.\
                  The relation of the symptom to the TMS are answered using 5 categories: \
                  (1) no, (2) unlikely, (3) possible, (4) probable, and (5) definitively.")) |>
    tab_header(md("**TMS adverse outcomes**")) |>
    tab_options(
      # hide the top-most border
      table.border.top.color = "white",
      # make the title size match the body text
      heading.title.font.size = px(16),
      # change the column labels section
      column_labels.border.top.width = 3,
      column_labels.border.top.color = "black",
      column_labels.border.bottom.width = 3,
      column_labels.border.bottom.color = "black",
      # change the bottom of the body
      table_body.border.bottom.color = "black",
      # hide the bottom-most line or footnotes
      # will have a border
      table.border.bottom.color = "white",
      # make the width 100%
      table.width = pct(100),
      table.background.color = "white"
    ) |>
    tab_style(
      style = list(
        # remove horizontal lines
        cell_borders(
          sides = c("top", "bottom"),
          color = "white",
          weight = px(1)
        ),
        # remove row striping in Markdown documents
        cell_fill(color = "white", alpha = NULL)
      ),
      #do this for all columns and rows
      locations = cells_body(
        columns = everything(),
        rows = everything()
      )
    )

  tms_adverse_outcome_tbl
  if(script_save_tables){
    gtsave(tms_adverse_outcome_tbl, "tables/adverse_outcomes_table-V2.docx")
  }

  ##  Figures    ======
  ### Symptom report    =====
  p1 <-
    tms_checklist_stim_diff |>
    filter(!str_detect(name, "_TMS")) |>
    mutate( name = str_replace_all(name, "_TMS", ""),
            name = str_replace_all(name, "_", " "),
            name = factor(name, levels = c(
              "other",
              "Trouble concentrating",
              "Tingling",
              "Sudden mood change",
              "Sleepiness",
              "Skin redness",
              "Scalp pain",
              "Neck pain",
              "Itching",
              "Headache",
              "Burning sensation"), ordered = T)) |>
    rename(Sham = `0`, Real = `1`) |>
    pivot_longer(c(Sham, Real), names_to = "stim") |>
    ggplot(aes(value, name, group = stim)) +
    facet_wrap( ~ stim) +
    geom_point(position = position_jitter(width = 0.1, height = 0.3), alpha =.25) +
    stat_summary(fun.data = mean_se, col="red", position = position_nudge()) +
    scale_x_continuous(breaks = 1:5,
                       labels = c("(1) No ", "(2) Unlikely", "(3) Possible", "(4) Probable", "(5) Definitively")) +
    labs(y="Symptom", x = "", title = "A) Side-effect report")

  p1
  if(script_save_figures){
    ggsave("figs/TMS_Checklist_figure_symptom.svg", p1, height = 5, width = 10)
  }

  ### Relation to TMS   =====
  p2 <-
    tms_checklist_stim_diff |>
    filter(str_detect(name, "_TMS")) |>
    mutate( name = str_replace_all(name, "_TMS", ""),
            name = str_replace_all(name, "_", " "),
            name = factor(name, levels = c(
              "other",
              "Trouble concentrating",
              "Tingling",
              "Sudden mood change",
              "Sleepiness",
              "Skin redness",
              "Scalp pain",
              "Neck pain",
              "Itching",
              "Headache",
              "Burning sensation"), ordered = T)) |>
    rename(Sham = `0`, Real = `1`) |>
    pivot_longer(c(Sham, Real), names_to = "stim") |>
    ggplot(aes(value, name, group = stim)) +
    facet_wrap( ~ stim) +
    geom_point(position = position_jitter(width = 0.1, height = 0.3), alpha =.25) +
    stat_summary(fun.data = mean_se, col="red", position = position_nudge()) +
    scale_x_continuous(breaks = 1:5,
                       labels = c("(1) No ", "(2) Unlikely", "(3) Possible", "(4) Probable", "(5) Definitively")) +
    labs(y="Symptom", x = "Response", title = "B) Relation to TMS")

  p2
  if(script_save_figures){
    ggsave("figs/TMS_Checklist_figure_tms.svg", p2, height = 5, width = 10)
  }
}

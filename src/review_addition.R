# Script parameters
script_run_bayesian_models <- FALSE
script_save_bayesian_models <- FALSE

# Results        =====
## Contingency table of the probes   =====
ag.data |>
  mutate(
    cond_mw  = if_else( as.numeric(probe1) > 2, "MW", "ON"),
    cond_mb  = if_else( as.numeric(probe2) > 2, "MB", "Content"),
    cond_smw = if_else( as.numeric(probe3) > 2, "SMW", "DMW"),
  ) |>
  summarise(
    .by  = c(cond_mw, cond_mb, cond_smw),
    n = n()
  ) |>
  pivot_wider(names_from = c(cond_mw,cond_mb), values_from = n) |>
  gt() |>
  tab_spanner("Mind wandering", starts_with("MW_")) |>
  tab_spanner("On-task", starts_with("ON_")) |>
  cols_label(
    cond_smw = "",
    ends_with("_MB")      ~ "Blanking",
    ends_with("_Content") ~ "Content",
  ) |>
  cols_align("center") |>
  tab_fmt_APA() |>
  gtsave("tables/ag_contigency_table.docx")


## Descriptive probes ======
### Percentage =====
plot_data <-
  ag.data |>
  summarise(
    .by = c(subj, stimulation),
    MW  = 100 * mean(probe1 > 2),
    MB  = 100 * mean(probe2[probe1 > 2] > 2, na.rm=T),
    SMW = 100 * mean(probe3[probe1 > 2] > 2, na.rm=T),
  ) |>
  pivot_longer(c(MW,MB,SMW), names_to="name", values_to="value") |>
  mutate(
    name = ordered(name, levels = c("MW", "MB", "SMW")),
    name = fct_recode(name, `% MW` = "MW", `% MB` = "MB", `% Spontaneous` = "SMW")
  )

ag_probe_percentage <-
  plot_data |>
  ggplot(aes(x = stimulation, y = value, col = stimulation)) +
  facet_wrap(~name) +
  # all lines
  stat_summary(aes(group = subj), col = "darkgrey", geom ="line", alpha =.25, # line
               position = position_jitter(.2, seed=389)) +
  # all data points
  stat_summary(aes(group = subj),  alpha =.25,
               position = position_jitter(.2, seed=389)) +
  # Cross
  # GRAND mean point - CONTINOUS
  stat_summary(aes(
    group = name), fun.data = mean_se, geom = "pointrange",
    color = "black", alpha = .65, size = .4 ,shape = 4,
  ) +
  # GRAND mean line - CONTINOUS
  stat_summary(aes(
    group = name), fun = mean, geom = "line",
    color = "black", alpha = .65, linewidth = .5,
  ) +
  labs(y = "Percentage", x = "Condition", title="a)") +
  theme(legend.position = "none")


### bar (count) plot          ======
plot2_data_sum <-
  ag.data |>
  mutate(
    across(c(probe1, probe2, probe3), ~as.numeric(.x))
  ) |>
  pivot_longer(c(probe1, probe2, probe3), names_to="probe_type", values_to="probe_value") |>
  summarise(
    .by = c(stimulation, probe_type, probe_value),
    cont_n = n(),
  ) |>
  mutate(
    probe_type = case_when(
      probe_type=="probe1" ~ "Mind wandering",
      probe_type=="probe2" ~ "Mind blanking",
      probe_type=="probe3" ~ "Spontaneous mind wandering",
    ) |> fct_relevel("Mind wandering"),
  )

# Diff geom
ag_probe_count <-
  plot2_data_sum |>
  ggplot(aes(x = probe_value, y = cont_n, fill = stimulation, group = stimulation)) +
  facet_wrap(~probe_type) +
  geom_bar(stat = "identity", color="black", position = position_dodge(), linewidth=0.2) +
  scale_fill_manual(values = c("#78ADFF", "#F8766D") ) +
  labs(x = "Thought probe response", y = "Count", title = "b)", fill = "Condition") +
  theme(legend.position = "top", legend.direction = "horizontal")


### SAVE    =====
ggsave(
  "figures/ag_probe_descriptives.svg",
  ag_probe_percentage + ag_probe_count +
    plot_layout(nrow=2,heights = 8, widths = 7)
  , width = 7, height = 8,
)


## Full probit-behaviour models         ====
if(script_run_bayesian_models){
  ag_mod <- list()


  ### Mind wandering        ======
  ag_mod[["mw"]] <- brm(
    probe1 ~ stimulation * block + zlogapen + zlogbv + proberound_prop + (1|subj)
    , data = ag.data, family = cumulative("probit"), backend = "cmdstanr"
    , cores=6, init = 0, chains = 6, iter = 6000) |>
    brms::add_criterion(c("bayes_R2", "loo", "loo_R2"))
  # Diagnostics
  brms::pp_check(   ag_mod[["mw"]], ndraws=50)
  bayes_chain_stab( ag_mod[["mw"]])
  bayes_diag(       ag_mod[["mw"]])


  ### Mind blanking           ======
  ag_mod[["mb"]] <- brm(
    probe2 ~ stimulation * block + zlogapen + zlogbv + proberound_prop + (1|subj)
    , data = ag.data |> filter(probe1>2), family = cumulative("probit"),  backend = "cmdstanr"
    , cores=6, init = 0, chains = 6, iter = 6000) |>
    brms::add_criterion(c("bayes_R2", "loo", "loo_R2"))
  # Diagnose:
  brms::pp_check(   ag_mod[["mb"]], ndraws=50)
  bayes_chain_stab( ag_mod[["mb"]])
  bayes_diag(       ag_mod[["mb"]])


  ### Spontaneous Mind wandering    ========
  ag_mod[["smw"]] <- brm(
    probe3 ~ stimulation * block + zlogapen + zlogbv + proberound_prop + (1|subj)
    , data = ag.data |> filter(probe1>2), family = cumulative("probit"),
    cores = 6, backend = "cmdstanr", init = 0, chains = 6, iter = 6000) |>
    brms::add_criterion(c("bayes_R2", "loo", "loo_R2"))

  # Diagnose
  brms::pp_check(  ag_mod[["smw"]], ndraws=50)
  bayes_chain_stab(ag_mod[["smw"]])
  bayes_diag(      ag_mod[["smw"]])


  if( script_save_bayesian_models ){
    save(ag_mod, file = "data/AG_bayes_probe_full_model+behaviour.rdata")
  }
} else {
  load("data/AG_bayes_probe_full_model+behaviour.rdata")
}


### Table Report        ======
bayes_tbl_sum(ag_mod[["mw"]], apa_table = T) |>
  bayes_tbl_add_sig() |>
  rename_with(~paste0("mw_", .x), c(-group, -var)) |>
  left_join(
    bayes_tbl_sum(ag_mod[["mb"]], apa_table = T) |>
      bayes_tbl_add_sig() |>
      rename_with(~paste0("mb_", .x), c(-group, -var))
    , by = c("group","var")
  ) |>
  left_join(
    bayes_tbl_sum(ag_mod[["smw"]],  apa_table = T) |>
      bayes_tbl_add_sig() |>
      rename_with(~paste0("smw_", .x), c(-group, -var))
    , by = c("group", "var")
  ) |>
  mutate(e1="",e2="") |>
  gt(groupname_col = "group") |>
  tab_spanner("Mind wandering", starts_with("mw_")) |>
  tab_spanner("Mind blanking", starts_with("mb_")) |>
  tab_spanner("Spontaneous mind wandering", starts_with("smw_")) |>
  cols_move(e1, mw_p) |>
  cols_move(e2, mb_p) |>
  cols_label(starts_with("e") ~ "" ) |>
  text_case_match(
    "stimulationreal" ~ "Stimulation"
    , "blockB1" ~ "Block 1"
    , "blockB2" ~ "Block 2"
    , "blockB3" ~ "Block 3"
    , "zlogbv" ~ "BV"
    , "zlogapen" ~ "AE"
    , "proberound_prop" ~ "Time"
    , "stimulationreal:blockB1" ~ "Stimulation x Block 1"
    , "stimulationreal:blockB2" ~ "Stimulation x Block 2"
    , "stimulationreal:blockB3" ~ "Stimulation x Block 3"
  ) |>
  tab_bayes_generics(
    pre_footnote = "For ordinal models, the leave one out (LOO) R² and R² may
    not be accurate since the outcome is treated as a continuous variable."
  ) |>
  gtsave("tables/ag_probit_table.docx")


## Test carry-over-effect of S1 stimulation on S2          =====
if( script_run_bayesian_models ) {

  # # data
  # ag.carry_over_data <-
  #   ag.data |>
  #   filter(block == "B0") |>
  #   mutate(
  #     # session 1 stimulation.
  #     s1_stim = if_else(session=="S1", stimulation, NA) |> fct_relevel("sham")
  #   ) |>
  #   group_by(subj) |>
  #   fill("s1_stim")

  carry_over <- list()

  ### Mind wandering          ======
  carry_over[["mw"]] <- brm(
    probe1 ~ session * stimulation + proberound_prop + (1|subj)
    , data = ag.data.demo |> filter( block=="B0")
    , family = cumulative("probit"), backend = "cmdstanr", cores = 6
    , init = 0, chains = 6, iter = 6000) |>
    brms::add_criterion(c("bayes_R2", "loo", "loo_R2"))

  # Diagnose
  brms::pp_check(  carry_over[["mw"]], ndraws = 50)
  bayes_chain_stab(carry_over[["mw"]])
  bayes_diag(      carry_over[["mw"]])


  ### Mind blanking         ======
  carry_over[["mb"]] <- brm(
    probe2 ~ session * stimulation + proberound_prop  + (1|subj)
    , data = ag.data.demo |> filter(block == "B0" & probe1 > 2)
    , family = cumulative("probit"), backend = "cmdstanr", cores = 6
    , init = 0, chains = 6, iter = 6000) |>
    brms::add_criterion(c("bayes_R2", "loo", "loo_R2"))

  # Diagnose
  brms::pp_check(  carry_over[["mb"]], ndraws = 50)
  bayes_chain_stab(carry_over[["mb"]])
  bayes_diag(      carry_over[["mb"]])


  ### Spontaneous mind wandering      ======
  carry_over[["smw"]] <- brm(
    probe3 ~ session * stimulation + proberound_prop + (1|subj)
    , data = ag.data.demo |> filter(block=="B0" & probe1 > 2)
    , family = cumulative("probit"), backend = "cmdstanr", cores = 6
    , init = 0, chains = 6, iter = 6000) |>
    brms::add_criterion(c("bayes_R2", "loo", "loo_R2"))

  # Diagnose
  brms::pp_check(  carry_over[["smw"]], ndraws = 50)
  bayes_chain_stab(carry_over[["smw"]])
  bayes_diag(      carry_over[["smw"]])


  ### Behavioural variability          ======
  carry_over[["bv"]] <- brm(
    zlogbv ~ session * stimulation + proberound_prop + (1|subj)
    , data = ag.data.demo |> filter(block=="B0"), backend = "cmdstanr", cores = 6
    , init = 0, chains = 6, iter = 6000) |>
    brms::add_criterion(c("bayes_R2", "loo", "loo_R2"))

  # Diagnose
  brms::pp_check(  carry_over[["bv"]], ndraws = 50)
  bayes_chain_stab(carry_over[["bv"]])
  bayes_diag(      carry_over[["bv"]])


  ### Approximate entropy         =======
  carry_over[["ae"]] <- brm(
    zlogapen ~ session * stimulation + proberound_prop + (1|subj)
    , data = ag.data.demo |> filter(block=="B0"), backend = "cmdstanr" , cores = 6
    , init = 0, chains = 6, iter = 6000) |>
    brms::add_criterion(c("bayes_R2", "loo", "loo_R2"))

  # Diagnose
  brms::pp_check(  carry_over[["ae"]], ndraws = 50)
  bayes_chain_stab(carry_over[["ae"]])
  bayes_diag(      carry_over[["ae"]])


  if( script_save_bayesian_models ){
    save(carry_over, file = "data/AG_bayes_carry_over_effect.rdata")
  }
} else {
  load("data/AG_bayes_carry_over_effect.rdata")
}

### Table report      ======
#### Probes      =====
bayes_tbl_sum(carry_over[["mw"]], apa_table = T) |>
  bayes_tbl_add_sig() |>
  rename_with(~paste0("mw_", .x), c(-var, -group)) |>
  left_join(
    bayes_tbl_sum(carry_over[["mb"]], apa_table = T) |>
      bayes_tbl_add_sig() |>
      rename_with(~paste0("mb_", .x), c(-group,-var))
    , by = c("var", "group")
  ) |>
  left_join(
    bayes_tbl_sum(carry_over[["smw"]], apa_table = T) |>
      bayes_tbl_add_sig() |>
      rename_with(~paste0("smw_", .x), c(-group,-var))
    , by = c("var", "group")
  ) |>
  mutate(var = case_when(
    var == "sessionS2" ~ "Session 2",
    var == "stimulationreal" ~ "Stimulation",
    var == "sessionS2:stimulationreal" ~ "Stimulation X Session 2",
    var == "proberound_prop" ~ "Trial",
    T ~ var )
  ) |>
  gt(groupname_col = "group") |>
  tab_spanner("Mind wandering", starts_with("mw_")) |>
  tab_spanner("Mind blanking", starts_with("mb_")) |>
  tab_spanner("Spontaneous mind wandering", starts_with("smw_")) |>
  cols_label(var ~ "") |>
  cols_align("center", c(everything(), -var)) |>
  tab_bayes_generics(pre_footnote = "
  For ordinal models, the leave one out (LOO) R² and R² may
    not be accurate since the outcome is treated as a continuous variable.
  The carry-over effect can be assessed form the interaction between session and stimulation. "
  ) |>
  gtsave("tables/carry_over_probes.docx")


#### Behaviour           =====
bayes_tbl_sum(carry_over[["bv"]], apa_table = T) |>
  bayes_tbl_add_sig() |>
  rename_with( ~paste0("bv_",.x), c(-var, -group) ) |>
  left_join(
    bayes_tbl_sum(carry_over[["ae"]], apa_table = T) |>
      bayes_tbl_add_sig() |>
      rename_with( ~paste0("ae_",.x), c(-var, -group) )
    , by = c("var", "group")
  ) |>
  mutate(var = case_when(
    var == "sessionS2" ~ "Session 2",
    var == "stimulationreal" ~ "Stimulation",
    var == "sessionS2:stimulationreal" ~ "Stimulation X Session 2",
    var == "proberound_prop" ~ "Trial",
    T ~ var )
  ) |>
  gt(groupname_col = "group") |>
  tab_spanner("Behavioural variability", starts_with("bv")) |>
  tab_spanner("Approximate entropy", starts_with("ae_")) |>
  cols_label( var ~ "" ) |>
  cols_align("center", c(everything(), -var)) |>
  tab_bayes_generics(
    pre_footnote = "For ordinal models, the leave one out (LOO) R² and R² may
    not be accurate since the outcome is treated as a continuous variable.
    The carry-over effect can be assessed form the interaction between session and stimulation."
  ) |>
  gtsave("tables/carry_over_behaviour.docx")



## Musical experience     =======
### Bayes models      ======
if( project[["bayes"]][["run_models"]] ){

  ag.mus <- list()

  # MW
  ag.mus$mw <- brm(
    probe1 ~ stimulation * block + proberound_prop + Music_years + (1|subj),
    data = ag.data.demo, backend = "cmdstanr", cores = 6, family = cumulative("probit"),
    init = 0, chains = 6, iter = 6000) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))
  # Diagnose
  brms::pp_check(   ag.mus[["mw"]] )
  bayes_chain_stab( ag.mus[["mw"]] )
  bayes_diag(       ag.mus[["mw"]] )
  #  bayes_tbl_sum(ag.mus$mw)

  # BV
  ag.mus$bv <- brm(
    zlogbv ~ stimulation * block + proberound_prop + Music_years + (1|subj),
    data = ag.data.demo,  backend = "cmdstanr", cores = 6,
    init = 0, chains = 6, iter = 6000) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))
  # Diagnose
  brms::pp_check(   ag.mus[["bv"]] )
  bayes_chain_stab( ag.mus[["bv"]] )
  bayes_diag(       ag.mus[["bv"]] )
  #  bayes_tbl_sum(ag.mus$bv)


  # AE
  ag.mus$ae <- brm(
    zlogapen ~ stimulation * block + proberound_prop + Music_years + (1|subj),
    data = ag.data.demo, backend = "cmdstanr", cores = 6,
    init = 0, chains = 6, iter = 6000) |>
    add_criterion(c("bayes_R2", "loo", "loo_R2"))
  # Diagnose
  brms::pp_check(   ag.mus[["ae"]] )
  bayes_chain_stab( ag.mus[["ae"]] )
  bayes_diag(       ag.mus[["ae"]] )
  #  bayes_tbl_sum(ag.mus$ae)


  ### Save      =======
  if(script_save_bayesian_model){
    save(ag.mus, file = "data/ag_bayes_music_on_behaviour.Rdata")
  }
} else {
  load("data/ag_bayes_music_on_behaviour.Rdata")
}

### Table report      =====
bayes_tbl_sum(ag.mus[["mw"]], apa_table = T) |>
  bayes_tbl_add_sig() |>
  rename_with( ~paste0("mw_",.x), c(-var, -group) ) |>
  left_join(
    bayes_tbl_sum(ag.mus[["bv"]], apa_table = T) |>
      bayes_tbl_add_sig() |>
      rename_with( ~paste0("bv_",.x), c(-var, -group) ) |>
      add_row(var = "Intercept[2]") |>
      add_row(var = "Intercept[3]") |>
      mutate(var = if_else(var=="Intercept", "Intercept[1]", var))
    # across(everything(), ~ if_else(is.na(.x), " ", as.character(.x))))
    , by = c("var", "group")
  ) |>
  left_join(
    bayes_tbl_sum(ag.mus[["ae"]], apa_table = T) |>
      bayes_tbl_add_sig() |>
      rename_with( ~paste0("ae_",.x), c(-var, -group) ) |>
      add_row(var = "Intercept[2]") |>
      add_row(var = "Intercept[3]") |>
      mutate(var = if_else(var=="Intercept", "Intercept[1]",var))
    , by = c("var", "group")
  ) |>
  gt(groupname_col = "group") |>
  tab_spanner("Mind wandering", starts_with("mw_")) |>
  tab_spanner("Behavioural variability", starts_with("bv")) |>
  tab_spanner("Approximate entropy", starts_with("ae_")) |>
  cols_label(
    ends_with("_m") ~ md("*b*"),
    ends_with("_hdi") ~ md("HDI"),
    ends_with("_er") ~ md("ER~dir~"),
    ends_with("_p") ~ md("*p*~dir~"),
    var ~ ""
  ) |>
  cols_align("center", c(everything(), -var)) |>
  text_case_match(
    "stimulationreal" ~ "Stimulation"
    , "blockB1" ~ "Block 1"
    , "blockB2" ~ "Block 2"
    , "blockB3" ~ "Block 3"
    , "zlogbv" ~ "BV"
    , "zlogapen" ~ "AE"
    , "proberound_prop" ~ "Time"
    , "stimulationreal:blockB1" ~ "Stimulation x Block 1"
    , "stimulationreal:blockB2" ~ "Stimulation x Block 2"
    , "stimulationreal:blockB3" ~ "Stimulation x Block 3"
    , "stimulationreal:zlogbv"  ~ "Stimulation x BV"
    , "stimulationreal:zlogapen" ~ "Stimulation x AE"
    , "Music_years" ~ "Music (years)"
  ) |>
  tab_bayes_generics(pre_footnote =
  "The leave one out (LOO) R² and R² may not be accurate for the ordinal model.
  Music Experience is coded as the raw experience (in years) the participants
  have, treated as a linear variable.")  |>
  fmt_missing(missing_text = "") |>
  gtsave("tables/music_on_behaviour+MW.docx")


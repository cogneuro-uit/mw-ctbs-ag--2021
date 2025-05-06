#' export for Ragnhild
#'
library(ProjectTemplate)
load.project()


#' We will perform a 2x4 rmANOVA (repeated measures analysis of variance) 
#' of task-focus (mind wandering) with stimulation type (active versus sham) 
#' and block (B0, B1, B2, & B3).
data.probe.cond |> filter(region=="Angular Gyrus") |> 
  select(subj,block,proberound,MW=probe1,AE=zlogapen, BV=zlogbv, stimulation) |>
  group_by(subj,block,stimulation) |>
  summarize(MW=mean(as.numeric(MW)), AE=mean(AE), BV=mean(BV)) |> 
  gather(var,val,MW,AE,BV) |>
  ungroup() |>
  pivot_wider(names_from=c("var","stimulation","block"),
              values_from = val) -> data.wide.ag
View(data.wide.ag)
write_csv(data.wide.ag, file="data/export/data_ag_aggregate_wide.csv")


data.probe.cond |> filter(region=="Angular Gyrus") |> 
  select(subj,block,proberound,MW1=probe1, MW2=probe2, MW3=probe3, AE=zlogapen, BV=zlogbv, stimulation) |>
  group_by(subj,block,stimulation) |>
  summarize(MW1=mean(as.numeric(MW1)), MW2=mean(as.numeric(MW2)), MW3=mean(as.numeric(MW3)), 
            AE=mean(AE), BV=mean(BV)) |> 
  gather(var,val,MW1,MW2,MW3,AE,BV) |>
  ungroup() |>
  pivot_wider(names_from=c("var","stimulation","block"),
              values_from = val) -> data.wide.ag.mw23
View(data.wide.ag.mw23)
write_csv(data.wide.ag.mw23, file="data/export/data_ag_aggregate_wide_mwall.csv")


#' If this rmANOVA is significant, we will calculate the difference between 
#' real and sham session (real-sham) for each block (both measures are 
#' within-subject). We will then compare this difference between B0 
#' (baseline) and the other three blocks using planned contrast (with Tukey's 
#' adjustment for multiple comparisons). We expect that this difference will 
#' be larger than baseline (B0) in all three post-stimulation blocks (B1, B2, 
#' and B3) indicating that real stimulation reduced the amount of MW relative 
#' to sham stimulation. Next, we will investigate whether this tentative effect 
#' will grow with repeated stimulations by comparing real-sham difference 
#' between B1 and B2, B1 and B3 as well as B2 and B3. We expect that all these 
#' comparisons to be larger than zero, indicating that effect gets stronger 
#' with repeated application of the stimulation. 
data.probe.cond |> filter(region=="Angular Gyrus") |> na.omit() |> 
  select(subj, region, stimulation, block, proberound, zlogapen, zlogbv, probe1)|>
  mutate(probe1=as.integer(probe1)) |>
  gather(var,val,zlogapen,zlogbv,probe1) |>
  pivot_wider(names_from=c(block,var), values_from=val) |>
  mutate(across(starts_with("B"), as.numeric)) |>
  mutate(B0_ae=0,
         B1_ae=B1_zlogapen-B0_zlogapen,
         B2_ae=B2_zlogapen-B0_zlogapen,
         B3_ae=B3_zlogapen-B0_zlogapen,
         B0_bv=0,
         B1_bv=B1_zlogbv-B0_zlogbv,
         B2_bv=B2_zlogbv-B0_zlogbv,
         B3_bv=B3_zlogbv-B0_zlogbv,
         B0_mw=0,
         B1_mw=-(B1_probe1-B0_probe1), ## x -1 so that more MW is positive
         B2_mw=-(B2_probe1-B0_probe1),
         B3_mw=-(B3_probe1-B0_probe1)) |>
  select(-ends_with("zlogapen"), -ends_with("zlogbv"), -ends_with("probe1")) |>
  pivot_longer(starts_with("B"), names_to = c("block","variable"), names_sep = "_") |>
  mutate(variable=fct_recode(variable, AE="ae",BV="bv",MW="mw")) -> data.probe.cond.diff

data.probe.cond.diff |>
  ggplot(aes(y=value, x=block, color=stimulation))+
  stat_summary(fun.data=mean_se, geom="pointrange", position=position_dodge(width=0.2))+
  stat_summary(fun=mean, geom="line", aes(group=stimulation), position=position_dodge(width=0.2))+
  geom_hline(yintercept = 0, linetype="dashed")+
  scale_color_manual(values=c("sham"="cornflowerblue",real="red"))+
  facet_grid(.~variable)

## looks like I did not average across proberounds for this plot, fixing:
data.probe.cond |> filter(region=="Angular Gyrus") |> na.omit() |> 
  select(subj, region, stimulation, block, proberound, zlogapen, zlogbv, probe1)|>
  mutate(probe1=as.integer(probe1)) |>
  group_by(subj, stimulation, block) |>
  summarize(across(c(probe1, zlogapen, zlogbv), mean)) |>
  gather(var,val,zlogapen,zlogbv,probe1) |>
  pivot_wider(names_from=c(block,var), values_from=val) |>
  mutate(across(starts_with("B"), as.numeric)) |>
  mutate(B0_ae=0,
         B1_ae=B1_zlogapen-B0_zlogapen,
         B2_ae=B2_zlogapen-B0_zlogapen,
         B3_ae=B3_zlogapen-B0_zlogapen,
         B0_bv=0,
         B1_bv=B1_zlogbv-B0_zlogbv,
         B2_bv=B2_zlogbv-B0_zlogbv,
         B3_bv=B3_zlogbv-B0_zlogbv,
         B0_mw=0,
         B1_mw=-(B1_probe1-B0_probe1), ## x -1 so that more MW is positive
         B2_mw=-(B2_probe1-B0_probe1),
         B3_mw=-(B3_probe1-B0_probe1)) |>
  select(-ends_with("zlogapen"), -ends_with("zlogbv"), -ends_with("probe1")) |>
  pivot_longer(starts_with("B"), names_to = c("block","variable"), names_sep = "_") |>
  mutate(variable=fct_recode(variable, AE="ae",BV="bv",MW="mw")) -> data.probe.cond.diff2


data.probe.cond.diff2 |>
  ggplot(aes(y=value, x=block, color=stimulation))+
  stat_summary(fun.data=mean_se, geom="pointrange", position=position_dodge(width=0.2))+
  stat_summary(fun=mean, geom="line", aes(group=stimulation), position=position_dodge(width=0.2))+
  geom_hline(yintercept = 0, linetype="dashed")+
  scale_color_manual(values=c("sham"="cornflowerblue",real="red"))+
  facet_grid(.~variable)


#' prop MB and spontaneous MW across blocks
#' from stimulation script:
#'   instruction_text = u"To what degree where you focused on the task right before this question?",
#'   scale_labels = [u"Clearly \n NOT FOCUSED", "", "", u"Clearly \n FOCUSED"])
#'   instruction_text = u"To the degree to which you were not focusing on the task, were you thinking about nothing or were you thinking about something?",
#'   scale_labels = [u"Clearly \n NOTHING", "", "", u"Clearly \n SOMETHING"])
#'   instruction_text = u"Were you deliberate about where you focused your attention (either on-task or elsewhere) or did it happen spontaneously?",
#'   scale_labels = [u"Clearly \n SPONTANEOUS", "", "", u"Clearly \n DELIBERATE"])

data.probe.cond |> filter(region=="Angular Gyrus") |> na.omit() |> 
  select(subj, region, stimulation, block, proberound, zlogapen, zlogbv, starts_with("probe")) |>
  mutate(blank=as.integer(probe1<=2 & probe2<=2),
         spontaneous=as.integer(probe1<=2 & probe3<=2)) |>
  select(-probe2,-probe3) |> 
  mutate(probe1=as.integer(probe1)) |>
  group_by(subj, stimulation, block) |>
  summarize(across(c(probe1, blank, spontaneous, zlogapen, zlogbv), mean)) |>
  gather(var,val,zlogapen,zlogbv,probe1,blank,spontaneous) |>
  pivot_wider(names_from=c(block,var), values_from=val) |>
  mutate(across(starts_with("B"), as.numeric)) |>
  mutate(B0_ae=0,
         B1_ae=B1_zlogapen-B0_zlogapen,
         B2_ae=B2_zlogapen-B0_zlogapen,
         B3_ae=B3_zlogapen-B0_zlogapen,
         B0_bv=0,
         B1_bv=B1_zlogbv-B0_zlogbv,
         B2_bv=B2_zlogbv-B0_zlogbv,
         B3_bv=B3_zlogbv-B0_zlogbv,
         B0_mw=0,
         B1_mw=-(B1_probe1-B0_probe1), ## x -1 so that more MW is positive
         B2_mw=-(B2_probe1-B0_probe1),
         B3_mw=-(B3_probe1-B0_probe1),
         B0_mb=0,
         B1_mb=B1_blank-B0_blank,
         B2_mb=B2_blank-B1_blank,
         B3_mb=B3_blank-B2_blank,
         B0_smw=0,
         B1_smw=B1_spontaneous-B0_spontaneous,
         B2_smw=B2_spontaneous-B1_spontaneous,
         B3_smw=B3_spontaneous-B2_spontaneous) |>
  select(-ends_with("zlogapen"), -ends_with("zlogbv"), -ends_with("probe1"), -ends_with("blank"), -ends_with("spontaneous")) |>
  pivot_longer(starts_with("B"), names_to = c("block","variable"), names_sep = "_") |>
  mutate(variable=fct_recode(variable, AE="ae",BV="bv",MW="mw", MB="mb", `spontaneous MW`="smw")) -> data.probe.cond.diff3



data.probe.cond |> filter(region=="Angular Gyrus") |> na.omit() |> 
  select(subj, region, stimulation, block, proberound, zlogapen, zlogbv, starts_with("probe")) |>
  filter(probe1<=2) |> ## only MW probes!
  mutate(blank=as.integer(probe2<=2),
         spontaneous=as.integer(probe3<=2)) |>
  select(-probe1,-probe2,-probe3) |> 
  group_by(subj, stimulation, block) |>
  summarize(across(c(blank, spontaneous), mean)) |>
  gather(var,val,blank,spontaneous) |>
  pivot_wider(names_from=c(block,var), values_from=val) |>
  mutate(across(starts_with("B"), as.numeric)) |>
  mutate(B0_mb=0,
         B1_mb=B1_blank-B0_blank,
         B2_mb=B2_blank-B1_blank,
         B3_mb=B3_blank-B2_blank,
         B0_smw=0,
         B1_smw=B1_spontaneous-B0_spontaneous,
         B2_smw=B2_spontaneous-B1_spontaneous,
         B3_smw=B3_spontaneous-B2_spontaneous) |>
  select(-ends_with("zlogapen"), -ends_with("zlogbv"), -ends_with("probe1"), -ends_with("blank"), -ends_with("spontaneous")) |>
  pivot_longer(starts_with("B"), names_to = c("block","variable"), names_sep = "_") |>
  mutate(variable=fct_recode(variable, MB="mb", `spontaneous MW`="smw")) -> data.probe.cond.diff4



data.probe.cond.diff3 |>
  filter(!(variable %in% c("MB", "spontaneous MW"))) |>
  ggplot(aes(y=value, x=block, color=stimulation))+
  stat_summary(fun.data=mean_se, geom="pointrange", position=position_dodge(width=0.2))+
  stat_summary(fun=mean, geom="line", aes(group=stimulation), position=position_dodge(width=0.2))+
  geom_hline(yintercept = 0, linetype="dashed")+
  scale_color_manual(values=c("sham"="cornflowerblue",real="red"))+
  labs(y=latex2exp::TeX("$\\Delta$ Z-score"))+
  facet_grid(.~variable)+theme(legend.position="none") -> p1


data.probe.cond.diff4 |>
  filter(variable %in% c("MB", "spontaneous MW")) |>
  ggplot(aes(y=value, x=block, color=stimulation))+
  stat_summary(fun.data=mean_se, geom="pointrange", position=position_dodge(width=0.2))+
  stat_summary(fun=mean, geom="line", aes(group=stimulation), position=position_dodge(width=0.2))+
  geom_hline(yintercept = 0, linetype="dashed")+
  scale_color_manual(values=c("sham"="cornflowerblue",real="red"))+
  labs(y=latex2exp::TeX("$\\Delta$ proportion"))+
  facet_grid(.~variable) -> p2

library(patchwork)
pobj <- (p1+p2+plot_layout(widths = c(3, 2)))
ggsave(filename = "graphs/paper_ag_summary.pdf", width=10, height=3)
  
##' Quantify differences using the Bayesian regression models
##' 
library(brms)
library(bayesplot)
library(cmdstanr)

#' Model for AG only
#' 
#' ====== MW =======
data.probe.cond.ag <- data.probe.cond |> filter(region=="Angular Gyrus")

mod.ag.mw.behav <- brm(probe1 ~ stimulation + block*stimulation + zlogapen * zlogbv + scale(proberound) + (1|subj), 
                 init=0, family=cumulative("probit"), data=data.probe.cond.ag, 
                 backend = "cmdstanr", chains = 6, iter=3000)
summary(mod.ag.mw.behav)

mod.ag.mw <- brm(probe1 ~ stimulation + block*stimulation + scale(proberound) + (1|subj), 
                 init=0, family=cumulative("probit"), data=data.probe.cond.ag, 
                 backend = "cmdstanr", chains = 6, iter=3000)
brms::rhat(mod.ag.mw) %>% max(na.rm=T)
summary(mod.ag.mw)
pars=setdiff(variables(mod.ag.mw), c("disc","lp__", "lprior"))
gpars=pars[!str_detect(pars, "(r_.+)|(z_.*)|(Intercept.*)")]
mcmc_intervals(as.matrix(mod.ag.mw), pars=gpars, prob_outer = 0.95)+
  geom_vline(xintercept = 0, linetype="dashed")+labs(title="MW")


#' MW model with common baseline in B0
data.probe.cond.ag2 <- data.probe.cond |> filter(region=="Angular Gyrus") |>
  mutate(blockB1=as.integer(block=="B1"),
         blockB2=as.integer(block=="B2"),
         blockB3=as.integer(block=="B3"),
         zproberound=scale(proberound))
mod.ag.mw2 <- brm(probe1 ~ blockB1 + blockB1:stimulation + blockB2 + blockB2:stimulation + blockB3 + blockB3:stimulation + 
                    zproberound + (1|subj), 
                 init=0, family=cumulative("probit"), data=data.probe.cond.ag2, backend = "cmdstanr", chains = 6, iter=3000)

brms::rhat(mod.ag.mw2) %>% max(na.rm=T)
summary(mod.ag.mw2)
pars=setdiff(variables(mod.ag.mw2), c("disc","lp__", "lprior"))
gpars=pars[!str_detect(pars, "(r_.+)|(z_.*)|(Intercept.*)")]
mcmc_intervals(as.matrix(mod.ag.mw2), pars=gpars)+
  geom_vline(xintercept = 0, linetype="dashed")+labs(title="MW")


mod.ag.mw |> add_criterion(criterion = c("loo","bayes_R2")) -> mod.ag.mw
mod.ag.mw2 |> add_criterion(criterion = c("loo","bayes_R2")) -> mod.ag.mw2
loo_compare(mod.ag.mw, mod.ag.mw2)


#' ====== MB + spontaneous =========

data.probe.cond.ag |> 
  filter(probe1<=2) |>
  mutate(zproberound=scale(proberound)) |>
  mutate(MB=probe2<=2,
         sMW=probe3<=2) -> dd

## MB logistic regression
mod.ag.mb <- brm(MB ~ stimulation + block*stimulation + zproberound + (1|subj), 
                 init=0, data=dd, family=bernoulli(link="logit"),  
                 backend = "cmdstanr", chains = 6, iter=3000)
brms::rhat(mod.ag.mb) %>% max(na.rm=T)
summary(mod.ag.mb)
pars=setdiff(variables(mod.ag.mb), c("disc","lp__", "lprior"))
gpars=pars[!str_detect(pars, "(r_.+)|(z_.*)|(Intercept.*)")]

mcmc_intervals(as.matrix(mod.ag.mb), pars=gpars, prob_outer = 0.95)+
  geom_vline(xintercept = 0, linetype="dashed")+labs(title="MB")

## spontaneous logistic regression
mod.ag.smw <- brm(sMW ~ stimulation + block*stimulation + zproberound + (1|subj), 
                 init=0, data=dd, family=bernoulli(link="logit"),  
                 backend = "cmdstanr", chains = 6, iter=3000)
brms::rhat(mod.ag.smw) %>% max(na.rm=T)
summary(mod.ag.smw)
pars=setdiff(variables(mod.ag.smw), c("disc","lp__", "lprior"))
gpars=pars[!str_detect(pars, "(r_.+)|(z_.*)|(Intercept.*)")]

mcmc_intervals(as.matrix(mod.ag.smw), pars=gpars, prob_outer = 0.95)+
  geom_vline(xintercept = 0, linetype="dashed")+labs(title="SMW")


#' ====== AE =======
mod.ag.ae <- brm(zlogapen ~ stimulation + block*stimulation + scale(proberound) + (1|subj), 
                 init=0, data=data.probe.cond.ag, backend = "cmdstanr", chains = 6, iter=3000)
brms::rhat(mod.ag.ae) %>% max(na.rm=T)
summary(mod.ag.ae)
mcmc_intervals(as.matrix(mod.ag.ae), pars=gpars, prob_outer = 0.95)+
  geom_vline(xintercept = 0, linetype="dashed")+labs(title="AE")

#' ====== BV =======
mod.ag.bv <- brm(zlogbv ~ stimulation + block*stimulation + scale(proberound) + (1|subj), 
                 init=0, data=data.probe.cond.ag, backend = "cmdstanr", chains = 6, iter=3000)
brms::rhat(mod.ag.bv) %>% max(na.rm=T)
summary(mod.ag.bv)
mcmc_intervals(as.matrix(mod.ag.bv), pars=gpars, prob_outer = 0.95)+
  geom_vline(xintercept = 0, linetype="dashed")+labs(title="BV")

mod.ag.ae  <- brms::add_criterion(mod.ag.ae,  criterion = c("bayes_R2", "loo"))
mod.ag.bv  <- brms::add_criterion(mod.ag.bv,  criterion = c("bayes_R2", "loo"))
mod.ag.mw  <- brms::add_criterion(mod.ag.mw,  criterion = c("bayes_R2", "loo"))
mod.ag.mb  <- brms::add_criterion(mod.ag.mb,  criterion = c("bayes_R2", "loo"))
mod.ag.smw <- brms::add_criterion(mod.ag.smw, criterion = c("bayes_R2", "loo"))


save(mod.ag.ae, mod.ag.bv, mod.ag.mw, mod.ag.mb, mod.ag.smw, file="data/export/paper_vars.RData")
load("data/export/paper_vars.RData")

## plot from models
library(tidybayes)

as.data.frame(mod.ag.mw) |> 
  select(starts_with("b_block"), starts_with("b_stimulation")) |> 
  mutate(B0_mw_sham=0,
         B0_mw_real=b_stimulationreal,
         B1_mw_sham=b_blockB1,
         B1_mw_real=b_blockB1+`b_stimulationreal:blockB1`,
         B2_mw_sham=b_blockB2,
         B2_mw_real=b_blockB2+`b_stimulationreal:blockB2`,
         B3_mw_sham=b_blockB3,
         B3_mw_real=b_blockB3+`b_stimulationreal:blockB3`) |> 
  select(starts_with("B", ignore.case=F)) |>
  gather(var,val) |>
  mutate(val=-1*val) |>
  separate(var, c("block","var","stimulation"), sep="_") |>
  ggplot(aes(x=block, y=val, color=stimulation))+
  stat_summary(fun.data=mean_qi, geom="pointrange", position=position_dodge(width=0.2))+
  stat_summary(fun=mean, geom="line", aes(group=stimulation), position=position_dodge(width=0.2))


as.data.frame(mod.ag.mw2) |> 
  select(starts_with("b_block"), starts_with("b_stimulation")) |> 
  mutate(B0_mw_sham=0,
         B0_mw_real=0,
         B1_mw_sham=b_blockB1,
         B1_mw_real=b_blockB1+`b_blockB1:stimulationreal`,
         B2_mw_sham=b_blockB2,
         B2_mw_real=b_blockB2+`b_blockB2:stimulationreal`,
         B3_mw_sham=b_blockB3,
         B3_mw_real=b_blockB3+`b_blockB3:stimulationreal`) |> 
  select(starts_with("B", ignore.case=F)) |>
  gather(var,val) |>
  mutate(val=-1*val) |>
  separate(var, c("block","var","stimulation"), sep="_") |>
  ggplot(aes(x=block, y=val, color=stimulation))+
  stat_summary(fun.data=mean_qi, geom="pointrange", position=position_dodge(width=0.2))+
  stat_summary(fun=mean, geom="line", aes(group=stimulation), position=position_dodge(width=0.2))
         



## simple t-tests
with(filter(data.probe.cond.diff, variable=="MW", block=="B1"),
     t.test(value ~ stimulation, paired=T))
with(filter(data.probe.cond.diff, variable=="MW", block=="B2"),
     t.test(value ~ stimulation, paired=T))
with(filter(data.probe.cond.diff, variable=="MW", block=="B3"),
     t.test(value ~ stimulation, paired=T))

with(filter(data.probe.cond.diff, variable=="AE", block=="B1"),
     t.test(value ~ stimulation, paired=T))
with(filter(data.probe.cond.diff, variable=="AE", block=="B2"),
     t.test(value ~ stimulation, paired=T))
with(filter(data.probe.cond.diff, variable=="AE", block=="B3"),
     t.test(value ~ stimulation, paired=T))

with(filter(data.probe.cond.diff, variable=="BV", block=="B1"),
     t.test(value ~ stimulation, paired=T))
with(filter(data.probe.cond.diff, variable=="BV", block=="B2"),
     t.test(value ~ stimulation, paired=T))
with(filter(data.probe.cond.diff, variable=="BV", block=="B3"),
     t.test(value ~ stimulation, paired=T))




library(ProjectTemplate)
load.project()

data |>
  group_by(subj,session,block) |> 
  summarize(nbeep=sum(stimulus=="stimulus"),
            ntap=sum(response %in% c("lctrl", "rctrl")),
            ntapleft=sum(response %in% c("lctrl")),
            ntapright=sum(response %in% c("rctrl")),
            nprobes=sum(str_starts(stimulus,"probe")),
            ntapother=sum(!is.na(response))-ntap-nprobes,
            mprobe1=mean(as.integer(response[stimulus=="probe1"])),
            mprobe2=mean(as.integer(response[stimulus=="probe2"])),
            mprobe3=mean(as.integer(response[stimulus=="probe3"]))) |> View()

nback=25
data |> group_by(subj,session,block,proberound) |> do({
  d <- .
  
  ## unravel taps (keys)
  taps=d$response[d$stimulus=="tap"]
  taps=factor(taps, levels=c("lctrl","rctrl"), labels=c("left","right"))
  taps=as.integer(taps)-1
  taps=tail(taps, nback)
  
  iti=diff(d$time[d$stimulus=="tap"]) |> tail(nback-1) 
  
  probe1.resp=as.integer(d$response[d$stimulus=="probe1"])+1
  probe2.resp=as.integer(d$response[d$stimulus=="probe2"])+1
  probe3.resp=as.integer(d$response[d$stimulus=="probe3"])+1

  ## return summary of it
  data.frame(
    probe1=probe1.resp,
    probe2=probe2.resp,
    probe3=probe3.resp,
    apen=apen_int(taps, 2)[3],
    bv=sd(iti)
  )
}) |> ungroup() |>  
  mutate(probe1=ordered(probe1, levels=1:4),
         probe2=ordered(probe2, levels=1:4),
         probe3=ordered(probe3, levels=1:4),
         logapen=-log(log(2)-apen),
         logbv=log(bv),
         zlogapen=(logapen-mean(logapen,na.rm=T))/sd(logapen,na.rm=T),
         zlogbv=(logbv-mean(logbv,na.rm=T))/sd(logbv,na.rm=T)) -> data.probe


#' put together with randomization list
#' 
randlist.ag <- read_csv("data/export/randlist_ag_Nyp7ObM.csv", comment = "#")
randlist.pfc <- read_csv("data/export/randlist_pfc_Nyp7ObM.csv", comment="#")
left_join(data.probe, bind_rows(randlist.ag, randlist.pfc) ) |>
  mutate(stimulation=if_else(session=="S1", session1, session2),
         stimulation=factor(stimulation, levels=c("sham","real"))) -> data.probe.cond

data.probe.cond |> group_by(region,session) |>
  summarize(length(unique(subj)))


data.probe.cond |> group_by(region,session) |>
  summarize(unique(subj)) |> data.frame()


#' Bayesian model (combining regions)
#' 
library(brms)
library(bayesplot)
mod <- brm(probe1 ~ zlogapen*zlogbv + stimulation + block*stimulation + scale(proberound) + (1|subj), 
           init=0, family=cumulative("probit"), data=data.probe.cond)
brms::rhat(mod) %>% max(na.rm=T)
summary(mod)
pars=setdiff(variables(mod), c("disc","lp__"))
gpars=pars[!str_detect(pars, "r_")][!str_detect(pars, "Intercept")]

mcmc_intervals(as.matrix(mod), pars=gpars)+geom_vline(xintercept = 0, linetype="dashed")
conditional_effects(mod)

#' Model for AG only
#' 
data.probe.cond.ag <- data.probe.cond |> filter(region=="Angular Gyrus")
mod.ag <- brm(probe1 ~ zlogapen*zlogbv + stimulation + block*stimulation + scale(proberound) + (1|subj), 
              init=0, family=cumulative("probit"), data=data.probe.cond.ag)
brms::rhat(mod.ag) %>% max(na.rm=T)
summary(mod.ag)
pars=setdiff(variables(mod.ag), c("disc","lp__"))
gpars=pars[!str_detect(pars, "r_")][!str_detect(pars, "Intercept")]

mcmc_intervals(as.matrix(mod.ag), pars=gpars)+geom_vline(xintercept = 0, linetype="dashed")
conditional_effects(mod.ag)

#' Model for PFC only
#' 
data.probe.cond.pfc <- data.probe.cond |> filter(region=="Prefrontal Cortex")
mod.pfc <- brm(probe1 ~ zlogapen*zlogbv + stimulation + block*stimulation + scale(proberound) + (1|subj), 
              init=0, family=cumulative("probit"), data=data.probe.cond.pfc)
brms::rhat(mod.pfc) %>% max(na.rm=T)
summary(mod.pfc)
pars=setdiff(variables(mod.pfc), c("disc","lp__"))
gpars=pars[!str_detect(pars, "r_")][!str_detect(pars, "Intercept")]

mcmc_intervals(as.matrix(mod.pfc), pars=gpars)+geom_vline(xintercept = 0, linetype="dashed")
conditional_effects(mod.pfc)

#' BV model
mod.bv <- brm(zlogbv ~ block*region*stimulation + (1|subj), init=0, data=data.probe.cond)
brms::rhat(mod.bv) %>% max(na.rm=T)
summary(mod.bv)
pars=setdiff(variables(mod.bv), c("disc","lp__"))
gpars=pars[!str_detect(pars, "r_")][!str_detect(pars, "Intercept")]

mcmc_intervals(as.matrix(mod.bv), pars=gpars)+geom_vline(xintercept = 0, linetype="dashed")
conditional_effects(mod.bv)

data.probe.cond |> 
  ggplot(aes(y=zlogbv, x=block, color=stimulation))+
  stat_summary(fun.data=mean_se, geom="pointrange")+
  stat_summary(fun=mean, geom="line", mapping=aes(group=stimulation))+
  facet_wrap(~region)

data.probe.cond |> 
  ggplot(aes(y=zlogapen, x=block, color=stimulation))+
  stat_summary(fun.data=mean_se, geom="pointrange")+
  stat_summary(fun=mean, geom="line", mapping=aes(group=stimulation))+
  facet_wrap(~region)


data.probe.cond |>
  select(subj, region, stimulation, block, proberound, zlogapen, zlogbv, probe1)|>
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
         B1_mw=B1_probe1-B0_probe1,
         B2_mw=B2_probe1-B0_probe1,
         B3_mw=B3_probe1-B0_probe1) |>
  select(-ends_with("zlogapen"), -ends_with("zlogbv"), -ends_with("probe1")) |>
  pivot_longer(starts_with("B"), names_to = c("block","variable"), names_sep = "_") |>
  mutate(variable=fct_recode(variable, AE="ae",BV="bv",MW="mw")) -> data.probe.cond.diff

data.probe.cond.diff |>
  ggplot(aes(y=value, x=block, color=stimulation))+
  stat_summary(fun.data=mean_se, geom="pointrange", position=position_dodge(width=0.2))+
  stat_summary(fun=mean, geom="line", aes(group=stimulation), position=position_dodge(width=0.2))+
  geom_hline(yintercept = 0, linetype="dashed")+
  facet_grid(region~variable)


#' AE and BV over blocks split by sham vs. real and region
#' 
data.probe.cond |>
  gather(var,val,zlogapen,zlogbv) |>
  ggplot(aes(x=block,y=val,color=stimulation))+
  stat_summary(fun.data = mean_cl_boot, geom="pointrange", position=position_dodge(width=0.2))+
#  stat_summary(fun=mean, geom="line", mapping=aes(group=subj), color="grey")+
  stat_summary(fun=mean, geom="line", mapping=aes(group=stimulation), position=position_dodge(width=0.2))+
  facet_grid(region~var, scales="free")+
  labs()

#' change from B0
#' 
data.probe.cond |>
  select(subj,stimulation,block,zlogapen,zlogbv)





data.probe.cond |>
  ggplot(aes(x=block,y=as.integer(probe1),color=stimulation))+
  stat_summary(fun.data = mean_cl_boot, geom="pointrange")+
  stat_summary(fun.y=mean, geom="line", mapping=aes(group=subj), color="grey")+
  stat_summary(fun.y=mean, geom="line", mapping=aes(group=stimulation))+
  #facet_wrap(~stimulation)+
  labs(title="Probe 1")


data.probe.cond |>
  ggplot(aes(x=block,y=as.integer(probe2)))+
  stat_summary(fun.data = mean_cl_boot, geom="pointrange")+
  stat_summary(fun.y=mean, geom="line", mapping=aes(group=subj), color="grey")+
  stat_summary(fun.y=mean, geom="line", mapping=aes(group=stimulation))+
  facet_wrap(~stimulation)+
  labs(title="Probe 2")


data.probe.cond |>
  ggplot(aes(x=block,y=as.integer(probe3)))+
  stat_summary(fun.data = mean_cl_boot, geom="pointrange")+
  stat_summary(fun.y=mean, geom="line", mapping=aes(group=subj), color="grey")+
  stat_summary(fun.y=mean, geom="line", mapping=aes(group=stimulation))+
  facet_wrap(~stimulation)+
  labs(title="Probe 3")


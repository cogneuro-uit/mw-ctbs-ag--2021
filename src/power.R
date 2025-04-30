#' Power-analysis for the 2 (stimulation: sham vs. real) x 4 (block) IA effect
#' 
#'
library(tidyverse) # you might have to install these packages
library(afex) # for the ANOVA
library(pbapply)

N=40
nblocks=4
eff.block=0.5 # how much does MW increase from block to block in the "sham" condition?
eff.ia=-0.3 # hoch much is the eff.block effect decreased during the active condition?



## We make a "data-generator" function which corresponds to running one full study
# This generator function can be used to test different values for the effect-sizes and sample size
generate_data <- function(N, eff.block, eff.ia){
  ## experimental design: each subject goes through 4 blocks at two different times (sham vs. real)
  d <- tibble(subj=rep(1:N, each=nblocks*2),
              block=rep(1:nblocks, N*2),
              stimulation=rep(rep(c("sham","real"),each=nblocks), N),
              
              # this is the mean MW score during each of the blocks and stimulation conditions
              MW.mean=eff.block*(block-1) + eff.ia*((block-1)*as.integer(stimulation=="real")),
              
              # this is the "observed" data (we set the SD to 1 so that we have standardized effects)
              MW=rnorm(N*nblocks*2, mean=MW.mean, sd=1)
  )
  
  # run an ANOVA with DV=MW and within-factors block and stimulation 
  mod <- aov.car(MW ~ block*stimulation + Error(subj/(block*stimulation)), data=d)
  
  # extract the p-value and the effect-size "ges": Generalized Eta-Squared measure of effect size (Bakeman, 2005).
  results <- tibble(
    p=mod$anova_table$`Pr(>F)`[3],
    ges=mod$anova_table$ges[3]
  )
  return(results);
}

ggplot(d, aes(x=stimulation, y=MW, color=factor(block)))+
  stat_summary(fun.data=mean_cl_boot, geom="pointrange", position=position_dodge(width=0.2))

## you can use this function generate a single result (p-val and ES )
generate_data(N, eff.block, eff.ia)

## now we run it many times (nrep times)
nrep=1000 # that's the number of replications; the higher the number the longer 
         # the sim takes and the more accurate is the solution
         # I would go with nrep=1000 or so
cores=8 # that the number of CPUs to use in order to speed things up a bit
        # if its way to slow, we can go to our server that has 80 cores

result <- pblapply(1:nrep, \(x) generate_data(N, eff.block, eff.ia), cl=cores)  |>
      map_df(~ .x)
mean(result$p < 0.05) # this is the power of the study (number of times p<0.05 divided by nrep)
mean(result$ges) # average effect size across samples


# We can also calculate power as a function of the effect size
eff.ias = -1*c(0.1, 0.2, 0.3, 0.4, 0.5) ## a list of possible IA effect sizes

eff.ias = seq(-0.5, 0, by=0.05) #-1*c(0.1, 0.2, 0.3, 0.4, 0.5) ## a list of possible IA effect sizes

# will take 5 x as long as the above simulation!
result <- map_df(eff.ias, \(ES){
  result <- pblapply(1:nrep, \(x) generate_data(N, eff.block, ES), cl=cores)  |>
    map_df(~ .x)
  tibble(ES=ES,
         power=mean(result$p<0.05),
         ges=mean(result$ges))
})

# graph: power as function of ES
ggplot(result, aes(x=ges, y=power))+
  geom_point()+geom_line()+geom_hline(yintercept = 0.8, color="grey")

## interpolate to get minimum effect size with 80% power
with(result, approx(power, ges, xout=0.8))


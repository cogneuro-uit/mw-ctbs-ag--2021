library(ProjectTemplate)
load.project()

pilot1 |> group_by(subj,day,block) |> 
  summarize(nbeep=sum(stimulus=="stimulus"),
            ntap=sum(response %in% c("lctrl", "rctrl")),
            ntapleft=sum(response %in% c("lctrl")),
            ntapright=sum(response %in% c("rctrl")),
            nprobes=sum(str_starts(stimulus,"probe")),
            ntapother=sum(!is.na(response))-ntap-nprobes,
            mprobe1=mean(as.integer(response[stimulus=="probe1"])),
            mprobe2=mean(as.integer(response[stimulus=="probe2"])),
            mprobe3=mean(as.integer(response[stimulus=="probe3"])))





nback=25
behavioural |>  group_by(subj, part) |> do({
  d <- .
  blocklen=diff(c(0, which(d$stimulus=="probe1")))
  block=rep(1:length(blocklen), blocklen)
  d |> mutate(block=block)
}) |> group_by(subj,part,block) |> do({
  d <- .
  
  ## unravel taps (keys)
  taps=d$response[d$stimulus=="tap"]
  taps=as.integer(factor(taps, levels=c("z","m"), labels=c("left","right")))-1
  taps=tail(taps, nback)
  
  iti=diff(d$time[d$stimulus=="tap"]) |> tail(nback-1) 
  
  probe.resp=as.integer(tail(d,1)$response)+1
  ## return summary of it
  data.frame(
    probe=probe.resp,
    apen=apen_int(taps, 2)[3],
    bv=sd(iti)
  )
}) |> ungroup() |>  
  mutate(probe=ordered(probe, levels=1:4),
         logapen=-log(log(2)-apen),
         probenum=block/20,
         logbv=log(bv),
         zlogapen=(logapen-mean(logapen,na.rm=T))/sd(logapen,na.rm=T),
         zlogbv=(logbv-mean(logbv,na.rm=T))/sd(logbv,na.rm=T)) |>
  full_join(groups, by="subj") -> behaviour.probe
  

                
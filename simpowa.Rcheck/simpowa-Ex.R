pkgname <- "simpowa"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "simpowa-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('simpowa')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("sim.mosq.longsfe.comint")
### * sim.mosq.longsfe.comint

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sim.mosq.longsfe.comint
### Title: Simulate Mosquito Count Data for Long-Term Semi-Field Experiment
###   Testing Combined Interventions
### Aliases: sim.mosq.longsfe.comint

### ** Examples

sim.mosq.longsfe.comint(
  n.ch.per.trt = 4,
  exp.length = 90,
  sampl.freq = "weekly",
  lambda = 10,
  intvn1.effect = 0.7,
  intvn2.effect = 0.8,
  ixn.effect = 0.8,
  chamber.var = 0.1807,
  time.var = 0.2266,
  theta = 10,
  use.random = "ALL"
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sim.mosq.longsfe.comint", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sim.mosq.longsfe.sinint")
### * sim.mosq.longsfe.sinint

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sim.mosq.longsfe.sinint
### Title: Simulate Mosquito Count Data for Long-Term Semi-Field Experiment
###   Testing Single Intervention
### Aliases: sim.mosq.longsfe.sinint

### ** Examples

sim.mosq.longsfe.sinint(
  n.ch.per.trt = 4,
  exp.length = 90,
  sampl.freq = "weekly",
  lambda = 10,
  intvn.effect = 0.8,
  chamber.var = 0.1807,
  time.var = 0.2266,
  theta = 10,
  use.random = "ALL"
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sim.mosq.longsfe.sinint", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sim.mosq.shortsfe.comint")
### * sim.mosq.shortsfe.comint

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sim.mosq.shortsfe.comint
### Title: Simulate Mosquito Count Data for Short-Term Semi-Field
###   Experiment Testing Combined Interventions
### Aliases: sim.mosq.shortsfe.comint

### ** Examples

sim.mosq.shortsfe.comint(
  n.ch.per.trt = 4,
  lambda = 50,
  intvn1.effect = 0.7,
  intvn2.effect = 0.8,
  ixn.effect = 0.5,
  chamber.var = 0.1807,
  use.random = "ALL"
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sim.mosq.shortsfe.comint", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sim.mosq.shortsfe.sinint")
### * sim.mosq.shortsfe.sinint

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sim.mosq.shortsfe.sinint
### Title: Simulate Mosquito Count Data for Short-Term Semi-Field
###   Experiment Testing Single Intervention
### Aliases: sim.mosq.shortsfe.sinint

### ** Examples

sim.mosq.shortsfe.sinint(
  n.ch.per.trt = 4,
  lambda = 50,
  intvn.effect = 0.8,
  chamber.var = 0.1807,
  use.random = "ALL"
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sim.mosq.shortsfe.sinint", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sim.plot.longsfe.comint")
### * sim.plot.longsfe.comint

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sim.plot.longsfe.comint
### Title: Plot Mosquito Counts for Long-Term Semi-Field Experiment Testing
###   Combined Interventions
### Aliases: sim.plot.longsfe.comint

### ** Examples

sim.plot.longsfe.comint(
  n.ch.per.trt = 4,
  exp.length = 90,
  sampl.freq = "weekly",
  lambda = 10,
  intvn1.effect = 0.7,
  intvn2.effect = 0.8,
  ixn.effect = 0.8,
  chamber.var = 0.1807,
  time.var = 0.2266,
  theta = 10,
  use.random = TRUE
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sim.plot.longsfe.comint", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sim.plot.longsfe.sinint")
### * sim.plot.longsfe.sinint

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sim.plot.longsfe.sinint
### Title: Plot Mosquito Counts for Long-Term Semi-Field Experiment Testing
###   Single Intervention
### Aliases: sim.plot.longsfe.sinint

### ** Examples

sim.plot.longsfe.sinint(
  n.ch.per.trt = 4,
  exp.length = 90,
  sampl.freq = "weekly",
  lambda = 10,
  intvn.effect = 0.8,
  chamber.var = 0.1807,
  time.var = 0.2266,
  theta = 10,
  use.random = TRUE
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sim.plot.longsfe.sinint", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sim.plot.shortsfe.comint")
### * sim.plot.shortsfe.comint

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sim.plot.shortsfe.comint
### Title: Plot Mosquito Counts for Short-Term Semi-Field Experiment
###   Testing Combined Interventions
### Aliases: sim.plot.shortsfe.comint

### ** Examples

sim.plot.shortsfe.comint(
  n.ch.per.trt = 4,
  lambda = 50,
  intvn1.effect = 0.7,
  intvn2.effect = 0.8,
  ixn.effect = 0.5,
  chamber.var = 0.1807,
  use.random = TRUE,
  jitter = TRUE
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sim.plot.shortsfe.comint", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sim.plot.shortsfe.sinint")
### * sim.plot.shortsfe.sinint

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sim.plot.shortsfe.sinint
### Title: Plot Mosquito Counts from Short-Term Semi-Field Experiment
###   Testing Single Intervention
### Aliases: sim.plot.shortsfe.sinint

### ** Examples

sim.plot.shortsfe.sinint(
  n.ch.per.trt = 4,
  lambda = 50,
  intvn.effect = 0.8,
  chamber.var = 0.1807,
  use.random = TRUE,
  jitter = TRUE
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sim.plot.shortsfe.sinint", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sim.power.longsfe.comint")
### * sim.power.longsfe.comint

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sim.power.longsfe.comint
### Title: Estimate Empirical Power for Long-Term Semi-Field Experiment
###   Testing Combined Interventions
### Aliases: sim.power.longsfe.comint

### ** Examples

# A short fast runnable example (nsim = 2)
sim.power.longsfe.comint(
  n.ch.per.trt = 4,
  exp.length = 90,
  sampl.freq = "weekly",
  lambda = 10,
  intvn1.effect = 0.7,
  intvn2.effect = 0.8,
  ixn.effect = 0.8,
  chamber.var = 0.1807,
  time.var = 0.2266,
  theta = 10,
  nsim = 2,
  n.cores = 1,
  use.random = TRUE
)

## No test: 
# Longer simulation (nsim = 100)
sim.power.longsfe.comint(
  n.ch.per.trt = 4,
  exp.length = 90,
  sampl.freq = "weekly",
  lambda = 10,
  intvn1.effect = 0.7,
  intvn2.effect = 0.8,
  ixn.effect = 0.8,
  chamber.var = 0.1807,
  time.var = 0.2266,
  theta = 10,
  nsim = 100,
  n.cores = 1,
  use.random = TRUE
)
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sim.power.longsfe.comint", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sim.power.longsfe.sinint")
### * sim.power.longsfe.sinint

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sim.power.longsfe.sinint
### Title: Estimate Empirical Power for Long-Term Semi-Field Experiment
###   Testing Single Intervention
### Aliases: sim.power.longsfe.sinint

### ** Examples

# A short fast runnable example (nsim = 2)
sim.power.longsfe.sinint(
  n.ch.per.trt = 4,
  exp.length = 90,
  sampl.freq = "weekly",
  lambda = 10,
  intvn.effect = 0.8,
  chamber.var = 0.1807,
  time.var = 0.2266,
  theta = 10,
  nsim = 2,
  n.cores = 1,
  use.random = TRUE
)

## No test: 
# Longer simulation (nsim = 100)
sim.power.longsfe.sinint(
  n.ch.per.trt = 4,
  exp.length = 90,
  sampl.freq = "weekly",
  lambda = 10,
  intvn.effect = 0.8,
  chamber.var = 0.1807,
  time.var = 0.2266,
  theta = 10,
  nsim = 100,
  n.cores = 1,
  use.random = TRUE
)
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sim.power.longsfe.sinint", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sim.power.shortsfe.comint")
### * sim.power.shortsfe.comint

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sim.power.shortsfe.comint
### Title: Estimate Empirical Power for Short-Term Semi-Field Experiment
###   Testing Combined Intervention
### Aliases: sim.power.shortsfe.comint

### ** Examples

# A short fast runnable example (nsim = 2)
sim.power.shortsfe.comint(
  n.ch.per.trt = 4,
  lambda = 50,
  intvn1.effect = 0.7,
  intvn2.effect = 0.8,
  ixn.effect = 0.5,
  chamber.var = 0.1807,
  nsim = 2,
  n.cores = 1,
  use.random = TRUE
)

## No test: 
# Longer simulation (nsim = 100)
sim.power.shortsfe.comint(
  n.ch.per.trt = 4,
  lambda = 50,
  intvn1.effect = 0.7,
  intvn2.effect = 0.8,
  ixn.effect = 0.5,
  chamber.var = 0.1807,
  nsim = 100,
  n.cores = 1,
  use.random = TRUE
)
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sim.power.shortsfe.comint", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sim.power.shortsfe.sinint")
### * sim.power.shortsfe.sinint

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sim.power.shortsfe.sinint
### Title: Estimate Empirical Power for Short-Term Semi-Field Experiment
###   Testing Single Intervention
### Aliases: sim.power.shortsfe.sinint

### ** Examples

# A short fast runnable example (nsim = 2)
sim.power.shortsfe.sinint(
  n.ch.per.trt = 4,
  lambda = 50,
  intvn.effect = 0.8,
  chamber.var = 0.1807,
  nsim = 2,
  n.cores = 1,
  use.random = TRUE
)

## No test: 
# Longer simulation (nsim = 100)
sim.power.shortsfe.sinint(
  n.ch.per.trt = 4,
  lambda = 50,
  intvn.effect = 0.8,
  chamber.var = 0.1807,
  nsim = 100,
  n.cores = 1,
  use.random = TRUE
)
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sim.power.shortsfe.sinint", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sim.pval.longsfe.comint")
### * sim.pval.longsfe.comint

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sim.pval.longsfe.comint
### Title: Extract p-value from Simulated GLMM for Long-Term Semi-Field
###   Experiment Testing Combined Interventions
### Aliases: sim.pval.longsfe.comint

### ** Examples

sim.pval.longsfe.comint(
  n.ch.per.trt = 4,
  exp.length = 90,
  sampl.freq = "weekly",
  lambda = 10,
  intvn1.effect = 0.7,
  intvn2.effect = 0.8,
  ixn.effect = 0.8,
  chamber.var = 0.1807,
  time.var = 0.2266,
  theta = 10,
  use.random = TRUE
)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sim.pval.longsfe.comint", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sim.pval.longsfe.sinint")
### * sim.pval.longsfe.sinint

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sim.pval.longsfe.sinint
### Title: Extract p-value from Simulated GLMM for Long-Term Semi-Field
###   Experiment Testing Single Intervention
### Aliases: sim.pval.longsfe.sinint

### ** Examples

sim.pval.longsfe.sinint(
  n.ch.per.trt = 4,
  exp.length = 90,
  sampl.freq = "weekly",
  lambda = 10,
  intvn.effect = 0.8,
  chamber.var = 0.1807,
  time.var = 0.2266,
  theta = 10,
  use.random = TRUE
)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sim.pval.longsfe.sinint", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sim.pval.shortsfe.comint")
### * sim.pval.shortsfe.comint

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sim.pval.shortsfe.comint
### Title: Extract p-value from Simulated GLMM for Short-Term Semi-Field
###   Experiment Testing Combined Interventions
### Aliases: sim.pval.shortsfe.comint

### ** Examples

sim.pval.shortsfe.comint(
  n.ch.per.trt = 4,
  lambda = 50,
  intvn1.effect = 0.7,
  intvn2.effect = 0.8,
  ixn.effect = 0.5,
  chamber.var = 0.1807,
  use.random = TRUE
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sim.pval.shortsfe.comint", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sim.pval.shortsfe.sinint")
### * sim.pval.shortsfe.sinint

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sim.pval.shortsfe.sinint
### Title: Extract p-values from Simulated GLMM for Short-Term Semi-Field
###   Experiment Testing Single Intervention
### Aliases: sim.pval.shortsfe.sinint

### ** Examples

sim.pval.shortsfe.sinint(
  n.ch.per.trt = 4,
  lambda = 50,
  intvn.effect = 0.8,
  chamber.var = 0.1807,
  use.random = TRUE
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sim.pval.shortsfe.sinint", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sim.scen.longsfe.comint")
### * sim.scen.longsfe.comint

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sim.scen.longsfe.comint
### Title: Simulate Design Scenarios for Long-Term Semi-Filed Experiment
###   Testing Combined Interventions
### Aliases: sim.scen.longsfe.comint

### ** Examples

sim.scen.longsfe.comint(
  n.ch.per.trt = 4,
  exp.length = 90,
  sampl.freq = "weekly"
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sim.scen.longsfe.comint", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sim.scen.longsfe.sinint")
### * sim.scen.longsfe.sinint

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sim.scen.longsfe.sinint
### Title: Simulate Design Scenarios for Long-Term Semi-Field Experiment
###   Testing Single Intervention
### Aliases: sim.scen.longsfe.sinint

### ** Examples

sim.scen.longsfe.sinint(
n.ch.per.trt = 4,
exp.length = 90,
sampl.freq = "weekly"
)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sim.scen.longsfe.sinint", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sim.scen.shortsfe.comint")
### * sim.scen.shortsfe.comint

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sim.scen.shortsfe.comint
### Title: Simulate Design Scenarios for Short-Term Semi-Field Experiment
###   Testing Combined Interventions
### Aliases: sim.scen.shortsfe.comint

### ** Examples

sim.scen.shortsfe.comint(n.ch.per.trt = 4)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sim.scen.shortsfe.comint", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sim.scen.shortsfe.sinint")
### * sim.scen.shortsfe.sinint

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sim.scen.shortsfe.sinint
### Title: Simulate Design Scenarios for Short-Term Semi-Field Experiment
###   Testing Single Intervention
### Aliases: sim.scen.shortsfe.sinint

### ** Examples

sim.scen.shortsfe.sinint(n.ch.per.trt = 4)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sim.scen.shortsfe.sinint", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

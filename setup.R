install.packages(c("DALEX",
                   "mlr",
                   "live",
                   "auditor",
                   "ICEbox"))
library(DALEX)
library(live)
library(mlr)
library(auditor)
load(url("https://github.com/pbiecek/DALEX_docs/raw/master/workshops/eRum2018/houses.rda"))

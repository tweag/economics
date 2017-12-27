## Apparently this will make Jenny Bryan set my computer on fire:
## https://twitter.com/hadleywickham/status/940021008764846080
rm(list=ls(all=TRUE))

library("arm")
library("foreign")

## Read in the combined poll
marriage.data <- read.dta("http://www.princeton.edu/~jkastell/MRP_primer/gay_marriage_megapoll.dta", convert.underscore = TRUE)

## Read in state-level dataset
Statelevel <- read.dta("http://www.princeton.edu/~jkastell/MRP_primer/state_level_update.dta",convert.underscore = TRUE)
Statelevel <- Statelevel[order(Statelevel$sstate.initnum),]

## Read in Census data
Census <- read.dta("http://www.princeton.edu/~jkastell/MRP_primer/poststratification%202000.dta",convert.underscore = TRUE)
Census <- Census[order(Census$cstate),]
Census$cstate.initnum <-  match(Census$cstate, Statelevel$sstate)

ginal <- subset(marriage.data,!(is.na(marriage.data$race.wbh) | is.na(marriage.data$age.cat) | is.na(marriage.data$edu.cat) | is.na(marriage.data$state.initnum)))

ginal <- marriage.data[!(is.na(marriage.data$race.wbh)) | (is.na(marriage.data$age.cat)), ]

ginal <- marriage.data[!(is.na(marriage.data$race.wbh)) | (is.na(marriage.data$age.cat)) | !(is.na(marriage.data$edu.cat)) | !(is.na(marriage.data$state.initnum)), ]

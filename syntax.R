#######################################################################
###--------------------------------------------------------------------

### PATHWAY AND PARKAGE LOAD ###
setwd("D:/OneDrive/COVID/topic7")
packages=c('reshape2','dplyr','excessmort','lubridate')
lapply(packages, require, character.only=T)

select = dplyr::select
rename = dplyr::rename
load("data_all.rda")
load("data_digest.rda")



#######################################################################
###--------------------------------------------------------------------

data0 = data_all %>% select(date,gi2,pop) %>% rename(outcome=gi2, population=pop)

exclude_dates <- c(seq(make_date(2020, 3, 7), make_date(2022, 9, 24), by = "day"))
interval <- list(covid = seq(make_date(2020, 3, 7), make_date(2022, 9, 24), by = "day"))
fit = excess_model(data0, exclude = exclude_dates, interval = interval, knots.per.year = 6, model = "quasipoisson", verbose = F)

result <- data.frame(fit)
result <- result %>% mutate(expected.lwr=expected-1.96*sd, expected.upr=expected+1.96+sd)
result <- result %>% mutate(excess.lwr=excess-1.96*sd, excess.upr=excess+1.96+sd)
result[,7:13] <- round(result[,7:13], 0)

pop <- data0$population[nrow(data0)]
observed <- result$observed
expected <- paste0(result$expected," (",result$expected.lwr,", ",result$expected.upr,")")
ed <- paste0(result$excess," (",result$excess.lwr,", ",result$excess.upr,")")

drate.est <- format(round(result$excess/pop*1000000,1),nsmall=1)
drate.lwr <- format(round(result$excess.lwr/pop*1000000,1),nsmall=1)
drate.upr <- format(round(result$excess.upr/pop*1000000,1),nsmall=1)
drate <- paste0(drate.est," (",drate.lwr,", ",drate.upr,")")

ep.est <- format(round(result$excess/result$expected*100,1), nsmall=1)
ep.lwr <- format(round(((sqrt(result$observed)-1.96/2)^2/(result$expected)-1)*100,1),nsmall=1)
ep.upr <- format(round(((sqrt(result$observed)+1.96/2)^2/(result$expected)-1)*100,1),nsmall=1)
ep <- paste0(ep.est," (",ep.lwr,", ",ep.upr,")")

cbind(observed,expected,ed,drate,ep)



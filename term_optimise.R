# A sample R script for forecasting a term-structure in a random financial market
# This code tracks and replicates a target term structure provided for time 
# horizons of next month to several years ahead. The adjustment is deemed 
# necessary due to market prices over certain periods. See Wiki tab for more 
# information on the aim and scope of this project.

# The objective function is defined to maximize the correlation between the
# constructed curve and reference curve while penalising deviation from Mean 
# The penalty term is given by variable "penalty_factor"

# Scripts by Arman Hassanniakalager GitHub @hkalager
# Last reviewed 10 March 2022

# Key inputs
setwd('~/Documents/GitHub/misc')
term_db <- read.csv('term_quotes.csv') # the database
penalty_factor <- 1 
# First replicate the series based on target series variations from the provided 
# starting points 
library(lubridate)
date_ser <- term_db$date_base
date_ser <- as.Date(date_ser,format="%d/%m/%Y")
first_na <- which(is.na(term_db$price_build))[1]
size_ser <- dim(term_db)[1]
for (s in (first_na:size_ser))
{
  term_db$price_build[s]=(term_db$Reference.curve[s]/term_db$Reference.curve[s-1])*term_db$price_build[s-1]
}
cor_0=cor(term_db$price_build,term_db$Reference.curve)

q22021_rng=which(year(date_ser)==2021 & month(date_ser)>=4 & month(date_ser)<=6)
q22021_build_avg <- mean(term_db$price_build[q22021_rng])
q22021_build_adj_factor <- term_db$mean_Q[1]/q22021_build_avg
q22021_build_adj_diff <- term_db$mean_Q[1]-q22021_build_avg
for (s in q22021_rng[-1])
{
  term_db$price_build[s] <- term_db$price_build[s]+q22021_build_adj_diff/2
}
q32021_rng=which(year(date_ser)==2021 & month(date_ser)>=7 & month(date_ser)<=9)
q32021_build_avg <- mean(term_db$price_build[q32021_rng])
q32021_build_adj_factor <- term_db$mean_Q[2]/q32021_build_avg
for (s in q32021_rng)
{
  term_db$price_build[s] <- term_db$price_build[s]*q32021_build_adj_factor
}

q42021_rng=which(year(date_ser)==2021 & month(date_ser)>=10 & month(date_ser)<=12)
q42021_build_avg <- mean(term_db$price_build[q42021_rng])
q42021_build_adj_factor <- term_db$mean_Q[3]/q42021_build_avg
for (s in q42021_rng)
{
  term_db$price_build[s] <- term_db$price_build[s]*q42021_build_adj_factor
}
q12022_rng=which(year(date_ser)==2022 & month(date_ser)<=3)
q12022_build_avg <- mean(term_db$price_build[q12022_rng])
q12022_build_adj_factor <- term_db$mean_Q[4]/q12022_build_avg
for (s in q12022_rng)
{
  term_db$price_build[s] <- term_db$price_build[s]*q12022_build_adj_factor
}
adj_factor_rem=mean(c(q12022_build_adj_factor,q42021_build_adj_factor,q32021_build_adj_factor
                      ,q22021_build_adj_factor))
last_observed_idx=q12022_rng[length(q12022_rng)]

for (s in (last_observed_idx+1):size_ser)
{
  term_db$price_build[s] <- term_db$price_build[s]*adj_factor_rem
}

cor0=cor(term_db$price_build,term_db$Reference.curve)
x0=term_db$price_build
mean0=mean(x0[first_na:last_observed_idx])
mean1=mean(x0[(last_observed_idx+1):size_ser])

fun1 <- function (x)
{ y=term_db$Reference.curve
  x1 <- x
  x1[1:2]=x0[1:2]
  return (-cor(x1,y)+penalty_factor*(abs(mean(x1[first_na:last_observed_idx])-mean0)))
}

res <- optim(x0,fun1)
print('Printing the optimal series and objective function value')
res

opt_ser <- res$par[1:length(x0)]
opt_cor <- cor(opt_ser,term_db$Reference.curve)
paste("Initial correlation ",toString(round(cor0,digits = 3)),
      " vs optimised series correlation ",toString(round(opt_cor,digits = 3)))

## Plot it!
y_min <- min(min(x0),min(term_db$Reference.curve))
y_max <- max(max(x0),max(term_db$Reference.curve))

plot(date_ser,x0,type="l",col="blue",xlab="Time",ylab="",ylim=c(y_min,y_max))
par(new=TRUE)
plot( date_ser, opt_ser, type="l", col="red",xlab="",ylab="$",ylim=c(y_min,y_max),axes=FALSE)
par(new=TRUE)
plot( date_ser, term_db$Reference.curve, type="l", col="green",xlab="",ylab="$",ylim=c(y_min,y_max),axes=FALSE)

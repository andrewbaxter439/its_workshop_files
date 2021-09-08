#' 
#' Interrupted Time Series Analysis - England's Teenage Pregnancy Strategy
#' 
#' Interrupted time series analyses provide a method for evaluating the impact
#' of an exposure on a measured outcome. They have the advantage that they 
#' compare before/after trends and therefore don't *need* a control group (but
#' controls can be used for more robust comparisons - as we shall see). They can
#' account for baseline trends and therefore control for some degree of secular
#' change. One limitation is they are succeptible to other exposures producing
#' spurious results, but controls can be used to overcome this.
#' 
#' There are several ways of doing an ITS analysis.
#' 
#' For more
#' 
#' 
#' 
#' 
#' 
#' 
#' 

# Introduction - The Teenage Pregnancy Strategy --------------------------
#' The Teenage Pregnancy Strategy (TPS) was launched in England in 1999 to
#' address high rates of teenage pregnancy. A stated goal was a 50% reduction in
#' under-18 pregnancy rates.

# Loading required packages
## In this 
library(tidyverse)

# Loading data
# The main dataset we will use here is under-18 pregnancy rates in England from
# 1992 to 2016 (data available from www.ons.gov.uk). If the TPS had had an
# effect on teenage pregnancy rates, we would expect to see a change in this
# target population and outcome.

# load the data and have a look
england <- read_csv("data/England.csv")

england

# Country: here only England data is included (to denote 'exposed' unit)
# Year: from earliest observation (1992) to latest (2016)
# preg_rate: rate of pregnancies to women aged under-18 per 1,000

# Let's have a look at the data in a scatter plot

england %>% 
  ggplot(aes(x = Year, y = preg_rate, colour = Country)) +
  geom_point() +
  # Add in a line between 1998 and 1999 to mark Strategy launch
  geom_vline(xintercept = 1998.5, linetype = "dashed") +
  # Label to make axis clearer and include 0 in y axis range
  ylab("Rate of pregnancies per 1,000 women aged under-18") +
  ylim(0, NA)

# What do you notice about pregnancy rate trends before and after the 1999
# Strategy launch?
# Are these trends (roughly) linear?

# It seems that an interrupted time series model could work here to detect
# changes in pregnancy rates consistent with the launch of the strategy. I
# propose that a slope change at 1999 best represents the impact model seen here
# (see Lopez et al., 2017; Figure 2). Do you agree?


# Fitting a linear model --------------------------------------------------



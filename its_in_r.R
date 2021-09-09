# Note - this is the same code as the 'its_in_r.Rmd' file, but formatted as an R script instead.
# Use this if you prefer normal R script layout.
# Load the required package `tidyverse` to get started:
# 
## ----setup
library(tidyverse)
# if `tidyverse` isn't installed, run `install.packages("tidyverse")` first then
# re-run the `library(tidyverse)` command first.

# When editing in the notebook itself, Shift+Click the links to follow them
# 
# 
# # Interrupted Time Series Analysis - England's Teenage Pregnancy Strategy -----
# 
# Interrupted time series (*ITS*) analyses provide a method for evaluating the
# impact of an exposure on a measured outcome. They have the advantage that they
# compare before/after trends and therefore don't *need* a control group (but
# controls can be used for more robust comparisons - as we shall see). They can
# account for baseline trends and therefore control for some degree of secular
# change. One limitation is they are susceptible to other exposures producing
# spurious results, but controls can be used to overcome this.
# 
# There are several ways of doing an ITS analysis. We'll choose a simple approach
# of a segmented regression model.
# 
# More details on how this can be used can be found in these two papers:
# 
# James Lopez Bernal, Steven Cummins, Antonio Gasparrini, Interrupted time series
# regression for the evaluation of public health interventions: a tutorial,
# International Journal of Epidemiology, Volume 46, Issue 1, February 2017, Pages
# 348–355, https://doi.org/10.1093/ije/dyw098
# 
# James Lopez Bernal, Steven Cummins, Antonio Gasparrini, The use of controls in
# interrupted time series studies of public health interventions, International
# Journal of Epidemiology, Volume 47, Issue 6, December 2018, Pages 2082–2093,
# https://doi.org/10.1093/ije/dyy135
#
# ## Introduction - The Teenage Pregnancy Strategy The Teenage Pregnancy Strategy
# (TPS) was launched in England in 1999 to address high rates of teenage
# pregnancy. A stated goal was a 50% reduction in under-18 pregnancy rates.
# 
# ## The data The main dataset we will use here is under-18 pregnancy rates in
# England from 1992 to 2016 (data available from [The Office for National
# Statisics](www.ons.gov.uk)). If the TPS had had an effect on teenage pregnancy
# rates, we would expect to see a change in this target population and outcome.
# 
# ## Load the data and have a look
# 
# Within each R chunk, replace `___` with R code to produce the desired
# outputs. R will alert you with any errors stopping the code from running
#
##
#----load_england_data 
england <- read_csv("___") # Replace `___` with the path to `England.csv` (hint: in `data` folder)

england

# This includes the variables:
# 
#  - `Country`: here only England data is included (to denote 'exposed' unit)
#  - `Year`: from earliest observation (1992) to latest (2016)
#  - `preg_rate`: rate of pregnancies to women aged under-18 per 1,000
# 
# ## Let's have a look at the data in a scatter plot
## ----graph_rates
eng_plot <- england %>% 
  ggplot(aes(x = ___, y = ___, colour = Country)) +
  geom____() + # add scatter points; Hint: You'll find a function for this in the ggplot2 cheatsheet!
  
  # Add in a line between 1998 and 1999 to mark Strategy launch
  geom_vline(xintercept = 1998.5, linetype = "dashed") +
  
  # Label to make axis clearer and include 0 in y axis range
  ylab("Rate of pregnancies per 1,000 women aged under-18") +
  ylim(0, NA)

eng_plot

# 
# Your graph should look like [this](https://i.imgur.com/YS0Gxm0.png).
# 
# 
# > What do you notice about pregnancy rate trends before and after the 1999
# Strategy launch? >Are these trends (roughly) linear?
# 
# It seems that an interrupted time series model could work here to detect
# changes in pregnancy rates consistent with the launch of the strategy. I
# propose that a slope change at 1999 with a possible level change best
# represents the impact model seen here (see Lopez Bernal et al., 2017; Figure
# 2). Do you agree?
# 
# 
# ## Fitting a linear model
# 
# To test whether level or trend changes were seen at 1999 we fit a linear model
# with coefficients for:
# 
# - Baseline (pre-1992) pregnancy rate - Pre-strategy trend change by year
# ('Time') - Level change at strategy launch ('Level') - Trend change from
# strategy launch ('Trend')
# 
# This gives us the following equation:
# 
# $Rate = \beta_0+\beta_1*Time+\beta_2*Level+\beta_3*Trend+\epsilon$
# 
# To do this, we first need to create dummy variables for Time, Level and Trend:
# 
# - 'Time' should begin with 1 in 1992 and increment by 1 each year.
# - 'Level' should be 0 before the strategy launch and 1 in all years after
# - 'Trend' should be 0 before the strategy launch, begin with 1 in 1999 at strategy launch
# and increment by 1 each year
# 
# Let's add these variables to a new dataframe for testing:
#
##
#----construct_test_df----------------------------------------------------------------------------------------------------------------

# The `ifelse(a, b, c)` function says "If 'a' is TRUE, return 'b', else return 'c'"
# An example is given in setting Level: "If Year is 1999 or later, set Level = 1, else Level = 0"
# Use above logic to complete the 'Trend' variable

england_its_data <- england %>% 
  mutate(
    Time = Year - ___,  # To set 1992 to 1, 1993 to 2 etc.
    Level = ifelse(Year >= 1999, 1, 0),
    Trend = ifelse(Year >= ___, ___, ___)  # 0 before strategy, 1 in 1999, 2 in 2000 etc.
  )

england_its_data

#
# Your data now should look like [this](https://i.imgur.com/vecy6e4.png). If not,
# what would you need to change to fix it?
# 
# We can now fit a straightforward linear model to our data using the `lm`
# function and the formula we derived above. This will attempt to test whether
# the coefficients represent significant level and trend changes at 1999.
# 
#
#----england_mod

# Check the equation above for what three variables to put to the right hand
# side of the `preg ~ ...` formula

england_mod <- lm(preg_rate ~ ___ + ___ + ___, data = ___)

summary(england_mod)


# 
# ## Interpreting our model ----------------------------------------------------
# 
# The estimates of our four coefficients are shown above. Let's interpret them
# according to our data.
# 
# > In England in 1991 there were approximately ___ pregnancies per 1,000 women
# aged under-18. This [increased/decreased] yearly by ___ [more/fewer]
# pregnancies per year from 1992 to 1998.
# 
# (note that in making `Time` begin with 1 in 1992 we implicitly made 1991 our
# `Time = 0` baseline, rather than using `Year` directly and making 1 B.C.E. our
# baseline!)
# 
# > In 1999 at strategy launch England saw a change in yearly pregnancy rates of
# ___ [fewer/more] pregnancies per 1,000 women aged under-18. This was a
# [significant/non-significant] change in level (indepenent of trend changes).
# 
# > From 1999 onwards, England saw [a decreasing/an increasing] trend of ___
# [fewer/more] pregnancies per 1,000 women aged under-18 per year. This was a
# [significant/non-significant] change in trend.
# 
# (note that this is relative to the baseline trend)
# 
# ## Graphing our model
# 
# We have fitted straight lines to our data. Now let's plot those lines over our
# scatter point graph we made above to see how it fits.
# 
# ----england_its_graph

eng_predict <- england_its_data %>% 
  mutate(fitted = predict(england_mod))

eng_predict_plot <- eng_plot + 
  geom_line(data = eng_predict, aes(y = ___, group = Level))  # `group = Level` splits (segments)
                                                              # the line into pre/post strategy parts

eng_predict_plot


#
# Your graph should look like [this](https://i.imgur.com/GbVbkKh.png).
#
# We can add to this a line representing our hypothesised 'no strategy'
# (counterfactual) England - following the baseline trends beyond the 1999
# strategy. We first create a dummy dataset, then apply the same model.
#
# ----eng_its_graph_nos

england_counterfactual <- england_its_data %>% 
  filter(Year >= 1999) %>% 
  mutate(Country = "No strategy",
         Level = ___,      # If England had not had a strategy, 
         Trend = ___) %>%  # Level and Trend would have stayed at 0
  mutate(fitted = predict(england_mod, newdata = .))

# Have a look at your data and see if it looks right
england_counterfactual

# 
## ----output_plot
eng_predict_plot +
  geom_line(data = england_counterfactual, aes(y = ___), linetype = "dashed")


#
# Your graph should look like [this](https://i.imgur.com/uWF1ZXw.png).
#
# > If England had followed the 1991-1998 trend from 1999 onwards, what would
# have happened? Does it look like the Teenage Pregnancy Strategy was effective
# in lowering pregnancy rates?
#
# # Using controls in interrupted time series ----------------------------------
#
# We'll now extend our model by adding a control. In some analyses controls
# might not be possible or necessary. See Lopez Bernal et al., (2018) for some
# things to think through.
#
# A key advantage of using a control is to adjust for other events and
# simultaneous interventions which might produce changes which *look* like
# they're caused by the intervention, but would have happened anyway. In the
# case of teenage pregnancy in England, there could be quite a few other things
# affecting rates across the observation period. We could try adjusting for
# these using Scotland's rates as a control, as Scotland was not exposed to the
# same strategy as England.
#
# Let's import a dataset of Scotland's rates (from [ISD
# Scotland](https://www.isdscotland.org)) and compare:
#
# ----scotland_import

scotland <- read_csv("___") # Read in 'Scotland.csv' (in `data` folder)

scotland

#
# These data are in the same format as England data above, so can be combined
# (with `Country` now able to distinguish the two countries as
# exposed/unexposed).
#
# Let's look at the yearly rates of both countries to see how they compare:
#
# ----eng_scot_plot

eng_scot_rates <- bind_rows(england, scotland)

# graph both countries together - hint: your code for your first scatter plot
# should work here!

eng_scot_plot <- eng_scot_rates %>% 
  ___ +
  ___ +
  ___ # etc.

eng_scot_plot


#
# Your graph should look like [this](https://i.imgur.com/2xhmhoN.png).
#
# > What changes do you see in (unexposed) Scotland? How does this compare with
# England?
#
# ## Adding extra dummies and running controlled ITS model
#
# To run a controlled ITS analyses we need a few extra coefficients (in addition
# to those defined above):
# 
#  - Difference in baseline level between countries ('Group')
#  - Difference in baseline trend between countries ('Group' * 'Time')
#  - Difference in level change at 1999 ('Group' * 'Level')
#  - Difference in trend change at 1999 ('Group' * 'Trend')
# 
# These go in the following equation:
# 
# $Rate = \beta_0+\beta_1*Time+\beta_2*Group+\beta_3*Group*Time+\beta_4*Level+\beta_5*Trend+\beta_6*Level*Group+\beta_7*Trend*Group+\epsilon$
# 
# To do this we'll create the dummy variables in our joined dataset:
# 
#  - 'Group' will be 1 for England and 0 for Scotland
#  - 'Group_time' will be the multiplication of Group and Time - England will
#  have an additional trend on top of the common 'Time' trend controlled for
#  with Scotland
#  - 'Group_level' will be the multiplication of Group and Level - again giving
#  England an additional level change only associated with exposure to the
#  strategy.
#  - 'Group_trend' as the multiplication of Group and Trend - an additional
#  trend change associated with exposure
#  
# For the joined dataset we can now create a full dummy dataset:
# 
# ----eng_sco_its_data

eng_scot_its <- eng_scot_rates %>%
  mutate(
    Time = Year - 1991,
    Level = ifelse(Year >= 1999, 1, 0),
    Trend = ifelse(Year >= 1999, Year - 1998, 0),
    Group = ifelse(Country ==  "___", 1, 0),  # Set to 1 only for England
    Group_time =  ___ * ___,
    Group_level =  ___ * ___,
    Group_trend =  ___ * ___
  )

eng_scot_its


# 
# Your data should look like [this](https://i.imgur.com/0QWbNse.png).
# 
# Now let's fit a controlled model:
# 
# ----eng_scot_mod

# Use the equation above and your new variables to complete the formula

eng_scot_mod <-
  lm(preg_rate ~ Time + Group + ___ + ___ + ___ + ___ + ___,
     data = eng_scot_its
     )

summary(eng_scot_mod)

#
# **Side note - `lm` _can_ do interactions for us: `Group*Time` would
# automatically add the terms `Group + Time + Group:Time`, giving the
# interactions we need. However it's perhaps simpler to explicitly create the
# variables and pass them to the function ourselves, both to keep them in the
# order we specified them and to aid reading the output**
#
# Now see what we can tell from our data.
#
# > In 1991 Scotland had an estimated ___ pregnancies per 1,000 women aged under
# 18, and England had ___ [more/fewer].
#
# > When the strategy launched in England in 1999, Scotland saw [an increase/a
# decrease] in pregnancy rates of ___ per 1,000 women, whist England's level
# change was ___ [more/fewer] pregnancies than Scotland. This difference was
# [significant/not significant].
#
# > Whilst not exposed to the strategy, Scotland saw a
# [significant/non-significant] [increase/decrease] in trend in pregnancies of
# ___ per 1,000 women each year. England's rates [increased/decreased] by ___
# *in addition to Scotland's trend change*. This difference was [significant/not
# significant].
#
# (remember that these trend changes are relative. In Scotland's case the trend
# change was relative to baseline. In England's case the trend change was
# relative to the trend change seen in Scotland).
#
# ## Graphing its model with comparators ---------------------------------------
#
# We can use our model to add prediction lines to both countries to see what the
# fitted models look like.
#
# ----eng_scot_its_graph

eng_scot_predict <- eng_scot_its %>%
  mutate(fitted = predict(___)) # pass the new model output to the `predict` function

eng_scot_predict_plot <- eng_scot_plot +
  geom_line(data = eng_scot_predict, aes(
    y =  ___,
    group = interaction(Level, Country),  # These two lines make a break at 1999 and preserve colours according to country
    colour = Country
  ))

eng_scot_predict_plot


#
# Your graph should look like [this](https://i.imgur.com/L4P20LC.png).
#
# We can further add a predicted line for the proposed path England's rates
# would have followed if it had seen the same effects as Scotland (i.e. in the
# absence of the strategy whilst controlling for confounders). This is a little
# trickier! We again need to create a dummy dataset.
#
# ----eng_scot_its_graph_nos

eng_scot_counterfactual <- eng_scot_its %>%
  filter(Country == "England", Year >= 1999) %>%
  mutate(Country = "No strategy",
         Group_level = ___,
         Group_trend = ___) %>%  # In the absence of the strategy both of these terms would be ...?
  mutate(fitted = predict(___, newdata = .))  # pass in new model

eng_scot_predict_plot +
  geom_line(data = eng_scot_counterfactual, aes(y = ___), linetype = "dashed")


#
# Your graph should look like [this](https://i.imgur.com/Sqo1RGI.png).
#
# > Did exposure to the strategy make England's rates fall faster than in
# unexposed Scotland?
#
# > Did the strategy contribute to falling teenage pregnancy rates?
#
# > Are there potentially other UK-wide (or worldwide?) events which could have
# contributed to falling rates, independent of the strategy?
#
# # Drawing conclusions
#
# You've hopefully now seen what can be tested using an interrupted time series
# analysis, and the potential advantages of using a control. This is an
# introduction to using ITS to evaluate a policy, but there will likely be more
# things to address in analyses like this one:
#
# - How well fitted are the linear models, and do other elements (like
# seasonality and known events) need to be added to the model to give a more
# accurate picture?
#
# - Was the model of level and trend change the best one to use? In our example,
# would omitting the 'Level' change variable (i.e. assuming no 'immediate'
# effects of the strategy on 1999 rates) better fit the data?
#
# - The data may be affected by autocorrelation. Correcting for this is
# important but beyond the scope of this practical (see Lopez Bernal et al.,
# 2017 for some other pointers).
#
# - Is the control appropriate to give estimates of unexposed trends? Is the
# control trend likely to have been affected (directly or indirectly) by the
# tested exposure also? Here we have used a control country recording the same
# outcome (pregnancy rates). In some cases it might be better to use an
# unaffected outcome in the same population.
#
#
# # Going further --------------------------------------------------------------
#
# To cover the advanced aspects of this regression-based approach to interrupted
# time series analysis, I can recommend the course [Policy analysis using
# interrupted time
# series](https://www.edx.org/course/policy-analysis-using-interrupted-time-series),
# available for free on edx. This will cover the further elements of testing for
# and correcting for autocorrelation, as well as correcting for other things
# observed in the data.
#
# To explore the analyses we've just done here, and conduct a repeat comparison
# using Wales's rates as a control, I created a [Shiny
# app](https://phd.andybaxter.me/ITS_shinyapp/) which runs the same syntax on
# the same data in R, but with an interactive user interface. The fuller
# publication of this analysis can be found
# [here](https://www.sciencedirect.com/science/article/pii/S0277953621000174)
# (including next week's subject of synthetic control analysis to approach the
# same problem).
#
# Get in touch through Classrooms if you have any questions!

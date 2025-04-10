##############################################################################
#Ben Horner
#Lab 10
#Math240
##############################################################################
##############################################################################
#Libraries
##############################################################################
library(ggplot2)
library(tidyverse)
library(patchwork)

##############################################################################
#Task 1: Basic Simulation
##############################################################################
true.p = 0.39

#generate 10k polls
sample.size = 1004
basic.simulation = rbinom(sample.size, 10000, true.p)
sim.df = data.frame(id = 1:1004,
                    num.satisfied = basic.simulation)

basic.simulation.plot = ggplot(sim.df, aes(x = num.satisfied)) + #assigns the data as the data frame and aes = x axis
  geom_histogram(aes(y=after_stat(density)), binwidth = 15, fill = "lightblue", color = "black") + #makes histogram
  labs(title = "Simulated Num Satisified", #titles and axis labels
       x = "Satisified per 10000 people", 
       y = "Density")

basic.simulation.plot

lower_bound <- quantile(sim.df$num.satisfied, 0.025)
upper_bound <- quantile(sim.df$num.satisfied, 0.975)
(range.mid.95 <- upper_bound - lower_bound)
(margin.error = range.mid.95/2)


#########
#What if we Double Sample Size?
#########
sample.size = 2008
basic.simulation = rbinom(sample.size, 10000, true.p)
sim.df = data.frame(id = 1:1004,
                    num.satisfied = basic.simulation)

basic.simulation.plot = ggplot(sim.df, aes(x = num.satisfied)) + #assigns the data as the data frame and aes = x axis
  geom_histogram(aes(y=after_stat(density)), binwidth = 15, fill = "lightblue", color = "black") + #makes histogram
  labs(title = "Simulated Num Satisified", #titles and axis labels
       x = "Satisified per 10000 people", 
       y = "Density")

basic.simulation.plot

lower_bound <- quantile(sim.df$num.satisfied, 0.025)
upper_bound <- quantile(sim.df$num.satisfied, 0.975)
(range.mid.95 <- upper_bound - lower_bound)
#range of middle 95% goes down.
(margin.error = range.mid.95/2)

##############################################################################
#Task 2: Resampling
##############################################################################
#Recreating Gallup data
samp.size = 1004
satisfied = round(0.39*samp.size)
disatisfied = round(0.61*samp.size)
Gallup.df = data.frame(people = 1:samp.size, satisfied = c(rep(1, satisfied), rep(0, disatisfied)))


tally.satisfied = tally(Gallup.df, satisfied)
tally.satisfied[1, 1]

#Resampling
R <- 10000 #Number of resamples
resamples <- tibble(num.satisfied = numeric(R))

for( i in 1:R){
  # Take a resample
  curr.resample <- sample(x = Gallup.df$satisfied,
                          size = nrow(Gallup.df),
                          replace = T)
  curr.df = data.frame(people = 1:nrow(Gallup.df), satisfied = curr.resample)
  # compute the stat on the resample
  resamples$num.satisfied[i] <- (tally(curr.df, satisfied)[1, 1])/1004
}


resampling.plot = ggplot(resamples, aes(x = num.satisfied)) + #assigns the data as the data frame and aes = x axis
  geom_histogram(aes(y=after_stat(density)), binwidth = .01, fill = "lightblue", color = "black") + #makes histogram
  labs(title = "Resampled Num Satisified", #titles and axis labels
       x = "Satisified percentage of people", 
       y = "Density")

resampling.plot

#Margin of Error
lower_bound <- quantile(resamples$num.satisfied, 0.025)
upper_bound <- quantile(resamples$num.satisfied, 0.975)
range.middle.95 = upper_bound - lower_bound
margin.error = range.middle.95/2


##############################################################################
#Task 3: Simulation over n and p
##############################################################################
n = c()
for(i in (1:290)){
  num = i*10 + 100
  n = append(n, num)
}
p = c()
for(i in (0:99)){
  num = i*.01
  p = append(p, num)
}

for(n in n){
  for(p in p){
    sim = rbinom(n, 10000, p)
    
  }
}
n =100
p = 0.5
sim = rbinom(n, 10000, p)
sim





R <- 10000 #Number of resamples
resamples.n.p <- tibble(moe = numeric(R))

for( i in 1:R){
  # Take a resample
  curr.resample <- sample(x = Gallup.df$satisfied,
                          size = nrow(Gallup.df),
                          replace = T)
  curr.df = data.frame(people = 1:nrow(Gallup.df), satisfied = curr.resample)
  # compute the stat on the resample
  resamples$num.satisfied[i] <- (tally(curr.df, satisfied)[1, 1])/1004
}


##############################################################################
#Task 3: Simulation over n and p
##############################################################################






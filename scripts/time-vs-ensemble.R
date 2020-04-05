library(tidyverse)

pay <- c(.5, -.4)  # payoff if win or loss
ppay <- c(.5, .5)   # probability of win and loss 
steps <- 100      # number of steps in each simulation run
threturn <- sum((pay + 1) * ppay)

trajectory <- function(steps){
  # Creates simulation of random returns and returns 
  # the data series
  wealth <- 1
  
  for (i in c(1:steps)){
    # randomly draw a value from (1,2) and
    # assign corresponding payoff to return
    # then add 1 to convert to multiplier for wealth
    ret <- 1 + sample(c(1,2), 1, prob = ppay) %>% pay[.] 
    
    # record new wealth value
    wealth[i+1] <- wealth[i] * ret
  }
  
  return(wealth)
}


ensemble <- function(N, steps){
  # Creates ensemble of simulation trajectories
  # uses trajectory() function
  ens <- data.frame(wealth=NULL, 
                    step=NULL, 
                    id=NULL)
  for (i in seq(1:N)){
    wealth <- trajectory(steps)
    step <- seq(1:(steps + 1))
    id <- rep(i, steps + 1)
    ens <- rbind(ens, data.frame(wealth, step, id))
    if (i %% (N %/% 10) == 0) print(rep('*', i %/% (N %/% 10))) # progress bar
  }
  return(ens)
}

df <- ensemble(1000, 1000)

df_summary <- df %>% 
  group_by(step) %>%
  summarise(mean = mean(wealth), median = median(wealth))

head_count <- function(w){
  # subracts vector lagged by 1 from the initial vector 
  # and counts positive instances (heads)
  sum(w[2:length(w)] - w[1:(length(w) - 1)] > 0)
}


# add head count information to trajectories
df <- df %>%
  group_by(id) %>%
  summarise(heads = head_count(wealth)) %>%
  merge(df)

# theoretical E(x) curve
wealth = 1
for (i in seq(1:(nrow(df_summary) - 1))){
  wealth = c(wealth, wealth[i] * threturn)
}
th_trajectory <- data.frame(step = df_summary$step, 
                            wealth = wealth) 


ggplot(df, aes(x = step, y = wealth, group=id))+
  geom_line(aes(color = heads), alpha = .2)+
  geom_line(data = df_summary, aes(y = mean, group = NULL), color = 'red')+
  geom_line(data = df_summary, aes(y = median, group = NULL), color = 'blue')+
  geom_line(data = th_trajectory, aes(y = wealth, group = NULL), color = 'green4')+
  scale_y_log10()+
  scale_color_gradient2(low = 'black', mid = 'yellow', high = 'green', 
                        midpoint = 500)+
  theme_bw()

ggplot(df, aes(x = step, y = wealth, group=id))+
  geom_line(aes(color = id / 1), alpha = .2)+
  geom_line(data = df_summary, aes(y = mean, group = NULL), color = 'red')+
  geom_line(data = df_summary, aes(y = median, group = NULL), color = 'blue')+
  geom_line(data = th_trajectory, aes(y = wealth, group = NULL), color = 'green4')+
  scale_color_gradient2(low = 'black', mid = 'yellow', high = 'green', 
                        midpoint = nrow(df_summary) / 2)+
  theme_bw()+
  xlim(0, 100)+
  ylim(0, 144)

# For N=1000, in order to get ensemble average after T=1000 tosses E(x)???1.05^T 
# you'll need to have one extremely lucky trajectory in your ensemble that has 
# wealth of N × 1.05^T (all the rest contribute ~0 to E(x))
# Solving inequality 1.5^x× 0.6^(T-x) > N×1.05^T gives necessary number of heads
# x ??? 619 in order to realize such an ensemble.
# Probability of x ??? 619 heads out of 1000 tosses is ~4e-14. So with N=1000 trials
# you'd have probability to capture such an event ~4e-11, i.e. almost impossible 
# within a single simulation.

x = 0
for (i in seq(619:1000)){
  x = x + choose(1000, i)*0.5^1000
} 


# To DO: 
# 
# 1. Derive the equation for expected value based on (binomial) distribution of outcomes
# 
# fun fact radius of observable universe is 4.4e26 meters; volume is 3.57e80 cubic meters (if spherical)
# 4/3*pi*(4.4e26^3)

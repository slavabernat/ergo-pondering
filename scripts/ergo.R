library(tidyverse)
pheads <- function(p = 0.5, h, t) choose(t, h)*p^t
ret <- function(h,t) 1.5^h*0.6^(t-h)

# All numbers of heads possible
h <- c(0:1000)
# corresponding probabilities
dh <- pheads(h = h, t = 1000)
# payoff for each number of heads
rh <- ret(h = h, t = 1000)
# total return over T tosses
total_return <- dh * rh

# Check if the total return is the same as arithmetical (1.05^T)
sum(total_return) / 1.05^1000 # yes it is!

# what is the probability to have positive return?
positive_return_idx <- which(rh > 1)
# summing up the probabilities for these outcomes
prob_positive_return <- dh[positive_return_idx] %>% sum()

# if we define ruin as anything less then 0.001
ruin_idx <- which(rh < 0.001)
# then probability of ruin is:
prob_ruin <- dh[ruin_idx] %>% sum()

# Now how does that depends on T?

p.win <- function(t){
  h <- c(0:t)
  dh <- pheads(h = h, t = t)
  rh <- ret(h = h, t = t)
  positive_return_idx <- which(rh > 1)
  prob_positive_return <- dh[positive_return_idx] %>% sum()
  return(prob_positive_return)
}

t <- c(1:1000)
y <- c()
for (i in t){
  y <- c(y, p.win(i))
}

pwin_vs_t <- data.frame(t, y)

ggplot(pwin_vs_t,aes(x = t, y = y))+
  geom_line()+
  scale_y_log10()+
  ylab('P(wealth > 1)')+
  xlab('T')

# distribution of probabilities (beta?)
ggplot(data.frame(h, dh), aes(x = h, y = dh))+
  geom_line()+
  scale_y_log10()

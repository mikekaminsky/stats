library(survey)

N <- 1000
s <- c(0.2, 0.5, 0.3)
p <- c(0.05, 0.15, 0.2)
n <- s*N

strat_mean <- function(sample_sizes, props){
  return(sum(sample_sizes*props)/sum(sample_sizes))
}

# TODO: Get someone to double-check this
strat_var <- function(sample_sizes, props){
  s <- sample_sizes / sum(sample_sizes)
  # SUM(W_h^2 (p_h (1-p_h) / n_h ) )
  return(sum(s**2*props*(1-props)/sum(sample_sizes)))
}

strat_mean(n,p)
strat_var(n,p)

normal_mean <- function(sample_sizes, props){
  return(sum(sample_sizes*props)/sum(sample_sizes))
}

normal_var <- function(sample_sizes, props){
  p <- sum(sample_sizes*props)/sum(sample_sizes)
  # p(1-p)/n
  return(p*(1-p)/sum(sample_sizes))
}


strat_mean(n,p)
strat_var(n,p)

normal_mean(n,p)
normal_var(n,p)

# TODO: generalize to difference in proportions for A/B test
# TODO: calculate difference in sample required
# TODO: replicate with some H' data



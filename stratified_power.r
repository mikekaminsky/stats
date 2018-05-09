#######################################################
# How much more efficiently could we run a/b test if we used stratified sampling?
# http://ocw.jhsph.edu/courses/StatMethodsForSampleSurveys/PDFs/Lecture4.pdf
# http://home.iitk.ac.in/~shalab/sampling/chapter4-sampling-stratified-sampling.pdf
#######################################################

library(survey)

pop_N <- 1000000
N <- 1000
s <- c(0.5, 0.5)
#p <- c(0.05, 0.15, 0.2)
n <- s*N

strat_mean <- function(sample_sizes, props){
  return(sum(sample_sizes*props)/sum(sample_sizes))
}

# TODO: Get someone to double-check this
strat_se <- function(sample_sizes, props){
  s <- sample_sizes / sum(sample_sizes)
  s_sq <- s**2
  se <- props*(1-props) / sum(sample_sizes)
  return(sqrt(sum(s_sq*se)))
}

normal_mean <- function(sample_sizes, props){
  return(sum(sample_sizes*props)/sum(sample_sizes))
}

normal_se <- function(sample_sizes, props){
  p <- sum(sample_sizes*props)/sum(sample_sizes)
  # p(1-p)/n
  return(sqrt(p*(1-p)/sum(sample_sizes)))
}

# Current implementation doesn't seem correct, because if all samples have the same 
# proportion, shouldn't we get the same results?
p <- c(0.05, 0.05)

print("stratified")
print(strat_mean(n,p))
print(strat_se(n,p))

print("normal")
print(normal_mean(n,p))
print(normal_se(n,p))

# TODO: generalize to difference in proportions for A/B test
# TODO: calculate difference in sample required
# TODO: replicate with some H' data



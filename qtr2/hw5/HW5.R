#Kevin N
#Assignment 5

library(igraph)
require(MASS)
get_fb_degree_list = function() {
  # Load Facebook edge list
  fb_edges = read.csv("facebook_edge_list.csv", stringsAsFactors=FALSE)
  fb_degree_list = table(c(fb_edges$Source))
  return (fb_degree_list)
}

createHist = function(fb_degree_list) {
  fb_hist = hist(fb_degree_list, breaks=50, freq=FALSE)
}
# Fit power law
get_pow_fit = function(fb_degree_list) {
  pow_fit = power.law.fit(fb_degree_list)
  return(pow_fit)
}


poissonTest = function() {
  #ks_stat function
  ks_stat = function(x_min,x_max, dist_a, dist_b){
    x_seq = seq(x_min,x_max,len=1000)
    y_cdf1 = sapply(x_seq, function(x){
      sum(dist_a<x)/length(dist_a)
    })
    y_cdf2 = sapply(x_seq, function(x){
      sum(dist_b<x)/length(dist_b)
    })
    k_s_stat = max(abs(y_cdf1-y_cdf2))
    return(k_s_stat)
  }
  #  get k-s distance:
  
  k_s_simulate = function(mean_lambda, length_vec){
    # Distributions
    dist1 = fb_degree_list
    dist2 = rpois(length_vec, mean_lambda)
    # Get k-s distance
    return(ks_stat(1, length_vec, dist1, dist2))
  }
  
  # Now that we can get 1 k-s stat under the null, let's get 10,000 of them.
  k_s_distribution = sapply(1:10000, function(x){
    k_s_simulate(fb_mean_degree, 534)
  })
  
  # Now we just have to sum up how many are equal to or bigger than 1.
  # Note we can never get larger than 1). Since none are larger than 1,
  # we say our p-value is at most 1 / 10,000 = 0.0001. So we reject the null.
  loginfo(paste('Reject null hypothesis. No values above 1, largest is', max(k_s_distribution)))
  
}

if(interactive()) {
  #Setup
  logReset()
  basicConfig(level='DEBUG')
  setwd('C:/Users/kevin/Google Drive/datasci/hw5/')
  addHandler(writeToFile, file="~/hw5.log",level='FINEST') 
  loginfo(paste('Starting HW5.'))
  #Get degree list
  fb_degree_list = get_fb_degree_list()
  
  #Mean
  fb_mean_degree = mean(fb_degree_list)
  loginfo(paste("Mean of degrees=",fb_mean_degree))
  
  #Create hist
  createHist(fb_degree_list)
  
  #Test if the distribution of degrees is Poisson. (reuse K-S code) Just report the k-s distance from our code.
  distFit <-fitdistr(fb_degree_list,"Poisson")
  ks_test = ks.test(fb_degree_list,"ppois",lambda=distFit$estimate)
  loginfo(paste('Result of "ks.test" function says reject null hypothesis. Data is not from poisson distribution. P value=', ks_test$p.value))
  
  #Perform ks stat
  poissonTest()
  
  #Test if the distribution of degrees is a Power Law. (Use igraph library)
  pow_fit = get_pow_fit(fb_degree_list)
  loginfo(paste('Data set is a power law fit: fail to reject null hypothesis. P value is', pow_fit$KS.p))
  
  #Exit
  loginfo(paste('Closing HW5.'))
  
}
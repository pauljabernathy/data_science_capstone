
factorHist = function(x) {

  factors = levels(as.factor(x));
  counts = vector();
  for(i in 1:length(factors)) {
    counts[i] = length(which(x == factors[i]));
  }
  h = data.frame(factors, counts, counts / length(x));
  names(h) = c("item", "counts", "percents");
  h = h[order(h$counts, decreasing=TRUE),]
  h['cum_sum'] = cumsum(h$counts)
  h['cum_frac'] = cumsum(h$percents)
  return(h);
}

factorHist2 = function(x) {
  # Sort the items so that it shows up sorted, but also so that hopefully 
  # the counts can be done by going through the factor list once 
  # => O(nlogn) + O(n); the other is probably O(n^2)
  x = sort(x)
  factors = levels(as.factor(x));
  current_factor = factors[1]
  for(i in 1:length(factors)) {
    # TODO complete this; don't have time to deal with it right now
  }
  
  
}



previousFactorHist = function(x) {
  factors = levels(as.factor(x));
  for(i in 1:length(factors)) {
    #print(factors[i]);
  }
  counts = vector();
  for(i in 1:length(factors)) {
    counts[i] = length(which(x == factors[i]));
  }
  #return(counts);
  #h = data.frame("counts", "percents");
  #h = cbind(h, counts);
  #h[,1] = counts;
  #h = cbind(h, counts / length(x));
  #h[,2] = counts / length(x);
  h = data.frame(factors, counts, counts / length(x));
  names(h) = c("item", "counts", "percents");
  return(h);
}



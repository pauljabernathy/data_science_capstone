
counts_table = function(tokens) {
  # meant to reaplace all the factor hist functions
  a = table(tokens)
  df = data.frame(a)
  df = df[order(df$Freq, decreasing = TRUE),]
  names(df) = c("keys", "counts")
  return(df)
}

factorHist = function(x) {

  factors = unique(x) #levels(as.factor(x));
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
  #factors = levels(as.factor(x));
  #current_factor = factors[1]
  current_unique_value = x[1]
  current_count = 1
  keys = c(x[1])
  counts = c(1)
  key_num = 1
  for(i in 2:length(x)) {
    #print(i)
    if(x[i] == current_unique_value) {
      #current_count = current_count + 1
      counts[key_num] = counts[key_num] + 1
    } else {
      current_unique_value = x[i]
      key_num = key_num + 1
      #current_count = 1
      keys[key_num] = current_unique_value
      counts[key_num] = 1
    }
  }
  h = data.frame(keys, counts)
  h = h[order(h$counts, decreasing = TRUE),]
  h$counts = as.integer(h$counts)
  return(h)
}

factorHist3 = function(x) {
  counts_map = list()
  for(i in 1:length(x)) {
    key = x[i] #as.character(x[i])
    #key = as.character(x[i])
    if(is.null(counts_map[[key]])) {
      counts_map[[key]] = 1
    } else {
      counts_map[[key]] = counts_map[[key]] + 1
    }
  }
  return(counts_map)
  # So we have a "map" with the information...unfortunately, we want it in the form of a data frame.
  if(FALSE) {
  keys = unique(x)
  counts = c()
  for(i in 1:length(keys)) {
    counts[i] = counts_map[[keys[i]]]
  }
  #df = data.frame(keys, counts)
  h = data.frame(keys, counts)
  h = h[order(h$counts, decreasing = TRUE),]
  return(h)
  }
}


# For a "vectorizable" function that can be parallelized, maybe
# find the unique set first, and a map (List, really) object that maps item -> count
# For each item in the data, call a function where you pass in the item and the count map.
# This will only work if it can modify the count map in parallel.

factorHist4 = function(x) {
  values = unique(x)
  # TODO:  complete this
}

count_function = function(item, counts_map) {

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



library(RUnit)
source('util.r')
source('basic_operations.r')

test_counts_table = function() {
  text = "four three two one four three two four three four"
  tokens = tokenize_string(text)
  df = counts_table(tokens)
  print(df)

  df2 = factorHist2(tokens)
  print(df2)
  #checkEquals(df2, df)  # Aren't equal because keys is a factor in one and characters in the other.

}

test_factorHist2 = function() {
  text = "four three two one four three two four three four"
  tokens = tokenize_string(text)
  df = factorHist2(tokens)
  #print(df)
  #checkEquals(c("four", "one", "three", "two"), df$key)
  #checkEquals(c(4, 1, 3, 2), df$counts)
  checkEquals(c("four", "three", "two", "one"), df$key)
  checkEquals(c(4, 3, 2, 1), df$counts)
  
  td = generate_test_data(5)
  #print(td)
  #df2 = factorHist2(tokenize_string(td))
  df2 = factorHist2(td)  # Don't need to tokenize because it is already a vector
  print(df2)
  checkEquals(c("5", "4", "3", '2', '1'), df2$keys)
  checkEquals(c(5, 4, 3, 2, 1), df2$counts)
}

test_factorHist3 = function() {
  text = "four three two one four three two four three four"
  tokens = tokenize_string(text)
  result = factorHist3(tokens)
  #print(result)

  # Now in the other order
  text_2 = "one two three four two three four three four four"
  tokens_2 = tokenize_string(text_2)
  result_2 = factorHist3(tokens_2)
  #print(result_2)
  # checkEquals(result, result_2)
  #print(result == result_2)
  checkEquals(0, sum(result != result_2))

  td = generate_test_data(5)
  df = factorHist2(td)  # Don't need to tokenize because it is already a vector
  #print(df)
  checkEquals(c("5", "4", "3", '2', '1'), df$keys)
  checkEquals(c(5, 4, 3, 2, 1), df$counts)

  td_2 = generate_test_data(15)
  df_2 = factorHist2(td_2)  # Don't need to tokenize because it is already a vector
  #print(df_2)
  checkEquals(c("15", "14", "13", "12", "11", "10", "9", "8", "7", "6", "5", "4", "3", '2', '1'), df_2$keys)
  checkEquals(c(15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1), df_2$counts)
}

compare_performanace = function() {
  #text = read_file_as_string('./en_US/twitter_validate.txt')
  #tokens = tokenize_string(text)
  print(Sys.time())
  #compare_performanace_one_n(100)
  #compare_performanace_one_n(1000)
  #compare_performanace_one_n(10000)
  compare_performanace_one_n(200)
  compare_performanace_one_n(400)
  compare_performanace_one_n(800)
  compare_performanace_one_n(1600)
  compare_performanace_one_n(3200)
}

compare_performanace_one_n = function(n) {
  print(paste("comparing for", n))
  tokens = generate_test_data(n)
  before_counts = Sys.time()
  ct = counts_table(tokens)
  after_counts = Sys.time()

  before_fh_3 = Sys.time()
  fh_3 = factorHist3(tokens)
  after_fh_3 = Sys.time()

  before_fh_2 = Sys.time()
  fh_2 = factorHist2(tokens)
  after_fh_2 = Sys.time()

  before_fh_1 = Sys.time()
  fh_1 = factorHist(tokens)
  after_fh_1 = Sys.time()

  diff_counts = after_counts - before_counts
  diff_1 = after_fh_1 - before_fh_1
  diff_2 = after_fh_2 - before_fh_2
  diff_3 = after_fh_3 - before_fh_3
  #print(before_fh_2)
  #print(after_fh_2)
  #print(before_fh_1)
  #print(after_fh_1)

  print(paste('counts_table', diff_counts))
  print(paste('3', diff_3))
  print(paste('2', diff_2))
  print(paste('1', diff_1))

}

compare_performanace_actual_files = function() {
  compare_performance_one_file("en_US/twitter_test_2.txt")
  compare_performance_one_file("en_US/twitter_test_1.txt")
  compare_performance_one_file("en_US/moby_dick_no_header.txt")
  compare_performance_one_file("en_US/twitter_test.txt")
  compare_performance_one_file("en_US/twitter_train.txt")
  compare_performance_one_file("en_US/twitter_sample.txt")
  # example output:
  #"comparing  en_US/twitter_sample.txt"
  #"counts_table():  1.81812787055969"
  #"factorHist2():  16.8150939941406"
}

compare_performance_one_file = function(file_name) {
  print("")
  print(paste("comparing ", file_name))
  text = read_file_as_string(file_name)
  tokens = tokenize_string(text)

  before_counts_table = Sys.time()
  ct = counts_table(tokens)
  after_counts_table = Sys.time()
  ct_diff = after_counts_table - before_counts_table
  print(paste("counts_table(): ", ct_diff))

  before_factor_hist = Sys.time()
  fh = factorHist2(tokens)
  after_factor_hist = Sys.time()
  fh_diff = after_factor_hist - before_factor_hist
  print(paste("factorHist2(): ", fh_diff))
}

unique_vs_factor = function() {
  unique_vs_factor_one_n(1000)
  unique_vs_factor_one_n(2000)
  unique_vs_factor_one_n(3000)
  unique_vs_factor_one_n(4000)
}

unique_vs_factor_one_n = function(n) {
  print(n)
  td = generate_test_data(n)
  t1 = Sys.time()
  levels(as.factor(td))
  t2 = Sys.time()

  t3 = Sys.time()
  unique(td)
  t4 = Sys.time()

  factor_time = t2 - t1
  unique_time = t4 - t3
  print(factor_time)
  print(unique_time)
  return(c(factor_time, unique_time))
}

generate_test_data = function(max) {
  nums = c()
  for(i in 1:max) {
    nums = c(nums, max:i)
  }
  return(as.character(nums))
}

test_counts_table()
#test_factorHist2()
#test_factorHist3()
#compare_performanace()
#unique_vs_factor()
#compare_performanace_actual_files()


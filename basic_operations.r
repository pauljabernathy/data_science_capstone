source('util.r')
library(readr)

SENTENCE_END_REGEX = '[.?!]' # '[\\.?!]'


read_file_as_string = function(file_name, num_lines=1000000000) {
  # Read a file as a glob, as a sting string, instead of a list of lines as in 
  # readLines().  Designed to simulate the following in Python:
  # with open(file_name, 'r', encoding='UTF-8') as f:
  #     test = f.read()

  if(FALSE) {
    # This method works for small files but for larger files is untractably slow.
    con = file(file_name)
    text_lines = readLines(con, num_lines)
    close(con)
    text = text_lines[1]
    for(i in 2:length(text_lines)) {
      text = paste(text, text_lines[i])
    }
    return(text)
  }
  # I found a library called "readr" that has a function called read_file, which is what I was looking for.
  # I might be able to remove this function entirely.
  text = read_file(file_name)
  text = gsub('\r\n', ' ', text)
  return(text)
}

strip = function(some_string) {
  
  # First, remove the spaces at the front.
  while(substring(some_string, 1, 1) == ' ') {
    some_string = substring(some_string, 2, nchar(some_string))
  }
  
  #Now the spaces at the end.
  while(substring(some_string, nchar(some_string), nchar(some_string)) == ' ') {
    some_string = substring(some_string, 1, nchar(some_string) - 1)
  }
  return(some_string)
}

cleanse = function(text) {
  text = tolower(text)
  text = gsub("'", '', text)
  text = gsub('[[:punct:]]+', ' ', text)
  # text = gsub(' +', ' ', text)  # Remove extra spacing.
  # can also use the following:
  text = gsub('\n', ' ', text)
  text = gsub('[[:blank:]]+', ' ', text)  # Remove extra spacing.
  text = gsub(" $", "", text)  # remove a trailing space; this only removes one; could there be more?
  #print(text)
  text#[[1]]
}

tokenize_string = function(sentence) {
  #print(sentence)
  if(sentence == '') {
    return(c(''))
  }
  # I think the above is generating a warning
  #Warning message:
  #In if (sentence == "") { :
  #    the condition has length > 1 and only the first element will be used
  sentence = cleanse(sentence)
  return(strsplit(sentence, ' ')[[1]])
}


split_to_sentences = function(text) {
  sentences = strsplit(text, SENTENCE_END_REGEX)[[1]]
  for(i in 1:length(sentences)) {
    sentences[i] = strip(sentences[i])
  }
  return(sentences)
}

tokenize_by_sentence = function(text) {
  sentences = split_to_sentences(text)
  result = list()
  #for(sentence in sentences) {
  for(i in 1:length(sentences)) {
    sentence = sentences[i]
    current_result = tokenize_string(sentence)
    if(!is.null(current_result) && current_result != '') {
      result[[i]] = current_result
    }
  }
  return(result)
}

find_sentence_length_hist = function(list_of_sentences) {
  lengths = c()
  for(i in 1:length(list_of_sentences)) {
    lengths = c(lengths, length(list_of_sentences[[i]]))
  }
  h = data.frame(lengths)
  # or maybe factorHist
  fh = factorHist(lengths)
  return(fh)
}

find_word_stats = function(text) {
  tokens = tokenize_string(text)
  fh = factorHist(tokens)
  #fh = counts_table(tokens)
  return(fh)
}


# ngrams

find_n_grams_1_d_list = function (input, n) {
  ngrams = list()
  if(n <= 0 || n > length(input)) {
    return(ngrams)
  }
  for(i in 1:(length(input) - n + 1)) {
    end = i + n - 1
    ngrams[[i]] = input[i:end]
  }
  return(ngrams)
}

find_n_grams_list_of_lists = function(list_of_sentences, n) {
  ngrams = c()
  if(is.null(list_of_sentences)) {
    return(ngrams)
  }
  #for(item in list_of_sentences) {
  for(i in 1:length(list_of_sentences)) {
    item = list_of_sentences[[i]]
    current_ngrams = find_n_grams_1_d_list(item, n)
    ngrams = c(ngrams, current_ngrams)
  }
  return(ngrams)
}

find_n_grams_from_text = function(text, n) {
  lists_of_words = tokenize_by_sentence(text)
  if(FALSE) {
  text = "One sentence.  Two sentences.  To be or not to be.  Whatever.  The problem is that I don't even know what a sentence is."
  tokens = tokenize_by_sentence(text)
  for(i in 1:length(tokens)) {
  }
  result2 = find_n_grams_list_of_lists(tokens, 2)
  return(result2)
  }
  ngrams = find_n_grams_list_of_lists(lists_of_words, n)
  return(ngrams)
}

should_run_analysis = FALSE
if(should_run_analysis) {
  start = Sys.time()
  file_name = 'en_US/twitter_train.txt'
  file_name = 'en_US/moby_dick_no_header.txt'
  file_text = read_file_as_string(file_name)
  word_stats = find_word_stats(file_text)
  sentences = tokenize_by_sentence(file_text)
  sentence_lengths = find_sentence_length_hist(sentences)
  two_grams = find_n_grams_list_of_lists(sentences, 2)
  three_grams = find_n_grams_list_of_lists(sentences, 3)
  end = Sys.time()
  print(paste("completed in", (end - start), "seconds"))
}
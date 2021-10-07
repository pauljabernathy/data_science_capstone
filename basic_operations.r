source('util.r')

PUNCTUATION_REGEX = '[.?!]' # '[\\.?!]'


read_file_as_string = function(file_name, num_lines=1000000000) {
  # Read a file as a glob, as a sting string, instead of a list of lines as in 
  # readLines().  Designed to simulate the following in Python:
  # with open(file_name, 'r', encoding='UTF-8') as f:
  #     test = f.read()
  con = file(file_name)
  text_lines = readLines(con, num_lines)
  close(con)
  text = text_lines[1]
  for(i in 2:length(text_lines)) {
    text = paste(text, text_lines[i])
  }
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
  sentences = strsplit(text, PUNCTUATION_REGEX)[[1]]
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

find_sentence_length_hist = function(list_ofsentences) {
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
  return(fh)
}


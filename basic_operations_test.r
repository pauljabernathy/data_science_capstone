library(RUnit)
source('basic_operations.r')

should_log = TRUE
# TODO:  For each of these, test some potentially problematic input.  
# Empty strings, anything else.  For the coursera project, each file will have
# plenty of words and sentences, but for long term robustness, come up with 
# more test cases.

log = function(message) {
  if(should_log) {
    print(message)
  }
}
# Overhead
test_read_file_as_string = function() {
  log('test_read_file_as_string')
  file_name = 'en_US/twitter_test_2.txt'
  text = read_file_as_string(file_name)
  print(text)
  checkEquals(1, length(text))

  text2 = read_file(file_name)
  print(text2)
  text2 = gsub('\r\n', ' ', text2)
  print(text2)
  checkEquals(text, text2)
}


test_strip = function(){
  log('test_strip')
  some_string = "   something  "
  result = strip(some_string)
  checkEquals("something", result)
  checkEquals("something", strip("something"))
  checkEquals("something", strip("something     "))
  checkEquals("something", strip("        something"))
  checkEquals("", strip(""))
  checkEquals("", strip("                   "))
  checkEquals("\n", strip("       \n            "))
  checkEquals("\n", strip("\n"))
  checkEquals("a    b", strip("a    b"))
  checkEquals("a    b", strip("a    b   "))
  checkEquals("a    b", strip("   a    b"))
}


# Cleaning

test_cleanse = function() {
  log('test_cleanse')
  text = "you're nice.  maybe; I don't think I want to be a robot!"
  result = cleanse(text)
  #print(result)
  #print(result == "youre nice maybe i dont think i want to be a robot")
  expected_result = "youre nice maybe i dont think i want to be a robot"
  checkEquals(expected_result, result)
  
  text = "You're nice.  maybe; I don't think I want to bE a Robot!"
  checkEquals(expected_result, cleanse(text))
  
  text = "You're nice.  maybe; \nI don't think I want to bE a Robot!"
  checkEquals(expected_result, cleanse(text))
}

test_cleanse_apostrophe = function() {
  log('test_cleanse_apostrophe')
  text = "what's up you're hot"
  checkEquals("whats up youre hot", cleanse(text))
}

test_cleanse_with_punct_without_space = function() {
  log('test_cleanse_with_punct_without_space')
  text = "hey,what's up.NOt much here."
  checkEquals("hey whats up not much here", cleanse(text))
}

test_cleanse_file = function() {
  log('test_cleanse_file')
  text = read_file_as_string('en_US/twitter_test_2.txt')
  print(text)
  cleansed_text = cleanse(text)
  print(cleansed_text)
  tokens = tokenize_string(cleansed_text)
  print(tokens)
}


# Tokenization

test_basic_tokenize = function() {
  log('test_basic_tokenize')
  text = "youre nice maybe i dont think i want to be a robot"
  result = tokenize_string(text)
  expected_result = c('youre', 'nice', 'maybe', 'i', 'dont', 'think', 'i', 'want', 'to', 'be', 'a', 'robot')
  checkEquals(sum(expected_result == result), length(expected_result))
}

test_tokenize_with_some_punct_and_upper = function() {
  log('test_tokenize_with_some_punct_and_upper')
  text = "You're nice.  maybe; I don't think I want to bE a Robot!"
  result = tokenize_string(text)
  expected_result = c('youre', 'nice', 'maybe', 'i', 'dont', 'think', 'i', 'want', 'to', 'be', 'a', 'robot')
  checkEquals(sum(expected_result == result), length(expected_result))  
}

test_tokenize_blank_input = function() {
  log('test_tokenize_blank_input')
  result = tokenize_string('')
  checkEquals(c(''), result)
}


test_split_to_sentences = function() {
  log('test_split_to_sentences')
  text = "One sentence.  Two sentences.  To be or not to be.  Whatever.  The problem is that I don't even know what a sentence is."
  sentences = split_to_sentences(text)
  #print(sentences)
  #print(length(sentences))
  checkEquals(5, length(sentences))
  checkEquals("One sentence", sentences[1])
  checkEquals("Two sentences", sentences[2])
  checkEquals("To be or not to be", sentences[3])
  checkEquals("Whatever", sentences[4])
  checkEquals("The problem is that I don't even know what a sentence is", sentences[5])
}

test_tokenize_to_sentences = function() {
  log('test_tokenize_to_sentences')
  text = "One sentence.  Two sentences.  To be or not to be.  Whatever.  The problem is that I don't even know what a sentence is."
  tokens = tokenize_by_sentence(text)
  expected_result = list(c('one', 'sentence'), c('two', 'sentences'), c('to', 'be', 'or', 'not', 'to', 'be'),
                     c('whatever'), c('the', 'problem', 'is', 'that', 'i', 'dont', 'even', 'know', 'what', 'a',
                                    'sentence', 'is'))
  checkEquals(length(expected_result), length(tokens))
  checkEquals(expected_result, tokens)
  # expected_result == tokens gives an error message, but checkEquals(list, list) works
  # Apparently you can't check that two lists are equal
}


# Stats

test_find_sentence_lengths_hist = function() {
  log('test_find_sentence_lengths_hist')
  text = "One sentence.  Two sentences.  To be or not to be.  Whatever.  The problem is that I don't even know what a sentence is."
  #print(text)
  sentences = tokenize_by_sentence(text)
  result = find_sentence_length_hist(sentences)
  #print(result)
  #checkEquals(c(2, 1, 6, 12), result$item) # It complains about the type.
  #checkEquals(c("2", "1", "6", "12"), result$item)
  #checkEquals(c(2, 1, 1, 1), result$counts)
  
  # TODO: test empty string
}

test_find_word_stats = function() {
  log('test_find_word_stats')
  text = "You're nice.  maybe; I don't think I want to bE a Robot!"
  stats = find_word_stats(text)
  #print(stats)
  # After changing to using counts_table() instead of factorHistX() in find_word_stats, the sorting of word with the
  # same count is different, so in this sentence only the first word is matching between the two ways of doing it.
  #checkEquals(c("i", "a", "be", "dont", "maybe", "nice", "robot", "think", "to",
  #              "want", "youre"), stats$item)
  checkEquals("i", stats$item[1])
  
}

# n grams

test_bi_grams = function() {
  log('test_bi_grams')
  input = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  bi_grams = find_n_grams_1_d_list(input, 2)
  #print(bi_grams)
  checkEquals(list(c(0, 1), c(1, 2), c(2, 3), c(3, 4), c(4, 5), c(5, 6), c(6, 7), c(7, 8), c(8, 9)), bi_grams)
}

test_tri_grams = function() {
  log('test_tri_grams')
  input = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  tri_grams = find_n_grams_1_d_list(input, 3)
  checkEquals(list(c(0, 1, 2), c(1, 2, 3), c(2, 3, 4), c(3, 4, 5), c(4, 5, 6), c(5, 6, 7), c(6, 7, 8), c(7, 8, 9) ),
tri_grams)
  checkEquals(1, 2/2)
}

test_less_intelligent_n = function() {
  log('test_less_intelligen_n')
  input = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  checkEquals(list(c(0), c(1), c(2), c(3), c(4), c(5), c(6), c(7), c(8), c(9)), find_n_grams_1_d_list(input, 1))
  checkEquals(list(), find_n_grams_1_d_list(input, 0))
  checkEquals(list(), find_n_grams_1_d_list(input, -1))

  input = c(1)
  checkEquals(list(c(1)), find_n_grams_1_d_list(input, 1))
  checkEquals(list(), find_n_grams_1_d_list(input, 2))
}

test_find_n_grams_list_of_lists = function() {
  log('test_find_n_gram_list_of_lists')
  input = list(c('one', 'sentence'), c('two', 'sentences'), c('to', 'be', 'or', 'not', 'to', 'be'),
                 c('whatever'), c('the', 'problem', 'is', 'that', 'i', 'dont', 'even', 'know', 'what', 'a',
                                'sentence', 'is'))
  text = "One sentence.  Two sentences.  To be or not to be.  Whatever.  The problem is that I don't even know what a sentence is."
  tokens = tokenize_by_sentence(text)
  checkEquals(tokens, input)
  result = find_n_grams_list_of_lists(input, 2)
  result2 = find_n_grams_list_of_lists(tokens, 2)
  #print('result')
  #print(result)
  #print('result2:')
  #print(result2)
  checkEquals(result, result2)
}

test_find_n_grams_from_text = function() {
  log('test_find_n_grams_from_text')
  text = "One sentence.  Two sentences.  To be or not to be.  Whatever.  The problem is that I don't even know what a sentence is."
  expected_result = list( c('one', 'sentence'), c('two', 'sentences'), c('to', 'be'),c('be', 'or'), c('or', 'not'),
                           c('not', 'to'), c('to', 'be'), c('the', 'problem'), c('problem', 'is'), c('is', 'that'),
                           c('that', 'i'), c('i', 'dont'), c('dont', 'even'), c('even', 'know'), c('know', 'what'),
                           c('what', 'a'), c('a', 'sentence'), c('sentence', 'is'))
  result = find_n_grams_from_text(text, 2)
  #print('2 grams')
  #print(result)
  checkEquals(expected_result, result)
}

should_run_them = FALSE
should_run_them = TRUE
#test_read_file_as_string()

if(should_run_them){

  test_read_file_as_string()
  test_strip()
  test_cleanse()
  test_cleanse_apostrophe()
  test_cleanse_with_punct_without_space()
  #test_cleanse_file()
  test_basic_tokenize()
  test_tokenize_with_some_punct_and_upper()
  test_tokenize_blank_input()
  test_split_to_sentences()

  test_tokenize_to_sentences()
  test_find_sentence_lengths_hist()

  test_find_word_stats()

  test_bi_grams()


  test_read_file_as_string()
  test_tri_grams()
  test_less_intelligent_n()
  test_find_n_grams_list_of_lists()
  test_find_n_grams_from_text()
}

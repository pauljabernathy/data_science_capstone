library(RUnit)
source('./basic_operations.r')

# TODO:  For each of these, test some potentially problematic input.  
# Empty strings, anything else.  For the coursera project, each file will have
# plenty of words and sentences, but for long term robustness, come up with 
# more test cases.

# Overhead
test_read_file_as_string = function() {
  file_name = 'en_US/twitter_test_2.txt'
  text = read_file_as_string(file_name)
  #print(text)
  checkEquals(1, length(text))
}


test_strip = function(){
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
  text = "what's up you're hot"
  checkEquals("whats up youre hot", cleanse(text))
}

test_cleanse_with_punct_without_space = function() {
  text = "hey,what's up.NOt much here."
  checkEquals("hey whats up not much here", cleanse(text))
}

test_cleanse_file = function() {
  text = read_file_as_string('en_US/twitter_test_2.txt')
  print(text)
  cleansed_text = cleanse(text)
  print(cleansed_text)
  tokens = tokenize_string(cleansed_text)
  print(tokens)
}


# Tokenization

test_basic_tokenize = function() {
  text = "youre nice maybe i dont think i want to be a robot"
  result = tokenize_string(text)
  expected_result = c('youre', 'nice', 'maybe', 'i', 'dont', 'think', 'i', 'want', 'to', 'be', 'a', 'robot')
  checkEquals(sum(expected_result == result), length(expected_result))
}

test_tokenize_with_some_punct_and_upper = function() {
  text = "You're nice.  maybe; I don't think I want to bE a Robot!"
  result = tokenize_string(text)
  expected_result = c('youre', 'nice', 'maybe', 'i', 'dont', 'think', 'i', 'want', 'to', 'be', 'a', 'robot')
  checkEquals(sum(expected_result == result), length(expected_result))  
}

test_tokenize_blank_input = function() {
  result = tokenize_string('')
  checkEquals(c(''), result)
}


test_split_to_sentences = function() {
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
  text = "One sentence.  Two sentences.  To be or not to be.  Whatever.  The problem is that I don't even know what a sentence is."
  print(text)
  sentences = tokenize_by_sentence(text)
  result = find_sentence_length_hist(sentences)
  print(result)
  #checkEquals(c(2, 1, 6, 12), result$item) # It complains about the type.
  checkEquals(c("2", "1", "6", "12"), result$item)
  checkEquals(c(2, 1, 1, 1), result$counts)
  
  # TODO: test empty string
}

test_find_word_stats = function() {
  text = "You're nice.  maybe; I don't think I want to bE a Robot!"
  stats = find_word_stats(text)
  print(stats)
  checkEquals(c("i", "a", "be", "dont", "maybe", "nice", "robot", "think", "to", 
                "want", "youre"), stats$item)
  
}

# n grams

test_find_n_grams_list_of_string = function() {
  
}










if(F){
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
}
test_find_word_stats()

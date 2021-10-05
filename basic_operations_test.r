library(RUnit)
source('./basic_operations.r')

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

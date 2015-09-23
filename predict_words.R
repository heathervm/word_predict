preprocessTextInput = function(textInput, blackList) {
  textInputCorpus = Corpus(VectorSource(textInput))
  
  textInputCorpus = tm_map(textInputCorpus,
                           removeNonASCII,
                           mc.cores=1)
  
  textInputCorpus = tm_map(textInputCorpus,
                           customRemovePunctuation,
                           mc.cores=1)
  
  textInputCorpus = tm_map(textInputCorpus,
                           removeNumbers,
                           mc.cores=1)
  
  textInputCorpus = tm_map(textInputCorpus,
                           stripWhitespace,
                           mc.cores=1)
  
  textInputCorpus = tm_map(textInputCorpus,
                           removeWords,
                           blackList,
                           mc.cores=1)
  
  predictorInput =    unlist(str_split(as.character(textInputCorpus[[1]])," "))
  
  predictorInput = predictorInput[predictorInput != ""]
  
  return(predictorInput)
}



# http://stackoverflow.com/questions/9934856/removing-non-ascii-characters-from-data-files
# http://stackoverflow.com/questions/18153504/removing-non-english-text-from-corpus-in-r-using-tm
removeNonASCII <-
  content_transformer(function(x) iconv(x, "latin1", "ASCII", sub=""))

# http://stackoverflow.com/questions/14281282/
# how-to-write-custom-removepunctuation-function-to-better-deal-with-unicode-cha
#
# http://stackoverflow.com/questions/8697079/remove-all-punctuation-except-apostrophes-in-r
customRemovePunctuation <- content_transformer(function(x) {
  x <- gsub("[[:punct:]]"," ",tolower(x))
  return(x)
})










predictNextWord <- function(curPhrase,
                            numberOfTerms,
                            textPredictor) {
  #--------------------------------------------------------------------------
  # Predicts the next word of an n-gram using a Markov chain
  #
  # Args:
  #   curPhrase: String that stores an n-gram
  #
  #   numberOfTerms: Number of terms to predict
  #
  #   textPredictor: Markovchain class object
  #
  # Returns:
  #   textPrediction: List that contains prediction(s) of the next term
  #
  #   Keyword:        Description:
  #   -------         -----------
  #   stateHistory    Character vector that stores the markov chain state
  #                   history
  #
  #   textPrediction  Numeric vector that stores the conditional probability
  #                   for the predicted next term(s)
  #--------------------------------------------------------------------------
  textPrediction <- list()
  textPrediction$stateHistory <- character()
  
  numberWords <- length(curPhrase)
  curState <- curPhrase[1]
  vocabulary <- states(textPredictor)
  
  if (!curState %in% vocabulary) {
    randomIdx <- floor(length(vocabulary) * runif(1)) + 1
    curState <- vocabulary[randomIdx]
  }
  
  textPrediction$stateHistory <- 
    append(textPrediction$stateHistory, curState)
  
  for (n in seq(2,numberWords)) {
    nextState <- curPhrase[n]
    if (!nextState %in% vocabulary) {
      curConditionalProbability <- 
        conditionalDistribution(textPredictor, curState)
      
      nextState <- names(which.max(curConditionalProbability))
      
      if (length(nextState) > 1) {
        randomIdx <- floor(length(nextState) * runif(1)) + 1
        nextState <- nextState[randomIdx]
      }
    }
    curState <- nextState
    
    textPrediction$stateHistory <- 
      append(textPrediction$stateHistory, curState)
  }
  
  textPrediction$conditionalProbability <- 
    sort(conditionalDistribution(textPredictor, curState),
         decreasing=TRUE)[1:numberOfTerms]
  
  return(textPrediction)
}
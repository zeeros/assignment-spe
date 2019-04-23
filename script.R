source("./utils.R")
#number of samples per character
N=10000
#flag to use letters missing from the given files
ADD_MISSING_LETTERS=TRUE
#file containing the message to decrypt
message<-"secret.csv"
#vector of given words
words<-c(
  "byxrshaj",
  "houcazfs",
  "hpjqlrdu",
  "jpqdaoek",
  "luoxenzr",
  "svxorhbe")

#matrix with the available letters
letters_params<-getLettersParams(words, db=TRUE)
if(ADD_MISSING_LETTERS){
  letters_params<-addMissingLetters(letters_params)
}
#matrix with the unknown message letters
messageParams<-getLettersParams(message, db=FALSE)
#vector of uncrypted letters
uncrypted_letters<-apply(messageParams, 1, getLetter, letters_params=letters_params)
#concatenate the letters to get the uncrypted message
uncrypted_message<-paste(uncrypted_letters,collapse='')

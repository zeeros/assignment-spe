getShapeParameter<-function(mn,vr){
  return(mn^2/vr)
}
getRateParameter<-function(mn,vr){
  return(mn/vr)
}
#vector of given words
words<-c(
  "byxrshaj",
  "houcazfs",
  "hpjqlrdu",
  "jpqdaoek",
  "luoxenzr",
  "svxorhbe")
#number of samples per character
N=10000
#given a matrix with M elements, returns a list with the same length as letters object.
#each element of the returned list is the result of a function applied to N elements,
#the returned list size is M/N
applyFunctionEveryN <- function(N, matrix, fun, letters=NULL){
  l<-lapply(split(matrix, ceiling(seq_along(matrix)/N)),fun)
  if(!is.null(letters)){
    #match the results with each letter
    names(l)<-letters
    #when a letter is duplicated, keep the average of the values of duplicate letters
    l<-tapply(unlist(l),names(l),mean)
  }
  return(l)
}
#return a matrix containing for each available letter: mean, variance, alpha and lambda
getAlphabetParams <- function(words, N){
  #load the files on a matrix, one file per column
  words_matrix<-sapply(words, scan, what=numeric(), skip=1)
  #for each sample get the absolute value
  words_matrix<-apply(words_matrix, c(1,2), abs)
  
  #concatenate the given words and split by single letter
  letters<-strsplit(paste(words, collapse = ''),'')[[1]]
  means<-applyFunctionEveryN(N, words_matrix, mean, letters)
  vars<-applyFunctionEveryN(N, words_matrix, var, letters)
  #merge mean and variance lists
  letters_params<-mapply(c, means, vars, SIMPLIFY=FALSE)
  #convert list to frame
  letters_params<-do.call(rbind.data.frame, letters_params)
  #rename rows with the letters
  rownames(letters_params)<-rownames(vars)
  #rename cols as mean and variance
  colnames(letters_params)<-c("mean","variance")
  #compute and add alpha to the table
  letters_params <- cbind(letters_params, alpha = getShapeParameter(letters_params[,"mean"],letters_params[,"variance"]))
  #compute and add lambda to the table
  letters_params <- cbind(letters_params, lambda = getRateParameter(letters_params[,"mean"],letters_params[,"variance"]))
  return(letters_params)
}
letters_params<-getAlphabetParams(words,N)
###################################################################
#file containing the message to decrypt
message<-"secret.csv"
#return a matrix containing for each available letter: mean, variance, alpha and lambda
getMessageParams <- function(message, N){
  #load the files on a matrix, one file per column
  secret_matrix<-sapply(message, scan, what=numeric(), skip=1)
  #for each sample get the absolute value
  secret_matrix<-apply(secret_matrix, c(1,2), abs)
  means<-applyFunctionEveryN(N, secret_matrix, mean)
  vars<-applyFunctionEveryN(N, secret_matrix, var)
  #merge mean and variance lists
  message_params<-mapply(c, means, vars, SIMPLIFY=FALSE)
  #convert list to frame
  message_params<-do.call(rbind.data.frame, message_params)
  #rename cols as mean and variance
  colnames(message_params)<-c("mean","variance")
  #compute and add alpha to the table
  message_params <- cbind(message_params, alpha = getShapeParameter(message_params[,"mean"],message_params[,"variance"]))
  #compute and add lambda to the table
  message_params <- cbind(message_params, lambda = getRateParameter(message_params[,"mean"],message_params[,"variance"]))
  return(message_params)
}
messageParams<-getMessageParams(message, N)
#####################################################################
#compare values of message with available letters
getLetter<-function(x){
  #find the index of the most similar letter by parameters value
  idx<-which.min(colSums((t(letters_params) - x)^2))
  letter<-rownames(letters_params[idx,])
  #get the distance between the parameters of the identified letter and the parameters of the message letter
  param_diff<-abs(letters_params[idx,"alpha"]-x["alpha"]+letters_params[idx,"lambda"]-x["lambda"])
  if(param_diff>0.45){
    #The params of the identified letter are too different from the parameters of the message letter
    letter="*"
  }
  return(letter)
}
#messageParams[,"letter"]<-apply(messageParams, 1, getLetter)
#paste(c(messageParams[,"letter"]),collapse='')
uncrypted_message<-apply(messageParams, 1, getLetter)
paste(uncrypted_message,collapse='')
#Why do you want to kill me with this exercise
#W,T,I,M
#No G

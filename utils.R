getShapeParameter<-function(mn,vr){
  return(mn^2/vr)
}
getRateParameter<-function(mn,vr){
  return(mn/vr)
}
addMissingLetters<-function(letters_params){
  missing_letters<-data.frame(
    "w"=c(2.4883263,2.4104026,2.5687692,1.0323281),
    "t"=c(7.5267774,38.2770766,1.4800602,0.1966393),
    "i"=c(4.0385586,8.1813180,1.9935609,0.4936318),
    "m"=c(4.9864062,23.9207021,1.0394447,0.2084557)
  )
  missing_letters<-t(missing_letters)
  colnames(missing_letters)<-c("mean","variance","alpha","lambda")
  #Add space character
  space_character<-data.frame(mean=4.9835774,variance=10.1868914,alpha=2.4380395,lambda=0.4892147)
  rownames(space_character)<-" "
  missing_letters<-(rbind(missing_letters,space_character))
  return(rbind(letters_params,missing_letters))
}
applyFunctionEveryN <- function(N, matrix, fun, letters=NULL){
  #split matrix into N parts and apply fun
  l<-lapply(split(matrix, ceiling(seq_along(matrix)/N)),fun)
  if(!is.null(letters)){
    #rename the results with its letter
    names(l)<-letters
    #for any duplicated letter keep the average of the values
    l<-tapply(unlist(l),names(l),mean)
  }
  return(l)
}
#get a matrix containing for each letter: mean, variance, alpha and lambda
getLettersParams<-function(files, db){
  #load the files on a matrix, one file per column
  matrix<-sapply(files, scan, what=numeric(), skip=1)
  #for each sample get the absolute value
  matrix<-apply(matrix, c(1,2), abs)
  letters<-NULL
  if(db){
    #concatenate the given words and split by single letter
    letters<-strsplit(paste(files, collapse = ''),'')[[1]]
  }
  means<-applyFunctionEveryN(N, matrix, mean, letters)
  vars<-applyFunctionEveryN(N, matrix, var, letters)
  #merge mean and variance lists
  matrix_params<-mapply(c, means, vars, SIMPLIFY=FALSE)
  #convert list to frame
  matrix_params<-do.call(rbind.data.frame, matrix_params)
  #rename rows with the letters
  if(db)
    rownames(matrix_params)<-rownames(vars)
  #rename cols as mean and variance
  colnames(matrix_params)<-c("mean","variance")
  #compute and add alpha to the table
  matrix_params <- cbind(matrix_params, alpha = getShapeParameter(matrix_params[,"mean"],matrix_params[,"variance"]))
  #compute and add lambda to the table
  matrix_params <- cbind(matrix_params, lambda = getRateParameter(matrix_params[,"mean"],matrix_params[,"variance"]))
  return(matrix_params)
}
#retrieve letter from the available values
getLetter<-function(x,letters_params){
  #find the index of the most similar letter by parameters value
  idx<-which.min(colSums((t(letters_params) - x)^2))
  letter<-rownames(letters_params[idx,])
  #get the distance between the parameters of the identified letter and the parameters of the message letter
  param_diff<-abs(letters_params[idx,"alpha"]-x["alpha"]+letters_params[idx,"lambda"]-x["lambda"])
  if(param_diff>0.45){
    #the params of the identified letter are too different from the parameters of the message letter
    letter="*"
  }
  return(letter)
}
mlpredict <- function(input){
  #input can either be csv file or data    
  #newdata <- if(is.character(input) && file.exists(input)){
  #  read.csv(input)
  #} else {
  #  as.data.frame(input)
  #}
  newdata <- as.data.frame(input)
  stopifnot("teamA" %in% names(newdata))
  stopifnot("teamB" %in% names(newdata))
    
  #tv_model is included with the package
  winner <- c("barca!")
  newdata$predictedWinner <- winner
  return(newdata)
}
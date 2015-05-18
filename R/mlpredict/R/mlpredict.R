
mlpredict <- function(input){
  
  library(randomForest)
  newdata <- as.data.frame(input)
  stopifnot("teamA" %in% names(newdata))
  stopifnot("teamB" %in% names(newdata))

  predictGame <- function(model, teamID1, teamID2)
  {
        team_stats = model$team_stats
        t1_stats = team_stats[match(teamID1,team_stats$teamID),]
        t2_stats = team_stats[match(teamID2,team_stats$teamID),]
        colnames(t1_stats) <- paste("T1",colnames(team_stats),sep="")
        colnames(t2_stats) <- paste("T2",colnames(team_stats),sep="")
        game_prediction_stats <- cbind(t1_stats, t2_stats)
        predictions = predict(model, game_prediction_stats)
        predictions = as.character(predictions)
        t1_w = predictions == "T1"
        winners = teamID2
        winners[t1_w] = teamID1[t1_w]
        return(winners)
  }


  teamIDs <- readRDS(paste(system.file(package='mlpredict'),"/teams.rds",sep=''))
  teamIDs[match(input$teamA,teamIDs[,2]),1] -> teamID1
  teamIDs[match(input$teamB,teamIDs[,2]),1] -> teamID2
  print(teamID1)
  print(teamID2)
  model <- readRDS(paste(system.file(package='mlpredict'),"/mmmodel2.rds",sep=''))
  res <- predictGame(model, teamID1, teamID2)

  teamIDs[match(res,teamIDs[,1]),2] -> winner

    
  newdata$predictedWinner <- as.character(winner)
  return(newdata)
}

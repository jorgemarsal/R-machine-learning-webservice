library(HPdclassifier)
distributedR_start()

formatGameData <- function(data)
{
	data = data[,c(-1,-3,-5)]
	t1 = data[,c(2,6:18)]
	t2 = data[,c(3,19:31)]
	old_colnames = substring(colnames(t1),2,10)

	colnames(t1) <- paste("T1",old_colnames,sep="")
	colnames(t2) <- paste("T2",old_colnames,sep="")
	t1_w = cbind(t1, t2, W = rep("T1",nrow(t1)))
	colnames(t1) <- paste("T2",old_colnames,sep="")
	colnames(t2) <- paste("T1",old_colnames,sep="")
	t2_w = cbind(t2,t1, W = rep("T2", nrow(t2)))
	games = rbind(t1_w, t2_w)
	return(games)
}

formatTeamData <- function(data)
{
	w_id = data$wteam
	l_id = data$lteam
	data = data[,c(-1,-3,-5)]
	wloc = as.integer(factor(data$wloc, levels = c("A", "N", "H"))) - 2
	lloc = -wloc
	t1 = data[,c(2,4,6:18)]
	t1$wloc = wloc
	t2 = data[,c(3,19:31)]
	t2$lloc = lloc
	t1 = t1[,c(1,3:ncol(t1),2)]

	old_colnames = substring(colnames(t1),2,10)
	colnames(t1) <- old_colnames
	colnames(t2) <- old_colnames
	team_stats = rbind(t1,t2)
	teamID = data.frame(teamID = c(w_id,l_id))
	team_stats = cbind(team_stats, teamID)
	team_stats = aggregate(.~teamID, team_stats, mean)
	team_stats$loc = NULL
	return(team_stats)
}

predictGame <- function(model, teamID1, teamID2)
{
	team_stats = model$team_stats
	t1_stats = team_stats[match(teamID1,team_stats$teamID),]
	t2_stats = team_stats[match(teamID2,team_stats$teamID),]
	colnames(t1_stats) <- paste("T1",colnames(team_stats),sep="")
	colnames(t2_stats) <- paste("T2",colnames(team_stats),sep="")
	game_prediction_stats <- cbind(t1_stats, t2_stats)
	predictions = predict(model, game_prediction_stats)
	predictions = as.character(predictions$predictions)
	t1_w = predictions == "T1"
	winners = teamID2
	winners[t1_w] = teamID1[t1_w]
	return(winners)
}

game_train_data <- read.csv("Data/regular_season_detailed_results.csv")
team_train_data <- read.csv("Data/regular_season_detailed_results.csv")
team_train_data <- team_train_data[team_train_data$season >= 2014,]
game_data <- formatGameData(game_train_data)
team_data <- formatTeamData(team_train_data)


formula = formula("W ~ I(T1fgm/T1fga) + I(T1fgm3/T1fga3) + I(T1ftm/T1fta) + T1or + T1dr + T1ast + T1to + T1stl + T1blk + T1pf + I(T2fgm/T2fga) + I(T2fgm3/T2fga3) + I(T2ftm/T2fta) + T2or + T2dr + T2ast + T2to + T2stl + T2blk + T2pf")


model <- hpdRF_parallelTree(formula, data = game_data, ntree = 10,
      	completeModel = FALSE, do.trace = TRUE)



print(model)
model$team_stats = team_data
team_seeds = read.csv("Data/tourney_seeds_2015.csv")
team_slots = read.csv("Data/tourney_slots_2015.csv")
correct_results = read.csv("Data/marchmadness_results.csv")
team_seeds = cbind(team_seeds, conditional = team_seeds$team, correct = team_seeds$team)
colnames(team_seeds)[3] = "unconditional"

saveRDS(model, "/tmp/mmmodel2.rds")

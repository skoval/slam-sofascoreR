library(sofascoreR)

con <- make_connection()

RMariaDB::dbRemoveTable(con, "slam_point_by_point")

data(matches)

matches_split <- split(matches, interaction(matches$tour, matches$event, matches$round))

for(data in matches_split)
	write_pbp(data) # Create DB of pbp
	
	
x <- tbl(con, "slam_point_by_point")	 %>%
	collect()
	
	
# Replace missing
x <- x %>%
	dplyr::mutate(
		game_group = ifelse(game >= 13, 13, game)
	) %>%
	group_by(tour, set, game_group, serving) %>%
	dplyr::mutate(
		opponentid = as.character(opponentid),
		playerid = as.character(playerid),
		server_loss_importance_median = median(server_loss_importance, na.rm = T),
		receiver_loss_importance_median = median(receiver_loss_importance, na.rm = T),
		importance_median = median(importance, na.rm = T),
		server_loss_importance = ifelse(is.na(server_loss_importance), server_loss_importance_median, server_loss_importance),
		receiver_loss_importance = ifelse(is.na(receiver_loss_importance), receiver_loss_importance_median, receiver_loss_importance),
		importance = ifelse(is.na(importance), importance_median, importance)
	)
	
RMariaDB::dbWriteTable(
	conn = con,
	name = "slam_point_by_point",
	value = x,
	overwrite = T,
	temp = F,
	row = F
)	
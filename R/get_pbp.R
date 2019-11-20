#' Get PBP and Pressure Scores
#' 
#' @params matches. Data frame of completed matches
#' @params con. DB connection
#'
#' @export
write_pbp <- function(matches, con = make_connection()){
	
	matches <- matches %>%
		dplyr::filter(statusDescription == "FT") # Limit to completed matches
		
	results <- do.call("rbind", lapply(matches$id, function(x) tryCatch(pbp(x), error = function(e) NULL)))
	
	results <- results %>%
		inner_join(matches %>% dplyr::select(event, tour, round, matchid = id, winnerCode, formatedStartDate, player1id, player1name, player2id, player2name), by = "matchid")
		
	results <- results %>%
		dplyr::mutate(
			year = str_extract(formatedStartDate, "20[0-9][0-9]"),
			playerid = ifelse(player_ref == 1, player1id, player2id),
			playername = ifelse(player_ref == 1, player1name, player2name),
			opponentid = ifelse(player_ref == 2, player1id, player2id),
			opponentname = ifelse(player_ref == 2, player1name, player2name),
			wonMatch = winnerCode == player_ref,
			format = ifelse(tour == "atp", "bestof5", "bestof3"),
			advantage = !(year == 2019 & event %in% c("australian-open")) & event != "us-open",
			tiebreak10 = grepl("2019", formatedStartDate) & event == "australian-open"
		)	%>%
		dplyr::select(-player1id, -player1name, -player2id, -player2name)
		
	
	results <- assign_pressure(results) # Assign scores to each point
	
	if(dbExistsTable(con, "slam_point_by_point"))
		RMariaDB::dbWriteTable(
			conn = con,
			name = "slam_point_by_point",
			value = as.data.frame(results),
			append = T,
			row.names = F
		)
	else
		RMariaDB::dbWriteTable(
			conn = con,
			name = "slam_point_by_point",
			value = as.data.frame(results),
			overwrite = T,
			temp = F,
			row.names = F
		)		
	
	RMariaDB::dbDisconnect(con)
	
print("Successfully wrote pbp to DB")
}
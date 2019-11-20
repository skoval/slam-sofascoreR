#' Collect Main Draw Matches
#'
#'
#' @params the_year. Years to extract
#' @params dir. Directory where matches will be saved
#'
#' @export
get_matches <- function(years = c("2017", "2018", "2019"), dir = "~/Software/slam-sofascoreR/"){

	tournaments <- do.call("rbind", lapply(years, function(x){
		tournaments$year <- x
	tournaments
	}))
	
	matches <- do.call("rbind", mapply(
		matchids,
		the_tour = tournaments$tour,
		id = tournaments$id,
		the_year = tournaments$year,
		SIMPLIFY = F
	))
	
	matches$uniqueTournamentId <- as.character(matches$uniqueTournamentId)
	
	matches <- matches %>%
		inner_join(tournaments %>% dplyr::select(uniqueTournamentId = id, event, tour) %>% unique(), by = "uniqueTournamentId")
	
save(matches, file = file.path(dir, "data/matches.RData"))
}

#' Collect URLs and Match Meta Data
#'
#' This function obtains urls and match-level info for all rounds at the specified event and tour
#'
#' @param the_tour Name of tour level as in \code{tournaments}
#' @param id Event id as in \code{tournaments}
#' @param the_year Year of the event
#'
#' @export
matchids <- function(the_tour, id, the_year){

	yearid <- get_yearid(id, the_year)
				
	vars <- c("firstToServe", 
				    "customId", 
				    "winnerCode", 
					"id", 
					"hasStatistics", 
					"hasFirstToServe", 
					"hasDraw", 
					"hasTime", 
					"name", 
					"startTime", 
					"formatedStartDate", 
					"startTimestamp", 
					"statusDescription", 
					"slug", 
					"uniqueTournamentId")
		
	
	get_match_info <- function(url){
			
			match_info <- fromJSON(url)
			
			match  <- match_info$roundMatches$tournaments$events[[1]]				
		
			player1id <- match$homeTeam$id
			player1name <- match$homeTeam$slug
			player2id <- match$awayTeam$id
			player2name <- match$awayTeam$slug
										
			check.complete <- setdiff(vars, names(match))
		
			if(length(check.complete) > 0){
				for(i in check.complete)
					match[,i] <- NA
			}
		
			match$round <- rounds$round[rounds$url == url]
			
			match <- as.data.frame(match[,c(vars, "round")], stringsAsFactors = F)			
			
			match$player1id <- player1id
			match$player1name <- player1name
			match$player2id <- player2id
			match$player2name <- player2name		
			
		match
		}
	
		rounds$url <- sub("YEAR", yearid, sub("EVENT", id, rounds$url))
		
		try <- lapply(rounds$url, function(x) tryCatch(get_match_info(x), error = function(x) NULL))
		
do.call("rbind", try[sapply(try, class) == "data.frame"])
}

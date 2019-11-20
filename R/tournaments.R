#' @export
get_tournaments <- function(dir = "~/Software/slam-sofascoreR/data/"){
	
	urls <- c(
		"https://www.sofascore.com/esi/category/3/tournaments?_=152826353",
		"https://www.sofascore.com/esi/category/6/tournaments?_=152826440"		
	)
	
	lines <- lapply(urls, read_html)
		
	links <- lapply(lines, function(x) {
		 x %>% 
			html_nodes("a") %>%
			html_attr("href")		
	})
		
	event <- lapply(links, function(x){
		sapply(str_split(x, "/"), function(x) x[length(x) - 1])
		})
		
	slams <- lapply(event, function(x) x %in% c("australian-open" , "us-open", "wimbledon", "roland-garros"))

	for(i in 1:length(slams)){
		event[[i]] <- event[[i]][slams[[i]]]
		links[[i]] <- links[[i]][slams[[i]]]
	}
	
	tournaments <- data.frame(
		tour = rep(c("atp", "wta"), each = length(event[[1]])),
		event = as.vector(unlist(event)),
		links = as.vector(unlist(links)),
		stringsAsFactors = F
	) %>%
	unique()
	

save(tournaments, file = file.path(dir, "tournaments.RData"))
}
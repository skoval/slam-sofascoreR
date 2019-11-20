#' @export
get_yearid <- function(id, year){

	
	url <- grep(id, tournaments$links, val = T)

	page <- read_html(paste("https://www.sofascore.com", url, sep = ""))
	
	ids <- page %>% 
		html_nodes(xpath = "//ul[contains(@class, 'dropdown-menu dropdown__menu dropdown__menu--compact dropdown__menu--scroll js-uniqueTournament-page-seasons-select')]") %>%
		html_nodes("a") %>% 
		html_attr("data-season-id")
	
	years <- page %>% 
		html_nodes(xpath = "//ul[contains(@class, 'dropdown-menu dropdown__menu dropdown__menu--compact dropdown__menu--scroll js-uniqueTournament-page-seasons-select')]") %>%
		html_nodes("a") %>% 
		html_text()

	if(any(grepl(year, years))){
		ids[grep(year, years)]
	}
	else{
		NA
	}
}
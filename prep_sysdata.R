library(dplyr)
library(devtools)
library(clutch)
library(usethis)

data(pressure_scores, package = "clutch")

setwd("~/Software/slam-sofascoreR/")	

load(file.path("data", "tournaments.RData"))
load(file.path("data", "rounds.RData"))

tournaments <- tournaments %>%
	dplyr::mutate(
		id = sub("(.*/)([0-9]+$)", "\\2", links)
	)
	
	
rounds <- rounds %>%
	dplyr::filter(round %in% c("F", "SF", "QF", "R16", "R32", "R64", "R128"))

usethis::use_data(
	tournaments,
	rounds,
	pressure_scores,
	internal = T,
	overwrite = T
)
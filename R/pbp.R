#' Collect Point-by-Point Data
#'
#' This function obtains point-level scores for a match from the sofascore site
#'
#' @param id Match id
#'
#' @export
pbp <- function(id, correct_point_sequences = TRUE){
  
  url <- sub("ID", id, "https://www.sofascore.com/event/ID/json?")
  
  x <- fromJSON(url)
  
  # If point-by-point unavailable -> return NA
  if (!x$event$hasStatistics | (length(x$pointByPoint) == 0)) return(NA)
  
  score <- function(x, y, tiebreak){
    if(tiebreak)
      as.numeric(x)
    else{
      if(x == "0")
        0
      else if(x == "15")
        1
      else if(x == "30" | (x == "40" & y == "A"))
        2
      else
        3
    }
  }
  
  score <- Vectorize(score)
  
  match_df <- function(pbp){
    
    point_df <- function(set, pbp, servers){
      
      # Games
      
      games <- pbp$games[[set]]$game
      
      game_scores <- pbp$games[[set]]$score
      
      game_scores$game <- games
      
      # Points
      
      point_scores <- pbp$games[[set]]$points
      
      points <- do.call("rbind", lapply(1:length(point_scores), function(x){
        
        data <- point_scores[[x]]
        
        isTiebreak <- any(data$homePoint == 1 | data$awayPoint == 1)
        
        data <- data.frame(
          isTiebreak = isTiebreak,
          homePoint = c(0, data$homePoint),
          awayPoint = c(0, data$awayPoint),
          homePointScore = c(0, score(data$homePoint, data$awayPoint, isTiebreak)),
          awayPointScore = c(0, score(data$awayPoint, data$homePoint, isTiebreak)),
          homePointType = c(NA, data$homePointType),
          awayPointType = c(NA, data$awayPointType),
          point = 1:(nrow(data) + 1),
          game = games[x],
          stringsAsFactors = F
        )
        
        if(isTiebreak)
          
          data <- data[-nrow(data),]
        
        data
      })
      )
      
      game_winners <- points %>% 
        group_by(game) %>% 
        filter(point == max(point)) %>%
        summarise(game_winner = case_when(!isTiebreak & homePointScore > awayPointScore & homePointScore >= 3 ~ 1,
                                          !isTiebreak & awayPointScore > homePointScore & awayPointScore >= 3 ~ 2,
                                          isTiebreak & homePointScore > awayPointScore & homePointScore >= 6 ~ 1, 
                                          isTiebreak & awayPointScore > homePointScore & awayPointScore >= 6 ~ 2, 
                                          TRUE ~ NaN))
      
      # Check for NA - works for isolated missing games
      if(any(is.na(game_scores$homeScore))){
        
        index <- game_scores$game[is.na(game_scores$homeScore)]
        
        for(i in sort(index)){
          
          if(i > 1 & i < max(game_scores$game)){
            game_scores$serving[game_scores$game == i] <- ifelse(game_scores$serving[game_scores$game == i - 1] == 1, 2, 1)
            home_won <- game_scores$homeScore[game_scores$game == (i + 1)] - as.numeric(game_scores$scoring[game_scores$game == (i + 1)] == 1) - game_scores$homeScore[game_scores$game == (i -1)] > 0
            
            game_scores$homeScore[game_scores$game == i] <- game_scores$homeScore[game_scores$game == (i - 1)] + as.numeric(home_won)
            
            game_scores$awayScore[game_scores$game == i] <- game_scores$awayScore[game_scores$game == (i - 1)] + as.numeric(!home_won)
            
            game_scores$scoring[game_scores$game == i] <- ifelse(home_won, 1, 2)
          }
          # else{
          #   game_scores$serving[game_scores$game == i] <- ifelse(game_scores$serving[game_scores$game == i + 1] == 1, 2, 1)
          #   
          #   home_won <- game_scores$homeScore[game_scores$game == (i  + 1)] > game_scores$awayScore[game_scores$game == (i + 1)]
          #   
          #   game_scores$homeScore[game_scores$game == i] <- as.numeric(home_won)
          #   
          #   game_scores$awayScore[game_scores$game == i] <- as.numeric(!home_won)		
          #   
          #   game_scores$scoring[game_scores$game == i] <- ifelse(home_won, 1, 2)
          # }	
        }
      }
      
      # Check for NA - works for more than one consecutive game missing
      if(any(is.na(game_scores$homeScore))){
        
        first_home_server_game <- game_scores %>% filter(serving == 1) %>% .$game %>% min
        
        game_scores <- game_scores %>% 
          left_join(game_winners, by = "game") %>%
          arrange(game) %>%
          mutate(scoring = ifelse(is.na(scoring), game_winner, scoring),
                 homeCumSum = cumsum(scoring == 1),
                 awayCumSum = cumsum(scoring == 2),
                 serving_pattern = case_when(game %% 2 == 0 & first_home_server_game %% 2 == 0 ~ 1,
                                             game %% 2 != 0 & first_home_server_game %% 2 == 0 ~ 2,
                                             game %% 2 == 0 & first_home_server_game %% 2 != 0 ~ 2,
                                             game %% 2 != 0 & first_home_server_game %% 2 != 0 ~ 1),
                 homeScore = ifelse(is.na(homeScore), homeCumSum, homeScore),
                 awayScore = ifelse(is.na(awayScore), awayCumSum, awayScore),
                 serving = ifelse(is.na(serving), serving_pattern, serving)
          ) %>%
          select(-game_winner, -homeCumSum, -awayCumSum, -serving_pattern)
      }
      
      names(game_scores) <- sub("Score", "GameScore", names(game_scores))
      
      data <- merge(points, game_scores, by = "game")
      
      if(any(data$isTiebreak)){
        
        if(data$game[data$isTiebreak][1] == 1){
          serving <- servers[set + 1,]
          serving <- ifelse(serving$games %% 2 == 1, serving$first.serve, ifelse(serving$first.serve == 1, 2, 1))
          data$serving <- serving
        }
        else
          serving <- data$serving[data$game == 12][1]
        
        first <- ifelse(serving == 1, 2, 1)
        
        serve.order <- c(first, rep(rep(c(serving, first), each = 2), length = sum(data$isTiebreak) - 1))
        
        data$serving[data$isTiebreak] <- serve.order
      }
      
      data
    }
    
    
    sets <- pbp$set
    
    servers <- do.call("rbind", lapply(pbp$games, function(x){
      data.frame(games = max(x$game), first.serve = x$score$serving[nrow(x$score)])
    }))
    
    servers$set <- sets
    
    do.call("rbind", lapply(1:length(sets), function(set, pbp){
      df <- point_df(set, pbp, servers)
      df$set <- sets[set]
      df
    }, pbp = pbp))
    
  }
  
  # Valid scoring sequence?
  match <- match_df(x$pointByPoint) %>% 
    arrange(set, game, point) %>%
    ungroup() %>%
    mutate(uniqueGame = paste0(set, ":", game)) %>%
    group_by(uniqueGame) %>%
    mutate(sumPointScore = homePointScore + awayPointScore,
           diff_sumPointScore = c(NA, diff(sumPointScore)),
           diffHomePointScore = c(diff(homePointScore), as.numeric(unique(scoring) == 1)),
           diffAwayPointScore = c(diff(awayPointScore), as.numeric(unique(scoring) == 2)),
           valid_sequence = case_when(diff_sumPointScore == 1 ~ TRUE,
                                      is.na(diff_sumPointScore) & point == 1 ~ TRUE,
                                      diff_sumPointScore == -1 & (awayPoint == "A" | homePoint == "A") ~ TRUE,
                                      TRUE ~ FALSE))
  
  if (correct_point_sequences){
    
    # Try to correct point sequences within invalid games - initially repeated sequences to correct mistakes
    match <- match %>% 
      group_by(uniqueGame) %>%
      mutate(resetPointIndex = ifelse(!valid_sequence, point, NA),
             resetPointIndex = max(c(0, resetPointIndex), na.rm = TRUE),
             resetSumPointScore = ifelse(!valid_sequence, sumPointScore, NA),
             resetSumPointScore = max(c(0, resetSumPointScore), na.rm = TRUE),
             override = point < resetPointIndex & sumPointScore >= resetSumPointScore) %>%
      filter(!override) %>%
      mutate(point = 1:n(),
             diff_sumPointScore = c(NA, diff(sumPointScore)),
             valid_sequence = case_when(diff_sumPointScore == 1 ~ TRUE,
                                        is.na(diff_sumPointScore) & point == 1 ~ TRUE,
                                        diff_sumPointScore == -1 & (awayPoint == "A" | homePoint == "A") ~ TRUE,
                                        TRUE ~ FALSE)) %>%
      select(-resetPointIndex, -resetSumPointScore, -override) 
    
    # Now can we add in any of the missing points? e.g. 0-0 to 30-0, add 15-0
    toRepeat <- match %>%
      filter(diffHomePointScore > 1 & diffAwayPointScore == 0 | diffHomePointScore == 0 & diffAwayPointScore > 1) %>%
      mutate(correctHome = diffHomePointScore > 0,
             nCorrect = diffHomePointScore + diffAwayPointScore - 1)
    for (i in seq_len(nrow(toRepeat))){
      n_rep <- toRepeat$nCorrect[i]
      to_add <- do.call("rbind", replicate(n_rep, toRepeat[i, ], simplify = FALSE))
      for (j in seq_len(n_rep)){
        if (to_add$correctHome[1]){
          to_add$homePoint[j] <- paste0(to_add$homePoint[j], "+", j) # Rather than trying to fill in -> all future calculations use Point Score
          to_add$homePointScore[j] <- to_add$homePointScore[j] + j
          to_add$point[j] <- to_add$point[j] + j / (j + 1)
        } else {
          to_add$awayPoint[j] <- paste0(to_add$awayPoint[j], "+", j) # Rather than trying to fill in -> all future calculations use Point Score
          to_add$awayPointScore[j] <- to_add$awayPointScore[j] + j
          to_add$point[j] <- to_add$point[j] + j / (j + 1)
        }
      }
      match <- bind_rows(match, to_add) %>%
        select(-correctHome, -nCorrect) %>%
        group_by(uniqueGame) %>%
        arrange(set, game, point) %>%
        mutate(point = 1:n(),
               sumPointScore = homePointScore + awayPointScore,
               diff_sumPointScore = c(NA, diff(sumPointScore)),
               valid_sequence = case_when(diff_sumPointScore == 1 ~ TRUE,
                                          is.na(diff_sumPointScore) & point == 1 ~ TRUE,
                                          diff_sumPointScore == -1 & (awayPoint == "A" | homePoint == "A") ~ TRUE,
                                          TRUE ~ FALSE),
               diffHomePointScore = c(diff(homePointScore), as.numeric(unique(scoring) == 1)),
               diffAwayPointScore = c(diff(awayPointScore), as.numeric(unique(scoring) == 2)))
    }
  }
  
  # Mark each point as HomeWon / Away Won
  match <- match %>%
    group_by(uniqueGame) %>%
    mutate(HomeWonPoint = case_when(diffHomePointScore == 1 & diffAwayPointScore == 0 ~ TRUE,
                                    homePoint == "40" & awayPoint == "40" & diffHomePointScore == 0 & diffAwayPointScore == -1 ~ TRUE,
                                    TRUE ~ FALSE),
           AwayWonPoint = case_when(diffAwayPointScore == 1 & diffHomePointScore == 0 ~ TRUE,
                                    homePoint == "40" & awayPoint == "40" & diffAwayPointScore == 0 & diffHomePointScore == -1 ~ TRUE,
                                    TRUE ~ FALSE),
           invalidPoint = HomeWonPoint == AwayWonPoint | is.na(HomeWonPoint) | is.na(AwayWonPoint),
           invalidGame = any(!valid_sequence | invalidPoint))
  
  # Extract box score and check that the point by point final game score matches
  Nsets <- length(x$pointByPoint$set)
  homeGames <- x$event$homeScore[2:(Nsets + 1)] %>% unlist %>% unname
  awayGames <- x$event$awayScore[2:(Nsets + 1)] %>% unlist %>% unname
  boxScore <- match %>% 
    group_by(set) %>% 
    filter(game == max(game)) %>% 
    filter(point == max(point)) %>% 
    select(set, pbp_homeGameScore = homeGameScore, pbp_awayGameScore = awayGameScore) %>%
    ungroup %>%
    mutate(box_homeGameScore = homeGames, 
           box_awayGameScore = awayGames,
           setWinner = case_when(box_homeGameScore > box_awayGameScore + 1 & box_homeGameScore >= 6 ~ 1,
                                 box_awayGameScore > box_homeGameScore + 1 & box_awayGameScore >= 6 ~ 2,
                                 box_homeGameScore == 7 & box_awayGameScore == 6 ~ 1,
                                 box_homeGameScore == 6 & box_awayGameScore == 7 ~ 2),
           homeWonSet = setWinner == 1,
           awayWonSet = setWinner == 2,
           invalidSet = box_homeGameScore != pbp_homeGameScore | box_awayGameScore != pbp_awayGameScore | is.na(setWinner),
           setScoreHome = paste0(pbp_homeGameScore, "-", pbp_awayGameScore),
           setScoreAway = paste0(pbp_awayGameScore, "-", pbp_homeGameScore))
  
  # Merge invalidSet column into match dataframe
  match <- match %>% left_join(select(boxScore, set, invalidSet, homeWonSet, awayWonSet, setScoreHome, setScoreAway), by = "set")
  
  # Add match score, winner and total number of games
  match$totalGames <- sum(homeGames, awayGames)
  match$matchScoreHome <- paste0(homeGames, "-", awayGames, collapse = ", ")
  match$matchScoreAway <- paste0(awayGames, "-", homeGames, collapse = ", ")
  match$homeWonMatch <- x$event$winnerCode == 1
  match$awayWonMatch <- x$event$winnerCode == 2
  
  # Adjust game scores to be _before_ that game
  currentGameScore <- match %>% group_by(uniqueGame, set, game) %>%
    summarise(homeGameScore = homeGameScore[1],
              awayGameScore = awayGameScore[1]) %>% 
    arrange(set, game) %>%
    group_by(set) %>%
    mutate(homeGameScore = c(0, homeGameScore[-n()]),
           awayGameScore = c(0, awayGameScore[-n()]))
  match <- match %>% 
    select(-homeGameScore, -awayGameScore) %>% 
    left_join(currentGameScore, by=c("uniqueGame", "set", "game"))
  
  # Add current set score
  currentSetScore <- match %>%
    group_by(set) %>%
    dplyr::summarise(
      homeSetScore = HomeWonPoint[n()],
      awaySetScore = AwayWonPoint[n()]
    ) %>%
    dplyr::mutate(
      homeSetScore = c(0, cumsum(homeSetScore)[-n()]),
      awaySetScore = c(0, cumsum(awaySetScore)[-n()])
    )
  match <- match %>% 
    left_join(currentSetScore, by=c("set"))
  
  
  # Prepare player specific datasets
  match_home <- match %>% select(set, game, serving, isTiebreak, point, score = homePoint, opponent_score = awayPoint, 
                                 points = homePointScore, opponent_points = awayPointScore, won_point = HomeWonPoint, 
                                 opponent_point = AwayWonPoint, games = homeGameScore, opponent_games = awayGameScore,
                                 sets = homeSetScore, opponent_sets = awaySetScore,
                                 uniqueGame, invalidPoint, invalidGame, invalidSet, totalGames, matchScore = matchScoreHome, setScore = setScoreHome,
                                 wonSet = homeWonSet, wonMatch = homeWonMatch)
  
  match_away <- match %>% select(set, game, serving, isTiebreak, point, score = awayPoint, opponent_score = homePoint, 
                                 points = awayPointScore, opponent_points = homePointScore, won_point = AwayWonPoint, 
                                 opponent_point = HomeWonPoint,  games = awayGameScore, opponent_games = homeGameScore,
                                 sets = awaySetScore, opponent_sets = homeSetScore,
                                 uniqueGame, invalidPoint, invalidGame, invalidSet, totalGames, matchScore = matchScoreAway, setScore = setScoreAway,
                                 wonSet = awayWonSet, wonMatch = awayWonMatch)
  
  
  match_home$serving <- as.numeric(match_home$serving == 1)
  match_away$serving <- as.numeric(match_away$serving == 2)
  
  match_home$player_ref <- 1
  match_away$player_ref <- 2
  
  pbp <- rbind(match_home, match_away)
  
  pbp$matchid <- id
  
  pbp$missingData <- any(pbp$invalidPoint) | any(pbp$invalidGame) | any(pbp$invalidSet)
  
  pbp %>%
    ungroup
}

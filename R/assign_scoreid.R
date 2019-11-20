#' Add Pressure Score to Point-level Data
#'
#'
#' @param data Dataframe of points and events
#'
#' @export
assign_pressure <- function(data){
  
  data <- data %>%
    dplyr::mutate(
      replacePoint= case_when(
          isTiebreak & points >= 6 & opponent_points >= 6 &
          opponent_points == points ~ 1, 
          isTiebreak & points >= 6 & opponent_points >= 6 &
          opponent_points > points ~ 2, 		
          isTiebreak & points >= 6 & opponent_points >= 6 &
          opponent_points < points ~ 3,        
          !isTiebreak & points >= 3 & opponent_points >= 3 &
          opponent_points == points ~ 7, 
        !isTiebreak & points >= 3 & opponent_points >= 3 &
          opponent_points > points ~ 8, 		
        !isTiebreak & points >= 3 & opponent_points >= 3 &
          opponent_points < points ~ 9, 					
        TRUE ~ 0),											
      points = case_when(
        replacePoint %in% c(1, 2) ~ 5,
        replacePoint == 3 ~ 6,
        replacePoint %in% c(7, 9) ~ 3,
        replacePoint == 8 ~ 2,	
        TRUE ~ points),		
      opponent_points = case_when(
        replacePoint %in% c(1, 3) ~ 5,
        replacePoint == 2 ~ 6,
        replacePoint %in% c(7, 8) ~ 3,
        replacePoint == 9 ~ 2,	
        TRUE ~ opponent_points),
      replaceGame  = case_when(
        format == "bestof3" & set == 3 & advantage &
          games >= 6 & opponent_games >= 6 &
          games == opponent_games ~ 1, 
        format == "bestof3" & set == 3 &  advantage &
          games >= 6 & opponent_games >= 6 &
          games > opponent_games ~ 2, 
        format == "bestof3" & set == 3 & advantage &
          games >= 6 & opponent_games >= 6 &
          games < opponent_games ~ 3, 		
        format == "bestof5" & set == 5 & advantage &
          games >= 6 & opponent_games >= 6 &
          games == opponent_games ~ 4, 
        format == "bestof5" & set == 5 &  advantage &
          games >= 6 & opponent_games >= 6 &
          games > opponent_games ~ 5, 
        format == "bestof5" & set == 5 & advantage &
          games >= 6 & opponent_games >= 6 &
          games < opponent_games ~ 6,
        format == "bestof5" & set == 5 & isTiebreak &
          games >= 6 & opponent_games >= 6 &
          games == opponent_games ~ 9,          
        TRUE ~ 0),									
      games_converted = case_when(
        replaceGame %in% c(1, 3, 4, 6) ~ 5,
        replaceGame %in% c(2, 5, 9) ~ 6,
        TRUE ~ games),
      opponent_games_converted = case_when(
        replaceGame %in% c(1, 2, 4, 5) ~ 5,
        replaceGame %in% c(3, 6, 9) ~ 6,
        TRUE ~ opponent_games),		
      id = ifelse(serving == 1, paste(points, opponent_points, games_converted, opponent_games_converted, sets, opponent_sets, sep = ""), 
                  paste(opponent_points, points, opponent_games_converted, games_converted, opponent_sets, sets, sep = ""))		
    ) %>%
    select(-replaceGame, -replacePoint, -opponent_games_converted, -games_converted)
  
  result <- data %>% 
    left_join(pressure_scores, by = c("tour", "format", "advantage", "tiebreak10", "id")) %>%
     mutate(importance = ifelse(serving == 1, server_loss_importance, receiver_loss_importance))
    
unique(result)
}	


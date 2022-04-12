#################################################
########## NFL Data Preparation Script ##########
#################################################
#
#
#
#
#

# Load Data
load("~/nfl_pbp_2018_2020.rda")
# Extract Games
games <- unique(pbp_2018_2020[, c(2,4,5,7,284,322, 323)])

# Drop games after week 15
pbp_use <- pbp_2018_2020[pbp_2018_2020$week <= 15,]


# Create index for play by play
pbp_index <- rep(NA, nrow(pbp_use))

for(i in 1:nrow(pbp_use)){
  if(pbp_use$season[i] == 2018){
    pbp_index[i] <- pbp_use$week[i]
  } else if (pbp_use$season[i] == 2019){
    pbp_index[i] <- pbp_use$week[i] + 15
  } else if (pbp_use$season[i] == 2020){
    pbp_index[i] <- pbp_use$week[i] + 30
  }
}

pbp_use$index <- pbp_index

games <- unique(pbp_use[, c(2,4,5,7,284,322, 323, 371)])

#### Define Functions for calculations

epa_calculator_off <- function(data, week_lag = 5, db){
  #'
  #' This function calculates the average EPA for each team for 
  #' different play types. 
  #' 
  #' @param data The play-by-play dataset
  #' @param week_lag The number of weeks to use to calculate averages
  #' @param db The games to create stats for 
  #' 
  #' @return A data frame with the calculated values for each of the teams
  #'
  #'
  
  # Create empty columns to store results
  hrun_epa <- hrun_freq <- hpass_epa <- hpass_freq <- rep(NA, nrow(db))
  hgames <- rep(NA, nrow(db))
  
  arun_epa <- arun_freq <- apass_epa <- apass_freq <- rep(NA, nrow(db))
  agames <- rep(NA, nrow(db))
  
  # For each game
  for(i in 1:nrow(db)){
    # Calculate start index
    start_index <- max(1, db$index[i] - week_lag - 1)
    # Calculate end index
    end_index <- db$index[i] - 1
    # Extract plays involving teams of interest falling in window
    temp <- data[which(data$index > start_index & 
                         data$index <= end_index &
                         data$posteam %in% c(db$home_team[i], db$away_team[i])),]
    # If any such plays exist
    if(nrow(temp) >0 ){
      # Calculate total home run epa
      hrun_epa[i] <- sum(temp$epa[which(temp$play_type == "run" & 
                                          temp$posteam == db$home_team[i])], na.rm = T)
      # Calculate total home run plays
      hrun_freq[i] <- nrow(temp[which(temp$play_type == "run" & 
                                        temp$posteam == db$home_team[i]),])
      # Calculate total home pass epa
      hpass_epa[i] <- sum(temp$epa[which(temp$play_type == "pass" & 
                                           temp$posteam == db$home_team[i])], na.rm = T)
      # Calculate total home pass plays
      hpass_freq[i] <- nrow(temp[which(temp$play_type == "pass" & 
                                         temp$posteam == db$home_team[i]),])
      
      # Calculate total home number of games played
      hgames[i] <- length(unique(na.omit(temp$game_id[which(temp$posteam == db$home_team[i])])))
      
      # Calculate total away run epa
      arun_epa[i] <- sum(temp$epa[which(temp$play_type == "run" & 
                                          temp$posteam == db$away_team[i])], na.rm = T)
      # Calculate total away run plays
      arun_freq[i] <- nrow(temp[which(temp$play_type == "run" & 
                                        temp$posteam == db$away_team[i]),])
      # Calculate total away pass epa
      apass_epa[i] <- sum(temp$epa[which(temp$play_type == "pass" & 
                                           temp$posteam == db$away_team[i])], na.rm = T)
      # Calculate total away pass plays
      apass_freq[i] <- nrow(temp[which(temp$play_type == "pass" & 
                                         temp$posteam == db$away_team[i]),])
      
      # Calculate total number of away games played
      agames[i] <- length(unique(na.omit(temp$game_id[which(temp$posteam == db$away_team[i])])))
    }
    
  }
  # Calculate averages per play
  havg_run_epa <- hrun_epa/hrun_freq
  havg_pass_epa <- hpass_epa/hpass_freq
  
  # Calculate play type frequency per play
  hruns_per_game <- hrun_freq/hgames
  hpass_per_game <- hpass_freq/hgames
  
  # Calculate averages per play
  aavg_run_epa <- arun_epa/arun_freq
  aavg_pass_epa <- apass_epa/apass_freq
  
  # Calculate play type frequency per play
  aruns_per_game <- arun_freq/agames
  apass_per_game <- apass_freq/agames
  
  # Join results columns
  res_dat <- cbind.data.frame(hrun_epa, hrun_freq, hpass_epa, hpass_freq, 
                              havg_run_epa, havg_pass_epa,  hgames,
                              hruns_per_game, hpass_per_game,
                              arun_epa, arun_freq, apass_epa, apass_freq, 
                              aavg_run_epa, aavg_pass_epa,  agames,
                              aruns_per_game, apass_per_game)
  
  # Fill missing with 0
  #res_dat[is.na(res_dat)] <- 0
  # Return Results
  return(res_dat)
}



epa_calculator_def <- function(data, week_lag = 5, db){
  #'
  #' This function calculates the average EPA for each team for 
  #' different play types. 
  #' 
  #' @param data The play-by-play dataset
  #' @param week_lag The number of weeks to use to calculate averages
  #' @param db The games to create stats for 
  #' 
  #' @return A data frame with the calculated values for each of the teams
  #'
  #'
  
  # Create empty columns to store results
  hrun_epa <- hrun_freq <- hpass_epa <- hpass_freq <- rep(NA, nrow(db))
  hgames <- rep(NA, nrow(db))
  
  arun_epa <- arun_freq <- apass_epa <- apass_freq <- rep(NA, nrow(db))
  agames <- rep(NA, nrow(db))
  
  # For each game
  for(i in 1:nrow(db)){
    # Calculate start index
    start_index <- max(1, db$index[i] - week_lag - 1)
    # Calculate end index
    end_index <- db$index[i] - 1
    # Extract plays involving teams of interest falling in window
    temp <- data[which(data$index > start_index & 
                         data$index <= end_index &
                         data$defteam %in% c(db$home_team[i], db$away_team[i])),]
    # If any such plays exist
    if(nrow(temp) >0 ){
      # Calculate total home run epa
      hrun_epa[i] <- sum(temp$epa[which(temp$play_type == "run" & 
                                          temp$defteam == db$home_team[i])], na.rm = T)
      # Calculate total home run plays
      hrun_freq[i] <- nrow(temp[which(temp$play_type == "run" & 
                                        temp$defteam == db$home_team[i]),])
      # Calculate total home pass epa
      hpass_epa[i] <- sum(temp$epa[which(temp$play_type == "pass" & 
                                           temp$defteam == db$home_team[i])], na.rm = T)
      # Calculate total home pass plays
      hpass_freq[i] <- nrow(temp[which(temp$play_type == "pass" & 
                                         temp$defteam == db$home_team[i]),])
      
      # Calculate total home number of games played
      hgames[i] <- length(unique(na.omit(temp$game_id[which(temp$defteam == db$home_team[i])])))
      
      # Calculate total away run epa
      arun_epa[i] <- sum(temp$epa[which(temp$play_type == "run" & 
                                          temp$defteam == db$away_team[i])], na.rm = T)
      # Calculate total away run plays
      arun_freq[i] <- nrow(temp[which(temp$play_type == "run" & 
                                        temp$defteam == db$away_team[i]),])
      # Calculate total away pass epa
      apass_epa[i] <- sum(temp$epa[which(temp$play_type == "pass" & 
                                           temp$defteam == db$away_team[i])], na.rm = T)
      # Calculate total away pass plays
      apass_freq[i] <- nrow(temp[which(temp$play_type == "pass" & 
                                         temp$defteam == db$away_team[i]),])
      
      # Calculate total number of away games played
      agames[i] <- length(unique(na.omit(temp$game_id[which(temp$defteam == db$away_team[i])])))
    }
    
  }
  # Calculate averages per play
  havg_run_epa <- hrun_epa/hrun_freq
  havg_pass_epa <- hpass_epa/hpass_freq
  
  # Calculate play type frequency per play
  hruns_per_game <- hrun_freq/hgames
  hpass_per_game <- hpass_freq/hgames
  
  # Calculate averages per play
  aavg_run_epa <- arun_epa/arun_freq
  aavg_pass_epa <- apass_epa/apass_freq
  
  # Calculate play type frequency per play
  aruns_per_game <- arun_freq/agames
  apass_per_game <- apass_freq/agames
  
  # Join results columns
  res_dat <- cbind.data.frame(hrun_epa, hrun_freq, hpass_epa, hpass_freq, 
                              havg_run_epa, havg_pass_epa,  hgames,
                              hruns_per_game, hpass_per_game,
                              arun_epa, arun_freq, apass_epa, apass_freq, 
                              aavg_run_epa, aavg_pass_epa,  agames,
                              aruns_per_game, apass_per_game)
  # Change names to indicate defense
  names(res_dat) <- paste(names(res_dat), "_def")
  # Fill missing with 0
  #res_dat[is.na(res_dat)] <- 0
  # Return Results
  return(res_dat)
}



### Calculate stats

# Calculate offensive stats
off_stats <- epa_calculator_off(data = pbp_use, week_lag = 5, db = games)

# Calculate defensive stats
def_stats <- epa_calculator_def(data = pbp_use, week_lag = 5, db = games)

mod_dat <- cbind.data.frame(games, off_stats, def_stats)

save(mod_dat, file = "game_level_stats.rda")





























































































names(mod_dat2)
# Extract game level stats
game_stats <- mod_dat2[,c(1:8, 45,46)]
# Extract home stats
hstats <- mod_dat2[,c(2,9:17, 27:35)]
# Extract away stats
astats <- mod_dat2[,c(3, 18:26,36:44)]

# Fix names
names(hstats) <- names(astats) <- c("team", "run_epa", "run_freq", "pass_epa", "pass_freq",         
                   "avg_run_epa", "avg_pass_epa", "games", "runs_per_game",     
                   "pass_per_game", "run_epa_def", "run_freq_def",     
                   "pass_epa_def", "pass_freq_def", "avg_run_epa_def",
                   "avg_pass_epa_def", 
                   "games_def", "runs_per_game_def", "pass_per_game_def")
# Create copy of home stats
hstats_1 <- hstats
# Append opp to names for copy of home stats
names(hstats_1) <- paste(names(hstats), "_opp", sep = "")

# Create copy of away stats
astats_1 <- astats
# Append opp to names for copy of away stats
names(astats_1) <- paste(names(astats), "_opp", sep = "")

# Join original home and copy of away
data_1 <- cbind.data.frame(hstats, astats_1)
# Join original away and copy of home
data_2 <- cbind.data.frame(astats, hstats_1)

# Create location columns
data_1$location <- "home"
data_2$location <- "away"

# Create winner columns
data_1$winner <- as.numeric(as.character(mod_dat2$winner))
data_2$winner <- abs(1 - as.numeric(as.character(mod_dat2$winner)))

# Create point differential columns
data_1$point_diff <- mod_dat2$point_dif
data_2$point_diff <- -mod_dat2$point_dif

# Rejoin data
use_data <- rbind.data.frame(cbind.data.frame(game_stats, data_1),
                             cbind.data.frame(game_stats, data_2))

# Check stats are correctly linked
use_data[use_data$game_id == "2019_08_TB_TEN",]

# Save dataset
save(use_data, file = "doubled_mod_data.rda")



































































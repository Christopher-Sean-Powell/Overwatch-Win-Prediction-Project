library(dplyr)
library(ggplot2)
library(boot)
library(ISLR)
library(class)
library(MASS)
library(glmnet)
library(tree)
library(randomForest)
library(gbm)
library(pROC)

# Reading in all relevant data frames:

map_data <- read.csv('match_map_stats.csv')

stage_1_2018 <- read.csv('phs_2018_stage_1.csv')
stage_2_2018 <- read.csv('phs_2018_stage_2.csv')
stage_3_2018 <- read.csv('phs_2018_stage_3.csv')
stage_4_2018 <- read.csv('phs_2018_stage_4.csv')
playoffs_2018 <- read.csv('phs_2018_playoffs.csv')


stage_1_2019 <- read.csv('phs_2019_stage_1.csv')
stage_2_2019 <- read.csv('phs_2019_stage_2.csv')
stage_3_2019 <- read.csv('phs_2019_stage_3.csv')
stage_4_2019 <- read.csv('phs_2019_stage_4.csv')
playoffs_2019 <- read.csv('phs_2019_playoffs.csv')

stage_1_2020 <- read.csv('phs_2020_1.csv')
stage_2_2020 <- read.csv('phs_2020_2.csv')

# Now that the initial data frames have been read in we can begin to manipulate them.



# Combine data frames for each year together, so we can work with a single table. Note that column names changed 
# from 2019 to 2020, will need to rename them to match for consistency:

all_2018_data <- bind_rows(stage_1_2018, stage_2_2018, stage_3_2018, stage_4_2018, playoffs_2018)

all_2019_data <- bind_rows(stage_1_2019, stage_2_2019, stage_3_2019, stage_4_2019, playoffs_2019)

all_2020_data <- bind_rows(stage_1_2020, stage_2_2020)

# Renaming columns of the 2020 table to match the nomenclature from previous years' tables:
all_2020_data <- all_2020_data %>% rename(match_id = esports_match_id, player = player_name, hero = hero_name, team = team_name, stage = tournament_title)

# Note: each of these tables (excluding 2020 which has fewer games so less data) has over 1 million rows. Most of these are 
# unnecessary for my analysis, so I will cut them down once I have narrowed down the important rows.

# Now we will leave these original tables as they are and start manipulating them as new objects, starting with dropping some 
# unnecessary columns:

all_2018_data_adjusted <- dplyr::select(all_2018_data, -1, -3, -11)
all_2019_data_adjusted <- dplyr::select(all_2019_data, -1, -3, -11)
all_2020_data_adjusted <- dplyr::select(all_2020_data, -1, -3, -11)




# The next goal is to create a table that includes only the 5 stats that represent medal categories for each player involved in each
# round of each match (for each year of stats), then arrange so that player stats for each medal category are grouped together.

# First, we delete all rows that are not related to aggregate stats for each medal category (for each year), such as individual hero
# stats:


player_stats_2018 = bind_rows(filter(all_2018_data_adjusted, stat_name == 'Eliminations', hero == 'All Heroes'), 
                                        filter(all_2018_data_adjusted, stat_name == 'Objective Kills', hero == 'All Heroes'),
                                        filter(all_2018_data_adjusted, stat_name == 'Hero Damage Done', hero == 'All Heroes'),
                                        filter(all_2018_data_adjusted, stat_name == 'Objective Time', hero == 'All Heroes'),
                                        filter(all_2018_data_adjusted, stat_name == 'Healing Done', hero == 'All Heroes'))



player_stats_2019 = bind_rows(filter(all_2019_data_adjusted, stat_name == 'Eliminations', hero == 'All Heroes'), 
                                        filter(all_2019_data_adjusted, stat_name == 'Objective Kills', hero == 'All Heroes'),
                                        filter(all_2019_data_adjusted, stat_name == 'Hero Damage Done', hero == 'All Heroes'),
                                        filter(all_2019_data_adjusted, stat_name == 'Objective Time', hero == 'All Heroes'),
                                        filter(all_2019_data_adjusted, stat_name == 'Healing Done', hero == 'All Heroes'))



player_stats_2020 = bind_rows(filter(all_2020_data_adjusted, stat_name == 'Eliminations', hero == 'All Heroes'), 
                                        filter(all_2020_data_adjusted, stat_name == 'Objective Kills', hero == 'All Heroes'),
                                        filter(all_2020_data_adjusted, stat_name == 'Hero Damage Done', hero == 'All Heroes'),
                                        filter(all_2020_data_adjusted, stat_name == 'Objective Time', hero == 'All Heroes'),
                                        filter(all_2020_data_adjusted, stat_name == 'Healing Done', hero == 'All Heroes'))

# Note: as described above on line ~41, the original tables were massive with over 1 million rows each, and they are now cut down to 
# closer to 60 thousand rows each.

# And, we can arrange the data so that the aggregate medal stats for each player on a team in a game are clustered together by stat 
# and in descending order:

player_stats_2018 = player_stats_2018 %>% arrange(match_id, map_name, team, stat_name, desc(stat_amount))
player_stats_2019 = player_stats_2019 %>% arrange(match_id, map_name, team, stat_name, desc(stat_amount))
player_stats_2020 = player_stats_2020 %>% arrange(match_id, map_name, team, stat_name, desc(stat_amount))

# We can now visually see the spread of medals for each stat in the "stat_amount" column, with gold being the top value in each 
# cluster, silver the second, etc.



# Now we want to add a game_id to the table, since the match_id is not particularly useful to us currently:

player_stats_2018[,'game_id'] <- NA # adding an empty column for game_id
player_stats_2019[,'game_id'] <- NA
player_stats_2020[,'game_id'] <- NA

player_stats_2018[, 9][1] <- 1   # Adding the first value manually so that the loop below can start at the second row and count up

for(i in 2:length(player_stats_2018[, 1])){
  
  ifelse(player_stats_2018[, 3][i] == player_stats_2018[, 3][i-1], player_stats_2018[, 9][i] <- player_stats_2018[, 9][i-1], 
         player_stats_2018[, 9][i] <- (player_stats_2018[, 9][i-1] + 1))
  
}

# This loop simply checks if the map_name of the current row is the same as the previous row, and if it is then the game_id is passed
# down and if not then the game_id is incremented +1. Note that there will never be any games back to back with the same map_name
# because the map_names are organized alphabetically within match_id (so the last game within a match_id chunk will be late in the
# alphabet and the next one will be early) and games with the same match_id will never repeat maps.

# 1109 is the last game_id for 2018, so we will start from 1110 for the 2019 data:

player_stats_2019[, 9][1] <- 1110

for(i in 2:length(player_stats_2019[, 1])){
  
  ifelse(player_stats_2019[, 3][i] == player_stats_2019[, 3][i-1], player_stats_2019[, 9][i] <- player_stats_2019[, 9][i-1], 
         player_stats_2019[, 9][i] <- (player_stats_2019[, 9][i-1] + 1))
  
}

# 2476 is the last game_id for 2019, so we will start from 2477 for the 2020 data:

player_stats_2020[, 9][1] <- 2477

for(i in 2:length(player_stats_2020[, 1])){
  
  ifelse(player_stats_2020[, 3][i] == player_stats_2020[, 3][i-1], player_stats_2020[, 9][i] <- player_stats_2020[, 9][i-1], 
         player_stats_2020[, 9][i] <- (player_stats_2020[, 9][i-1] + 1))
  
}

# Now that we have an ID for each game we can see that there are a total of 3616 games in the data set.

# It would be nice to have the remaining columns in a slightly different order to make viewing easier, so now we can rearrange the columns:

player_stats_2018 <- player_stats_2018 %>% dplyr::select(match_id, game_id, map_name, map_type, team, player, hero, stat_name, stat_amount)
player_stats_2019 <- player_stats_2019 %>% dplyr::select(match_id, game_id, map_name, map_type, team, player, hero, stat_name, stat_amount)
player_stats_2020 <- player_stats_2020 %>% dplyr::select(match_id, game_id, map_name, map_type, team, player, hero, stat_name, stat_amount)

# Now the data frame is in a good format to begin adding new features, such as who won the game (from map_data) and the medals awarded.



# First, we add an empty column for the medal_awarded category:

player_stats_2018[,'medal_awarded'] <- NA
player_stats_2019[,'medal_awarded'] <- NA
player_stats_2020[,'medal_awarded'] <- NA

# Now, we can loop through the table and populate the medal_awarded column with the appropriate values:

player_stats_2018[, 10][1] <- 'gold'   # Adding the first value manually so the loop starts at the second row and counts up

for (i in 2:length(player_stats_2018[, 1])){
  
  ifelse(player_stats_2018[, 10][i-1] == 'gold' && player_stats_2018[,8][i] == player_stats_2018[, 8][i-1],
         
         player_stats_2018[, 10][i] <- 'silver', 
         
         ifelse(player_stats_2018[, 10][i-1] == 'silver' && player_stats_2018[, 8][i] == player_stats_2018[, 8][i-1],
                
                player_stats_2018[, 10][i] <- 'bronze',
                
                ifelse(player_stats_2018[, 10][i-1] == 'bronze' && player_stats_2018[, 8][i] == player_stats_2018[, 8][i-1],
                       
                       player_stats_2018[, 10][i] <- NA,
                       
                       ifelse(is.na(player_stats_2018[, 10][i-1]) && player_stats_2018[, 8][i] == player_stats_2018[, 8][i-1],
                              
                              player_stats_2018[, 10][i] <- NA, player_stats_2018[, 10][i] <- 'gold'))))
  
}

# Convoluted loop here is checking the row above the current row to see the value of the medal_awarded in the previous row and also
# checking to make sure that the stat_name is the same as the previous row and then either adding the next descending medal or if
# the stat_name category has changed then it awards gold to the first row of the new stat_name category.

# Now for the other 2 tables:

player_stats_2019[, 10][1] <- 'gold'   # Adding the first value manually again

for (i in 2:length(player_stats_2019[, 1])){
  
  ifelse(player_stats_2019[, 10][i-1] == 'gold' && player_stats_2019[,8][i] == player_stats_2019[, 8][i-1],
         
         player_stats_2019[, 10][i] <- 'silver', 
         
         ifelse(player_stats_2019[, 10][i-1] == 'silver' && player_stats_2019[, 8][i] == player_stats_2019[, 8][i-1],
                
                player_stats_2019[, 10][i] <- 'bronze',
                
                ifelse(player_stats_2019[, 10][i-1] == 'bronze' && player_stats_2019[, 8][i] == player_stats_2019[, 8][i-1],
                       
                       player_stats_2019[, 10][i] <- NA,
                       
                       ifelse(is.na(player_stats_2019[, 10][i-1]) && player_stats_2019[, 8][i] == player_stats_2019[, 8][i-1],
                              
                              player_stats_2019[, 10][i] <- NA, player_stats_2019[, 10][i] <- 'gold'))))
  
}


player_stats_2020[, 10][1] <- 'gold'   # Adding the first value manually again

for (i in 2:length(player_stats_2020[, 1])){
  
  ifelse(player_stats_2020[, 10][i-1] == 'gold' && player_stats_2020[,8][i] == player_stats_2020[, 8][i-1],
         
         player_stats_2020[, 10][i] <- 'silver', 
         
         ifelse(player_stats_2020[, 10][i-1] == 'silver' && player_stats_2020[, 8][i] == player_stats_2020[, 8][i-1],
                
                player_stats_2020[, 10][i] <- 'bronze',
                
                ifelse(player_stats_2020[, 10][i-1] == 'bronze' && player_stats_2020[, 8][i] == player_stats_2020[, 8][i-1],
                       
                       player_stats_2020[, 10][i] <- NA,
                       
                       ifelse(is.na(player_stats_2020[, 10][i-1]) && player_stats_2020[, 8][i] == player_stats_2020[, 8][i-1],
                              
                              player_stats_2020[, 10][i] <- NA, player_stats_2020[, 10][i] <- 'gold'))))
  
}

# Now that the medals have been added, we can add the game_winner information from the map_data table



map_winners <- c() # empty list to store map_winners

map_winners[1] <- 'Los Angeles Valiant' # adding the first entry since the loop will start at i = 2

map_data <- map_data %>%  arrange(match_id, map_name) # must organize the map_data table to be in the same game order as the other tables so that the games line up

for (i in 2:length(map_data[,1])){
  
  ifelse(map_data[, 9][i] != map_data[, 9][i-1], map_winners <- c(map_winners, map_data[, 7][i]), next)
}

map_winners

# This loop populates the map_winners vector with the winner of each map, only adding the value to the map_winners vector when the 
# map_name changes. It includes 11 extra games that are not included in the other data, but they are at the end and we can ignore


# Now, we need to loop through the player_stats tables and add the game_winner info:


player_stats_2018[,'map_winner_team'] <- NA

for (i in 1:length(player_stats_2018[, 1])){
  
  player_stats_2018[, 11][i] <- map_winners[player_stats_2018[,2][i]]
}

player_stats_2019[,'map_winner_team'] <- NA

for (i in 1:length(player_stats_2019[, 1])){
  
  player_stats_2019[, 11][i] <- map_winners[player_stats_2019[,2][i]]
}

player_stats_2020[,'map_winner_team'] <- NA

for (i in 1:length(player_stats_2020[, 1])){
  
  player_stats_2020[, 11][i] <- map_winners[player_stats_2020[,2][i]]
}

# Now each of the player_stats tables contain a column telling which team won. This is not as useful as an indicator of "yes" or "no",
# so we can add another column relating that info now:


player_stats_2018[,'game_winner'] <- NA # adding the new empty column
player_stats_2019[,'game_winner'] <- NA
player_stats_2020[,'game_winner'] <- NA


for (i in 1:length(player_stats_2018[, 1])){
  
  ifelse(player_stats_2018[, 11][i] == 'draw', player_stats_2018[, 12][i] <- player_stats_2018[, 11][i], 
         
         ifelse(player_stats_2018[, 11][i] == player_stats_2018[, 5][i], player_stats_2018[, 12][i] <- 'yes', 
                player_stats_2018[, 12][i] <- 'no'))
}


for (i in 1:length(player_stats_2019[, 1])){
  
  ifelse(player_stats_2019[, 11][i] == 'draw', player_stats_2019[, 12][i] <- player_stats_2019[, 11][i], 
         
         ifelse(player_stats_2019[, 11][i] == player_stats_2019[, 5][i], player_stats_2019[, 12][i] <- 'yes', 
                player_stats_2019[, 12][i] <- 'no'))
}


for (i in 1:length(player_stats_2020[, 1])){
  
  ifelse(player_stats_2020[, 11][i] == 'draw', player_stats_2020[, 12][i] <- player_stats_2020[, 11][i], 
         
         ifelse(player_stats_2020[, 11][i] == player_stats_2020[, 5][i], player_stats_2020[, 12][i] <- 'yes', 
                player_stats_2020[, 12][i] <- 'no'))
}


# these loops compare the values in the team_name column and the map_winner_team column and then adds 'yes' or 'no' to the map_winner
# column accordingly (or draw).

# Now we can delete the map_winner_team column:

player_stats_2018 <- player_stats_2018 %>% dplyr::select(-11)
player_stats_2019 <- player_stats_2019 %>% dplyr::select(-11)
player_stats_2020 <- player_stats_2020 %>% dplyr::select(-11)


# Some games end in a "draw", which I just realized will cause problems for the model later, so I will now remove those rows:

player_stats_2018 <- player_stats_2018 %>% filter(game_winner != 'draw')
player_stats_2019 <- player_stats_2019 %>% filter(game_winner != 'draw')
player_stats_2020 <- player_stats_2020 %>% filter(game_winner != 'draw')


# Now, since the game_id's are all messed up I will recycle the previous code and fix the game_id order. This is helpful to know how many games there are total
# after dropping the draws (and also because we need this information later)

for(i in 2:length(player_stats_2018[, 1])){
  
  ifelse(player_stats_2018[, 3][i] == player_stats_2018[, 3][i-1], player_stats_2018[, 2][i] <- player_stats_2018[, 2][i-1], 
         player_stats_2018[, 2][i] <- (player_stats_2018[, 2][i-1] + 1))
  
}

# This loop simply checks if the map_name of the current row is the same as the previous row, and if it is then the game_id is passed
# down and if not then the game_id is incremented +1. Note that there will never be any games back to back with the same map_name
# because the map_names are organized alphabetically within match_id (so the last game within a match_id chunk will be late in the
# alphabet and the next one will be early) and games with the same match_id will never repeat maps.

# 1084 is the last game_id for 2018, so we will start from 1085 for the 2019 data:

player_stats_2019[, 2][1] <- 1085 # Adding the first value manually so that the loop below can start at the second row and count up

for(i in 2:length(player_stats_2019[, 1])){
  
  ifelse(player_stats_2019[, 3][i] == player_stats_2019[, 3][i-1], player_stats_2019[, 2][i] <- player_stats_2019[, 2][i-1], 
         player_stats_2019[, 2][i] <- (player_stats_2019[, 2][i-1] + 1))
  
}

# 2419 is the last game_id for 2019, so we will start from 2420 for the 2020 data:

player_stats_2020[, 2][1] <- 2420

for(i in 2:length(player_stats_2020[, 1])){
  
  ifelse(player_stats_2020[, 3][i] == player_stats_2020[, 3][i-1], player_stats_2020[, 2][i] <- player_stats_2020[, 2][i-1], 
         player_stats_2020[, 2][i] <- (player_stats_2020[, 2][i-1] + 1))
  
}

# Now that we have fixed the game IDs we can see that our total games is now 3541, which means that 75 out of the original 3616 games ended in a draw.



# Our tables are finally complete, so we can make one large one combining them all:

all_player_stats <- bind_rows(player_stats_2018, player_stats_2019, player_stats_2020)


# The next hurdle is to add entropy features for each team in each game. Then we can collapse the data so that each game is one row.


'
This function takes as input a dataframe from my overwatch project, a game id index, and a stat category. It then filters the data frame
to only include rows where the game_id is the given index and the stat_name is the given category. It will then break this up into
the winner and loser of that game in particular, and then will return the entropy of the given stat for the given game in the table
for both teams involved as a list. The function also checks if the game ends in a draw which needs to be handled differently.
'
categorical_entropy <- function(df, category, index){
  
  winner_df = df %>% filter(game_id == index, stat_name == category, game_winner == 'yes')
  loser_df = df %>% filter(game_id == index, stat_name == category, game_winner == 'no')
  
  winner_denominator = sum(winner_df[,'stat_amount'], na.rm = TRUE)
  winner_entropy = 0
  for (i in 1:dim(winner_df)[1]){
    
    winner_entropy = winner_entropy + ((winner_df$stat_amount[i]/winner_denominator)*log2(1/(winner_df$stat_amount[i]/winner_denominator)))
  }
  ifelse(length(winner_entropy) == 0, winner_entropy <- 0, winner_entropy <- winner_entropy) # this deals with the occasional case where the entropy is numeric(0)
  
  loser_denominator = sum(loser_df[,'stat_amount'], na.rm = TRUE)
  loser_entropy = 0
  for (i in 1:dim(loser_df)[1]){
    loser_entropy = loser_entropy + ((loser_df$stat_amount[i]/loser_denominator)*log2(1/(loser_df$stat_amount[i]/loser_denominator)))
  }
  ifelse(length(loser_entropy) == 0, loser_entropy <- 0, loser_entropy <- loser_entropy)
  
  
  winner_list = list('yes', winner_entropy) # coupling yes/no w/ entropy values for clarity and later assignment based on game_winner column entry
  loser_list = list('no', loser_entropy)
  
  entropy_list = list(winner_list, loser_list)
  
  return(entropy_list)
}


# Here is the empty final table that all of these values will be added into:
player_entropy = as.data.frame(matrix(nrow= 3541, ncol = 16))
colnames(player_entropy) = c('game_id', 'map_name', 'map_type', 'team_1_eliminations', 'team_1_healing_done', 
                             'team_1_hero_damage_done', 'team_1_objective_kills', 'team_1_objective_time', 
                             'team_2_eliminations', 'team_2_healing_done', 'team_2_hero_damage_done', 'team_2_objective_kills', 
                             'team_2_objective_time', 'game_winner', 'winner_higher_entropy', 'entropy_difference')

# rows equal to number of games (from highest game_id), 
# columns for game_id, map_name, map_type, a column for each stat_name for each team, and finally game_winner (plus a few extras for my own interest)


# Now we can write out a test loop to see if the process is working, using a test entropy table:

player_entropy_test = as.data.frame(matrix(nrow= 146, ncol = 16))
colnames(player_entropy_test) = c('game_id', 'map_name', 'map_type', 'team_1_eliminations', 'team_1_healing_done', 
                             'team_1_hero_damage_done', 'team_1_objective_kills', 'team_1_objective_time', 
                             'team_2_eliminations', 'team_2_healing_done', 'team_2_hero_damage_done', 'team_2_objective_kills', 
                             'team_2_objective_time', 'game_winner', 'winner_higher_entropy', 'entropy_difference')


# This loop runs the categorical_entropy function for each of the stat categories for each of the games (based on the iteration variable being equal to the 
# game_id) and then adds these into the final entropy table; thus, the original all_player_stats table is reduced down to only containing one row per game
# with an entropy for each medal stat category for each team. It is important to note the randomness added: if it weren't for this, my code would always
# return team_1 as the winner and this would be a problem. Instead, the random number makes it change which team is listed as winning. 

for (i in 1:dim(player_entropy_test)[1]){
  RV1 = sample(1:2, 1, replace = F) # random number to randomize the naming of the winning team as team_1 or team_2
  RV2 = 0
  ifelse(RV1 == 1, RV2 <- 2, RV2 <- 1)
  
  elim_entropy = categorical_entropy(all_player_stats, 'Eliminations', i)
  player_entropy_test[, 'team_1_eliminations'][i] <- elim_entropy[[RV1]][[2]]
  player_entropy_test[, 'team_2_eliminations'][i] <- elim_entropy[[RV2]][[2]]
  ifelse(elim_entropy[[RV1]][[1]] == 'yes', player_entropy_test[, 'game_winner'][i] <- 'team 1', player_entropy_test[, 'game_winner'][i] <- 'team 2')
  
  healing_entropy = categorical_entropy(all_player_stats, 'Healing Done', i)
  player_entropy_test[, 'team_1_healing_done'][i] <- healing_entropy[[RV1]][[2]]
  player_entropy_test[, 'team_2_healing_done'][i] <- healing_entropy[[RV2]][[2]]
  
  damage_entropy = categorical_entropy(all_player_stats, 'Hero Damage Done', i)
  player_entropy_test[, 'team_1_hero_damage_done'][i] <- damage_entropy[[RV1]][[2]]
  player_entropy_test[, 'team_2_hero_damage_done'][i] <- damage_entropy[[RV2]][[2]]
  
  obj_kills_entropy = categorical_entropy(all_player_stats, 'Objective Kills', i)
  player_entropy_test[, 'team_1_objective_kills'][i] <- obj_kills_entropy[[RV1]][[2]]
  player_entropy_test[, 'team_2_objective_kills'][i] <- obj_kills_entropy[[RV2]][[2]]
  
  time_entropy = categorical_entropy(all_player_stats, 'Objective Time', i)
  player_entropy_test[, 'team_1_objective_time'][i] <- time_entropy[[RV1]][[2]]
  player_entropy_test[, 'team_2_objective_time'][i] <- time_entropy[[RV2]][[2]]
  
  # As a fun way to check my original hypothesis (that the team with more evenly distributed stats, i.e. higher entropy, has a higher chance of winning) 
  # I added a bit of code to check whether the winning team also had higher entropy. Then I can count these up and see what percentage of the time my 
  # hypothesis is correct.
  
  team_1_entropy = (player_entropy_test[,4][i] + player_entropy_test[,5][i] + player_entropy_test[,6][i] + player_entropy_test[,7][i] + player_entropy_test[,8][i])
  team_2_entropy = (player_entropy_test[,9][i] + player_entropy_test[,10][i] + player_entropy_test[,11][i] + player_entropy_test[,12][i] + player_entropy_test[,13][i])
  
  player_entropy_test[,16][i] = abs(team_1_entropy - team_2_entropy)
  
  ifelse(player_entropy_test[,14][i] == 'team 1' & team_1_entropy > team_2_entropy, player_entropy_test[,15][i] <- 'yes', 
         ifelse(player_entropy_test[,14][i] == 'team 2' & team_2_entropy > team_1_entropy, player_entropy_test[,15][i] <- 'yes', player_entropy_test[,15][i] <- 'no'))
  
  player_entropy_test[, "game_id"][i] <- i
  player_entropy_test[, "map_name"][i] <- filter(all_player_stats, game_id == i)[, 'map_name'][1]
  player_entropy_test[, 'map_type'][i] <- filter(all_player_stats, game_id == i)[, 'map_type'][1]
  
}



# Now the finale loop to add each of the entropy features to the final table:

for (i in 1:dim(player_entropy)[1]){
  RV1 = sample(1:2, 1, replace = F) # random number to randomize the naming of the winning team as team_1 or team_2
  RV2 = 0
  ifelse(RV1 == 1, RV2 <- 2, RV2 <- 1)
  
  elim_entropy = categorical_entropy(all_player_stats, 'Eliminations', i)
  player_entropy[, 'team_1_eliminations'][i] <- elim_entropy[[RV1]][[2]]
  player_entropy[, 'team_2_eliminations'][i] <- elim_entropy[[RV2]][[2]]
  ifelse(elim_entropy[[RV1]][[1]] == 'yes', player_entropy[, 'game_winner'][i] <- 'team 1', player_entropy[, 'game_winner'][i] <- 'team 2')
  
  healing_entropy = categorical_entropy(all_player_stats, 'Healing Done', i)
  player_entropy[, 'team_1_healing_done'][i] <- healing_entropy[[RV1]][[2]]
  player_entropy[, 'team_2_healing_done'][i] <- healing_entropy[[RV2]][[2]]
  
  damage_entropy = categorical_entropy(all_player_stats, 'Hero Damage Done', i)
  player_entropy[, 'team_1_hero_damage_done'][i] <- damage_entropy[[RV1]][[2]]
  player_entropy[, 'team_2_hero_damage_done'][i] <- damage_entropy[[RV2]][[2]]
  
  obj_kills_entropy = categorical_entropy(all_player_stats, 'Objective Kills', i)
  player_entropy[, 'team_1_objective_kills'][i] <- obj_kills_entropy[[RV1]][[2]]
  player_entropy[, 'team_2_objective_kills'][i] <- obj_kills_entropy[[RV2]][[2]]
  
  time_entropy = categorical_entropy(all_player_stats, 'Objective Time', i)
  player_entropy[, 'team_1_objective_time'][i] <- time_entropy[[RV1]][[2]]
  player_entropy[, 'team_2_objective_time'][i] <- time_entropy[[RV2]][[2]]
  
  # As a fun way to check my original hypothesis (that the team with more evenly distributed stats, i.e. higher entropy, has a higher chance of winning) 
  # I added a bit of code to check whether the winning team also had higher entropy. Then I can count these up and see what percentage of the time my 
  # hypothesis is correct.
  
  team_1_entropy = (player_entropy[,4][i] + player_entropy[,5][i] + player_entropy[,6][i] + player_entropy[,7][i] + player_entropy[,8][i])
  team_2_entropy = (player_entropy[,9][i] + player_entropy[,10][i] + player_entropy[,11][i] + player_entropy[,12][i] + player_entropy[,13][i])
  
  player_entropy[,16][i] = abs(team_1_entropy - team_2_entropy)
  
  ifelse(player_entropy[,14][i] == 'team 1' & team_1_entropy > team_2_entropy, player_entropy[,15][i] <- 'yes', 
         ifelse(player_entropy[,14][i] == 'team 2' & team_2_entropy > team_1_entropy, player_entropy[,15][i] <- 'yes', player_entropy[,15][i] <- 'no'))
  
  player_entropy[, "game_id"][i] <- i
  player_entropy[, "map_name"][i] <- filter(all_player_stats, game_id == i)[, 'map_name'][1]
  player_entropy[, 'map_type'][i] <- filter(all_player_stats, game_id == i)[, 'map_type'][1]
  
}




# With the table finished we can export it. Now, we can simply import it if we want to make changes.
write.csv(player_entropy, 'C:\\Users\\Christopher S Powell\\Desktop\\SFSU MS Data Science\\Math 448\\Overwatch Project\\player_entropy.csv', row.names = F)



# testing rate of my "initial hypothesis":

yes_count = 0
for (i in 1:dim(team_entropy)[1]){
  ifelse(team_entropy[,15][i] == 'yes', yes_count <- yes_count + 1, yes_count <- yes_count + 0)
  
}

yes_count
yes_count/3541

# It appears that 63.25% of games the winner has the higher sum of the total entropy of the medal stats. 

team_entropy <- player_entropy # renaming the table with a more apt name

# exporting the newly renamed table:
write.csv(team_entropy, 'C:\\Users\\Christopher S Powell\\Desktop\\SFSU MS Data Science\\Math 448\\Overwatch Project\\team_entropy.csv', row.names = F)




# import the table:

team_entropy <- read.csv('team_entropy.csv')



# I can now run some statistical models on the table: 

# Notes: Cannot use linear regression because my Y value is qualitative and not quantitative. 


# Changing some variables to factors:

team_entropy$game_winner <- as.factor(team_entropy$game_winner) # changing game winner to binary
contrasts(team_entropy$game_winner) # how did R code the factor levels: team 1 = 0, team 2 = 1
team_entropy$map_type <- as.factor(team_entropy$map_type) # map type does not seem to be valuable, and is also directly correlated to map_name, so this variable
# is removed from the model
team_entropy$winner_higher_entropy <- as.factor(team_entropy$winner_higher_entropy)


# first I'll split the data in half for the training and test subsets:

set.seed(11)
train = sample(1:dim(team_entropy)[1], dim(team_entropy)[1] / 2)
test <- -train
team_entropy.train <- team_entropy[train, ]
team_entropy.test <- team_entropy[test, ]




# Logistic regression: WITH ONLY THE STAT ENTROPY VARIABLES

glm.fit <- glm(game_winner ~ team_1_eliminations + team_1_healing_done + team_1_hero_damage_done + team_1_objective_kills + 
               team_1_objective_time + team_2_eliminations + team_2_healing_done + team_2_hero_damage_done + team_2_objective_kills + 
               team_2_objective_time, data = team_entropy, family = binomial)

summary(glm.fit)
coef(glm.fit) # estimates of beta values
summary(glm.fit)$coef # estimates of beta values, standard error, z value, p-values etc.

glm.probs=predict(glm.fit,type="response") # predict P(Y=1|X=each x)

glm.probs[1:10] # predicted p(x) for first 10 observations

contrasts(team_entropy$game_winner) # how did R code "Y=1" -> team 1 = 0, team 2 = 1

glm.pred=rep("team 1", 3541) # generate a 3541-dim vector with each element as "team 1", name this vector glm.pred (use team 1 since team 1 = 0)

glm.pred[glm.probs>.5] = "team 2" # if the predicted value > 0.5, assign predicted winner as 'team 2'

table(glm.pred, team_entropy$game_winner)

mean(glm.pred == team_entropy$game_winner) #true=1, false=0, return (# of true)/total, i.e., correct prediction rate (correctly predicts 68.8%)

cv.err = cv.glm(team_entropy, glm.fit) # LOOCV 

cv.err$delta # LOOCV estimate for the test MSE, the average of the n (n = 3541, number of observations) different test error estimates
# Returns a value of 19.62% for the CV estimate of the MSE.

cv.err.10 = cv.glm(team_entropy, glm.fit, K = 10) # k-fold CV, with k = 10
cv.err.10$delta # k-fold CV estimate for test MSE
# 19.67%, very similar to LOOCV

# Notes (after analysis): 


glm.pred_factors = rep(NA, 3541)
for (i in 1:length(glm.pred)){
  ifelse(glm.pred[i] == 'team 1', glm.pred_factors[i] <- 0, glm.pred_factors[i] <- 1)
  
}


team_entropy$game_winner <- as.factor(team_entropy$game_winner)

test_ROC = roc(team_entropy$game_winner ~ glm.pred_factors, plot = TRUE, print.auc = TRUE)


# Logistic regression: INCLUDING MORE VARIABLES, WORSE MODEL (try using test and train set here)

glm.fit.2 <- glm(game_winner ~.-map_type -game_id -map_name, data = team_entropy, family = binomial)

summary(glm.fit.2)
coef(glm.fit.2) # estimates of beta values
summary(glm.fit.2)$coef # estimates of beta values, standard error, z value, p-values etc.

glm.probs.2 = predict(glm.fit.2,type="response") # predict P(Y=1|X=each x)

glm.probs.2[1:10] # predicted p(x) for first 10 observations

contrasts(team_entropy$game_winner) # how did R code "Y=1" -> team 1 = 0, team 2 = 1

glm.pred.2 = rep("team 1", 3541) # generate a 3541-dim vector with each element as "team 1", name this vector glm.pred (use team 1 since team 1 = 0)

glm.pred.2[glm.probs.2>.5] = "team 2" # if the predicted value > 0.5, assign predicted winner as 'team 2'

table(glm.pred.2, team_entropy$game_winner)

mean(glm.pred.2 == team_entropy$game_winner) #true=1, false=0, return (# of true)/total, i.e., correct prediction rate (correctly predicts 68.3%)

cv.err.2 = cv.glm(team_entropy, glm.fit.2) # LOOCV

cv.err.2$delta # LOOCV estimate for the test MSE, the average of the n (n = 3541, number of observations) different test error estimates
# Returns a value of 19.64% for the CV estimate of the test error. Not too bad!

cv.err.10.2 = cv.glm(team_entropy, glm.fit, K = 10) # k-fold CV, with k = 10
cv.err.10.2$delta # k-fold CV estimate for test MSE
# 19.77%, very similar to LOOCV

table(glm.pred.2, team_entropy$game_winner) # Correctly predicted (1289+1132)/3541



#LDA:

lda.fit = lda(game_winner ~  team_1_eliminations + team_1_healing_done + team_1_hero_damage_done + team_1_objective_kills + 
                team_1_objective_time + team_2_eliminations + team_2_healing_done + team_2_hero_damage_done + team_2_objective_kills + 
                team_2_objective_time, data = team_entropy)
lda.fit
plot(lda.fit) # bar graph showing ??? ASK PROF

# (may not end up including LDA) 





# Lasso:

grid <- 10 ^ seq(4, -2, length = 100) # lambda matrix

train.mat <- model.matrix(game_winner ~. -map_name -game_id -map_type, data = team_entropy.train) 
test.mat <- model.matrix(game_winner ~. -map_name -game_id -map_type, data = team_entropy.test) # test and training matrices

fit.lasso <- glmnet(train.mat, team_entropy.train$game_winner, alpha = 1, lambda = grid, thresh = 1e-12, family = 'binomial') # lasso fit

cv.lasso <- cv.glmnet(train.mat, team_entropy.train$game_winner, alpha = 1, lambda = grid, thresh = 1e-12, family = 'binomial') # using CV to find the best lambda

bestlam.lasso <- cv.lasso$lambda.min # saving the value of the best lambda 

bestlam.lasso # 0.01

pred.lasso <- predict(fit.lasso, s = bestlam.lasso, newx = test.mat, type = 'class') # testing the model on the test half of the data

mean(pred.lasso == team_entropy.test$game_winner) # returns 0.6657, which I assume is 66.79% accuracy rate on the test data? Not very good

summary(fit.lasso)

coefficients <- predict(fit.lasso, s = bestlam.lasso, newx = test.mat, type = 'coef')
coefficients


# Single Decision tree: WITH ONLY THE STAT ENTROPY VARIABLES

tree.fit = tree(game_winner ~ team_1_eliminations + team_1_healing_done + team_1_hero_damage_done + team_1_objective_kills + 
                  team_1_objective_time + team_2_eliminations + team_2_healing_done + team_2_hero_damage_done + team_2_objective_kills + 
                  team_2_objective_time, data = team_entropy)

# "NAs introduced by coercion" error came up when map_name was included, so that variable was removed from this model

summary(tree.fit)

plot(tree.fit)
text(tree.fit, pretty=0)

# This initial tree seems to only focus on objective kills for each team, which is an interesting choice. Classification error rate is 36.2%


# Single Decision tree: more variables

tree.fit.2 = tree(game_winner ~.-map_type -game_id -map_name -entropy_difference, data = team_entropy)

# "NAs introduced by coercion" error came up when map_name was included, so that variable was removed from this model

summary(tree.fit.2)

plot(tree.fit.2)
text(tree.fit.2, pretty=0)

# This tree looks at other factors, and achieves higher accuracy: reports a classification error rate of 30.7%




# Random forest: (including all predictors)

set.seed(1) # important since random forest is random

random.forest.model = randomForest(game_winner ~.-map_name, data = team_entropy.train, mtry=6, importance=TRUE) # copied from Ch. 8 lab

yhat.rf = predict(random.forest.model, newdata = team_entropy.test)

mean(yhat.rf == team_entropy.test$game_winner) # 81.8% correct prediction rate

importance(random.forest.model)
varImpPlot(random.forest.model)


# leaving out game_id, map_name, entropy_difference:

set.seed(2) # important since random forest is random

team_entropy.rf = randomForest(game_winner ~.-map_type -game_id -map_name -entropy_difference, data = team_entropy.train, mtry=6, importance=TRUE) # copied from Ch. 8 lab

predict.rf = predict(team_entropy.rf, newdata = team_entropy.test)

mean(predict.rf == team_entropy.test$game_winner) # 83.1% correct prediction rate

importance(team_entropy.rf)
varImpPlot(team_entropy.rf)





# Boosting

# The model is being very stubborn about game_winner being in {0,1}, so going to manually change the table the table here:
set.seed(15)
team_entropy_boost = team_entropy # copying the main table to make a model-specific table for boosting, with game_winner encoded as 0 or 1

team_entropy_boost$class=rep(0,nrow(team_entropy_boost)) # new column for the response coded as 0, 1

team_entropy_boost$class[team_entropy_boost$game_winner=="team 1"]=1 # change the 0's to 1's for team 1

team_entropy_boost$game_winner=NULL # removing the old response column

team_entropy_boost$winner_higher_entropy = as.factor(team_entropy_boost$winner_higher_entropy) # changing to factor

team_entropy_boost.train <- team_entropy_boost[train, ] # generating test and train sets, using the same sets as for all other models
team_entropy_boost.test <- team_entropy_boost[test, ]

team_entropy.boost = gbm(class ~.-map_name- game_id -map_type, data = team_entropy_boost.train, distribution = "bernoulli", n.minobsinnode = 6, n.trees = 5000, interaction.depth = 10, shrinkage = 0.1)

summary(team_entropy.boost) # relative influence of predictors

predict.boost = predict(team_entropy.boost, newdata = team_entropy_boost.test, type = "response") # predict on the test set

ytest.pred=rep(0,length(test)) # generate the vector of proper length with all 0's for the class

ytest.pred[predict.boost>0.5]=1 # switch 0's to 1's based on probabilities, since GBM outputs a probability

table(ytest.pred,team_entropy_boost.test$class) # generate the confusion matrix -> n.minobsinnode = 6, n.trees = 5000, interaction.depth = 10, shrinkage = 0.1 -> 708+782/1771 = 84.1%




#team_entropy_boost$game_winner = as.character(team_entropy_boost$game_winner) # changing to a character for the following replacement loop to work
#for (i in 1:dim(team_entropy_boost)[1]){
#  ifelse(team_entropy_boost[,14][i] == "team 1", team_entropy_boost[,14][i] <- 0, team_entropy_boost[,14][i] <- 1) # team 1 = 0, team 2 = 1
  

# Making new train and test sets for this new "game_winner in {0,1}" table:

#set.seed(11)
#train = sample(1:dim(team_entropy_boost)[1], dim(team_entropy_boost)[1] / 2)
#test <- -train
#team_entropy_boost.train <- team_entropy_boost[train, ]
#team_entropy_boost.test <- team_entropy_boost[test, ]



#set.seed(1)

#team_entropy.boost = gbm(game_winner ~.-map_name-game_id-map_name -entropy_difference, data = team_entropy_boost.train, distribution = "bernoulli", n.trees = 5000, interaction.depth = 4)

#summary(team_entropy.boost)

#predict.boost = predict(team_entropy.boost, newdata = team_entropy_boost.test, type = "response")

#mean(predict.boost == team_entropy_boost.test$game_winner) # 83.2% correct prediction rate??? I thought I got that initially but now the code won't run







# Before continuing, I will export the player stat tables as they currently are and then add code to read them back in. This way, I
# don't need to re-execute all of my code every time the global environment is reset and instead I can read in these tables:

write.csv(player_stats_2018, 'C:\\Users\\Christopher S Powell\\Desktop\\SFSU MS Data Science\\Math 448\\Overwatch Project\\player_stats_2018.csv', row.names = TRUE)
write.csv(player_stats_2019, 'C:\\Users\\Christopher S Powell\\Desktop\\SFSU MS Data Science\\Math 448\\Overwatch Project\\player_stats_2019.csv', row.names = TRUE)
write.csv(player_stats_2020, 'C:\\Users\\Christopher S Powell\\Desktop\\SFSU MS Data Science\\Math 448\\Overwatch Project\\player_stats_2020.csv', row.names = TRUE)
write.csv(all_player_stats, 'C:\\Users\\Christopher S Powell\\Desktop\\SFSU MS Data Science\\Math 448\\Overwatch Project\\all_player_stats.csv', row.names = TRUE)


player_stats_2018 <- read.csv('player_stats_2018.csv') # Read them back in, only needs to be used if the global environment is cleared
player_stats_2019 <- read.csv('player_stats_2019.csv')
player_stats_2020 <- read.csv('player_stats_2020.csv')
all_player_stats <- read.csv('all_player_stats.csv')


# THE CODE GRAVEYARD, WHERE UNUSED CODE GOES TO DIE (OR POTENTIALLY BE USED IN THE FUTURE):

'
# I think it will be useful to have certain names and values in separate tables in order to iterate through by these values later on

# Creating a table of all the map names:
all_map_names <- map_data %>% select(map_name) %>% distinct


# Creating a table of all the team names:
all_team_names <- distinct(bind_rows(select(all_2018_data, team), select(all_2019_data, team), select(all_2020_data, team)))


# Creating a table of all the player names:
all_player_names <- distinct(bind_rows(select(all_2018_data, player), select(all_2019_data, player), select(all_2020_data, player)))

# Changing each of these tables into a list, in case that is useful for iteration:
match_id_list = as.vector(distinct_match_ids[, 1])
map_name_list = as.vector(all_map_names[, 1])
team_name_list = as.vector(all_team_names[, 1])
player_name_list = as.vector(all_player_names[, 1])
'



'
# A function to combine these lists into one list: (ended up not using this)
CombineListsAsOne <-function(list1, list2, list3){
  n <- c()
  for(x in list1){
    n<-c(n, x)
  }
  for(y in list2){
    n<-c(n, y)
  }
  for(z in list3){
    n<-c(n, z)
  }
  return(n)
}

# Here is the list of start times, which will be used to add a new column for Game Id since each start time corresponds to a game:
start_times = CombineListsAsOne(all_times_2018, all_times_2019, all_times_2020)
'




'

# First, we need to make sure that the games are in the same order as our other tables, so we reorganize the rows of map_data:

map_data = map_data %>% arrange(match_id, map_name)

# Next, using similar code to above, we can add a game_id column to map_data that should line up with our other tables:

map_data[,'game_id'] <- NA # adding an empty column for game_id

map_data[, 26][1] <- 1 # manually entering the first game_id

for(i in 2:length(map_data[, 1])){
  
  ifelse(map_data[, 4][i] == map_data[, 4][i-1] && map_data[, 9][i] == map_data[, 9][i-1], map_data[, 26][i] <- map_data[, 26][i-1], 
         map_data[, 26][i] <- (map_data[, 26][i-1] + 1))
  
}

# Now map_data has a corresponding game_id column that matches our other data frames



# Before continuing, I want to split the map_data up into 3 tables corresponding to each year and make sure game_id is lining up

game_ids_2018 <- seq(1, 1109, by = 1) # list of the game_ids to subset the map_data table for 2018 games
map_data_2018 <- subset(map_data, game_id %in% game_ids_2018)

game_ids_2019 <- seq(1110, 2476, by = 1)
map_data_2019 <- subset(map_data, game_id %in% game_ids_2019)

game_ids_2020 = seq(2477, 3616, by = 1)
map_data_2020 <- subset(map_data, game_id %in% game_ids_2020)

# Game_id values seem to line up for the first two years but not 2020

'













library(baseballr)
library(dplyr)
library(readr)
library(archive) # for writing zip files

major_dat <- list()
triple_a_dat <- list()


for (s in 2025) {
  season <- mlb_schedule(season = s, level_ids = 1)
  major_dat[[s - 2024]] <- season |>
    filter(series_description != "Spring Training" & 
             series_description != "MLB All-Star Game" & 
             series_description != "Exhibition Game" & 
             series_description != "Exhibition" & 
             series_description != "International Exhibition" & 
             series_description != "World Baseball Classic Exhibition"
    )
  
  season <- mlb_schedule(season = s, level_ids = 11)
  triple_a_dat[[s - 2024]] <- season 
  
}

major_dat <- do.call('rbind', major_dat)
triple_a_dat <- do.call('rbind', triple_a_dat)

all_dat <- rbind(major_dat, triple_a_dat)
game_pks <- all_dat$game_pk

pitch_list <- vector("list")
games <- game_pks
iter <- length(games)
for (i in 1:iter) {
  checkpoints <- floor(seq(from = iter*0.05, to = iter, length.out = 20))
  if (i %in% checkpoints) {
    pct <- round((i /iter) * 100)
    cat(sprintf("Progress: %d%% reached\n", pct))
  }
  # see if we can pull data for the ith game
  game_dat <- tryCatch({
    baseballr::mlb_pbp(games[i])
  }, error = function(e) {
    message(paste0("Error on index ", i, " (Game PK: ", games[i], "): ", conditionMessage(e)))
    return(NULL) 
  })
  
  if(!is.null(game_dat)){
    game_dat$league <- ifelse(games[i] %in% major_dat$game_pk, 1, 0)
    
    starters <- game_dat |>
      select(about.inning, matchup.pitcher.fullName) |>
      filter(about.inning == 1) |>
      distinct() |>
      mutate(starter = 1)
    
    game_dat <- game_dat |>
      filter(isPitch == T) |>
      group_by(matchup.pitcher.fullName) |>
      arrange(lubridate::as_datetime(endTime)) |>
      mutate(pitch_count = 1:n()) |>
      ungroup()
    
    game_dat <- game_dat |> left_join(starters)
    
    game_dat$starter[is.na(game_dat$starter)] <- 0
    
    # extract features
    features <- c('game_pk', 'game_date', 'league','about.isTopInning',
                  'isPitch', 'pitchNumber', 'pitch_count', 
                  'details.isInPlay', 'details.isOut','details.isStrike', 
                  'home_team', 'away_team','starter',
                  'matchup.batSide.code', 'matchup.pitchHand.code',
                  'matchup.batter.id', 'matchup.pitcher.id',
                  'matchup.batter.fullName', 'matchup.pitcher.fullName', 
                  'atBatIndex', 'result.event','result.eventType', 'result.rbi')
    
    game_dat <- game_dat[,features]
    game_dat <- game_dat[game_dat$isPitch == T,]
    game_dat <- game_dat |>
      dplyr::group_by(atBatIndex) |>
      filter(pitchNumber == max(pitchNumber)) |>
      ungroup()
    pitch_list[[(length(pitch_list) + 1)]] <- game_dat
  }

}

pitch_dat <- do.call('rbind', pitch_list)
rm(tripple_a_dat);rm(major_dat);rm(season);rm(game_pks)
rm(i);rm(pct);rm(s);rm(features);rm(checkpoints); rm(iter)
# output is saved to all_data_2025.csv.zip
readr::write_csv(pitch_dat, 'data/all_data_2025.csv.zip')
rm(list = ls())

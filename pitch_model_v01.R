library(baseballr)
library(tidyverse)
library(mvtnorm)
library(glmnet)
library(patchwork)
library(stringr)
theme_set(theme_bw())

# Function to scrape Statcast data by week
annual_statcast_query <- function(season) {
  
  data_base_column_types <- read_csv("https://app.box.com/shared/static/q326nuker938n2nduy81au67s2pf9a3j.csv")
  
  dates <- seq.Date(as.Date(paste0(season, '-03-01')),
                    as.Date(paste0(season, '-12-01')), by = '4 days')
  
  date_grid <- tibble::tibble(start_date = dates, 
                              end_date = dates + 3)
  
  safe_savant <- purrr::safely(scrape_statcast_savant)
  
  payload <- purrr::map(.x = seq_along(date_grid$start_date), 
                        ~{message(paste0('\nScraping week of ', date_grid$start_date[.x], '...\n'))
                          
                          payload <- safe_savant(start_date = date_grid$start_date[.x], 
                                                 end_date = date_grid$end_date[.x], type = 'pitcher')
                          
                          return(payload)
                        })
  
  payload_df <- purrr::map(payload, 'result')
  
  number_rows <- purrr::map_df(.x = seq_along(payload_df), 
                               ~{number_rows <- tibble::tibble(week = .x, 
                                                               number_rows = length(payload_df[[.x]]$game_date))}) %>%
    dplyr::filter(number_rows > 0) %>%
    dplyr::pull(week)
  
  payload_df_reduced <- payload_df[number_rows]
  
  payload_df_reduced_formatted <- purrr::map(.x = seq_along(payload_df_reduced), 
                                             ~{cols_to_transform <- c("fielder_2", "pitcher_1", "fielder_2_1", "fielder_3",
                                                                      "fielder_4", "fielder_5", "fielder_6", "fielder_7",
                                                                      "fielder_8", "fielder_9")
                                             
                                             df <- purrr::pluck(payload_df_reduced, .x) %>%
                                               dplyr::mutate_at(.vars = cols_to_transform, as.numeric) %>%
                                               dplyr::mutate_at(.vars = cols_to_transform, function(x) {
                                                 ifelse(is.na(x), 999999999, x)
                                               })
                                             
                                             character_columns <- data_base_column_types %>%
                                               dplyr::filter(class == "character") %>%
                                               dplyr::pull(variable)
                                             
                                             numeric_columns <- data_base_column_types %>%
                                               dplyr::filter(class == "numeric") %>%
                                               dplyr::pull(variable)
                                             
                                             integer_columns <- data_base_column_types %>%
                                               dplyr::filter(class == "integer") %>%
                                               dplyr::pull(variable)
                                             
                                             df <- df %>%
                                               dplyr::mutate_if(names(df) %in% character_columns, as.character) %>%
                                               dplyr::mutate_if(names(df) %in% numeric_columns, as.numeric) %>%
                                               dplyr::mutate_if(names(df) %in% integer_columns, as.integer)
                                             
                                             return(df)
                                             })
  
  combined <- payload_df_reduced_formatted %>%
    dplyr::bind_rows()
  
  combined
}

# Format scrapped Statcast data into a dataframe, add Spray Angle and VAA.
format_append_statcast <- function(df) {
  
  # function for appending new variables to the data set
  
  additional_info <- function(df) {
    
    # apply additional coding for custom variables
    
    df$hit_type <- with(df, ifelse(type == "X" & events == "single", 1,
                                   ifelse(type == "X" & events == "double", 2,
                                          ifelse(type == "X" & events == "triple", 3, 
                                                 ifelse(type == "X" & events == "home_run", 4, NA)))))
    
    df$hit <- with(df, ifelse(type == "X" & events == "single", 1,
                              ifelse(type == "X" & events == "double", 1,
                                     ifelse(type == "X" & events == "triple", 1, 
                                            ifelse(type == "X" & events == "home_run", 1, NA)))))
    
    df$fielding_team <- with(df, ifelse(inning_topbot == "Bot", away_team, home_team))
    
    df$batting_team <- with(df, ifelse(inning_topbot == "Bot", home_team, away_team))
    
    df <- df %>%
      dplyr::mutate(barrel = ifelse(launch_angle <= 50 & launch_speed >= 98 & launch_speed * 1.5 - launch_angle >= 117 & launch_speed + launch_angle >= 124, 1, 0))
    
    df <- df %>%
      dplyr::mutate(spray_angle = round(
        (atan(
          (hc_x-125.42)/(198.27-hc_y)
        )*180/pi*.75)
        ,1)
      )
    
    df <- df %>%
      dplyr::mutate(vy_f = -sqrt(vy0^2 - (2 * ay *(50 - (17/12)))),
             t = (vy_f-vy0) / ay,
             vz_f = vz0 + (az * t),
             vaa = -atan(vz_f/vy_f) * (180 / pi))
    
    df <- df %>%
      dplyr::filter(!is.na(game_year))
    
    return(df)
  }
  
  df <- df %>%
    additional_info()
  
  df$game_date <- as.character(df$game_date)
  
  df <- df %>%
    dplyr::arrange(game_date)
  
  df <- df %>%
    dplyr::filter(!is.na(game_date))
  
  df <- df %>%
    dplyr::ungroup()
  
  df <- df %>%
    dplyr::select(setdiff(names(.), c("error")))
  
  return(df)
}

payload_statcast <- annual_statcast_query(2021)
df <- format_append_statcast(df = payload_statcast)
df <- as.data.frame(df)

# Saving time by scraping if code crashes
# write_csv(df, "2021_statcast_pitch.csv")
df <- read_csv("baseball/data/2021_statcast_pitch.csv")

# Get IDs, clean names, drop deprecated columns
people <- read_csv("https://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv")
mlbam_bat <- people %>%
  mutate(batter_name = paste(name_first, name_last)) %>%
  select(batter_name, key_mlbam, key_fangraphs)
names(mlbam_bat)[names(mlbam_bat) == 'key_mlbam'] <- 'batter'
names(mlbam_bat)[names(mlbam_bat) == 'key_fangraphs'] <- 'fg_bat'
df1 <- left_join(df, mlbam_bat, by ="batter")
mlbam_pitch <- people %>%
  mutate(pitcher_name = paste(name_first, name_last)) %>%
  select(pitcher_name, key_mlbam, key_fangraphs)
names(mlbam_pitch)[names(mlbam_pitch) == 'key_mlbam'] <- 'pitcher'
names(mlbam_pitch)[names(mlbam_pitch) == 'key_fangraphs'] <- 'fg_pitch'
df1 <- left_join(df1, mlbam_pitch, by ="pitcher")
df1 = subset(df1, select = -c(player_name))
df1 <- df1 %>%
  relocate(batter_name, .after = batter) %>%
  relocate(fg_bat, .after = batter) %>%
  relocate(pitcher_name, .after = pitcher) %>%
  relocate(fg_pitch, .after = pitcher)
df2 <- df1 %>%
  filter(game_date > '2021-03-31') %>%
  filter(game_date < '2021-10-04')
df3 = subset(df2, select = -c(spin_dir, spin_rate_deprecated, break_angle_deprecated,
                              break_length_deprecated, tfs_deprecated, tfs_zulu_deprecated))


# Get heights
heights <- read_csv("https://raw.githubusercontent.com/chadwickbureau/baseballdatabank/master/core/People.csv") %>%
  mutate(player_name = paste(nameFirst, nameLast),
         height = height / 12) %>%
  select(bbrefID, height, weight, throws) %>%
  distinct(bbrefID, .keep_all = TRUE)

heights_main <- people %>%
  select(key_mlbam, key_bbref) 

names(heights_main)[names(heights_main) == 'key_bbref'] <- 'bbrefID'
heights_main <- left_join(heights_main, heights, by ="bbrefID")
names(heights_main)[names(heights_main) == 'key_mlbam'] <- 'pitcher'

df4 <- left_join(df3, heights_main, by ="pitcher")

df4 = subset(df4, select = -c(bbrefID, throws, weight))
df4 <- df4 %>%
  relocate(height, .after = pitcher_name)

# Estimate Arm Angle
library(REdaS)
df5 <- df4 %>%
  mutate(adj = (release_pos_z - height*0.7),
         opp = abs(release_pos_x), # dist. from middle of chest to arm
         hyp = sqrt(opp^2 + adj^2),
         arm_angle = rad2deg(acos((adj^2 + hyp^2 - opp^2) / (2*(adj*hyp)))),
         arm_slot = case_when(arm_angle >= 90 ~ "Submarine",
                              arm_angle >= 70 & arm_angle < 90 ~ "Side Arm",
                              arm_angle < 70 & arm_angle >= 30 ~ "Three-Quarters",
                              arm_angle < 30 ~ "Overhand"))
df5 = subset(df5, select = -c(adj, opp, hyp))
df5 <- df5 %>%
  relocate(arm_angle, .after = height) %>%
  relocate(arm_slot, .after = arm_angle)

# Scrape Fangraphs data
data_2021 <- fg_pitch_leaders(2021, 2021, qual=0, ind=0)
data_2021 <- data_2021 %>%
  select(playerid, FIP) %>% 
  mutate(playerid = as.numeric(playerid))
names(data_2021)[names(data_2021) == 'playerid'] <- 'fg_pitch'

df5 <- left_join(df5, data_2021, by="fg_pitch")
df5 <- df5 %>%
  relocate(FIP, .after = pitcher)

df6 <- run_expectancy_code(df5, level="pitch")
try(linear_weights_savant(df6, level="pitch"))
lin_weights <- linear_weights_savant(df6, level="pitch")

outevent <- c("field_out", "grounded_into_double_play",
              "field_error", "force_out", "fielders_choice", "sac_fly", "double_play", 
              "fielders_choice_out","sac_fly_double_play", "triple_play")

df7 <- df6

unique(df7$events)

# Linear weights
df7$lin_weight = NA
df7$lin_weight[df7$type == "B"] = 0.05 # Every ball gets Linear Weight Value of 0.05
df7$lin_weight[df7$type == "S"] = -0.05 # Every strike gets Linear Weight Value of -0.05
df7$lin_weight[df7$events %in% outevent] = -0.24 # All non-strikeout out events get -0.24
df7$lin_weight[df7$events == "hit_by_pitch"] = 0.39
df7$lin_weight[df7$events == "single"] = 0.47
df7$lin_weight[df7$events == "double"] = 0.78
df7$lin_weight[df7$events == "triple"] = 1.06
df7$lin_weight[df7$events == "home_run"] = 1.37



# K = 5.153e-03

# # Inferred spin using Alan Nathan calculator
# K <-  5.383E-03
# z_constant <- 32.174
# df6 <- df6 %>% 
#   mutate(yR = 60 - release_extension) %>% 
#   mutate(tR =(-vy0-sqrt(vy0^2-2*ay*(50-yR)))/ay) %>% 
#   mutate(vxR = vx0+ax*tR, 
#          vyR = vy0+ay*tR,
#          vzR = vz0+az*tR
#   ) %>% 
#   mutate(tf = (-vyR-sqrt(vyR^2-2*ay*(yR-17/12)))/ay) %>% 
#   mutate(vxbar = (2*vxR+ax*tf)/2, 
#          vybar = (2*vyR+ay*tf)/2,
#          vzbar = (2*vzR+az*tf)/2 
#   ) %>% 
#   mutate(vbar =sqrt(vxbar^2+vybar^2+vzbar^2)) %>% 
#   mutate(adrag = -(ax*vxbar+ay*vybar+(az+z_constant)*vzbar)/vbar) %>% 
#   mutate(amagx = ax+adrag*vxbar/vbar, 
#          amagy = ay+adrag*vybar/vbar,
#          amagz = az+adrag*vzbar/vbar+z_constant 
#   ) %>%  
#   mutate(amag = sqrt(amagx^2 + amagy^2 + amagz^2)) %>% 
#   mutate(Cl = amag/(K*vbar^2))%>% 
#   mutate(s = 0.166*log(0.336/(0.336-Cl))) %>% 
#   mutate(spinT = 78.92*s*vbar) %>% 
#   mutate(spin_efficiency = spinT/release_spin_rate)

season_mlb <- df7%>%
  filter(!is.na(lin_weight))

# Clean up
rm(mlbam_bat, mlbam_pitch, people, outevent)

# Fastballs
p_avgs <- season_mlb%>%
  filter(pitch_type %in% c("FF", "SI"))%>%
  group_by(pitcher_name)%>%
  summarise(avg_velo = mean(release_speed,na.rm=TRUE),
            avg_vmov = mean(pfx_z,na.rm=TRUE),
            avg_hmov = mean(pfx_x,na.rm=TRUE))

season_mlb2 <- left_join(p_avgs, season_mlb, by = "pitcher_name")

# Pitch Differentials from Fastballs
season_mlb3 <- season_mlb2%>%
  mutate(
    velo_diff = ifelse(
      pitch_type %in% c("SL", "CB", "FS", "KC", "CH", "FC"), 
      release_speed - avg_velo, NA),
    hmov_diff = ifelse(
      pitch_type %in% c("SL", "CB", "FS", "KC", "CH", "FC"), 
      pfx_x - avg_hmov, NA),
    vmov_diff = ifelse(
      pitch_type %in% c("SL", "CB", "FS", "KC", "CH", "FC"), 
      pfx_z - avg_vmov, NA))

season_mlb3$velo_diff[is.na(season_mlb3$velo_diff)] = season_mlb3$release_speed[is.na(season_mlb3$velo_diff)] - 
  season_mlb3$avg_velo[is.na(season_mlb3$velo_diff)]

season_mlb3$hmov_diff[is.na(season_mlb3$hmov_diff)] = season_mlb3$pfx_x[is.na(season_mlb3$hmov_diff)] - 
  season_mlb3$avg_hmov[is.na(season_mlb3$hmov_diff)]

season_mlb3$vmov_diff[is.na(season_mlb3$vmov_diff)] = season_mlb3$pfx_z[is.na(season_mlb3$vmov_diff)] - 
  season_mlb3$avg_vmov[is.na(season_mlb3$vmov_diff)]


rm(season_mlb, season_mlb2)

season_mlb4 <- season_mlb3%>%
  filter(!is.na(lin_weight),
         !is.na(p_throws),
         !is.na(stand),
         !is.na(release_spin_rate),
         !is.na(release_extension),
         !is.na(spin_axis))%>%
  mutate(release_pos_x_adj = ifelse(p_throws == "R", release_pos_x, -release_pos_x),
         pfx_x_adj = ifelse(p_throws == "R", pfx_x, -pfx_x))

colSums(is.na(season_mlb4)) # Check for NAs

season_mlb5 <- season_mlb4 %>%
  filter(!is.na(hmov_diff),
         !is.na(pfx_x_adj))

rm(season_mlb4)

library(Boruta)
library(zoo)
library(psych)
library(lubridate)
library(randomForest)

feature_selection <- function(pitch_data){

  if(unique(pitch_data$pitch_type %in% c("FF", "SI"))){ #feature selection for fastballs which does not include diff variables
    rr_data = pitch_data%>%
      filter(p_throws == "R",
             stand =="R",
             !is.na(release_spin_rate),
             !is.na(release_extension)) 
    
    rr_data_sampled = rr_data[sample(nrow(rr_data), size = nrow(rr_data)*.25),]
    
    Boruta_PitchType <- Boruta(lin_weight ~ release_speed + release_pos_x_adj + release_extension+
                                 release_pos_z + pfx_x_adj + pfx_z + plate_x + plate_z + release_spin_rate + spin_axis + vaa,
                               data = rr_data_sampled)
    
    print(Boruta_PitchType)
    
    feature_importance <- data.frame(attStats(Boruta_PitchType))
    
    feature_importance%>%
      select(meanImp, decision)%>%
      arrange(desc(meanImp)) 
    
    Features <- getSelectedAttributes(Boruta_PitchType, withTentative = F) #return the list of confirmed important variables for use in the model
    
    return(Features)
    
    
  }else{
    
    rr_data = pitch_data%>%
      filter(p_throws == "R",
             stand =="R",
             !is.na(release_spin_rate),
             !is.na(release_extension)) 
    
    rr_data_sampled = rr_data[sample(nrow(rr_data), size = nrow(rr_data)*.25),]
    
    Boruta_PitchType <- Boruta(lin_weight ~ release_speed + release_pos_x_adj + release_extension+
                                 release_pos_z + pfx_x_adj + pfx_z + plate_x + plate_z + release_spin_rate + spin_axis +
                                 velo_diff + hmov_diff + vmov_diff,
                               data = rr_data_sampled)
    
    print(Boruta_PitchType)
    
    feature_importance <- data.frame(attStats(Boruta_PitchType))
    
    feature_importance%>%
      select(meanImp, decision)%>%
      arrange(desc(meanImp)) 
    
    Features <- getSelectedAttributes(Boruta_PitchType, withTentative = F) #return the list of confirmed important variables for use in the model
    
    return(Features)
    
  }}

# Model Validation

rmse = function(m, o){ 
  sqrt(mean((m - o)^2))
}

rf_model <- function(df, features) { 
  
  features1 <- append(features, c("lin_weight"))
  df1 <- df[,features1]
  
  randomForest(lin_weight ~ ., data = df1, importance = TRUE, na.action=na.roughfix)
  
}

validate <- function(data, features){ 
  #(ff_data, FF_features)
  feats_only <- data[,c("p_throws", "stand", "lin_weight", features)] 
  data_shuffled <- feats_only[sample(nrow(data), replace = F),]
  
  # train
  suppressWarnings(nested <- 
                     data_shuffled[1:round(nrow(data_shuffled)*.7),]%>% 
                     nest(-p_throws, -stand)%>% 
                     rename(myorigdata = data))
  
  # test
  suppressWarnings(new_nested <- 
                     data_shuffled[1:round(nrow(data_shuffled)*.3),]%>%
                     nest(-p_throws, -stand)%>% 
                     rename(mynewdata = data))
  
  featurelist <- rep(list(features),length(nested$myorigdata))
  
  suppressWarnings(one <- nested %>% 
                     mutate(my_model = map2(myorigdata, featurelist, rf_model)))
  
  two <- one%>%
    full_join(new_nested, by = c("p_throws", "stand"))
  
  two$my_new_pred = NA
  for(i in 1:nrow(two)){
    
    two$my_new_pred[i] = as_tibble(predict(two$my_model[[i]], two$mynewdata[[i]][-1]))
    
  }
  
  name <- deparse(substitute(data))
  
  assign(paste(name, "_models_val", sep=""), two, envir = globalenv()) 
  
  three = two
  
  four <- three%>%
    select(p_throws,stand, mynewdata, my_new_pred)
  
  five<- four%>%
    unnest(c(mynewdata, my_new_pred))
  
  six <- five%>%
    rename(preds = my_new_pred)
  
  predictions = six
  
  print("RMSE:")
  print(round(rmse(predictions$preds, predictions$lin_weight), digits = 5)) 
  
  return(predictions) 
}

application <- function(data, features){
  
  feats_only <- data[,c("p_throws", "stand", "lin_weight", features)] 
  
  # train
  suppressWarnings(nested <- feats_only%>% 
                     nest(-p_throws, -stand)%>% 
                     rename(myorigdata = data))
  
  # test
  suppressWarnings(new_nested <- feats_only%>% 
                     nest(-p_throws, -stand)%>% 
                     rename(mynewdata = data))
  
  featurelist <- rep(list(features),length(nested$myorigdata))
  
  suppressWarnings(one <- nested %>% 
                     mutate(my_model = map2(myorigdata, featurelist, rf_model)))
  
  two <- one%>%
    full_join(new_nested, by = c("p_throws", "stand")) 
  
  two$my_new_pred = NA
  for(i in 1:nrow(two)){
    
    two$my_new_pred[i] = as_tibble(predict(two$my_model[[i]], two$mynewdata[[i]][-1]))
    
  }
  
  name <- deparse(substitute(data))
  
  assign(paste(name, "_models_total", sep=""), two, envir = globalenv())
  
  three = two
  
  four <- three%>%
    select(p_throws, stand, mynewdata, my_new_pred)
  
  five<- four%>%
    unnest(c(mynewdata, my_new_pred))
  
  six <- five%>%
    rename(preds = my_new_pred)
  
  predictions = six
  
  print("RMSE:")
  print(round(rmse(predictions$preds, predictions$lin_weight), digits = 5)) #print overall RMSE for data
  
  return(predictions) #return predictions
}

#Get the features for our models for each pitch type group using our function

# 4-Seam Fastballs
ff_data <- season_mlb5%>%
  filter(pitch_type == "FF")
FF_features <- feature_selection(ff_data)

# Sinkers

si_data <- season_mlb5%>%
  filter(pitch_type == "SI")
Si_features <- feature_selection(si_data)

# Cutters

fc_data <- season_mlb5%>%
  filter(pitch_type == "FC")
FC_features <- feature_selection(fc_data)

# Changeups and Splitters

ch_data <- season_mlb5%>%
  filter(pitch_type %in% c("CH", "FS"))
CH_features <- feature_selection(ch_data)

# Sliders
sl_data <- season_mlb5%>%
  filter(pitch_type == "SL")
SL_features <- feature_selection(sl_data)

# Curveballs

cb_data <- season_mlb5%>%
  filter(pitch_type %in% c("CU", "KC"))
CB_features <- feature_selection(cb_data)

# Test/Train

ff_w_preds_val <- validate(ff_data, FF_features)
si_w_preds_val <- validate(si_data, Si_features)
fc_w_preds_val <- validate(fc_data, FC_features)
ch_w_preds_val <- validate(ch_data, CH_features)
sl_w_preds_val <- validate(sl_data, SL_features)
cb_w_preds_val <- validate(cb_data, CB_features)

importance(ff_data_models_val$my_model[[1]])
varImpPlot(ff_data_models_val$my_model[[1]])
ff_data_models_val$my_model[1]
importance(ch_data_models_val$my_model[[1]])
varImpPlot(ch_data_models_val$my_model[[1]])
ch_data_models_val$my_model[1]

needed_cols = c("lin_weight", "preds")

total_preds_val <- rbind(ff_w_preds_val[,needed_cols], si_w_preds_val[,needed_cols], fc_w_preds_val[,needed_cols], 
                         ch_w_preds_val[,needed_cols], sl_w_preds_val[,needed_cols], cb_w_preds_val[,needed_cols])

# Total RMSE Validation
rmse(total_preds_val$lin_weight, total_preds_val$preds)

# Application of model
ff_w_preds <- application(ff_data, FF_features)
si_w_preds <- application(si_data, Si_features)
fc_w_preds <- application(fc_data, FC_features)
ch_w_preds <- application(ch_data, CH_features)
sl_w_preds <- application(sl_data, SL_features)
cb_w_preds <- application(cb_data, CB_features)

# Rbind all features that are important
needed_cols = intersect(FF_features, Si_features)
needed_cols = intersect(needed_cols, FC_features)
needed_cols = intersect(needed_cols, CH_features)
needed_cols = intersect(needed_cols, SL_features)
needed_cols = intersect(needed_cols, CB_features)
needed_cols = c(needed_cols, "lin_weight", "preds")
needed_cols

total_preds <- rbind(ff_w_preds[,needed_cols], si_w_preds[,needed_cols], fc_w_preds[,needed_cols], 
                     ch_w_preds[,needed_cols], sl_w_preds[,needed_cols], cb_w_preds[,needed_cols])

# Get total RMSE for all pitches
rmse(total_preds$lin_weight, total_preds$preds)

# Join predictions to original data frame
join_cols = needed_cols[needed_cols != "preds"]
final_mlb <- left_join(season_mlb5, total_preds, by = join_cols) 

# MLB Leaderboard
final_mlb%>%
  group_by(pitcher_name)%>%
  summarise(n=n(), x_rv = (100*sum(preds,na.rm=T)/n))%>%
  filter(n>500)%>%
  arrange((x_rv))%>%
  print(n=50)

#Changeup
final_mlb%>%
  filter(pitch_type %in% c("CH"))%>%
  group_by(pitcher_name, pitch_type)%>%
  summarise(pitches=n(), x_rv = 100*sum(preds,na.rm=T)/pitches,
            rv = 100*sum(lin_weight)/pitches)%>%
  filter(pitches>150)%>%
  arrange((x_rv))%>%
  print(n=20)

# Curveballs
final_mlb%>%
  filter(pitch_type %in% c("CU", "KC"))%>%
  group_by(pitcher_name, pitch_type)%>%
  summarise(pitches=n(), x_rv = 100*sum(preds,na.rm=T)/pitches,
            rv = 100*sum(lin_weight)/pitches)%>%
  filter(pitches>150)%>%
  arrange((x_rv))%>%
  print(n=20)

plot <- final_mlb%>%
  select(plate_x, plate_z, preds, lin_weight, sz_top, sz_bot) %>%
  group_by(plate_x, plate_z) %>%
  mutate(total = n(),
         x_rv = mean(preds,na.rm=T),
         rv = mean(lin_weight))

cb_fc <- final_mlb%>%
  select(pitcher_name, pitch_type, plate_x, plate_z, preds, lin_weight, sz_top, sz_bot) %>%
  group_by(plate_x, plate_z) %>%
  mutate(total = n(),
         x_rv = 100*sum(preds,na.rm=T)/total,
         rv = 100*sum(lin_weight)/total) %>%
  filter(pitcher_name == "Corbin Burnes") %>%
  filter(pitch_type == "FC")

cb_cb <- final_mlb%>%
  select(pitcher_name, pitch_type, plate_x, plate_z, preds, lin_weight, sz_top, sz_bot) %>%
  group_by(plate_x, plate_z) %>%
  mutate(total = n(),
         x_rv = 100*sum(preds,na.rm=T)/total,
         rv = 100*sum(lin_weight)/total) %>%
  filter(pitcher_name == "Corbin Burnes") %>%
  filter(pitch_type == "CU")

dr_ff <- final_mlb%>%
  select(pitcher_name, pitch_type, plate_x, plate_z, preds, lin_weight, sz_top, sz_bot) %>%
  group_by(plate_x, plate_z) %>%
  mutate(total = n(),
         x_rv = 100*sum(preds,na.rm=T)/total,
         rv = 100*sum(lin_weight)/total) %>%
  filter(pitcher_name == "Drew Rasmussen") %>%
  filter(pitch_type == "FF")

dr_sl <- final_mlb%>%
  select(pitcher_name, pitch_type, plate_x, plate_z, preds, lin_weight, sz_top, sz_bot) %>%
  group_by(plate_x, plate_z) %>%
  mutate(total = n(),
         x_rv = 100*sum(preds,na.rm=T)/total,
         rv = 100*sum(lin_weight)/total) %>%
  filter(pitcher_name == "Drew Rasmussen") %>%
  filter(pitch_type == "SL")

p <- ggplot(plot, aes(x=plate_x, y=plate_z, weight=x_rv)) +
  geom_hex(bins=500, show.legend = FALSE) +
  scale_x_continuous(limits = c(-2.5, 2.5)) +
  scale_y_continuous(limits = c(0, 4)) +
  geom_segment(aes(x = -0.85, y = 1.6, xend = 0.85, yend = 1.6)) +
  geom_segment(aes(x = -0.85, y = 3.5, xend = 0.85, yend = 3.5)) +
  geom_segment(aes(x = -0.85, y = 1.6, xend = -0.85, yend = 3.5)) +
  geom_segment(aes(x = 0.85, y = 1.6, xend = 0.85, yend = 3.5)) +
  scale_fill_gradient2(low = "blue", high = "red", space = "Lab", midpoint = 0) +
  xlab("Plate X (Catcher's Perspective)") +
  labs(title = "Overall xRun Value by Pitch Location (2021)")

p1 <- ggplot(plot, aes(x=plate_x, y=plate_z, weight=rv)) +
  geom_hex(show.legend = FALSE) +
  geom_segment(aes(x = -0.85, y = 1.6, xend = 0.85, yend = 1.6)) +
  geom_segment(aes(x = -0.85, y = 3.5, xend = 0.85, yend = 3.5)) +
  geom_segment(aes(x = -0.85, y = 1.6, xend = -0.85, yend = 3.5)) +
  geom_segment(aes(x = 0.85, y = 1.6, xend = 0.85, yend = 3.5)) +
  scale_fill_gradient2(low = "blue", high = "red", space = "Lab", midpoint = 0) +
  xlab("Plate X (Catcher's Perspective)") +
  labs(title = "Overall Run Value by Pitch Location (2021)")

p + p1

cb1 <- ggplot(cb_fc, aes(x=plate_x, y=plate_z, weight=x_rv)) +
  geom_hex(show.legend = FALSE) +
  geom_segment(aes(x = -0.85, y = 1.6, xend = 0.85, yend = 1.6)) +
  geom_segment(aes(x = -0.85, y = 3.5, xend = 0.85, yend = 3.5)) +
  geom_segment(aes(x = -0.85, y = 1.6, xend = -0.85, yend = 3.5)) +
  geom_segment(aes(x = 0.85, y = 1.6, xend = 0.85, yend = 3.5)) +
  scale_x_continuous(limits = c(-2.5, 2.5)) +
  scale_y_continuous(limits = c(0, 4)) +
  scale_fill_gradient2(low = "blue", high = "red", space = "Lab", midpoint = 0) +
  xlab("Plate X (Catcher's Perspective)") +
  labs(title = "Corbin Burnes, FC (Expected RV, 2021)")

cb2 <- ggplot(cb_cb, aes(x=plate_x, y=plate_z, weight=x_rv), xlim=c(-2.5, 2.5), ylim=c(0,4.5)) +
  geom_hex(show.legend = FALSE) +
  geom_segment(aes(x = -0.85, y = 1.6, xend = 0.85, yend = 1.6)) +
  geom_segment(aes(x = -0.85, y = 3.5, xend = 0.85, yend = 3.5)) +
  geom_segment(aes(x = -0.85, y = 1.6, xend = -0.85, yend = 3.5)) +
  geom_segment(aes(x = 0.85, y = 1.6, xend = 0.85, yend = 3.5)) +
  scale_x_continuous(limits = c(-2.5, 2.5)) +
  scale_y_continuous(limits = c(0, 4)) +
  scale_fill_gradient2(low = "blue", high = "red", space = "Lab", midpoint = 0) +
  xlab("Plate X (Catcher's Perspective)") +
  labs(title = "Corbin Burnes, CU (Expected RV, 2021)")

patchwork <- (cb1 + cb2)
patchwork + plot_annotation(title = "Corbin Burnes, Cutter and Curveball")

dr1 <- ggplot(dr_ff, aes(x=plate_x, y=plate_z, weight=x_rv), xlim=c(-2.5, 2.5), ylim=c(0,4.5)) +
  geom_hex(show.legend = FALSE) +
  geom_segment(aes(x = -0.85, y = 1.6, xend = 0.85, yend = 1.6)) +
  geom_segment(aes(x = -0.85, y = 3.5, xend = 0.85, yend = 3.5)) +
  geom_segment(aes(x = -0.85, y = 1.6, xend = -0.85, yend = 3.5)) +
  geom_segment(aes(x = 0.85, y = 1.6, xend = 0.85, yend = 3.5)) +
  scale_x_continuous(limits = c(-2.5, 2.5)) +
  scale_y_continuous(limits = c(0, 4)) +
  scale_fill_gradient2(low = "blue", high = "red", space = "Lab", midpoint = 0) +
  xlab("Plate X (Catcher's Perspective)") +
  ylab("Plate Y") +
  labs(title = "Drew Rasmussen, FF (Expected RV, 2021)")

dr2 <- ggplot(dr_sl, aes(x=plate_x, y=plate_z, weight=x_rv), xlim=c(-2.5, 2.5), ylim=c(0,4.5)) +
  geom_hex(show.legend = FALSE) +
  geom_segment(aes(x = -0.85, y = 1.6, xend = 0.85, yend = 1.6)) +
  geom_segment(aes(x = -0.85, y = 3.5, xend = 0.85, yend = 3.5)) +
  geom_segment(aes(x = -0.85, y = 1.6, xend = -0.85, yend = 3.5)) +
  geom_segment(aes(x = 0.85, y = 1.6, xend = 0.85, yend = 3.5)) +
  scale_x_continuous(limits = c(-2.5, 2.5)) +
  scale_y_continuous(limits = c(0, 4)) +
  scale_fill_gradient2(low = "blue", high = "red", space = "Lab", midpoint = 0) +
  xlab("Plate X (Catcher's Perspective)") +
  labs(title = "Drew Rasmussen, SL (Expected RV, 2021)")

drewras <- (dr1 + dr2)
drewras + plot_annotation(title = "Drew Rasmussen, Fastball and Slider (2021)")

library(patchwork)
  

library(DT)
table_this <- final_mlb%>%
  filter(pitch_type %in% c("CU",))%>%
  group_by(pitcher_name, pitch_type)%>%
  summarise(pitches=n(), RV = round(100*sum(lin_weight)/pitches, digits = 2),
            xRV = round(100*sum(preds,na.rm=T)/pitches,digits = 2))%>%
  ungroup()%>% 
  mutate(xRV_plus = round(as.numeric(rescale(-xRV, mean = 100, sd=10, df = F)),0))%>%  
  filter(pitches>=500)%>%
  arrange((xRV))
datatable(table_this, class = 'cell-border stripe', filter = 'top', 
          options = list(pageLength = 10, autoWidth = TRUE,
                         columnDefs = list(list(className = 'dt-center', targets = 0:6))),
          colnames = c("Pitcher" = "pitcher_name",
                       "Pitch Type" = "pitch_type",
                       "Pitch Count" = "pitches",
                       "Run Value" = "RV",
                       "xRV" = "xRV",
                       "xRV+" = "xRV_plus")) %>% 
  formatStyle('xRV', backgroundColor =  "lightblue")

datatable(table_this, class = 'cell-border stripe', filter = 'top', 
          options = list(pageLength = 10, autoWidth = TRUE,
                         columnDefs = list(list(className = 'dt-center', targets = 0:6))),
          colnames = c("Pitcher" = "pitcher_name",
                       "Pitch Type" = "pitch_type",
                       "Pitch Count" = "pitches",
                       "Run Value" = "RV",
                       "xRV" = "xRV",
                       "xRV+" = "xRV_plus")) %>% 
  formatStyle('xRV', backgroundColor =  "lightblue")

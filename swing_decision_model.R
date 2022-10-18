library(tidyverse)
library(DBI)
library(RMySQL)
library(ggplot2)
library(xgboost)
library(caTools)
library(caret)
library(ParBayesianOptimization)

statcast_db <- DBI::dbConnect(RMySQL::MySQL(),
                              dbname = "statcast",
                              user = <username>,
                              password = <password>,
                              host = "localhost",
                              port = 5432)

# Data pulled from SQL database, but baseballr Statcast function can work too.
df1 <- dbGetQuery(statcast_db, "SELECT * FROM statcast WHERE game_year = '2019'")
df2 <- dbGetQuery(statcast_db, "SELECT * FROM statcast WHERE game_year = '2020'")
df3 <- dbGetQuery(statcast_db, "SELECT * FROM statcast WHERE game_year = '2021'")

df1 <- df1 %>%
  filter(game_date >= "2019-03-28" & game_date <= "2019-09-29")

df2 <- df2 %>%
  filter(game_date >= "2020-07-23" & game_date <= "2020-09-27")

df3 <- df3 %>%
  filter(game_date >= "2021-04-01" & game_date <= "2021-10-03")

rawdata <- rbind(df1, df2, df3)
rm(df1, df2, df3)

swingdata <- rawdata %>%
  filter(description != "ball" & description != "called_strike" & description != "blocked_ball" & description != "pitchout" & description != "hit_by_pitch")%>% 
  mutate(outcome = ifelse(description == "swinging_strike" | description == "swinging_strike_blocked" | description == "foul_tip" | (strikes == 2 & description == "foul_bunt") | description == "bunt_foul_tip" | description == "missed_bunt", 1,
                          ifelse(description == "foul" | description == "foul_bunt", 2,  
                                 ifelse(events == "single", 3, 
                                        ifelse(events == "double", 4, 
                                               ifelse(events == "triple", 5, ifelse(events == "home_run", 6, 7)))))))%>% 
  mutate(stand = as.factor(stand))%>% 
  filter(!is.na(outcome), 
         !is.na(plate_x), 
         !is.na(plate_z), 
         !is.na(pfx_x), 
         !is.na(pfx_z), 
         !is.na(release_speed), 
         !is.na(stand), 
         !is.na(p_throws))%>% 
  select(outcome, plate_x, plate_z, pfx_x, pfx_z, release_speed, stand)%>%
  mutate(outcome = outcome - 1)

run_value <- rawdata %>%
  filter(description != "ball" & description != "called_strike" & description != "blocked_ball" & description != "pitchout" & description != "hit_by_pitch")%>% 
  mutate(outcome = ifelse(description == "swinging_strike" | description == "swinging_strike_blocked" | description == "foul_tip" | (strikes == 2 & description == "foul_bunt") | description == "bunt_foul_tip" | description == "missed_bunt", 1,
                          ifelse(description == "foul" | description == "foul_bunt", 2,  
                                 ifelse(events == "single", 3, ifelse(events == "double", 4, 
                                                                      ifelse(events == "triple", 5, ifelse(events == "home_run", 6, 7)))))))%>% 
  mutate(stand = as.factor(stand))%>% 
  filter(!is.na(outcome), 
         !is.na(plate_x), 
         !is.na(plate_z), 
         !is.na(pfx_x), 
         !is.na(pfx_z), 
         !is.na(release_speed), 
         !is.na(stand), 
         !is.na(p_throws))%>% 
  mutate(outcome = outcome - 1)%>%
  group_by(outcome)%>%
  summarize(mean_rv = mean(delta_run_exp, na.rm = TRUE)) %>%
  ungroup()
  

description <- c("miss", "foul", "single", "double", "triple", "home_run", "out")
run_value <- data.frame(run_value, description)
rm(description)

factor <- swingdata %>%
  select(stand)

dumb <- dummyVars ( ~ ., data = factor)
dumb_data <- data.frame(predict(dumb, newdata = factor))

swingdata.1 <- swingdata %>%
  select(-stand) %>%
  data.frame(dumb_data)

rm(dumb, dumb_data, factor, swingdata)

set.seed(1)
swing_sample <- sample.split(swingdata.1$outcome, SplitRatio = .75)
swing_train <- subset(swingdata.1, swing_sample == TRUE)
swing_test <- subset(swingdata.1, swing_sample == FALSE)

x_data <- swing_train %>%
  select(-outcome) %>%
  as.matrix()

y_data <- swing_train %>%
  pull(outcome)

x_data_test <- swing_test %>%
  select(-outcome) %>%
  as.matrix()

y_data_test <- swing_test %>%
  pull(outcome)

set.seed(27)
folds <- list(fold1 = as.integer(seq(1, nrow(x_data), by=3)),
              fold2 = as.integer(seq(2, nrow(x_data), by=3)),
              fold3 = as.integer(seq(3, nrow(x_data), by=3)),
              fold4 = as.integer(seq(4, nrow(x_data), by=3)),
              fold5 = as.integer(seq(5, nrow(x_data), by=3)))
obj_func <- function(max_depth, min_child_weight, subsample, colsample_bytree) {
  
  params <- list(
    eta = 0.1,
    num_class = 7,
    max_depth = max_depth,
    min_child_weight = min_child_weight,
    subsample = subsample,
    colsample_bytree = colsample_bytree,
    
    booster = "gbtree",
    objective = "multi:softprob",
    eval_metric = "merror")
  
  xgbcv <- xgb.cv(params = params,
                  data = x_data,
                  label = y_data,
                  nround = 150,
                  folds = folds,
                  prediction = TRUE,
                  early_stopping_rounds = 25,
                  verbose = 1,
                  maximize = F)
  
  list <- list(
    Score = -min(xgbcv$evaluation_log$test_merror_mean),
    nrounds = xgbcv$best_iteration
  )
  
  return(list)
}

bounds <- list(max_depth = c(1L, 10L),
               min_child_weight = c(3, 10),
               subsample = c(0.25, 1),
               colsample_bytree = c(.1, 1))

set.seed(33)

bayes_opt <- bayesOpt(FUN = obj_func, bounds = bounds, initPoints = length(bounds) + 6, iters.n = 15,
                      plotProgress = TRUE)

bayes_opt$scoreSummary
data.frame(getBestPars(bayes_opt))

params <- list(booster = "gbtree", num_class = 7, eval_metric = "merror",
               eta = 0.1, objective = "multi:softprob", max_depth = 10,
               min_child_weight = 3, subsample = 0.7086962, colsample_bytree = 0.9089091)

xgb.train <- xgb.DMatrix(data = x_data, label = y_data)
xgb.test <- xgb.DMatrix(data = x_data_test, label = y_data_test)

xgbcv <- xgb.cv(params = params, data = xgb.train, nrounds = 250, nfold = 7, showsd = TRUE,
                stratified = TRUE, print_every_n = 3, early_stopping_rounds = 20, maximize = FALSE)

print(xgbcv$best_iteration)

swings_xgb <- xgb.train(data = xgb.train, params = params, nrounds = 45,
                        watchlist = list(val = xgb.test, train = xgb.train))

pred_y = predict(swings_xgb, xgb.test)
mse = mean((y_data_test - pred_y)^2)
mae = caret::MAE(y_data_test, pred_y)
rmse = caret::RMSE(y_data_test, pred_y)

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)

saveRDS(swings_xgb, "swings_xgb.rds")

library(mgcv)

takes <- rawdata %>%
  filter(!is.na(plate_x),
         !is.na(plate_z)) %>%
  filter(description == "ball" | description == "called_strike") %>%
  mutate(strike = ifelse(description == "called_strike", 1, 0))

takes_left <- takes %>%
  filter(stand == "L")

takes_right <- takes %>%
  filter(stand == "R")

cs_gam_left <- gam(strike ~ s(plate_x, plate_z), data=takes_left, family=binomial())
cs_gam_right <- gam(strike ~ s(plate_x, plate_z), data=takes_right, family=binomial())

saveRDS(cs_gam_left, "cs_gam_left.rds")
saveRDS(cs_gam_right, "cs_gam_right.rds")

sd_rv <- function(swing_model, cs_mod_l, cs_mod_r, df){
  library(caret)
  library(dplyr)
  
  df <- df%>%
    filter(!is.na(plate_x), 
           !is.na(plate_z), 
           !is.na(pfx_x), 
           !is.na(pfx_z), 
           !is.na(release_speed), 
           !is.na(stand), 
           !is.na(p_throws))
  
  swing_data <- df%>%
    filter(!is.na(plate_x), 
           !is.na(plate_z), 
           !is.na(pfx_x), 
           !is.na(pfx_z), 
           !is.na(release_speed), 
           !is.na(stand), 
           !is.na(p_throws))%>%
    mutate(stand = as.factor(stand))%>%
    select(plate_x, plate_z, pfx_x, pfx_z, release_speed, stand)
  
  factor <- swing_data%>%
    select(stand)
  
  dummy <- dummyVars( ~ ., data = factor)
  dummy_data <- data.frame(predict(dummy, newdata = factor))
  
  swing_data_f <-  swing_data%>%
    select(-stand)%>%
    data.frame(dummy_data)
  
  rm(dummy, dummy_data, factor, swing_data)
  
  
  
  swing_preds <- as.data.frame(predict(swing_model, newdata = as.matrix(swing_data_f), reshape = TRUE))
  
  new_df <- data.frame(df, swing_preds)
  
  df_l <- new_df%>%
    filter(stand == "L")
  
  df_r <- new_df%>%
    filter(stand == "R")
  
  cs_pred_l <- as.data.frame(predict(cs_mod_l, newdata = df_l, type = "response"))
  cs_pred_l <- cs_pred_l%>%
    rename(strike_prob_yes = 'predict(cs_mod_l, newdata = df_l, type = "response")')%>%
    mutate(strike_prob_no = 1- strike_prob_yes)
  
  cs_pred_r <- as.data.frame(predict(cs_mod_r, newdata = df_r, type = "response"))
  cs_pred_r <- cs_pred_r%>%
    rename(strike_prob_yes = 'predict(cs_mod_r, newdata = df_r, type = "response")')%>%
    mutate(strike_prob_no = 1- strike_prob_yes)
  
  full_l <- data.frame(df_l, cs_pred_l)
  full_r <- data.frame(df_r, cs_pred_r)
  
  full_df <- rbind(full_l, full_r)
  
  full_df <- full_df%>%
    mutate(swing = ifelse(description %in%c("foul_tip", "swinging_strike",  
                                            "swinging_strike_blocked", "missed_bunt",
                                            "foul", "hit_into_play", "foul_bunt",
                                            "bunt_foul_tip"), 1, 0))%>%
    mutate(take_rv = (strike_prob_no * 0.0586) + (strike_prob_yes * -0.0665))%>%
    mutate(swing_rv = (-0.12167482 * V1) + (-0.03803784 * V2) + (0.48028049 * V3) +
             (0.77163585 * V4) + (1.03731489 * V5) + (1.37242980 * V6) + (-0.25050568 * V7))%>%
    mutate(should_swing = ifelse(swing_rv > take_rv, 1, 0), correct = ifelse(should_swing == swing, 1, 0))%>%
    mutate(decision_rv = ifelse(swing == 1, swing_rv - take_rv, take_rv - swing_rv),
           sd_plus = as.numeric(psych::rescale(decision_rv, mean = 100, sd = 50, df = FALSE)))
  
  return(full_df)
  
}

df2022 <-dbGetQuery(statcast_db, "SELECT * FROM statcast WHERE game_year = '2022'")

df2022 <- df2022 %>%
  filter(game_date >= "2022-04-07" & game_date <= "2022-10-5")

sd_rv22 <- sd_rv(swings_xgb, cs_gam_left, cs_gam_right, df2022)

topKzone = 3.5
botKzone = 1.6
inKzone = -.95
outKzone = 0.95
kZone = data.frame(x = c(inKzone, inKzone, outKzone, outKzone, inKzone),
                   y = c(botKzone, topKzone, topKzone, botKzone, botKzone))

sd_rv22%>%
  mutate(swing_new = ifelse(swing == 1, "Yes", "No"))%>%
  filter(plate_x >= -2.5, plate_x <= 2.5)%>%
  filter(plate_z >= -.5, plate_z <= 5)%>%
  ggplot(aes(plate_x, plate_z))+
  geom_point(aes(shape = as.factor(swing_new), color = sd_plus))+
  geom_path(data = kZone, aes(x,y))+
  scale_color_gradient2(low = "blue", high = "red", midpoint = 100)+
  coord_fixed()+
  theme_bw()+
  labs(shape = "Swing",
       color = "Swing Decision+",
       x = "Plate X",
       y = "Plate Z",
       title = "Swing Decisions Model, 2022")

df2021 <- dbGetQuery(statcast_db, "SELECT * FROM statcast WHERE game_year = '2021'")

df2021 <- df2021 %>%
  filter(game_date >= "2021-04-01" & game_date <= "2021-10-03")

sd_rv21 <- sd_rv(swings_xgb, cs_gam_left, cs_gam_right, df2021)

xbh21 <- sd_rv21%>%
  mutate(p_xbh = V4 + V5 + V6)%>%
  filter(swing == 1)%>%
  group_by(player_name, batter)%>%
  summarize(n = n(), avg = mean(p_xbh)*100)%>%
  filter(n >= 500)%>%
  arrange(desc(avg))%>%
  ungroup()

acuna <- sd_rv22%>%
  mutate(p_xbh = V4 + V5 + V6)%>%
  filter(swing == 1)%>%
  filter(batter == 0660670) %>%
  summarize(n = n(), avg = mean(p_xbh)*100)%>%
  arrange(desc(avg))%>%
  ungroup()

xbh22 <- sd_rv22%>%
  mutate(p_xbh = V4 + V5 + V6)%>%
  filter(swing == 1)%>%
  group_by(player_name, batter)%>%
  summarize(n_22 = n(), avg_22 = mean(p_xbh)*100)%>%
  filter(n_22 >= 250)%>%
  arrange(desc(avg_22))%>%
  ungroup()

sd_rv21%>%
  filter(player_name == "Ohtani, Shohei")%>%
  mutate(p_xbh = V4 + V5 + V6)%>%
  filter(swing == 1)%>%
  ggplot(aes(plate_x, plate_z))+
  geom_point(aes(color = p_xbh), size = 2)+
  scale_color_gradient2(high = "red", low = "blue", midpoint = 0.05, breaks = c(.025, .05, .075),
                        limits = c(0, .1), oob = scales::squish)+
  geom_path(data = kZone, mapping = aes(x, y), lwd = 1.3, color = "black")+
  xlim(-2, 2)+
  ylim(-1, 4.5)+
  theme_bw()+
  coord_fixed()+
  annotate(geom = "text", x = -1, y = -.75, label = "Mean p(XBH): 6.01%%")+
  labs(title = "Shohei Ohtani Swings, 2021",
       x = "Plate X",
       y = "Plate Z",
       color = "p(XBH)",
       subtitle = "Probabilities generated with pitch location, \nspeed, movement, and batter hand")

sd_rv22%>%
  filter(player_name == "Ohtani, Shohei")%>%
  mutate(p_xbh = V4 + V5 + V6)%>%
  filter(swing == 1)%>%
  ggplot(aes(plate_x, plate_z))+
  geom_point(aes(color = p_xbh), size = 2)+
  scale_color_gradient2(high = "red", low = "blue", midpoint = 0.05, breaks = c(.025, .05, .075),
                        limits = c(0, .1), oob = scales::squish)+
  geom_path(data = kZone, mapping = aes(x, y), lwd = 1.3, color = "black")+
  xlim(-2, 2)+
  ylim(-1, 4.5)+
  theme_bw()+
  coord_fixed()+
  annotate(geom = "text", x = -1, y = -.75, label = "Mean p(XBH): 6.28%")+
  labs(title = "Shohei Ohtani Swings, 2022",
       x = "Plate X",
       y = "Plate Z",
       color = "p(XBH)",
       subtitle = "Probabilities generated with pitch location, \nspeed, movement, and batter hand")

sd_rv21%>%
  filter(batter == "660670")%>%
  mutate(p_xbh = V4 + V5 + V6)%>%
  filter(swing == 1)%>%
  ggplot(aes(plate_x, plate_z))+
  geom_point(aes(color = p_xbh), size = 2)+
  scale_color_gradient2(high = "red", low = "blue", midpoint = 0.05, breaks = c(.025, .05, .075),
                        limits = c(0, .1), oob = scales::squish)+
  geom_path(data = kZone, mapping = aes(x, y), lwd = 1.3, color = "black")+
  xlim(-2, 2)+
  ylim(-1, 4.5)+
  theme_bw()+
  coord_fixed()+
  annotate(geom = "text", x = -1, y = -.75, label = "Mean p(XBH): 6.87%")+
  labs(title = "Ronald Acuña Jr. Swings, 2021",
       x = "Plate X",
       y = "Plate Z",
       color = "p(XBH)",
       subtitle = "Probabilities based on pitch location, \nspeed, movement, and batter hand")

sd_rv22%>%
  filter(batter == "660670")%>%
  mutate(p_xbh = V4 + V5 + V6)%>%
  filter(swing == 1)%>%
  ggplot(aes(plate_x, plate_z))+
  geom_point(aes(color = p_xbh), size = 2)+
  scale_color_gradient2(high = "red", low = "blue", midpoint = 0.05, breaks = c(.025, .05, .075),
                        limits = c(0, .1), oob = scales::squish)+
  geom_path(data = kZone, mapping = aes(x, y), lwd = 1.3, color = "black")+
  xlim(-2, 2)+
  ylim(-1, 4.5)+
  theme_bw()+
  coord_fixed()+
  annotate(geom = "text", x = -1, y = -.75, label = "Mean p(XBH): 6.51%")+
  labs(title = "Ronald Acuña Jr. Swings, 2022",
       x = "Plate X",
       y = "Plate Z",
       color = "p(XBH)",
       subtitle = "Probabilities based on pitch location, \nspeed, movement, and batter hand")


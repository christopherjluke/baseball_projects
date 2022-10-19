library(broom)
library(tidyr)
library(ggplot2)
library(dplyr)
library(readr)
library(ggrepel)

# Inspired from this article:
# https://community.fangraphs.com/reverse-engineering-swing-mechanics-from-statcast-data/

statcast_db <- DBI::dbConnect(RMySQL::MySQL(),
                              dbname = "statcast",
                              user = "root",
                              password = "Wasspord143!",
                              host = "localhost",
                              port = 5432)

batters <- dbGetQuery(statcast_db, "SELECT * FROM statcast WHERE game_year = '2022'")
batters <- batters %>%
  filter(game_date >= "2022-04-07" & game_date <= "2022-10-5")

people <- read_csv("https://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv")

mlbam_bat <- people %>%
  mutate(batter_name = paste(name_first, name_last)) %>%
  select(batter_name, key_mlbam, key_fangraphs)
names(mlbam_bat)[names(mlbam_bat) == 'key_mlbam'] <- 'batter'

batters <- left_join(batters, mlbam_bat, by ="batter")

inplay <- batters %>%
  filter(type == 'X') %>%
  filter(!is.na(launch_speed),
         !is.na(launch_angle),
         !is.na(release_speed)) %>%
  group_by(batter_name) %>%
  mutate(bip = n(), ev_pctl = ntile(launch_speed, 100), pitch_speed_hp = release_speed*.916) %>%
  ungroup()

top.15 <- inplay%>%
  group_by(batter_name)%>%
  arrange(-launch_speed)%>%
  dplyr::slice(1:15)%>%
  ungroup()%>%
  group_by(batter_name, batter)%>%
  filter(bip>=100)%>%
  mutate(ea = .21)%>%
  summarize(launch_speed = mean(launch_speed), ea = mean(ea))

bot.15 <- inplay%>%
  group_by(batter_name)%>%
  arrange(launch_speed)%>%
  dplyr::slice(1:15)%>%
  ungroup()%>%
  group_by(batter_name, batter)%>%
  filter(bip>=100)%>%
  mutate(ea = -.1)%>%
  summarize(launch_speed = mean(launch_speed), ea = mean(ea))

topbot15 <- rbind(bot.15, top.15)

ea_lm <- topbot15 %>%
  group_by(batter_name, batter)%>%
  do(fitEa = tidy(lm(ea ~ launch_speed, data = .)))%>%
  unnest(fitEa)%>%
  mutate(term_num = ifelse(term == "launch_speed", 1,0))

intercept <- ea_lm%>%
  filter(term_num == 0)%>%
  select(batter_name, batter, estimate)%>%
  rename(intercept = estimate)

launch_speed_est <- ea_lm%>%
  filter(term_num == 1)%>%
  select(batter_name, batter, estimate)%>%
  rename(launch_speed_est = estimate)

inplay <- inplay%>%
  left_join(intercept, by = c("batter_name", "batter"))%>%
  left_join(launch_speed_est, by = c("batter_name", "batter"))%>%
  mutate(ea = intercept + (launch_speed_est * launch_speed))%>%
  filter(!is.na(intercept))%>%
  mutate(hard_hit = ifelse( launch_speed >= 95, 1, 0))%>%
  mutate(est_swing_speed = (launch_speed - ea * pitch_speed_hp)/(1 + ea))%>%
  mutate(est_smash_factor = 1 + (launch_speed - est_swing_speed)/(pitch_speed_hp + est_swing_speed))

swing_speed <- inplay%>%
  group_by(batter_name, batter)%>%
  summarize(bat_speed = (mean(est_swing_speed)), hard_hit_perc = mean(hard_hit)*100,
            ea = mean(ea), bip = mean(bip), smash_factor = mean(est_smash_factor),
            wobacon = mean(woba_value), avg_ev = mean(launch_speed),
            xwobacon = mean(estimated_woba_using_speedangle))%>%
  arrange(desc(bat_speed)) %>%
  ungroup()

lm_mod <- lm(avg_ev ~ bat_speed, data = swing_speed)
swing_speed$residual <- lm_mod$residuals
swing_speed_outliers <- swing_speed%>%
  filter(abs(residual)  >= 2.95)

swing_speed%>%
  ggplot(aes(bat_speed, avg_ev))+
  geom_point(aes(color = ea))+
  scale_color_distiller(palette = "Spectral")+
  geom_smooth(method = "lm")+
  geom_label_repel(data = swing_speed_outliers, aes(bat_speed, avg_ev, label = batter_name),
                   min.segment.length = .02)+
  theme_bw()+
  labs(title = "Average Exit Velocity and Estimated Bat Speed",
       subtitle = "(2022, min: 100 Balls-in-Play)",
       x = "Estimated Bat Speed",
       y = "Average Exit Velocity",
       color = "Avg. Collision Eff.",
       caption = "Data Source: Baseball Savant")
  

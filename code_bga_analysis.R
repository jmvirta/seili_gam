#-----------------
#
# BGA analyses
# 09.12.2020
# JV
# jomivi(at)utu.fi
#
#-----------------


library(tidyverse)
library(lubridate)
library(mgcv)
library(scales)


# Put here the folder which contains the data files (and where the output will go)
my_folder <- "/folder/subfolder"


# The following data files are needed to conduct the analyses
# (The data can be obtained from the Seili website)

# chl_2011.csv -- chl_2019.csv 
# bga_2011.csv -- bga_2019.csv 
# temperature_2011.csv -- temperature_2019.csv 
# weather_2015.csv
# weather_2016.xlsx
# weather_2017.xlsx
# weather_2018_2019.csv




# Read Bga and Temp and preprocess by
# - dividing into two depth ranges (< 20, >= 20)
# - taking averages over days

bga <- NULL

for(i in 2015:2019){
  bga_temp <- read.csv(paste0(my_folder, "/bga_", i, ".csv")) %>%
    select(-type) %>%
    mutate(depth = if_else(depth < 20, 1, 2)) %>%
    mutate(date = as.Date(substr(date, 1, 10))) %>%
    group_by(date, depth) %>%
    summarise(bga = mean(value, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(year = year(date), day = yday(date)) %>%
    select(year, day, depth, bga)
  
  bga <- bga %>%
    bind_rows(bga_temp)
}






temp <- NULL

for(i in 2015:2019){
  temp_temp <- read.csv(paste0(my_folder, "/temperature_", i, ".csv")) %>%
    select(-type) %>%
    mutate(depth = if_else(depth < 20, 1, 2)) %>%
    mutate(date = as.Date(substr(date, 1, 10))) %>%
    group_by(date, depth) %>%
    summarise(temp = mean(value, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(year = year(date), day = yday(date)) %>%
    select(year, day, depth, temp)
  
  temp <- temp %>%
    bind_rows(temp_temp)
}



# Remove the bottom depth category (the interesting thing are at the surface)
bga <- bga %>%
  filter(depth == 1) %>%
  select(-depth)

temp <- temp %>%
  filter(depth == 1) %>%
  select(-depth)



# Read wind data

wind <- NULL

# 2015
wind_temp <- read.csv(paste0(my_folder, "/weather_", 2015, ".csv"), sep = ";") %>%
  select(Date, Wspeed) %>%
  mutate(Wspeed = gsub(",", ".", Wspeed)) %>%
  mutate(Wspeed = as.numeric(Wspeed)) %>%
  mutate(Date = as.Date(Date, format = "%d.%m.%Y")) %>%
  mutate(year = year(Date), day = yday(Date)) %>%
  group_by(year, day) %>%
  summarize(wind = mean(Wspeed, na.rm = TRUE)) %>%
  ungroup() %>%
  select(year, day, wind)

wind <- wind %>%
  bind_rows(wind_temp)


# 2016
wind_temp <- readxl::read_xlsx(paste0(my_folder, "/weather_", 2016, ".xlsx"))
wind_temp <- wind_temp[, c(1, 7)]
colnames(wind_temp) <- c("Date", "Wspeed")
wind_temp <- wind_temp %>%
  mutate(day = yday(as.Date(substr(Date, 1, 10), format = "%Y-%m-%d"))) %>%
  mutate(year = year(as.Date(substr(Date, 1, 10), format = "%Y-%m-%d"))) %>%
  group_by(year, day) %>%
  summarize(wind = mean(Wspeed, na.rm = TRUE)) %>%
  ungroup() %>%
  select(year, day, wind)


wind <- wind %>%
  bind_rows(wind_temp)



# 2017
wind_temp <- readxl::read_xlsx(paste0(my_folder, "/weather_", 2017, ".xlsx"))
wind_temp <- wind_temp[, c(1, 7)]
colnames(wind_temp) <- c("Date", "Wspeed")
wind_temp <- wind_temp %>%
  mutate(day = yday(as.Date(substr(Date, 1, 10), format = "%Y-%m-%d"))) %>%
  mutate(year = year(as.Date(substr(Date, 1, 10), format = "%Y-%m-%d"))) %>%
  group_by(year, day) %>%
  summarize(wind = mean(Wspeed, na.rm = TRUE)) %>%
  ungroup() %>%
  select(year, day, wind)


wind <- wind %>%
  bind_rows(wind_temp)



# 2018 & 2019
wind_temp <- read.csv(paste0(my_folder, "/weather_2018_2019", ".csv"), sep = ",") %>%
  select(date, wind_speed) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  mutate(wind_speed = as.character(wind_speed)) %>%
  mutate(wind_speed = ifelse(wind_speed == "NULL", NA, wind_speed)) %>%
  mutate(wind_speed = as.numeric(wind_speed)) %>%
  mutate(year = year(date), day = yday(date)) %>%
  group_by(year, day) %>%
  summarize(wind = mean(wind_speed, na.rm = TRUE)) %>%
  ungroup() %>%
  select(year, day, wind)

wind <- wind %>%
  bind_rows(wind_temp)



# Bind
seili <- full_join(bga, temp, by = c("year", "day"))
seili <- full_join(seili, wind, by = c("year", "day"))



# # Remove the spikes in 2019
# seili <- seili %>%
#   filter((year %in% 2011:2018) | day < 323)



# Estimate the bottom level for 2018

seili %>%
  filter(year != 2018) %>%
  select(year, bga) %>%
  group_by(year) %>%
  summarise(min_bga = min(bga, na.rm = TRUE)) %>%
  ungroup() %>%
  select(min_bga) %>%
  summarize(avg_min_bga = mean(min_bga))

seili %>%
  filter(year == 2018) %>%
  select(bga) %>%
  summarize(min_bga = min(bga, na.rm = TRUE))

seili_adjust <- seili %>%
  mutate(bga = ifelse(year == 2018, bga + 360 + 1168, bga))

ggplot(seili, aes(x = day, y = bga)) +
  geom_line(aes(colour = factor(year)), size = 1.1) +
  scale_color_brewer(palette = "PuOr") +
  theme_bw() +
  labs(x = "Day", y = "Value", colour = "Year")

ggplot(seili_adjust, aes(x = day, y = bga)) +
  geom_line(aes(colour = factor(year)), size = 1.1) +
  scale_color_brewer(palette = "Set2") +
  theme_bw() +
  labs(x = "Day", y = "Value", colour = "Year")



# 2017 starts after the spike and we use the 2016 value there
# similarly for 2018
new_day <- (seili_adjust$day - 109)*(seili_adjust$year == "2015") +
  (seili_adjust$day - 110)*(seili_adjust$year == "2016") +
  (seili_adjust$day - 110)*(seili_adjust$year == "2017") +
  (seili_adjust$day - 110)*(seili_adjust$year == "2018") +
  (seili_adjust$day - 115)*(seili_adjust$year == "2019")


# The new data with aligned curves
seili_offset <- seili_adjust %>%
  ungroup() %>%
  mutate(day = new_day)

# Some plots
pdf(paste0(my_folder, "/plot_bga_align.pdf"), width = 8, height = 5)
ggplot(seili_offset, aes(x = day, y = bga)) +
  geom_line(aes(colour = factor(year)), size = 1.1) +
  scale_color_brewer(palette = "PuOr") +
  theme_bw() +
  labs(x = "Day", y = "BGA [cells/mL]", colour = "Year") +
  coord_cartesian(xlim = c(-50, 250))
dev.off()






##### Prediction with "Difference-day model" (not in the paper)


seili_work_diff <- seili_offset %>%
  select(day, bga, temp, wind, year) %>%
  arrange(year, day) %>%
  group_by(year) %>%
  mutate(bga_diff = bga - lag(bga)) %>%
  ungroup() %>%
  mutate(year = as.factor(year))


# Remove the days without BGA_diff
seili_work_diff <- seili_work_diff %>%
  filter(!is.na(bga_diff))


seili_2018_diff <- seili_work_diff %>%
  filter(year %in% 2015:2018)


seili_2019_diff <- seili_work_diff %>%
  filter(year == 2019)


day_list <- seili_2019_diff %>%
  arrange(year, day) %>%
  select(day) %>%
  c()

day_list <- day_list$day

day_list



##### 10 days ahead

init_days <- 44
incr_days <- 10
k_max <- 200/incr_days

start_days <- day_list[seq(init_days + 1, by = incr_days, length.out = k_max)]

seili_res_diff <- NULL


for(k in 1:k_max){
  
  seili_model_diff <- seili_2019_diff %>%
    filter(day %in% day_list[1:(init_days + (k - 1)*incr_days)]) %>%
    bind_rows(seili_2018_diff)
  
  seili_test_diff <- seili_2019_diff %>%
    filter(day %in% day_list[(init_days + (k - 1)*incr_days + 1):(init_days + k*incr_days)])
  
  gamm_1 <- gamm(bga_diff ~ s(temp) + s(wind) + s(day),
                 random = list(year = ~1), data = seili_model_diff, family = gaussian(link = identity), method = "REML", niterPQL = 100)

  preds <- predict(gamm_1$gam, seili_test_diff, se.fit = TRUE)
  
  # gam_1 <- gam(chl_diff ~ s(temp, by = depth) + s(day, by = depth) + depth + year,
  #                data = seili_model_diff, family = gaussian(link = identity))
  # 
  # preds <- predict(gam_1, seili_test_diff, se.fit = TRUE)

  
  seili_temp_diff <- seili_test_diff %>%
    mutate(bga_diff_pred = preds$fit)
  
  seili_res_diff <- bind_rows(seili_res_diff, seili_temp_diff)
  print(k)
}








# seili_res_diff_plot <- seili_res_diff %>%
#   select(day, year, depth, chl_diff, chl_diff_pred) %>%
#   gather(key = var, value = value, chl_diff, chl_diff_pred) %>%
#   mutate(var = factor(var, levels = c("chl_diff", "chl_diff_pred"), labels = c("True", "Predicted")))
# 
# ggplot(seili_res_diff_plot, aes(x = day, y = value)) +
#   geom_line(aes(col = var)) +
#   facet_wrap(. ~ depth) +
#   coord_cartesian(ylim = c(-1, 1))

# ggplot(filter(seili_work_diff, year == "2019", depth == "<20"), aes(x = day, y = chl_diff)) +
#   geom_line() +
#   facet_wrap(. ~ depth)
# 
# seili_diff_test <- seili_offset %>%
#   filter(year == 2019, depth == "<20") %>%
#   mutate(chl_diff = chl - lag(chl))
# 
# ggplot(seili_diff_test, aes(x = day, y = chl_diff)) +
#   geom_line()




seili_res_diff_2 <- seili_res_diff %>%
  mutate(window_between = rep(1:k_max, each = incr_days)[1:200],
         window_within = rep(1:incr_days, k_max)[1:200]) %>%
  mutate(bga_sum = ifelse(window_within == 1, bga, bga_diff_pred)) %>%
  group_by(window_between) %>%
  mutate(bga_diff_pred_cumsum = cumsum(bga_sum)) %>%
  ungroup(window_between) %>%
  select(day, year, bga, bga_diff_pred_cumsum) %>%
  rename(bga_pred = bga_diff_pred_cumsum)

seili_res_diff_3 <- seili_res_diff_2 %>%
  gather(key = type, value = value, -day, -year,) %>%
  mutate(type = factor(type, levels = c("bga", "bga_pred"), labels = c("True", "Predicted")))

seili_final_diff <- seili_2019_diff %>%
  filter(day < 15) %>%
  select(-temp, -wind, -bga_diff) %>%
  rename(value = bga) %>%
  mutate(type = "bga") %>%
  mutate(type = factor(type, levels = c("bga", "bga_pred"), labels = c("True", "Predicted"))) %>%
  select(day, year, type, value) %>%
  bind_rows(seili_res_diff_3)

pdf(paste0(my_folder, "/plot_predict_bga_temp_wind.pdf"), width = 10, height = 5)
ggplot(seili_final_diff, aes(x = day)) +
  geom_line(aes(y = value, linetype = type)) +
  theme_bw() +
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Day", y = "BGA [cells/mL]", linetype = "Type") +
  geom_vline(xintercept = start_days, linetype = 2, col = "coral") +
  coord_cartesian(xlim = c(-20, 220))
dev.off()







#  Prediction with "Difference-day model" without temp as predictor (not in the paper)


seili_res_diff <- NULL


for(k in 1:k_max){
  
  seili_model_diff <- seili_2019_diff %>%
    filter(day %in% day_list[1:(init_days + (k - 1)*incr_days)]) %>%
    bind_rows(seili_2018_diff)
  
  seili_test_diff <- seili_2019_diff %>%
    filter(day %in% day_list[(init_days + (k - 1)*incr_days + 1):(init_days + k*incr_days)])
  
  gamm_1 <- gamm(bga_diff ~ s(wind) + s(day),
                 random = list(year = ~1), data = seili_model_diff, family = gaussian(link = identity), method = "REML", niterPQL = 100)
  
  preds <- predict(gamm_1$gam, seili_test_diff, se.fit = TRUE)
  
  # gam_1 <- gam(chl_diff ~ s(temp, by = depth) + s(day, by = depth) + depth + year,
  #                data = seili_model_diff, family = gaussian(link = identity))
  # 
  # preds <- predict(gam_1, seili_test_diff, se.fit = TRUE)
  
  
  seili_temp_diff <- seili_test_diff %>%
    mutate(bga_diff_pred = preds$fit)
  
  seili_res_diff <- bind_rows(seili_res_diff, seili_temp_diff)
  print(k)
}








# seili_res_diff_plot <- seili_res_diff %>%
#   select(day, year, depth, chl_diff, chl_diff_pred) %>%
#   gather(key = var, value = value, chl_diff, chl_diff_pred) %>%
#   mutate(var = factor(var, levels = c("chl_diff", "chl_diff_pred"), labels = c("True", "Predicted")))
# 
# ggplot(seili_res_diff_plot, aes(x = day, y = value)) +
#   geom_line(aes(col = var)) +
#   facet_wrap(. ~ depth) +
#   coord_cartesian(ylim = c(-1, 1))

# ggplot(filter(seili_work_diff, year == "2019", depth == "<20"), aes(x = day, y = chl_diff)) +
#   geom_line() +
#   facet_wrap(. ~ depth)
# 
# seili_diff_test <- seili_offset %>%
#   filter(year == 2019, depth == "<20") %>%
#   mutate(chl_diff = chl - lag(chl))
# 
# ggplot(seili_diff_test, aes(x = day, y = chl_diff)) +
#   geom_line()




seili_res_diff_2 <- seili_res_diff %>%
  mutate(window_between = rep(1:k_max, each = incr_days)[1:200],
         window_within = rep(1:incr_days, k_max)[1:200]) %>%
  mutate(bga_sum = ifelse(window_within == 1, bga, bga_diff_pred)) %>%
  group_by(window_between) %>%
  mutate(bga_diff_pred_cumsum = cumsum(bga_sum)) %>%
  ungroup(window_between) %>%
  select(day, year, bga, bga_diff_pred_cumsum) %>%
  rename(bga_pred = bga_diff_pred_cumsum)

seili_res_diff_3 <- seili_res_diff_2 %>%
  gather(key = type, value = value, -day, -year,) %>%
  mutate(type = factor(type, levels = c("bga", "bga_pred"), labels = c("True", "Predicted")))

seili_final_diff <- seili_2019_diff %>%
  filter(day < 15) %>%
  select(-temp, -wind, -bga_diff) %>%
  rename(value = bga) %>%
  mutate(type = "bga") %>%
  mutate(type = factor(type, levels = c("bga", "bga_pred"), labels = c("True", "Predicted"))) %>%
  select(day, year, type, value) %>%
  bind_rows(seili_res_diff_3)

pdf(paste0(my_folder, "/plot_predict_bga_wind.pdf"), width = 10, height = 5)
ggplot(seili_final_diff, aes(x = day)) +
  geom_line(aes(y = value, linetype = type)) +
  theme_bw() +
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Day", y = "BGA [cells/mL]", linetype = "Type") +
  geom_vline(xintercept = start_days, linetype = 2, col = "coral") +
  coord_cartesian(xlim = c(-20, 220))
dev.off()







##### "Difference-day model" for full data (not in the paper)


global_gamm_1 <- gamm(bga_diff ~ s(temp) + s(wind) + s(day),
                   random = list(year = ~1), data = seili_work_diff, family = gaussian(link = identity), method = "REML", niterPQL = 100)

summary(global_gamm_1)
summary(global_gamm_1$gam)

plot(1:831, resid(global_gamm_1$gam), type = "l")

ggplot(seili_work_diff, aes(x = day, y = bga_diff)) +
  geom_line(aes(colour = factor(year)), size = 1.1) +
  scale_color_brewer(palette = "PuOr") +
  theme_bw() +
  labs(x = "Day", y = "BGA [cells/mL]", colour = "Year") +
  coord_cartesian(xlim = c(-50, 250))









###### Prediction with "Raw values model"




# Predictors: day (function), temp (function), wind (function)
# The model is GAMM with random year term and Gaussian response family with identity link
# We use 2019 as a test data

seili_work <- seili_offset %>%
  select(day, bga, temp, wind, year) %>%
  mutate(year = as.factor(year)) %>%
  arrange(year, day)

seili_2018 <- seili_work %>%
  filter(year %in% 2011:2018)

seili_2019 <- seili_work %>%
  filter(year == 2019)

day_list <- seili_2019 %>%
  select(day) %>%
  c()

day_list <- day_list$day

day_list



##### 10 days ahead

init_days <- 44
incr_days <- 10
k_max <- 200/incr_days

start_days <- day_list[seq(init_days + 1, by = incr_days, length.out = k_max)]

seili_res <- NULL


for(k in 1:k_max){
  
  seili_model <- seili_2019 %>%
    filter(day %in% day_list[1:(init_days + (k - 1)*incr_days)]) %>%
    bind_rows(seili_2018)
  
  seili_test <- seili_2019 %>%
    filter(day %in% day_list[(init_days + (k - 1)*incr_days + 1):(init_days + k*incr_days)])
  
  gamm_1 <- gamm(bga ~ s(temp) + s(wind) + s(day),
                 random = list(year = ~1), data = seili_model, family = gaussian(link = identity), method = "REML", niterPQL = 100)
  
  preds <- predict(gamm_1$gam, seili_test, se.fit = TRUE)
  
  # gam_1 <- gam(chl ~ s(temp, by = depth) + s(day, by = depth) + depth + year,
  #                data = seili_model, family = gaussian(link = identity))
  # 
  # preds <- predict(gam_1, seili_test, se.fit = TRUE)
  
  seili_temp <- seili_test %>%
    mutate(bga_pred = preds$fit)
  
  seili_res <- bind_rows(seili_res, seili_temp)
  print(k)
}






seili_final_1 <- seili_2019 %>%
  filter(day %in% day_list[1:init_days]) %>%
  bind_rows(seili_res) %>%
  gather(key = type, value = value, -day, -temp, -wind, -year) %>%
  mutate(type = factor(type, levels = c("bga", "bga_pred"), labels = c("True", "Predicted")))


pdf(paste0(my_folder, "/plot_predict_bga_temp_wind_true_values.pdf"), width = 10, height = 5)
ggplot(seili_final_1, aes(x = day)) +
  geom_line(aes(y = value, linetype = type)) +
  theme_bw() +
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Day", y = "BGA [cells/mL]", linetype = "Type") +
  geom_vline(xintercept = start_days, linetype = 2, col = "coral") +
  coord_cartesian(xlim = c(-20, 220))
dev.off()





###### "Raw values model" prediction with no wind as predictor


# 1. Prediction

# Predictors: day (function), temp (function)
# The model is GAMM with random year term and Gaussian response family with identity link
# We use 2019 as a test data

seili_work <- seili_offset %>%
  select(day, bga, temp, year) %>%
  mutate(year = as.factor(year)) %>%
  arrange(year, day)

seili_2018 <- seili_work %>%
  filter(year %in% 2011:2018)

seili_2019 <- seili_work %>%
  filter(year == 2019)

day_list <- seili_2019 %>%
  select(day) %>%
  c()

day_list <- day_list$day

day_list



##### 10 days ahead

init_days <- 44
incr_days <- 10
k_max <- 200/incr_days

start_days <- day_list[seq(init_days + 1, by = incr_days, length.out = k_max)]

seili_res <- NULL


for(k in 1:k_max){
  
  seili_model <- seili_2019 %>%
    filter(day %in% day_list[1:(init_days + (k - 1)*incr_days)]) %>%
    bind_rows(seili_2018)
  
  seili_test <- seili_2019 %>%
    filter(day %in% day_list[(init_days + (k - 1)*incr_days + 1):(init_days + k*incr_days)])
  
  gamm_1 <- gamm(bga ~ s(temp) + s(day),
                 random = list(year = ~1), data = seili_model, family = gaussian(link = identity), method = "REML", niterPQL = 100)
  
  preds <- predict(gamm_1$gam, seili_test, se.fit = TRUE)
  
  # gam_1 <- gam(chl ~ s(temp, by = depth) + s(day, by = depth) + depth + year,
  #                data = seili_model, family = gaussian(link = identity))
  # 
  # preds <- predict(gam_1, seili_test, se.fit = TRUE)
  
  seili_temp <- seili_test %>%
    mutate(bga_pred = preds$fit)
  
  seili_res <- bind_rows(seili_res, seili_temp)
  print(k)
}






seili_final_1 <- seili_2019 %>%
  filter(day %in% day_list[1:init_days]) %>%
  bind_rows(seili_res) %>%
  gather(key = type, value = value, -day, -temp, -year) %>%
  mutate(type = factor(type, levels = c("bga", "bga_pred"), labels = c("True", "Predicted")))


pdf(paste0(my_folder, "/plot_predict_bga_temp_wind_true_values.pdf"), width = 10, height = 5)
ggplot(seili_final_1, aes(x = day)) +
  geom_line(aes(y = value, linetype = type)) +
  theme_bw() +
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Day", y = "BGA [cells/mL]", linetype = "Type") +
  geom_vline(xintercept = start_days, linetype = 2, col = "coral") +
  coord_cartesian(xlim = c(-20, 220))
dev.off()





##### "Raw values model" for full data


global_gamm_1 <- gamm(bga ~ s(temp) + s(wind) + s(day),
                      random = list(year = ~1), data = seili_work, family = gaussian(link = identity), method = "REML", niterPQL = 100)

summary(global_gamm_1)
summary(global_gamm_1$gam)



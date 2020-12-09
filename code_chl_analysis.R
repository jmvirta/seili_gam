#-----------------
#
# Chlorophyll analyses
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



# Read Chl and Temp and preprocess by
# - dividing into two depth ranges (< 20, >= 20)
# - taking averages over days

chl <- NULL

for(i in 2011:2019){
  chl_temp <- read.csv(paste0(my_folder, "/chl_", i, ".csv")) %>%
    select(-type) %>%
    mutate(depth = if_else(depth < 20, 1, 2)) %>%
    mutate(date = as.Date(substr(date, 1, 10))) %>%
    group_by(date, depth) %>%
    summarise(chl = mean(value, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(year = year(date), day = yday(date)) %>%
    select(year, day, depth, chl)
  
  chl <- chl %>%
    bind_rows(chl_temp)
}


temp <- NULL

for(i in 2011:2019){
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



# Bind
seili <- full_join(chl, temp, by = c("year", "day", "depth")) %>%
  mutate(depth = factor(depth, levels = c(1, 2), labels = c("<20", ">=20")))


# Remove the spikes in 2019
seili <- seili %>%
  filter((year %in% 2011:2018) | day < 323)


ggplot(seili, aes(x = day, y = chl)) +
  geom_line(aes(colour = factor(year)), size = 1.1) +
  facet_grid(. ~ depth, scales = "free_y") +
  scale_color_brewer(palette = "PuOr") +
  theme_bw() +
  labs(x = "Day", y = "Value", colour = "Year")

ggplot(filter(seili, year == "2019"), aes(x = day, y = chl)) +
  geom_line(aes(colour = factor(year)), size = 1.1) +
  facet_grid(. ~ depth, scales = "free_y") +
  scale_color_brewer(palette = "PuOr") +
  theme_bw() +
  labs(x = "Day", y = "Value", colour = "Year")



# We align the data by the dates with maximal yearly surface Chl-value
seili %>%
  select(day, year, chl, depth) %>%
  group_by(year, depth) %>%
  top_n(n = 1, wt = chl) %>%
  filter(depth == "<20") %>%
  select(day)


# 2017 starts after the spike and we use the 2016 value there
# similarly for 2018
new_day <- (seili$day - 139)*(seili$year == "2011") +
  (seili$day - 127)*(seili$year == "2012") +
  (seili$day - 133)*(seili$year == "2013") +
  (seili$day - 109)*(seili$year == "2014") +
  (seili$day - 109)*(seili$year == "2015") +
  (seili$day - 110)*(seili$year == "2016") +
  (seili$day - 110)*(seili$year == "2017") +
  (seili$day - 110)*(seili$year == "2018") +
  (seili$day - 115)*(seili$year == "2019")


# The new data with aligned curves
seili_offset <- seili %>%
  ungroup() %>%
  mutate(day = new_day)


# Some plots
seili_long <- seili_offset %>%
  gather(key = var, value = value, temp, chl) %>%
  mutate(var = factor(var, levels = c("chl", "temp"), labels = c("Chlorophyll", "Temperature")))

pdf(paste0(my_folder, "/plot_align.pdf"), width = 10, height = 5)
ggplot(seili_long %>% filter(var == "Chlorophyll") %>% mutate(depth = factor(depth, labels = c("Surface (<20m)", "Bottom (>=20m)"))), aes(x = day, y = value)) +
  geom_line(aes(colour = factor(year)), size = 1.1) +
  facet_grid(. ~ depth, scales = "free_y") +
  scale_color_brewer(palette = "PuOr") +
  theme_bw() +
  labs(x = "Day", y = "Chlorophyll-a [ug/L]", colour = "Year")
dev.off()

# pdf(paste0(my_folder, "/plot_align_2011_2017.pdf"), width = 10, height = 5)
# ggplot(seili_long %>% filter(var == "Chlorophyll", year %in% 2011:2017) %>% mutate(depth = factor(depth, labels = c("Surface (<20m)", "Bottom (>=20m)"))), aes(x = day, y = value)) +
#   geom_line(aes(colour = factor(year)), size = 1.1) +
#   facet_grid(. ~ depth, scales = "free_y") +
#   scale_color_brewer(palette = "PuOr") +
#   theme_bw() +
#   labs(x = "Day", y = "Chlorophyll-a [ug/L]", colour = "Year") +
#   coord_cartesian(ylim = c(0, 20.5))
# dev.off()



# 1. Prediction with "Raw values model"


# Predictors: day (function), depth (categorical), temp (function)
# The model is GAMM with random year term and Gaussian response family with identity link
# We use 2019 as a test data

seili_work <- seili_offset %>%
  select(day, chl, temp, year, depth) %>%
  mutate(year = as.factor(year))

seili_2018 <- seili_work %>%
  filter(year %in% 2011:2018)

seili_2019 <- seili_work %>%
  filter(year == 2019)

day_list <- seili_2019 %>%
  select(day) %>%
  c()

day_list <- day_list$day
day_list <- day_list[seq(1, length(day_list), 2)]

day_list



##### 10 days ahead prediction

init_days <- 25
incr_days <- 10
k_max <- 180/incr_days

start_days <- day_list[seq(init_days + 1, by = incr_days, length.out = k_max)]

seili_res <- NULL


for(k in 1:k_max){
  
  seili_model <- seili_2019 %>%
    filter(day %in% day_list[1:(init_days + (k - 1)*incr_days)]) %>%
    bind_rows(seili_2018)
  
  seili_test <- seili_2019 %>%
    filter(day %in% day_list[(init_days + (k - 1)*incr_days + 1):(init_days + k*incr_days)])
  
  gamm_1 <- gamm(chl ~ s(temp, by = depth) + s(day, by = depth) + depth,
                 random = list(year = ~1), data = seili_model, family = gaussian(link = identity), method = "REML")

  preds <- predict(gamm_1$gam, seili_test, se.fit = TRUE)
  
  # gam_1 <- gam(chl ~ s(temp, by = depth) + s(day, by = depth) + depth + year,
  #                data = seili_model, family = gaussian(link = identity))
  # 
  # preds <- predict(gam_1, seili_test, se.fit = TRUE)
  
  seili_temp <- seili_test %>%
    mutate(chl_pred = preds$fit,
           chl_lower = preds$fit - 1.96*preds$se.fit,
           chl_upper = preds$fit + 1.96*preds$se.fit)
  
  seili_res <- bind_rows(seili_res, seili_temp)
  print(k)
}






seili_final_1 <- seili_2019 %>%
  filter(day %in% day_list[1:init_days]) %>%
  bind_rows(seili_res) %>%
  gather(key = type, value = value, -day, -temp, -year, -depth) %>%
  mutate(type = factor(type, levels = c("chl", "chl_pred", "chl_lower", "chl_upper"), labels = c("True", "Predicted", "Lower", "Upper"))) %>%
  mutate(depth = factor(depth, labels = c("Surface (<20m)", "Bottom (>=20m)")))



pdf(paste0(my_folder, "/plot_predict_1.pdf"), width = 10, height = 5)
ggplot(filter(seili_final_1, type %in% c("True", "Predicted")), aes(x = day)) +
  geom_line(aes(y = value, linetype = type)) +
  facet_wrap(~ depth, nrow = 2) +
  theme_bw() +
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Day", y = "Chlorophyll", linetype = "Type") +
  geom_vline(xintercept = start_days, linetype = 2, col = "coral")
  # ggtitle("Predicting chlorophyll", subtitle = "10 days ahead at a time") +
  # theme(plot.title = element_text(hjust = 0.5)) + 
  # theme(plot.subtitle = element_text(hjust = 0.5))
dev.off()
# 
# 
# pdf(paste0(my_folder, "/plot_predict_1_alt.pdf"), width = 10, height = 5)
# ggplot(seili_final_1, aes(x = day)) +
#   geom_line(aes(y = value, linetype = type)) +
#   facet_wrap(~ depth, nrow = 2) +
#   theme_bw() +
#   # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
#   labs(x = "Day", y = "Chlorophyll", linetype = "Type") +
#   geom_vline(xintercept = start_days, linetype = 2, col = "coral")
# # ggtitle("Predicting chlorophyll", subtitle = "10 days ahead at a time") +
# # theme(plot.title = element_text(hjust = 0.5)) +
# # theme(plot.subtitle = element_text(hjust = 0.5))
# dev.off()





# 2. Prediction with "Difference-day model"

seili_work_diff <- seili_offset %>%
  select(day, chl, temp, year, depth) %>%
  arrange(depth, year, day) %>%
  group_by(year, depth) %>%
  mutate(chl_diff = chl - lag(chl)) %>%
  ungroup() %>%
  mutate(year = as.factor(year))


seili_2018_diff <- seili_work_diff %>%
  filter(year %in% 2011:2018)

seili_2019_diff <- seili_work_diff %>%
  filter(year == 2019)

day_list <- seili_2019_diff %>%
  arrange(year, day, depth) %>%
  select(day) %>%
  c()

day_list <- day_list$day
day_list <- day_list[seq(1, length(day_list), 2)]




##### 10 days ahead

init_days <- 25
incr_days <- 10
k_max <- 180/incr_days

start_days <- day_list[seq(init_days + 1, by = incr_days, length.out = k_max)]

seili_res_diff <- NULL


for(k in 1:k_max){
  
  seili_model_diff <- seili_2019_diff %>%
    filter(day %in% day_list[1:(init_days + (k - 1)*incr_days)]) %>%
    bind_rows(seili_2018_diff)
  
  seili_test_diff <- seili_2019_diff %>%
    filter(day %in% day_list[(init_days + (k - 1)*incr_days + 1):(init_days + k*incr_days)])
  
  gamm_1 <- gamm(chl_diff ~ s(temp, by = depth) + s(day, by = depth) + depth,
                 random = list(year = ~1), data = seili_model_diff, family = gaussian(link = identity), method = "REML", niterPQL = 100)

  preds <- predict(gamm_1$gam, seili_test_diff, se.fit = TRUE)
  
  # gam_1 <- gam(chl_diff ~ s(temp, by = depth) + s(day, by = depth) + depth + year,
  #                data = seili_model_diff, family = gaussian(link = identity))
  # 
  # preds <- predict(gam_1, seili_test_diff, se.fit = TRUE)

  
  seili_temp_diff <- seili_test_diff %>%
    mutate(chl_diff_pred = preds$fit)
  
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




seili_res_diff_surface <- seili_res_diff %>%
  filter(depth == "<20") %>%
  # mutate(chl_lag = lag(chl)) %>%
  # slice(-1) %>%
  mutate(window_between = rep(1:k_max, each = incr_days)[1:178],
         window_within = rep(1:incr_days, k_max)[1:178]) %>%
  mutate(chl_sum = ifelse(window_within == 1, chl, chl_diff_pred)) %>%
  group_by(window_between) %>%
  mutate(chl_diff_pred_cumsum = cumsum(chl_sum)) %>%
  #        chl_pred = chl_lag[window_within == 1] + chl_diff_pred_cumsum) %>%
  # mutate(chl_error = chl - chl_pred) %>%
  ungroup(window_between) %>%
  select(day, year, chl, chl_diff_pred_cumsum, depth) %>%
  rename(chl_pred = chl_diff_pred_cumsum)


seili_res_diff_bottom <- seili_res_diff %>%
  filter(depth == ">=20") %>%
  # mutate(chl_lag = lag(chl)) %>%
  # slice(-1) %>%
  mutate(window_between = rep(1:k_max, each = incr_days)[1:178],
         window_within = rep(1:incr_days, k_max)[1:178]) %>%
  mutate(chl_sum = ifelse(window_within == 1, chl, chl_diff_pred)) %>%
  group_by(window_between) %>%
  mutate(chl_diff_pred_cumsum = cumsum(chl_sum)) %>%
  #        chl_pred = chl_lag[window_within == 1] + chl_diff_pred_cumsum) %>%
  # mutate(chl_error = chl - chl_pred) %>%
  ungroup(window_between) %>%
  select(day, year, chl, chl_diff_pred_cumsum, depth) %>%
  rename(chl_pred = chl_diff_pred_cumsum)


seili_res_diff_both <- bind_rows(seili_res_diff_surface, seili_res_diff_bottom) %>%
  gather(key = type, value = value, -day, -year, -depth) %>%
  mutate(type = factor(type, levels = c("chl", "chl_pred"), labels = c("True", "Predicted")))

seili_final_diff <- seili_2019_diff %>%
  filter(day <= 15) %>%
  select(-temp, -chl_diff) %>%
  rename(value = chl) %>%
  mutate(type = "chl") %>%
  mutate(type = factor(type, levels = c("chl", "chl_pred"), labels = c("True", "Predicted"))) %>%
  select(day, year, depth, type, value) %>%
  bind_rows(seili_res_diff_both) %>%
  mutate(depth = factor(depth, labels = c("Surface (<20m)", "Bottom (>=20m)")))

pdf(paste0(my_folder, "/plot_predict_2.pdf"), width = 10, height = 5)
ggplot(seili_final_diff, aes(x = day)) +
  geom_line(aes(y = value, linetype = type)) +
  facet_wrap(~ depth, nrow = 2) +
  theme_bw() +
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Day", y = "Chlorophyll", linetype = "Type") +
  geom_vline(xintercept = start_days, linetype = 2, col = "coral")
# ggtitle("Predicting chlorophyll", subtitle = "10 days ahead at a time") +
# theme(plot.title = element_text(hjust = 0.5)) + 
# theme(plot.subtitle = element_text(hjust = 0.5))
dev.off()













# 3. Model for full data


# k <- k_max + 1
# 
# seili_model <- seili_2019 %>%
#   filter(day %in% day_list[1:(init_days + (k - 1)*incr_days)]) %>%
#   bind_rows(seili_2018)


gamm_1 <- gamm(chl ~ s(temp, by = depth) + s(day, by = depth) + depth,
               random = list(year = ~1), data = seili_offset, family = gaussian(link = identity), method = "REML")

# gamm_1 <- gam(chl ~ s(temp, by = depth) + s(day, by = depth) + depth + factor(year), data = seili_offset, family = gaussian(link = identity))


summary(gamm_1$gam)

summary(gamm_1)

summary(gamm_1$lme)


pdf(paste0(my_folder, "/plot_beta_temperature.pdf"), width = 10, height = 5)
par(mfrow = c(1, 2))
plot(gamm_1$gam, select = 1, scale = 0, xlab = "Temperature", ylab = "Effect on chlorophyll", main = expression("Surface, "<20 * m), rug = FALSE, se = 1.96, lwd = 2)
grid(col = "gray")
plot(gamm_1$gam, select = 2, scale = 0, xlab = "Temperature", ylab = "Effect on chlorophyll", main = expression("Bottom, ">=20 * m), rug = FALSE, se = 1.96, lwd = 2)
grid(col = "gray")
par(mfrow = c(1, 1))
dev.off()


pdf(paste0(my_folder, "/plot_beta_day.pdf"), width = 10, height = 5)
par(mfrow = c(1, 2))
plot(gamm_1$gam, select = 3, scale = 0, xlab = "Day", ylab = "Effect on chlorophyll", main = expression("Surface, "<20 * m), rug = FALSE, se = 1.96, lwd = 2)
grid(col = "gray")
plot(gamm_1$gam, select = 4, scale = 0, xlab = "Day", ylab = "Effect on chlorophyll", main = expression("Bottom, ">=20 * m), rug = FALSE, se = 1.96, lwd = 2)
grid(col = "gray")
par(mfrow = c(1, 1))
dev.off()






# Analysis code for Runion et al. 2025, "Early warning signs of salt marsh drowning indicated by widespread vulnerability from declining belowground plant biomass, PNAS. DOI: 10.1073/pnas.2425501122.

## This is the code used for analysis, and not all data were made publicly available due to size constraints. Data made available: https://doi.org/10.6073/pasta/286d539cac2353abbc7c993dad840a76
## Any folder paths should be edited before use.

library(plyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(data.table)
library(readr)
library(terra)
library(tidyterra)
library(raster)
library(sp)
library(sf)
library(ggthemes)
library(extrafont)
library(lubridate)
library(paletteer)
library(broom)


# Data organized by watershed HUC10 code

## Pixel AGB, BGB mean and annual trend

watersheds <- read.csv("data/huc10_list.csv", colClasses = "character")
watersheds <-watersheds$huc10

for (w in 1:length(watersheds)){
  filenames <- list.files(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w]), pattern = ".csv", full.names = T)
  bgb_list <- lapply(filenames, fread, select = c(1,4,9,47))
  pix_out <- data.frame()
  cut_no <- length(list.files(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w]), pattern = ".csv", full.names = T))
  
  for (c in 1:(cut_no)){
    bgb <- fread(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w], "/modeled_xgb_belowground_biomass_", watersheds[w], "_cutno", c, ".csv"), select = c("pix", "date", "predag.allom.l8", "predbg.xgb"))
    bgb <- bgb %>%
      drop_na(date)
    bgb$mo_num <- time_length(interval(as.Date("2013-12-15"), bgb$date),  unit = "month")
    bgb <- bgb[order(bgb$pix, bgb$mo_num),]
    bgb$date <- NULL
    #grab the coefficients of the linear model for each pixel
    #bgb
    biomass <- bgb %>%
      group_by(pix) %>% 
      do(data.frame(., as.list(coef(lm(predbg.xgb ~ mo_num, data = .))))) %>%
      rename_at(5:6, ~c("intc_bg", "coef_bg"))
    #agb
    biomass <- biomass %>%
      drop_na(predag.allom.l8)
    biomass <- biomass %>%
      group_by(pix) %>% 
      mutate(est_count = n()) %>%
      do(data.frame(., as.list(coef(lm(predag.allom.l8 ~ mo_num, data = ., na.action = na.omit))))) %>%
      rename_at(8:9, ~c("intc_ag", "coef_ag"))
    #pix_summ gives pixel level avg bgb, agb, linear model results
    pix_summ <- biomass %>%
      group_by(pix) %>%
      summarise(across(c(1:2,4:8), ~ mean(.x, na.rm = T))) %>%
      filter(est_count > 100)
    #merge
    pix_out <- rbind(pix_out, pix_summ)
    rm(bgb)
    rm(biomass)
    rm(pix_summ)
    rm(pix_count)
  }
  write_csv(pix_out, paste0("/media/kyle/Seagate Expansion Drive/data/results/pix_summ/", watersheds[w], ".csv"))
}

## 5 YEAR INTERVAL

watersheds <- read.csv("data/huc10_list.csv", colClasses = "character")
watersheds <-watersheds$huc10

for (w in 1:length(watersheds)){
  filenames <- list.files(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w]), pattern = ".csv", full.names = T)
  bgb_list <- lapply(filenames, fread, select = c(1,4,9,47))
  pix_out <- data.frame()
  cut_no <- length(list.files(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w]), pattern = ".csv", full.names = T))
  
  for (c in 1:(cut_no)){
    bgb <- fread(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w], "/modeled_xgb_belowground_biomass_", watersheds[w], "_cutno", c, ".csv"), select = c("pix", "date", "predag.allom.l8", "predbg.xgb"))
    bgb <- bgb %>%
      drop_na(date)
    bgb$mo_num <- time_length(interval(as.Date("2013-12-15"), bgb$date),  unit = "month")
    bgb <- bgb[order(bgb$pix, bgb$mo_num),]
    bgb$date <- NULL
    bgb$interval <- ifelse(bgb$mo_num < 61, 1, 2)
    #grab the coefficients of the linear model for each pixel
    #bgb
    biomass <- bgb %>%
      group_by(pix, interval) %>%
      do(data.frame(., as.list(coef(lm(predbg.xgb ~ mo_num, data = .))))) %>%
      rename_at(6:7, ~c("intc_bg", "coef_bg"))
    #agb
    biomass <- biomass %>%
      drop_na(predag.allom.l8)
    biomass <- biomass %>%
      group_by(pix, interval) %>%
      mutate(est_count = n()) %>%
      do(data.frame(., as.list(coef(lm(predag.allom.l8 ~ mo_num, data = ., na.action = na.omit))))) %>%
      rename_at(9:10, ~c("intc_ag", "coef_ag"))
    #pix_summ gives pixel level avg bgb, agb, linear model results
    pix_summ <- biomass %>%
      group_by(pix, interval) %>%
      summarise(across(c(1:2,4:8), ~ mean(.x, na.rm = T)))
    
    
    #merge
    pix_out <- rbind(pix_out, pix_summ)
    rm(bgb)
    rm(biomass)
    rm(pix_summ)
    rm(pix_count)
  }
  write_csv(pix_out, paste0("/media/kyle/Seagate Expansion Drive/data/results/pix_summ_5yr/", watersheds[w], ".csv"))
}

## count of estimates, mean and sd AGB and BGB
filenames <- list.files("/media/kyle/Seagate Expansion Drive/data/results/pix_summ_5yr/", pattern = ".csv", full.names = T)
tables <- lapply(filenames, read.csv, header = TRUE)
pix_summ<- do.call(rbind , tables)
pix_summ <- pix_summ %>%
  mutate(ag_trend = coef_ag/predag.allom.l8*12*100, bg_trend = coef_bg/predbg.xgb*12*100) %>%
  drop_na(c(ag_trend, bg_trend))
agb_vul_pix_int2 <- read.table("results/abg_vul_int2.txt", sep = ",", colClasses = "character")

## AGB, BGB date-level mean and sd
watersheds <- read.csv("data/huc10_list.csv", colClasses = "character")
watersheds <-watersheds$huc10

for (w in 1:length(watersheds)){
  filenames <- list.files(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w]), pattern = ".csv", full.names = T)
  bgb_list <- lapply(filenames, fread, select = c(1,4,9,47))
  pix_out <- data.frame()
  cut_no <- length(list.files(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w]), pattern = ".csv", full.names = T))
  
  for (c in 1:(cut_no)){
    biomass <- fread(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w], "/modeled_xgb_belowground_biomass_", watersheds[w], "_cutno", c, ".csv"), select = c("pix", "date", "predag.allom.l8", "predbg.xgb"))
    biomass <- biomass %>%
      drop_na(date)
    #bgb$mo_num <- time_length(interval(as.Date("2013-12-15"), bgb$date),  unit = "month")
    #bgb <- bgb[order(bgb$pix, bgb$mo_num),]
    #bgb$date <- NULL
    #mean and sd
    biomass <- biomass %>%
      group_by(date) %>% 
      summarise(count = n(), bgb_mean = mean(predbg.xgb, na.rm = T), bgb_sd = sd(predbg.xgb, na.rm = T), agb_mean = mean(predag.allom.l8, na.rm = T), agb_sd = sd(predag.allom.l8, na.rm = T)) 
    #merge
    pix_out <- rbind(pix_out, biomass)
    rm(biomass)
  }
  write_csv(pix_out, paste0("/media/kyle/Seagate Expansion Drive/data/results/date_stats/", watersheds[w], ".csv"))
}

filenames <- list.files("/media/kyle/Seagate Expansion Drive/data/results/date_stats/", pattern = ".csv", full.names = T)
tables <- lapply(filenames, read.csv, header = TRUE)
df<- do.call(rbind , tables)
df <- df %>%
  group_by(date) %>%
  summarise(bgb_mean = weighted.mean(bgb_mean, count), bgb_sd = weighted.mean(bgb_sd, count), agb_mean = weighted.mean(agb_mean, count), agb_sd = weighted.mean(agb_sd, count))
write_csv(df, "/media/kyle/Seagate Expansion Drive/data/results/date_mean_sd/df.csv")

## count of estimates, mean and sd AGB and BGB
filenames <- list.files("/media/kyle/Seagate Expansion Drive/data/results/pix_summ/", pattern = ".csv", full.names = T)
tables <- lapply(filenames, read.csv, header = TRUE)
pix_summ<- do.call(rbind , tables)
pix_summ2 <- pix_summ %>%
  mutate(ag_trend = coef_ag/predag.allom.l8*12*100, bg_trend = coef_bg/predbg.xgb*12*100) %>%
  #drop_na(c(ag_trend, bg_trend)) %>%
  summarise(bgb_mean = mean(predbg.xgb, na.rm = T), bgb_sd = sd(predbg.xgb), bgb_max = max(predbg.xgb), bgb_min = min(predbg.xgb),
            agb_mean = mean(predag.allom.l8), agb_sd = sd(predag.allom.l8), agb_max = max(predag.allom.l8), agb_min = min(predag.allom.l8),
            bgb_trend_mean = mean(bg_trend), bgb_trend_sd = sd(bg_trend), bgb_trend_max = max(bg_trend), bgb_trend_min = min(bg_trend),
            agb_trend_mean = mean(ag_trend), agb_trend_sd = sd(ag_trend), agb_trend_max = max(ag_trend), agb_trend_min = min(ag_trend),
            rs_ratio = mean(predbg.xgb/predag.allom.l8)) %>%
  mutate(est_count = nrow(pix_summ)*120, 
         agb_trend_perc_p = nrow(pix_summ[pix_summ$coef_ag > 0,]) / nrow(pix_summ), agb_trend_perc_n = nrow(pix_summ[pix_summ$coef_ag < 0,]) / nrow(pix_summ),
         bgb_trend_perc_p = nrow(pix_summ[pix_summ$coef_bg > 0,]) / nrow(pix_summ), bgb_trend_perc_n = nrow(pix_summ[pix_summ$coef_bg < 0,]) / nrow(pix_summ),
         bgb_n_agb_p = nrow(pix_summ[pix_summ$coef_bg < 0 & pix_summ$coef_ag > 0,]) / nrow(pix_summ),
         bgb_n_agb_n = nrow(pix_summ[pix_summ$coef_bg < 0 & pix_summ$coef_ag < 0,]) / nrow(pix_summ))
est_bgb_stock <- c(pix_summ2$bgb_mean, pix_summ2$bgb_sd, pix_summ2$bgb_max, pix_summ2$bgb_min)
est_bgb_trend <- c(pix_summ2$bgb_trend_mean, pix_summ2$bgb_trend_sd, pix_summ2$bgb_trend_max, pix_summ2$bgb_trend_min)
est_agb_stock <- c(pix_summ2$agb_mean, pix_summ2$agb_sd, pix_summ2$agb_max, pix_summ2$agb_min)
est_agb_trend <- c(pix_summ2$agb_trend_mean, pix_summ2$agb_trend_sd, pix_summ2$agb_trend_max, pix_summ2$agb_trend_min)
est_table <- data.frame(est_bgb_stock, est_bgb_trend, est_agb_stock, est_agb_trend)
write_csv(pix_summ2, file = "results/estimates_summ.csv")
write_csv(est_table, file = "results/estimates_table.csv")


## grab pixel ancillary data (elevation and flood_time)

watersheds <- read.csv("data/huc10_list.csv", colClasses = "character")
watersheds <-watersheds$huc10
pix_attr <- data.frame()

for (w in 1:length(watersheds)){
  filenames <- list.files(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w]), pattern = ".csv", full.names = T)
  pix_out <- data.frame()
  cut_no <- length(list.files(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w]), pattern = ".csv", full.names = T))
  
  for (c in 1:(cut_no)){
    dat <- fread(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w], "/modeled_xgb_belowground_biomass_", watersheds[w], "_cutno", c, ".csv"), select = c("pix", "date", "elevation", "flood_time"))
    dat <- dat %>%
      drop_na(date) %>%
      group_by(pix) %>%
      summarise(elevation = mean(elevation, na.rm = T), flood_time = mean(flood_time, na.rm = T))
    
    #merge
    pix_out <- rbind(pix_out, dat)
    rm(dat)
  }
  pix_attr <- rbind(pix_attr, pix_out)
}
write_csv(pix_attr, paste0("/media/kyle/Seagate Expansion Drive/data/results/pix_attr/pix_attr.csv"))

## grab pixel ancillary data (doy_greenup)

watersheds <- read.csv("data/huc10_list.csv", colClasses = "character")
watersheds <-watersheds$huc10
pix_attr <- data.frame()

for (w in 1:length(watersheds)){
  filenames <- list.files(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w]), pattern = ".csv", full.names = T)
  pix_out <- data.frame()
  cut_no <- length(list.files(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w]), pattern = ".csv", full.names = T))
  
  for (c in 1:(cut_no)){
    dat <- fread(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w], "/modeled_xgb_belowground_biomass_", watersheds[w], "_cutno", c, ".csv"), select = c("pix", "date", "greendoy"))
    dat <- dat %>%
      drop_na(date) %>%
      group_by(pix) %>%
      summarise(greendoy = mean(greendoy, na.rm = T))
    
    #merge
    pix_out <- rbind(pix_out, dat)
    rm(dat)
  }
  pix_attr <- rbind(pix_attr, pix_out)
}
write_csv(pix_attr, paste0("/media/kyle/Seagate Expansion Drive/data/results/pix_attr/pix_attr_greenup.csv"))

## grab pixel ancillary data (elevation and inund_inten) at interval

watersheds <- read.csv("data/huc10_list.csv", colClasses = "character")
watersheds <-watersheds$huc10
pix_attr <- data.frame()

for (w in 1:length(watersheds)){
  filenames <- list.files(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w]), pattern = ".csv", full.names = T)
  pix_out <- data.frame()
  cut_no <- length(list.files(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w]), pattern = ".csv", full.names = T))
  
  for (c in 1:(cut_no)){
    dat <- fread(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w], "/modeled_xgb_belowground_biomass_", watersheds[w], "_cutno", c, ".csv"), select = c("pix", "date", "elevation", "local_hiwater"))
    dat <- dat %>%
      drop_na(date) %>%
      #change date for interval
      mutate(interval = ifelse(date < as.Date("2018-12-31"),1,2)) %>%
      group_by(pix, interval) %>%
      summarise(elevation = mean(elevation, na.rm = T), inund_inten = mean(local_hiwater, na.rm = T))
    
    #merge
    pix_out <- rbind(pix_out, dat)
    rm(dat)
  }
  pix_attr <- rbind(pix_attr, pix_out)
}
write_csv(pix_attr, paste0("/media/kyle/Seagate Expansion Drive/data/results/pix_attr/pix_attr_interval.csv"))

## Mean and Median II in 2014 and 2023
# Mean
watersheds <- read.csv("data/huc10_list.csv", colClasses = "character")
watersheds <-watersheds$huc10
pix_out <- data.frame()

for (w in 1:length(watersheds)){
  filenames <- list.files(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w]), pattern = ".csv", full.names = T)
  cut_no <- length(list.files(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w]), pattern = ".csv", full.names = T))
  
  for (c in 1:(cut_no)){
    dat <- fread(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w], "/modeled_xgb_belowground_biomass_", watersheds[w], "_cutno", c, ".csv"), select = c("year", "local_hiwater"))
    dat_2014 <- dat %>%
      filter(year == "2014") %>%
      summarise(count = n(), ii_med = mean(local_hiwater, na.rm = T)) %>%
      mutate(year = "2014")
    dat_2023 <- dat %>%
      filter(year == "2023") %>%
      summarise(count = n(), ii_med = mean(local_hiwater, na.rm = T)) %>%
      mutate(year = "2023")
    rm(dat)
    #merge
    pix_out <- rbind(pix_out, dat_2014, dat_2023)
  }
  write_csv(pix_out, paste0("/media/kyle/Seagate Expansion Drive/data/results/ii_mean/", watersheds[w], ".csv"))
}
filenames <- list.files("/media/kyle/Seagate Expansion Drive/data/results/ii_mean/", pattern = ".csv", full.names = T)
tables <- lapply(filenames, read.csv, header = TRUE)
df<- do.call(rbind , tables)
df <- df %>%
  group_by(year) %>%
  summarise(ii_mean = weighted.mean(ii_med, count))
write_csv(df, "/media/kyle/Seagate Expansion Drive/data/results/ii_mean.csv")

# Median
watersheds <- read.csv("data/huc10_list.csv", colClasses = "character")
watersheds <-watersheds$huc10
pix_out <- data.frame()

for (w in 1:length(watersheds)){
  filenames <- list.files(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w]), pattern = ".csv", full.names = T)
  cut_no <- length(list.files(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w]), pattern = ".csv", full.names = T))
  
  for (c in 1:(cut_no)){
    dat <- fread(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w], "/modeled_xgb_belowground_biomass_", watersheds[w], "_cutno", c, ".csv"), select = c("year", "local_hiwater"))
    dat_2014 <- dat %>%
      filter(year == "2014") %>%
      summarise(count = n(), ii_med = median(local_hiwater, na.rm = T)) %>%
      mutate(year = "2014")
    dat_2023 <- dat %>%
      filter(year == "2023") %>%
      summarise(count = n(), ii_med = median(local_hiwater, na.rm = T)) %>%
      mutate(year = "2023")
    rm(dat)
    #merge
    pix_out <- rbind(pix_out, dat_2014, dat_2023)
  }
  write_csv(pix_out, paste0("/media/kyle/Seagate Expansion Drive/data/results/ii_med/", watersheds[w], ".csv"))
}
filenames <- list.files("/media/kyle/Seagate Expansion Drive/data/results/ii_med/", pattern = ".csv", full.names = T)
tables <- lapply(filenames, read.csv, header = TRUE)
df<- do.call(rbind , tables)
df <- df %>%
  group_by(year) %>%
  summarise(ii_median = weighted.mean(ii_med, count))
write_csv(df, "/media/kyle/Seagate Expansion Drive/data/results/ii_med.csv")


### Identify vulnerable pixels

watersheds <- read.csv("data/huc10_list.csv", colClasses = "character")
watersheds <-watersheds$huc10
out3 <- data.frame()

for (w in 1:length(watersheds)){
  filenames <- list.files(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w]), pattern = ".csv", full.names = T)
  bgb_list <- lapply(filenames, fread, select = c(1))
  out <- data.frame()
  out2 <- data.frame()
  cut_no <- length(list.files(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w]), pattern = ".csv", full.names = T))
  
  for (c in 1:(cut_no)){
    bgb <- fread(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w], "/modeled_xgb_belowground_biomass_", watersheds[w], "_cutno", c, ".csv"), select = c("pix", "year", "predag.allom.l8", "predn.l8", "predbg.xgb", "flood_time", "elevation"))
    out <- bgb %>%
      drop_na(pix) %>%
      group_by(pix,year) %>%
      summarise(bgb_mean = mean(predbg.xgb, na.rm = T), bgb_min = min(predbg.xgb, na.rm = T), bgb_max = max(predbg.xgb, na.rm = T))
    out2 <- rbind(out2, out)
  }
  out3 <- rbind(out3,out2)
  
}
write_csv(out3, "/media/kyle/Seagate Expansion Drive/data/results/bgb_annual_mean_min_max.csv")          	 

bgb_decline <- read_csv("/media/kyle/Seagate Expansion Drive/data/results/bgb_annual_mean_min_max.csv")
bgb_decline2 <- bgb_decline %>%
  dplyr::select(pix, year, bgb_mean) %>%
  group_by(pix) %>%
  filter(bgb_mean == max(bgb_mean) | bgb_mean == min(bgb_mean)) %>%
  summarise(bgb_raw_change = bgb_mean[year == max(year)] - bgb_mean[year == min(year)])

ggplot(data = bgb_decline2, aes(x = bgb_raw_change)) +
  geom_histogram()
length(which(bgb_decline2$bgb_raw_change > 0))
length(which(bgb_decline2$bgb_raw_change < 0))
length(which(bgb_decline2$bgb_raw_change < -344)) / nrow(bgb_decline2)
length(which(bgb_decline2$bgb_raw_change > 344))

write_csv(bgb_decline2, "/media/kyle/Seagate Expansion Drive/data/results/bgb_change.csv")

bgb_decline3 <- bgb_decline %>%
  dplyr::select(pix, year, bgb_max) %>%
  group_by(pix) %>%
  filter(bgb_max == max(bgb_max) | bgb_max == min(bgb_max)) %>%
  summarise(bgb_raw_change = bgb_max[year == max(year)] - bgb_max[year == min(year)])

write_csv(bgb_decline3, "/media/kyle/Seagate Expansion Drive/data/results/bgb_max_change.csv")

bgb_decline4 <- bgb_decline %>%
  dplyr::select(pix, year, bgb_max) %>%
  filter(year %in% c(2014, 2023)) %>%
  group_by(pix) %>%
  summarise(bgb_raw_change = bgb_max[year == max(year)] - bgb_max[year == min(year)])

vul <- read_csv("/media/kyle/Seagate Expansion Drive/data/results/bgb_max_change.csv")
vul %>%
  filter(bgb_raw_change < -344) %>%
  summarise(count = n())

## Identify AGB decline pixels

watersheds <- read.csv("data/huc10_list.csv", colClasses = "character")
watersheds <-watersheds$huc10
out3 <- data.frame()

for (w in 1:length(watersheds)){
  filenames <- list.files(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w]), pattern = ".csv", full.names = T)
  agb_list <- lapply(filenames, fread, select = c(1))
  out <- data.frame()
  out2 <- data.frame()
  cut_no <- length(list.files(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w]), pattern = ".csv", full.names = T))
  
  for (c in 1:(cut_no)){
    agb <- fread(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w], "/modeled_xgb_belowground_biomass_", watersheds[w], "_cutno", c, ".csv"), select = c("pix", "year", "predag.allom.l8", "predn.l8", "predbg.xgb", "flood_time", "elevation"))
    out <- agb %>%
      drop_na(pix) %>%
      group_by(pix,year) %>%
      summarise(agb_mean = mean(predag.allom.l8, na.rm = T), agb_min = min(predag.allom.l8, na.rm = T), agb_max = max(predag.allom.l8, na.rm = T))
    out2 <- rbind(out2, out)
  }
  out3 <- rbind(out3,out2)
  
}
write_csv(out3, "/media/kyle/Seagate Expansion Drive/data/results/agb_annual_mean_min_max.csv")

### AGB Decline Pix
agb_decline <- read_csv("/media/kyle/Seagate Expansion Drive/data/results/agb_annual_mean_min_max.csv")

agb_decline3 <- agb_decline %>%
  dplyr::select(pix, year, agb_max) %>%
  filter(year > 2018) %>%
  group_by(pix) %>%
  filter(agb_max == max(agb_max) | agb_max == min(agb_max)) %>%
  summarise(agb_raw_change = agb_max[year == max(year)] - agb_max[year == min(year)])

write_csv(agb_decline3, "/media/kyle/Seagate Expansion Drive/data/results/agb_max_change_int2.csv")

agb_decline3 %>%
  filter(agb_raw_change < -55) %>%
  summarise(count = n())
agb_decline_pix <- agb_decline3$pix[agb_decline3$agb_raw_change < -55]
agb_decline_pix <- na.omit(agb_decline_pix)
write.table(agb_decline_pix, "results/abg_vul_int2.txt", sep = ",")
length(agb_decline_pix)
##
col_pix <- read_csv("results/col_pix.csv")
agb_decline_pix2 <- col_pix$pix[col_pix$col == "1"]
length(intersect(agb_decline_pix, agb_decline_pix2))

## Mean and Median II for Collapse Pix vs Whole Coast
# Mean
watersheds <- read.csv("data/huc10_list.csv", colClasses = "character")
watersheds <-watersheds$huc10
pix_out <- data.frame()
pix_out_w <- data.frame()
pix_out_c <- data.frame()

for (w in 1:length(watersheds)){
  filenames <- list.files(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w]), pattern = ".csv", full.names = T)
  cut_no <- length(list.files(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w]), pattern = ".csv", full.names = T))
  
  for (c in 1:(cut_no)){
    dat <- fread(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w], "/modeled_xgb_belowground_biomass_", watersheds[w], "_cutno", c, ".csv"), select = c("pix", "date", "local_hiwater"))
    dat$interval <- ifelse(dat$date < as.Date("2018-12-31"), 1, 2)
    dat <- dat %>%
      filter(!is.na(local_hiwater))
    dat$col <- ifelse(dat$pix %in% agb_decline_pix, 1, 0)
    dat_whole <- dat %>%
      group_by(interval) %>%
      summarise(ii_mean_w = mean(local_hiwater, na.rm = T), ii_sd_w = sd(local_hiwater, na.rm = T), n_w = n())
    dat_col <- dat %>%
      group_by(interval, col) %>%
      summarise(ii_mean_c = mean(local_hiwater, na.rm = T), ii_sd_c = sd(local_hiwater, na.rm = T), n_c = n())
    pix_out_w <- rbind(pix_out_w, dat_whole)
    pix_out_c <- rbind(pix_out_c, dat_col)
  }
  write_csv(pix_out_w, paste0("/media/kyle/Seagate Expansion Drive/data/results/ii_stats_w/", watersheds[w], ".csv"))
  write_csv(pix_out_c, paste0("/media/kyle/Seagate Expansion Drive/data/results/ii_stats_c/", watersheds[w], ".csv"))
  
}
filenames <- list.files("/media/kyle/Seagate Expansion Drive/data/results/ii_stats_c/", pattern = ".csv", full.names = T)
tables <- lapply(filenames, read.csv, header = TRUE)
df<- do.call(rbind , tables)
df <- df %>%
  group_by(interval, col) %>%
  summarise(ii_mean = weighted.mean(ii_mean_c, n_c),
            ii_sd = weighted.mean(ii_sd_c, n_c))
write_csv(df, "/media/kyle/Seagate Expansion Drive/data/results/ii_stats_c.csv")

# Root to shoot ratio by date

watersheds <- read.csv("data/huc10_list.csv", colClasses = "character")
watersheds <-watersheds$huc10
pix_out <- data.frame()

for (w in 1:length(watersheds)){
  cut_no <- length(list.files(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w]), pattern = ".csv", full.names = T))
  
  for (c in 1:(cut_no)){
    df <- fread(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w], "/modeled_xgb_belowground_biomass_", watersheds[w], "_cutno", c, ".csv"), select = c("pix", "date", "predag.allom.l8", "predbg.xgb"))
    df <- df %>%
      drop_na(date)
    df$mo_num <- time_length(interval(as.Date("2013-12-15"), df$date),  unit = "month")
    df <- df[order(df$pix, df$mo_num),]
    df <- df %>%
      group_by(mo_num) %>%
      summarise(rs = mean(predbg.xgb/predag.allom.l8, na.rm = T), n = n())
    #merge
    pix_out <- rbind(pix_out, df)
    rm(df)
  }
}
write_csv(pix_out, paste0("/media/kyle/Seagate Expansion Drive/data/results/rs/rs_by_month.csv"))

rs_month <- read_csv("/media/kyle/Seagate Expansion Drive/data/results/rs/rs_by_month_mean.csv")
rs_month_lm <- lm(rs ~ mo_num, data = rs_month)
summary(rs_month_lm)

df <- pix_out %>%
  group_by(mo_num) %>%
  summarise(rs = weighted.mean(rs, n))
write_csv(df, "/media/kyle/Seagate Expansion Drive/data/results/rs/rs_by_month_mean.csv")

rs <- read_csv("/media/kyle/Seagate Expansion Drive/data/results/rs/rs_by_month_mean.csv")
rs$year <- ifelse(rs$mo_num <=12, "2014", NA)
rs$year <- ifelse(rs$mo_num >=109, "2023", rs$year)
df <- rs %>%
  group_by(year) %>%
  summarise(rs_mean = mean(rs, na.rm = T))

filenames <- list.files("/media/kyle/Seagate Expansion Drive/data/results/pix_summ/", pattern = ".csv", full.names = T)
tables <- lapply(filenames, read.csv, header = TRUE)
pix_summ<- do.call(rbind , tables)
div <- pix_summ$pix[pix_summ$coef_ag > 0 & pix_summ$coef_bg < 0]

# Root:Shoot Average and Standard Deviation Across Divergent Biomass Pixels
watersheds <- read.csv("data/huc10_list.csv", colClasses = "character")
watersheds <-watersheds$huc10
pix_out <- data.frame()

for (w in 1:length(watersheds)){
  cut_no <- length(list.files(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w]), pattern = ".csv", full.names = T))
  
  for (c in 1:(cut_no)){
    df <- fread(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w], "/modeled_xgb_belowground_biomass_", watersheds[w], "_cutno", c, ".csv"), select = c("pix", "date", "predag.allom.l8", "predbg.xgb"))
    df <- df %>%
      drop_na(date) %>%
      filter(pix %in% div) %>%
      filter(date < as.Date("2014-12-31") | date > as.Date("2023-01-01")) %>%
      mutate(date = as.Date(date)) %>%
      mutate(year = year(date)) %>%
      mutate(rs = predbg.xgb/predag.allom.l8)
    df <- df %>%
      group_by(year) %>%
      summarise(rs_mean = mean(rs, na.rm = T), rs_sd = sd(rs, na.rm = T),n = n())
    #merge
    pix_out <- rbind(pix_out, df)
    rm(df)
  }
}
df <- pix_out %>%
  group_by(year) %>%
  summarise(rs_mean = weighted.mean(rs_mean, n), rs_sd = weighted.mean(rs_sd, n))

write_csv(df, paste0("/media/kyle/Seagate Expansion Drive/data/results/rs/rs_div_2014_2023_mean_sd.csv"))

# Root:Shoot Average and Standard Deviation Across Whole Coast
watersheds <- read.csv("data/huc10_list.csv", colClasses = "character")
watersheds <-watersheds$huc10
pix_out <- data.frame()

for (w in 1:length(watersheds)){
  cut_no <- length(list.files(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w]), pattern = ".csv", full.names = T))
  
  for (c in 1:(cut_no)){
    df <- fread(paste0("/media/kyle/Seagate Expansion Drive/data/results/model_xgb_belowground_biomass/", watersheds[w], "/modeled_xgb_belowground_biomass_", watersheds[w], "_cutno", c, ".csv"), select = c("pix", "date", "predag.allom.l8", "predbg.xgb"))
    df <- df %>%
      drop_na(date) %>%
      filter(date < as.Date("2014-12-31") | date > as.Date("2023-01-01")) %>%
      mutate(date = as.Date(date)) %>%
      mutate(year = year(date)) %>%
      mutate(rs = predbg.xgb/predag.allom.l8)
    df <- df %>%
      group_by(year) %>%
      summarise(rs_mean = mean(rs, na.rm = T), rs_sd = sd(rs, na.rm = T),n = n())
    #merge
    pix_out <- rbind(pix_out, df)
    rm(df)
  }
}
df <- pix_out %>%
  group_by(year) %>%
  summarise(rs_mean = weighted.mean(rs_mean, n), rs_sd = weighted.mean(rs_sd, n))
write_csv(df, paste0("/media/kyle/Seagate Expansion Drive/data/results/rs/rs_whole_2014_2023_mean_sd.csv"))

# Root:Shoot Range
rs_min <- min(pix_summ$predbg.xgb/pix_summ$predag.allom.l8)
rs_max <- max(pix_summ$predbg.xgb/pix_summ$predag.allom.l8)
rs_range <- data.frame(rs_min, rs_max)
write_csv(rs_range, paste0("/media/kyle/Seagate Expansion Drive/data/results/rs/rs_range.csv"))


#write tiffs
library(raster);library(rasterVis)
library(latticeExtra);library(tidyverse);library(terra); library(magick)
library(tidyterra)


#change this based on what you want
filenames <- list.files("/media/kyle/Seagate Expansion Drive/data/results/pix_summ/", full.names = T)
newpixels_list <- lapply(filenames, read_csv, lazy = T)
newpixels <- do.call(rbind, newpixels_list)
rm(newpixels_list)
newpixels<-data.frame(newpixels)
newpixels$utm_east <- as.numeric(substr(newpixels$pix, 1,6))
newpixels$utm_north <- as.numeric(substr(newpixels$pix, 8,14))


# mean BGB
newpixels2 <- newpixels %>%
  dplyr::select(utm_east, utm_north, predbg.xgb) #change this for maps
bgarea <-as_spatraster(newpixels2, xycols = 1:2, crs = 32617)
writeRaster(bgarea, file = "output/maps/predbg_mean.tif", overwrite = T)
writeRaster(bgarea, file = "/media/kyle/Seagate Expansion Drive/data/results/maps/data/predbg_mean.tif", overwrite = T)

# trend BGB
newpixels <- newpixels %>%
  mutate(annual_trend = coef_bg/predbg.xgb)
newpixels2 <- newpixels %>%
  dplyr::select(utm_east, utm_north, annual_trend) #change this for maps
bgarea <-as_spatraster(newpixels2, xycols = 1:2, crs = 32617)
writeRaster(bgarea, file = "output/maps/bg_trend_mean.tif", overwrite = T)
writeRaster(bgarea, file = "/media/kyle/Seagate Expansion Drive/data/results/maps/data/bg_trend_mean.tif", overwrite = T)

# mean AGB
newpixels2 <- newpixels %>%
  dplyr::select(utm_east, utm_north, predag.allom.l8) #change this for maps
bgarea <-as_spatraster(newpixels2, xycols = 1:2, crs = 32617)
writeRaster(bgarea, file = "output/maps/predag_mean.tif", overwrite = T)
writeRaster(bgarea, file = "/media/kyle/Seagate Expansion Drive/data/results/maps/data/predag_mean.tif", overwrite = T)

# trend AGB
newpixels2 <- newpixels %>%
  mutate(annual_trend = coef_ag/predag.allom.l8)
newpixels2 <- newpixels2 %>%
  dplyr::select(utm_east, utm_north, annual_trend) #change this for maps
bgarea <-as_spatraster(newpixels2, xycols = 1:2, crs = 32617)
writeRaster(bgarea, file = "output/maps/ag_trend_mean.tif", overwrite = T)
writeRaster(bgarea, file = "/media/kyle/Seagate Expansion Drive/data/results/maps/data/ag_trend_mean.tif", overwrite = T)

# change BGB
newpixels <- read_csv("results/bgb_max_change.csv")
newpixels$utm_east <- as.numeric(substr(newpixels$pix, 1,6))
newpixels$utm_north <- as.numeric(substr(newpixels$pix, 8,14))

newpixels2 <- newpixels %>%
  dplyr::select(utm_east, utm_north, bgb_raw_change) #change this for maps
bgarea <-as_spatraster(newpixels2, xycols = 1:2, crs = 32617)
writeRaster(bgarea, file = "output/maps/bgb_max_change.tif", overwrite = T)
writeRaster(bgarea, file = "/media/kyle/Seagate Expansion Drive/data/results/maps/data/bgb_max_change.tif", overwrite = T)

# flooding frequency
newpixels <- read_csv("/media/kyle/Seagate Expansion Drive/data/results/pix_attr/pix_attr.csv")
newpixels$utm_east <- as.numeric(substr(newpixels$pix, 1,6))
newpixels$utm_north <- as.numeric(substr(newpixels$pix, 8,14))

newpixels2 <- newpixels %>%
  dplyr::select(utm_east, utm_north, flood_time) #change this for maps
bgarea <-as_spatraster(newpixels2, xycols = 1:2, crs = 32617)
writeRaster(bgarea, file = "output/maps/flood_time.tif", overwrite = T)
writeRaster(bgarea, file = "/media/kyle/Seagate Expansion Drive/data/results/maps/data/flood_time.tif", overwrite = T)

# elevation
newpixels2 <- newpixels %>%
  dplyr::select(utm_east, utm_north, elevation) #change this for maps
bgarea <-as_spatraster(newpixels2, xycols = 1:2, crs = 32617)
writeRaster(bgarea, file = "output/maps/elevation.tif", overwrite = T)
writeRaster(bgarea, file = "/media/kyle/Seagate Expansion Drive/data/results/maps/data/elevation.tif", overwrite = T)


# grab stats for marsh units

flux_marsh <- vect("/media/kyle/Seagate Expansion Drive/data/results/maps/data/FluxOutline.shp")
flux_marsh <- project(flux_marsh, "epsg:32617")
skida_marsh <-  vect("/media/kyle/Seagate Expansion Drive/data/results/maps/data/skidaway_outline.shp")
skida_marsh <- project(skida_marsh, "epsg:32617")

#trend AGB
ag_pix_trend <- rast("/media/kyle/Seagate Expansion Drive/data/results/maps/data/ag_trend_mean.tif")
ag_pix_trend <- ag_pix_trend*12*100 ## trend is actually monthly, so multiply by 12 to get annual and 100 to get %
flux_trend_agb <- crop(ag_pix_trend, flux_marsh)
flux_trend_agb_mean <- global(flux_trend_agb, mean, na.rm = T)
skida_trend_agb <- crop(ag_pix_trend, skida_marsh)
skida_trend_agb_mean <- global(skida_trend_agb, mean, na.rm = T)

#trend BGB
bg_pix_trend <- rast("/media/kyle/Seagate Expansion Drive/data/results/maps/data/bg_trend_mean.tif")
bg_pix_trend <- bg_pix_trend*12*100 ## trend is actually monthly, so multiply by 12 to get annual and 100 to get %
flux_trend_bgb <- crop(bg_pix_trend, flux_marsh)
flux_trend_bgb_mean <- global(flux_trend_bgb, mean, na.rm = T)
skida_trend_bgb <- crop(bg_pix_trend, skida_marsh)
skida_trend_bgb_mean <- global(skida_trend_bgb, mean, na.rm = T)

# flood_time
flood_time <- rast("/media/kyle/Seagate Expansion Drive/data/results/maps/data/flood_time.tif")
flux_flood <- crop(flood_time, flux_marsh)
flux_flood_mean <- global(flux_flood, mean, na.rm = T)
skida_flood <- crop(flood_time, skida_marsh)
skida_flood_mean <- global(skida_flood, mean, na.rm = T)

# vul_pix
vulnerable <- rast("/media/kyle/Seagate Expansion Drive/data/results/maps/data/bgb_max_change.tif")
vulnerable[vulnerable >= -344] <- 0
vulnerable[vulnerable < -344] <- 1
flux_vul <- crop(vulnerable, flux_marsh)
flux_vul_mean <- global(flux_vul, mean, na.rm = T)
skida_vul <- crop(vulnerable, skida_marsh)
skida_vul_mean <- global(skida_vul, mean, na.rm = T)

trend_agb <- c(flux_trend_agb_mean$mean, skida_trend_agb_mean$mean)
trend_bgb <- c(flux_trend_bgb_mean$mean, skida_trend_bgb_mean$mean)
flood <- c(flux_flood_mean$mean, skida_flood_mean$mean)
vul <- c(flux_vul_mean$mean, skida_vul_mean$mean)
site <- c("flux", "skida")
out <- data.frame(site, trend_agb, trend_bgb, flood, vul)

write_csv(out, file = "results/marsh_unit_stats.csv")

#vulnerable pixels
vulnerable <- rast("/media/kyle/Seagate Expansion Drive/data/results/maps/data/bgb_max_change.tif")
vulnerable[vulnerable >= -344] <- 0
vulnerable[vulnerable < -344] <- 1
vul_mean <- global(vulnerable, mean, na.rm = T)
vul_df <- as.data.frame(vulnerable, xy = T)
vul_df <- vul_df %>%
  mutate(pix = paste(x, y)) %>%
  rename(vul = bgb_raw_change) %>%
  dplyr::select(-c(x,y))
write_csv(vul_df, file = "results/vul_pix.csv")
pix_attr <- read_csv("/media/kyle/Seagate Expansion Drive/data/results/pix_attr/pix_attr.csv")
vul_pix <- merge(vul_df, pix_attr, by = "pix")


filenames <- list.files("/media/kyle/Seagate Expansion Drive/data/results/pix_summ", full.names = T)
pix_summ <- lapply(filenames, read_csv, lazy = T)
pix_summ <- do.call(rbind, pix_summ)
pix_summ <- pix_summ %>%
  dplyr::select(pix, predag.allom.l8, predbg.xgb, coef_ag, coef_bg)
pix_summ$ag_trend <- pix_summ$coef_ag / pix_summ$predag.allom.l8 *12*100
pix_summ$bg_trend <- pix_summ$coef_bg / pix_summ$predbg.xgb *12*100

vul_pix2 <- merge(vul_pix, pix_summ, by = "pix")
vul_pix3 <- vul_pix2 %>%
  group_by(vul) %>%
  summarise(across(where(is.numeric), mean, na.rm = T)) %>%
  dplyr::select(-c(coef_ag, coef_bg, flood_time))
write_csv(vul_pix3, file = "results/vul_pix_metrics.csv")

---
  agb_decline <- read_csv("/media/kyle/Seagate Expansion Drive/data/results/agb_annual_mean_min_max.csv")

agb_decline3 <- agb_decline %>%
  dplyr::select(pix, year, agb_max) %>%
  filter(year > 2018) %>%
  group_by(pix) %>%
  filter(agb_max == max(agb_max) | agb_max == min(agb_max)) %>%
  summarise(agb_raw_change = agb_max[year == max(year)] - agb_max[year == min(year)])

write_csv(agb_decline3, "/media/kyle/Seagate Expansion Drive/data/results/agb_max_change_int2.csv")

vul <- read_csv("/media/kyle/Seagate Expansion Drive/data/results/agb_max_change_int2.csv")
vul %>%
  filter(agb_raw_change < -55) %>%
  summarise(count = n())
agb_decline_pix <- agb_decline3$pix[agb_decline3$agb_raw_change < -55]

#agb_decline
agb_decline <- read_csv("/media/kyle/Seagate Expansion Drive/data/results/agb_annual_mean_min_max.csv")
agb_decline3 <- agb_decline %>%
  dplyr::select(pix, year, agb_max) %>%
  filter(year > 2018) %>%
  group_by(pix) %>%
  filter(agb_max == max(agb_max) | agb_max == min(agb_max)) %>%
  summarise(agb_raw_change = agb_max[year == max(year)] - agb_max[year == min(year)])
length(which(agb_decline3$agb_raw_change < -55))

newpixels <- agb_decline3
newpixels$utm_east <- as.numeric(substr(newpixels$pix, 1,6))
newpixels$utm_north <- as.numeric(substr(newpixels$pix, 8,14))

newpixels2 <- newpixels %>%
  dplyr::select(utm_east, utm_north, agb_raw_change) #change this for maps
bgarea <-as_spatraster(newpixels2, xycols = 1:2, crs = 32617)
#writeRaster(bgarea, file = "output/maps/predbg_mean.tif", overwrite = T)
writeRaster(bgarea, file = "/media/kyle/Seagate Expansion Drive/data/results/maps/data/agb_max_change.tif", overwrite = T)

#collapsed/deteriorated pix
#collapsed pixels
collapsed <- rast("/media/kyle/Seagate Expansion Drive/data/results/maps/data/agb_max_change.tif")
collapsed[collapsed >= -55] <- 0
collapsed[collapsed < -55] <- 1
col_mean <- global(collapsed, mean, na.rm = T)
write_csv(col_mean, "/media/kyle/Seagate Expansion Drive/data/results/col_mean.csv")
col_df <- as.data.frame(collapsed, xy = T)
col_df <- col_df %>%
  mutate(pix = paste(x, y)) %>%
  rename(col = agb_raw_change) %>%
  dplyr::select(-c(x,y))
write_csv(col_df, file = "results/col_pix.csv")
collapsed_pixels <- col_df$pix[col_df$col == 1]
pix_attr <- read_csv("/media/kyle/Seagate Expansion Drive/data/results/pix_attr/pix_attr_interval.csv")
col_pix <- merge(col_df, pix_attr, by = "pix")

filenames <- list.files("/media/kyle/Seagate Expansion Drive/data/results/pix_summ_5yr", full.names = T)
pix_summ <- lapply(filenames, read_csv, lazy = T)
pix_summ <- do.call(rbind, pix_summ)
pix_summ <- pix_summ %>%
  dplyr::select(pix, predag.allom.l8, predbg.xgb, coef_ag, coef_bg, interval)
pix_summ$ag_trend <- pix_summ$coef_ag / pix_summ$predag.allom.l8 *12*100
pix_summ$bg_trend <- pix_summ$coef_bg / pix_summ$predbg.xgb *12*100

col_pix2 <- merge(col_pix, pix_summ, by = c("pix", "interval"))

# Stats by marsh location/spartina height forms

zone <- read_csv("/media/kyle/Seagate Expansion Drive/data/results/spal_ht_zone.csv")
pix_attr <- read_csv("/media/kyle/Seagate Expansion Drive/data/results/pix_attr/pix_attr.csv")
vul_pix <- read_csv("results/vul_pix.csv")

filenames <- list.files("/media/kyle/Seagate Expansion Drive/data/results/pix_summ", full.names = T)
pix_summ <- lapply(filenames, read_csv, lazy = T)
pix_summ <- do.call(rbind, pix_summ)
pix_summ <- pix_summ %>%
  dplyr::select(pix, predag.allom.l8, predbg.xgb, coef_ag, coef_bg)
pix_summ$ag_trend <- pix_summ$coef_ag / pix_summ$predag.allom.l8 *12*100
pix_summ$bg_trend <- pix_summ$coef_bg / pix_summ$predbg.xgb *12*100

pix_summ <- merge(pix_summ, zone, by = "pix")
pix_summ <- merge(pix_summ, pix_attr, by = "pix")
pix_summ <- merge(pix_summ, vul_pix, by = "pix")

zone_counts <- as.data.frame(table(pix_summ$zone))
zone_counts <- spread(zone_counts, key = Var1, value = Freq)

write_csv(zone_counts, file = "results/zone_counts.csv")

pix_summ2 <- pix_summ %>%
  group_by(zone) %>%
  summarise(agb_trend = mean(ag_trend), bgb_trend = mean(bg_trend), agb = mean(predag.allom.l8), bgb = mean(predbg.xgb), elev = mean(elevation), ff = mean(flood_time), vul = mean(vul))

write_csv(pix_summ2, file = "results/height_forms_stats.csv")

# Paket listesini tanımla
packages <- c("sf", "dplyr", "tidyverse", "ggplot2", 
              "paletteer", "ggthemes", "ggpubr", "spdep", "tmap", 
              "stars", "gstat", "doParallel", "leaflet")

# Eksik paketleri kontrol et ve yükle
missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]

if(length(missing_packages)) {
  install.packages(missing_packages, dependencies = TRUE)
}

# Tüm paketleri yükle
lapply(packages, library, character.only = TRUE)

library(quadkeyr)
library(sf)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(paletteer)
library(ggthemes)
library(ggpubr)
library(spdep)
library(tmap)
library(stars)
library(gstat)
library(doParallel)
library(leaflet)

loaded_data <- load("C:\\Users\\LENOVO\\Desktop\\Master\\Winter-2024-2025\\Study_Project\\cityHADcomparison\\filleddortmundHAD.Rda")
dortmund <- get(loaded_data)

loaded_data <- load("C:\\Users\\LENOVO\\Desktop\\Master\\Winter-2024-2025\\Study_Project\\cityHADcomparison\\filled_duisburgHAD.Rda")
duisburg <- get(loaded_data)

loaded_data <- load("C:\\Users\\LENOVO\\Desktop\\Master\\Winter-2024-2025\\Study_Project\\cityHADcomparison\\filledHAD_Leipzig.Rda")
leipzig <- get(loaded_data)

loaded_data <- load("C:\\Users\\LENOVO\\Desktop\\Master\\Winter-2024-2025\\Study_Project\\cityHADcomparison\\filledHADdresden.Rda")
dresden <- get(loaded_data)

# Calculate Weeks
dortmund$UNIX_WEEK <- week(as.Date(dortmund$AGG_DAY_PERIOD))
duisburg$UNIX_WEEK <- week(as.Date(duisburg$AGG_DAY_PERIOD))
leipzig$UNIX_WEEK <- week(as.Date(leipzig$AGG_DAY_PERIOD))
dresden$UNIX_WEEK <- week(as.Date(dresden$AGG_DAY_PERIOD))

# Get only full weeks with 7 days to ensure compatability
dortmund <- dortmund[dortmund$UNIX_WEEK < 53, ]
duisburg <- duisburg[duisburg$UNIX_WEEK < 53, ]
leipzig <- leipzig[leipzig$UNIX_WEEK < 53, ]
dresden <- dresden[dresden$UNIX_WEEK < 53, ]

# Compute the weekly average
weekly_avg_dortmund <- dortmund %>%
  group_by(GEOGRAPHY,UNIX_WEEK) %>%
  summarize( mean_weekly = mean(ACTIVITY_INDEX_TOTAL))
weekly_avg_duisburg <- duisburg %>%
  group_by(GEOGRAPHY,UNIX_WEEK) %>%
  summarize( mean_weekly = mean(ACTIVITY_INDEX_TOTAL))
weekly_avg_leipzig <- leipzig %>%
  group_by(GEOGRAPHY,UNIX_WEEK) %>%
  summarize( mean_weekly = mean(ACTIVITY_INDEX_TOTAL))
weekly_avg_dresden <- dresden %>%
  group_by(GEOGRAPHY,UNIX_WEEK) %>%
  summarize( mean_weekly = mean(ACTIVITY_INDEX_TOTAL))

# Compute yearly average
year_avg_dortmund <- weekly_avg_dortmund %>%
  group_by(GEOGRAPHY) %>%
  summarize( mean_year = mean(mean_weekly))

year_avg_duisburg <- weekly_avg_duisburg %>%
  group_by(GEOGRAPHY) %>%
  summarize( mean_year = mean(mean_weekly))

year_avg_leipzig <- weekly_avg_leipzig %>%
  group_by(GEOGRAPHY) %>%
  summarize( mean_year = mean(mean_weekly))

year_avg_dresden <- weekly_avg_dresden %>%
  group_by(GEOGRAPHY) %>%
  summarize( mean_year = mean(mean_weekly))

###### Compute Difference week by week ###### 
weekly_diff_dortmund <- weekly_avg_dortmund %>%  mutate(diff_weekly = mean_weekly - lag(mean_weekly))
weekly_diff_duisburg <- weekly_avg_duisburg %>%  mutate(diff_weekly = mean_weekly - lag(mean_weekly))
weekly_diff_leipzig <- weekly_avg_leipzig %>%  mutate(diff_weekly = mean_weekly - lag(mean_weekly))
weekly_diff_dresden <- weekly_avg_dresden %>%  mutate(diff_weekly = mean_weekly - lag(mean_weekly))

# Omit the first and last week of the year because you cannot calculate a changerate for those
weekly_diff_dortmund <- weekly_diff_dortmund %>%
  filter(UNIX_WEEK > 1 & UNIX_WEEK < 52)

weekly_diff_duisburg <- weekly_diff_duisburg %>%
  filter(UNIX_WEEK > 1 & UNIX_WEEK < 52)

weekly_diff_leipzig <- weekly_diff_leipzig %>%
  filter(UNIX_WEEK > 1 & UNIX_WEEK < 52)

weekly_diff_dresden <- weekly_diff_dresden %>%
  filter(UNIX_WEEK > 1 & UNIX_WEEK < 52)

weekly_diff_dortmund_avg <- weekly_diff_dortmund %>%
  group_by(UNIX_WEEK) %>%
  summarize( mean_diff_weekly = mean(diff_weekly))
weekly_diff_duisburg_avg <- weekly_diff_duisburg %>%
  group_by(UNIX_WEEK) %>%
  summarize( mean_diff_weekly = mean(diff_weekly))
weekly_diff_leipzig_avg <- weekly_diff_leipzig %>%
  group_by(UNIX_WEEK) %>%
  summarize( mean_diff_weekly = mean(diff_weekly))
weekly_diff_dresden_avg <- weekly_diff_dresden %>%
  group_by(UNIX_WEEK) %>%
  summarize( mean_diff_weekly = mean(diff_weekly))


milestones <- data.frame(
  week = c(5, 7, 9, 11, 12, 13, 16, 21, 31, 45, 48, 51),
  description = c(
    "27 Jan: First Case",
    "15 Feb: Heinsberg Carnival",
    "25-26 Feb: Italian Outbreak Cases",
    "9 Mar: First Deaths",
    "13 Mar: School Closures",
    "15 Mar: Border Closures",
    "15 Apr: Restrictions Loosened",
    "24, 25, 26 May: Muslim Holiday",
    "1 Aug: Berlin Demonstration",
    "2 Nov: Partial Lockdown",
    "27 Nov: 1M Cases",
    "15 Dec: Hard Lockdown"
  ),
  label_y = -0.0035
)

ggplot() +
  geom_line(data=weekly_diff_dortmund_avg, aes(x=UNIX_WEEK, y=mean_diff_weekly, color="blue")) +
  geom_line(data=weekly_diff_duisburg_avg, aes(x=UNIX_WEEK, y=mean_diff_weekly, color="red")) +
  geom_line(data=weekly_diff_leipzig_avg, aes(x=UNIX_WEEK, y=mean_diff_weekly, color="green")) +
  geom_line(data=weekly_diff_dresden_avg, aes(x=UNIX_WEEK, y=mean_diff_weekly, color="orange")) +
  geom_vline(data = milestones, aes(xintercept = week), color = "red", linetype = "dashed") +
  geom_text(data = milestones, aes(x = week, y = label_y, label = description), 
            angle = 90, hjust = 0.5, vjust = 1.2, color = "red", size = 3) + 
  ggtitle("Differences between Mobility index per Week in Selected Cities") +
  scale_color_manual(name = "Region", labels = c("Dortmund", "Duisburg", "Leipzig", "Dresden"), values = c("red"="red", "blue"="blue", "green"="green", "orange"="orange")) + 
  labs(x="Week", y="Difference to previous week") +
  ylim(-0.005, 0.005)+
  theme_minimal()

ggsave("C:/Users/LENOVO/Desktop/Master/Winter-2024-2025/Study_Project/cityHADcomparison/weekly_change/fourcity.png", bg = "white")

ggplot() +
  geom_line(data=weekly_diff_dortmund_avg, aes(x=UNIX_WEEK, y=mean_diff_weekly, color="blue")) +
  geom_line(data=weekly_diff_leipzig_avg, aes(x=UNIX_WEEK, y=mean_diff_weekly, color="green")) +
  geom_vline(data = milestones, aes(xintercept = week), color = "red", linetype = "dashed") +
  geom_text(data = milestones, aes(x = week, y = label_y, label = description), 
            angle = 90, hjust = 0.5, vjust = 1.2, color = "red", size = 3) + 
  ggtitle("Differences between Mobility index per Week in Selected Cities") +
  scale_color_manual(name = "Region", labels = c("Dortmund", "Leipzig"), values = c("blue"="blue", "green"="green")) + 
  labs(x="Week", y="Difference to previous week")+
  ylim(-0.005, 0.005)+
  theme_minimal()

ggsave("C:/Users/LENOVO/Desktop/Master/Winter-2024-2025/Study_Project/cityHADcomparison/weekly_change/dortmundleipzig.png", bg = "white")

ggplot() +
  geom_line(data=weekly_diff_dortmund_avg, aes(x=UNIX_WEEK, y=mean_diff_weekly, color="blue")) +
  geom_line(data=weekly_diff_duisburg_avg, aes(x=UNIX_WEEK, y=mean_diff_weekly, color="red")) +
  geom_vline(data = milestones, aes(xintercept = week), color = "red", linetype = "dashed") +
  geom_text(data = milestones, aes(x = week, y = label_y, label = description), 
            angle = 90, hjust = 0.5, vjust = 1.2, color = "red", size = 3) + 
  ggtitle("Differences between Mobility index per Week in Selected Cities") +
  scale_color_manual(name = "Region", labels = c("Dortmund", "Duisburg"), values = c("red"="red", "blue"="blue")) + 
  labs(x="Week", y="Difference to previous week")+
  ylim(-0.005, 0.005)+
  theme_minimal()

ggsave("C:/Users/LENOVO/Desktop/Master/Winter-2024-2025/Study_Project/cityHADcomparison/weekly_change/dortmund_duisburg.png", bg = "white")

ggplot() +
  geom_line(data=weekly_diff_leipzig_avg, aes(x=UNIX_WEEK, y=mean_diff_weekly, color="green")) +
  geom_line(data=weekly_diff_dresden_avg, aes(x=UNIX_WEEK, y=mean_diff_weekly, color="orange")) +
  geom_vline(data = milestones, aes(xintercept = week), color = "red", linetype = "dashed") +
  geom_text(data = milestones, aes(x = week, y = label_y, label = description), 
            angle = 90, hjust = 0.5, vjust = 1.2, color = "red", size = 3) + 
  ggtitle("Differences between Mobility index per Week in Selected Cities") +
  scale_color_manual(name = "Region", labels = c("Leipzig", "Dresden"), values = c("green"="green", "orange"="orange")) + 
  labs(x="Week", y="Difference to previous week")+
  ylim(-0.005, 0.005)+
  theme_minimal()

ggsave("C:/Users/LENOVO/Desktop/Master/Winter-2024-2025/Study_Project/cityHADcomparison/weekly_change/LeipzigDresden.png", bg = "white")

###### Calculation of weekly difference to yearly average as base line ###### 
# Merge year_avg and weekly_avg for each city
year_avg_dortmund <- inner_join(year_avg_dortmund, weekly_avg_dortmund, by = "GEOGRAPHY")
year_avg_duisburg <- inner_join(year_avg_duisburg, weekly_avg_duisburg, by = "GEOGRAPHY")
year_avg_leipzig <- inner_join(year_avg_leipzig, weekly_avg_leipzig, by = "GEOGRAPHY")
year_avg_dresden <- inner_join(year_avg_dresden, weekly_avg_dresden, by = "GEOGRAPHY")

dortmund_year_diff <- year_avg_dortmund %>%  mutate(diff_base_weekly = ((mean_weekly - mean_year) / mean_year * 100))
duisburg_year_diff <- year_avg_duisburg %>%  mutate(diff_base_weekly = ((mean_weekly - mean_year) / mean_year * 100))
leipzig_year_diff <- year_avg_leipzig %>%  mutate(diff_base_weekly = ((mean_weekly - mean_year) / mean_year * 100))
dresden_year_diff <- year_avg_dresden %>%  mutate(diff_base_weekly = ((mean_weekly - mean_year) / mean_year * 100))

dortmund_year_diff <- dortmund_year_diff %>%
  filter(UNIX_WEEK > 1 & UNIX_WEEK < 52)

duisburg_year_diff <- duisburg_year_diff %>%
  filter(UNIX_WEEK > 1 & UNIX_WEEK < 52)

leipzig_year_diff <- leipzig_year_diff %>%
  filter(UNIX_WEEK > 1 & UNIX_WEEK < 52)

dresden_year_diff <- dresden_year_diff %>%
  filter(UNIX_WEEK > 1 & UNIX_WEEK < 52)


dortmund_year_diff_avg <- dortmund_year_diff %>%
  group_by(UNIX_WEEK) %>%
  summarize(mean_diff_base_weekly = mean(diff_base_weekly, na.rm = TRUE))

duisburg_year_diff_avg <- duisburg_year_diff %>%
  group_by(UNIX_WEEK) %>%
  summarize(mean_diff_base_weekly = mean(diff_base_weekly, na.rm = TRUE))

leipzig_year_diff_avg <- leipzig_year_diff %>%
  group_by(UNIX_WEEK) %>%
  summarize(mean_diff_base_weekly = mean(diff_base_weekly, na.rm = TRUE))

dresden_year_diff_avg <- dresden_year_diff %>%
  group_by(UNIX_WEEK) %>%
  summarize(mean_diff_base_weekly = mean(diff_base_weekly, na.rm = TRUE))

milestones <- data.frame(
  week = c(5, 7, 9, 11, 12, 13, 16, 21, 31, 45, 48, 51),
  description = c(
    "27 Jan: First Case",
    "15 Feb: Heinsberg Carnival",
    "25-26 Feb: Italian Outbreak Cases",
    "9 Mar: First Deaths",
    "13 Mar: School Closures",
    "15 Mar: Border Closures",
    "15 Apr: Restrictions Loosened",
    "24, 25, 26 May: Muslim Holiday",
    "1 Aug: Berlin Demonstration",
    "2 Nov: Partial Lockdown",
    "27 Nov: 1M Cases",
    "15 Dec: Hard Lockdown"
  ),
  label_y = -40
)

ggplot() +
  geom_line(data=dortmund_year_diff_avg, aes(x=UNIX_WEEK, y=mean_diff_base_weekly, color="blue")) +
  geom_line(data=duisburg_year_diff_avg, aes(x=UNIX_WEEK, y=mean_diff_base_weekly, color="red")) +
  geom_line(data=leipzig_year_diff_avg, aes(x=UNIX_WEEK, y=mean_diff_base_weekly, color="green")) +
  geom_line(data=dresden_year_diff_avg, aes(x=UNIX_WEEK, y=mean_diff_base_weekly, color="orange")) +
  geom_vline(data = milestones, aes(xintercept = week), color = "red", linetype = "dashed") +
  geom_text(data = milestones, aes(x = week, y = label_y, label = description), 
            angle = 90, hjust = 0.5, vjust = 1.2, color = "red", size = 3) + 
  ggtitle("Differences in Mobility Index and Year Average per Week in Selected Cities") +
  scale_color_manual(name = "Region", labels = c("Dortmund", "Duisburg", "Leipzig", "Dresden"), values = c("red"="red", "blue"="blue", "green"="green", "orange"="orange")) + 
  labs(x = "Week", y = "Difference to Year Average in %") +
  ylim(-50, 50)+
  theme_minimal()

ggsave("C:/Users/LENOVO/Desktop/Master/Winter-2024-2025/Study_Project/cityHADcomparison/yearly_weekly_diff/fourcity.png", bg = "white")


ggplot() +
  geom_line(data=dortmund_year_diff_avg, aes(x=UNIX_WEEK, y=mean_diff_base_weekly, color="blue")) +
  geom_line(data=leipzig_year_diff_avg, aes(x=UNIX_WEEK, y=mean_diff_base_weekly, color="green")) +
  geom_vline(data = milestones, aes(xintercept = week), color = "red", linetype = "dashed") +
  geom_text(data = milestones, aes(x = week, y = label_y, label = description), 
            angle = 90, hjust = 0.5, vjust = 1.2, color = "red", size = 3) + 
  ggtitle("Differences in Mobility index and year Average per Week in Dortmund and Leipzig") +
  scale_color_manual(name = "Region", labels = c("Dortmund", "Leipzig"), values = c("blue"="blue", "green"="green")) + 
  labs(x="Week", y="Difference to year average in %")+
  ylim(-50, 50)+
  theme_minimal()

ggsave("C:/Users/LENOVO/Desktop/Master/Winter-2024-2025/Study_Project/cityHADcomparison/yearly_weekly_diff/dortmund_leipzig.png", bg = 'white')

ggplot() +
  geom_line(data=dortmund_year_diff_avg, aes(x=UNIX_WEEK, y=mean_diff_base_weekly, color="blue")) +
  geom_line(data=duisburg_year_diff_avg, aes(x=UNIX_WEEK, y=mean_diff_base_weekly, color="red")) +
  geom_vline(data = milestones, aes(xintercept = week), color = "red", linetype = "dashed") +
  geom_text(data = milestones, aes(x = week, y = label_y, label = description), 
            angle = 90, hjust = 0.5, vjust = 1.2, color = "red", size = 3) + 
  ggtitle("Differences in Mobility index and year Average per Week in Dortmund and Duisburg") +
  scale_color_manual(name = "Region", labels = c("Dortmund", "Duisburg"), values = c("red"="red", "blue"="blue")) + 
  labs(x="Week", y="Difference to year average in %")+
  ylim(-50, 50)+
  theme_minimal()

ggsave("C:/Users/LENOVO/Desktop/Master/Winter-2024-2025/Study_Project/cityHADcomparison/yearly_weekly_diff/dortmund_duisburg.png", bg = 'white')

ggplot() +
  geom_line(data=leipzig_year_diff_avg, aes(x=UNIX_WEEK, y=mean_diff_base_weekly, color="green")) +
  geom_line(data=dresden_year_diff_avg, aes(x=UNIX_WEEK, y=mean_diff_base_weekly, color="orange")) +
  geom_vline(data = milestones, aes(xintercept = week), color = "red", linetype = "dashed") +
  geom_text(data = milestones, aes(x = week, y = label_y, label = description), 
            angle = 90, hjust = 0.5, vjust = 1.2, color = "red", size = 3) + 
  ggtitle("Differences in Mobility index and year Average per Week in Lepzig and Dresden") +
  scale_color_manual(name = "Region", labels = c("Leipzig", "Dresden"), values = c("green"="green", "orange"="orange")) + 
  labs(x="Week", y="Difference to year average in %")+
  ylim(-50, 50)+
  theme_minimal()

ggsave("C:/Users/LENOVO/Desktop/Master/Winter-2024-2025/Study_Project/cityHADcomparison/yearly_weekly_diff/Leipzig_Dresden.png", bg = 'white')
# --------------------------------------------------------------------------------------------------------------------------

# Final Project
# Jensen Brown & Cameron Zinn

# Required libraries
library(rgdal)
library(ggplot2)
library(readxl)
library(tidyverse)

# Reading spreadsheets, merging spreadsheets, and filtering spreadsheet
class_data <- read_xlsx("ENVS 4826 Project Data.xlsx")
tree_data <-  read_xlsx("SMUtreedatabase2021.xlsx")
project_data <- merge(class_data, tree_data, by = "uid_4826")
campus_data <- filter(project_data, location == "SMU campus")
campus_data %>% drop_na(crown_condition)

# Reading shapefiles
campus_buildings <- readOGR(dsn = 'C:/Users/jense/Desktop/ENVS 4826 - Data Science in the Environment/FinalProject/CampusMap_Background_SHP', layer = "CampusBuildings")
greenspace <- readOGR(dsn = 'C:/Users/jense/Desktop/ENVS 4826 - Data Science in the Environment/FinalProject/CampusMap_Background_SHP', layer = "Greenspace")
pathways <- readOGR(dsn = 'C:/Users/jense/Desktop/ENVS 4826 - Data Science in the Environment/FinalProject/CampusMap_Background_SHP', layer = "Pathways")
pavement <- readOGR(dsn = 'C:/Users/jense/Desktop/ENVS 4826 - Data Science in the Environment/FinalProject/CampusMap_Background_SHP', layer = "Pavement")

# Fortifying shapefiles to be dataframes
campus_buildings_df <- fortify(campus_buildings)
greenspace_df <- fortify(greenspace)
pathways_df <- fortify(pathways)
pavement_df <- fortify(pavement)

# Plotting Map
ggplot() +
  geom_polygon(data = campus_buildings_df, aes(x = long, y = lat, group = group), colour = "Blue", fill = "Light Blue") +
  geom_point(data = campus_data, aes(x = UTMX, y = UTMY, col = crown_condition), size = 1) +
  scale_colour_manual(values = c('Yellow', 'Green', 'Red', 'Black'), labels = c('Fair', 'Good', 'Poor', 'NA')) +
  labs(title = "Crown Condition of Trees Around SMU Campus", x = "Longitude", y = "Latitude", colour = "Crown Condition") +
  theme_bw()

#Crown condition Binomial

crown_condition_recode<- recode(campus_data$crown_condition, "G"="G","F"="FP","P"="FP")

ggplot() +
  geom_polygon(data = campus_buildings_df, aes(x = long, y = lat, group = group), colour = "Blue") +
  geom_polygon(data = pavement_df, aes(x = long, y = lat, group = group), colour = "Red", fill="Pink") +
  geom_polygon(data = pathways_df, aes(x = long, y = lat, group = group), colour = "Light Grey") +
  geom_point(data = campus_data, aes(x = UTMX, y = UTMY, col = crown_condition_recode)) +
  scale_colour_manual(values = c('Green', 'Red', 'Black')) +
  geom_polygon(data = campus_buildings_df, aes(x = long, y = lat, group = group), colour = "Blue", fill = "Light Blue") +
  geom_point(data = campus_data, aes(x = UTMX, y = UTMY, col = crown_condition_recode), size = 1) +
  scale_colour_manual(values = c('Green', 'Red', 'Black'), labels = c('Good', 'Poor/Fair', 'NA')) +
  labs(title = "Crown Condition of Trees Around SMU Campus", x = "Longitude", y = "Latitude", colour = "Crown Condition")

#Trunk damages
ggplot() +
  geom_polygon(data = campus_buildings_df, aes(x = long, y = lat, group = group), colour = "Blue") +
  geom_polygon(data = pavement_df, aes(x = long, y = lat, group = group), colour = "Red", fill="Pink") +
  geom_polygon(data = pathways_df, aes(x = long, y = lat, group = group), colour = "Light Grey") +
  geom_point(data = campus_data, aes(x = UTMX, y = UTMY, col = trunk_damage)) +
  scale_colour_manual(values = c('Yellow', 'Green')) +
  geom_polygon(data = campus_buildings_df, aes(x = long, y = lat, group = group), colour = "Blue", fill = "Light Blue") +
  geom_point(data = campus_data, aes(x = UTMX, y = UTMY, col = trunk_damage), size = 1) +
  scale_colour_manual(values = c('Yellow', 'Green'), labels = c('Yes', 'No')) +
  labs(title = "Trunk Damage of Trees Around SMU Campus", x = "Longitude", y = "Latitude", colour = "Trunk Damage")


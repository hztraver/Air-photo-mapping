library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)

## Photo interpretation data within 300m cells
## historic percent cover of herbaceous/graminoid, forest, shrub, bare, water
## change in percent cover of each land cover type 
## proportion of each cell covered by 6 Landsat structural classes 

setwd("E:/MS4/00_code_clean")

#### PHOTO INTERPRETATION SUMMARY TABLES ####
## Photo interpretation within 300m grid cells
## H_ = Historic percent cover of forest, shrub, herb, water, bare
## C_ = Change in percent cover of forest, shrub, herb, water, bare
## values represent categorical ranges (i.e. 1-5%, 5-20% etc)
ptab = read.csv("01_transect_plots.csv")

# name the transects
ptab$Transect_names = factor(ptab$Transect, labels = c("T4:WFE", "T1:CM", "T2:GSU", "T3:WFW"))
# percent change classes: decrease, no change, trace increase, increase 5-10%, 
#     increase 10-20%, increase >20%
ptab %>% 
  mutate(forest_change_class = ifelse(C_Forest < 0, -1, C_Forest)) %>%
  mutate(shrub_change_class = ifelse(C_Shrub < 0, -1, C_Shrub))

## TABLE 4
## frequency tables of forest and shrub change class by transect
options(digits=4)
prop.table(table(ptab$Transect_names, ptab$forest_change_class), margin = 1)*100
prop.table(table(ptab$Transect_names, ptab$shrub_change_class), margin = 1)*100

## TABLE 5
## fire frequency tables
## percent burned vs unburned cells by transect
# 0 = unburned, 1 = burned
prop.table(table(ptab$Fire, ptab$Transect_names), margin = 2)*100

## filter burned cells and calculate increase & decrease in forest and shrub cover 
# decrease cover = -1, no change = 0, increase = 1
fire_table = ptab %>% filter(Fire == 1) %>% 
  mutate(forest_change_class = ifelse(C_Forest < 0, "decrease", ifelse(C_Forest == 0, "no change", "increase"))) %>%
  mutate(shrub_change_class = ifelse(C_Shrub < 0, "decrease", ifelse(C_Shrub == 0, "no change", "increase"))) 

options(digits=4)
prop.table(table(fire_table$forest_change_class, fire_table$Transect_names), margin = 2)*100
prop.table(table(fire_table$shrub_change_class, fire_table$Transect_names), margin = 2)*100

#### LANDSAT CORRELATIONS ####

#### FIGURE 5
## Compare historic percent cover of forest, shrub, herbaceous with Landsat
## stable forest, shrub and no canopy classes within 300m grid cells 
## Labels for historic percent cover classes 
ptab$H_Shrub = as.factor(ptab$H_Shrub)
levels(ptab$H_Shrub) = c("None", "1-5%", "5-25%", "25-40%", "40-60%", ">60%")

ptab$H_Forest = as.factor(ptab$H_Forest)
levels(ptab$H_Forest) = c("None", "1-5%", "5-25%", "25-40%", "40-60%", ">60%")

ptab$H_Gram = as.factor(ptab$H_Gram)
levels(ptab$H_Gram) = c("None", "1-5%", "5-25%", "25-40%", "40-60%", ">60%")

p1 = ptab %>% filter(Fire == 0) %>%
  ggplot(aes(x = as.factor(H_Forest), y = stable_forest, fill = as.factor(H_Forest)))+
  geom_boxplot(outlier.alpha = 0, outlier.size = 2)+
  scale_fill_brewer(palette = "Greens")+
  xlab("Air Photo Historic Forest % Cover")+
  ylab("Landsat % Stable Forest")+
  theme_bw()+
  theme(legend.position = "none", axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 12))

p2 = ptab %>% filter(Fire == 0) %>%
  ggplot(aes(x = as.factor(H_Shrub), y = stable_shr, fill = as.factor(H_Shrub)))+
  geom_boxplot(outlier.alpha = 0, outlier.size = 2)+
  scale_fill_brewer(palette = "Greens")+
  xlab("Air Photo Historic Shrub % Cover")+
  ylab("Landsat % Stable Shrub")+
  theme_bw()+
  theme(legend.position = "none", axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 12))

p3 = ptab %>% filter(Fire == 0) %>%
  ggplot(aes(x = as.factor(H_Gram), y = stable_no, fill = as.factor(H_Gram)))+
  geom_boxplot(outlier.alpha = 0, outlier.size = 2)+
  scale_fill_brewer(palette = "Greens")+
  xlab("Air Photo Historic Herbaceous % Cover")+
  ylab("Landsat % Stable No Canopy")+
  theme_bw()+
  theme(legend.position = "none", axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 12))

grid.arrange(p1, p2, p3, nrow = 1)

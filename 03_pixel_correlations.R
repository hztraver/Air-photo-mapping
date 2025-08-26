library(dplyr)
library(tidyr)
library(caret)
library(gridExtra)
library(ggplot2)

setwd("E:/MS4/00_code_clean")

#### PIXEL CORRELATIONS ####
## agreement between photo interpreted forest and shrub change with 
## landsat classification in 646 pixels (30m)
dt = read.csv("02_pixel_samples.csv") 

# Confusion matrices and plots for each time period and vegetation type
# this is ugly but it works

## Forest 1970
## confusion matrix & accuracy statistics
confusion_matrix = dt %>% filter(time_period == "1970") %>%
  { with(., caret::confusionMatrix(factor(lsat_forest), factor(photo_forest), dnn = c("Landsat Change", "Photo Change"))) }
## convert to data frame and calculate class proportions 
ftab = as.data.frame(confusion_matrix $table)
ftab  = ftab  %>% group_by(Photo.Change) %>% mutate(Photo.Total = sum(Freq)) %>% ungroup()
ftab $Prop = round(ftab$Freq / ftab$Photo.Total, digits = 2)

## this is the unique plot 
x1 = ggplot(ftab , aes(Photo.Change, Landsat.Change,  fill = Prop)) +
  geom_tile() + geom_text(aes(label=Prop), size = 8)+
  scale_fill_gradient(low="white", high="springgreen4")+
  xlab("")+
  ylab("Landsat Change")+
  ggtitle("Forest 1970")+
  theme_bw()+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 15),
        legend.position = "none")

## Forest 1930
confusion_matrix = dt %>% filter(time_period == "1930") %>%
  { with(., caret::confusionMatrix(factor(lsat_forest), factor(photo_forest), dnn = c("Landsat Change", "Photo Change"))) }
ftab = as.data.frame(confusion_matrix$table)
ftab = ftab %>% group_by(Photo.Change) %>% mutate(Photo.Total = sum(Freq)) %>% ungroup()
ftab$Prop = round(ftab$Freq / ftab$Photo.Total, digits = 2)

x2 = ggplot(ftab, aes(Photo.Change, Landsat.Change,  fill = Prop)) +
  geom_tile() + geom_text(aes(label=Prop), size = 8)+
  scale_fill_gradient(low="white", high="springgreen4")+
  xlab("")+
  ylab("")+
  ggtitle("Forest 1930")+
  theme_bw()+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 15),
        legend.position = "none")

## Shrub 1970
confusion_matrix = dt %>% filter(time_period == "1970") %>%
  { with(., caret::confusionMatrix(factor(lsat_shrub), factor(photo_shrub), dnn = c("Landsat Change", "Photo Change"))) }
ftab = as.data.frame(confusion_matrix$table)
ftab = ftab %>% group_by(Photo.Change) %>% mutate(Photo.Total = sum(Freq)) %>% ungroup()
ftab$Prop = round(ftab$Freq / ftab$Photo.Total, digits = 2)

x3 = ggplot(ftab, aes(Photo.Change, Landsat.Change,  fill = Prop)) +
  geom_tile() + geom_text(aes(label=Prop), size = 8)+
  scale_fill_gradient(low="white", high="springgreen4")+
  xlab("Air Photo Change")+
  ylab("Landsat Change")+
  ggtitle("Shrub 1970")+
  theme_bw()+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 15),
        legend.position = "none")

## Shrub 1930
confusion_matrix = dt %>% filter(time_period == "1930") %>%
  { with(., caret::confusionMatrix(factor(lsat_shrub), factor(photo_shrub), dnn = c("Landsat Change", "Photo Change"))) }
ftab = as.data.frame(confusion_matrix$table)
ftab = ftab %>% group_by(Photo.Change) %>% mutate(Photo.Total = sum(Freq)) %>% ungroup()
ftab$Prop = round(ftab$Freq / ftab$Photo.Total, digits = 2)

x4 = ggplot(ftab, aes(Photo.Change, Landsat.Change,  fill = Prop)) +
  geom_tile() + geom_text(aes(label=Prop), size = 8)+
  scale_fill_gradient(low="white", high="springgreen4")+
  xlab("Air Photo Change")+
  ylab("")+
  ggtitle("Shrub 1930")+
  theme_bw()+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 15),
        legend.position = "none")

#### FIGURE 6 ####
## Combine into single plot
grid.arrange(x1, x2, x3, x4, nrow = 2)

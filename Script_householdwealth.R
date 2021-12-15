
setwd("C:/Users/Utente/Dropbox/Chicago_RA")
rm(list=ls())

library(ggplot2)
library(dplyr)
library(tidyverse)
library(hrbrthemes)
library(lubridate)
library(plyr)
library(tibble)
library(zoo)
library(magrittr)
library(matrixStats)
library(scales)
library(data.table)

data <- as.data.table(read.csv("RA_21_22.csv", sep = ","))


maxdata<-data[, .SD[which.max(income)], by = education]
maxincome<-maxdata$income
dummy<-c()
for(n in 1:47776){
  if(data$income[n]%in%maxincome){dummy[n]=1}
  else{dummy[n]=0}
}

data <- data %>%
  add_column(dummy = NA)

data$dummy <- dummy

data[, .SD[which.max(income)], by = education]


# Define the variable Wealth = Assets - Debt

wealth <- data$asset_total - data$debt_total
data[,12] <- wealth
colnames(data)[12] <- "wealth"

# We check first for missing observations
sum(is.na(data$wealth))

#####
##1##
#####

# Since I want to use ggplot2 I create a new year variable in date format to
# as ggplot2 has particular graphic features for time series

year_plot <- as.Date(as.yearmon(data$year))
data[,13] <- year_plot
colnames(data)[13] <- "year_plot"


# Median wealth for Race
# I pretested that matrixStats::weightedMedian() gives correct result withing the
# summarize() function

data_median <- data %>% 
  dplyr::group_by(year_plot, race) %>%
  dplyr::summarize(median_wealth = matrixStats::weightedMedian(wealth, weight))

# I will do all graphs in thousands of dollars as it makes it easier to read
data_median$median_wealth <- data_median$median_wealth/1000

png(file="Median_wealth_race.png")
ggplot(data_median, aes(x=year_plot, y = median_wealth, group = race)) +
  geom_line(aes(y=median_wealth, color = race), size=2) +
  geom_point(aes(y=median_wealth)) +
  xlab("") + 
  theme_ipsum() +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", 
               limits = as.Date(c('1989-01-01','2016-01-01'))) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  ggtitle("Evolution of Median Wealth by Race") +
  ylab("Median Wealth") +
  guides(color=guide_legend(title="Legend")) +
  labs(subtitle = "Median Wealth in Thousand of $ (2016 adjusted)") +
  scale_color_hue(labels = c("Black", "Hispanic", "Other", "White"))
dev.off()

# Median wealth for Education levels

data_median <- data %>% 
  dplyr::group_by(year_plot, education) %>%
  dplyr::summarize(median_wealth = matrixStats::weightedMedian(wealth, weight))

data_median$median_wealth <- data_median$median_wealth/1000

png(file="Median_wealth_Edu.png")
ggplot(data_median, aes(x=year_plot, y = median_wealth, group = education)) +
  geom_line(aes(y=median_wealth, color = education), size=2) +
  geom_point(aes(y=median_wealth)) +
  xlab("") + 
  theme_ipsum() +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", 
               limits = as.Date(c('1989-01-01','2016-01-01'))) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  ggtitle("Evolution of Median Wealth for Education Level") +
  ylab("Median Wealth") +
  guides(color=guide_legend(title="Legend")) +
  labs(subtitle = "Median Wealth in Thousands of $ (2016 adjusted)") +
  scale_color_hue(labels = c("College Degree", "No College", "Some College"))
dev.off()


################################################################################

#####
##2##
#####

# Median Wealth for black and white households

data[,14] <- data$asset_total - data$asset_housing
colnames(data)[14] <- "asset_nonhousing"

data[,15] <- data$debt_total - data$debt_housing
colnames(data)[15] <- "debt_nonhousing"

data[,16] <- data$asset_nonhousing - data$debt_nonhousing
colnames(data)[16] <- "wealth_nonhousing"

data[,17] <- data$asset_housing - data$debt_housing
colnames(data)[17] <- "wealth_housing"

data_median <- data %>% 
  dplyr::group_by(year_plot, race) %>%
  dplyr::summarize(median_wealth_housing = matrixStats::weightedMedian(wealth_housing, weight))

data_median$median_wealth_housing <- data_median$median_wealth_housing/1000

# Here we take the subset of data that we want to plot

data_median_bw <- subset.data.frame(data_median, race == "white" | race == "black")

png(file="Median_housing_wealth_all.png")
ggplot(data_median_bw, aes(x=year_plot, y = median_wealth_housing, group = race)) +
  geom_line(aes(y=median_wealth_housing, color = race), size=2) +
  geom_point(aes(y=median_wealth_housing)) +
  xlab("") + 
  theme_ipsum() +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", 
               limits = as.Date(c('1989-01-01','2016-01-01'))) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  ggtitle("Median Housing Wealth for Black and White Households") +
  ylab("Median Housing Wealth") +
  guides(color=guide_legend(title="Legend")) +
  labs(subtitle = "Median Wealth in Thousands of $ (2016 adjusted)") +
  scale_color_hue(labels = c("Black", "White"))
dev.off()

# We can also see trend by race and education together, for clarity in the figure
# we do not consider some college

data_median <- data %>% 
  dplyr::group_by(year_plot, race, education) %>%
  dplyr::summarize(median_wealth_housing = matrixStats::weightedMedian(wealth_housing, weight))

data_median$median_wealth_housing <- data_median$median_wealth_housing/1000

data_median_bw <- subset.data.frame(data_median, race == "white" | race == "black")
data_median_bw <- subset.data.frame(data_median_bw, education != "some college")

for (n in 1:40) {
  
  if (data_median_bw[n,2] == "black" & data_median_bw[n,3] == "college degree") {
    data_median_bw[n,5] = "Black and College"
  } else if (data_median_bw[n,2] == "black" & data_median_bw[n,3] == "no college"){
    data_median_bw[n,5] = "Black No College"
  } else if (data_median_bw[n,2] == "white" & data_median_bw[n,3] == "college degree"){
    data_median_bw[n,5] = "White and College"
  } else if (data_median_bw[n,2] == "white" & data_median_bw[n,3] == "no college"){
    data_median_bw[n,5] = "White No College"
  } 
}

colnames(data_median_bw)[5] <- "edu_race"

png(file="Median_wealth_BW_Edu.png")
ggplot(data_median_bw, aes(x=year_plot, y = median_wealth_housing, group = edu_race)) +
  geom_line(aes(y=median_wealth_housing, color = edu_race), size=2) +
  geom_point(aes(y=median_wealth_housing)) +
  xlab("") + 
  theme_ipsum() +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", 
               limits = as.Date(c('1989-01-01','2016-01-01'))) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  ggtitle("Median Housing Wealth for Black and White Households") +
  ylab("Median Housing Wealth") +
  guides(color=guide_legend(title="Legend")) +
  labs(subtitle = "Median Wealth in Thousands of $ (2016 adjusted)")
dev.off()

################################################################################

#####
##3##
#####

# Now let us restrict the data to 25 yrs old or older black or white individuals
# in the original dataset

data_25 <- subset.data.frame(data, (age >= 25 & race == "white") | (age >= 25 & race=="black"))

# Here we create the non-housing assets and debts, the non housing and housing
# wealth

data_25[,14] <- data_25$asset_total - data_25$asset_housing
colnames(data_25)[14] <- "asset_nonhousing"

data_25[,15] <- data_25$debt_total - data_25$debt_housing
colnames(data_25)[15] <- "debt_nonhousing"

data_25[,16] <- data_25$asset_nonhousing - data_25$debt_nonhousing
colnames(data_25)[16] <- "wealth_nonhousing"

data_25[,17] <- data_25$asset_housing - data_25$debt_housing
colnames(data_25)[17] <- "wealth_housing"

data_median_25 <- data_25 %>% 
  dplyr::group_by(year_plot, race) %>%
  dplyr::summarize(median_wealth = matrixStats::weightedMedian(wealth, weight),
                   median_wealth_housing = matrixStats::weightedMedian(wealth_housing, weight),
                   median_wealth_nonhousing = matrixStats::weightedMedian(wealth_nonhousing, weight))

data_median_25$median_wealth <- data_median_25$median_wealth/1000
data_median_25$median_wealth_housing <- data_median_25$median_wealth_housing/1000
data_median_25$median_wealth_nonhousing <- data_median_25$median_wealth_nonhousing/1000

# Trend in total wealth

png(file="Median_wealth_25.png")
ggplot(data_median_25, aes(x=year_plot, y = median_wealth, group = race)) +
  geom_line(aes(y=median_wealth, color = race), size=2) +
  geom_point(aes(y=median_wealth)) +
  xlab("") + 
  theme_ipsum() +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", 
               limits = as.Date(c('1989-01-01','2016-01-01'))) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  ggtitle("Evolution of Median Total Wealth by Race") +
  ylab("Median Wealth") +
  guides(color=guide_legend(title="Legend")) +
  labs(subtitle = "Subsample of Black and White Households above 25 years old (Wealth in Thousand of $, 2016 adjusted)") +
  scale_color_hue(labels = c("Black", "White"))

dev.off()

# Trend in housing wealth

png(file="Median_housing_wealth_25.png")
ggplot(data_median_25, aes(x=year_plot, y = median_wealth_housing, group = race)) +
  geom_line(aes(y=median_wealth_housing, color = race), size=2) +
  geom_point(aes(y=median_wealth_housing)) +
  xlab("") + 
  theme_ipsum() +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", 
               limits = as.Date(c('1989-01-01','2016-01-01'))) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  ggtitle("Median Housing Wealth by Race") +
  ylab("Median Housing Wealth") +
  guides(color=guide_legend(title="Legend")) +
  labs(subtitle = "Subsample of Black and White Households above 25 years old") +
  scale_color_hue(labels = c("Black", "White"))
dev.off()

# Trend in non-housing wealth

png(file="Median_nonhousing_wealth_25.png")
ggplot(data_median_25, aes(x=year_plot, y = median_wealth_nonhousing, group = race)) +
  geom_line(aes(y=median_wealth_nonhousing, color = race), size=2) +
  geom_point(aes(y=median_wealth_nonhousing)) +
  xlab("") + 
  theme_ipsum() +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", 
               limits = as.Date(c('1989-01-01','2016-01-01'))) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  ggtitle("Median Non-housing Wealth by Race") +
  ylab("Median Non-Housing Wealth") +
  guides(color=guide_legend(title="Legend")) +
  labs(subtitle = "Subsample of Black and White Households above 25 years old") +
  scale_color_hue(labels = c("Black", "White"))
dev.off()

# Here we create both the absolute and relative changes from 2007 levels for
# both races

for (n in 1:20) {
  if (data_median_25$race[n] == "white"){
    data_median_25[n,6] <- data_median_25$median_wealth_housing[n] - data_median_25$median_wealth_housing[14]
    data_median_25[n,7] <- (data_median_25$median_wealth_housing[n] - data_median_25$median_wealth_housing[14])/data_median_25$median_wealth_housing[14]
  } else if (data_median_25$race[n] == "black"){
    data_median_25[n,6] <- data_median_25$median_wealth_housing[n] - data_median_25$median_wealth_housing[13]
    data_median_25[n,7] <- (data_median_25$median_wealth_housing[n] - data_median_25$median_wealth_housing[13])/data_median_25$median_wealth_housing[13]
  } 
}
 
colnames(data_median_25)[6] <- "change_wealth"
colnames(data_median_25)[7] <- "rel_change_wealth"

# Here we plot absolute changes in wealth

png(file="Absolute_Changes.png")
ggplot(data_median_25, aes(x=year_plot, y = change_wealth, group = race)) +
  geom_line(aes(y=change_wealth, color = race), size=2) +
  geom_point(aes(y=change_wealth)) +
  xlab("") + 
  theme_ipsum() +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", 
               limits = as.Date(c('1989-01-01','2016-01-01'))) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  ggtitle("Absolute Changes in Median Wealth by Race") +
  ylab("$ Change in Median Housing Wealth") +
  guides(color=guide_legend(title="Legend")) +
  labs(subtitle = "Median Housing Wealth in Thousand of $ (2016 adjusted, 2007 as baseline)") +
  scale_color_hue(labels = c("Black", "White"))
dev.off()

# Here we plot relative changes in wealth

png(file="Relative_Changes.png")
ggplot(data_median_25, aes(x=year_plot, y = rel_change_wealth, group = race)) +
  geom_line(aes(y=rel_change_wealth, color = race), size=2) +
  geom_point(aes(y=rel_change_wealth)) +
  xlab("") + 
  theme_ipsum() +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", 
               limits = as.Date(c('1989-01-01','2016-01-01'))) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  ggtitle("Relative Changes in Median Wealth by Race") +
  ylab("% Change in Median Housing Wealth") +
  guides(color=guide_legend(title="Legend")) +
  labs(subtitle = "Median Housing Wealth in Thousand of $ (2016 adjusted, 2007 as baseline)") +
  scale_color_hue(labels = c("Black", "White"))
dev.off()

################################################################################

data_median_25 <- data_25 %>% 
  dplyr::group_by(year_plot, race) %>%
  dplyr::summarize(median_income = matrixStats::weightedMedian(income, weight))

data_median_25$median_income <- data_median_25$median_income/1000

png(file="Income_gap.png")
ggplot(data_median_25, aes(x=year_plot, y = median_income, group = race)) +
  geom_line(aes(y= median_income, color = race), size=2) +
  geom_point(aes(y= median_income)) +
  xlab("") + 
  theme_ipsum() +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", 
               limits = as.Date(c('1989-01-01','2016-01-01'))) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  ggtitle("Evolution of Gap in Median Income by Race") +
  ylab("Median Income") +
  guides(color=guide_legend(title="Legend")) +
  labs(subtitle = "Median Income in Thousand of $ (2016 adjusted)") +
  scale_color_hue(labels = c("Black", "White"))
dev.off()



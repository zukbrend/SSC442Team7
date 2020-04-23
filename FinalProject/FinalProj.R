# Load required libraries
library(tidyverse)
library(ggplot2)
library(caret)

# Load in data sets from .csv files 
Metro15 <- read.csv("DataSets/15_Metro_All.csv")
Metro16 <- read.csv("DataSets/16_Metro_All.csv")
Metro17 <- read.csv("DataSets/17_Metro_All.csv")
Metro18 <- read.csv("DataSets/18_Metro_All.csv")
State15 <- read.csv("DataSets/15_State_All.csv")
State16 <- read.csv("DataSets/16_State_All.csv")
State17 <- read.csv("DataSets/17_State_All.csv")
State18 <- read.csv("DataSets/18_State_All.csv")
teamRankings <- read.csv("DataSets/Team_Year_Rankings.csv")

# Initial visualizations

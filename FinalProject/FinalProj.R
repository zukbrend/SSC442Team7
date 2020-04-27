# Load required libraries
library(tidyverse)
library(gridExtra)
library(ggpubr)
library(glmnet)
library(broom)
library(caret)
library(RColorBrewer)

# Load in data sets from .csv files 
Metro15 <- read.csv("DataSets/15_Metro_All.csv")
Metro16 <- read.csv("DataSets/16_Metro_All.csv")
Metro17 <- read.csv("DataSets/17_Metro_All.csv")
Metro18 <- read.csv("DataSets/18_Metro_All.csv")
teamRankings <- read.csv("DataSets/Team_Year_Rankings.csv")

# Initial Pre-processing to remove random characters added during csv conversion
names(Metro15) <- 
  c( 
    "Year", "City", "State", "Team", "PositionInLeague", "InSuperBowl", "WonSuperBowl",
    "TotalPop", "TotalMale", "PctTotalMale", "TotalFemale",	"PctTotalFemale",	
    "Under5",	"PctUnder5", "5to9", "Pct5to9",	"10to14",	"Pct10to14", "15to19",
    "Pct15to19", "20to24", "Pct20to24",	"25to34",	"Pct25to34", "35to44", "Pct35to44",
    "45to54",	"Pct45to54", "55to59", "Pct55to59",	"60to64",	"Pct60to64", "65to74", "Pct65to74",
    "75to84",	"Pct75to84", "85andover",	"Pct85andover",	"18andover", "Pct18andover", "18andoverMale",
    "Pct18andoverMale",	"18andoverFemale", "Pct18andoverFemale", "65andover", "Pct65andover",	
    "65andoverMale",	"Pct65andoverMale",	"65andoverFemale", "Pct65andoverFemale", "Onerace",
    "PctOnerace", "White", "PctWhite", "BlackorAfricanAmerican", "PctBlackorAfricanAmerican",
    "AmericanIndianandAlaskaNative", "PctAmericanIndianandAlaskaNative", "Asian", "PctAsian",
    "NativeHawaiianandOtherPacificIslander", "PctNativeHawaiianandOtherPacificIslander",
    "HispanicLatino",	"PctHispanicLatino", "Someotherrace",	"PctSomeotherrace",	"Twoormoreraces",
    "PctTwoormoreraces", "BelowPovertyLevel",	"AtAbovePovertyLevel", "LessHighSchool",
    "HighSchoolGraduate",	"SomeCollege/Associates", "Bachelors+",	"Employment16to19",
    "Employment20to24",	"Employment25to29",	"Employment30to34",	"Employment35to44",
    "Employment45to54", "Employment55to59",	"Employment60to64",	"Employment65to74",
    "Employment75andover", "Employment20to64", "Employment20to64Male", "Employment20to64Female",
    "EmploymentBelowpovertylevel", "EmploymentAtAbovePovertyLevel", "Employment25to64",
    "EmploymentLessHighSchool", "EmploymentHighSchool", "EmploymentSomeCollege/Associates",
    "EmploymentBachelors+", "Unemployment16to19",	"Unemployment20to24",	"Unemployment25to29",
    "Unemployment30to34",	"Unemployment35to44",	"Unemployment45to54",	"Unemployment55to59",
    "Unemployment60to64",	"Unemployment65to74",	"Unemployment75andover", "Unemployment20to64",
    "Unemployment20to64Male",	"Unemployment20to64Female",	"UnemploymentBelowPovertyLevel",
    "UnemploymentAtAbovePovertyLevel", "Unemployment", "UnemploymentLessHighSchool",
    "UnemploymentHighSchool",	"UnemploymentSomeCollege/Associates",	"UnemploymentBachelors+"
  )
  
names(Metro16) <- 
  c( 
    "Year", "City", "State", "Team", "PositionInLeague", "InSuperBowl", "WonSuperBowl",
    "TotalPop", "TotalMale", "PctTotalMale", "TotalFemale",	"PctTotalFemale",	
    "Under5",	"PctUnder5", "5to9", "Pct5to9",	"10to14",	"Pct10to14", "15to19",
    "Pct15to19", "20to24", "Pct20to24",	"25to34",	"Pct25to34", "35to44", "Pct35to44",
    "45to54",	"Pct45to54", "55to59", "Pct55to59",	"60to64",	"Pct60to64", "65to74", "Pct65to74",
    "75to84",	"Pct75to84", "85andover",	"Pct85andover",	"18andover", "Pct18andover", "18andoverMale",
    "Pct18andoverMale",	"18andoverFemale", "Pct18andoverFemale", "65andover", "Pct65andover",	
    "65andoverMale",	"Pct65andoverMale",	"65andoverFemale", "Pct65andoverFemale", "Onerace",
    "PctOnerace", "White", "PctWhite", "BlackorAfricanAmerican", "PctBlackorAfricanAmerican",
    "AmericanIndianandAlaskaNative", "PctAmericanIndianandAlaskaNative", "Asian", "PctAsian",
    "NativeHawaiianandOtherPacificIslander", "PctNativeHawaiianandOtherPacificIslander",
    "HispanicLatino",	"PctHispanicLatino", "Someotherrace",	"PctSomeotherrace",	"Twoormoreraces",
    "PctTwoormoreraces", "BelowPovertyLevel",	"AtAbovePovertyLevel", "LessHighSchool",
    "HighSchoolGraduate",	"SomeCollege/Associates", "Bachelors+",	"Employment16to19",
    "Employment20to24",	"Employment25to29",	"Employment30to34",	"Employment35to44",
    "Employment45to54", "Employment55to59",	"Employment60to64",	"Employment65to74",
    "Employment75andover", "Employment20to64", "Employment20to64Male", "Employment20to64Female",
    "EmploymentBelowpovertylevel", "EmploymentAtAbovePovertyLevel", "Employment25to64",
    "EmploymentLessHighSchool", "EmploymentHighSchool", "EmploymentSomeCollege/Associates",
    "EmploymentBachelors+", "Unemployment16to19",	"Unemployment20to24",	"Unemployment25to29",
    "Unemployment30to34",	"Unemployment35to44",	"Unemployment45to54",	"Unemployment55to59",
    "Unemployment60to64",	"Unemployment65to74",	"Unemployment75andover", "Unemployment20to64",
    "Unemployment20to64Male",	"Unemployment20to64Female",	"UnemploymentBelowPovertyLevel",
    "UnemploymentAtAbovePovertyLevel", "Unemployment", "UnemploymentLessHighSchool",
    "UnemploymentHighSchool",	"UnemploymentSomeCollege/Associates",	"UnemploymentBachelors+"
  )

names(Metro17) <- 
  c( 
    "Year", "City", "State", "Team", "PositionInLeague", "InSuperBowl", "WonSuperBowl",
    "TotalPop", "TotalMale", "PctTotalMale", "TotalFemale",	"PctTotalFemale",	
    "Under5",	"PctUnder5", "5to9", "Pct5to9",	"10to14",	"Pct10to14", "15to19",
    "Pct15to19", "20to24", "Pct20to24",	"25to34",	"Pct25to34", "35to44", "Pct35to44",
    "45to54",	"Pct45to54", "55to59", "Pct55to59",	"60to64",	"Pct60to64", "65to74", "Pct65to74",
    "75to84",	"Pct75to84", "85andover",	"Pct85andover",	"18andover", "Pct18andover", "18andoverMale",
    "Pct18andoverMale",	"18andoverFemale", "Pct18andoverFemale", "65andover", "Pct65andover",	
    "65andoverMale",	"Pct65andoverMale",	"65andoverFemale", "Pct65andoverFemale", "Onerace",
    "PctOnerace", "White", "PctWhite", "BlackorAfricanAmerican", "PctBlackorAfricanAmerican",
    "AmericanIndianandAlaskaNative", "PctAmericanIndianandAlaskaNative", "Asian", "PctAsian",
    "NativeHawaiianandOtherPacificIslander", "PctNativeHawaiianandOtherPacificIslander",
    "HispanicLatino",	"PctHispanicLatino", "Someotherrace",	"PctSomeotherrace",	"Twoormoreraces",
    "PctTwoormoreraces", "BelowPovertyLevel",	"AtAbovePovertyLevel", "LessHighSchool",
    "HighSchoolGraduate",	"SomeCollege/Associates", "Bachelors+",	"Employment16to19",
    "Employment20to24",	"Employment25to29",	"Employment30to34",	"Employment35to44",
    "Employment45to54", "Employment55to59",	"Employment60to64",	"Employment65to74",
    "Employment75andover", "Employment20to64", "Employment20to64Male", "Employment20to64Female",
    "EmploymentBelowpovertylevel", "EmploymentAtAbovePovertyLevel", "Employment25to64",
    "EmploymentLessHighSchool", "EmploymentHighSchool", "EmploymentSomeCollege/Associates",
    "EmploymentBachelors+", "Unemployment16to19",	"Unemployment20to24",	"Unemployment25to29",
    "Unemployment30to34",	"Unemployment35to44",	"Unemployment45to54",	"Unemployment55to59",
    "Unemployment60to64",	"Unemployment65to74",	"Unemployment75andover", "Unemployment20to64",
    "Unemployment20to64Male",	"Unemployment20to64Female",	"UnemploymentBelowPovertyLevel",
    "UnemploymentAtAbovePovertyLevel", "Unemployment", "UnemploymentLessHighSchool",
    "UnemploymentHighSchool",	"UnemploymentSomeCollege/Associates",	"UnemploymentBachelors+"
  )

names(Metro18) <- 
  c( 
    "Year", "City", "State", "Team", "PositionInLeague", "InSuperBowl", "WonSuperBowl",
    "TotalPop", "TotalMale", "PctTotalMale", "TotalFemale",	"PctTotalFemale",	
    "Under5",	"PctUnder5", "5to9", "Pct5to9",	"10to14",	"Pct10to14", "15to19",
    "Pct15to19", "20to24", "Pct20to24",	"25to34",	"Pct25to34", "35to44", "Pct35to44",
    "45to54",	"Pct45to54", "55to59", "Pct55to59",	"60to64",	"Pct60to64", "65to74", "Pct65to74",
    "75to84",	"Pct75to84", "85andover",	"Pct85andover",	"18andover", "Pct18andover", "18andoverMale",
    "Pct18andoverMale",	"18andoverFemale", "Pct18andoverFemale", "65andover", "Pct65andover",	
    "65andoverMale",	"Pct65andoverMale",	"65andoverFemale", "Pct65andoverFemale", "Onerace",
    "PctOnerace", "White", "PctWhite", "BlackorAfricanAmerican", "PctBlackorAfricanAmerican",
    "AmericanIndianandAlaskaNative", "PctAmericanIndianandAlaskaNative", "Asian", "PctAsian",
    "NativeHawaiianandOtherPacificIslander", "PctNativeHawaiianandOtherPacificIslander",
    "HispanicLatino",	"PctHispanicLatino", "Someotherrace",	"PctSomeotherrace",	"Twoormoreraces",
    "PctTwoormoreraces", "BelowPovertyLevel",	"AtAbovePovertyLevel", "LessHighSchool",
    "HighSchoolGraduate",	"SomeCollege/Associates", "Bachelors+",	"Employment16to19",
    "Employment20to24",	"Employment25to29",	"Employment30to34",	"Employment35to44",
    "Employment45to54", "Employment55to59",	"Employment60to64",	"Employment65to74",
    "Employment75andover", "Employment20to64", "Employment20to64Male", "Employment20to64Female",
    "EmploymentBelowpovertylevel", "EmploymentAtAbovePovertyLevel", "Employment25to64",
    "EmploymentLessHighSchool", "EmploymentHighSchool", "EmploymentSomeCollege/Associates",
    "EmploymentBachelors+", "Unemployment16to19",	"Unemployment20to24",	"Unemployment25to29",
    "Unemployment30to34",	"Unemployment35to44",	"Unemployment45to54",	"Unemployment55to59",
    "Unemployment60to64",	"Unemployment65to74",	"Unemployment75andover", "Unemployment20to64",
    "Unemployment20to64Male",	"Unemployment20to64Female",	"UnemploymentBelowPovertyLevel",
    "UnemploymentAtAbovePovertyLevel", "Unemployment", "UnemploymentLessHighSchool",
    "UnemploymentHighSchool",	"UnemploymentSomeCollege/Associates",	"UnemploymentBachelors+"
  )

names(State15) <- 
  c( 
    "Year", "City", "State", "Team", "PositionInLeague", "InSuperBowl", "WonSuperBowl",
    "TotalPop", "TotalMale", "PctTotalMale", "TotalFemale",	"PctTotalFemale",	
    "Under5",	"PctUnder5", "5to9", "Pct5to9",	"10to14",	"Pct10to14", "15to19",
    "Pct15to19", "20to24", "Pct20to24",	"25to34",	"Pct25to34", "35to44", "Pct35to44",
    "45to54",	"Pct45to54", "55to59", "Pct55to59",	"60to64",	"Pct60to64", "65to74", "Pct65to74",
    "75to84",	"Pct75to84", "85andover",	"Pct85andover",	"18andover", "Pct18andover", "18andoverMale",
    "Pct18andoverMale",	"18andoverFemale", "Pct18andoverFemale", "65andover", "Pct65andover",	
    "65andoverMale",	"Pct65andoverMale",	"65andoverFemale", "Pct65andoverFemale", "Onerace",
    "PctOnerace", "White", "PctWhite", "BlackorAfricanAmerican", "PctBlackorAfricanAmerican",
    "AmericanIndianandAlaskaNative", "PctAmericanIndianandAlaskaNative", "Asian", "PctAsian",
    "NativeHawaiianandOtherPacificIslander", "PctNativeHawaiianandOtherPacificIslander",
    "HispanicLatino",	"PctHispanicLatino", "Someotherrace",	"PctSomeotherrace",	"Twoormoreraces",
    "PctTwoormoreraces", "BelowPovertyLevel",	"AtAbovePovertyLevel", "LessHighSchool",
    "HighSchoolGraduate",	"SomeCollege/Associates", "Bachelors+",	"Employment16to19",
    "Employment20to24",	"Employment25to29",	"Employment30to34",	"Employment35to44",
    "Employment45to54", "Employment55to59",	"Employment60to64",	"Employment65to74",
    "Employment75andover", "Employment20to64", "Employment20to64Male", "Employment20to64Female",
    "EmploymentBelowpovertylevel", "EmploymentAtAbovePovertyLevel", "Employment25to64",
    "EmploymentLessHighSchool", "EmploymentHighSchool", "EmploymentSomeCollege/Associates",
    "EmploymentBachelors+", "Unemployment16to19",	"Unemployment20to24",	"Unemployment25to29",
    "Unemployment30to34",	"Unemployment35to44",	"Unemployment45to54",	"Unemployment55to59",
    "Unemployment60to64",	"Unemployment65to74",	"Unemployment75andover", "Unemployment20to64",
    "Unemployment20to64Male",	"Unemployment20to64Female",	"UnemploymentBelowPovertyLevel",
    "UnemploymentAtAbovePovertyLevel", "Unemployment", "UnemploymentLessHighSchool",
    "UnemploymentHighSchool",	"UnemploymentSomeCollege/Associates",	"UnemploymentBachelors+"
  )

names(State16) <- 
  c( 
    "Year", "City", "State", "Team", "PositionInLeague", "InSuperBowl", "WonSuperBowl",
    "TotalPop", "TotalMale", "PctTotalMale", "TotalFemale",	"PctTotalFemale",	
    "Under5",	"PctUnder5", "5to9", "Pct5to9",	"10to14",	"Pct10to14", "15to19",
    "Pct15to19", "20to24", "Pct20to24",	"25to34",	"Pct25to34", "35to44", "Pct35to44",
    "45to54",	"Pct45to54", "55to59", "Pct55to59",	"60to64",	"Pct60to64", "65to74", "Pct65to74",
    "75to84",	"Pct75to84", "85andover",	"Pct85andover",	"18andover", "Pct18andover", "18andoverMale",
    "Pct18andoverMale",	"18andoverFemale", "Pct18andoverFemale", "65andover", "Pct65andover",	
    "65andoverMale",	"Pct65andoverMale",	"65andoverFemale", "Pct65andoverFemale", "Onerace",
    "PctOnerace", "White", "PctWhite", "BlackorAfricanAmerican", "PctBlackorAfricanAmerican",
    "AmericanIndianandAlaskaNative", "PctAmericanIndianandAlaskaNative", "Asian", "PctAsian",
    "NativeHawaiianandOtherPacificIslander", "PctNativeHawaiianandOtherPacificIslander",
    "HispanicLatino",	"PctHispanicLatino", "Someotherrace",	"PctSomeotherrace",	"Twoormoreraces",
    "PctTwoormoreraces", "BelowPovertyLevel",	"AtAbovePovertyLevel", "LessHighSchool",
    "HighSchoolGraduate",	"SomeCollege/Associates", "Bachelors+",	"Employment16to19",
    "Employment20to24",	"Employment25to29",	"Employment30to34",	"Employment35to44",
    "Employment45to54", "Employment55to59",	"Employment60to64",	"Employment65to74",
    "Employment75andover", "Employment20to64", "Employment20to64Male", "Employment20to64Female",
    "EmploymentBelowpovertylevel", "EmploymentAtAbovePovertyLevel", "Employment25to64",
    "EmploymentLessHighSchool", "EmploymentHighSchool", "EmploymentSomeCollege/Associates",
    "EmploymentBachelors+", "Unemployment16to19",	"Unemployment20to24",	"Unemployment25to29",
    "Unemployment30to34",	"Unemployment35to44",	"Unemployment45to54",	"Unemployment55to59",
    "Unemployment60to64",	"Unemployment65to74",	"Unemployment75andover", "Unemployment20to64",
    "Unemployment20to64Male",	"Unemployment20to64Female",	"UnemploymentBelowPovertyLevel",
    "UnemploymentAtAbovePovertyLevel", "Unemployment", "UnemploymentLessHighSchool",
    "UnemploymentHighSchool",	"UnemploymentSomeCollege/Associates",	"UnemploymentBachelors+"
  )

names(State17) <- 
  c( 
    "Year", "City", "State", "Team", "PositionInLeague", "InSuperBowl", "WonSuperBowl",
    "TotalPop", "TotalMale", "PctTotalMale", "TotalFemale",	"PctTotalFemale",	
    "Under5",	"PctUnder5", "5to9", "Pct5to9",	"10to14",	"Pct10to14", "15to19",
    "Pct15to19", "20to24", "Pct20to24",	"25to34",	"Pct25to34", "35to44", "Pct35to44",
    "45to54",	"Pct45to54", "55to59", "Pct55to59",	"60to64",	"Pct60to64", "65to74", "Pct65to74",
    "75to84",	"Pct75to84", "85andover",	"Pct85andover",	"18andover", "Pct18andover", "18andoverMale",
    "Pct18andoverMale",	"18andoverFemale", "Pct18andoverFemale", "65andover", "Pct65andover",	
    "65andoverMale",	"Pct65andoverMale",	"65andoverFemale", "Pct65andoverFemale", "Onerace",
    "PctOnerace", "White", "PctWhite", "BlackorAfricanAmerican", "PctBlackorAfricanAmerican",
    "AmericanIndianandAlaskaNative", "PctAmericanIndianandAlaskaNative", "Asian", "PctAsian",
    "NativeHawaiianandOtherPacificIslander", "PctNativeHawaiianandOtherPacificIslander",
    "HispanicLatino",	"PctHispanicLatino", "Someotherrace",	"PctSomeotherrace",	"Twoormoreraces",
    "PctTwoormoreraces", "BelowPovertyLevel",	"AtAbovePovertyLevel", "LessHighSchool",
    "HighSchoolGraduate",	"SomeCollege/Associates", "Bachelors+",	"Employment16to19",
    "Employment20to24",	"Employment25to29",	"Employment30to34",	"Employment35to44",
    "Employment45to54", "Employment55to59",	"Employment60to64",	"Employment65to74",
    "Employment75andover", "Employment20to64", "Employment20to64Male", "Employment20to64Female",
    "EmploymentBelowpovertylevel", "EmploymentAtAbovePovertyLevel", "Employment25to64",
    "EmploymentLessHighSchool", "EmploymentHighSchool", "EmploymentSomeCollege/Associates",
    "EmploymentBachelors+", "Unemployment16to19",	"Unemployment20to24",	"Unemployment25to29",
    "Unemployment30to34",	"Unemployment35to44",	"Unemployment45to54",	"Unemployment55to59",
    "Unemployment60to64",	"Unemployment65to74",	"Unemployment75andover", "Unemployment20to64",
    "Unemployment20to64Male",	"Unemployment20to64Female",	"UnemploymentBelowPovertyLevel",
    "UnemploymentAtAbovePovertyLevel", "Unemployment", "UnemploymentLessHighSchool",
    "UnemploymentHighSchool",	"UnemploymentSomeCollege/Associates",	"UnemploymentBachelors+"
  )

names(State18) <- 
  c( 
    "Year", "City", "State", "Team", "PositionInLeague", "InSuperBowl", "WonSuperBowl",
    "TotalPop", "TotalMale", "PctTotalMale", "TotalFemale",	"PctTotalFemale",	
    "Under5",	"PctUnder5", "5to9", "Pct5to9",	"10to14",	"Pct10to14", "15to19",
    "Pct15to19", "20to24", "Pct20to24",	"25to34",	"Pct25to34", "35to44", "Pct35to44",
    "45to54",	"Pct45to54", "55to59", "Pct55to59",	"60to64",	"Pct60to64", "65to74", "Pct65to74",
    "75to84",	"Pct75to84", "85andover",	"Pct85andover",	"18andover", "Pct18andover", "18andoverMale",
    "Pct18andoverMale",	"18andoverFemale", "Pct18andoverFemale", "65andover", "Pct65andover",	
    "65andoverMale",	"Pct65andoverMale",	"65andoverFemale", "Pct65andoverFemale", "Onerace",
    "PctOnerace", "White", "PctWhite", "BlackorAfricanAmerican", "PctBlackorAfricanAmerican",
    "AmericanIndianandAlaskaNative", "PctAmericanIndianandAlaskaNative", "Asian", "PctAsian",
    "NativeHawaiianandOtherPacificIslander", "PctNativeHawaiianandOtherPacificIslander",
    "HispanicLatino",	"PctHispanicLatino", "Someotherrace",	"PctSomeotherrace",	"Twoormoreraces",
    "PctTwoormoreraces", "BelowPovertyLevel",	"AtAbovePovertyLevel", "LessHighSchool",
    "HighSchoolGraduate",	"SomeCollege/Associates", "Bachelors+",	"Employment16to19",
    "Employment20to24",	"Employment25to29",	"Employment30to34",	"Employment35to44",
    "Employment45to54", "Employment55to59",	"Employment60to64",	"Employment65to74",
    "Employment75andover", "Employment20to64", "Employment20to64Male", "Employment20to64Female",
    "EmploymentBelowpovertylevel", "EmploymentAtAbovePovertyLevel", "Employment25to64",
    "EmploymentLessHighSchool", "EmploymentHighSchool", "EmploymentSomeCollege/Associates",
    "EmploymentBachelors+", "Unemployment16to19",	"Unemployment20to24",	"Unemployment25to29",
    "Unemployment30to34",	"Unemployment35to44",	"Unemployment45to54",	"Unemployment55to59",
    "Unemployment60to64",	"Unemployment65to74",	"Unemployment75andover", "Unemployment20to64",
    "Unemployment20to64Male",	"Unemployment20to64Female",	"UnemploymentBelowPovertyLevel",
    "UnemploymentAtAbovePovertyLevel", "Unemployment", "UnemploymentLessHighSchool",
    "UnemploymentHighSchool",	"UnemploymentSomeCollege/Associates",	"UnemploymentBachelors+"
  )

# Initial visualizations
UnempPlot15 <- ggplot(
  data = Metro15,
  mapping = aes(
    x=Team,
    y=Unemployment,
    fill=Unemployment
  )
) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(legend.position = "none") +
  geom_text(
    mapping = aes(
      label = PositionInLeague
    ),
    position = position_dodge(1),
    vjust = -0.5,
    size = 4
  ) +
  ggtitle("2015 Unemployment Rate vs. Team with Team Position in League") +
  xlab("NFL Team") + 
  ylab("Unemployment Rate of Team Metropolitan Area") + 
  scale_fill_gradient(
    low = "red",
    high = "purple"
  )

UnempPlot18 <- ggplot(
  data = Metro18,
  mapping = aes(
    x=Team,
    y=Unemployment,
    fill=Unemployment
  )
) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(legend.position = "none") +
  geom_text(
    mapping = aes(
      label = PositionInLeague
    ),
    position = position_dodge(1),
    vjust = -0.5,
    size = 4
  ) +
  ggtitle("2018 Unemployment Rate vs. Team with Team Position in League") +
  xlab("NFL Team") + 
  ylab("Unemployment Rate of Team Metropolitan Area") + 
  scale_fill_gradient(
    low = "red",
    high = "purple"
  )

PctWhitePlot15 <- ggplot(
  data = Metro15,
  mapping = aes(
    x = Team,
    y = PctWhite,
    fill=PctWhite
  )
) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  geom_text(
    aes(
      label = PositionInLeague
    ),
    position = position_dodge(1),
    vjust = -0.5,
    size = 4
  ) +
  ggtitle("2015 Percent White Composition of Home Metropolitan Area vs. Team with Team Position in League") +
  xlab("NFL Team") + 
  ylab("White Racial Composition of Team Metropolitan Area") + 
  scale_fill_gradient(
    low = "seagreen",
    high = "seagreen3"
  )

PctWhitePlot18 <- ggplot(
  data = Metro18,
  mapping = aes(
    x = Team,
    y = PctWhite,
    fill = PctWhite
  )
) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(legend.position = "none") +
  geom_text(
    aes(
      label = PositionInLeague
    ),
    position = position_dodge(1),
    vjust = -0.5,
    size = 4
  ) +
  ggtitle("2018 Percent White Composition of Home Metropolitan Area vs. Team with Team Position in League") +
  xlab("NFL Team") + 
  ylab("White Racial Composition of Team Metropolitan Area") + 
  scale_fill_gradient(
    low = "seagreen",
    high = "seagreen3"
  )

# grid.arrange(
#   UnempPlot15,
#   PctWhitePlot15,
#   UnempPlot18,
#   PctWhitePlot18,
#   ncol = 1,
#   nrow = 4,
#   top = "Title of this grid of graphs"
# )

multiPage <- ggarrange(
  UnempPlot15, 
  PctWhitePlot15, 
  UnempPlot18, 
  PctWhitePlot18,
  ncol = 1,
  nrow = 2
)

multiPage[[1]]
multiPage[[2]]

# The above grid of graphs shows some possible trends appear that high ranking 
# teams have some differing characteristics in the metropolitan area they call 
# home as compared to teams that rank worse in the NFL

# Train on 2015 and 2016 data, test on 2018 data
allMetroTrain <- Metro15
allMetroTest <- Metro18

# https://stackoverflow.com/questions/4605206/drop-data-frame-columns-by-name
drops <- c("City", "State", "Team","InSuperBowl", "WonSuperBowl")
allMetroTrain <- allMetroTrain[ , !(names(allMetroTrain) %in% drops)]
allMetroTest <- allMetroTest[ , !(names(allMetroTest) %in% drops)]

# elastic net penalized regression on all training years of metropolitan area data
elasticFit <- train(
  data = allMetroTrain,
  PositionInLeague ~ .,
  method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 20
)
# elasticFit is pushing all coefficients to zero, so attempting a forward selection method

# Forward selection model
fullMetroModel <- lm(PositionInLeague ~ ., data = allMetroTrain)
ForwardSelectStart <- lm(PositionInLeague ~ 1, data = allMetroTrain)

step(ForwardSelectStart, direction="forward", scope=formula(fullMetroModel), steps=20)
# Resulting Model: 
# PositionInLeague ~ `EmploymentSomeCollege/Associates` + PctWhite + 
#                     Unemployment65to74 + Unemployment55to59

# From this, we see that some racial classifiers are meaningful and some 
# employment/unemployment classifiers are meaningful, while none of the age or
# total population statitistics are very meaningful. This also show us that the 
# percentage statistics aren't very useful in modeling, so next step is to remove
# all percentage predictors

allMetroTrain <- allMetroTrain %>% select(-starts_with("Pct"))
allMetroTest <- allMetroTest %>% select(-starts_with("Pct"))

# Now we will separate the racial composition classifiers from the employment
# and unemployment statistics, while removing the age and total population data

allMetroTrainRace <- allMetroTrain[ , c(2, c(25:33))]
allMetroTrainEmp <- allMetroTrain[ , c(2, c(40:79))]

# Now run an elastic net regression on these data sets
raceElasticFit <- train(
  data = allMetroTrainRace,
  PositionInLeague ~ .,
  method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 20
)

coef(raceElasticFit$finalModel, raceElasticFit$bestTune$lambda)

empElasticFit <- train(
  data = allMetroTrainEmp,
  PositionInLeague ~ .,
  method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 20
)

coef(empElasticFit$finalModel, empElasticFit$bestTune$lambda)

# Both forced all coefficients to zero, assuming this data has no correlation with
# the outcome variable

# Last attempt, forward selection on both race and employment data sets
fullRaceMetroModel <- lm(PositionInLeague ~ ., data = allMetroTrainRace)
raceForwardSelectStart <- lm(PositionInLeague ~ 1, data = allMetroTrainRace)
step(raceForwardSelectStart, direction="forward", scope=formula(fullRaceMetroModel), steps=5)

fullEmpMetroModel <- lm(PositionInLeague ~ ., data = allMetroTrainEmp)
empForwardSelectStart <- lm(PositionInLeague ~ 1, data = allMetroTrainEmp)
step(empForwardSelectStart, direction="forward", scope=formula(fullEmpMetroModel), steps=10)

# One minorly meaningful variable in both forward selections indicates that 
# metropolitan area demographics/statistics have none to minimal ability to 
# predict how a NFL team will perform, so final conclusion is that there isn't
# an attribute of the community the team could look to change through community 
# outreach in order to predict better success


# Combine all metro area statistics into one data set
allMetro <- rbind(Metro15, Metro16, Metro17, Metro18)

rankTotalPopDotPlot <- ggplot(
  data=allMetro,
  mapping = aes(
    x=TotalPop,
    y=PositionInLeague,
    size=Year,
    color=Team
  )
) +
  geom_point() +
  ggtitle("Metropolitan Area Total Population vs. Home Team Final Position in NFL") +
  xlab("Total Metropolitan Area Population") + 
  ylab("Position in NFL at End of Season") + 
  scale_color_manual(
    values = rainbow(32)
  )

rankTotalPopDotPlot

# The total scatter of the final graph is a representation of how any individual 
# characteristic of the metropolitan area has little impact on how that team will 
# perform


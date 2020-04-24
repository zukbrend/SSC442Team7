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

# Initial Preprocessign to remove random characters added during csv conversion
names(Metro15) <- 
  c(
    "City",	"State",	"Team",	"Year",	"PositionInLeague",	"InSuperBowl",	"WonSuperBowl",
    "MetroArea",	"TotalPop",	"TotalMale",	"PctTotalMale",	"TotalFemale",	
    "PctTotalFemale",	"Under5",	"PctUnder5",	"5to9",	"Pct5to9",	"10to14",	"Pct10to14",
    "15to19",	"Pct15to19",	"20to24",	"Pct20to24",	"25to34",	"Pct25to34",	"35to44",	"Pct35to44",
    "45to54",	"Pct45to54",	"55to59",	"Pct55to59",	"60to64",	"Pct60to64",	"65to74",	"Pct65to74",
    "75to84",	"Pct75to84",	"85andover",	"Pct85andover",	"18andover",	"Pct18andover",	"18andoverMale",
    "Pct18andoverMale",	"18andoverFemale",	"Pct18andoverFemale",	"65andover",	"Pct65andover",	
    "65andoverMale",	"Pct65andoverMale",	"65andoverFemale",	"Pct65andoverFemale",	"Onerace",
    "PctOnerace",	"White",	"PctWhite",	"BlackorAfricanAmerican",	"PctBlackorAfricanAmerican",
    "AmericanIndianandAlaskaNative",	"PctAmericanIndianandAlaskaNative",	"Asian",	"PctAsian",
    "NativeHawaiianandOtherPacificIslander",	"PctNativeHawaiianandOtherPacificIslander",
    "HispanicLatino",	"PctHispanicLatino",	"Someotherrace",	"PctSomeotherrace",	"Twoormoreraces",
    "PctTwoormoreraces",	"BelowPovertyLevel",	"AtAbovePovertyLevel",	"LessHighSchool",
    "HighSchoolGraduate",	"SomeCollege/Associates",	"Bachelors+",	"Employment16to19",
    "Employment20to24",	"Employment25to29",	"Employment30to34",	"Employment35to44",
    "Employment45to54",	"Employment55to59",	"Employment60to64",	"Employment65to74",
    "Employment75andover",	"Employment20to64",	"Employment20to64Male",	"Employment20to64Female",
    "EmploymentBelowpovertylevel",	"EmploymentAtAbovePovertyLevel",	"Employment25to64",
    "EmploymentLessHighSchool",	"EmploymentHighSchool",	"EmploymentSomeCollege/Associates",
    "EmploymentBachelors+",	"Unemployment16to19",	"Unemployment20to24",	"Unemployment25to29",
    "Unemployment30to34",	"Unemployment35to44",	"Unemployment45to54",	"Unemployment55to59",
    "Unemployment60to64",	"Unemployment65to74",	"Unemployment75andover",	"Unemployment20to64",
    "Unemployment20to64Male",	"Unemployment20to64Female",	"UnemploymentBelowPovertyLevel",
    "UnemploymentAtAbovePovertyLevel",	"Unemployment",	"UnemploymentLessHighSchool",
    "UnemploymentHighSchool",	"UnemploymentSomeCollege/Associates",	"UnemploymentBachelors+"
    
  )
  
names(Metro16) <- 
  c(
    "MetroArea",	"TotalPop",	"TotalMale",	"%TotalMale",	"TotalFemale",	
    "%TotalFemale",	"Under5",	"%Under5",	"5to9",	"%5to9",	"10to14",	"%10to14",
    "15to19",	"%15to19",	"20to24",	"%20to24",	"25to34",	"%25to34",	"35to44",	"%35to44",
    "45to54",	"%45to54",	"55to59",	"%55to59",	"60to64",	"%60to64",	"65to74",	"%65to74",
    "75to84",	"%75to84",	"85andover",	"%85andover",	"18andover",	"%18andover",	"18andoverMale",
    "%18andoverMale",	"18andoverFemale",	"%18andoverFemale",	"65andover",	"%65andover",	
    "65andoverMale",	"%65andoverMale",	"65andoverFemale",	"%65andoverFemale",	"Onerace",
    "%Onerace",	"White",	"%White",	"BlackorAfricanAmerican",	"%BlackorAfricanAmerican",
    "AmericanIndianandAlaskaNative",	"%AmericanIndianandAlaskaNative",	"Asian",	"%Asian",
    "NativeHawaiianandOtherPacificIslander",	"%NativeHawaiianandOtherPacificIslander",
    "HispanicLatino",	"%HispanicLatino",	"Someotherrace",	"%Someotherrace",	"Twoormoreraces",
    "%Twoormoreraces",	"BelowPovertyLevel",	"AtAbovePovertyLevel",	"LessHighSchool",
    "HighSchoolGraduate",	"SomeCollege/Associates",	"Bachelors+",	"Employment16to19",
    "Employment20to24",	"Employment25to29",	"Employment30to34",	"Employment35to44",
    "Employment45to54",	"Employment55to59",	"Employment60to64",	"Employment65to74",
    "Employment75andover",	"Employment20to64",	"Employment20to64Male",	"Employment20to64Female",
    "EmploymentBelowpovertylevel",	"EmploymentAtAbovePovertyLevel",	"Employment25to64",
    "EmploymentLessHighSchool",	"EmploymentHighSchool",	"EmploymentSomeCollege/Associates",
    "EmploymentBachelors+",	"Unemployment16to19",	"Unemployment20to24",	"Unemployment25to29",
    "Unemployment30to34",	"Unemployment35to44",	"Unemployment45to54",	"Unemployment55to59",
    "Unemployment60to64",	"Unemployment65to74",	"Unemployment75andover",	"Unemployment20to64",
    "Unemployment20to64Male",	"Unemployment20to64Female",	"UnemploymentBelowPovertyLevel",
    "UnemploymentAtAbovePovertyLevel",	"Unemployment",	"UnemploymentLessHighSchool",
    "UnemploymentHighSchool",	"UnemploymentSomeCollege/Associates",	"UnemploymentBachelors+"
  )

names(Metro17) <- 
  c(
    "MetroArea",	"TotalPop",	"TotalMale",	"%TotalMale",	"TotalFemale",	
    "%TotalFemale",	"Under5",	"%Under5",	"5to9",	"%5to9",	"10to14",	"%10to14",
    "15to19",	"%15to19",	"20to24",	"%20to24",	"25to34",	"%25to34",	"35to44",	"%35to44",
    "45to54",	"%45to54",	"55to59",	"%55to59",	"60to64",	"%60to64",	"65to74",	"%65to74",
    "75to84",	"%75to84",	"85andover",	"%85andover",	"18andover",	"%18andover",	"18andoverMale",
    "%18andoverMale",	"18andoverFemale",	"%18andoverFemale",	"65andover",	"%65andover",	
    "65andoverMale",	"%65andoverMale",	"65andoverFemale",	"%65andoverFemale",	"Onerace",
    "%Onerace",	"White",	"%White",	"BlackorAfricanAmerican",	"%BlackorAfricanAmerican",
    "AmericanIndianandAlaskaNative",	"%AmericanIndianandAlaskaNative",	"Asian",	"%Asian",
    "NativeHawaiianandOtherPacificIslander",	"%NativeHawaiianandOtherPacificIslander",
    "HispanicLatino",	"%HispanicLatino",	"Someotherrace",	"%Someotherrace",	"Twoormoreraces",
    "%Twoormoreraces",	"BelowPovertyLevel",	"AtAbovePovertyLevel",	"LessHighSchool",
    "HighSchoolGraduate",	"SomeCollege/Associates",	"Bachelors+",	"Employment16to19",
    "Employment20to24",	"Employment25to29",	"Employment30to34",	"Employment35to44",
    "Employment45to54",	"Employment55to59",	"Employment60to64",	"Employment65to74",
    "Employment75andover",	"Employment20to64",	"Employment20to64Male",	"Employment20to64Female",
    "EmploymentBelowpovertylevel",	"EmploymentAtAbovePovertyLevel",	"Employment25to64",
    "EmploymentLessHighSchool",	"EmploymentHighSchool",	"EmploymentSomeCollege/Associates",
    "EmploymentBachelors+",	"Unemployment16to19",	"Unemployment20to24",	"Unemployment25to29",
    "Unemployment30to34",	"Unemployment35to44",	"Unemployment45to54",	"Unemployment55to59",
    "Unemployment60to64",	"Unemployment65to74",	"Unemployment75andover",	"Unemployment20to64",
    "Unemployment20to64Male",	"Unemployment20to64Female",	"UnemploymentBelowPovertyLevel",
    "UnemploymentAtAbovePovertyLevel",	"Unemployment",	"UnemploymentLessHighSchool",
    "UnemploymentHighSchool",	"UnemploymentSomeCollege/Associates",	"UnemploymentBachelors+"
  )

names(Metro18) <- 
  c(
    "MetroArea",	"TotalPop",	"TotalMale",	"%TotalMale",	"TotalFemale",	
    "%TotalFemale",	"Under5",	"%Under5",	"5to9",	"%5to9",	"10to14",	"%10to14",
    "15to19",	"%15to19",	"20to24",	"%20to24",	"25to34",	"%25to34",	"35to44",	"%35to44",
    "45to54",	"%45to54",	"55to59",	"%55to59",	"60to64",	"%60to64",	"65to74",	"%65to74",
    "75to84",	"%75to84",	"85andover",	"%85andover",	"18andover",	"%18andover",	"18andoverMale",
    "%18andoverMale",	"18andoverFemale",	"%18andoverFemale",	"65andover",	"%65andover",	
    "65andoverMale",	"%65andoverMale",	"65andoverFemale",	"%65andoverFemale",	"Onerace",
    "%Onerace",	"White",	"%White",	"BlackorAfricanAmerican",	"%BlackorAfricanAmerican",
    "AmericanIndianandAlaskaNative",	"%AmericanIndianandAlaskaNative",	"Asian",	"%Asian",
    "NativeHawaiianandOtherPacificIslander",	"%NativeHawaiianandOtherPacificIslander",
    "HispanicLatino",	"%HispanicLatino",	"Someotherrace",	"%Someotherrace",	"Twoormoreraces",
    "%Twoormoreraces",	"BelowPovertyLevel",	"AtAbovePovertyLevel",	"LessHighSchool",
    "HighSchoolGraduate",	"SomeCollege/Associates",	"Bachelors+",	"Employment16to19",
    "Employment20to24",	"Employment25to29",	"Employment30to34",	"Employment35to44",
    "Employment45to54",	"Employment55to59",	"Employment60to64",	"Employment65to74",
    "Employment75andover",	"Employment20to64",	"Employment20to64Male",	"Employment20to64Female",
    "EmploymentBelowpovertylevel",	"EmploymentAtAbovePovertyLevel",	"Employment25to64",
    "EmploymentLessHighSchool",	"EmploymentHighSchool",	"EmploymentSomeCollege/Associates",
    "EmploymentBachelors+",	"Unemployment16to19",	"Unemployment20to24",	"Unemployment25to29",
    "Unemployment30to34",	"Unemployment35to44",	"Unemployment45to54",	"Unemployment55to59",
    "Unemployment60to64",	"Unemployment65to74",	"Unemployment75andover",	"Unemployment20to64",
    "Unemployment20to64Male",	"Unemployment20to64Female",	"UnemploymentBelowPovertyLevel",
    "UnemploymentAtAbovePovertyLevel",	"Unemployment",	"UnemploymentLessHighSchool",
    "UnemploymentHighSchool",	"UnemploymentSomeCollege/Associates",	"UnemploymentBachelors+"
  )

names(State15) <- 
  c(
    "MetroArea",	"TotalPop",	"TotalMale",	"%TotalMale",	"TotalFemale",	
    "%TotalFemale",	"Under5",	"%Under5",	"5to9",	"%5to9",	"10to14",	"%10to14",
    "15to19",	"%15to19",	"20to24",	"%20to24",	"25to34",	"%25to34",	"35to44",	"%35to44",
    "45to54",	"%45to54",	"55to59",	"%55to59",	"60to64",	"%60to64",	"65to74",	"%65to74",
    "75to84",	"%75to84",	"85andover",	"%85andover",	"18andover",	"%18andover",	"18andoverMale",
    "%18andoverMale",	"18andoverFemale",	"%18andoverFemale",	"65andover",	"%65andover",	
    "65andoverMale",	"%65andoverMale",	"65andoverFemale",	"%65andoverFemale",	"Onerace",
    "%Onerace",	"White",	"%White",	"BlackorAfricanAmerican",	"%BlackorAfricanAmerican",
    "AmericanIndianandAlaskaNative",	"%AmericanIndianandAlaskaNative",	"Asian",	"%Asian",
    "NativeHawaiianandOtherPacificIslander",	"%NativeHawaiianandOtherPacificIslander",
    "HispanicLatino",	"%HispanicLatino",	"Someotherrace",	"%Someotherrace",	"Twoormoreraces",
    "%Twoormoreraces",	"BelowPovertyLevel",	"AtAbovePovertyLevel",	"LessHighSchool",
    "HighSchoolGraduate",	"SomeCollege/Associates",	"Bachelors+",	"Employment16to19",
    "Employment20to24",	"Employment25to29",	"Employment30to34",	"Employment35to44",
    "Employment45to54",	"Employment55to59",	"Employment60to64",	"Employment65to74",
    "Employment75andover",	"Employment20to64",	"Employment20to64Male",	"Employment20to64Female",
    "EmploymentBelowpovertylevel",	"EmploymentAtAbovePovertyLevel",	"Employment25to64",
    "EmploymentLessHighSchool",	"EmploymentHighSchool",	"EmploymentSomeCollege/Associates",
    "EmploymentBachelors+",	"Unemployment16to19",	"Unemployment20to24",	"Unemployment25to29",
    "Unemployment30to34",	"Unemployment35to44",	"Unemployment45to54",	"Unemployment55to59",
    "Unemployment60to64",	"Unemployment65to74",	"Unemployment75andover",	"Unemployment20to64",
    "Unemployment20to64Male",	"Unemployment20to64Female",	"UnemploymentBelowPovertyLevel",
    "UnemploymentAtAbovePovertyLevel",	"Unemployment",	"UnemploymentLessHighSchool",
    "UnemploymentHighSchool",	"UnemploymentSomeCollege/Associates",	"UnemploymentBachelors+"
  )

names(State16) <- 
  c(
    "MetroArea",	"TotalPop",	"TotalMale",	"%TotalMale",	"TotalFemale",	
    "%TotalFemale",	"Under5",	"%Under5",	"5to9",	"%5to9",	"10to14",	"%10to14",
    "15to19",	"%15to19",	"20to24",	"%20to24",	"25to34",	"%25to34",	"35to44",	"%35to44",
    "45to54",	"%45to54",	"55to59",	"%55to59",	"60to64",	"%60to64",	"65to74",	"%65to74",
    "75to84",	"%75to84",	"85andover",	"%85andover",	"18andover",	"%18andover",	"18andoverMale",
    "%18andoverMale",	"18andoverFemale",	"%18andoverFemale",	"65andover",	"%65andover",	
    "65andoverMale",	"%65andoverMale",	"65andoverFemale",	"%65andoverFemale",	"Onerace",
    "%Onerace",	"White",	"%White",	"BlackorAfricanAmerican",	"%BlackorAfricanAmerican",
    "AmericanIndianandAlaskaNative",	"%AmericanIndianandAlaskaNative",	"Asian",	"%Asian",
    "NativeHawaiianandOtherPacificIslander",	"%NativeHawaiianandOtherPacificIslander",
    "HispanicLatino",	"%HispanicLatino",	"Someotherrace",	"%Someotherrace",	"Twoormoreraces",
    "%Twoormoreraces",	"BelowPovertyLevel",	"AtAbovePovertyLevel",	"LessHighSchool",
    "HighSchoolGraduate",	"SomeCollege/Associates",	"Bachelors+",	"Employment16to19",
    "Employment20to24",	"Employment25to29",	"Employment30to34",	"Employment35to44",
    "Employment45to54",	"Employment55to59",	"Employment60to64",	"Employment65to74",
    "Employment75andover",	"Employment20to64",	"Employment20to64Male",	"Employment20to64Female",
    "EmploymentBelowpovertylevel",	"EmploymentAtAbovePovertyLevel",	"Employment25to64",
    "EmploymentLessHighSchool",	"EmploymentHighSchool",	"EmploymentSomeCollege/Associates",
    "EmploymentBachelors+",	"Unemployment16to19",	"Unemployment20to24",	"Unemployment25to29",
    "Unemployment30to34",	"Unemployment35to44",	"Unemployment45to54",	"Unemployment55to59",
    "Unemployment60to64",	"Unemployment65to74",	"Unemployment75andover",	"Unemployment20to64",
    "Unemployment20to64Male",	"Unemployment20to64Female",	"UnemploymentBelowPovertyLevel",
    "UnemploymentAtAbovePovertyLevel",	"Unemployment",	"UnemploymentLessHighSchool",
    "UnemploymentHighSchool",	"UnemploymentSomeCollege/Associates",	"UnemploymentBachelors+"
  )

names(State17) <- 
  c(
    "MetroArea",	"TotalPop",	"TotalMale",	"%TotalMale",	"TotalFemale",	
    "%TotalFemale",	"Under5",	"%Under5",	"5to9",	"%5to9",	"10to14",	"%10to14",
    "15to19",	"%15to19",	"20to24",	"%20to24",	"25to34",	"%25to34",	"35to44",	"%35to44",
    "45to54",	"%45to54",	"55to59",	"%55to59",	"60to64",	"%60to64",	"65to74",	"%65to74",
    "75to84",	"%75to84",	"85andover",	"%85andover",	"18andover",	"%18andover",	"18andoverMale",
    "%18andoverMale",	"18andoverFemale",	"%18andoverFemale",	"65andover",	"%65andover",	
    "65andoverMale",	"%65andoverMale",	"65andoverFemale",	"%65andoverFemale",	"Onerace",
    "%Onerace",	"White",	"%White",	"BlackorAfricanAmerican",	"%BlackorAfricanAmerican",
    "AmericanIndianandAlaskaNative",	"%AmericanIndianandAlaskaNative",	"Asian",	"%Asian",
    "NativeHawaiianandOtherPacificIslander",	"%NativeHawaiianandOtherPacificIslander",
    "HispanicLatino",	"%HispanicLatino",	"Someotherrace",	"%Someotherrace",	"Twoormoreraces",
    "%Twoormoreraces",	"BelowPovertyLevel",	"AtAbovePovertyLevel",	"LessHighSchool",
    "HighSchoolGraduate",	"SomeCollege/Associates",	"Bachelors+",	"Employment16to19",
    "Employment20to24",	"Employment25to29",	"Employment30to34",	"Employment35to44",
    "Employment45to54",	"Employment55to59",	"Employment60to64",	"Employment65to74",
    "Employment75andover",	"Employment20to64",	"Employment20to64Male",	"Employment20to64Female",
    "EmploymentBelowpovertylevel",	"EmploymentAtAbovePovertyLevel",	"Employment25to64",
    "EmploymentLessHighSchool",	"EmploymentHighSchool",	"EmploymentSomeCollege/Associates",
    "EmploymentBachelors+",	"Unemployment16to19",	"Unemployment20to24",	"Unemployment25to29",
    "Unemployment30to34",	"Unemployment35to44",	"Unemployment45to54",	"Unemployment55to59",
    "Unemployment60to64",	"Unemployment65to74",	"Unemployment75andover",	"Unemployment20to64",
    "Unemployment20to64Male",	"Unemployment20to64Female",	"UnemploymentBelowPovertyLevel",
    "UnemploymentAtAbovePovertyLevel",	"Unemployment",	"UnemploymentLessHighSchool",
    "UnemploymentHighSchool",	"UnemploymentSomeCollege/Associates",	"UnemploymentBachelors+"
  )

names(State18) <- 
  c(
    "MetroArea",	"TotalPop",	"TotalMale",	"%TotalMale",	"TotalFemale",	
    "%TotalFemale",	"Under5",	"%Under5",	"5to9",	"%5to9",	"10to14",	"%10to14",
    "15to19",	"%15to19",	"20to24",	"%20to24",	"25to34",	"%25to34",	"35to44",	"%35to44",
    "45to54",	"%45to54",	"55to59",	"%55to59",	"60to64",	"%60to64",	"65to74",	"%65to74",
    "75to84",	"%75to84",	"85andover",	"%85andover",	"18andover",	"%18andover",	"18andoverMale",
    "%18andoverMale",	"18andoverFemale",	"%18andoverFemale",	"65andover",	"%65andover",	
    "65andoverMale",	"%65andoverMale",	"65andoverFemale",	"%65andoverFemale",	"Onerace",
    "%Onerace",	"White",	"%White",	"BlackorAfricanAmerican",	"%BlackorAfricanAmerican",
    "AmericanIndianandAlaskaNative",	"%AmericanIndianandAlaskaNative",	"Asian",	"%Asian",
    "NativeHawaiianandOtherPacificIslander",	"%NativeHawaiianandOtherPacificIslander",
    "HispanicLatino",	"%HispanicLatino",	"Someotherrace",	"%Someotherrace",	"Twoormoreraces",
    "%Twoormoreraces",	"BelowPovertyLevel",	"AtAbovePovertyLevel",	"LessHighSchool",
    "HighSchoolGraduate",	"SomeCollege/Associates",	"Bachelors+",	"Employment16to19",
    "Employment20to24",	"Employment25to29",	"Employment30to34",	"Employment35to44",
    "Employment45to54",	"Employment55to59",	"Employment60to64",	"Employment65to74",
    "Employment75andover",	"Employment20to64",	"Employment20to64Male",	"Employment20to64Female",
    "EmploymentBelowpovertylevel",	"EmploymentAtAbovePovertyLevel",	"Employment25to64",
    "EmploymentLessHighSchool",	"EmploymentHighSchool",	"EmploymentSomeCollege/Associates",
    "EmploymentBachelors+",	"Unemployment16to19",	"Unemployment20to24",	"Unemployment25to29",
    "Unemployment30to34",	"Unemployment35to44",	"Unemployment45to54",	"Unemployment55to59",
    "Unemployment60to64",	"Unemployment65to74",	"Unemployment75andover",	"Unemployment20to64",
    "Unemployment20to64Male",	"Unemployment20to64Female",	"UnemploymentBelowPovertyLevel",
    "UnemploymentAtAbovePovertyLevel",	"Unemployment",	"UnemploymentLessHighSchool",
    "UnemploymentHighSchool",	"UnemploymentSomeCollege/Associates",	"UnemploymentBachelors+"
  )

# Initial visualizations
UnempPlot <- ggplot(
  data = Metro15,
  mapping = aes(
    x=Team,
    y=Unemployment
  )
) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_text(
    mapping = aes(
      label = PositionInLeague
    ),
    position = position_dodge(1),
    vjust = 0
  )

UnempPlot

PercentWhitePlot <- ggplot(
  data = Metro15,
  mapping = aes(
    x = Team,
    y = PctWhite
  )
) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_text(
    aes(
      label = PositionInLeague
    ),
    position = position_dodge(1),
    vjust = 0
  )

PercentWhitePlot






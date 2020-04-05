hpop <- read.csv("Spring 19 data.csv")

education_levels <- unique(hpop$Education..Categorical.)



hs <- subset(hpop, hpop$Education..Categorical. == "High school")
lesshs <- subset(hpop, hpop$Education..Categorical. == "Less than high school")
someColl <-subset(hpop, hpop$Education..Categorical. == "Some college")
bacc <- subset(hpop, hpop$Education..Categorical. == "Bachelor's degree or higher")

model <- lm()


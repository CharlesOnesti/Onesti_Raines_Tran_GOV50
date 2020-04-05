hpop <- read.csv("Spring 19 data.csv")

education_levels <- unique(hpop$Education..Categorical.)



hs <- subset(hpop, hpop$Education..Categorical. == "High school")
lesshs <- subset(hpop, hpop$Education..Categorical. == "Less than high school")
someColl <-subset(hpop, hpop$Education..Categorical. == "Some college")
bacc <- subset(hpop, hpop$Education..Categorical. == "Bachelor's degree or higher")

model <- lm(climate ~ 
              Education..Categorical. + 
              Household.Income + 
              Which.of.the.following.best.describes.the.area.in.which.you.live..if.in.college..when.not.in.school.., data = hpop, na.action = na.omit)
coef(model)
summary(model)

plot(NA, NA, xlim = c(0,1), ylim = c(0,1))
abline(model)

hpop$climate <- NA 
hpop$climate[hpop$Which.of.the.following.statements.comes.closest.to.your.view.regarding.climate.change. == "It's a crisis and demands urgent action"] <- 3
hpop$climate[hpop$Which.of.the.following.statements.comes.closest.to.your.view.regarding.climate.change. == "It's a problem, but we don't need urgent action now; other issues are more important"] <- 2
hpop$climate[hpop$Which.of.the.following.statements.comes.closest.to.your.view.regarding.climate.change. == "It's not a problem; attempting to address is will be harmful to the economy"] <- 0




library(ggplot2)
hpop <- read.csv("Spring 19 data.csv")

names(hpop)[names(hpop) == "Which.of.the.following.best.describes.the.area.in.which.you.live..if.in.college..when.not.in.school.."] <- "urbanization"
names(hpop)[names(hpop) == "Education..Categorical."] <- "education"
names(hpop)[names(hpop) == "Which.of.the.following.statements.comes.closest.to.your.view.regarding.climate.change."] <- "climate_opinion"
names(hpop)[names(hpop) == "Household.Income"] <- "income"
names(hpop)[names(hpop) == "X.Most.important..Which.of.the.following.do.you.consider.to.be.the.most.and.second.most.important.goals.of.U.S..foreign.policy."] <- "first"
names(hpop)[names(hpop) == "X.Second.most.important..Which.of.the.following.do.you.consider.to.be.the.most.and.second.most.important.goals.of.U.S..foreign.policy."] <- "second"


hpop_clean = subset(hpop, select = c(education, income, urbanization, climate_opinion, first, second))

education_levels <- unique(hpop_clean$Education..Categorical.)



hs <- subset(hpop_clean, hpop_clean$education == "High school")
lesshs <- subset(hpop_clean, hpop_clean$education == "Less than high school")
someColl <-subset(hpop_clean, hpop_clean$education == "Some college")
bacc <- subset(hpop_clean, hpop_clean$education == "Bachelor's degree or higher")

model <- lm(climate ~ 
              education + 
              income + 
              urbanization, data = hpop_clean, na.action = na.omit)
coef(model)
summary(model)

# plot(NA, NA, xlim = c(0,1), ylim = c(0,1))
# abline(model)

hpop_clean$climate <- ifelse(hpop_clean['climate_opinion'] == "It's a crisis and demands urgent action", 3, ifelse(hpop_clean['climate_opinion'] == "It's a problem, but we don't need urgent action now; other issues are more important", 2, 0))

averages_education <- c('Less than HS diploma' = mean(lesshs$climate), 'HS Diploma' = mean(hs$climate), 'Some College' = mean(someColl$climate), 'Bachelor\'s degree or higher' = mean(bacc$climate))


barplot(xlab = 'Education Level', ylab = 'Climate Attitude', averages_education)



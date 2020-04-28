library(ggplot2)
hpop <- read.csv("Spring 19 data.csv")

names(hpop)[names(hpop) == "Which.of.the.following.best.describes.the.area.in.which.you.live..if.in.college..when.not.in.school.."] <- "urbanization"
names(hpop)[names(hpop) == "Education..Categorical."] <- "education"
names(hpop)[names(hpop) == "Which.of.the.following.statements.comes.closest.to.your.view.regarding.climate.change."] <- "climate_opinion"
names(hpop)[names(hpop) == "Household.Income"] <- "income"
names(hpop)[names(hpop) == "X.Most.important..Which.of.the.following.do.you.consider.to.be.the.most.and.second.most.important.goals.of.U.S..foreign.policy."] <- "first"
names(hpop)[names(hpop) == "X.Second.most.important..Which.of.the.following.do.you.consider.to.be.the.most.and.second.most.important.goals.of.U.S..foreign.policy."] <- "second"


hpop_clean = subset(hpop, select = c(education, income, urbanization, climate_opinion, first, second))

hpop_clean$climate <- ifelse(hpop_clean['climate_opinion'] == "It's a crisis and demands urgent action", 2, ifelse(hpop_clean['climate_opinion'] == "It's a problem, but we don't need urgent action now; other issues are more important", 1, 0))

hpop_clean$income_category <- ifelse(hpop_clean['income'] == 'Less than $5,000' |
                                       hpop_clean['income'] == '$5,000 to $7,499' |
                                       hpop_clean['income'] == '$7,500 to $9,999' |
                                       hpop_clean['income'] == '$10,000 to $12,499' |
                                       hpop_clean['income'] == '$12,500 to $14,999' |
                                       hpop_clean['income'] == '$15,000 to $19,999' |
                                       hpop_clean['income'] == '$20,000 to $24,999' |
                                       hpop_clean['income'] == '$25,000 to $29,999' |
                                       hpop_clean['income'] == '$30,000 to $34,999' |
                                       hpop_clean['income'] == '$35,000 to $39,999',
                                       'lower', ifelse(hpop_clean['income'] == '$40,000 to $49,999' |
                                                   hpop_clean['income'] == '$50,000 to $59,999' |
                                                   hpop_clean['income'] == '$60,000 to $74,999' |
                                                   hpop_clean['income'] == '$75,000 to $84,999' |
                                                   hpop_clean['income'] == '$85,000 to $99,999' |
                                                   hpop_clean['income'] == '$100,000 to $124,999' |
                                                   hpop_clean['income'] == '$125,000 to $149,999', 'middle', 'upper'))


hs <- subset(hpop_clean, hpop_clean$education == "High school")
lesshs <- subset(hpop_clean, hpop_clean$education == "Less than high school")
someColl <-subset(hpop_clean, hpop_clean$education == "Some college")
bacc <- subset(hpop_clean, hpop_clean$education == "Bachelor's degree or higher")

model <- lm(climate ~ 
              education + 
              income_category + 
              urbanization, data = hpop_clean, na.action = na.omit)
coef(model)
summary(model)

unique(hpop_clean$urbanization)
unique(hpop_clean$income_category)
unique(hpop_clean$education)

# plot(NA, NA, xlim = c(0,1), ylim = c(0,1))
# abline(model)


averages_education <- c('Less than HS diploma' = mean(lesshs$climate), 'HS Diploma' = mean(hs$climate), 'Some College' = mean(someColl$climate), 'Bachelor\'s or Higher' = mean(bacc$climate))


barplot(xlab = 'Education Level', ylab = 'Climate Attitude', ylim = c(0,2), averages_education)



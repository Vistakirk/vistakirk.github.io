library(DAAG)
library(party)
library(rpart)
library(rpart.plot)
library(mlbench)
library(pROC)
library(tree)
library(caret)
library(e1071)
library(lime)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(car)

setwd("D:/STAT E109/Week 13/")

birds <- read.csv("faa_data_subset.csv")


top50 <- birds %>% 
  group_by(birds$Wildlife..Species) %>%
  summarize(Count=n()) %>%
  arrange(desc(Count)) %>%
  head(50)
top50


damagedflights <- birds %>% 
  group_by(birds$Effect..Indicated.Damage) %>%
  summarize(Count=n()) %>%
  arrange(desc(Count))
damagedflights

# Cleaning
birds$When..Time.of.day <- ifelse(nchar(birds$When..Time.of.day)==0, "Night", birds$When..Time.of.day)
birds$When..Time.of.day[birds$When..Time.of.day == "Dawn"] <- "Day"
birds$When..Time.of.day[birds$When..Time.of.day == "Dusk"] <- "Night"

birds$Feet.above.ground[birds$Feet.above.ground == ""] <- 0


# factor time of day
birds$timeofday[birds$When..Time.of.day == "Day"] <- 1
birds$timeofday[birds$When..Time.of.day == "Night"] <- 2



# Remove NAs
birds[!(is.na(birds$Typical.Weight) | birds$Typical.Weight==""), ]
birds.truncated <- birds[!(is.na(birds$Typical.Weight) | birds$Typical.Weight==""), ]
birds.truncated <- birds[!(is.na(birds$Typical.Weight) | birds$Typical.Weight==""), ]

# tod.factor %>% fct_lump(3) %>%table()
# summary(tod.factor)

# tree <- rpart(Effect..Indicated.Damage ~ Aircraft..Number.of.engines + phaseFACTOR + todFACTOR, data = birds)
# rpart.plot(tree)
# printcp(tree)
# plotcp(tree)

# Split data
set.seed(4321)
ind <- sample(2, nrow(birds.truncated), replace = T, prob = c(0.7, 0.3))
train <- birds.truncated[ind == 1,]
test <- birds.truncated[ind == 2,]


# One-Hot encoding for categorical data ? may not need

# Check for multicolinearity
# Linear model can't be used anyhow


logisticm <- glm(as.factor(Effect..Indicated.Damage)~Aircraft..Number.of.engines + timeofday + Typical.Weight + Feet.above.ground, data = train, family = 'binomial')
summary(logisticm)


# create datasubset of only collision events 

birds.onlydamage <- filter(birds,Effect..Indicated.Damage == "Caused damage")

linear <- lm(Cost..Total ~ Aircraft..Number.of.engines + timeofday + Typical.Weight + Feet.above.ground - 1, data = train)
summary(linear)

linearsimple <- lm(Cost..Total ~ Typical.Weight- 1, data = train)
summary(linearsimple)

# test for regression assumptions
plot(linearsimple, 1)
plot(linearsimple, 2)
plot(linearsimple, 3)

# convert linear model to log transform

linearlog <- lm(log1p(Cost..Total) ~ log1p(Typical.Weight)- 1, data = train)
summary(linearlog)
plot(linearlog, 1)
plot(linearlog, 2)
plot(linearlog, 3)

nonlinear <- lm(log1p(Cost..Total) ~ poly(Typical.Weight, 2)- 1, data = train)
plot(nonlinear, 1)
plot(nonlinear, 2)
plot(nonlinear, 3)




###DY 05-01-24

#Categorize dates into seasons: 1=Spring, 2=Summer, 3=Fall, 4=Winter (append chunk to Cleaning)
birds.truncated$Collision.Date.and.Time <- as.Date(birds.truncated$Collision.Date.and.Time, format="%m/%d/%Y")
birds.truncated$season[format(birds.truncated$Collision.Date.and.Time, '%m') %in% c('03','04','05')] <- 1
birds.truncated$season[format(birds.truncated$Collision.Date.and.Time, '%m') %in% c('06','07','08')] <- 2
birds.truncated$season[format(birds.truncated$Collision.Date.and.Time, '%m') %in% c('09','10','11')] <- 3
birds.truncated$season[format(birds.truncated$Collision.Date.and.Time, '%m') %in% c('12','01','02')] <- 4
birds.truncated$season <- factor(birds.truncated$season)


set.seed(4321)
ind <- sample(2, nrow(birds.truncated), replace = T, prob = c(0.7, 0.3))
train <- birds.truncated[ind == 1,]
test <- birds.truncated[ind == 2,]

#Logistic Regression model with all significant predictors by replacing timeofday with season
logisticm2 <- glm(factor(Effect..Indicated.Damage) ~ Aircraft..Number.of.engines + Typical.Weight + Feet.above.ground + season, data=train, family='binomial')
summary(logisticm2)

cat('Baseline Rate : ', sum(train$Effect..Indicated.Damage=='No damage') / nrow(train), '\n')
p1 = predict(logisticm2, train, type='response')
pred1 = ifelse(p1>0.89, 'No damage', 'Caused damage')
confusionMatrix(factor(pred1), factor(train$Effect..Indicated.Damage), positive='No damage')


# using test data on model
p2 = predict(logisticm2, test, type='response')
pred2 = ifelse(p2>0.89, 'No damage', 'Caused damage')
confusionMatrix(factor(pred2), factor(test$Effect..Indicated.Damage), positive='No damage')

par(mfrow=c(1,1))
ggplot(birds_vs_mammals, aes(x=Typical.Weight, y=log(Cost..Total), col=Wildlife..Animal.Category)) +
  geom_point() +
  geom_smooth(method='lm', se=0) +
  labs(title='Total Cost of Damage vs. Typical Weight, per Animal Category',
       y='Total Cost (Log)', x='Animal Weight (oz)')

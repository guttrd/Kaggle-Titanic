# Notes:
# 1. Load raw data
# 2. Combine data sets
# 3. Decide what data type the variables need to be
# 4. Form and test hypothesis
# 5. 

# Set working directory
setwd("~/Documents/R/Kaggle-Titanic/Data")

# Load Raw Data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# Add a "Survived" to the test set to allow for combining data sets
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])

# Combine data sets
data.combined <- rbind(train, test.survived)

# A bit about R data types (e.g., factors)
str(data.combined)
?factor

data.combined$Pclass <- as.factor(data.combined$Pclass)
data.combined$Survived <- as.factor(data.combined$Survived)

# Take a look at gross survival rates
table(data.combined$Survived)

table(data.combined$Pclass)

# Load ggplot2 package for data visualization
library(ggplot2)

# Hypothesis: Rich people survived at a higher rate
train$Pclass <- as.factor(train$Pclass)
str(train)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(width = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Examine the first names in the training data set
head(as.character(train$Name))

# How many unique names are there across both train and test?
length(unique(as.character(data.combined$Name)))

# Two duplicate names, take a closer look
# First, get the duplicate names and display them as a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

# Second, take a look at the records in the combined data set
data.combined[which(data.combined$Name %in% dup.names), ]

# What's up with the Mr. & Mrs. thing?
library(stringr)

# Any correlation with other variables (e.g., SibSp)?
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]

# Hypothesis: Name titles correspond with age
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses[1:5,]

# Check males to see if pattern continues 1:08:45
males <- data.combined[which(train$Sex == "male"),]
males[1:5,]

# Expand upon the relationship between "Survived" and "Pclass" by adding the new "Title" variable to the data set
# And then explore a potential 3-dimensional relationship

# Create a utility function to help with title extraction
extractTitle <- function(Name) {
  name <- as.character(Name)
  
  if (length(grep("Miss.", Name)) > 0) {
    return("Miss.")
  } else if (length(grep("Master.", Name)) > 0) {
    return("Master.")
  } else if (length(grep("Mrs.", Name)) > 0) {
    return("Mrs.")
  } else if (length(grep("Mr.", Name)) > 0) {
    return("Mr.")
  } else {
    return("Other")
  }
}

titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i, "Name"]))
}
data.combined$Title <- as.factor(titles)

# Since we only have "Survived" labels for train set, only use first 891 rows
ggplot(data.combined[1:891,], aes(x = Title, fill = Survived)) +
  stat_count(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

# What's the distribution of females to males across train & test?
table(data.combined$Sex)

# Visualize the 3-way relationship of Sex, Pclass, and Survival, and compare to analysis
ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived)) +
  stat_count(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Ok, age and sex seem pretty important as derived from analysis of title,
# Let's try to lok at the distributions of age over the entire data set
summary(data.combined$Age)

# Just to be thorough, take a look at survival rates broken out by sex, age, pclass, and 
ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) +
  geom_histogram(binwidth = 10) +
  facet_wrap(~Sex + Pclass) +
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count")

# Validate that "Master." is a pretty good proxy for male children
boys <- data.combined[which(data.combined$Title == "Master."),]
summary(boys$Age)

# We know that "Miss." is a little more complicated, let's examine further
misses <- data.combined[which(data.combined$Title == "Miss."),]
summary(misses$Age)

ggplot(misses[misses$Survived != "None",], aes(x = Age, fill = Survived)) +
  facet_wrap(~Pclass) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age for Miss. by Pclass")+
  xlab("Age") +
  ylab("Total Count")

# Ok, appears female children may have different survival rate
# Could be a candidate for feature engineering later
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))

# Move on to the sibsp variable, summarize the variable
summary(data.combined$SibSp)

# Can we treat as a factor?
length(unique(data.combined$SibSp))

data.combined$SibSp <- as.factor(data.combined$SibSp)

# We believe title is predictive. Visualize survival rates by SibSp, Pclass, and Title
ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived)) +
  stat_count(width = 1) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")
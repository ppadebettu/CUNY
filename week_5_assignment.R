#Week 5 Assignment Answers

#Exercise 1

#Write down 3 questions that you might want to answer based on this data.

 #Question 1) What is the overall percentage of "Yes" and "No" votes?

 #Question 2) What is the total # of "Yes" votes who are in the age group of 25 or more?

 #Question 3) How many total votes polled in Glasgow?

# Exercise 2

# Create an R data frame with 2 observations to store this data in its current "messy" state. Use whatever 
# method you want to re-create and/or load the data.

library(plyr)
library(dplyr)
library(tidyr)

survey <- read.csv("C:\\Data\\food-survey.csv",sep="," ,stringsAsFactors=FALSE)
# Result: 2 obs. of 6 variables loaded into data frame survey (in messy data format)

# Exercise 3
# Use the functionality in the tidyr package to convert the data frame to be "tidy data."

survey.1 <- survey %>% gather(City, Votes,-Item,-Yes_No)
# Result: A data frame with 8 obversations of 4 variables, this will retain Item and 
# Yes_No variables, but normalizes City and Votes into new rows

survey.2 <- survey.1 %>% separate(City, into = c("City", "Age"), sep = "\\.")
# Result: A data frame with 8 obversations of 5 variables, this further
# normalizes the previous data frame by splitting City variable into City and Age variables

# Exercise 4
# Use the functionality in the plyr package to answer the questions that you asked in step 1.

# 1) What is the overall percentage of "Yes" and "No" votes?

plyr.1 <- ddply(survey.2, "Yes_No", summarise, sum.Votes = sum(Votes))

paste(round((subset(plyr.1, Yes_No=="Yes", select=sum.Votes) / sum(plyr.1$sum.Votes)*100),2),"%")
# Result:  "48.57 %"
paste(round((subset(plyr.1, Yes_No=="No", select=sum.Votes) / sum(plyr.1$sum.Votes)*100),2),"%")
#Result: "51.43 %"

# 2) What is the total # of "Yes" votes who are in the age group of 25 or more?

plyr.2 <- ddply(survey.2, c("Yes_No", "Age"), summarise, sum.Votes = sum(Votes))

subset(plyr.2, Yes_No=="Yes" & Age=="25_More", select=sum.Votes)
# Result: 293400

# 3) How many total votes polled in Glasgow?

plyr.3 <- ddply(survey.2, "City", summarise, sum.Votes = sum(Votes))

subset(plyr.3, City=="Glasgow", select=sum.Votes)
# Result: 499800


# Exercise 5
# Answer: I believe I asked the right kind of questions and also built the data frames(messy and tidy) in the rigt format. 
# But to reduce the number of intermediary data frames to answer these questions, dplyr would have been a 
# better package compared to plyr package because of it's chaining functionality.

# I think even if I posed different questions like "How many people preferred Partan Bree
# over Cullen Skink, it can be answered with data frame survey 2. After further analysis
# I realized, the values in Item variable are not appropriate. They should not be two individual
# food products which is mis-leading, but a question like "Preferred Cullen Skink over Partan Bree?"
# and the variable should be named Question instead of Item.


# Load ggplot2 package
library(ggplot2)

# Load movies data into memory
#movies <- read.table("c:\\Data\\movies.tab", sep="\t", header=TRUE, quote="", comment="")
data(movies)


#***********Exercise 1

ggplot(movies, aes(x=year)) + geom_histogram(color="blue", fill="green", binwidth=10) + xlab("Year") + ylab("# of Movies") + 
stat_bin(geom="text", aes(label=..count..,binwidth=10, position="identity"))

# Analysis: A bar plot with # of movies by decade


************Exercise 2

# load reshape package
require(reshape)

# convert movies dataframe into normalized format
movies.long <- melt(movies, id = c("rating","year","length", "votes"), measure = c("Action","Animation","Comedy","Drama","Documentary","Romance","Short"))

ggplot(movies.long, aes(value,rating,colour = variable)) + geom_line() 
# Analysis: Animation movie ratings are falling while others are increasing

ggplot(movies.long, aes(year, rating ,colour = year)) + geom_line() 
# Analysis: Starting 1970's, every decade saw the ratings go thru a cycle of ups and downs



************Exercise 3

ggplot(movies, aes(x=rating)) + geom_histogram(color="blue", fill="green", binwidth=.1) +
  xlab("Rating") +
  ylab("Length")
# Analysis: The ratings are falling after the durations of the movies have reached a certain limit



************Exercise 4

ggplot(movies) + geom_line(aes(x = Animation, y = length, weight=length , color = "Animation")) + geom_line(aes(x = Action, y = length, weight=length , color = "Action")) + geom_line(aes(x = Comedy, y = length, weight=length , color = "Comedy")) + geom_line(aes(x = Action, y = length, weight=length , color = "Drama")) + geom_line(aes(x = Documentary, y = length, weight=length , color = "Documentary")) + geom_line(aes(x = Action, y = length, weight=length , color = "Romance")) + geom_line(aes(x = Short, y = length, weight=length , color = "Short"))
# Analysis: The comedy and documentary movies seem to be in the lengthy categories



************Exercise 5

ggplot(movies, aes(x=year, weight=votes)) +
  geom_histogram(color="blue", fill="green", binwidth=10) +
  xlab("Decade") + ylab("# of Votes")
# Analysis: There is a gradual increase in the number of votes towards late 20th century though this is not indicative of future trends


ggplot(movies.long, aes(value,votes,colour = variable)) + geom_line() 
# Analysis: It looks like the Genre (like Action, Comedy) are a high vote pulling factor

ggplot(movies, aes(x=rating, weight=votes)) +
  geom_histogram(color="blue", fill="green", binwidth=1) +
  xlab("Rating") + ylab("# of Votes")
# Analysis: Above average ratings are drawing more votes

ggplot(movies, aes(x=rating, weight=length)) +
  geom_histogram(color="blue", fill="green", binwidth=1) +
  xlab("Length") + ylab("# of Votes")
# Analysis: Similiar to ratings, the movies above average duration are drawing more votes

# Final Analysis: The Genre has a major role in predicting the number of votes for a particular movie


 


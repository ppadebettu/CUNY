# Exercise 1

require(XML)

theURL <- "http://www.jaredlander.com/2012/02/another-kind-of-super-bowl-pool/"

bowlPool <- readHTMLTable(theURL, which = 1, header = FALSE, stringsAsFactors = FALSE)

class(bowlPool)

# Result is a data.frame

# Exercise 2

theURL <- "http://www.w3schools.com/html/html_tables.asp"

hvalues <- readHTMLTable(theURL)

class(hvalues)

# Result is a list

# Exercise 3

# First, let's create a function to count number of HTML tables
getNumTables <- function(inDF){
  l <- 0
  k <- 0
  for(i in 1:length(inDF)){
  k <- length(inDF[[i]])
  if (k>0) {
    l <- l+1
  }
}
return(l)
}
        
getNumTables(hvalues)

# Result is 2 tables

# Exercise 4

newDF <- readHTMLTable(theURL, which = 1, stringsAsFactors = FALSE)

# Exercise 5

names(newDF)

newDF[3:4]

# Result is a data frame with Last Name and Points data

# Exercise 6

theURL <- "http://www.isro.org/"
newDF1 <- readHTMLTable(theURL)
class(newDF1)
# newDF1 is a list

getNumTables(newDF1)

# Result is 24 tables

# Exercise 7

I use FireFox 22.0

# After I open a web page, I right click on "View Page Source"
# It opens up the source information in a notepad and we can look into all the HTML elements 
# like <HTML>,  <HEAD>,  <title> in the page. We can also find scripting code like javascript in the 
# page source

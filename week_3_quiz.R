# Exercise 1
vector1 <- c(1,3,5,7,9,9,11,11,13,15,17,19,21,23,25,27,29,31,33,36)
vec.mean <- function(x)
{
  return(mean(x))
}
vec.mean(vector1) # Result is 17.25

# Exercise 2
vector1 <- c(1,3,5,7,9,9,11,11,NA,13,15,17,19,NA,21,23,25,NA,27,29,31,33,36)
newvec.mean <- function(x)
{
  return(mean(x,na.rm=TRUE))
}
newvec.mean(vector1) # Result is 17.25

# Exercise 3
fact.num <- function(xIn,yIn) {
  x <- as.integer(xIn)
  y <- as.integer(yIn)
  seqx <- seq_len(abs(x))
  seqy <- seq_len(abs(y))
  combseq <- unique(c(seqx,seqy))
  divlistx <- combseq[x %% combseq == 0L]
  divlisty <- combseq[y %% combseq == 0L]
  newdivlist <- unique(intersect(divlistx,divlisty))
  return(max(newdivlist))
}
fact.num(872,200) # Result is 8

# Exercise 4
euclid.gcd <- function(a, b) {
  while(b != 0) {
    t <- b
    b <- a %% b
    a <- t
  }
  return(a)
}
euclid.gcd(1134,336) #Result is 42


# Exercise 5
func.num <- function(xIn,yIn) {
  x <- as.integer(xIn)
  y <- as.integer(yIn)
  #Apply equation x^2+2xy-xy^2
  return((x^2)+(2*x*y)-(x*(y^2)))
}
func.num(2,3) #Result is -2

# Exercise 6
price.dat <- read.csv("C:\\Data\\week-3-price-data.csv",sep=",")

model.dat <- read.csv("C:\\Data\\week-3-make-model-data.csv",sep=",")

comb.dat <- merge(price.dat, model.dat, by="ModelNumber")

# Exercise 7
comb.dat <- merge(price.dat, model.dat, by="ModelNumber", all.x=TRUE)

# Exercise 8
newdata <- subset(comb.dat, Year==2010,
                  select=c(ModelNumber:Year))
# Exercise 9
newdata1 <- subset(comb.dat, Color=="Red" & Price > 10000,
                   select=c(ModelNumber:Year))

# Exercise 10
newdata2 <- newdata1[c(-1,-3)]

# Exercise 11
num.vec1 <- function(inVec){
  int.vec <- nchar(inVec)
  return(int.vec)
}

num.vec1(c("new", "good", "welcome")) # Result is [1] 3 4 7

# Exercise 12
vec1 <- c("welcome", "old", "hello")
vec2 <- c("aboard","friend","world")

comb.vec <- function(inV1, inV2) {
  a <- c() 
  if (length(inV1) == length(inV2))
    for (i in 1:length(vec1)) {
      a <- c(a,paste(vec1[i],vec2[i],sep=' '))
    }
  else
    return("Sorry, the two vectors should be of equal length")
  return(a)
}
comb.vec(vec1,vec2) # Result is [1] "welcome aboard" "old friend"     "hello world"   

vec2 <- c("aboard","friend","world", "good")
comb.vec(vec1,vec2) # Result is "Sorry, the two vectors should be of equal length"

# Exercise 13
ret.vowelstr <- function(inV)
{
  p <- regexpr("[aeiou]", inV, perl=TRUE)
  return(substr(inV,ifelse(p>0,p,NA), ifelse(p>0,p+2,NA)))
}

ret.vowelstr(c("good", "friend", "apple", "umbrella", "glkw")) # Result is [1] "ood" "ien" "app" "umb" NA 

# Exercise 14
get.date <- function(monthcol, daycol, yearcol)
{
  date.dat <- data.frame(monthcol,daycol,yearcol)
  date.vec <- as.Date(paste(monthcol, daycol, yearcol, sep="-"),"%m-%d-%Y")
  date.dat["date"] <- date.vec
  return(date.dat)
}

get.date(c(3,7,8,4,9), c("11","5","23","17","2"), c(2003,2010,2013,1998,1995)) # Result is a data frame


# Exercise 15
func.date <- function(dt) {
  return(as.Date(dt, "%m-%d-%Y"))
}

func.date("07-31-2009") # Result is [1] "2009-07-31"

# Exercise 16
func.ret_mon <- function(dt) {
  # extract month
  return(as.numeric(format(dt, "%m")))
}

func.ret_mon(as.Date("1990-12-11")) # Result is 12

# Exercise 17
date.list <- seq(as.Date("2005/1/1"), as.Date("2014/12/31"),"days")


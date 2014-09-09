# Answers to Week 2 Quiz Questions
# Student Name:       Prashanth Padebettu

# Exercise 1
vector1 <- c(1,3,5,7,9,9,11,11,13,15,17,19,21,23,25,27,29,31,33,36)
print(vector1)

# Exercise 2
vector1 <- as.character(vector1)
print(vector1)

# Exercise 3
vector1 <- factor(vector1)

# Exercise 4
print(levels(vector1))

# Exercise 5
vector1 <- c(1,3,5,7,9,9,11,11,13,15,17,19,21,23,25,27,29,31,33,36)
vector2 <- (3 * (vector1 ^ 2)) - (4 * vector1) + 1
print(vector2)

# Exercise 6
X <- matrix( 
  c(1,1,1,1,1,1,1,1,5,4,6,2,3,2,7,8,8,9,4,7,4,9,6,4), 
  nrow=8, 
  ncol=3)


y <- matrix(c(45.2,46.9,31.0,35.3,25.0,43.1,41.0,35.1),
            nrow=8,
            ncol=1)


# Applying the formula for inverse of sqaure matrix (solve), and calculating least squares regression
beta <- as.vector(solve(t(x) %*% x) %*% t(x) %*% y)
beta



# Exercise 7
v = list(months=c("jan","feb","mar","apr","may","jun"), quarters=c(1,2))
v["months"]
print(v$months)
print(v$quarters)

# Exercise 8
memberInitial <- c("KG","PP","TS","AF","DR","JS","DT","ES","GV","WS")
memberState <-factor(c("NY","NY","CT","NJ","NJ","NY","CT","NY","NJ","NJ"))
memberID <- c(1001,1002,1003,1004,1005,1006,1007,1008,1009,1010)
memberAppDate <- as.Date(c("2011-08-31","2012-01-01","2012-02-14","2011-07-31","2011-06-29","2012-01-16","2011-08-17","2011-07-15","2011-06-30","2011-05-14"))
member.dat <- data.frame(memberInitial,memberState,memberID,memberAppDate)

# Exercise 9
newRow <- data.frame(memberInitial="RD",memberState="ME",memberID=1011,memberAppDate=as.Date("2011-07-12"))
member.dat <- rbind(member.dat,newRow)

# Exercise 10
setwd("C:\\Temp")
temp.dat <- read.csv("temperatures.csv",skip=1)

# Exercise 11
height.dat <- read.csv("C:\\Data\\measurements.txt",sep="\t",skip=1)

# Exercise 12
web.dat <- read.table("http://data.princeton.edu/wws509/datasets/effort.dat", sep="\t")

# Exercise 13
y <- 1
for(i in 1:12){
  y <-y*((1:12)[i])
}
print(y) 

# Exercise 14
bal <- 1500
rate <- 0.0324
for(i in 1:72) {
  int.amt <- bal * (rate/12)
  bal <- bal + int.amt
}
final.bal <- round(bal,digits=2)
print(final.bal)


# Exercise 15
vector1 <- c(1,3,5,7,9,9,11,11,13,15,17,19,21,23,25,27,29,31,33,36)
tot.vec <- 0
i <- 3
while (i <= length(vector1)) {
  
  tot.vec <- tot.vec + vector1[i]
  
  i <- i+3
  
}
print(tot.vec)

# Exercise 16
z <- 0
for (i in 1:10) {
  z <- z + (2 ^ i)
}
print(z)

# Exercise 17
z <- 0
i <- 1
while (i <=10) {
  z <- z + (2 ^ i)
  i <- i+1
}
print(z)

# Exercise 18
z <- 0
z <- ( (2 ^ 1) + (2 ^ 2) + (2 ^ 3) + (2 ^ 4) + (2 ^ 5) + (2 ^ 6) + (2 ^ 7) + (2 ^ 8) + (2 ^ 9) + (2 ^ 10) )
print(z)

# Exercise 19
num.vector <- seq(from=20, to=50, by=5)
print(num.vector)

# Exercise 20
ten.elem <- c("example","example","example","example","example","example","example","example","example","example")
print(ten.elem)

# Exercise 21
# Let's take quadratic equation 5x^2 + 6x + 1 = 0

a <- 5
b <- 6
c <- 1

#Solve for x = [ -b +or- v(b2-4ac) ] / 2a

x <- (-b + sqrt((b ^ 2)-(4*a*c))) / (2*a)
print(x) #OR
x <- (-b - sqrt((b ^ 2)-(4*a*c))) / (2*a)
print(x)


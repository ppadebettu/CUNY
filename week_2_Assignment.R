# Week 2 Assignment

# Exercise 1

queue <- c("James", "Mary", "Steve", "Alex", "Patricia" )
print(queue)
queue <- c(queue,"Harold")
#queue <- queue[queue != "James"]
queue <- queue[-1]
queue <- append(queue, "Pam", after=1)
queue <- queue[-6]
queue <- queue[queue != "Alex"]
which(queue=="Patricia") # Patricia's position in the queue is 4
length(queue) # Number of people in the queue is 4


# Exercise 2
# Let's take quadratic equation ax^2 + bx + 1 = 0

# eg:5,6,1 (discriminant > 0)
# eg:1,2,1 (discriminant ==0)
# eg:1,1,1 (discriminant < 0)

quad.eq <- function(in1,in2,in3) {

a <- as.integer(in1)
b <- as.integer(in2)
c <- as.integer(in3)

discri.val <- (b ^ 2)-(4*a*c)

if(discri.val >0){
  x <- (-b + sqrt((b ^ 2)-(4*a*c))) / (2*a)
  y <- (-b - sqrt((b ^ 2)-(4*a*c))) / (2*a)
  return(paste("There are two roots to the quadratic equation ->",x,",", y))
} else if(discri.val==0){
  return(paste("The two roots to the quadratic equation are same->", (-b + sqrt((b ^ 2)-(4*a*c)))/ (2*a)))
} else {
  x <- (-b + sqrt(as.complex((b ^ 2)-(4*a*c)))) / (2*a)
  y <- (-b - sqrt(as.complex((b ^ 2)-(4*a*c)))) / (2*a)
  return(paste("The two roots to the quadratic equation are complex and unequal->",x,",", y))
}
}

quad.eq(5,6,1) # Result -0.2 , -1
quad.eq(1,2,1) # Result -1
quad.eq(1,1,1) # Result -0.5+0.866025403784439i , -0.5-0.866025403784439i"


# Exercise 3
x <- c(1:1000)
length(x) - length(x[(x %% 3==0) | (x %% 7==0 ) | (x %% 11==0)]) # Result is 520

# Exercise 4
py.triple <- function(in1,in2,in3) {
#f <- as.integer(readline("enter a positive integer: ")) #5
#g <- as.integer(readline("enter a positive integer: ")) #12
#h <- as.integer(readline("enter a positive integer: ")) #13
f <- as.integer(in1)
g <- as.integer(in2)
h <- as.integer(in3)
v <- c(f,g,h)
v.max.pos <- which(v==max(v))
v.max.sq = v[v.max.pos] ^ 2
v <- v[-v.max.pos]
v.sum.sq = (v[1]^2) + (v[2]^2)
if(v.max.sq==v.sum.sq) return(paste("The input constants ",f," ",g," ",h," ", " form a Pythagorean Triple")) else
  return(paste("The input constants ",f," ",g," ",h," ", " do not form a Pythagorean Triple"))
}

py.triple(5,12,13) # Result -> "The input constants  5   12   13    form a Pythagorean Triple"




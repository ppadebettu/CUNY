# A generic function to remove NA from vectors and used in the assignment below
rm.na <- function(inV){
  return(inV[!is.na(inV)])
}

# Exercise 1
missing.val <- function(inV){
  return(sum(is.na(inV)))
}
vec1 <- c(1:9,rep(NA,5),NA,NA,NA,101,102,103,NA)
missing.val(vec1)


# Exercise 2
df.missing.val <- function(inDF){
  ret.val <- c()
 for(i in 1:ncol(inDF)) {
   ret.val <- c(ret.val,missing.val(inDF[,i]))
  }
 names(ret.val) <- c(names(dataset)[1:ncol(inDF)])
 return(ret.val)
}

vec1 <- c(1:9,rep(NA,5),NA,NA,NA,101,102,103,NA)
vec2 <- c(NA,NA,4,5,9,NA,NA,23:36)
vec3 <- c("good","world","new",NA,rep("example",10),rep("news",5),NA,NA)
dataset <- data.frame(col1=vec1,col2=vec2,col3=vec3)
df.missing.val(dataset)

# Exercise 3

findMin <- function(inV) {
  inV <- rm.na(inV)
  minX <- inV[1] 
  for (i in 1:length(inV)) {
    if(inV[i] < minX) 
       minX <- inV[i] 
  }
  return(minX)
}

findMax <- function(inV) {
  inV <- rm.na(inV)
  maxX <- inV[1] 
  for (i in 1:length(inV)) {
    if(inV[i] > maxX) 
       maxX <- inV[i]  
  }
  return(maxX)
}

findMean <- function(inV){
  inV <- rm.na(inV)
  totsum <- sum(inV)
  totcount <- length(inV)
  return(round(totsum/totcount,digits=2))
}

findMedian <- function(inV) {
  inV <- rm.na(inV)
  odd.even <- length(inV)%%2
  if (odd.even == 0)
    return((sort(inV)[length(inV)/2] + sort(inV)[1 + length(inV)/2])/2)
  else return(sort(inV)[ceiling(length(inV)/2)])
}

findQuartile <- function (inV, p) {
  inV <- rm.na(inV)
  v = sort(inV)
  h = ((length(inV)-1)*p)+1
  q <- v[floor(h)]+((h-floor(h))*(v[floor(h)+1]- v[floor(h)]))
  return(q)
}

findSD <- function(inV){
  inV <- rm.na(inV)
  meanofV <- sum(inV) / length(inV)
  sdofV <- sqrt(sum((inV - meanofV)^2) / (length(inV) - 1))
  return(round(sdofV,digits=2))
}

disp.summary <- function(inV){
  newlist <- list("Min"=findMin(inV), "1stQ"=findQuartile(inV,0.25),
                  "Median"=findMedian(inV), "Mean"=findMean(inV),
                  "3rdQ"=findQuartile(inV,0.75), "Max"=findMax(inV),
                  "SD"=findSD(inV), "NAval"=missing.val(inV))
  return(newlist)
}


int.vec <- c(17,14,345,23,11,21,NA,NA,NA,NA)
findMin(int.vec)
findMax(int.vec)
findMean(int.vec)
findMedian(int.vec)
findQuartile(int.vec,0.25)
findQuartile(int.vec,0.75)
findSD(int.vec)
missing.val(int.vec)

disp.summary(vec1)


# Exercise 4

elem.summary <- function(inV){
  distinct.elem <- length(unique(inV))
  t <- table(inV, useNA="always")
  common.elem <- names(which(t==max(t)))
  num.com.elem <- max(t)
  missing.val <- sum(is.na(inV))
  
  newlist <- list("# of distinct elements"=distinct.elem,
                  "most common element(s)"=common.elem,
                  "# of times common elements occured"=num.com.elem,
                  "# of missing values"=missing.val)
  return(newlist)
}

char.vec <- c("good","good","good","news","news",NA,NA,"news","school","school")
elem.summary(char.vec)

# Exercise 5

bool.summary <- function(inV){
  tot.elem <- length(inV)
  t <- table(inV, useNA="always")
  true.val <- t["TRUE"]
  false.val <- t["FALSE"]
  true.ratio <- paste(100*round((true.val / tot.elem),digit=2),"%")
  missing.val <- sum(is.na(inV))
  
  newlist <- list("# of true values"=true.val,
                  "# of false values"=false.val,
                  "ratio of true values"=true.ratio,
                  "# of missing values"=missing.val)
  return(newlist)
}

bool.vec <- c(TRUE,TRUE,FALSE,FALSE,TRUE,TRUE,NA,FALSE,T, F)
bool.summary(bool.vec)

# Exercise 6

df.summary <- function(inDF){
  inV1 <- inDF[,1]
  inV2 <- inDF[,2]
  inV3 <- inDF[,3]
 
  info1 <- c("Min"=findMin(inV1), "1stQ"=findQuartile(inV1,0.25),
                  "Median"=findMedian(inV1), "Mean"=findMean(inV1),
                  "3rdQ"=findQuartile(inV1,0.75), "Max"=findMax(inV1),
                  "NAval"=missing.val(inV1))
  
  info2 <- c(table(as.factor(inV2),useNA="always"))
  
  info3 <- c(Mode=class(inV3),table(inV3,useNA="always"))
  
  newlist <- list(attr1=info1,attr2=info2,attr3=info3)
  print(newlist)
}

dataset <- data.frame(attr1=int.vec,attr2=char.vec,attr3=bool.vec)
df.summary(dataset)



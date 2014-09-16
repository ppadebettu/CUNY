# Load data from csv file
dataset <- read.csv("C:\\Data\\entropy-test-file.csv", sep=",",header=TRUE)


# Generic function to calculate entropy on any vector of catagorical counts
# and can be re-used from multiple places

entropy.val <- function(inV){
  E.val <- 0
 for(data in inV) {
    E.val <- E.val + (-1*((data/sum(inV)) * (log2(data/sum(inV)))))
  } 
 return(E.val)
}

# Main calling function for entropy based on outcomes
entropy <- function(inV) {
   outcomes.val <- c(table(as.factor(inV)))
   return(entropy.val(outcomes.val))
}

entropy(dataset$answer) # Result is 0.9832692

infogain <- function(inV1, inV2) {
 outcomes.val <- table(as.factor(inV2))
 total.val <- table(as.factor(inV1),as.factor(inV2))
 i <- 1
 j <- i+1
 E.val <- 0
 break.lvl <- (length(total.val) / length(outcomes.val))
 for (data in outcomes.val){
   E.val <- E.val + ((data/sum(outcomes.val)) * entropy.val(total.val[i:j]))
   i <- i+break.lvl
   j <- j+break.lvl
 }
 final.val <- entropy(dataset$answer) - E.val
 return(final.val)
}


infogain(dataset$answer,dataset$attr1) # Result is 2.411565e-05
infogain(dataset$answer,dataset$attr2) # Result is  0.2599038
infogain(dataset$answer,dataset$attr3) # Result is 0.002432707


decide <- function(inDF,val){
  ig.val <- c()
  for(i in 1:(val-1)){
    ig.val <- c(ig.val, infogain(dataset[,val],dataset[,i]))
}
names(ig.val) <- c(names(dataset)[1:val-1])
newlist <- list(max=which(ig.val==max(ig.val)),gains=ig.val)
return(newlist)
}

decide(dataset,4)

#Result
#$max
#attr2 
#2 

#$gains
#attr1        attr2        attr3 
#2.411565e-05 2.599038e-01 2.432707e-03 




setwd("~/R_projects")

library(RWeka) # rweka (embedded Weka software)
library(kknn) # knn library

# process data to run R/Weka
pendigits_tra <- read.csv("Pendigits.tra.csv", sep = ",", header=F)
pendigits_tra2 <- pendigits_tra[,1:16]
classy <- as.factor(pendigits_tra[,17])
pendigits_tra2$class <- classy
str(pendigits_tra2)

pendigits_tes <- read.csv("Pendigits.tes.csv", sep = ",", header=F)
pendigits_tes2 <- pendigits_tes[,1:16]
classyt <- as.factor(pendigits_tes[,17])
pendigits_tes2$class <- classyt
str(pendigits_tes2)

#Run Weka
beg <- 1
end <- 20
r <- 1
acc <- vector()
ks <- vector()
for(i in beg:end) {
  classifier2 <- IBk(class ~., data =pendigits_tra2 , control = Weka_control(K = i))
  e <- evaluate_Weka_classifier(classifier2, newdata=pendigits_tes2)
  acc[r] <- e$details[[1]]
  ks[r] <- i
  r = r + 1
}

plot(ks, acc,xlim=c(beg,end),ylim=c(min(acc),max(acc)))



# plot all the numbers in the training set

par(mfrow=c(1,1))
plot.new()
par(mfrow=c(2,5))

for(i in 0:9) {
  plot(c(0,0), c(0,0),xlim=c(0,100),ylim=c(0,100))
  digit <- pendigits_tra2[pendigits_tra2$class==i,]

  for(ii in 1:nrow(digit)) {
    xx <- as.vector(digit[ii,seq(1,16,by=2)], mode="numeric")
    yy <- as.vector(digit[ii,seq(2,16,by=2)], mode="numeric")
    
    lines(xx,yy, col=rgb(0,1,0,0.1))
    
  }
}

# plot all the numbers in the test set
rm(xx,yy,digit)
par(mfrow=c(1,1))
plot.new()
par(mfrow=c(2,5))

for(i in 0:9) {
  plot(c(0,0), c(0,0),xlim=c(0,100),ylim=c(0,100))
  digit <- pendigits_tes2[pendigits_tes2$class==i,]
  
  for(ii in 1:nrow(digit)) {
    xx <- as.vector(digit[ii,seq(1,16,by=2)], mode="numeric")
    yy <- as.vector(digit[ii,seq(2,16,by=2)], mode="numeric")
    
    lines(xx,yy, col=rgb(0,1,0,0.1))
    
  }
}


#Run Weka with k=3

classifier2 <- IBk(class ~., data =pendigits_tra2 , control = Weka_control(K = 3))
e <- evaluate_Weka_classifier(classifier2, newdata=pendigits_tes2)
e



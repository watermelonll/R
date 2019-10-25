# import class dataset
# \ means special char, need \\ in a location
class <- read.table("E:\\R\\class.csv", header = TRUE, sep = ",")
class

# create new num variable
class <- transform(class, bmi = Weight/2/(Height/100)**2)
class

# sort;
newcls <- class[order(class$Height),]
newcls <- class[order(class$Height, -class$Weight),]
newcls

# create new char variable
newcls <- within(newcls,{
          agecat <- NA
          agecat[newcls$Age<14] <- "young"
          agecat[newcls$Age>=14] <- "old" })
newcls

# simple plot
attach(newcls)
plot(newcls$Height, class$Weight, type = "b", lty = 2, pch=17,
     main = "Test class data",
     xlab="Height", ylab="Weight",
     xlim = c(0,80), ylim = c(0,150))
abline(h=c(70,100),lty=3,col="brown")
abline(v=c(70), lty=3,col="green")
detach(newcls)

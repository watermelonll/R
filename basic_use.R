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

library(dplyr)
library(reshape)

# rename variable, case 1   to   = from
newcls <- newcls %>% rename(wddh = bmi)
newcls

# rename, case 2
colnames(newcls)
names(newcls)[6] <- "bmi"
newcls

# rtf
library(rtf)
rtffile <- RTF("d:\\test.rtf")
addParagraph(rtffile,"this is test1\n")
addTable(rtffile,outtab)
addParagraph(rtffile,"\n\nthis is test2")
addTable(rtffile, cbind(rownames(mtcars), mtcars))
done(rtffile)

library(dplyr)
# head, get the first xx records
d1 <- head(mtcars,6)
d2 <- within(d1,{
            cat <- NA 
            cat[d1$gear==4] <- "h" 
            cat[d1$gear==3] <- "l"})
# mutate: Compute and append one or more new columns
# union: set together
d3 <- mutate(d2,cat="t") %>% union(d2,)
View(d3)
filter(d3,carb<2)
## to be updated



dim(Puromycin)
Puromycin
help("Puromycin")
head(Puromycin)
puroa <- subset(Puromycin, state =="treated")
plot(rate ~ conc, data = puroa)
with(puroa, plot(rate ~ conc))
plot(puroa$rate ~ puroa$conc)

u <- 1:25
plot(u ~ 1, pch = u, col=u, cex =3)

plot(rate ~ conc, data = puroa, pch = 2, col = 4, cex = 1.5,
     xlim = c(0, 1.2), ylim = c(40, 210),
     ylab = "Concentration",
     xlab = "Rate", cex.lab = 1)
title(main = "Puromycin", cex.main = 2.5)

library(doBy)
puroa.mean <- summaryBy(rate ~ conc, data = puroa, FUN = mean)
plot(rate ~ conc, data = puroa, pch = 16, col = 4,
     cex = 1.0)
points(rate.mean ~ conc, data = puroa.mean, col = "cyan",
       lwd = 10, pch = "x")
lines(rate.mean ~ conc, data = puroa.mean, col = 2)
abline(v=0.5, col = "tan", lty = 2)
abline(h=120, col = "blue", lty =4)



q()
n

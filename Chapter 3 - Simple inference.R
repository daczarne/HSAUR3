######################################
#### CHAPTER 3 - SIMPLE INFERENCE ####
######################################

library(HSAUR3)
library(dplyr)

data("roomwidth", package="HSAUR3")

#### Sub-section 3.3.1 - Estimating the width of a room ####

convert = ifelse(roomwidth$unit=="feet", 1, 3.28)

tapply(roomwidth$width * convert, roomwidth$unit, summary)
tapply(roomwidth$width * convert, roomwidth$unit, sd)

# Figure 3.1
X11(15,15)
par(oma=c(0,1,0,1))
layout(matrix(c(1,2,1,3), nrow=2, ncol=2, byrow=FALSE))
boxplot(I(width*convert) ~ unit, data=roomwidth, varwidth=TRUE, ylab="Estimated width (feet)", names=c("Estimates in feet", "Estimates in meters (converted to feet)"))
feet <- roomwidth$unit=="feet"
qqnorm(roomwidth$width[feet], ylab="Estimated width (feet)")
qqline(roomwidth$width[feet])
qqnorm(roomwidth$width[!feet], ylab="Estimated width (meters)")
qqline(roomwidth$width[!feet])


I(width * convert) ~ unit

t.test(I(width * convert) ~ unit, data=roomwidth, var.equal=TRUE)
t.test(I(width * convert) ~ unit, data=roomwidth, var.equal=FALSE)
wilcox.test(I(width * convert) ~ unit, data=roomwidth, conf.int=TRUE)

#### Sub-section 3.3.2 - Wave energy device mooring ####

data("waves", package="HSAUR3")

mooringdiff <- waves$method1 - waves$method2

X11(15,15)
par(oma=c(0,1,0,1))
layout(matrix(1:2, ncol=2))
boxplot(mooringdiff, ylab="Differences (Newton meters", main="Boxplot")
abline(h=0, lty=2)
qqnorm(mooringdiff, ylab="Differences (Newton meters)")
qqline(mooringdiff)

t.test(mooringdiff)
wilcox.test(mooringdiff)

#### Sub-section 3.3.3 - Mortality and water hardness

data("water", package="HSAUR3")

X11(15,15)
par(oma=c(0,1,0,1))
nf <- layout(matrix(c(2,0,1,3),2,2,byrow=TRUE),c(2,1),c(1,2), TRUE)
psymb <- as.numeric(water$location)
plot(mortality ~ hardness, data=water, col=c("red","blue"), pch=psymb)
abline(lm(mortality ~ hardness, data=water), col="green")
legend("topright", legend=levels(water$location), col=c("red","blue"), pch=c(1,2), bty="n")
hist(water$hardness)
boxplot(water$mortality)

cor.test(~ mortality + hardness, data=water)

#### Sub-section 3.3.4 - Piston-ring failures

data("pistonrings", package="HSAUR3")

chisq.test(pistonrings)
chisq.test(pistonrings)$residuals

library(vcd)

X11(15,15)
assoc(pistonrings)

#### Sub-section 3.3.5 - Rearrests of juveniles

data("rearrests", package="HSAUR3")

rearrests

mcnemar.test(rearrests, correct=FALSE)
binom.test(rearrests[2], n=sum(rearrests[c(2,3)]))

################################
#### FIN DE LA PROGRAMACI?N ####
################################
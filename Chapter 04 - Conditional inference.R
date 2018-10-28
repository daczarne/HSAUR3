###########################################
#### CHAPTER 4 - CONDITIONAL INFERENCE ####
###########################################

#### Sub-secition 4.3.1 - Roomwidth

data("roomwidth", package="HSAUR3")
convert <- ifelse(roomwidth$unit == "feet", 1, 3.28)
feet <- roomwidth$unit == "feet"
meter <- !feet
y <- roomwidth$width * convert

T <- mean(y[feet]) - mean(y[meter])
T

meandiffs <- double(9999)
for (i in 1:length(meandiffs)) {
      sy <- sample(y)
      meandiffs[i] <- mean(sy[feet]) - mean(sy[meter])
}

X11(15,15)
par(oma=c(0,1,0,1))
hist(meandiffs)
abline(v=T, lty=2)
abline(v=-T, lty=2)

greater <- abs(meandiffs) > abs(T)
mean(greater)

binom.test(sum(greater), length(greater))$conf.int

install.packages("coin")
library(coin)

independence_test(y ~ unit, data=roomwidth, distribution = exact())
wilcox_test(y ~ unit, data=roomwidth, distribution = exact())

#### Sub-section 4.3.2 - Crowds and threatened suicide

data("suicides", package="HSAUR3")

################################
#### FIN DE LA PROGRAMACI?N ####
################################
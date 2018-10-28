###########################################################
#### CHAPTER 6 - SIMPLE AND MULTIPLE LINEAR REGRESSION ####
###########################################################

library(gamair)

#### Sub-section 6.4.1 - Estimating the age of the universe ####

data("hubble", package="gamair")

names(hubble) <- c("galaxy", "velocity", "distance")

X11(15,15)
par(oma=c(0,1,0,1))
plot(velocity ~ distance, data=hubble, pch=16, col="dodgerblue", xlab="Distancia", ylab="Velocidad")

sum(hubble$distance * hubble$velocity)/sum(hubble$distance^2)

hmod <- lm(velocity ~ distance - 1, data=hubble )

coef(hmod)

X11(15,15)
par(oma=c(0,1,0,1))
layout(matrix(1:2, ncol=2))
plot(velocity ~ distance, data=hubble, pch=16, col="dodgerblue", xlab="Distancia", ylab="Velocidad")
abline(hmod)
plot(hmod, which=1, pch=16, col="darkgreen")


X11(15,15)
par(oma=c(0,1,0,1))
plot(velocity ~ distance, data=hubble, pch=16, col="dodgerblue", xlab="Distancia", ylab="Velocidad")
abline(hmod, col="red")

X11(15,15)
par(oma=c(0,1,0,1))
layout(matrix(1:6, ncol=3, nrow=2))
plot(hmod, which=1, pch=16, col="darkgreen")
plot(hmod, which=2, pch=16, col="darkgreen")
plot(hmod, which=3, pch=16, col="darkgreen")
plot(hmod, which=4, pch=16, col="darkgreen")
plot(hmod, which=5, pch=16, col="darkgreen")
plot(hmod, which=6, pch=16, col="darkgreen")

Mpc <- 3.09 * 10^19
ysec <- 60^2 * 24 * 366.25
Mpcyear <- Mpc / ysec
1 / (coef(hmod) / Mpcyear)


#### Sub-section 6.4.2 - Cloud seeding ####

data("clouds", package="HSAUR3")

clouds <- rename(clouds, cloudc=cloudcover, prewet=prewetness, EM=echomotion, rain=rainfall)

X11(15,15)
par(oma=c(0,1,0,1))
layout(matrix(1:2, ncol=2))
bxpseeding <- boxplot(rain ~ seeding, data=clouds, ylab="Rainfall", xlab="Seeding")
bxpecho <- boxplot(rain ~ EM, data=clouds, ylab="Rainfall", xlab="Echo Motion")

X11(15,15)
par(oma=c(0,1,0,1))
layout(matrix(1:4, nrow=2))
plot(rain ~ time, data=clouds, pch=16, col="red")
plot(rain ~ cloudc, data=clouds, pch=16, col="blue")
plot(rain ~ sne, data=clouds, pch=16, col="darkgreen", xlab="S-Ne criterion")
plot(rain ~ prewet, data=clouds, pch=16, col="orange")

rownames(clouds)[clouds$rain %in% c(bxpseeding$out, bxpecho$out)]

clouds.formula <- rain ~ seeding + seeding:(sne + cloudc + prewet + EM) + time
Xstar <- model.matrix(clouds.formula, data=clouds)
class(Xstar)

clouds.lm <- lm(clouds.formula, data=clouds)
class(clouds.lm)

summary(clouds.lm)
betastar <- coef(clouds.lm); betastar

Vbetastar <- vcov(clouds.lm); Vbetastar

sqrt(diag(Vbetastar))


X11(15,15)
par(oma=c(0,1,0,1))
psymb <- as.numeric(clouds$seeding)
pcol <- ifelse(as.numeric(clouds$seeding) == 1, "red", "blue")
plot(rain ~ sne, data=clouds, pch=psymb, col=pcol, xlab="S-Ne criterion", ylab="Rainfall")
abline(lm(rain ~ sne, data=clouds, subset=seeding == "no"), col="red")
abline(lm(rain ~ sne, data=clouds, subset=seeding == "yes"), lty=2, col="blue")
legend("topright", legend=c("No seeding", "Seeding"), pch=1:2, lty=1:2, bty="n", col=c("red", "blue"))

resi <- residuals(clouds.lm)
fit <- fitted(clouds.lm)

X11(15,15)
par(oma=c(0,1,0,1))
plot(fit, resi, xlab="Fitted values", ylab="Residuals", type="n", ylim=max(abs(resi))*c(-1,1))
abline(h=0, lty=2)
for (i in 1:length(resi)) {text(x=fit[i], y=resi[i], labels=names(resi)[i])}


X11(15,15)
par(oma=c(0,1,0,1))
layout(matrix(1:6, ncol=3, nrow=2, byrow=TRUE))
plot(clouds.lm, which=1, pch=16, col="darkgreen")
plot(clouds.lm, which=2, pch=16, col="darkgreen")
plot(clouds.lm, which=3, pch=16, col="darkgreen")
plot(clouds.lm, which=4, pch=16, col="darkgreen")
plot(clouds.lm, which=5, pch=16, col="darkgreen")
plot(clouds.lm, which=6, pch=16, col="darkgreen")

################################
#### FIN DE LA PROGRAMACI?N ####
################################
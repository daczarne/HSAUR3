##########################################
#### CHAPTER 5 - ANALYSIS OF VARIANCE ####
##########################################

y ~ a + b               # Formula con a y b
y ~ a + b + a:b         # Formula con a, b, y ab
y ~ a * b               # Formula con a, b, y ab
y ~ a + b + a:b - 1     # Formula con a, b, y ab, sin constante

#### Sub-section 5.3.1 - Weight gain in rats ####

data("weightgain", package="HSAUR3")

tapply(weightgain$weightgain, list(weightgain$source, weightgain$type), mean)
tapply(weightgain$weightgain, list(weightgain$source, weightgain$type), sd)

X11(15,15)
par(oma=c(0,1,0,1))
plot.design(weightgain, main="Medias por nivel de factor", ylab="Media")

wg.aov <- aov(weightgain ~ source * type, data=weightgain)
summary(wg.aov)

summary(aov(weightgain ~ type * source, data=weightgain))

X11(15,15)
par(oma=c(0,1,0,1))
interaction.plot(weightgain$type, weightgain$source, weightgain$weightgain, col=c("red", "blue"),
                 ylab="Mean of weight gain", xlab="Type of diet", legend=FALSE)
legend("topright", legend=c("Cereal", "Beef"), col=c("blue", "red"), lty=c(1,2))

coef(wg.aov)

options("contrasts")

coef(aov(weightgain ~ source + type + source:type, data=weightgain, 
         contrasts=list(source=contr.sum)))

#### Sub-section 5.3.2 - Foster feeding of rats of different genotype ####

data("foster", package="HSAUR3")

X11(15,15)
par(oma=c(0,1,0,1))
plot.design(foster, ylab="Peso medio")

summary(aov(weight ~ motgen * litgen, data=foster))
summary(aov(weight ~ litgen * motgen, data=foster))

foster.aov <- aov(weight ~ litgen * motgen, data=foster)

foster.hsd <- TukeyHSD(foster.aov, "motgen"); foster.hsd

X11(15,15)
par(oma=c(0,1,0,1))
plot(foster.hsd)

#### Sub-section 5.3.3 - Water hardness and mortality

data("water", package="HSAUR3")

summary(manova(cbind(hardness,mortality) ~ location, data=water), test="Hotelling-Lawley")

tapply(water$hardness, water$location, mean)
tapply(water$mortality, water$location, mean)

north <- filter(water, location == "North")
south <- filter(water, location == "South")

var.test(north$mortality, south$mortality, ratio=1)

################################
#### FIN DE LA PROGRAMACI?N ####
################################
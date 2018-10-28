############################################################
#### CHAPTER 2 - DATA ANALYSIS USING GRAPHICAL DISPLAYS ####
############################################################

library(HSAUR3)
library(dplyr)

library(sp)
library(maps)
library(maptools)
library(lattice)

data("USmelanoma", package="HSAUR3")
str(USmelanoma)
head(USmelanoma)

#### Section 2.3 - Analysis using R ####

# Creates a vector of length two which will be passed to the xlim argument. This
# will ensure that both x-axis are the same.
xr = range(USmelanoma$mortality) * c(0.9,1.1)

# Figure 2.1: histogram and bow plot of mortality
X11(15,15)
layout(matrix(1:2, nrow=2))
par(mar=par("mar")*c(0.8,1,1,1))
boxplot(USmelanoma$mortality, ylim=xr, horizontal=TRUE, xlab="Mortality")
hist(USmelanoma$mortality, xlim=xr, xlab="", main="", axes=FALSE, ylab="")
axis(1)

# Figure 2.2: boxplot of mortality by ocean (costal/not costal)
# "ocean" is a factor with two levels: "no" & "yes".
# The "relationship" between the two variables must be specified via formula.
X11(15,15)
plot(mortality ~ ocean, data=USmelanoma, xlab="Contiguity to an ocean", ylab="Mortality")

# Figure 2.3: density kernels for mortality by ocean.
# First two objects of class density are created: dyes is the density for 
# ocean="yes" states, and dno is the density for the ocean="no" states.

dyes <- with(USmelanoma, density(mortality[ocean=="yes"]))
dno <- with(USmelanoma, density(mortality[ocean=="no"]))

# objects dyes and dno are basically lists. Their first two slots contain 
# numerical vectors (x and y) with the n coordinates of points where the density
# was estimated. bw contains the bandwidth with which it was estimated. n=512 is
# the default value, but can be changed. The type of kernel can also be
# chosen being "gaussian" the default.

# The "with" function is used here simply to specified the object containing the
# variables. The same could be achieved by the following:

dyes <- density(USmelanoma$mortality[USmelanoma$ocean=="yes"])
dno <- density(USmelanoma$mortality[USmelanoma$ocean=="no"])

X11(15,15)
plot(dyes, lty=1, xlim=xr, main="", ylim=c(0,0.018), xlab="Mortality")
lines(dno, lty=2)
legend("topleft", lty=1:2, legend=c("Costal State", "Land State"), bty="n")

# Figure 2.4: scatter plots of mostality by longitude and latitude.
X11(15,15)
layout(matrix(1:2, ncol=2))
plot(mortality ~ longitude, data=USmelanoma, ylab="Mortality", xlab="Longitude")
plot(mortality ~ latitude, data=USmelanoma, ylab="Mortality", xlab="Latitude")

# Figure 2.5: scatter plot of mortality vs latitude color and shape coded by
# ocean.
X11(15,15)
par(oma=c(0,1,0,1))
plot(mortality ~latitude, data=USmelanoma, col=c("red","blue")[ocean], 
     pch=(1:2)[ocean], ylab="Mortality", xlab="Latitude")
legend("topright", legend=c("Land State", "Costal State"), bty="n",
       col=c("red","blue"), pch=c(1:2))

# The highest mortality rates can be observed for the south coastal states with
# latitude les than 32.
subset(USmelanoma, latitude < 32)

# What follows will create a color coded map of the US. The color for each state
# represents the mortality rate in the melanoma data set. For this we will use
# the sp, maps, and maptools libraries.

# Object "states" is of class map. The "map" function creates polygons based on
# pre-stored data. Only a few maps are available via the map function, but more
# can be added using other functions in this library. Also, maps from other
# packages may be attached. The map to be used is selected via the database 
# argument ("wolrd" is the default). By default plot=TRUE will create a plot.
# The fill argument will tell R whether to draw lines or fill areas
states <- map(database="state", plot=FALSE, fill=TRUE)

# Object class map is just a special kind of list. It has four components. 
# x & y are numeric vectors of length 15.599. They contain lat and long values
# for the map polygon. These vectors may contain coordinates for diferent 
# polygons. If this is the case, the different polygons lat and long will be 
# separated by NAs. range will disply min and max (the range) for each vector.
# names is a character vector specifing the names of the different polygons.

# In order to make sure each rate is ploted to the corresponding state, the 
# names in the polygon must match thoes in the data set. IDs object is simply
# a list of the polygon names in the states object. Then lowercase state names
# are passed as rownames to the USmelanoma data set so that both will match.
IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
rownames(USmelanoma) <- tolower(rownames(USmelanoma))

# Now both objects can be merged in a SpatialPolygonDataFrame object. First we
# create a SpatialPolygon object from the map in WGS84 reference system, and
# then we merge the polygons. 
us1 <- map2SpatialPolygons(map=states, IDs=IDs, 
                        proj4string=CRS(projargs="+proj=longlat +datum=WGS84"))

# us1 is an object of class SpatialPolygon. The first argument must be an 
# object of class map. The IDs argument is a character vector of IDs. Duplicates
# will be combined to a single output. Integers may also be provided.
# proj4string must be an object of class "CRS"; holding a valid proj4 string.
# CRS stands for Coordinate Reference System. PROJ4 is a library for 
# conversions between cartographic projections. So the projargs are telling R
# that the data should be transformes from WGS84 (GPS data), to a longitud 
# latitud cartesian projection. WGS84 (which stands for World Geodetic System 
# 1984) is the geopositioning reference system for GPS which allows to locate
# any point on Earth using an ellipsoid as projection canvas (This is done this
# way since the Earth is no exactlly round). Therefore, the proj4string argument
# is simply an instruction, the data must be passed via de map argument.

# So to sum up: the whole idea behind this nomenclature is to use GPS 
# coordinates (which are easily obtained) to a cartesian projection. 

us2 <- SpatialPolygonsDataFrame(us1, USmelanoma)

# us2 is an object of class SpatialPolygonDataFrame. This is just a special 
# kind of data frame. We can access the data stored in the data frame the same
# way we would do with any other data frame, by simply calling the data using
# the $ operator. 

# What makes this data frame special is that it not only containg the data 
# itself, but it also holds all the information regarding how the data should be
# ploted into a map, plus al the information about the map.

# A character vector with the names of the attributes in us2 can be accessed by:
names(attributes(us2))

# Because SpatialPolygonsDataFrame objects are an S4 method, the metadata must
# be accessed using @ (all sp methods share this characteristic).
us2@proj4string

# Also, since the object is just a special kind of dataframe, we might be 
# interested in looking at all the information about one observation. This can
# be done simply by:
us2["arizona",]

# The first slot contains the row vector of information for arizona from the 
# original data frame. This information can be accessed directly by:
us2["arizona",]@data

# Next is the polygon data:
us2["arizona",]@polygons
# This is just a list:
class(us2["arizona",]@polygons)
# This list contains the information to process the map

# Now we can create the map with the spplot function as follows.

# Figure 2.6
X11(15,15)
spplot(us2, "mortality", col.regions=rev(grey.colors(100)))
# Other color combinations can be used:
spplot(us2, "mortality", col.regions=rev(heat.colors(100)))
spplot(us2, "mortality", col.regions=rev(topo.colors(100)))
spplot(us2, "mortality", col.regions=cm.colors(100))
spplot(us2, "mortality", col.regions=rev(terrain.colors(100)))



data("CHFLS", package="HSAUR3")

# Figure 2.7
X11(15,15)
barplot(xtabs(~ R_happy, data=CHFLS))

xtabs(~ R_happy + R_health, data=CHFLS)

# Figure 2.8
X11(15,15)
par(oma=c(0,1,0,1))
plot(R_happy ~ R_health, data=CHFLS, col=rev(heat.colors(4)), ylab="Happiness", xlab="Health")

# Figure 2.9
X11(15,15)
layout(matrix(1:2, ncol=2))
plot(R_happy ~ log(R_income + 1), data=CHFLS, col=cm.colors(5), ylab="Happines", xlab="log(Income + 1)")
cdplot(R_happy ~ log(R_income + 1), data=CHFLS, col=cm.colors(5), ylab="Happines", xlab="log(Income + 1)")

# Figure 2.10
X11(15,15)
xyplot(jitter(log(R_income + 0.5)) ~ jitter(log(A_income + 0.5)) | R_edu, data=CHFLS, pch=19, col=rgb(0.8, 0, 0, 0.1), ylab="log(Wife's income + 0.5)", xlab="log(Husband's income + 0.5)")

###################
#### EXERCISES ####
###################

#### EX 2.1 ####

data("household", package="HSAUR3")

household <- household %>% mutate(total=housing+food+goods+service)

library(ggplot2)
source("C:/Users/dacza/Dropbox/Funciones R/multiplot.r")

p1 <- ggplot(data=household, mapping=aes(log(total), log(housing))) +
      geom_point(colour="red") +
      facet_grid(gender ~ .)
p2 <- ggplot(data=household, mapping=aes(log(total), log(food))) +
      geom_point(colour="blue") +
      facet_grid(gender ~ .)
p3 <- ggplot(data=household, mapping=aes(log(total), log(goods))) +
      geom_point(colour="green3") +
      facet_grid(gender ~ .)
p4 <- ggplot(data=household, mapping=aes(log(total), log(service))) +
      geom_point(color="orange") +
      facet_grid(gender ~ .)

multiplot(p1,p2,p3,p4, cols=2)


################################
#### FIN DE LA PROGRAMACI?N ####
################################
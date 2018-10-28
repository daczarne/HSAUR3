##########################################
#### CHAPTER 1 - AN INTRODUCTION TO R ####
##########################################

library(HSAUR3)
library(dplyr)

#### Section 1.4 - Data Objects in R ####

data("Forbes2000", package="HSAUR3")

print(Forbes2000)
str(Forbes2000)
class(Forbes2000)
dim(Forbes2000)
nrow(Forbes2000)
ncol(Forbes2000)
names(Forbes2000)

class(Forbes2000[,"rank"])
length(Forbes2000[,"rank"])

x=1:3
x=c(1,2,3)
x=seq(from=1, to=3, by=1)

class(Forbes2000[,"name"])
length(Forbes2000[,"name"])

Forbes2000[,"name"][1]

class(Forbes2000[,"category"])
nlevels(Forbes2000[,"category"])
levels(Forbes2000[,"category"])
table(Forbes2000[,"category"])


class(Forbes2000[,"sales"])
median(Forbes2000[,"sales"])
mean(Forbes2000[,"sales"])
range(Forbes2000[,"sales"])
summary(Forbes2000[,"sales"])

#### Section 1.5 - Data Import and Export ####

csvForbes2000 = read.table("Forbes2000.csv", header=TRUE, sep=",", row.names=1)
class(csvForbes2000[,"name"])

classes=c("character", "integer", "character", "factor", "factor", "numeric", "numeric", "numeric", "numeric")

csvForbes2000 = read.table("Forbes2000.csv", header=TRUE, sep=",", row.names=1, colClasses=classes)

class(csvForbes2000[,"name"])

all.equal(csvForbes2000, Forbes2000)
length(classes)
class(classes)

write.table(Forbes2000, file="Forbes2000.csv", sep=",", col.names=NA)
save(Forbes2000, file="Forbes2000.rda")
list.files(pattern="\\.rda")
load("Forbes2000.rda")

#### Section 1.6 - Basic Data Manipulation ####

companies = Forbes2000[,"name"]
companies[1]
companies[1:3]
companies[-(4:2000)]

Forbes2000[1:3, c("name","sales","profits","assets")]

companies = Forbes2000$name

order_sales = order(Forbes2000$sales)
companies[order_sales[1:3]]
Forbes2000[order_sales[c(2000:1998)],c("name","sales","profits","assets")]

Forbes2000[Forbes2000$assets>1000,c("name","sales","profits","assets")]
table(Forbes2000$assets>1000)

na_profits = is.na(Forbes2000$profits)
table(na_profits)

Forbes2000[na_profits, c("name","sales","profits","assets")]

table(complete.cases(Forbes2000))

UKcomp = subset(Forbes2000, country=="United Kingdom")
summary(UKcomp)
str(UKcomp)

#### Section 1.7 - Computing with Data ####

summary(Forbes2000)
class(Forbes2000$category)

lapply(Forbes2000, summary)

mprofits = tapply(Forbes2000$profits, Forbes2000$category, median, na.rm=TRUE)

median(Forbes2000$profits)
median(Forbes2000$profits, na.rm=TRUE)

class(mprofits)
sort(mprofits)
rev(sort(mprofits))[1:3]

iqr = function(x){
      q = quantile(x, prob=c(0.25,0.75), names=FALSE)
      return(diff(q))
}

class(iqr)

xdata = rnorm(100)
iqr(xdata)
IQR(xdata)

xdata[1] = NA
iqr(xdata)

iqr = function(x, ...){
      q = quantile(x, prob=c(0.25,0.75), names=FALSE, ...)
      return(diff(q))
}

iqr(xdata, na.rm=TRUE)
IQR(xdata, na.rm=TRUE)


iqr(Forbes2000$profits, na.rm=TRUE)

iqr_profits <- tapply(Forbes2000$profits, Forbes2000$category, iqr, na.rm=TRUE)

levels(Forbes2000$category)[which.min(iqr_profits)]
levels(Forbes2000$category)[which.max(iqr_profits)]

bcat = Forbes2000$category
iqr_profits2 = numeric(nlevels(bcat))
names(iqr_profits2) = levels(bcat)
for(cat in levels(bcat)){
      catprofit = subset(Forbes2000, category == cat)$profit
      this_iqr = iqr(catprofit, na.rm=TRUE)
      iqr_profits2[levels(bcat) == cat] = this_iqr
}

X11(15,15)
layout(matrix(1:2, nrow=2))
hist(Forbes2000$marketvalue)
hist(log(Forbes2000$marketvalue))

fm  <- marketvaule ~ sales
class(fm)

X11(15,15)
plot(marketvalue ~ sales, data=Forbes2000, pch=".")

X11(15,15)
plot(log(marketvalue) ~ log(sales), data=Forbes2000, pch=".")

X11(15,15)
plot(marketvalue ~ sales, data=Forbes2000, pch=16, col=rgb(0,0,0,0.1))

X11(15,15)
plot(log(marketvalue) ~ log(sales), data=Forbes2000, pch=16, col=rgb(0,0,1,0.25))
abline(h=0, lty=2, col="grey")
abline(v=0, lty=2, col="grey")

X11(15,15)
plot(log(marketvalue) ~ log(sales), data=Forbes2000, pch=16, col=rainbow(6))
abline(h=0, lty=2, col="grey")
abline(v=0, lty=2, col="grey")

tmp <- subset(Forbes2000, country %in% c("United Kingdom","Germany","India","Turkey"))
tmp$country <- tmp$country[, drop=TRUE]

X11(15,15)
plot(log(marketvalue) ~ country, data=tmp, ylab="log(maketvalue", varwidth=TRUE)

###################
#### EXERCISES ####
###################

#### EX 1.1 ####

# Option 1

UScomp <- subset(Forbes2000, country=="United States")
median(UScomp$profits, na.rm=TRUE)

UKcomp <- subset(Forbes2000, country=="United Kingdom")
median(UKcomp$profits, na.rm=TRUE)

FRcomp <- subset(Forbes2000, country=="France")
median(FRcomp$profits, na.rm=TRUE)

GRcomp <- subset(Forbes2000, country=="Germany")
median(GRcomp$profits, na.rm=TRUE)

# Option 2

median(Forbes2000$profits[Forbes2000$country=="United States"], na.rm=TRUE)

countries=c("United States", "United Kingdom", "France", "Germany")
for(i in countries){
      me <- median(Forbes2000$profits[Forbes2000$country==i], na.rm=TRUE) 
      print(i); print(me)
}

#### EX 1.2 ####

filter(Forbes2000, country=="Germany" & profits < 0)[,"name"]

#### EX 1.3 ####

filter(Forbes2000, country=="Bermuda")
table(filter(Forbes2000, country=="Bermuda")$category)

#### EX 1.4 ####

highsales <- dplyr::arrange(Forbes2000, desc(sales))[1:50,]

X11(15,15)
par(oma=c(1,1,1,1))
plot(log(sales) ~ log(assets), col="red", pch=20, data=highsales)
for(i in 1:dim(highsales)[1]){text(y=jitter(log(highsales$sales[i])),
                                   x=jitter(log(highsales$assets[i])),
                                   labels=abbreviate(highsales$country[i]))}

#### EX 1.5 ####

Forbes2000 %>% group_by(country) %>% summarise(avsales=mean(sales, na.rm=TRUE))
Forbes2000 %>% filter(profits > 5) %>% group_by(country) %>% summarise(obs=n()) 

#### EX 1.6 ####

unique((Forbes2000 %>% filter(country == "United Kingdom"))$category)

#### EX 1.7 ####

ukgr <- Forbes2000 %>% filter(country=="United Kingdom" | country=="Germany")

X11(15,15)
par(oma=c(1,1,1,1))
plot(log(sales) ~ log(marketvalue), data=ukgr, pch=c(11,12), col=c("red","blue"))

#### EX 1.8 ####

uktop10 <- Forbes2000 %>% filter(country=="United Kingdom") %>% arrange(desc(profits))
uktop10 <- uktop10[1:10,]

X11(15,15)
par(oma=c(1,1,1,1))
barplot(uktop10$profits, names.arg=abbreviate(uktop10$name))

#### EX 1.9 ####

table(((Forbes2000 %>% arrange(marketvalue))[1:20,])$country)[c("United States","United Kingdom")]

#### EX 1.10 ####

ger3 <- Forbes2000 %>% filter(assets > 3)
dim(ger3)[1]

X11(15,15)
par(oma=c(1,1,1,1))
hist(ger3$profits)

as.character(ger3[which.max(ger3$profits),"category"])

################################
#### FIN DE LA PROGRAMACI?N ####
################################
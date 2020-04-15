help("median")
require(lattice)
?example()

example(histogram)
?rep()
?c()
?seq()

x = c(8, 6, 7, 5, 3, 0, 9)
x

names = c("Owen", "Luke", "Anakin", "Leia", "Jacen", "Jaina")
names

heartDeck = c(rep(1, 13), rep(0, 39))
heartDeck

y = seq(7, 41, 1.5)
y


?data()
?dim()
?names()
?View()
?str()


data(iris)
names(iris)
dim(iris)
str(iris)
View(iris)

?rm()
data(iris)
data(faithful)
data(Puromycin)
data(LakeHuron)
ls()
rm(faithful)
ls()

?hist()
data(faithful)
hist(faithful$eruptions)
hist(faithful$eruptions, n=15)
hist(faithful$eruptions, breaks=seq(1.5,5.25,.25), col="red")
hist(faithful$eruptions, freq=F, n=15, main="Histogram of Old Faithful Eruption Times", xlab="Duration (mins)")

?library()

library(abd)
require(lattice)


?histogram()

require(lattice)
data(iris)
histogram(iris$Sepal.Length, breaks=seq(4,8,.25))
histogram(~ Sepal.Length, data=iris, main="Iris Sepals", xlab="Length") histogram(~ Sepal.Length | Species, data=iris, col="red") histogram(~ Sepal.Length | Species, data=iris, n=15, layout=c(1,3))

?read.csv

As.in.H2O = read.csv("http://www.calvin.edu/~scofield/data/comma/arsenicInWater.csv")
As.in.H2O

?read.table()

senate = read.table("http://www.calvin.edu/~scofield/data/tab/rc/senate99.dat", sep="\t", header=T)
senate

?mean()
?median()
?summary
?vars
?sd
?quantile

counties=read.csv("http://www.calvin.edu/~stob/data/counties.csv")
names(counties)
x = counties$LandArea
mean(x, na.rm = T)
median(x, na.rm = T)
summary(x)
sd(x, na.rm = T)
var(x, na.rm = T)
quantile(x, probs=seq(0, 1, .2), na.rm=T)

?sum

firstTwentyIntegers = 1:20
sum(firstTwentyIntegers)

die = 1:6
manyRolls = sample(die, 100, replace=T)
sixFreq = sum(manyRolls == 6)
sixFreq / 100 

?stem
monarchs = read.csv("http://www.calvin.edu/~scofield/data/comma/monarchReigns.csv")
stem(monarchs$years)

?table
?mosaicplot
?cut

pol = read.csv("http://www.calvin.edu/~stob/data/csbv.csv")
table(pol$sex)

table(pol$sex, pol$Political04)

xtabs(~sex, data=pol)

xtabs(~Political04 + Political07, data=pol)
mosaicplot(~Political04 + sex, data=pol)

monarchs = read.csv("http://www.calvin.edu/~scofield/data/comma/monarchReigns.csv")
table(monarchs$years)

xtabs(~years, data=monarchs)

cut(monarchs$years, breaks=seq(0,65,5))

table(cut(monarchs$years, breaks=seq(0,65,5)))
fiveYrLevels = cut(monarchs$years, breaks=seq(0,65,5))
xtabs(~fiveYrLevels)

?barplot
pol = read.csv("http://www.calvin.edu/~stob/data/csbv.csv")
barplot(table(pol$Political04), main="Political Leanings, Calvin Freshman 2004")
barplot(table(pol$Political04), horiz=T)
barplot(table(pol$Political04),col=c("red","green","blue","orange"))
barplot(table(pol$Political04),col=c("red","green","blue","orange"),names=c("Conservative","Far Right","Liberal","Centrist"))

barplot(xtabs(~sex + Political04, data=pol), legend=c("Female","Male"), beside=T)

?boxplot
data(iris)
boxplot(iris$Sepal.Length)
boxplot(iris$Sepal.Length, col="yellow") boxplot(Sepal.Length ~ Species, data=iris)
boxplot(Sepal.Length ~ Species, data=iris, col="yellow", ylab="Sepal length",main="Iris Sepal Length by Species")
?plot

data(faithful)
plot(waiting~eruptions,data=faithful)
plot(waiting~eruptions,data=faithful,cex=.5)
plot(waiting~eruptions,data=faithful,pch=6) 
plot(waiting~eruptions,data=faithful,pch=19)
plot(waiting~eruptions,data=faithful,cex=.5,pch=19,col="blue")
plot(waiting~eruptions, data=faithful, cex=.5, pch=19, col="blue", main="Old Faithful Eruptions", ylab="Wait time between eruptions", xlab="Duration of eruption")

?sample
sample(c("Heads","Tails"), size=1)
sample(c("Heads","Tails"), size=10, replace=T)
sample(c(0, 1), 10, replace=T)
sum(sample(1:6, 2, replace=T))
sample(c(0, 1), prob=c(.25,.75), size=10, replace=T)

sample(c(rep(1,13),rep(0,39)), size=5, replace=F)


?replicate
sample(c("Heads","Tails"), 2, replace=T)
replicate(5, sample(c("Heads","Tails"), 2, replace=T))
ftCount = replicate(100000, sum(sample(c(0, 1), 10, rep=T, prob=c(.6, .4))))
hist(ftCount, freq=F, breaks=-0.5:10.5, xlab="Free throws made out of 10 attempts",
       main="Simulated Sampling Dist. for 40% FT Shooter", col="green")


?dbinom
?pbinom

dbinom(0, 5, .5)
dbinom(0:5, 5, .5)
sum(dbinom(0:2, 5, .5))
pbinom(2, 5, .5)

flip5 = replicate(10000, sum(sample(c("H","T"), 5, rep=T)=="H"))
table(flip5) / 10000 # distribution (simulated) of count of heads in 5 flips
binom.test(29, 200, .21)
prop.test(29, 200, .21)
?pchisq
?qchisq
?chisq.test

1 - pchisq(3.1309, 5) # gives P-value associated with X-squared stat 3.1309 when df=5 
pchisq(3.1309, df=5, lower.tail=F) # same as above

qchisq(c(.001,.005,.01,.025,.05,.95,.975,.99,.995,.999), 2) # gives critical values like Table A

qchisq(c(.999,.995,.99,.975,.95,.05,.025,.01,.005,.001), 2, lower.tail=F) # same as above

observedCounts = c(35, 27, 33, 40, 47, 51)
claimedProbabilities = c(.13, .13, .14, .16, .24, .20)
chisq.test(observedCounts, p=claimedProbabilities) # goodness-of-fit test, assumes df = n-1

?addmargins
blood = read.csv("http://www.calvin.edu/~scofield/data/comma/blood.csv")
t = table(blood$Rh, blood$type)
addmargins(t)

addmargins(t, 1)
addmargins(t, 2)

?prop.table

smoke = matrix(c(51,43,22,92,28,21,68,22,9),ncol=3,byrow=TRUE)
colnames(smoke) = c("High","Low","Middle")
rownames(smoke) = c("current","former","never")
smoke = as.table(smoke)
smoke

summary(smoke)
prop.table(smoke)
prop.table(smoke, 1)

?par

par(mfrow = c(1,2))	# set figure so next two plots appear side-by-side
poisSamp = rpois(50, 3)	# Draw sample of size 50 from Pois(3)
maxX = max(poisSamp)	# will help in setting horizontal plotting region
hist(poisSamp, freq=F, breaks=-.5:(maxX+.5), col="green", xlab="Sampled values")
plot(0:maxX, dpois(0:maxX, 3), type="h", ylim=c(0,.25), col="blue", main="Probabilities for Pois(3)")
       
barplot(smoke,legend=T,beside=T,main='Smoking Status by SES')

?fisher.test
blood = read.csv("http://www.calvin.edu/~scofield/data/comma/blood.csv")
tblood = xtabs(~Rh + type, data=blood)
tblood	# contingency table for blood type and Rh factor
chisq.test(tblood)
fisher.test(tblood)
?dpois
?ppois

dpois(2:7, 4.2) # probabilities of 2, 3, 4, 5, 6 or 7 successes in Pois(4.211)
ppois(1, 4.2) # probability of 1 or fewer successes in Pois(4.2); same as sum(dpois(0:1, 4.2)) 
1 - ppois(7, 4.2) # probability of 8 or more successes in Pois(4.2) 

?pnorm
pnorm(17, 19, 3)
qnorm(c(.95, .975, .995))
nSamp = rnorm(10000, 7, 1.5)
hist(nSamp, freq=F, col="green", main="Sampled values and population density curve")
xs = seq(2, 12, .05)
lines(xs, dnorm(xs, 7, 1.5), lwd=2, col="blue")

?qt
qt(c(.95, .975, .995), df=9)
pt(-2.1, 11)
tSamp = rt(50, 11)
xs = seq(-5,5,.01)
plot(xs, dnorm(xs), type="l", lwd=2, col="black", ylab="pdf values",
     main="Some t dists alongside standard normal curve")

lines(xs, dt(xs, 1), lwd=2, col="blue")
lines(xs, dt(xs, 4), lwd=2, col="red")
lines(xs, dt(xs, 10), lwd=2, col="green")
legend("topright",col=c("black","blue","red","green"),
         legend=c("std. normal","t, df=1","t, df=4","t, df=10"), lty=1)

?by

data(warpbreaks)
by(warpbreaks$breaks, warpbreaks$tension, mean)
?t.test()
data(sleep)

?qqnorm()


t.test(extra ~ group, data=sleep)
sleepGrp1 = sleep$extra[sleep$group==1]
sleepGrp2 = sleep$extra[sleep$group==2]
t.test(sleepGrp1, sleepGrp2, conf.level=.99)



qqnorm(precip, ylab = "Precipitation [in/yr] for 70 US cities", pch=19, cex=.6)
qqline(precip)	# Is this line helpful? Is it the one you would eyeball?

?power.t.test()
power.t.test(n=20, delta=.1, sd=.4, sig.level=.05) # tells how much power at these settings
power.t.test(delta=.1, sd=.4, sig.level=.05, power=.8) # tells sample size needed for desired power         

?anova()
require(lattice)
require(abd)
data(JetLagKnees)
xyplot(shift ~ treatment, JetLagKnees, type=c('p','a'), col="navy", pch=19, cex=.5)
anova( lm( shift ~ treatment, JetLagKnees ) )

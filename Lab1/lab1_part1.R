days = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")
temp = c(28, 30.5, 32, 31.2, 29.3, 27.9, 26.4)
snowed = c(T, T, F, F, T, T, F)
help("data.frame")
RPI_Weather_Week = data.frame(days, temp, snowed)

RPI_Weather_Week
head(RPI_Weather_Week)

str(RPI_Weather_Week)

summary(RPI_Weather_Week)

RPI_Weather_Week[1,]
RPI_Weather_Week[,1]

RPI_Weather_Week[,"snowed"]
RPI_Weather_Week[,"days"]
RPI_Weather_Week[,"temp"]
RPI_Weather_Week[1:5,c("days","temp")]
RPI_Weather_Week$temp
subset(RPI_Weather_Week, subset=snowed==T)

sorted_snowed = order(RPI_Weather_Week["snowed"])

descending = order(-RPI_Weather_Week$temp)

RPI_Weather_Week[sorted_snowed,]
RPI_Weather_Week[descending,]

data1 <- read.csv(file.choose(), header=T)
EPI <- Filter(function(x){ !is.na(x) }, data1$EPI)
DALY = data1$DALY[!is.na(data1$DALY)]

filter_func = function(x){!is.na(x)}

pop = subset(data1, subset=!is.na(Population07)&!is.na(PopulationDensity))

length(EPI)

fivenum(EPI, na.rm=F)
hist(EPI, breaks=10, col="green", xlim=c(0, 100))
lines(density(EPI, bw=1))
rug(EPI)

stem(EPI)

cor(pop$Population07, pop$PopulationDensity)

plot(ecdf(EPI), do.points=F, verticals=T)

par(pty="s")
qqnorm(EPI)
qqline(EPI)

par(pty="m")
qqplot(pop$Population07, pop$PopulationDensity)
plot(pop$Population07, pop$PopulationDensity)

boxplot(pop$Population07, pop$PopulationDensity)

plot(ecdf(pop$Population07), do.points=F, verticals=T)

qqplot(EPI, DALY)
boxplot(EPI, DALY)

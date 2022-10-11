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

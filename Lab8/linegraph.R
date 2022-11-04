
library(gcookbook)
library(ggplot2)

??aes

ggplot(BOD, aes(x=Time, y=demand)) + geom_line()
head(BOD)

BOD1 = BOD

BOD1$Time = factor(BOD1$Time)

ggplot(BOD1, aes(x=Time, y=demand, group=1)) + geom_line()
ggplot(BOD1, aes(x=Time, y=demand, group=1)) + geom_line() + ylim(0, max(BOD1$demand))
ggplot(BOD1, aes(x=Time, y=demand, group=1)) + geom_line() + expand_limits(y=0)

ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + geom_point()
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + geom_point() + expand_limits(y=0)

ggplot(worldpop, aes(x=Year, y=Population)) + geom_line() + geom_point()
ggplot(worldpop, aes(x=Year, y=Population)) + geom_line() + geom_point() + scale_y_log10()
ggplot(worldpop, aes(x=Year, y=Population)) + geom_line() + geom_point() + scale_y_log10() + expand_limits(y=1)

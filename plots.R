library(dplyr)
library(ggplot2)
library(gridExtra)

# ERRORS
dat <- read.csv("error_checking_output.csv")

dat2 <- dat %>% group_by(frac, days) %>%
  summarize(avg = mean(accuracy), se = sd(accuracy)/sqrt(6)) %>% ungroup() %>%
  mutate(error = factor(frac))

pd <- position_dodge(5) # move points so they don't overlap

png("error_output.png",width=8.5,height=7.5,units="in",res=150)
ggplot(dat2, aes(x=days, y=avg, colour=error)) +
    geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=2, position=pd, size=3) +
    geom_line(position=pd) +
    geom_point(position=pd ,size=5, shape=21, fill="white", stroke=2) +
    scale_x_continuous(name="Training days", breaks=unique(dat2$days)) +
    ylab("Predictive accuracy") +
    theme_grey(base_size = 24) +
    expand_limits(y=1)
dev.off()

# GAPS
gaps <- read.csv("gap_filling_output.csv")

# mean values from each time series
mu <- rep(c(9.182572906662045, 82.94079690902352),each=5)
gaps$rmspe <- gaps$rmspe/mu
colnames(gaps)[4] <- "variable"

p1 <- ggplot(gaps, aes(x=gap, y=rmspe*100, colour=variable)) +
    geom_line() +
    geom_point(size=5, shape=21, fill="white", stroke=2) +
    scale_x_continuous(name="Gap size", breaks=unique(gaps$gap)) +
    ylab("RMSPE (%)") +
    theme_grey(base_size = 24) +
    expand_limits(y=0)

p2 <- ggplot(gaps, aes(x=gap, y=corr, colour=variable)) +
    geom_line() +
    geom_point(size=5, shape=21, fill="white", stroke=2) +
    scale_x_continuous(name="Gap size", breaks=unique(gaps$gap)) +
    ylab("Correlation") +
    theme_grey(base_size = 24) +
    expand_limits(y=0.51)

png("gap_output.png",width=8.5,height=7.5,units="in",res=150)
grid.arrange(p1,p2,ncol=1)
dev.off()

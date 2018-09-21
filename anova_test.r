library(tidyverse)
library(ggplot2)

dt= read.table("E:/20160303_Tg4510/Analysis/output_XPOW1/OUTPUT_XPOW1_2_summary.txt", header = TRUE)
attach(dt)

boxplot(theta1 ~ group, data=dt, xlim=c(-1,4), width=c(1,1))

results= aov(formula= theta1 ~ group)
summary(results)



ggplot(dt) + geom_point(mapping = aes(x = group, y = theta2))

ggplot(data = mpg) + geom_point(mapping = aes(x=displ, y=hwy, color=class))

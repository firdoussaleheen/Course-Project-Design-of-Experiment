library(xtable)
library(HH)
lattice.options(default.theme = standard.theme(color = FALSE))

setwd("E:/TEMPLE-STUDY/COURSE/Spring 2013/STAT8107/designofexpt/firdousstat")

################################################
# factor- force, depth, pot, response - number of pixel greater than ten

tispixel10 <- read.csv("tispixel10.csv",header=TRUE, stringsAsFactors=FALSE)


#three way Anova with three way interaction

tispixel10$Force <- as.factor(tispixel10$Force)
tispixel10$Depth <- as.factor(tispixel10$Depth)
tispixel10$Pot <- as.factor(tispixel10$Pot)

interaction2wt(Pixel ~ Force + Depth + Pot, data=tispixel10, rot=c(90,0),par.strip.text=list(cex=.70))


tispixel10.3aov <- aov(Pixel ~ Force * Depth * Pot, data=tispixel10)
anova(tispixel10.3aov)

#suppress just the 3-way and show all of the 2-way interactions

tispixel10.3aov <- aov(Pixel ~ (Force + Depth + Pot)^2, data=tispixel10)

anova(tispixel10.3aov)
xtable(anova(tispixel10.3aov))


################################################
# factor- size, force, depth, pot, response - number of pixel greater than ten

tispixel10wsize <- read.csv("tispixel10wsize.csv",header=TRUE, stringsAsFactors=FALSE)

# four way anova with four way interactions
tispixel10wsize$Size <- as.factor(tispixel10wsize$Size)
tispixel10wsize$Force <- as.factor(tispixel10wsize$Force)
tispixel10wsize$Depth <- as.factor(tispixel10wsize$Depth)
tispixel10wsize$Pot <- as.factor(tispixel10wsize$Pot)


interaction2wt(Pixel ~ Size + Force + Depth + Pot, data=tispixel10wsize, rot=c(90,0),par.strip.text=list(cex=.70))
tispixel10wsize.4aov <- aov(Pixel ~ Size * Force * Depth * Pot, data=tispixel10wsize)
anova(tispixel10wsize.4aov)
xtable(anova(tispixel10wsize.4aov))

tispixel10wsize.4aov <- aov(Pixel ~ (Size + Force + Depth + Pot)^2, data=tispixel10wsize)

anova(tispixel10wsize.4aov)
xtable(anova(tispixel10wsize.4aov))








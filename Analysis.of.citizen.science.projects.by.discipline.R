# Analysis of citizen science projects by discipline
# David R. Angelini, Colby College
# dave.angelini@colby.edu
# November 2, 2016
# for an invited post at Macroscope, the blog of American Scientist Online
# 
# Using R version 3.3.1

library(ggplot2)
library(RColorBrewer)
library(grDevices)

setwd("~/Desktop/AmerSci CitSci Posts")

# Project topics listed on SciStarter.org
# Subjects are not exclusive. Projects can be counted in more than one topic.
s <- read.csv("scistarter.projs.by.topic.csv.csv") 
head(s); dim(s)
# top10.counts <- order(s$count, decreasing = T)[1:10]
# s <- s[top10.counts,]; s$subject <- droplevels(s$subject); row.names(s) <- 1:10
s$rank <- c(1:dim(s)[1])

s.plot <- ggplot(s, aes(x=as.factor(rank), y=count, fill=as.factor(rank))) + 
  theme(legend.position="none",
        axis.text.x = element_text(size=12,angle=65,hjust=1,vjust=1, face="plain"),
        axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold"),
        panel.grid.major.x = element_blank(),
        axis.ticks.x = element_blank()) +
  geom_bar(stat = "identity", colour="black") +
  scale_x_discrete(labels=s$subject) +
  xlab("citizen science project topic") + ylab("projects")
s.plot
# Export as PDF 6 x 8

# Correlation between number of pubs and projects in a field?
pubs <- read.csv("citsci.citations.projects.by.topic.csv") 
head(pubs[,1:4]); dim(pubs)
plot(pubs$projects,pubs$citations)
pl <- pubs; pl[,2:3] <- log10(pl[,2:3])

shapiro.test(pl$projects) # W = 0.93144, p-value = 0.2301
shapiro.test(pl$citations) # W = 0.89772, p-value = 0.06222
cor.test(pl$projects,pl$citations) # Pearson's product-moment correlation
# t = 2.2888, df = 15, p-value = 0.03702
#   cor  0.5087626 
lm(pl$citations ~ pl$projects)
# slope = 0.7046  

dbnames <- levels(pl$database)
uniqueColors <- brewer.pal(length(dbnames),"YlOrRd")
pl$dbcolor <- NA
for (i in 1:length(dbnames)) {
  pl$dbcolor[which(pl$database==dbnames[i])] <- uniqueColors[i]
}
pl.plot <- ggplot(pl, aes(x=projects, y=citations)) + 
  theme(axis.title.x = element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14,face="bold") )+
  geom_point(size=4, color=pl$dbcolor) +
  stat_smooth(method = "lm", size = 1, se=F, color="darkred") +
  geom_text(label=pl$subject, hjust = 0, nudge_x = 0.04) +
  scale_x_continuous(limits = c(1.25,3.50), breaks=c(1:6)/2+0.5) +
  scale_y_continuous(limits = c(0.50,2.75), breaks=c(1:6)/2) +
  xlab(expression(paste(log[10]," number of citizen science projects", sep=""))) +
  ylab(expression(paste(log[10]," number of publications", sep=""))) 
pl.plot
# Export PDF at 6x6


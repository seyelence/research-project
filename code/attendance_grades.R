library(ggplot2)
library(dplyr)

d1=read.table("C:\\Users\\Pete\\Dropbox\\math345 proj\\data\\student-mat.csv",sep=";",header=TRUE)
d2=read.table("C:\\Users\\Pete\\Dropbox\\math345 proj\\data\\student-por.csv",sep=";",header=TRUE)

d3=rbind(d1,d2)

d3['final_grade'] <- 'na'
d3[(d3$G3  >= 15) & (d3$G3  <= 20), 'final_grade'] <- 'good'
d3[(d3$G3  >= 10) & (d3$G3  <= 14), 'final_grade'] <- 'average'
d3[(d3$G3  >= 0) & (d3$G3  <= 9), 'final_grade'] <- 'poor'

p <- ggplot(d3, aes(x=absences, y=G3, color=final_grade)) +
  geom_point() +
  geom_smooth(method=lm, na.rm = TRUE, fullrange= TRUE,
              aes(group=1),colour="black", se=FALSE)+
  scale_color_hue(l=75, c=35) + theme_bw() + labs(color='Final Grade') +
  xlab("Absences") + ylab("Score") +
  labs(title = "Amount of Absences with Scores")

# separated ver
q <- ggplot(d3, aes(x=absences, y=G3, color=final_grade)) +
  geom_point() + facet_wrap(~ final_grade, ncol = 3) +
  scale_color_hue(l=75, c=35) + theme_bw() +
  xlab("Absences") + ylab("Score") + theme(legend.position="none")+
  labs(title = "Amount of Absences with Scores")




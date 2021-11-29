library(readxl)
library(ggplot2)
library(modeest)

school_closures <- read_excel("C:/Users/Pete/Dropbox/math345 proj/data/school_closures.xlsx")



q <- ggplot(na.omit(school_closures), aes(x=region, y = days_fully_open, fill=income_group)) +
  geom_bar(stat="identity", position=position_dodge()) + theme_minimal()

q + scale_fill_brewer(palette="Pastel1") + coord_flip() + xlab(label = "Regions") + 
  ylab(label = "Days Fully Open at Schools") + theme(legend.position = "bottom", legend.box = "vertical") +
  labs(fill = "Income") + labs(title = "Comparison Between Regions and School Days Met in Person")

mean(school_closures$days_fully_open)


# histogram and density plot
t <- ggplot(na.omit(school_closures), aes(x = days_fully_closed)) + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "white",bins=35) +
  geom_density(alpha = .3, fill = "#B6A6D9") + xlab(label = "Count") 

t +  ylab(label = "Density") + labs(title = "School Closures Count Globally (2020-21)") + 
     theme_minimal() 
smode <-function(x){
  xtab<-table(x)
  modes<-xtab[max(xtab)==xtab]
  mag<-as.numeric(modes[1]) #in case mult. modes, this is safer
  themodes<-names(modes)
  mout<-list(themodes=themodes,modeval=mag)
  return(mout)
}

smode(school_closures$days_fully_closed)
summary(school_closures$days_fully_closed)

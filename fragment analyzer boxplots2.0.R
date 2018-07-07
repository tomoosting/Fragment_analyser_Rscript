setwd("D:\\Victoria University\\Data\\preservation_test")
getwd()
library(stringr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(gridExtra)

file <- read.csv("smear_inputfile_BP.csv", header = TRUE, check.names=FALSE)
names <- c("20-300","301-500","501-1000","1001-5000","5001-10000","10001-20000","20001-50000")
file$`fragment size range` <- factor(file$`fragment size range`,levels = names)
attach(file)

file2 <- filter(file, sample %in% str_subset(file$sample,"D[[:alnum:]][[:alnum:]]"))

ggplot(data = file2,mapping = aes(x=`fragment size range`,y=percentage, fill =`fragment size range`))+
  geom_boxplot()+
  #theme(legend.position="none")+
  coord_cartesian(ylim = c(0, 100))+
  scale_fill_discrete(breaks = names)

sets <- function(file,expr){
    
    #subsample input file based on regular expression
    subfile <- filter(file,sample %in% str_subset(file$sample,expr))
    
    #plot boxplot
    plot <- ggplot(data = subfile, mapping = aes(x=`fragment size range`,y=percentage, fill = `fragment size range`))+
      geom_boxplot()+
      scale_fill_discrete(breaks= names)+
      theme(
        legend.position=c(0.77,0.8), 
        legend.background = element_rect(fill="transparent"),
        legend.key.size = unit(0.015,"npc"),
        legend.title = element_blank(),
        axis.text.x=element_blank())+
      coord_cartesian(ylim = c(0,100))
  
  return(plot)
}

D1d <- sets(file,"D[[:alnum:]][[:alnum:]]1d")
D1w <- sets(file,"D[[:alnum:]][[:alnum:]]1w")
D2w <- sets(file,"D[[:alnum:]][[:alnum:]]2w")
D1m <- sets(file,"D[[:alnum:]][[:alnum:]]1m")
D3m <- sets(file,"D[[:alnum:]][[:alnum:]]3m")
D1d <- D1d + ggtitle("DMSO, 1 DAY")
D1w <- D1w + ggtitle("DMSO, 1 WEEK")
D2w <- D2w + ggtitle("DMSO, 2 WEEKS")
D1m <- D1m + ggtitle("DMSO, 1 MONTH")
D3m <- D3m + ggtitle("DMSO, 3 MONTHS")
grid.arrange(D1d, D1w, D2w, D1m, D3m, ncol=5)

#plot preservation ethanol over time
E1d <- sets(file,"E[[:alnum:]][[:alnum:]]1d")
E1w <- sets(file,"E[[:alnum:]][[:alnum:]]1w")
E2w <- sets(file,"E[[:alnum:]][[:alnum:]]2w")
E1m <- sets(file,"E[[:alnum:]][[:alnum:]]1m")
E3m <- sets(file,"E[[:alnum:]][[:alnum:]]3m")
E1d <- E1d + ggtitle("ETHANOL, 1 DAY")
E1w <- E1w + ggtitle("ETHANOL, 1 WEEK")
E2w <- E2w + ggtitle("ETHANOL, 2 WEEKS")
E1m <- E1m + ggtitle("ETHANOL, 1 MONTH")
E3m <- E3m + ggtitle("ETHANOL, 3 MONTHS")
grid.arrange(E1d, E1w, E2w, E1m, E3m, ncol=5)

grid.arrange(D1d, D1w, D2w, D1m, D3m, E1d, E1w, E2w, E1m, E3m, ncol=5, nrow=2)

#DMSO heat treated vs not heat treated
DH <- sets(file,"DH[[:alnum:]]")
DN <- sets(file,"DN[[:alnum:]]")
grid.arrange(DH, DN, ncol=2)

#DMSO fridge vs room temp
DR <- sets(file,"D[[:alnum:]]R")
DC <- sets(file,"D[[:alnum:]]C")
grid.arrange(DH, DN, ncol=2)

#ethanol heat treated vs not heat treated
EH <- sets(file,"EH[[:alnum:]]")
EN <- sets(file,"EN[[:alnum:]]")
grid.arrange(EH, EN, ncol=2)




#plot preservation ethanol over time
E1d <- sets(file,"E[[:alnum:]][[:alnum:]]1d")
E1w <- sets(file,"E[[:alnum:]][[:alnum:]]1w")
E2w <- sets(file,"E[[:alnum:]][[:alnum:]]2w")
E1m <- sets(file,"E[[:alnum:]][[:alnum:]]1m")
E3m <- sets(file,"E[[:alnum:]][[:alnum:]]3m")
grid.arrange(E1d, E1w, E2w, E1m, E3m, ncol=5)

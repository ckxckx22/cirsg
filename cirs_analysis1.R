#library
library(dplyr)
library(ggplot2)
library(ggpubr)
#setup
cirs3->CIRS
#1st baseline only
  CIRS[which(grepl("baseline",CIRS$event)),]->cirsbl
  cirsbl[which(!grepl("catchup",cirsbl$event)),]->cirsbl
  cirsbl[which(cirsbl$event==paste0("baseline_arm_",cirsbl$arm_num, sep="")),]->cirsbl
  #Total of baseline CIRS
  length(unique(cirsbl$masterdemoid))
  as.character(cirsbl$group)->cirsbl$group
  table(cirsbl$group)
#Number of baseline by sum
ggplot(cirsbl)+ geom_histogram(aes(x=cirsbl$cirs_sum), binwidth=1)
#Number of baseline over 3
ggplot(cirsbl)+ geom_histogram(aes(x=cirsbl$cirs_morethan3), binwidth=1)
#Number of baseline 1
ggplot(cirsbl) + geom_histogram(aes(x=cirsbl$cirsg_1_s), bins=5)->a
#number of baseline 2
ggplot(cirsbl) + geom_histogram(aes(x=cirsbl$cirsg_2_s), bins=5)->b
#number of baseline 3
ggplot(cirsbl) + geom_histogram(aes(x=cirsbl$cirsg_3_s), bins=5)->c
#number of baseline 4
ggplot(cirsbl) + geom_histogram(aes(x=cirsbl$cirsg_4_s), bins=5)->d
#number of baseline 5
ggplot(cirsbl) + geom_histogram(aes(x=cirsbl$cirsg_5_s), bins=5)->e
#number of baseline 6
ggplot(cirsbl) + geom_histogram(aes(x=cirsbl$cirsg_6_s), bins=5)->f
#number of baseline 7
ggplot(cirsbl) + geom_histogram(aes(x=cirsbl$cirsg_7_s), bins=5)->g
#number of baseline 8
ggplot(cirsbl) + geom_histogram(aes(x=cirsbl$cirsg_8_s), bins=5)->h
#number of baseline 9
ggplot(cirsbl) + geom_histogram(aes(x=cirsbl$cirsg_9_s), bins=5)->i
#number of baseline 10
ggplot(cirsbl) + geom_histogram(aes(x=cirsbl$cirsg_10_s), bins=5)->j
#number of baseline 11
ggplot(cirsbl) + geom_histogram(aes(x=cirsbl$cirsg_11_s), bins=5)->k
#number of baseline 12
ggplot(cirsbl) + geom_histogram(aes(x=cirsbl$cirsg_12_s), bins=5)->l
#number of baseline 13
ggplot(cirsbl) + geom_histogram(aes(x=cirsbl$cirsg_13_s), bins=5)->m
ggarrange(a,b,c,d,e,f,g,h,i,j,k,l,m,
          labels = c("1", "2", "3","4","5","6","7","8","9","10","11","12", "13"),
          ncol = 5, nrow = 3)

#Who has followup data and how far out (remove those without sum data)
CIRS[-which(is.na(CIRS$cirs_sum)),]->CIRS2
CIRS2$cirs_date




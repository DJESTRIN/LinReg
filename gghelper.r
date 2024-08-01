# Script Title: gghelper
# Author: DJ Estrin
# Date: 07-31-2024
# Description: Common ggplot functions for R

#Library dependencies 
library(ggplot2)
library(dplyr)
library(R6)
set.seed(123)
source("utils.r")

# LinReg class
gingestion <- R6Class(
  "gingestion",
  public = list(
    # Place holder for attributes
    data = NULL,
    y = NULL,
    x = NULL,
    group = NULL,
    summary_data=NULL,
    summary_y=NULL,
    summary_x=NULL,
    summary_error=NULL,

    #Initialization
    initialize = function(data, x, y, group, summary_data, summary_y, summary_x, summary_error) {
      self$data <- data
      self$x <- x
      self$y <- y
      self$group <- group
      self$summary_data <- summary_data
      self$summary_x <- summary_x
      self$summary_y <- summary_y
      self$summary_error <- summary_error
    },

    # Get suggested underlying package
    average_sem_bar = function() {
      plotoh <- ggplot(data = self$data, aes(x=self$x, y=self$y, fill=self$group,group=self$group))+
        geom_bar(stat="identity",color="black",size=2)+
        geom_errorbar(data=self$summary_data, aes(width=0,x=self$group, ymin=self$summary_y-self$summary_error, ymax=self$summary_y+self$summary_error), size=2)
      print(plotoh)
      ggsave(filename = "example_plot.jpg", plot = plotoh)
    }
  )
)

# Generate a large dataframe
large_df <- data.frame(
  ID = 1:10000,
  Category = sample(LETTERS[1:5], 10000, replace = TRUE),
  Subcategory = sample(paste("Sub", LETTERS[6:10]), 10000, replace = TRUE),
  Value1 = rnorm(10000, mean = 50, sd = 10),
  Value2 = rnorm(10000, mean = 30, sd = 5),
  Date = sample(seq(as.Date('2020-01-01'), as.Date('2023-01-01'), by="day"), 10000, replace = TRUE)
)

fil_df<-aggregate(data=large_df,Value1~ID+Category+Subcategory,FUN="mean")
sum_dat<-aggregate(data=fil_df,Value1~Category+Subcategory,FUN="mean")
sum_dat2<-aggregate(data=fil_df,Value1~Category+Subcategory,FUN=stderr)
sum_dat$error<-sum_dat2$Value1


p <- gingestion$new(fil_df, "Category","Subcategory","ID",fil_df,"Category","Subcategory","error")
p$average_sem_bar()

browser()


# p<-ggplot(data = LatencyBySession, aes( x =Session, y=Average))+geom_line(colour="seagreen", size=2)+
#   geom_errorbar(data=LatencyBySession, aes(width=0,x=Session, ymin=Average-SE, ymax=Average+SE), colour="seagreen", size=2)+
#   geom_point(shape=21, fill="white", stroke=2, colour="seagreen",size=10)+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black")) + theme(legend.position = "none") +
#   theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.line = element_blank()) +
#   scale_y_continuous(breaks = pretty(LatencyBySession$Average, n = 5))+
#   scale_x_continuous(breaks = pretty(LatencyBySession$Session, n = 6))+
#   theme(axis.ticks.length=unit(.5, "cm"))+theme(axis.ticks=element_line(colour = 'black', size = 2))=
#   theme(axis.text = element_blank())
# print(p)
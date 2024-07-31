p<-ggplot(data = LatencyAverages, aes( x =Sex, y=Averages, fill = Sex))+geom_bar(stat="identity", color="black",size=2)+
  geom_errorbar(data=LatencyAverages, aes(width=0,x=Sex, ymin=Averages-Error, ymax=Averages+Error), size=2)+
  scale_fill_manual("legend", values = c("Female" = "#999999", "Male" = "#56B4E9"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.line = element_blank()) +
  theme(axis.ticks.length=unit(.5, "cm"))+theme(axis.ticks=element_line(colour = 'black', size = 2))
theme(axis.text = element_blank())
print(p)

p<-ggplot(data = LatencyBySession, aes( x =Session, y=Average))+geom_line(colour="seagreen", size=2)+
  geom_errorbar(data=LatencyBySession, aes(width=0,x=Session, ymin=Average-SE, ymax=Average+SE), colour="seagreen", size=2)+
  geom_point(shape=21, fill="white", stroke=2, colour="seagreen",size=10)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.line = element_blank()) +
  scale_y_continuous(breaks = pretty(LatencyBySession$Average, n = 5))+
  scale_x_continuous(breaks = pretty(LatencyBySession$Session, n = 6))+
  theme(axis.ticks.length=unit(.5, "cm"))+theme(axis.ticks=element_line(colour = 'black', size = 2))=
  theme(axis.text = element_blank())
print(p)
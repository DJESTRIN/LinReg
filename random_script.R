################# Written By David James Estrin   ##################################################################

### Part I: Data Clean
### Part II: Analysis of Behavior  MUST SHOW WHY WE DEVIDE DATA INTO EARLY AND LATE!
### Part III: Analysis of Firing Rates

#####################################   PART I  ###############################################################################

#1) Clear ALL
rm(list=ls(all=TRUE)) # Clear all

#2) Libraries Needed for code
library(readxl) #Read in excel files
library(ggplot2) #Plotting with ggplot
library(lme4)  #for linear mixed models
library(MASS) 
library(car)
library(statmod)
library(tweedie) #may not be used
library(multcomp) #multiple comparisons
library(DistributionUtils) #May not be used
library(RUnit)
library(GeneralizedHyperbolic)
library(tidyr)
library(scatterplot3d) 
library(glmmTMB)
library(DHARMa)
library(nlme)
library(emmeans)
library(lsmeans)
library(gridExtra)
library(reshape2)
library(factoextra)
library(effsize)
library(plyr)

st.err <- function(x) {sd(x)/sqrt(length(x))} # Make function for standard error

#3) Neural Data Set: Load-in, re-arrange and clean
Neural <- read_excel("D://RESEARCH_DATA//WEST_LAB//Neural.xlsx") # Load in data


#4) Clean and relabel
Neural$Animal<-as.factor(Neural$Animal)
Neural$Abs<-abs(Neural$Firing-Neural$BaseLine) # Calculate Absoluate Firing
Neural <-  Neural[(Neural$Duration < 3 || Neural$Duration > 3 ) ,]  #Remove bad trials
Neural <- Neural[ Neural$Abs<300, ] # Remove Outlier
Neural <- Neural [ Neural$DrugLevel < 500 ,] #Remove Drug Level Outliers
Neural$Sex<-as.factor(Neural$Sex) # Make Sex a factor
levels(Neural$Sex)<-c("MALE", "FEMALE") #Change level labels
Neural$Resp<-as.factor(Neural$Resp) #Make Response a factor
levels(Neural$Resp)<-c("MISS", "HIT") #Change level labels
Neural$DrugLevelNorm<-Neural$DrugLevel + 1 # Create variable (DrugLevel +1)
Neural$NormbyDrug<-Neural$Abs/Neural$DrugLevelNorm #Normalize Absolute Firing By Drug Level
Neural$Period<-as.factor(Neural$Sess>7 )# Early or Late Periods
levels(Neural$Period) <- c("EARLY", "LATE") #Make early and late a factor
Neural$Type<-as.factor(Neural$Type) # Make type a factor
levels(Neural$Type)<-c("SHELL", "CORE", "Other1", "Other2") # Change level labels to reflect subregions
Neural<-Neural[!(Neural$Type=="Other1"),] # Remove neurons that are not core or shell
Neural<-Neural[!(Neural$Type=="Other2"),] # Remove neurons that are not core or shell
Neural<-Neural[!(Neural$Phase=="Maint"),] # Remove maintanence phase from data set.
Neural<-Neural[!(Neural$Same==2),]  # Remove Neurons that do not have sameness across sessions
names(Neural)[names(Neural) == 'GrandTrial'] <- 'TrialNum'   # Change Trial number variable name
names(Neural)[names(Neural) == 'ResponseInd'] <- 'Response' #
names(Neural)[names(Neural) == 'DrugLevel'] <- 'Drug' #
Neural$Phase<-as.factor(Neural$Phase)
levels(Neural$Phase)<-c("LOAD","PRE")
Neural$Sess<-as.factor(Neural$Sess)
Neural$TrialNum<-as.factor(Neural$TrialNum)
Neural$Response<-as.factor(Neural$Response)
Neural$Drug<-as.factor(Neural$Drug)

Neural<-Neural[,c(1,11,7,4,16,3,17, 2, 3, 5, 6, 8, 9, 10, 12,13,14,15,18,19,20,21,22)]

#5) Delete Variables that do not mean anything
DropVariables <- c("PhaseBL","Session", "Same", "Duration", "Concur", "TrialPhase")
Neural<-Neural[ , !(names(Neural) %in% DropVariables)]

#6) Rearrange the variables 
Neural<-Neural[,c(1,2,13,3,4,5,6,7,8,9,10,11,12,14,15,16,17)]
#Neural <- Neural[ -c(9) ] why is this here?
#7) Behavior Data Set: Load in, clean, rearrange
Behavior<- read_excel("D://RESEARCH_DATA//WEST_LAB//Book2.xlsx")

#8) Clean and relable
Behavior$Animal<-as.factor(Behavior$Animal)
Behavior$Sex<-as.factor(Behavior$Sex)
levels(Behavior$Sex)<-c("FEMALE", "MALE")
Behavior$Period<-as.factor(Behavior$Period)
levels(Behavior$Period)<-c("PRE", "PRE", "LOAD", "MAINT")
Behavior<-Behavior[!(Behavior$Period=="MAINT"),]
names(Behavior)[names(Behavior) == 'Period'] <- 'Phase'   #Change variable name
Behavior$Sess<-as.numeric(Behavior$Sess)
Behavior<-Behavior[!(Behavior$Sess > 14),]
Behavior$Sess<-as.factor(Behavior$Sess)
names(Behavior)[names(Behavior) == 'ResponceIndex'] <- 'Response'
names(Behavior)[names(Behavior) == 'TrialDrug'] <- 'Drug'

Behavior<-Behavior[,c(1,11,2,4,7, 8, 9,3,5,6,10,12,13,14,15,16 )]

Behavior<-droplevels(Behavior[!Behavior$Phase=='MAINT',])
Behavior$TrialNum<-as.factor(Behavior$TrialNum)
Behavior$Response<-as.factor(Behavior$Response)
Behavior$Drug<-as.factor(Behavior$Drug)

#9) Merge Data Sets & Rearrange
Neural<-data.frame(Neural)
Behavior<-data.frame(Behavior)
M<-merge(Neural, Behavior, all=TRUE,  by= c("Animal","Sex","Sess","TrialNum","Response","Phase","Drug"))#, "Sex","Sess", "TrialNum", "Response" ,"Phase", "Drug")) #Merge based on this variable
M<-M[,c(1,2,3,16,10,11,4,17,5,9,6,7,12,8,13,14,15,18,19,20,21,22,23,24,25)]

#10) Clean, Rearrange and perfect Merged data set
M$EstrousCycle<-as.factor(M$EstrousCycle) #Make Estrus a factor
levels(M$EstrousCycle)<-c("0", "Proestrus", "Estrus", "Metaestrus", "Diestrus", "NaN") #Rename levels of interest
levels(M$EstrousCycle)[levels(M$EstrousCycle)=='NaN'] <- NA #Make this data NA
levels(M$EstrousCycle)[levels(M$EstrousCycle)=='0'] <- NA #Make this data NA
M$Sess<-as.factor(M$Sess)
M$Sess <- factor(M$Sess, levels=c("1", "2", "3", "4","5", "6", "7", "8", "9", "10", "11", "12", "13", "14")) #Put Factors in correct order
index_1<-(M$Sess=="1" | M$Sess=="2" | M$Sess=="3" | M$Sess=="4" |M$Sess=="5")
index_2<-(   M$Sess=="6"| M$Sess=="7"|M$Sess=="8"| M$Sess=="9" | M$Sess=="10" | M$Sess=="11" | M$Sess=="12" | M$Sess=="13" | M$Sess=="14" | M$Sess=="15")
M$Period[index_1]<-"EARLY"
M$Period[index_2]<-"LATE"
M$Neuron<-paste(M$Animal,M$Channel)
indexna<-grepl("NA", M$Neuron, fixed = TRUE)
M$Neuron[indexna]<- NA
Relapse<-M[(M$Sess=="15"),]
M<-M[!(M$Sess=="15"),] # Remove session 15 (relapse day)
R<-M
M$Diff<-M$Firing-M$BaseLine
M$Perc<-M$Firing/(M$BaseLine+0.00000000000000000000000000000000000000000000000000000000000000000000000000000000000001)
#11) Set working directory to text files where results will be stored.
setwd("C:/Users/estrindj/Desktop/West Lab/WestLabPaper/Statistical Results/Text Files") #Working Directory where I throw results

###############################################      PART II     #########################################################################

#12) Get Latency Data
M$LatencyNA<-M$Latency #Create new variable
index5<-(M$Latency==30000) #Find non responses
M$LatencyNA[index5]<-NA #Set non responces to NA
LatencyBehy<-aggregate(LatencyNA~Animal+Sess+Sex+TrialNum, data= M, FUN = "mean") #Average Latency per animal per day dataframe
LatencyBeh<-aggregate(LatencyNA~Animal+Sess+Sex, data= LatencyBehy, FUN = "mean") #Average Latency per animal per day dataframe
LatencyBeh$Animal<-as.factor(LatencyBeh$Animal)
LatencyBeh$Sex<-as.factor(LatencyBeh$Sex)
LatencyBeh$Sess<-as.factor(LatencyBeh$Sess)


#13) Load in Estimated Marginal mean data calculated in this code
LatencyAverages<- read_excel("C:/Users/estrindj/Desktop/WestLabPaper/LatencyMarginalMeans.xlsx")
LatencyBySession<- read_excel("C:/Users/estrindj/Desktop/WestLabPaper/Statistical Results/LatencybySessionAverages.xlsx")


#14) GGPLOT Latency by Sex
p<-ggplot(data = LatencyAverages, aes( x =Sex, y=Averages, fill = Sex))+geom_bar(stat="identity", color="black",size=2)+
  geom_errorbar(data=LatencyAverages, aes(width=0,x=Sex, ymin=Averages-Error, ymax=Averages+Error), size=2)+
  scale_fill_manual("legend", values = c("Female" = "#999999", "Male" = "#56B4E9"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.line = element_blank()) +
  theme(axis.ticks.length=unit(.5, "cm"))+theme(axis.ticks=element_line(colour = 'black', size = 2))
theme(axis.text = element_blank())
print(p)

#15) GGPLOT Latency by Session
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

#16) GLM for Latency as an effect of Session and Sex
LatencyStat <- lme(LatencyNA~Sess*Sex,random=~1|Animal,data=LatencyBeh) 
sink("LatencyResults.txt")
print(cat("=============================\n"))
print(cat("Analysis of Latency"))
print(cat("=============================\n"))
print(anova(LatencyStat))
hist(residuals(LatencyStat),15) #Histogram of model residuals
LatencySes<-emmeans(LatencyStat, ~Sess)
LatencySex<-emmeans(LatencyStat, ~Sex)
print(cat("Post hoc comparisons of Session.... :"))
print(summary(LatencySes))
print(pairs(LatencySes, adjust = "fdr") )   #FDR used for many comparisons.. many of which are non sensical. 
sink()

#17) Get Weight Data
weightmat<-aggregate(Weight~Animal+Sex, data= Behavior, FUN = "mean") #Average weight per Animal
weightmat$Animal<-as.factor(weightmat$Animal)
weightmat$Sex<-as.factor(weightmat$Sex)

#18) GGPLOT Weight by sex
p<-ggplot(data=weightmat, aes(x=Sex, y=Weight))+geom_boxplot(outlier.shape=NA) + # Latency by Sex with Jitter per Animal
  geom_jitter(position=position_jitter(width=.1, height=0), aes(color=Animal))
print(p)

#19) GLM weight as an effect of Sex
sink("WeightResults.txt")
print(cat("=============================\n"))
print(cat("Analysis of Weight"))
print(cat("=============================\n"))
WeightStat<-lme(Weight~Sex, random=~1|Animal, data = weightmat)
print(anova(WeightStat))
sink()
hist(residuals(WeightStat),10) #Histogram of model residuals

#20) Get Response Data
Responser<-aggregate(Response~Animal+Sess+Sex+TrialNum, data= M, FUN = mean)
Responser$MissesLogic<-(Responser$Response==0)
Responser$MissesLogic<-as.integer(as.logical(  Responser$MissesLogic))
Response2<-aggregate(Response~Animal+Sess+Sex+TrialNum, data= Responser, FUN = "mean")
Response2<-aggregate(Response~Animal+Sess+Sex, data= Response2, FUN = sum)
print(ggplot(data=Response2, aes(x=Sess, y=Response, fill = Sex))+geom_boxplot())
ResponseMiss<-aggregate(MissesLogic~Animal+Sess+Sex, data= Responser, FUN = sum)
Response2$Ratio<-(ResponseMiss$MissesLogic/Response2$Response)
Response2$Animal<-as.factor(Response2$Animal)
Response2$Sex<-as.factor(Response2$Sex)
Response2$Sess<-as.factor(Response2$Sess)

#21) GLM for Response by Sex and Session
sink("ResponseResults.txt")
print(cat("=============================\n"))
print(cat("Analysis of Response (HITS)\n"))
print(cat("Response~Sex*Sess + (1|Animal)\n"))
print(cat("=============================\n"))
print(ResponseStat<-lme(Response~Sex*Sess, random=~1|Animal, data = Response2))
Response2$Ratiolog<-log(Response2$Ratio)
ResponseRatioStat<-lme(Ratiolog~Sess*Sex, random=~1|Animal, data = Response2)

glmmPQL(Ratiolog~Sess*Sex, random=~1|Animal, data=Response2, family=gaussian(link = "identity"))    #Delete?
GS_stat<-glmmPQL(log(TrialNum)~Sex*Period, random =~1|Animal, data = GateShot3, family=gaussian())  #Delete?

print(anova(ResponseStat))
hist(residuals(ResponseStat),50) #Histogram of model residuals
print(cat("Post hoc comparisons of Session"))
ResponseSes<-emmeans(ResponseStat, ~Sess)
ResponseSex<-emmeans(ResponseStat, ~Sex)
print(ResponseSex)
print(summary(ResponseSes))
pairs(ResponseSes, adjust = "fdr") # When is this p value adjustment necessary?
print(summary(glht(ResponseStat, linfct = mcp(Sess = "Tukey")),  test = adjusted("bonferroni")))
sink()


#22) GGPLOT of Response 
ResponseSum<-aggregate(Response~Sess+Sex, data = Response2, FUN = "mean")
ResponseSumy<-aggregate(Response~Sess+Sex, data = Response2, FUN = st.err)
ResponseSum$Error<-ResponseSumy$Response
p<-ggplot(data = Response2, aes(x=Response))+geom_histogram()
print(p)
p<-ggplot(data = ResponseSum, aes())
p<-ggplot(data = DrugAnimalSess, aes( x= DrugAnimalSess$Sess, y=Drug, fill=Sex))+geom_boxplot()
print(p)

#23) Load Estimated Marginal Means for Response by Session 
RespBySession<- read_excel("C:/Users/estrindj/Desktop/WestLabPaper/Statistical Results/ResponseSessionAverages.xlsx")
RespBySex<- read_excel("C:/Users/estrindj/Desktop/WestLabPaper/Statistical Results/ResponsebySexAverage.xlsx")


#24) GGPLOT Response EMM by Session
p<-ggplot(data = RespBySession, aes( x =Session, y=Average))+geom_line(colour="seagreen4", size=2)+
  geom_errorbar(data=RespBySession, aes(width=0,x=Session, ymin=Average-SE, ymax=Average+SE), colour="seagreen4", size=2)+
  geom_point(shape=21, fill="white", stroke=2, colour="seagreen4",size=10)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.line = element_blank()) +
  scale_y_continuous(breaks = pretty(RespBySession$Average, n = 5))+
  scale_x_continuous(breaks = pretty(RespBySession$Session, n = 6))+
  theme(axis.ticks.length=unit(.5, "cm"))+theme(axis.ticks=element_line(colour = 'black', size = 2))
#theme(axis.text = element_blank())
print(p)

#25) GGPLOT Response EMM by Sex
p<-ggplot(data=RespBySex, aes(x=Sex, y=emmean, fill=Sex))+geom_bar(stat="identity", color="black", size=2) +
  geom_errorbar(data=RespBySex, aes(width=0,x=Sex, ymin=emmean-SE, ymax=emmean+SE), size=2)+
  scale_fill_manual("legend", values = c("FEMALE" = "#999999", "MALE" = "#56B4E9"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.line = element_blank()) +
  theme(axis.ticks.length=unit(.5, "cm"))+theme(axis.ticks=element_line(colour = 'black', size = 2))+
  theme(axis.text = element_blank())
print(p)


#26) Get Drug Level Data
sink("druglevelstat.txt")
print(cat("=============================\n"))
print(cat("Analysis of Drug Level (uM)"))
print(cat("=============================\n"))
DrugAnimalSessy<-aggregate(Drug~Animal+Sess+Period+Sex+TrialNum, data= M, FUN = "mean") # Get average drug level for each animal per session along with sex
DrugAnimalSess<-aggregate(Drug~Animal+Sess+Period+Sex, data= DrugAnimalSessy, FUN = "mean") # Get average drug level for each animal per session along with sex
DrugAnimalPeriod<-aggregate(Drug~Animal+Period+Sex, data=DrugAnimalSess, FUN="mean" )
DrugAnimalSess$Sess<-as.factor(DrugAnimalSess$Sess)
DrugAnimalSess$Sess <- factor(DrugAnimalSess$Sess, levels=c("1", "2", "3", "4","5", "6", "7", "8", "9", "10", "11", "12", "13", "14")) #Put Factors in correct order


#27) GLM of drug level data 
DrugStat <- lme(Drug~Sess*Sex,random=~1|Animal,data=DrugAnimalSess)
DrugPeriodStat<-lme(Drug~Period*Sex,random=~1|Animal,data=DrugAnimalPeriod)
print(anova(DrugStat))
print(summary(DrugPeriodStat))
hist(residuals(DrugStat),20)
print(cat("Post Hoc Comparisons of Drug by Session... :"))
DrugStatses<-emmeans(DrugStat, ~Sess)
print(summary(DrugStatses))
print(pairs(DrugStatses, adjust = "fdr"))
print(cat("=============================\n"))
print(cat("Analysis of Drug Level (MgKg)"))
print(cat("=============================\n"))

DrugAnimalSessy<-aggregate(DrugMgKg~Animal+Sess+Sex+TrialNum, 
                           data= M, FUN = "mean") # Get average drug level for each animal per session along with sex
DrugAnimalSess<-aggregate(DrugMgKg~Animal+Sess+Sex, 
                          data= DrugAnimalSessy, FUN = "mean") # Get average drug level for each animal per session along with sex
DrugAnimalSess$Sess<-as.factor(DrugAnimalSess$Sess)
DrugAnimalSess$Sess <- factor(DrugAnimalSess$Sess, levels=c("1", "2", "3", "4","5", "6", "7", 
                                                            "8", "9", "10", "11", "12", "13", "14")) #Put Factors in correct order
DrugStat <- lme(DrugMgKg~Sess*Sex,random=~1|Animal,data=DrugAnimalSess)
print(anova(DrugStat))
hist(residuals(DrugStat),20)
print(cat("Post Hoc Comparisons of Drug (mgKg) by Session"))
DrugStatses<-emmeans(DrugStat, ~Sess)
print(summary(DrugStatses))
print(pairs(DrugStatses, adjust = "fdr"))
print(summary(glht(DrugStat, linfct = mcp(Sess = "Tukey")),  test = adjusted("fdr")))
DrugAnimalSess$Sex<-as.factor(DrugAnimalSess$Sex)
DrugAnimalSess$Sess<-as.factor(DrugAnimalSess$Sess)


#28) GGPLOT of Drug Level across days
p<-ggplot(data = DrugAnimalSess, aes(x=Sess, y=DrugMgKg, fill = Sex))+geom_boxplot(lwd=1)+theme(legend.position = "none") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  #theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())+
  theme(axis.line = element_blank()) + 
  theme(axis.ticks.x=element_line(colour = 'black', size = 2), axis.ticks.y=element_line(colour = 'black', size = 2))+
  theme(axis.ticks.length=unit(.5, "cm")) + scale_fill_manual(values =c( "#56B4E9","#999999"))
print(p) 
sink()

#29) Get Uncued Responses
sink("uncuedstats.txt")
print(cat("=============================\n"))
print(cat("Analysis of Uncued Responses"))
print(cat("=============================\n"))
RRe<-aggregate(UncuedResp2~Animal+Sess+Sex, data= M, FUN = "mean") # Get average drug level for each animal per session along with sex
RRe$Sess<-as.factor(RRe$Sess)
RRe$Animal<-as.factor(RRe$Animal)

#30) GLM of Uncued responses
RReStat <- lme(UncuedResp2~Sess*Sex,random=~1|Animal,data=RRe)
print(anova(RReStat))
sink()
UncuedResponses<-emmeans(RReStat, ~Sess)
p<-ggplot(RRe, aes(x=Sess,y=UncuedResp2))+geom_boxplot()
print(p)

#31) Correlate Drug levels and Firing with Drug level
print(cat("=============================\n"))
print(cat("Correlation of Drug Levels (uM vs MgKg) & Firing with Drug Level "))
print(cat("=============================\n"))
print(cor.test(~Drug+DrugMgKg, data = M, method = "pearson", continuity = FALSE, con.level = 0.95) )
print(cor.test(~Abs+Drug, data = M, method = "spearman", continuity = FALSE, con.level = 0.95) )
DrugFiringCo<-M[,c(12,15)]
DrugFiringCo$RDrung<-rank(DrugFiringCo$Drug)
DrugFiringCo$RAbs<-rank(DrugFiringCo$Abs)

p<-ggplot(data=DrugFiringCo, aes(x=Drug, y=Abs))+geom_point() + geom_smooth(method = "lm")
print(p)

#32) Relapse Trials Data
GateShot<-M
GateShot<-GateShot[GateShot$Phase == "PRE" & GateShot$Resp == "HIT",]
GateShot2<-aggregate(TrialNum~Animal+Sess+Neuron+Sex+Phase+Resp+Period, data=GateShot, FUN = "mean")
GateShot3<-aggregate(TrialNum~Animal+Sess+Sex+Period, data=GateShot2, FUN="median")
GateShot3$PeriodSex<-paste(GateShot3$Period, GateShot3$Sex)
print(ggplot(data = GateShot3, aes(x=Se, y=TrialNum, fill=Sex))+geom_boxplot())
GateShot3$Sess<-as.factor(  GateShot3$Sess)
GateShot3$Sex<-as.factor(  GateShot3$Sex)
GateShot3$Animal<-as.factor(  GateShot3$Animal)
GateStat<-lme(TrialNum~Sess*Sex, random=~1|Animal, data= GateShot3)
summary(GateStat)
hist(residuals(GateStat),40)
Gate_Sex<-emmeans(GateStat, ~Sex)
Gate_Sess<-emmeans(GateStat, ~Sess)

#33) Load Trials to Relapse by Sex
Relapse<- read_excel("C:/Users/estrindj/Desktop/WestLabPaper/Statistical Results/RelapseTrials.xlsx")

#34) GGPLOT Trials to Relapse by Sex  
p<-ggplot(data = Relapse, aes( x =SEX, y=EMMEAN, fill = SEX))+geom_bar(stat="identity", color="black",size=2)+
  geom_errorbar(data=Relapse, aes(width=0,x=SEX, ymin=EMMEAN-SE, ymax=EMMEAN+SE), size=2)+
  scale_fill_manual("legend", values = c("FEMALE" = "#999999", "MALE" = "#56B4E9"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.line = element_blank()) +
  theme(axis.ticks.length=unit(.5, "cm"))+theme(axis.ticks=element_line(colour = 'black', size = 2))+
  theme(axis.text = element_blank())
print(p)



###############################################      PART III     #########################################################################



#35) Aggregate Pre-Cocaine data for statistics
stat_pre<-aggregate(Abs~Animal+Neuron+Period+Phase+Type+Resp+Sex, data = M, FUN = "mean")
stat_pre<-stat_pre[!(stat_pre$Phase=="LOAD"),]
stat_pre$Neuron<-as.factor(stat_pre$Neuron)
stat_pre$Animal<-as.factor(stat_pre$Animal)

#36) GLMM for Pre-First Hit data 
sink("pre_firing_stats.txt")
print(cat("=============================\n"))
print(cat("Analysis of Pre-Drug Firing Rates \n"))
print(cat("=============================\n"))
print(Pre_Statistics<-glmmTMB(Abs~Period*Type*Sex*Resp+(1|Animal/Neuron), data = stat_pre, family=tweedie(link="log")))
print(summary(Pre_Statistics))
res = simulateResiduals(Pre_Statistics)
plot(res, rank = T)

zip<-glmmTMB(Abs~Period*Type*Sex*Resp+(1|Animal/Neuron), data = stat_pre, family=tweedie(link="log"))
car::Anova(Pre_Statistics, test.statistic=c("F"))
zip2 <- update (zip, . ~ . - spp:mined)
anova(zip,zip2)


#print(RespStat<-emmeans(Pre_Statistics, ~Resp,  weights='proportional'))
#print(pairs(RespStat, adjust = "bonferroni"))
#print(PeriodRespStat<-emmeans(Pre_Statistics, ~Period*Resp))
#print(pairs(PeriodRespStat, adjust = "bonferroni"))
#print(PeriodTypeSexStat<-emmeans(Pre_Statistics, ~Period*Type*Sex))
#print(pairs(PeriodTypeSexStat, adjust= "fdr"))
print(PeriodTypeSexResp<-emmeans(Pre_Statistics, ~Period*Type*Sex*Resp))
print(pairs(PeriodTypeSexResp, adjust = "fdr"))
sink()

#37) Calculate Cohen's D.. However, I have read this is not accurate for GLMMs.. Refrain from now. 
EarlyHitD<-stat_pre
LateHitD<-stat_pre
allHit<-stat_pre
allHit<-allHit[!(allHit$Resp=="MISS"),]
EarlyHitD<-EarlyHitD[!(EarlyHitD$Resp=="MISS"),]
LateHitD<-LateHitD[!(LateHitD$Resp=="MISS"),]
EarlyHitD<-EarlyHitD[!(EarlyHitD$Period=="LATE"),]
LateHitD<-LateHitD[!(LateHitD$Period=="EARLY"),]
EarlyHitDS<-mean(EarlyHitD$Abs)
LateHitDS<-mean(LateHitD$Abs)
print(cat("Cohen's D: Early Hit versus Late Hit"))
print(EarlyLateHitCohenD<-abs(EarlyHitDS-LateHitDS)/sd(allHit$Abs))
print(cat("Cohen's D: Early Miss versus Early Hit")) 
allEarly<-stat_pre
allEarly<-allEarly[!(allEarly$Period=="LATE"),]
EarlyMissD<-stat_pre
EarlyMissD<-EarlyMissD[!(EarlyMissD$Resp=="HIT"),]
EarlyMissD<-EarlyMissD[!(EarlyMissD$Period=="LATE"),]
EarlyMissDS<-mean(EarlyMissD$Abs)
print(EarlyHITMISSCohenD<-abs(EarlyHitDS-EarlyMissDS)/sd(allEarly$Abs))

#38) Aggregate Load-UP Firing Data
Stat_Load<-aggregate(Abs~Animal+Neuron+Period+Phase+Type+Resp+Sex, data = M, FUN = "mean")
Stat_Load<-Stat_Load[!(Stat_Load$Phase=="PRE"),]
Stat_Load$Neuron<-as.factor(Stat_Load$Neuron)
Stat_Load$Animal<-as.factor(Stat_Load$Animal)

#39) GLMM for LOAD-UP firing 
print(cat("=============================\n"))
print(cat("Analysis of Load-Up Firing Rates \n"))
print(cat("=============================\n"))
Load_Statistics<-glmmTMB(Abs~Period*Type*Sex*Resp+(1|Animal/Neuron), data = Stat_Load, family=tweedie(link="log"))
print(summary(Load_Statistics))
hist(residuals(Load_Statistics),50)
res = simulateResiduals(Load_Statistics)
plot(res, rank = T)
print(TypeSexLOADstat<-emmeans(Load_Statistics, ~Type*Sex))
summary(TypeSexLOADstat)
print(pairs(TypeSexLOADstat, adjust = "bonferroni"))

#40) Analysis for Estrous Cycle
sink("EstrousResultsFiring.txt")
print(cat("=============================\n"))
print(cat("Analysis of Estrous Cycle Firing Rates \n"))
print(cat("=============================\n"))
statestrous<-aggregate(Abs~Animal+Neuron+Sex+EstPhase+Phase, data = M, FUN = "mean")
statestrous<-statestrous[!(statestrous$Phase=="LOAD"),]
statestrous<-statestrous[!(statestrous$Sex=="MALE"),]
print(staterestrous2<-glmmTMB(Abs~EstPhase+(1|Animal/Neuron), data = statestrous, family=tweedie(link="log")))
print(summary(staterestrous2))
hist(residuals(staterestrous2),40)
res = simulateResiduals(staterestrous2)
plot(res, rank = T)
Pre_Est<-emmeans(staterestrous2, ~EstPhase)
sink()


DrugEstrous<-aggregate(Drug~Animal+Sess+Sex+EstPhase+TrialNum, data = M, FUN = "mean")
DrugEstrous<-aggregate(Drug~Animal+Sess+Sex+EstPhase, data = DrugEstrous, FUN = "mean")
DrugEstrous<-aggregate(Drug~Animal+Sex+EstPhase, data = DrugEstrous, FUN = "mean")
DrugEstrous<-DrugEstrous[!(DrugEstrous$Sex=="MALE"),]
DrugEstrous$EstPhase<-factor(DrugEstrous$EstPhase)

DrugEstrousStat<-lme(Drug~EstPhase, random=~1|Animal, data= DrugEstrous)
summary(DrugEstrousStat)
Drug_Est<-emmeans(DrugEstrousStat, ~EstPhase)
print(pairs(Drug_Est, adjust = "bonferroni"))

aov_drugest <- aov(Drug ~ EstPhase + Error(Animal), data=DrugEstrous)
summary(aov_drugest)
AOV_Drug_Est<-emmeans(aov_drugest, ~EstPhase)


#41) Inital Firing Figures ----- potentially delete 
barploty<-aggregate(Abs~Period+Resp, data = M, FUN = st.err)
barplot<-aggregate(Abs~Period+Resp, data = M, FUN = "mean")
barplot$PeriodResp<-paste(barplot$Period, barplot$Resp)
barplot$Sterr<-barploty$Abs
barplot$PeriodResp<-paste(barplot$Period, barplot$Resp)
barplot2<-stat1
barplot2<-barplot2[!(barplot2$PeriodResp=="EARLY MISS"),]
barplot2<-barplot2[!(barplot2$PeriodResp=="LATE MISS"),]
barplot<-barplot[!(barplot$PeriodResp=="EARLY MISS"),]
barplot<-barplot[!(barplot$PeriodResp=="LATE MISS"),]

p<-ggplot(data = barplot, aes(x=PeriodResp, y= Abs, group = 1))+geom_line(size=2, color="#CC0033") +
  geom_point(stat = "identity", shape = 21, stroke = 2,  colour= "#CC0033", fill= "white", size = 5)+
  geom_line(data  = barplot2, aes(x=PeriodResp, y=Abs, group = Neuron), alpha = 0.1) + coord_cartesian( xlim = c(1.4, 1.6), ylim = c(0,11)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none")
print(p)

p<-ggplot(data = barplot, aes(x=PeriodResp, y= Abs, group = 1))+geom_line(size=2, color="#CC0033") +
  geom_errorbar( width = 0 , aes(ymin=Abs-Sterr, ymax=Abs+Sterr), colour = "#CC0033", size = 2) +
  geom_point(stat = "identity", shape = 21, stroke = 3,  colour= "#CC0033", fill= "white", size = 30) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none")
print(p)

#42) Pre-Cocaine 
PreEMMEANS<- read_excel("C:/Users/estrindj/Desktop/WestLabPaper/Statistical Results/EMMEANS_LOGSCALE_Figure.xlsx")
p<-ggplot(data=PreEMMEANS, aes(x=Name, y=MEAN_LOG, color=Name))+geom_point(size=5)+geom_errorbar(aes(ymin=MEAN_LOG-SE_LOG, ymax=MEAN_LOG+SE_LOG), width=.2, size=1)+
  ylab('Estimated Marginal Means of Pre Firing (Log-scale)')+theme(axis.title.x=element_blank(), axis.text.x=element_blank())
print(p)

FEMALE_PRE<- read_excel("C:/Users/estrindj/Desktop/WestLabPaper/Statistical Results/EMMEANS_LOGSCALE_Figure2.xlsx")
p<-ggplot(data=FEMALE_PRE, aes(x=Type, y=MEAN_LOG, color=Name))+geom_point(size=5)+geom_errorbar(aes(ymin=MEAN_LOG-SE_LOG, ymax=MEAN_LOG+SE_LOG), width=.2, size=1)+
  ylab('Estimated Marginal Means of Pre Firing (Log-scale)')+theme(axis.title.x=element_blank())
print(p)


#43) PUBLICATION FIGURE 4
Pre_A_Neu<-aggregate(Abs~Period+Resp+Type+Neuron+Sex + Animal+Phase, data = M, FUN = "mean") # Calculate average pre firing by Neuron by Period, response, sex, type
Pre_A_Neu<-Pre_A_Neu[!(Pre_A_Neu$Phase=="LOAD"),]
Pre_A_Neu$PerTyRespSex<-paste(Pre_A_Neu$Period, Pre_A_Neu$Type, Pre_A_Neu$Resp, Pre_A_Neu$Sex) #Create variable that is Period, Response , Sex & Type 
vc<-c("EARLY SHELL MISS FEMALE", "EARLY CORE MISS FEMALE", "LATE SHELL MISS FEMALE", 
      "LATE CORE MISS FEMALE", "LATE SHELL MISS MALE", "LATE CORE MISS MALE") # Only graph these variables
Pre_A_Neu<-Pre_A_Neu[Pre_A_Neu$PerTyRespSex %in% vc,]  # Take out extra cases
# EM_Average_PAN<-PreEMMEANS # Load estimated marginal means
#EM_Average_PAN$PerTyRespSex<-paste(EM_Average_PAN$Period, EM_Average_PAN$Type, EM_Average_PAN$Response, EM_Average_PAN$Sex) # Create variable in EMMEANS
#EM_Average_PAN<-EM_Average_PAN[EM_Average_PAN$PerTyRespSex %in% vc,] 
Average_PAN<-aggregate(Abs~PerTyRespSex, data = Pre_A_Neu, FUN = "mean")
Average_PAN2<-aggregate(Abs~PerTyRespSex, data = Pre_A_Neu, FUN = st.err)
Average_PAN$Error<-Average_PAN2$Abs
Average_PAN$PerTyRespSex <- factor(Average_PAN$PerTyRespSex, levels = c("EARLY SHELL MISS FEMALE", 
                                                                        "EARLY CORE MISS FEMALE", "LATE SHELL MISS FEMALE", "LATE CORE MISS FEMALE",
                                                                        "LATE SHELL MISS MALE", "LATE CORE MISS MALE"))
Average_PAN$Group<-1
Average_PAN$Group[Average_PAN$PerTyRespSex==c("LATE SHELL MISS FEMALE")]<-2
Average_PAN$Group[Average_PAN$PerTyRespSex==c("LATE CORE MISS FEMALE")]<-2
Average_PAN$Group[Average_PAN$PerTyRespSex==c("LATE SHELL MISS MALE")]<-3
Average_PAN$Group[Average_PAN$PerTyRespSex==c("LATE CORE MISS MALE")]<-3
# PreFiringMissAverages<-merge(Average_PAN, EM_Average_PAN, all = TRUE, by = c("PerTyRespSex")) #Merge based on this variable
Pre_A_Neu$PerTyRespSex <- factor(Pre_A_Neu$PerTyRespSex, levels = c("EARLY SHELL MISS FEMALE", 
                                                                    "EARLY CORE MISS FEMALE", "LATE SHELL MISS FEMALE", "LATE CORE MISS FEMALE", "LATE SHELL MISS MALE", "LATE CORE MISS MALE"))
Pre_A_Neu$Group<-1
Pre_A_Neu$Group[Pre_A_Neu$PerTyRespSex==c("LATE SHELL MISS FEMALE")]<-2
Pre_A_Neu$Group[Pre_A_Neu$PerTyRespSex==c("LATE CORE MISS FEMALE")]<-2
Pre_A_Neu$Group[Pre_A_Neu$PerTyRespSex==c("LATE SHELL MISS MALE")]<-3
Pre_A_Neu$Group[Pre_A_Neu$PerTyRespSex==c("LATE CORE MISS MALE")]<-3
Pre_A_Neu$BigGroup<-paste(Pre_A_Neu$Group, Pre_A_Neu$Neuron)
Average_PAN$Group<-as.factor(Average_PAN$Group)
Pre_A_Neu$BigGroup<-as.factor(Pre_A_Neu$BigGroup)
Pre_A_Neu$PerTyRespSex<-as.factor(Pre_A_Neu$PerTyRespSex)

p<-ggplot(data=Average_PAN, aes(x=PerTyRespSex, y=Abs, group=Group, colour=Group))+
  geom_line(size=2)+
  geom_errorbar( width = 0, aes(ymin=Abs-Error, ymax=Abs+Error), size = 2)+
  geom_point(stat = "identity", shape = 21, stroke = 2,  fill= "white", size = 5)+
  scale_color_manual(values=c("gray42", "gray42", "dodgerblue2", "black"))+
  geom_jitter(data=Pre_A_Neu, aes(x=PerTyRespSex, y=Abs, group=BigGroup, colour="black"), size=1, width=0.2, alpha= 0.35)+
  coord_cartesian(  ylim = c(0,3.5))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                            panel.background = element_blank(), axis.line = element_line(colour = "black")) + theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.line = element_blank()) +
  theme(axis.ticks.length=unit(.5, "cm"))+theme(axis.ticks=element_line(colour = 'black', size = 2))
# theme(axis.text = element_blank())
print(p)

#44) PUBLICATION FIGURE 4  
Pre_A_Neu<-aggregate(Abs~Period+Resp+Type+Neuron+Sex + Animal+Phase, data = M, FUN = "mean")
Pre_A_Neu<-Pre_A_Neu[!(Pre_A_Neu$Phase=="LOAD"),]
Pre_A_Neu$PerTyRespSex<-paste(Pre_A_Neu$Period, Pre_A_Neu$Type, Pre_A_Neu$Resp, Pre_A_Neu$Sex)
Pre_A_Neu2<-Pre_A_Neu
vc2<-c("EARLY SHELL MISS MALE", "EARLY SHELL HIT MALE", "LATE SHELL MISS MALE", 
       "LATE SHELL HIT MALE", "EARLY SHELL MISS FEMALE", "EARLY SHELL HIT FEMALE")
Pre_A_Neu2<-Pre_A_Neu2[Pre_A_Neu2$PerTyRespSex %in% vc2,] 
Pre_A_Neu_M<-Pre_A_Neu2
# EM_Average_PAN_M<-PreEMMEANS # Load estimated marginal means
#EM_Average_PAN_M$PerTyRespSex<-paste(EM_Average_PAN_M$Period, EM_Average_PAN_M$Type, EM_Average_PAN_M$Response, EM_Average_PAN_M$Sex) # Create variable in EMMEANS
#EM_Average_PAN_M<-EM_Average_PAN_M[EM_Average_PAN_M$PerTyRespSex %in% vc2,] 
Average_PAN_M<-aggregate(Abs~PerTyRespSex, data = Pre_A_Neu_M, FUN = "mean")
Average_PAN_M2<-aggregate(Abs~PerTyRespSex, data = Pre_A_Neu_M, FUN = st.err)
Average_PAN_M$Error<-Average_PAN_M2$Abs
Average_PAN_M$PerTyRespSex <- factor(Average_PAN_M$PerTyRespSex, levels = c("EARLY SHELL MISS MALE", "EARLY SHELL HIT MALE", "LATE SHELL MISS MALE", 
                                                                            "LATE SHELL HIT MALE", "EARLY SHELL MISS FEMALE", "EARLY SHELL HIT FEMALE"))
Average_PAN_M$Group<-1
Average_PAN_M$Group[Average_PAN_M$PerTyRespSex==c("LATE SHELL MISS MALE")]<-2
Average_PAN_M$Group[Average_PAN_M$PerTyRespSex==c("LATE SHELL HIT MALE")]<-2
Average_PAN_M$Group[Average_PAN_M$PerTyRespSex==c("EARLY SHELL MISS FEMALE")]<-3
Average_PAN_M$Group[Average_PAN_M$PerTyRespSex==c("EARLY SHELL HIT FEMALE")]<-3
# PreFiringHITAverages<-merge(Average_PAN_M, EM_Average_PAN_M, all = TRUE, by = c("PerTyRespSex")) #Merge based on this variable
Pre_A_Neu_M$PerTyRespSex <- factor(Pre_A_Neu_M$PerTyRespSex, levels = c("EARLY SHELL MISS MALE", "EARLY SHELL HIT MALE", "LATE SHELL MISS MALE", 
                                                                        "LATE SHELL HIT MALE", "EARLY SHELL MISS FEMALE", "EARLY SHELL HIT FEMALE"))
Pre_A_Neu_M$Group<-1
Pre_A_Neu_M$Group[Pre_A_Neu_M$PerTyRespSex==c("LATE SHELL MISS MALE")]<-2
Pre_A_Neu_M$Group[Pre_A_Neu_M$PerTyRespSex==c("LATE SHELL HIT MALE")]<-2
Pre_A_Neu_M$Group[Pre_A_Neu_M$PerTyRespSex==c("EARLY SHELL MISS FEMALE")]<-3
Pre_A_Neu_M$Group[Pre_A_Neu_M$PerTyRespSex==c("EARLY SHELL HIT FEMALE")]<-3
Pre_A_Neu_M$BigGroup<-paste(Pre_A_Neu_M$Group, Pre_A_Neu_M$Neuron)
Average_PAN_M$Group<-as.factor(Average_PAN_M$Group)
Pre_A_Neu_M$BigGroup<-as.factor(Pre_A_Neu_M$BigGroup)
Pre_A_Neu_M$PerTyRespSex<-as.factor(Pre_A_Neu_M$PerTyRespSex)

p<-ggplot(data=Average_PAN_M, aes(x=PerTyRespSex, y=Abs, group=Group, colour=Group))+
  geom_line(size=2)+
  geom_errorbar( width = 0, aes(ymin=Abs-Error, ymax=Abs+Error), size = 2)+
  geom_point(stat = "identity", shape = 21, stroke = 2,  fill= "white", size = 5)+
  scale_color_manual(values=c("dodgerblue2", "dodgerblue2", "gray42", "black"))+
  geom_jitter(data=Pre_A_Neu_M, aes(x=PerTyRespSex, y=Abs, group=BigGroup, colour="black"), size=1, width=0.2, alpha= 0.3)+
  coord_cartesian(  ylim = c(0,3.5))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                            panel.background = element_blank(), axis.line = element_line(colour = "black")) + theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.line = element_blank()) +
  theme(axis.ticks.length=unit(.5, "cm"))+theme(axis.ticks=element_line(colour = 'black', size = 2))
#theme(axis.text = element_blank())
print(p)

#45) LOAD UP STATS
LOAD_A_Neu<-aggregate(Abs~Type+Neuron+Sex+Animal+Phase, data = M, FUN = "mean")
LOAD_A_Neu<-LOAD_A_Neu[!(LOAD_A_Neu$Phase=="PRE"),]
LOAD_A_Neu$SexType<-paste(LOAD_A_Neu$Sex, LOAD_A_Neu$Type)
parse1<-c("FEMALE SHELL", "FEMALE CORE", "MALE CORE")
LOAD_SEXTYPE<-LOAD_A_Neu[LOAD_A_Neu$SexType %in% parse1,] 
LOAD_SEXTYPE2<-aggregate(Abs~SexType, data = LOAD_SEXTYPE, FUN = st.err)
LOAD_Sextype_o<-aggregate(Abs~SexType, data = LOAD_SEXTYPE, FUN = "mean")
LOAD_Sextype_o$Error<-LOAD_SEXTYPE2$Abs
LOAD_Sextype_o$color<-c("gray42", "gray42", "dodgerblue2")
LOAD_Sextype_o$SexType <- factor(LOAD_Sextype_o$SexType, levels=c("FEMALE SHELL", "FEMALE CORE", "MALE CORE"))

p<-ggplot(data=LOAD_Sextype_o, aes(x=SexType, y=Abs, fill="black"))+
  geom_bar(stat = "identity", fill="black")+
  geom_errorbar( width = 0, aes(ymin=Abs-Error, ymax=Abs+Error), size = 2)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.line = element_blank()) +
  theme(axis.ticks.length=unit(.5, "cm"))+theme(axis.ticks=element_line(colour = 'black', size = 2))
# theme(axis.text = element_blank())
print(p)

#46) Load up drug levels
LOAD_DRUG<-aggregate(Drug~Sess+Animal+Sex+Phase+TrialNum, data=M, FUN="mean")
LOAD_DRUG<-aggregate(Drug~Sess+Phase+TrialNum, data=LOAD_DRUG, FUN="mean")
LOAD_DRUGY<-aggregate(Drug~Phase+TrialNum, data=LOAD_DRUG, st.err)
LOAD_DRUG<-aggregate(Drug~Phase+TrialNum, data=LOAD_DRUG, FUN="mean")
LOAD_DRUG$Error<-LOAD_DRUGY$Drug
LOAD_DRUG<-LOAD_DRUG[!(LOAD_DRUG$Phase=="PRE"),]

p<-ggplot(data=LOAD_DRUG, aes(x=TrialNum, y=Drug))+geom_line()+geom_ribbon(aes(ymin=Drug-Error, ymax=Drug))
print(p)

#47) Plot firing by Session
Scat_Neu<-aggregate(Abs~Sess+Type+Neuron+Phase, data = M, FUN = "mean")
Scat_Neu<-Scat_Neu[!(Scat_Neu$Phase=="LOAD"),]
Scat_Neu$Abs<-as.numeric(Scat_Neu$Abs)
Scat_Neu$Sess<-as.numeric(Scat_Neu$Sess)


write.table(Scat_Neu,"filename.txt",sep="\t",row.names=FALSE)

p<-ggplot(data=Scat_Neu, aes(x=Sess, y=Abs))+geom_jitter(width=.5, alpha=0.1, shape=21, fill="black", size=4)+
  geom_smooth(method=lm, color="black", fill="black")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.line = element_blank()) +
  theme(axis.ticks.length=unit(.5, "cm"))+theme(axis.ticks=element_line(colour = 'black', size = 2))
# theme(axis.text = element_blank())
print(p)

#48) Plot of Drug by Esttours

Est_Drug<-aggregate(Drug~Animal+EstPhase+Sess, data = M, FUN = "mean")
Est_Drug<-aggregate(Drug~Animal+EstPhase, data = Est_Drug, FUN = "mean")
Est_DrugE<-aggregate(Drug~EstPhase, data = M, st.err) 
Est_Drug<-aggregate(Drug~EstPhase, data = M, FUN="mean") 
Est_Drug$Error<-Est_DrugE$Drug


Est_Drug<- read_excel("C:/Users/estrindj/Desktop/WestLabPaper/Statistical Results/Est_emmeans.xlsx")

p<-ggplot(data=Est_Drug, aes(x=EstPhase, y=emmean, fill=EstPhase, color=EstPhase))+
  geom_bar(stat = "identity")+
  geom_errorbar( width = 0, aes(ymin=emmean-SE, ymax=emmean+SE), size = 5)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.line = element_blank()) +
  theme(axis.ticks.length=unit(.5, "cm"))+theme(axis.ticks=element_line(colour = 'black', size = 2))+
  theme(axis.text = element_blank())#+scale_fill_brewer(palette="PRGn")
print(p)

Est_firing<- read_excel("C:/Users/estrindj/Desktop/WestLabPaper/Statistical Results/Est_firing.xlsx")

p<-ggplot(data=Est_firing, aes(x=EstPhase, y=MEAN, fill=EstPhase, color=EstPhase))+
  geom_bar(stat = "identity")+
  geom_errorbar( width = 0, aes(ymin=MEAN-SE, ymax=MEAN+SE), size = 5)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.line = element_blank()) +
  theme(axis.ticks.length=unit(.5, "cm"))+theme(axis.ticks=element_line(colour = 'black', size = 2))
#theme(axis.text = element_blank())#+scale_fill_brewer(palette="PRGn")
print(p)





Neuron_facet<- read_excel("C:/Users/estrindj/Desktop/Neurons2/Neurons2.xlsx")
Neuron_facet$Neuron<-as.factor(  Neuron_facet$Neuron)
p<-ggplot(data= Neuron_facet, aes(x=Time, y=V))+geom_line(size=4)+facet_wrap(~Neuron)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + theme(legend.position = "none") +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.line = element_blank()) +
  theme(axis.text = element_blank())+ #+scale_fill_brewer(palette="PRGn")
  theme(strip.background = element_blank(),
        strip.text.y = element_blank(), strip.text.x = element_blank())
print(p)


#49 FIGURE of M Firing vs Baseline

p<-ggplot(data=M, aes(x=BaseLine, y=Firing))+geom_point()+geom_smooth(method="lm")+facet_wrap(~Sess)
print(p)

p<-ggplot(data=M, aes(x=as.numeric(Sess), y=BaseLine, color="blue"))+geom_jitter(size=3, color='#56B4E9')+
  geom_smooth(method="lm", color='#E69F00')+ scale_y_continuous(trans = log2_trans())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  theme(legend.position = "none") 
print(p)


(cor.test(~BaseLine+Firing, data = M, method = "pearson", continuity = FALSE, con.level = 0.95) )











# ----------------------------------------------------------------------------
# Matchbox Statistical Analysis 
# February 2017
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------


# To do:
# - Cohen's d			d = mean(sample1-sample2)/ std(sample1-sample2)
# - Partial eta squared		EtaSq
# - Within-subjects CI		#MS of Interaction in ANOVA: fit.etasq$MS[3]


# ----------------------------------------------------------------------------
# Analysis preparation
# ----------------------------------------------------------------------------
# Load what is needed and delete what is not
library(reshape2)
rm(list = ls())

# Analysis details
paramList <- c("Object placing time (s)","Object placing velocity",'Object transport velocity mean',
               'Number of velocity changes','Full movement time (s)') 
condOrder <- c("Default","Visible","Invisible") # NCS c("N","C","S") #

# File locations
dataFilepath <- "C:/Users/au592397/Work/Projects/Matchbox2019/analysis/mb/results/"
outputFilepath <- "C:/Users/au592397/Work/Projects/Matchbox2019/analysis/stats/results/"

# Constants
corrPvalue <- .05 #.01

# For saving the output
appendOutput <- FALSE
outputFilename <- paste(outputFilepath,"mb_output.txt",sep="")
sink(outputFilename, append=appendOutput, split=TRUE)
print(date())


# ----------------------------------------------------------------------------
# Compare conditions
# ----------------------------------------------------------------------------
for (param in paramList) {
  
  print(param)
  
  # Load file and turn into data frame
  data <- read.table(paste(dataFilepath,param," Exp 100  200.csv",sep=""),header=FALSE,sep=",",dec=".")
  data <- data.frame(data)
  names(data) <- c("experiment","pair","role","part","N","C","S") # condOrder
  
  data <- subset(data,role=="1")
  
  # Descriptives
  print(summary(data))
  
  
  # ----------------------------------------------------------------------------
  # 2x2x3 mixed Anova (between: experiment and part, within: condition)
  # ----------------------------------------------------------------------------
  
  # Re-arrange data
  data2 <- melt(data,id=c("experiment","pair","role","part"))
  names(data2) <- c("experiment","pair","role","part","cond","param")
  data2 <- subset(data2,(cond=="N"|cond=="C"|cond=="S"))
  # data2 <- subset(data2,(Pair!=103))
  data2$experiment <- factor(data2$experiment)
  # data2$role <- factor(data2$role)
  data2$part <- factor(data2$part)
  data2$cond <- factor(data2$cond)
  # str(data2)
  
  # Perform Anova 
  # fit <- aov(Param ~ Cond + Error(Pair/Cond),data2)
  # fit <- aov(Param ~ Exp * Cond + Error(Pair/Cond),data2)
  # fit <- aov(Param ~ Exp * Part * Cond + Error(Pair/Cond),data2)
  # fit <- aov(param ~ cond * exp * part + Error(pair/cond),data2)
  fit <- aov(param ~ cond * experiment * part + Error(pair/cond),data2) 
  print(summary(fit))
  #  fit.etasq <- EtaSq(fit, type=1, anova=TRUE)
  #  print(fit.etasq)
  
  #  with(data2,interaction.plot(Exp,Cond,Param),type="b",pch(12,23,4))#,ylim=c(0,4)) #col=c("red","blue","green"))
  boxplot(param ~ experiment * cond, subset(data2,(role=="1"&part=="1")), main=c(param,'part 1'), col=c("gold", "red")) #, ylim=c(0,4))
  boxplot(param ~ experiment * cond, subset(data2,(role=="1"&part=="2")), main=c(param,'part 2'), col=c("gold", "red")) #, ylim=c(0,4))
  
  # Post-hoc one-way ANOVA with only factor condition
  fit <- aov(param ~ cond + Error(pair/cond),subset(data2,(experiment=="1"&role=="1"&part=="1"))) 
  print(summary(fit))
  
  boxplot(param ~ experiment * cond, subset(data2,(experiment=="1"&role=="1"&part=="1")), main=c(param,'part 1'), col=c("gold", "red")) #, ylim=c(0,4))
  
  
  # ----------------------------------------------------------------------------
  # Uncorrected t-tests
  # ----------------------------------------------------------------------------
  
  for (expIdx in c(1,2)) {
  print("N vs. C")
  outcome = with(subset(data,(experiment==expIdx&role=="1"&part=="1")),
                 t.test(N,C,paired=TRUE,conf.level=1-corrPvalue))
  print(outcome)
  
  print("N vs. S")
  outcome = with(subset(data,(experiment==expIdx&role=="1"&part=="1")),
                 t.test(N,S,paired=TRUE,conf.level=1-corrPvalue))
  print(outcome)
  
  print("C vs. S")
  outcome = with(subset(data,(experiment==expIdx&role=="1"&part=="1")),
                 t.test(C,S,paired=TRUE,conf.level=1-corrPvalue))
  print(outcome)

  barplot(c(mean(data$N),mean(data$C),mean(data$S)),ylab=param,main=c(param,expIdx),names.arg=c("N","C","S"))
  }
  
  }


# ----------------------------------------------------------------------------
# This is the end
# ----------------------------------------------------------------------------
sink()



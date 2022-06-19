#==============================================================================
# 02_predicted_choices.R
# Purpose: compute predicted prohibition probabilities by country, compute CIs, and plot. 
# Authors: Soenke Ehret 
#==============================================================================


library(dplyr)
library(multiwayvcov)
library(lmtest)
library(stargazer)
library(ivpack)
library(sandwich)
library(car)
library(multcomp)
library(multiwayvcov)
library(prediction)
library(ggplot2)


load("conjoint_data_analysis_prdct.Rdata")


data=conjoint_data

#prepare level labels for prediction function

levels(data$Risk.of.unemployment)<-c( "very.low.unempl.risk","somewhat.low.risk","somewhat.high.risk",  
                                      "very.high.risk" )

levels(data$Job.creation)<-c("no.jobs",  "benefit.College.edu","benefit.manual.labor","jobs.benefit.all")

levels(data$Example.area.of.use)<-c( "advertising", "finance", "production",
                                     "medicial", "public service", "surveillance.safety")

levels(data$Privacy)<-c("customize", "gather.w.o.consent", 
                        "with.consent")
#table(conjoint_data$Choices.made.in.dilemma)
levels(data$Choices.made.in.dilemma)<-c("no.choices.made","humans.vs.money","humans.vs.humans",
                                        "money.vs.money")

levels(data$Transparency)<-c("easy.to.explain",
                             "nobody.can.understand", 
                             "only.experts.can.understand")

levels(data$Discrimination)<-c("neutral","recreate.bias")


### models used for the prediction function

#eqsimplem<-selected ~ Privacy+Job.creation+Risk.of.unemployment+Example.area.of.use+
#  Choices.made.in.dilemma+Transparency+Discrimination

#Use this equation for Appendix, Figure 3 (absolute values)
#eqsimplem<-selected ~ Privacy+Job.creation+Risk.of.unemployment+
#  Choices.made.in.dilemma+Transparency+Discrimination

#eqsimplem<-(selected-mean(selected, rm.na=, na.rm=T)) ~ Privacy+Job.creation+Risk.of.unemployment+Example.area.of.use+
#  Choices.made.in.dilemma+Transparency+Discrimination


eqsimplem<-(selected-mean(selected, na.rm=T)) ~ Privacy+Job.creation+Risk.of.unemployment+
 Choices.made.in.dilemma+Transparency+Discrimination # basic model


### simulated data frames

algo1<-data.frame(cbind(Risk.of.unemployment="very.high.risk",Privacy="customize",
                        Job.creation="no.jobs",Example.area.of.use="production",
                        Choices.made.in.dilemma="no.choices.made",Transparency="easy.to.explain",
                        Discrimination="neutral"))

algo2<-data.frame(cbind(Risk.of.unemployment="very.high.risk",Privacy="customize",
                        Job.creation="jobs.benefit.all",Example.area.of.use="production",
                        Choices.made.in.dilemma="no.choices.made",Transparency="easy.to.explain",
                        Discrimination="neutral"))

algo3<-data.frame(cbind(Risk.of.unemployment="very.low.unempl.risk",Privacy="customize",
                        Job.creation="no.jobs",Example.area.of.use="production",
                        Choices.made.in.dilemma="no.choices.made",Transparency="easy.to.explain",
                        Discrimination="neutral"))

algo4<-data.frame(cbind(Risk.of.unemployment="very.low.unempl.risk",Privacy="customize",
                        Job.creation="jobs.benefit.all",Example.area.of.use="production",
                        Choices.made.in.dilemma="no.choices.made",Transparency="easy.to.explain",
                        Discrimination="neutral"))

algo5<-data.frame(cbind(Risk.of.unemployment="very.high.risk",Privacy="gather.w.o.consent",
                        Job.creation="no.jobs",Example.area.of.use="surveillance.safety",
                        Choices.made.in.dilemma="humans.vs.money",Transparency="nobody.can.understand",
                        Discrimination="recreate.bias"))

algo6<-data.frame(cbind(Risk.of.unemployment="very.high.risk",Privacy="gather.w.o.consent",
                        Job.creation="jobs.benefit.all",Example.area.of.use="surveillance.safety",
                        Choices.made.in.dilemma="humans.vs.money",Transparency="nobody.can.understand",
                        Discrimination="recreate.bias"))

algo7<-data.frame(cbind(Risk.of.unemployment="very.low.unempl.risk",Privacy="gather.w.o.consent",
                        Job.creation="no.jobs",Example.area.of.use="surveillance.safety",
                        Choices.made.in.dilemma="humans.vs.money",Transparency="nobody.can.understand",
                        Discrimination="recreate.bias"))

algo8<-data.frame(cbind(Risk.of.unemployment="very.low.unempl.risk",Privacy="gather.w.o.consent",
                        Job.creation="jobs.benefit.all",Example.area.of.use="surveillance.safety",
                        Choices.made.in.dilemma="humans.vs.money",Transparency="nobody.can.understand",
                        Discrimination="recreate.bias"))


countries<-c("a) Germany","b) United Kingdom","c) China","d) Chile", "e) India")

countrysummarylist<-list()
datafull=data
countries[c]

algos<-list(algo1,algo2,algo3,algo4,algo5,algo6,algo7,algo8)
#algos<-list(algo1,algo5,algo2,algo6,algo3,algo7,algo4,algo8)#alternative ordering

#loop to calculate predicted y_hat and ci, using prediction package

governance<-c(rep(0,4),rep(1,4))
profile<-c(1:4,1:4)
for (a in 1: length(algos)){
  for (c in 1:length(countries)){
    data=datafull[datafull$Q_country==countries[c],]
    lmconjoint<-lm(eqsimplem, data=data)
    print(c)
    vcov<-vcovCL(lmconjoint, cluster = data$Response.ID)
    pred<-prediction_summary(lmconjoint, at = algos[[a]], type = "response",
                             calculate_se = TRUE, vcov=vcov)
    countrysummarylist[[c+(a-1)*length(countries)]]<-cbind(a,profile[a],governance[a],countries[c],pred[8],pred[12],pred[13])
    
  }
}


### preparing the plot, figure 2
predictiondf <-do.call(rbind.data.frame, countrysummarylist)




prediplot<-ggplot(data=predictiondf, aes(y=Prediction,x=as.factor(`profile[a]`), linetype=factor(`governance[a]`)))+
  geom_point(position = position_dodge(width = 0.5))+geom_errorbar(aes(ymin=lower, ymax=upper),size=1, width=0,position = position_dodge(width = 0.5))+
  facet_grid(.~`countries[c]`)+geom_hline(yintercept=0)+theme_bw()+theme(legend.position = "bottom",axis.text.x = element_text(angle = 0))+
  scale_linetype_discrete(name = "Normative features",labels = c("Best case", "Worst case"))+
  scale_x_discrete(breaks=c("1","2","3","4"),name="1: high unempl. risk, no jobs created. 2: high unempl. risk, new jobs created.\n 3: low  unempl. risk, no jobs created.. 4: low unempl. risk, new jobs created.")+
  scale_y_continuous(name="Probability of AI restriction choice", limits=c(-0.55,0.55))


#prediplot_absolute<-ggplot(data=predictiondf, aes(y=Prediction,x=as.factor(`profile[a]`), linetype=factor(`governance[a]`)))+
#  geom_point(position = position_dodge(width = 0.5))+geom_errorbar(aes(ymin=lower, ymax=upper),size=1, width=0,position = position_dodge(width = 0.5))+
#  facet_grid(.~`countries[c]`)+theme_bw()+theme(legend.position = "bottom",axis.text.x = element_text(angle = 0))+
#  scale_linetype_discrete(name = "Normative features",labels = c("Best case", "Worst case"))+
#  scale_x_discrete(breaks=c("1","2","3","4"),name="1: high unempl. risk, no jobs created. 2: high unempl. risk, new jobs created.\n 3: low  unempl. risk, no jobs created.. 4: low unempl. risk, new jobs created.")+
 # scale_y_continuous(name="Probability of AI restriction choice")

ggsave(prediplot, width=10, height=5, units="in", file="airestrictionprofile_full.bmp") 
#ggsave(prediplot_absolute, width=10, height=5, units="in", file="airestrictionprofile_full_new_abs.pdf") #figure 2, main manuscript. Use commented out line 54 specification to get figure 3, appendix, via line 149-


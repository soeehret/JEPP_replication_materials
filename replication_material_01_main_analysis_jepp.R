#==============================================================================
# 01_main_analysis_jepp.R
# Purpose: run the conjoint linear models, for both binary choice and likert scale dependent variables;
# run the cross-country differences analyses; create coefficient plots.
# Authors: Soenke Ehret 
#==============================================================================

rm(list = ls())

library(cregg)
library(prediction)
library(ggplot2)
library(dplyr)
library(xtable)


load("conjoint_data_analysis_prdct.Rdata")

### run the alternative profile choice models


################################################################################
### models
################################################################################

### with respondent covariates

eq<-selected ~ Privacy+Job.creation+Risk.of.unemployment+Example.area.of.use+
  Choices.made.in.dilemma+Transparency+Discrimination+
  socinsuremajsourc+privacyconcern+livingstandexp+digitalliteracy

### without respondent covariates (used  for the cross-country differences analysis)

eqsimple<-selected ~ Privacy+Job.creation+Risk.of.unemployment+Example.area.of.use+
  Choices.made.in.dilemma+Transparency+Discrimination


################################################################################
### main analysis, binary choice between two algorithm profiles
################################################################################

conjoint_data$Q_country_f<-factor(conjoint_data$Q_country) # need to transform country in factor variables for cj() 

x_amce_cov <- cj(conjoint_data,eq
                 ,id = ~ Response.ID,
                 estimate = "amce",  by = ~Q_country_f,
                 level_order="ascending")


#Prepare plotting#

x_amce_cov$feature=recode_factor(x_amce_cov$feature, Risk.of.unemployment="Risk of unemployment", 
                                 Job.creation="Job creation",Example.area.of.use="Example area of use",
                                 Choices.made.in.dilemma="Choices made in dilemma")#create nice factor labels for plot

excludereport=c("socinsuremajsourc", "privacyconcern", "livingstandexp", "digitalliteracy") #exclude subject level covariates from plotting later 



xplot_amce_cov_binary<-plot(x_amce_cov[!x_amce_cov$feature %in% excludereport,], 
                     vline = 0, feature_headers=TRUE, header_fmt="%s",size=1.25, xlab="Estimated Average Marginal Component Effects")+
  facet_grid( ~ BY, drop=TRUE)+ theme(legend.position = "none")+
  scale_y_discrete(labels=c("Risk of unemployment"=expression(bold("Risk of unemployment")),
                            "Job creation"=expression(bold("Job creation")),
                            "Example area of use"=expression(bold("Example area of use")),
                            "Choices made in dilemma"=expression(bold("Choices made in dilemma")),
                            "Privacy"=expression(bold("Privacy")),
                            "Transparency"=expression(bold("Transparency")),
                            "Discrimination"=expression(bold("Discrimination")),
                            parse=TRUE))+theme(
                              axis.ticks.y = element_line(color = 
                                                            c("black", "black", NA, "black", "black", "black", NA,
                                                              "black", "black", "black", NA, 
                                                              "black", "black", "black", "black", NA,
                                                              "black", "black", "black", "black", "black", "black", NA,
                                                              "black", "black", "black", "black", NA,
                                                              "black", "black", "black", "black", NA)))

#ggsave(xplot_amce_cov_binary, file="plotamcefull_amce_binary_revise.pdf",width = 9, height=5, units="in") # create figure 1
ggsave(xplot_amce_cov_binary, file="plotamcefull_amce_binary_revise.bmp",width = 9, height=4.75, units="in") # create figure 1



#export to print-out tables 2-6, Appendix
print(xtable(x_amce_cov[x_amce_cov$BY=="a) Germany",c(1,4:9)], type = "latex"), file = "conjoint_gernew.tex")
print(xtable(x_amce_cov[x_amce_cov$BY=="b) United Kingdom",c(1,4:9)], type = "latex"), file = "conjoint_uknew.tex")
print(xtable(x_amce_cov[x_amce_cov$BY=="c) China",c(1,4:9)], type = "latex"), file = "conjoint_chinanew.tex")
print(xtable(x_amce_cov[x_amce_cov$BY=="d) Chile",c(1,4:9)], type = "latex"), file = "conjoint_chilenew.tex")
print(xtable(x_amce_cov[x_amce_cov$BY=="e) India",c(1,4:9)], type = "latex"), file = "conjoint_indianew.tex")


### calculate & plot differences of amces across countries
conjoint_data$Q_country_f<-factor(conjoint_data$Q_country)
levels(conjoint_data$Q_country_f)<-c("Germany","a) United Kingdom","b) China","c) Chile","d) India") #adjust country labels
conjoint_data$Q_country_f<-relevel(conjoint_data$Q_country_f, ref="Germany")

xdiff <- cj(conjoint_data, eqsimple,
            id = ~ Response.ID,
            estimate = "amce_differences",  by = ~Q_country_f,
            level_order="ascending")

xdiff$feature=recode_factor(xdiff$feature, Risk.of.unemployment="Risk of unemployment", 
                                 Job.creation="Job creation",Example.area.of.use="Example area of use",
                                 Choices.made.in.dilemma="Choices made in dilemma")#create nice labels for plot


xplotxdiff_binary<-plot(xdiff, group = "Q_country_f", vline = 0,size=1.25, xlab="Estimated Difference of the Average Marginal Component Effects")+facet_grid(.~BY)+ theme(legend.position = "none")+
  scale_y_discrete(labels=c("(Risk of unemployment)"=expression(bold("Risk of unemployment")),
                            "(Job creation)"=expression(bold("Job creation")),
                            "(Example area of use)"=expression(bold("Example area of use")),
                            "(Choices made in dilemma)"=expression(bold("Choices made in dilemma")),
                            "(Privacy)"=expression(bold("Privacy")),
                            "(Transparency)"=expression(bold("Transparency")),
                            "(Discrimination)"=expression(bold("Discrimination")),
                            parse=TRUE))+theme(
                              axis.ticks.y = element_line(color = 
                                                            c("black",  NA, "black", "black", NA,
                                                               "black", "black", NA, 
                                                               "black", "black", "black", NA,
                                                               "black", "black", "black", "black", "black", NA,
                                                               "black", "black", "black", NA,
                                                               "black", "black", "black", NA)))

ggsave(xplotxdiff_binary, file="comparetoGER_amce_binary_new.pdf",width = 9.5, height=6, units="in") #appendix, figure 1


########################################
### repeat analysis for likert scale data
########################################

load("conjoint_data_rank_analysis.Rdata")


### main analysis
conjoint_data$Q_country_f<-factor(conjoint_data$Q_country) # need to transform country in factor variables for cj() 


x_amce_cov_rank <- cj(conjoint_data,eq
                 ,id = ~ Response.ID,
                 estimate = "amce",  by = ~Q_country_f,
                 level_order="ascending")
###Prepare plotting###
library(dplyr)
x_amce_cov_rank$feature=recode_factor(x_amce_cov_rank$feature, Risk.of.unemployment="Risk of unemployment", 
                                 Job.creation="Job creation",Example.area.of.use="Example area of use",
                                 Choices.made.in.dilemma="Choices made in dilemma")#create nice labels for plot




xplot_amce_cov_rank<-plot(x_amce_cov_rank[!x_amce_cov_rank$feature %in% excludereport,], 
                     vline = 0, feature_headers=TRUE, header_fmt="%s",size=1.25, xlab="Estimated Average Marginal Component Effects")+
  facet_grid( ~ BY, drop=TRUE)+ theme(legend.position = "none")+
  scale_y_discrete(labels=c("Risk of unemployment"=expression(bold("Risk of unemployment")),
                            "Job creation"=expression(bold("Job creation")),
                            "Example area of use"=expression(bold("Example area of use")),
                            "Choices made in dilemma"=expression(bold("Choices made in dilemma")),
                            "Privacy"=expression(bold("Privacy")),
                            "Transparency"=expression(bold("Transparency")),
                            "Discrimination"=expression(bold("Discrimination")),
                            parse=TRUE))+theme(
                              axis.ticks.y = element_line(color = 
                                                            c("black", "black", NA, "black", "black", "black", NA,
                                                              "black", "black", "black", NA, 
                                                              "black", "black", "black", "black", NA,
                                                              "black", "black", "black", "black", "black", "black", NA,
                                                              "black", "black", "black", "black", NA,
                                                              "black", "black", "black", "black", NA)))


ggsave(xplot_amce_cov_rank, file="plotamcefull_amce_rank_revise.pdf",width = 9.5, height=4.75, units="in") #Appendix, figure 2, top panel


conjoint_data$Q_country_f<-factor(conjoint_data$Q_country)
levels(conjoint_data$Q_country_f)<-c("Germany","a) United Kingdom","b) China","c) Chile","d) India") #adjust country labels
conjoint_data$Q_country_f<-relevel(conjoint_data$Q_country_f, ref="Germany")

xdiff_rank <- cj(conjoint_data, eqsimple,
            id = ~ Response.ID,
            estimate = "amce_differences",  by = ~Q_country_f,
            level_order="ascending")

xdiff_rank$feature=recode_factor(xdiff_rank$feature, Risk.of.unemployment="Risk of unemployment", 
                            Job.creation="Job creation",Example.area.of.use="Example area of use",
                            Choices.made.in.dilemma="Choices made in dilemma")#create nice labels for plot

xplotxdiff_binary_rank<-plot(xdiff_rank, group = "Q_country_f", vline = 0,size=1.25, xlab="Estimated Difference of the Average Marginal Component Effects")+facet_grid(.~BY)+ theme(legend.position = "none")+
  scale_y_discrete(labels=c("(Risk of unemployment)"=expression(bold("Risk of unemployment")),
                            "(Job creation)"=expression(bold("Job creation")),
                            "(Example area of use)"=expression(bold("Example area of use")),
                            "(Choices made in dilemma)"=expression(bold("Choices made in dilemma")),
                            "(Privacy)"=expression(bold("Privacy")),
                            "(Transparency)"=expression(bold("Transparency")),
                            "(Discrimination)"=expression(bold("Discrimination")),
                            parse=TRUE))+theme(
                              axis.ticks.y = element_line(color = 
                                                            c("black",  NA, "black", "black", NA,
                                                              "black", "black", NA, 
                                                              "black", "black", "black", NA,
                                                              "black", "black", "black", "black", "black", NA,
                                                              "black", "black", "black", NA,
                                                              "black", "black", "black", NA)))

ggsave(xplotxdiff_binary_rank, file="comparetoGER_amce_diff_rank.pdf",width = 9.5, height=4.75, units="in") #appendix, figure 2 bottom panel

###Notes on covariate categorical variable coding.

# socinsuremajsourc: In case of unemployment, how much do you expect to rely on Government Welfare or Unemployment Insurance -- will it be a major source of income, a minor source of income or not a source at all?
# Major source (1)
# Minor source (2)
# Not a source at all (3)
# Refuse to Answer / Don't know  (4)
# Non-response (NR)

#livingstandexp: Looking forward to the next three years, how confident do you feel about being able to keep your current living standard ?
# Very confident (12)
# Confident (22)
# Somewhat confident (32)
# Not confident (44)
# Refuse to Answer / Don't know  (52)
# Non-response (NR)

#privacyconcern:Are you concerned about people you do not know obtaining personal information about you from your online activities?
# Very concerned (13)
# Concerned (23)
# Somewhat concerned (33)
# Not concerned (43)
# Refuse to Answer / Don't know  (53)
# Non-response (NR)

#digitalliteracy: appendix, questionnaire, average of items 1-6 ratings, cut into three deciles.


####Additional micro-data presented in the Appendix can be obtained by contacting the author at sonke.ehret@gmail.com, subject to publication
####Also check https://github.com/soeehret for data published after publication.


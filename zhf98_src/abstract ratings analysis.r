
require(lme4)
require(lmerTest)
require(MuMIn)    # provides r.squaredGLMM()
require(effects)  # for effects plots
require(plyr)
require(dplyr)
require(ggplot2)
require(gridExtra)
require(psych)

# whether conservatives or liberals are rated is coded in the dummy "target" (cons = 0, lib = 1)

# you'll need to change these paths

politics <- read.csv( "bias rating results-Analysis.csv" )
publications <- read.csv( "gatekeeper_all_abstracts.csv" )
abstracts <- read.csv("Participants_Excel_study 1.csv")

politics <- plyr::rename(politics, c("participant_id"="id","AbstractID"="abstract"))
politics$target.f<-factor( politics$target, labels=c("con","lib") )

# select out non_native speakers who learned English after 10, and people who 
# say they didn't read carefully
politics$english_age<-as.numeric( politics$english_age )
politics<-subset( politics,
                  subset=( (native_english == "Yes" | english_age<10) & 
                             (read_carefully == "Yes") 
                           )
)

# copy and recode politics variables
politics$political_classification[politics$political_classification>7] <- NA
politics$social_issues[politics$social_issues>7] <- NA
politics$economic_issues[politics$economic_issues>7] <- NA
politics$political_party[politics$political_party>6] <- NA
politics$rater.pol<-rowMeans(cbind(politics$political_classification, 
                                   politics$social_issues, 
                                   politics$economic_issues,
                                   na.rm=TRUE)
)

# we preregistered that we would not do the composite for Ps with missing political classification
politics$rater.pol[is.na(politics$political_classification)] <- NA

# get rid of some -1 values, which mean "missing"
politics$eval[politics$eval == -1] <- NA
politics$explain[politics$explain == -1] <- NA

# different relevance-rating cutoffs
abstracts90 <- subset(abstracts, Prop.Yes >= .9)
abstracts80 <- subset(abstracts, Prop.Yes >= .8)
abstracts70 <- subset(abstracts, Prop.Yes >= .7)

# merge publication data
publications <- plyr::rename(publications, c("AbstractID"="abstract"))
politics <- merge(publications, politics, by="abstract", all.y = TRUE)
remove(publications)


# set below to restrict to more stringent rating cutoffs
# politics <- politics[politics$abstract %in% abstracts70$Abstract.No, ]

# Preregistered H1: Are conservatives evaluated more negatively? Significantly positive target dummy means "yes"
# rater and abstract are both random factors
model1<-lmer(eval ~ target.f + (1|id) + (1|abstract), data=politics)
summary(model1)

# Preregistered H2: Are conservatives explained more? Significantly _negative_ target dummy means "yes"
model2<-lmer(explain ~ target.f + (1|id) + (1|abstract), data=politics)
summary(model2)

# compute effect sizes for eval and explanatory bias
# variance components
veval<-as.data.frame(VarCorr(model1))
vexplain<-as.data.frame(VarCorr(model2))

# coefficient estimates
ceval<-as.data.frame(fixef(model1))
cexplain<-as.data.frame(fixef(model2))

# Estimate of ES (D2) is the "target" coefficient estimate divided by the square root 
# of the summed variance components (see Lai & Kwok, 2014, Eq. 9)
D2_eval = ceval["target.flib",] / sqrt(sum(veval[,"vcov"]))
D2_explain = cexplain["target.flib",] / sqrt(sum(vexplain[,"vcov"]))

D2_eval
D2_explain

# Preregistered H3: Does rater politics affect perceptions of explanatory/evaluative bias?
# Hypothesis is tested by target*social_issues interaction (preregistered)
model3<-lmer(eval ~ target.f + social_issues + target.f*social_issues + (1|id) + (1|abstract), data=politics)
summary(model3)

model4<-lmer(explain ~ target.f + social_issues + target.f*social_issues + (1|id) + (1|abstract), data=politics)
summary(model4)

# decompose interactions for models 3 and 4
model3_cons <-lmer(explain ~ social_issues + (1|id) + (1|abstract), data=subset(politics,target==0))
model3_lib <-lmer(explain ~ social_issues + (1|id) + (1|abstract), data=subset(politics,target==1))
model4_cons <-lmer(eval ~ social_issues + (1|id) + (1|abstract), data=subset(politics,target==0))
model4_lib <-lmer(eval ~ social_issues + (1|id) + (1|abstract), data=subset(politics,target==1))

summary(model3_cons)
summary(model3_lib)
summary(model4_cons)
summary(model4_lib)

# models with political conservatives only (Exploratory)
model1_consraters <-lmer(explain ~ target + (1|id) + (1|abstract), data=subset(politics,social_issues>4))
model2_consraters <-lmer(eval ~ target + (1|id) + (1|abstract), data=subset(politics,social_issues>4))

summary(model1_consraters)
summary(model2_consraters)

# Preregistered robustness checks with other measures of ideology - evaluative bias
model3_overall<-lmer(eval ~ target.f + political_classification + target.f*political_classification + (1|id) + (1|abstract),
                     data=politics)
model3_comp<-lmer(eval ~ target.f + rater.pol + target.f*rater.pol + (1|id) + (1|abstract),
                  data=politics)
model3_partyid<-lmer(eval ~ target.f + political_party + target.f*political_party + (1|id) + (1|abstract),
                     data=politics)

summary(model3_overall)
summary(model3_comp)
summary(model3_partyid)

# Preregistered robustness checks with other measures of ideology - explanatory bias
model4_overall<-lmer(explain ~ target.f + political_classification + target.f*political_classification + (1|id) + (1|abstract),
                     data=politics)
model4_comp<-lmer(explain ~ target.f + rater.pol + target.f*rater.pol + (1|id) + (1|abstract),
                  data=politics)
model4_partyid<-lmer(explain ~ target.f + political_party + target.f*political_party + (1|id) + (1|abstract),
                     data=politics)

summary(model4_overall)
summary(model4_comp)
summary(model4_partyid)

# Preregistred H4: Are evaluative and explanatory bias correlated at the abstract level?
explain_agg<-aggregate(explain~abstract+target, data=politics, mean, na.rm=TRUE)
eval_agg<-aggregate(eval~abstract+target, data=politics, mean, na.rm=TRUE)
pol_agg<-merge(eval_agg,explain_agg,by=c("abstract","target"))
remove(explain_agg,eval_agg)

# we need separate correlations for liberal (1) and conservative (0) targets
cor.test(pol_agg$eval[pol_agg$target==0],pol_agg$explain[pol_agg$target==0])
cor.test(pol_agg$eval[pol_agg$target==1],pol_agg$explain[pol_agg$target==1])

# Preregistered Exploratory H1: Does bias increase over time?
# center year, which may be unnecessary but uncentered generates a warning
politics$yearC <- scale(politics$year, scale = FALSE)

# evaluative differences
eval_time<-lmer(eval ~ target.f + yearC + target.f*yearC + (1|id) + (1|abstract), data=politics)
summary(eval_time)

# decompose interaction
eval_time_cons<-lmer(eval ~ yearC + (1|id) + (1|abstract), subset(politics,target==0) )
eval_time_lib<-lmer(eval ~ yearC + (1|id) + (1|abstract), subset(politics,target==1) )
summary(eval_time_cons)
summary(eval_time_lib)

# explanatory differences
explain_time<-lmer(explain ~ target.f + yearC + target.f*yearC + (1|id) + (1|abstract), data=politics)
summary(explain_time)

# decompose interaction
explain_time_cons<-lmer(explain ~ yearC + (1|id) + (1|abstract), subset(politics,target==0) )
explain_time_lib<-lmer(explain ~ yearC + (1|id) + (1|abstract), subset(politics,target==1) )
summary(explain_time_cons)
summary(explain_time_lib)

# Preregistered Exploratory H2: how much variance is explained by abstract vs. rater politics?
# note that we need to fit random slopes to answer the question: how much variance is explained by allowing for 
# differences across abstracts vs. allwoing differences across raters?

# baseline models w/o random slopes
eval_base<-model1
explain_base<-model2

# random slopes for for abstract
eval_rslope<-lmer(eval ~ target.f + (1|id) + (1 + target.f | abstract) , data=politics)
explain_rslope<-lmer(explain ~ target.f + (1|id) + (1 + target.f | abstract) , data=politics)

# random slopes for for abstract + rater politics
eval_rslope_poli<-lmer(eval ~ social_issues + target.f + target.f*social_issues + (1|id) + (1 + target.f | abstract) , 
                       data=politics)
explain_rslope_poli<-lmer(explain ~ social_issues + target.f + target.f*social_issues + (1|id) + (1 + target.f | abstract),
                          data=politics)

# evaluative bias
r.squaredGLMM(eval_base) 
r.squaredGLMM(eval_rslope) 
r.squaredGLMM(eval_rslope_poli) 

# explanatory bias
r.squaredGLMM(explain_base) 
r.squaredGLMM(explain_rslope) 
r.squaredGLMM(explain_rslope_poli) 

# graphing
plot(allEffects(model1))
plot(allEffects(model2))
plot(allEffects(model3))
plot(allEffects(model4))

# Reviewer requests - Descriptive stats
abstract.avg <- politics %>% group_by(abstract, target.f) %>% dplyr::summarize(eval = mean(eval, na.rm = TRUE), 
                                                                               explain = mean(explain, na.rm = TRUE)
                                                                               )
# save for plotting
abstract.plot <- abstract.avg

# reshape to wide, as.data.frame is needed because reshape() fails on tibbles
abstract.avg <- reshape(as.data.frame( abstract.avg), v.names = c("eval","explain"), idvar = "abstract", 
                        timevar = "target.f",  direction = "wide")

# merge in proportion rating abstracts as politics-relevant
abstract.avg <- merge(abstract.avg, abstracts, by.x="abstract", by.y="Abstract.No")

abstract.avg$eval.diff = abstract.avg$eval.lib - abstract.avg$eval.con
abstract.avg$explain.diff = abstract.avg$explain.lib - abstract.avg$explain.con

# code whether differences are greater or less than O
abstract.avg$eval.dummy <- ifelse(abstract.avg$eval.diff>0,1,0)
abstract.avg$explain.dummy <- ifelse(abstract.avg$explain.diff<0,1,0)

# six "explain" differences are exactly 0
abstract.avg$explain.dummy[abstract.avg$explain.diff==0]<-NA 

# percentage of abstracts evaluating liberals more favorably / explaining conservatives more
table(abstract.avg$eval.dummy)
sum(abstract.avg$eval.dummy)/length(abstract.avg$eval.dummy)

table(abstract.avg$explain.dummy)
sum(abstract.avg$explain.dummy==1, na.rm = TRUE)/length(abstract.avg$explain.dummy)
sum(abstract.avg$explain.dummy==0, na.rm = TRUE)/length(abstract.avg$explain.dummy)

# descriptives
describe(abstract.avg$eval.lib)
describe(abstract.avg$eval.con)
describe(abstract.avg$eval.diff)

describe(abstract.avg$explain.lib)
describe(abstract.avg$explain.con)
describe(abstract.avg$explain.diff)

# proportion of politics-relevant ratings does not correlate with rated bias
cor.test(abstract.avg$eval.diff, abstract.avg$Prop.Yes)
cor.test(abstract.avg$explain.diff, abstract.avg$Prop.Yes)

# plot of abstract-level rating averages
# Scatterplot with overlaid box
eval.plot <- ggplot(abstract.plot, aes(y=eval, x=target.f, color=target.f)) + 
  geom_jitter(aes(colour=target.f)) + geom_boxplot(outlier.shape = NA, alpha=0.5)

eval.plot <-  eval.plot + theme_bw() +
              scale_colour_manual(values=c("#9E2E2E","royalblue4")) + 
              scale_x_discrete( labels = c("Conservatives", "Liberals") ) +
              scale_y_continuous(limits = c(1, 7), expand = c(0,0)) +
              labs(y="Evaluative Rating", x="") + 
              theme(
                  text = element_text(size=16), 
                  panel.grid.major.x = element_blank(),
                  legend.position = "none"
              ) 

explain.plot <- ggplot(abstract.plot, aes(y=explain, x=target.f, color=target.f)) + 
  geom_jitter(aes(colour=target.f)) + geom_boxplot(outlier.shape = NA, alpha=0.5)

explain.plot <- explain.plot + theme_bw() +
                scale_colour_manual(values=c("#9E2E2E","royalblue4")) + 
                scale_x_discrete( labels = c("Conservatives", "Liberals") ) +
                scale_y_continuous(limits = c(1, 7), expand = c(0,0)) +
                labs(y="Explanatory Rating", x="") + 
                theme(
                    text = element_text(size=16), 
                    panel.grid.major.x = element_blank(),
                    legend.position = "none"
                ) 

grid.arrange(eval.plot,explain.plot, nrow = 1)

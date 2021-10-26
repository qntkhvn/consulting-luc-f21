#Loading tidyverse for cleaning/visualization
library(tidyverse)

#Data from openpsychometrics.org
data<-read.csv("Lutz-SEM-Presentation/DASS_data_21.02.19/data.csv",
               sep = "\t")

#view data
#Can see that we have answers (A) for each question along with response time (E)
#and order the questions were given in (I)
#We also have some basic demographics
str(data)
View(data)

#For this study, I am only interested in responses, along with gender, age, and
#marriage status (didn't end up using but considered this)

#Select only answers to 42 depression anxiety stress scale (and demos)
DASSans<-c(paste("Q", 1:42, "A", sep=""), "gender", "married", "age")
response<-data[DASSans]

str(response)

#The next step is to mutate these into variable type "factor"

#making gender and married factors
response<-mutate(response,
                 gender=factor(gender,
                               levels = c(1, 2, 3),
                               labels = c("Male", "Female", "Other")))
response<-mutate(response,
                 married=factor(married,
                                levels = c(1, 2, 3),
                                labels = c("Never Married", "Married", "Previously Married")))
str(response)

#Wanted to get an idea of demographics across marriage and gender status
response%>%
  group_by(married, gender)%>%
  summarize(n(), age=mean(age))

#Checking visualization
hist(data$age)
head(sort(data$age, decreasing=TRUE))

#Some people responded with their birth year as ages, so I'm deleting values 99 or greater
response<-filter(response, age<99)

head(sort(response$age, decreasing=TRUE))
#max age is now 89

hist(response$age)
#Looks like a ton of younger people take this (maybe college students for classes?)

response%>%
  group_by(married, gender)%>%
  summarize(n(), age=mean(age))

data%>%
  group_by(gender)%>%
  summarize(n())

response%>%
  group_by(gender)%>%
  summarize(n())

#Checked for missing values (doesn't seem to be any-did this for a few different Qs)
sum(is.na(response$Q1A))
sum(is.na(response$Q2A))
sum(is.na(response$Q3A))
sum(is.na(response$Q5A))
sum(is.na(response$Q10A))
sum(is.na(response$Q15A))
sum(is.na(response$Q20A))
sum(is.na(response$Q25A))
sum(is.na(response$Q30A))
sum(is.na(response$Q35A))
sum(is.na(response$Q40A))
sum(is.na(response$Q42A))

#While our SEM is going to do a better job of modeling these constructs than sum
#scores, it is still important to evaluate sum scores across groups

#creating scale and subscale scores
DASS<-paste("Q", 1:42, "A", sep="")
response$DASS<-rowSums(response[DASS])

Stress<-c("Q1A", "Q6A", "Q8A", "Q11A", "Q12A", "Q14A", "Q18A", 
          "Q22A", "Q27A", "Q29A", "Q32A", "Q33A", "Q35A", "Q39A")
response$Stress<-rowSums(response[Stress])

Anxiety<-c("Q2A", "Q4A", "Q7A", "Q9A", "Q15A", "Q19A", "Q20A",
           "Q23A","Q25A", "Q28A", "Q30A", "Q36A", "Q40A", "Q41A")
response$Anxiety<-rowSums(response[Anxiety])

Depression<-c("Q3A", "Q5A", "Q10A", "Q13A", "Q16A", "Q17A", "Q21A",
              "Q24A", "Q26A", "Q31A", "Q34A", "Q37A", "Q38A", "Q42A")
response$Depression<-rowSums(response[Depression])

#Viewing sum score differences across gender
response%>%
  group_by(gender)%>%
  summarize(DASS=mean(DASS),
            Stress=mean(Stress),
            Anxiety=mean(Anxiety),
            Depression=mean(Depression),
            count=n())

#For the sake of this presentation, I only looked at males and females, but you
#can see "Other" may be worth evaluating as well in future studies. For now, we
#will remove them.

#Removing NAs and Other
genders<-c("Male", "Female")
response<-filter(response, gender %in% genders)

#Viewing differences across gender (binary)
response%>%
  group_by(gender)%>%
  summarize(DASS=mean(DASS),
            Stress=mean(Stress),
            Anxiety=mean(Anxiety),
            Depression=mean(Depression),
            count=n())

#We know these t-tests will likely be significant because of our large sample size
#but it's still worth having these

t.test(DASS~gender, data=response)
t.test(Depression~gender, data=response)
t.test(Anxiety~gender, data=response)
t.test(Stress~gender, data=response)

#We can add visualizations to show differences across groups. These plots include
#overlaid boxplots and violin plots, which show distributions and median group
#differences

response%>%
  ggplot(aes(y=DASS, x=gender, fill=gender))+
  geom_violin(alpha=.1)+
  geom_boxplot(alpha = .3)+
  scale_fill_manual(values= c("blue", "red"))+
  ggtitle(label = element_text("DASS Total Scores"),
          subtitle = element_text("Males (M = 94.3); Females (M = 102.0)"))

response%>%
  ggplot(aes(y=Stress, x=gender, fill=gender))+
  geom_violin(alpha=.1)+
  geom_boxplot(alpha = .3)+
  scale_fill_manual(values= c("blue", "red"))+
  ggtitle(label = element_text("DASS Stress Scores"),
          subtitle = element_text("Males (M = 32.6); Females (M = 35.8)"))

response%>%
  ggplot(aes(y=Anxiety, x=gender, fill=gender))+
  geom_violin(alpha=.1)+
  geom_boxplot(alpha = .3)+
  scale_fill_manual(values= c("blue", "red"))+
  ggtitle(label = element_text("DASS Anxiety Scores"),
          subtitle = element_text("Males (M = 27.4); Females (M = 30.7)"))

response%>%
  ggplot(aes(y=Depression, x=gender, fill=gender))+
  geom_violin(alpha=.1)+
  geom_boxplot(alpha = .3)+
  scale_fill_manual(values= c("blue", "red"))+
  ggtitle(label = element_text("DASS Depression Scores"),
          subtitle = element_text("Males (M = 34.3); Females (M = 35.2)"))

#The sum score falls short, however, as it treats all items as equal contributors
#to the overall construct of "depression," "anxiety," and "stress."
#To expand on this concept, we need to evaluate the structural equation model

#loading SEM and diagramming packages
library(lavaan)
library(semPlot)

#Our first model will include all three latent factors as well as a higher-order
#DASS factor comprised of depression, stress, and anxiety
#We first create the model within ' ' then fit it using the cfa function.

#3-factor hierarchical model
DASS3H.model<- 'StressFac =~    Q1A + Q6A + Q8A + Q11A + Q12A + Q14A + Q18A +
                               Q22A + Q27A + Q29A + Q32A + Q33A + Q35A + Q39A
              AnxietyFac =~    Q2A + Q4A + Q7A + Q9A + Q15A + Q19A + Q20A +
                               Q23A + Q25A + Q28A + Q30A + Q36A + Q40A + Q41A
              DepressionFac =~ Q3A + Q5A + Q10A + Q13A + Q16A + Q17A + Q21A +
                               Q24A + Q26A + Q31A + Q34A + Q37A + Q38A + Q42A
              DASSFac =~ StressFac + AnxietyFac + DepressionFac'
DASS3H.fit <- cfa(model = DASS3H.model, data = response)

#We can also test whether the inclusion of the second order factor is worth it.
#The second model does not include the larger DASS factor and only has three
#first-order factors, which are all uncorrelated with one another

#3-factor one-level model (orthogonal)
DASS3.model<- 'StressFac =~    Q1A + Q6A + Q8A + Q11A + Q12A + Q14A + Q18A +
                               Q22A + Q27A + Q29A + Q32A + Q33A + Q35A + Q39A
              AnxietyFac =~    Q2A + Q4A + Q7A + Q9A + Q15A + Q19A + Q20A +
                               Q23A + Q25A + Q28A + Q30A + Q36A + Q40A + Q41A
              DepressionFac =~ Q3A + Q5A + Q10A + Q13A + Q16A + Q17A + Q21A +
                               Q24A + Q26A + Q31A + Q34A + Q37A + Q38A + Q42A
              DepressionFac~~0*StressFac
              AnxietyFac~~0*StressFac
              AnxietyFac~~0*DepressionFac'
DASS3.fit <- cfa(model = DASS3.model, data = response)

#We can also evaluate whether three factors are needed. Perhaps all 42 items are
#part of a single DASS factor, and there is no need for differentiating depression,
#stress, and anxiety

#1-factor model
DASS1.model<- 'DASSFac =~ Q1A + Q6A + Q8A + Q11A + Q12A + Q14A + Q18A +
                            Q22A + Q27A + Q29A + Q32A + Q33A + Q35A + Q39A +
                            Q2A + Q4A + Q7A + Q9A + Q15A + Q19A + Q20A +
                            Q23A + Q25A + Q28A + Q30A + Q36A + Q40A + Q41A +
                            Q3A + Q5A + Q10A + Q13A + Q16A + Q17A + Q21A +
                            Q24A + Q26A + Q31A + Q34A + Q37A + Q38A + Q42A'
DASS1.fit <- cfa(model = DASS1.model, data = response)

#Testing full model(3H) with orthogonal 3 factor model (3)
anova(DASS3.fit, DASS3H.fit)

#Testing full model (3H) with single latent factor model (1)
anova(DASS3H.fit, DASS1.fit)

#Smaller chi square suggests better fit for three-factor model
#We see that the three factors are important to include. We will move forward
#with these three factors in the model. In addition, the higher-order factor
#provides needed information to the model, since the orthogonal model is worse-
#fitting than the two-level model

#I have commented out the summaries for the worse-fitting models, but they can
#be viewed here if desired

#summary(DASS3.fit, standardized = T, fit.measures = T)
summary(DASS3H.fit, standardized = T, fit.measures = T)
#summary(DASS1.fit, standardized = T, fit.measures = T)
#Our fit measures are not perfect (RMSEA = .059, SRMR = .041, CFI = .898), but
#they are good enough to move forward with the example. We also get an idea of
#individual item loadings, second order loadings, and variances

#Our next step is visualizing our optimal model (also showing other examples)

#This is what our single DASS factor item looked like
semPaths(object = DASS1.fit,
         layout = "tree",
         rotation = 2,
         whatLabels = "std",
         edge.label.cex = .8,
#         what = "std",
#         edge.color = "blue",
#          sizeMan = 4,
#          sizeMan2 = 2
)

#This is our orthogonal 3-factor model (note all correlations between latent
#factors are zero). For this visualization (and all others afterward), I removed
#residuals, since they make the model more complicated. With fewer items, it
#might be worthwhile to include these.
semPaths(object = DASS3.fit,
         layout = "tree",
         rotation = 2,
         whatLabels = "std",
         edge.label.cex = 1,
         residuals = FALSE,
#         what = "std",
#         edge.color = "blue",
          sizeMan = 3,
          sizeMan2 = 1
)

#This next visualization is important if you only want to show latent factors
#I'm saving this for my own dissertation, which is more focused on the relationships
#between latent factors and less about individual item loadings
semPaths(object = DASS3H.fit,
         layout = "tree",
         rotation = 2,
         whatLabels = "std",
         edge.label.cex = 1,
         #                  what = "std",
         #                  edge.color = "blue",
#         panelGroups = FALSE,
         title = FALSE,
         ask = FALSE,
         intercepts = FALSE,
         residuals = FALSE,
         sizeMan = 3,
         sizeMan2 = 1,
         structural = TRUE
         #ABOVE COMMAND FOR DISSERTATION
         #        fixedStyle = c("red", 3),
         #        freeStyle = c("green", 1),
)

#Cleaned up version of optimal plot for PowerPoint presentation
library(semPlot)
semPaths(object = DASS3H.fit,
         layout = "tree",
         rotation = 2,
         whatLabels = "std",
         edge.label.cex = 1,
         residuals = FALSE,
         sizeMan = 3,
         sizeMan2 = 1)

#Next is getting into the invariance piece. We first create a model where all
#loadings are fixed to be equal across males and females (note the only added
#lines are "group =" and "group.equal =")
DASS3HInvEQ.fit <- cfa(model = DASS3H.model,
                     data = response,
                     group = "gender",
                     group.equal = "loadings")
#We can use this summary to check whether these loadings are indeed equal. We see
#that they are in the "Estimate column" of each group
summary(DASS3HInvEQ.fit, standardized = T, fit.measures = T)

#We then fit a model that does not force the loadings to be equal across groups
DASS3HInvUE.fit <- cfa(model = DASS3H.model,
                       data = response,
                       group = "gender")
#We now see in summary() that the loadings differ across males and females
summary(DASS3HInvUE.fit, standardized = T, fit.measures = T)

#We can then test whether it is "worth it" to estimate these additional 42
#parameters separately for each group. If it is not significantly diff., it is better
#to use the single estimate that is capable of representing both groups.
anova(DASS3HInvEQ.fit, DASS3HInvUE.fit)
#It is significant, so we opt for allowing loadings to differ across males and females

#We can then pull the estimates from our models and see where the differences are
#coming from. First, we double check and make sure there are no differences in the
#lambda (loading) matrix across genders.
EQmale_est<-lavInspect(DASS3HInvEQ.fit, what = "est",
           list.by.group = TRUE)$Male$lambda
EQfemale_est<-lavInspect(DASS3HInvEQ.fit, what = "est",
                       list.by.group = TRUE)$Female$lambda

EQlambda_diff<-female_est-male_est

EQlambda_diff
#Sure enough, the differences are all zero!

#For our unequal model, this is expected to be different across the two groups
#Our lambda_diff object will show us where the differences lie
UEmale_est<-lavInspect(DASS3HInvUE.fit, what = "est",
                       list.by.group = TRUE)$Male$lambda
UEfemale_est<-lavInspect(DASS3HInvUE.fit, what = "est",
                         list.by.group = TRUE)$Female$lambda

UElambda_diff<-UEfemale_est-UEmale_est

UElambda_diff
#Hard to see what's going on exactly, so I manipulated this a bit

DiffMat<-data.frame(UElambda_diff)
DiffMat<- mutate(DiffMat, 
                 difference=abs(StressFac+AnxietyFac+DepressionFac)*1000)
head(DiffMat[order(-DiffMat$difference),])
#We can now see that some of our largest contributors to the difference are:
#Q23  (Anxiety) Difficulty swallowing               (higher for females)
#Q9   (Anxiety) Situations most relieved when over  (higher for males)
#Q40  (Anxiety) Worried about making fool of self   (higher for males)
#Q4   (Anxiety) Breathing difficulties              (higher for females)
#Q15  (Anxiety) Feeling of faintness                (higher for females)
#Q18  (Stress)  Rather touchy                       (higher for males)

#While we could have explored any of these, I am moving forward with Q18
I18DASS3.fit<-cfa(model = DASS3H.model,
                  data = response,
                  group = "gender",
                  group.equal = "loadings",
                  group.partial = "StressFac=~Q18A")
summary(I18DASS3.fit)

#We see it is a better fit than when all items are forced to be equal
anova(DASS3HInvEQ.fit, I18DASS3.fit)

#But not as good as when all items are freely estimated
anova(DASS3HInvUE.fit, I18DASS3.fit)

#Future work could continue to explore item differences (see above) beyond just Q18

#Sample size (for presentation)
summarize(response, count=n())
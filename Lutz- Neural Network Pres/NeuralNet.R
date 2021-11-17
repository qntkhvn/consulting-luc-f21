#Loading tidyverse for cleaning/visualization
library(tidyverse)

#Plotting Sigmoid Function
x <- seq(-10, 10, .01)
y <- 1/(1+exp(-x))
plot(x, y)
abline(v = 0, lty = 2)

#Plotting sample ReLU function
x <- seq(-10, 10, .01)
y <- case_when(x<0 ~ 0,
               x >=0 ~ .5*x)
plot(x, y)
abline(v = 0, lty = 2)

#Data from openpsychometrics.org

data <- read.csv("/Users/lutznm/OneDrive - Loyola University Chicago/Fifth Year/Fall Classes/STAT 488/Neural Network Pres/DASS_data_21.02.19/data.csv",
                 sep = "\t")

#view data
#Can see that we have answers (A) for each question along with response time (E)
#and order the questions were given in (I)
#We also have some basic demographics
str(data)
View(data)

#For this study, I am only interested in DASS responses, along with gender, age, time elapsed,
#education, urban, voted, critical/quarrelsome personality (TIPI2), verbal scores, sexual orientation,
#marriage status, and country

#Select only answers to 42 depression anxiety stress scale (and demos)
DASSans<-c(paste("Q", 1:42, "A", sep=""), 
           paste("VCL", 1:16, sep = ""),
           "TIPI2", "testelapse", "education", "urban", "country",
           "gender", "married", "age", "orientation", "engnat", "voted")
response<-data[DASSans]

str(response)

response <- mutate(response,
                  gender_fac = factor(gender,
                                levels = c(1, 2),
                                labels = c("Male", "Female")),
                   married_fac =  factor(married,
                                  levels = c(1, 2),
                                  labels = c("Never Married", "Married")),
                   crit = TIPI2,
                   college = case_when(education <=2 ~ 0,
                                      education >=3 ~ 1),
                   college_fac = factor(college,
                                   levels = c(0, 1),
                                   labels = c("no college", "college")),
                  suburb = case_when(urban == 2 ~ 1,
                                     urban == 1 ~ 0,
                                     urban == 3 ~ 0),
                   urban2 = case_when(urban == 2 ~ 0,
                                     urban == 1 ~ 0,
                                     urban == 3 ~ 1),
                   us = case_when(country == "US" ~ 1,
                                 country != "US" ~ 0),
                   hetero = case_when(orientation == 1 ~ 1,
                                     orientation != 1 ~ 0),
                   EFL = case_when(engnat == 1 ~ 1,
                                  engnat == 2 ~ 0),
                   vote = case_when(voted == 1 ~ 1,
                                   voted == 2 ~ 0),
                   vote_fac = factor(vote, levels = c(0, 1), labels = c("No vote", "Voted")),
                   testelapse_minute = as.numeric(testelapse)/60)

#Checking age visualizations
hist(response$age)
head(sort(response$age, decreasing=TRUE))

#Some people responded with their birth year as ages, so I'm deleting values 99 or greater
response<-filter(response, age<99)

head(sort(response$age, decreasing=TRUE))
#max age is now 89

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

#VCL has three validity items (fake words at 6, 9, 12)
#Verbal is sum 0-13, which is automatically if they say they know a fake word
#Creating sums
Verbal <- paste("VCL", 1:16, sep = "")
response$Verbal <- rowSums(response[Verbal])
#Creating validity check
Invalid <- c("VCL6", "VCL9", "VCL12")
response$invalidverb <- rowSums(response[Invalid])

#Filter out invalid (> 0)
response <- filter(response, invalidverb == 0)

#Getting an idea of how long the participants took on the DASS questions (in minutes)
response%>%
  summarize  (min = min(testelapse_minute),
              max = max(testelapse_minute),
              mean = mean(testelapse_minute),
              sd = sd(testelapse_minute))

#Filtering out people who took the survey in under 1.5 minutes or over 15
head(sort(response$testelapse_minute))
sum(response$testelapse_minute<1.5)
head(sort(response$testelapse_minute, decreasing = TRUE))
sum(response$testelapse_minute>15)

response <-filter(response, testelapse_minute > 1.5)
response <-filter(response, testelapse_minute < 15)

response%>%
  group_by(vote_fac)%>%
  summarize(DASS = mean(DASS),
            Verbal = mean(Verbal),
            Critical = mean(crit),
            hetero = mean(hetero),
            college = mean(college))


#Looking at how DASS total scores impact voting
response%>%
  filter(vote_fac != "NA")%>%
  ggplot(aes(x = vote_fac, y = DASS, fill = vote_fac))+
  geom_violin(alpha = .1)+
  geom_boxplot(alpha = .5)

response%>%
  filter(vote_fac != "NA")%>%
  ggplot(aes(x = DASS, fill = vote_fac))+
  geom_density(alpha = .5)+
  facet_grid(vote_fac~.)

#Then how verbal scores impact voting
response%>%
  filter(vote_fac != "NA")%>%
  ggplot(aes(x = vote_fac, y = Verbal, fill = vote_fac))+
  geom_violin(alpha = .1, adjust = 2)+
  geom_boxplot(alpha = .5)

response%>%
  filter(vote_fac != "NA")%>%
  ggplot(aes(x = Verbal, fill = vote_fac))+
  geom_density(alpha = .5, adjust = 2)+
  facet_grid(vote_fac~.)

#Looking at critical and quarrelsome personality item
response%>%
  filter(vote_fac != "NA")%>%
  ggplot(aes(x = vote_fac, y = crit, fill = vote_fac))+
  geom_violin(alpha = .1, adjust = 2.5)+
  geom_boxplot(alpha = .5)

response%>%
  filter(vote_fac != "NA")%>%
  ggplot(aes(x = crit, fill = vote_fac))+
  geom_density(alpha = .5, adjust = 2.5)+
  facet_grid(vote_fac~.)

#Sexuality
response%>%
  filter(vote_fac != "NA")%>%
  ggplot(aes(x = hetero, fill = vote_fac))+
  geom_bar(stat = "count", alpha = .5, position = "fill")

#College
response%>%
  filter(vote_fac != "NA")%>%
  ggplot(aes(x = college_fac, fill = vote_fac))+
  geom_bar(stat = "count", alpha = .5, position = "fill")

#Install neural network package
install.packages("neuralnet")
library(neuralnet)

#Checking number who voted and those who didn't
response%>%
  group_by(vote_fac)%>%
  summarize(count = n())
#Removing NAs
response <- filter(response, vote_fac != "NA")

#Creating training dataset
summarize(response, count = n()) #32,749 in our dataset

#Dividing the dataset into 2/3 training and 1/3 testing
train <- sample(c(0, 1), 32749, replace = TRUE, prob = c(.33, .67))
response$train <- train
response%>%
  group_by(train)%>%
  summarize(count = n())

#21,991 in training; 10,668 in testing
training <- filter(response, train == 1)
testing <- filter(response, train == 0)

#Single hidden neuron (including run time)
start <- Sys.time()
nn1 <- neuralnet(vote ~ DASS + Verbal + crit + hetero + college,
                data = training,
                hidden = 1,
                threshold = .01,
                startweights = NULL,
                act.fct =  "logistic")
end <-Sys.time()
run <- end - start
run
plot(nn1)

#Single hidden layer (including run time)
#Removing crit, since it barely contributed
start <- Sys.time()
nn1 <- neuralnet(vote ~ DASS + Verbal + hetero + college,
                 data = training,
                 hidden = 1,
                 threshold = .01,
                 startweights = NULL,
                 act.fct =  "logistic")
end <-Sys.time()
run <- end - start
run
plot(nn1)

#Two hidden neurons
start <- Sys.time()
nn2 <- neuralnet(vote ~ DASS + Verbal + hetero + college,
                 data = training,
                 hidden = 2,
                 threshold = .01,
                 startweights = NULL,
                 act.fct =  "logistic")
end <- Sys.time()
run <- end - start
run
plot(nn2)

#Three hidden neurons
start <- Sys.time()
nn3 <- neuralnet(vote ~ DASS + Verbal + hetero + college,
                 data = training,
                 hidden = 3,
                 threshold = .01,
                 startweights = NULL,
                 act.fct =  "logistic")
end <- Sys.time()
run <- end - start
run
plot(nn3)

#Applying the three models to the test dataset
Predict1 <- compute(nn1, testing)
#net.result contains the probabilities of voting based on the neural network
prob1 <- Predict1$net.result
pred1 <-ifelse(prob1 > .5, 1, 0)
testing$pred1 <- pred1
testing$pred1Cor <- factor(abs(testing$pred1 - testing$vote),
                           levels = c(0, 1),
                           labels = c("Cor", "Incor"))
testing%>%
  group_by(pred1Cor)%>%
  summarize(correct = n(), prop.cor = n()/nrow(testing))

sum(testing$pred1)/nrow(testing)

#Note that a model that assumes no one voted is almost as accurate
pred2 <-ifelse(prob1 > 1, 1, 0)
testing$pred2 <- pred2
testing$pred2Cor <- factor(abs(testing$pred2 - testing$vote),
                           levels = c(0, 1),
                           labels = c("Cor", "Incor"))
testing%>%
  group_by(pred2Cor)%>%
  summarize(correct = n(), prop.cor = n()/nrow(testing))

#Doing the same with the two-neuron model
#Applying the three models to the test dataset
Predict2 <- compute(nn2, testing)
#net.result contains the probabilities of voting based on the neural network
prob2 <- Predict2$net.result
pred2 <-ifelse(prob2 > 0.5, 1, 0)
testing$pred2 <- pred2
testing$pred2Cor <- factor(abs(testing$pred2 - testing$vote),
                           levels = c(0, 1),
                           labels = c("Cor", "Incor"))
testing%>%
  group_by(pred2Cor)%>%
  summarize(correct = n(), prop.cor = n()/nrow(testing))

sum(testing$pred2)/nrow(testing)

#And again with the three-neuron model
#Applying the three models to the test dataset
Predict3 <- compute(nn3, testing)
#net.result contains the probabilities of voting based on the neural network
prob3 <- Predict3$net.result
pred3 <-ifelse(prob3 > 0.5, 1, 0)
testing$pred3 <- pred3
testing$pred3Cor <- factor(abs(testing$pred3 - testing$vote),
                           levels = c(0, 1),
                           labels = c("Cor", "Incor"))
testing%>%
  group_by(pred3Cor)%>%
  summarize(correct = n(), prop.cor = n()/nrow(testing))

sum(testing$pred3)/nrow(testing)

#New Model with multiple layers

#Making smaller training dataset
train2 <- sample(c(0, 1), 32749, replace = TRUE, prob = c(.9, .1))
response$train2 <- train2
response%>%
  group_by(train2)%>%
  summarize(count = n())

#3253 in training; 29496 in testing
training2 <- filter(response, train2 == 1)
testing2 <- filter(response, train2 == 0)

#Building larger model
start <- Sys.time()
nn3 <- neuralnet(vote ~ DASS + Verbal + hetero + college,
                 data = training2,
                 hidden = c(2, 3),
                 threshold = .01,
                 startweights = NULL,
                 act.fct =  "logistic")
end <- Sys.time()
run <- end - start
run
plot(nn3)

#Applying to test model
Predict3 <- compute(nn3, testing2)
#net.result contains the probabilities of voting based on the neural network
prob3 <- Predict3$net.result
pred3 <-ifelse(prob3 > 0.5, 1, 0)
testing2$pred3 <- pred3
testing2$pred3Cor <- factor(abs(testing2$pred3 - testing2$vote),
                           levels = c(0, 1),
                           labels = c("Cor", "Incor"))
testing2%>%
  group_by(pred3Cor)%>%
  summarize(correct = n(), prop.cor = n()/nrow(testing2))

sum(testing2$pred3)/nrow(testing2)
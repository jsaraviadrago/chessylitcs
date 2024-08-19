rm(list=ls())
# the link dl has to be changed to 1
game_chess <- "https://www.dropbox.com/s/5vrubrqhhvp5kyu/games_chess.csv?dl=1"



# Open libraries ####
library(data.table)
#library(bluegrafir)
library(tidyverse)
library(broom)
library(caret)
library(psych)
library(rms)
library(e1071) # required for confusion matrix
library(pROC)
library(klaR)
library(rpart)


# Open Data frame
data_chess <- fread(game_chess,
                    quote = "",
                    fill = T)

# Describe data to understand it
# names(data_chess)
# head(data_chess)
# str(data_chess)
# dim(data_chess)

##
# # Describing the data to understand it better
# 
# # Describing probable outcome variable (winner) 
# # Who wins
# grafi_distribution(data_chess$winner)
# 
# # Ways of winning (can be outcome variable)
# # Reasons for losing are unbalanced
# grafi_distribution(data_chess$victory_status)
# 
# # Understanding some variables
# # NUmber of moves in the opening phase
# grafi_distribution(data_chess$opening_ply)
# 
# # Check the openings types
# grafi_distribution(data_chess$opening_name)
# 
# # Standardized code for types of openings
# grafi_distribution(data_chess$opening_eco)
# 
# # Both variables are related to each other
# #data_chess$opening_eco == data_chess$opening_name

names(data_chess)
dim(data_chess)
prop.table(table(data_chess$winner))

##
## Create a training set ####

set.seed(12345)
data_chess_samples <-  data_chess$winner %>% 
  createDataPartition(p = 0.8, list = FALSE)
data_chess_train <- data_chess[data_chess_samples,]
data_chess_test <- data_chess[-data_chess_samples,]


# dim(data_chess_train)
# dim(data_chess_test)
# Calculate variables for underdogs

data_chess_train <- data_chess_train %>% mutate(
  underdog = case_when(
    white_rating > black_rating ~ "black",
    white_rating < black_rating ~ "white",
    TRUE ~ "No"))

# Filter winner or losers I am not interested in chess games that finished with draws
data_chess_train <- data_chess_train %>% 
  filter(winner != "draw")

# Recode winners to make analysis
data_chess_train$winner <- recode(data_chess_train$winner,
                                  "white" = "1",
                                  "black" = "0")

# Recode type of variable
data_chess_train$winner <- as.numeric(data_chess_train$winner)

# Doing some bivariate analysis to select some variables
# # It is related to white winning.
# m0 <- glm(winner ~ white_rating,
#           data = data_chess_train,
#           family = binomial)
# tidy(m0) # tidy table with broom
# 
# # It is inversely related to black winning. 
# m0.1 <- glm(winner ~ black_rating,
#             data =data_chess_train,
#             family = binomial)
# tidy(m0.1) # tidy table with broom

# Check how the underdog does against the theoretical better player

# Reorder reference group so whites are the reference group.
data_chess_train$underdog <- factor(data_chess_train$underdog,
                                    level= c("white","No","black"))

# Bivariate relationship between underdog and winner
# # Probability of underdog winning
# m0.2 <- glm(winner ~ underdog, 
#             data = data_chess_train,
#             family = binomial)
# tidy(m0.2) # tidy table with broom
# glance(m0.2) # tidy model fit with broom

##

# Are the amount of plays related to the winner?
#describeBy(data_chess_train$turns,
#           group = data_chess_train$winner)

# Make a simple histogram to understand the distribution of winner and amounts of plays
hist_cantidad <- ggplot(data_chess_train, aes(turns, group = winner))+
  geom_histogram(alpha=0.5, position="identity",
                 bins = 100)+
  theme(panel.background = element_blank())+
  labs(x = "Cantidad de turno",
       y = "Distribución")

ggsave("hist_cantidad.png", plot= hist_cantidad,
       width = 2, height = 2,
       limitsize = F)

# Visualize the relationship between winners and turns (super outlier detected)
ggplot(data_chess_train, aes(x=turns, y=winner))+
  geom_point()+
  theme(panel.background = element_blank()) +
  labs(x = "Turns",
       y = "Winner")

# Check for that outlier in order to recode it. 
#describeBy(data_chess_train$turns, data_chess_train$winner)

# Outlier detected
data_chess_train %>% 
  filter(turns == 349)

# Recoding outlier with the median because distributions are pretty skewed, the mean does not represent the center of the distribution
data_chess_train[data_chess_train$id=="pN0ioHNr",
                 "turns"] <- 54

# Centering variables to have interpretable zero and be able to eliminate intercept
data_chess_train$black_rating <- scale(data_chess_train$black_rating, center = T, scale = F)
data_chess_train$white_rating <- scale(data_chess_train$white_rating, center = T, scale = F)
data_chess_train$turns <- scale(data_chess_train$turns, center = T, scale = F)
data_chess_train$opening_ply <- scale(data_chess_train$opening_ply, center = T, scale = F)

# Check the type of relationship, is it linear, quadratic?
# # Linear
#mt <- glm(winner ~ turns,
#         data = data_chess_train)
#summary(mt)
# tidy(mt)
# glance(mt)
# lrm(mt)

# # Quadratic
# mt2 <- glm(winner ~ turns + I(turns^2),
#           data = data_chess_train)
# tidy(mt2)
# glance(mt2)
# 
# # Linear relationship seemed to be a good choice AIC does decrease much 
# 
# 
# 
# 
# # Underdog and turns
# m0.3 <- glm(winner ~ underdog + turns,
#             data = data_chess_train,
#             family = binomial)
# 
# tidy(m0.3)
# glance(m0.3)
# lrm(m0.3) # Pseudo R square

# Underdog, turns and elo (got to be careful with elo and multicollinearity) 

# check visually elo and underdog

# The scatterplot gives ideas that they are related
ggplot(data_chess_train, aes(x = underdog,
                             y =white_rating))+
  geom_point()+
  theme(panel.background = element_blank())+
  labs(x ="Underdog",
       y = "White rating")

# Trying a simple bivariate analysis of variance to further explore the relationship
# maov <- aov(white_rating ~ underdog,
#             data =data_chess_train)
# tidy(maov) # Results 
# glance(maov) # The R square does not show relationship
# TukeyHSD(maov) # There are difference between groups

# Introducing variables that seem to have a bivariate relationship with winning
# m0.4 <- glm(winner ~ underdog + turns + white_rating,
#             data = data_chess_train,
#             family = binomial)
# 
# tidy(m0.4)
# glance(m0.4)
# lrm(m0.4)

##
#cor.test(data_chess_train$black_rating,
#data_chess_train$white_rating) # Checking for multi colinearity

# Introducing more variables
# m0.5 <- glm(winner ~ underdog + turns + white_rating + black_rating,
#             data = data_chess_train,
#             family = binomial)
# tidy(m0.5)
# glance(m0.5)
# lrm(m0.5)

##
#Up to now I could stick with this model. 

# Lets try some models and different models and techniques to understand the relationship

#### Logistic Regression #####
m0.6 <- glm(winner ~ turns + white_rating + black_rating,
            data = data_chess_train,
            family = binomial)
# tidy(m0.6)
# glance(m0.6)
lrm(m0.6)

# Naive bayes ####
# Recode to fit the model using Naive Bayes
data_chess_train$winner <- as.factor(data_chess_train$winner)

# Fitting the model
mn1 <- NaiveBayes(winner ~ turns + white_rating + black_rating,
                  data = data_chess_train)

# Classiication tree ####

mct <- rpart(winner ~ turns + white_rating + black_rating,
             data = data_chess_train, method = "class")

par(xpd = NA) # Avoiding clipping the test in another place
plot(mct)
text(mct,digits=3)


### Diagnostics ####

data_chess_test <- data_chess_test %>% 
  filter(winner != "draw")

# Recoding as did in training set
data_chess_test$winner <- recode(data_chess_test$winner,
                                 "white" = "1",
                                 "black" = "0")

# Recoding type of variable
data_chess_test$winner <- as.numeric(data_chess_test$winner)

# Checking for similar relationships between variables 
ggplot(data_chess_test, aes(x=turns, y=winner))+
  geom_point()+
  theme(panel.background = element_blank()) +
  labs(x = "Turns",
       y = "Winner")

# Checking for similar outliers as the training set. 
#describeBy(data_chess_test$turns, data_chess_test$winner)

# Recoding the outlier
data_chess_test %>% 
  filter(turns == 349)

data_chess_test[data_chess_test$id=="pN0ioHNr",
                "turns"] <- 54

# Keeping uncetered value to plot afterwards (this line was typed after I know the result)
data_chess_test$turns_raw <- data_chess_test$turns


# Centering variables as training set
data_chess_test$black_rating <- scale(data_chess_test$black_rating, center = T, scale = F)
data_chess_test$white_rating <- scale(data_chess_test$white_rating, center = T, scale = F)
data_chess_test$turns <- scale(data_chess_test$turns, center = T, scale = F)

#### Predicting probabilities ####

# This should be the test set, predicting with glm
probabilities <-  m0.6 %>% 
  predict(data_chess_test,
          type = "response")
predicted.classes <- if_else(probabilities > 0.5, 1,
                             0)
# Prediction accuracy
observed.classes <- data_chess_test$winner
LGR<- mean(predicted.classes == observed.classes)

# Confusion matrix
table(observed.classes, predicted.classes)
table(observed.classes, predicted.classes) %>% 
  prop.table() %>% round(digits = 3)

# Precision, recall and specificity
predicted.classes <- as.factor(predicted.classes)
observed.classes <- as.factor(observed.classes)
confusionMatrix(predicted.classes, observed.classes,
                positive = "1")

### ROC curve and AUC GLM ####
res.roc <- roc(observed.classes, probabilities)
plot.roc(res.roc, print.auc = T)

### Model predictions in Naive Bayes ####
predNB <- mn1 %>% predict(data_chess_test)
### Model accuracy Naive Bayes ####
NB <- mean(predNB$class == data_chess_test$winner)

# Predictions on test set with the classification tree

predmct <- mct %>% 
  predict(data_chess_test, type = "class")
### Model accuracy
CT <- mean(predmct == data_chess_test$winner)


### Organizing results to see which one is more precise ####
# Judging by the precission I guess the logistic regresion is the best fitting model
Table_results <- data.frame(c(NB,LGR, CT))
nombres <- c("Logistic Regression", "Naive Bayes", "Classification trees")
Table_results <- tibble(Names = nombres,
                        results = c(LGR, NB, CT))

# Up to how many turns the probability of white winning changes?

data_chess_test$probabilities <-  m0.6 %>% 
  predict(data_chess_test,
          type = "response")

Cut_point <- ggplot(data_chess_test, aes(x=turns_raw,y =probabilities))+
  geom_point()+
  geom_smooth(method = "lm", formula = 'y ~ x')+
  geom_hline(yintercept=0.5, linetype="dashed", color = "red")+
geom_vline(xintercept = 90, linetype = "dashed", color = "green")+
  theme(panel.background = element_blank(), 
        axis.text.x = element_text(angle = 45))+
  labs(x= "Turns", y = "Probabilities")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1))+
  scale_x_continuous(breaks = seq(0,250, by =15))

ggsave("Cut_point.png", plot = Cut_point,
       width = 7, height = 7,
       limitsize = F)

# Check cutting point
data_chess_test <- data_chess_test %>%  
  mutate(
    Matches_turns = case_when(
      turns_raw <= 90 ~ "White has more chance of winning",
      turns_raw > 90 ~ "White has less chance of winning"))

data_chess_test_agregada <- group_by(data_chess_test, Matches_turns) %>%
  summarise(Cantidad_total = n())
  
data_chess_test_agregada_VS <- group_by(data_chess_test, victory_status,
                                        Matches_turns) %>%
  summarise(Cantidad = n())

  
### Getting some proportions of type of victory

data_chess_test_final <- left_join(data_chess_test_agregada_VS,
                                   data_chess_test_agregada, 
                                   by = "Matches_turns" )


data_chess_test_final$Proporcion <- data_chess_test_final$Cantidad/data_chess_test_final$Cantidad_total





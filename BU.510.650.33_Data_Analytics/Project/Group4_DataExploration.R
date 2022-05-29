setwd("~/Documents/JH_MBA/BU.510.650.33_Data_Analytics/Project")

require(data.table)
data = as.data.frame(fread("marketing_campaign.csv"))

# 2240 records
# 29 Columns

# Remove missing data
sum(is.na(data))
data = data[complete.cases(data), ]
sum(is.na(data))

summary(data) # Provides Quantilies and stats about each variable
sum(duplicated(data$ID)) # Each record is a unique customer

# What is the average household income of individuals purchasing wine via the company’s website versus directly in stores? 
web_customers = data[data$NumWebPurchases > 0 & data$NumStorePurchases == 0, ] # 5 Records
store_customers =  data[data$NumWebPurchases == 0 & data$NumStorePurchases > 0, ] # 38 Records
  # Insufficient data to answer this question - Small sample size, so take this with a grain of salt.
mean(web_customers$Income) # 32278.75
mean(store_customers$Income) # 36873.66


# Is there a correlation between the amount spent on wine versus other products? 
  # This can be answered by regressing wine purchases against other purchases and viewing the predictive power
lm_wine = lm(data$MntWines ~ 
               data$MntFishProducts+
               data$MntFruits+
               data$MntGoldProds+
               data$MntMeatProducts+
               data$MntSweetProducts)
summary(lm_wine)
# Multiple R-squared:  0.3627 -> Somewhat predictive
# P values for significance



# Is there a correlation between the number of kids and teens in a household and the number of purchases made with a discount? 
lm_discounts = lm(data$NumDealsPurchases ~ data$Kidhome+data$Teenhome)
summary(lm_discounts)
# P values are very indicative of relationship



# How many people visit the company’s website and how many people purchase through the website? 
web_visitors = data[data$NumWebVisitsMonth > 0, ] # 2206 visitors
web_purchasers = data[data$NumWebPurchases > 0, ] # 2168 purchasers



# Is there a correlation between the number of people who accepted the discount offer in the first campaign versus the number of people who accepted the offer in a later campaign? 
lm_acceptance = lm(data$AcceptedCmp1 ~
                     data$AcceptedCmp2+
                     data$AcceptedCmp3+
                     data$AcceptedCmp4+
                     data$AcceptedCmp5)
summary(lm_acceptance)
# P Values show they are indicative



### Feature Engineering ###
env_data = data[, c("MntWines", "Education", "Kidhome", "Teenhome", "Income", "Year_Birth", "Marital_Status", "Dt_Customer")]

# Convert Marital Status to Factor and clean up
unique(env_data$Marital_Status)
table(env_data$Marital_Status)
env_data[env_data$Marital_Status == "Alone", "Marital_Status"] = "Single"
env_data[env_data$Marital_Status == "Absurd" | 
           env_data$Marital_Status == "YOLO" | 
           env_data$Marital_Status == "Widow", "Marital_Status"] = "Other"
table(env_data$Marital_Status)


env_data$Marital_Status = factor(env_data$Marital_Status)
is.factor(env_data$Marital_Status)


# Calculate Age
require(stringr)
# Take the last 4 digits (i.e. Year) from transaction date
txn_year = strtoi(str_sub(env_data$Dt_Customer, -4, -1))
# Subtract the birth year to make the Age column
env_data$Age = txn_year - data$Year_Birth
# Clean up extra columns
env_data$Year_Birth = NULL
env_data$Dt_Customer = NULL


# Can we predict Wine Sales based on Family Environment
lm_wine_env = lm(env_data$MntWines ~
                   env_data$Education+
                   env_data$Kidhome+
                   env_data$Income+
                   env_data$Teenhome+
                   env_data$Age+
                   env_data$Marital_Status)
summary(lm_wine_env)
# Marital Status and age are not predictive
# Education, income, and children are predictive




# Can we predict who will complain
comp_data = env_data
comp_data$Complain = data$Complain
comp_data$MntWines = NULL

set.seed(0)
ind <- sample(2, nrow(comp_data), replace = TRUE, prob = c(0.8, 0.2))
comp_train <- comp_data[ind==1,]
comp_test <- comp_data[ind==2,]

logreg_comp = glm(Complain~., family=binomial, data=comp_train)
comp_probs = predict(logreg_comp, newdata=comp_test, type="response")

preds_original = rep(0, nrow(comp_test))
preds_original[comp_probs > 0.5] = 1
mean(preds_original==comp_test$Complain) # 99% match, but that is just the base rate - This model will not generalize
plot(comp_probs)

# Probabilities are very low due to imbalanced training data

# Balance the data
require(ROSE)
comp_balance <- ovun.sample(Complain~., data = comp_train, method = "over")$data
logreg_comp2 = glm(Complain~., family=binomial, data=comp_balance)
comp_probs2 = predict(logreg_comp2, newdata=comp_test, type="response")
plot(comp_probs2) # Much Better

preds = rep(0, nrow(comp_test))
preds[comp_probs2 > 0.5] = 1
mean(preds==comp_test$Complain) # We can predict who will complain about 63% of the time

# Compare with KNN Results
require(fastDummies)
comp_dummies = dummy_cols(comp_data)
comp_dummies$Education = NULL
comp_dummies$Marital_Status = NULL
dummy_train <- comp_dummies[ind==1,]
dummy_test <- comp_dummies[ind==2,]


require(class)
knn3 = knn(
  dummy_train[, !names(dummy_train) %in% c("Complain")],
  dummy_test[, !names(dummy_test) %in% c("Complain")] ,
  dummy_train$Complain, 3)
mean(knn3==dummy_test$Complain) # We can predict who will complain 98% of the time



setwd("~/Documents/JH_MBA/BU.510.650.33_Data_Analytics/Project")

require(data.table)
data = as.data.frame(fread("marketing_campaign.csv"))

# 2240 records
# 29 Columns

summary(data) # Provides Quantilies and stats about each variable
sum(duplicated(data$ID)) # Each record is a unique customer

# What is the average household income of individuals purchasing wine via the company’s website versus directly in stores? 
web_customers = data[data$NumWebPurchases > 0 & data$NumStorePurchases == 0, ] # 5 Records
store_customers =  data[data$NumWebPurchases == 0 & data$NumStorePurchases > 0, ] # 39 Records
  # Insufficient data to answer this question



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
web_visitors = data[data$NumWebVisitsMonth > 0, ] # 2229 visitors
web_purchasers = data[data$NumWebPurchases > 0, ] # 2191 purchasers



# Is there a correlation between the number of people who accepted the discount offer in the first campaign versus the number of people who accepted the offer in a later campaign? 
lm_acceptance = lm(data$AcceptedCmp1 ~
                     data$AcceptedCmp2+
                     data$AcceptedCmp3+
                     data$AcceptedCmp4+
                     data$AcceptedCmp5)
summary(lm_acceptance)
# P Values show they are indicative




# Instead of the last 2 years, what would change if the range of time increased to five years? 
  # I don't know what this is referring to


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
                   data$Marital_Status)
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
plot(comp_probs)
# Probabilities are very low due to imbalanced training data

# Balance the data
require(ROSE)
comp_balance <- ovun.sample(Complain~., data = comp_train, method = "over")$data
logreg_comp2 = glm(Complain~., family=binomial, data=comp_balance)
comp_probs2 = predict(logreg_comp2, newdata=comp_test, type="response")
plot(comp_probs2) # Much Better

preds = rep(0, nrow(comp_test))
preds[comp_probs2 < 0.5] = 1
mean(preds==comp_test$Complain) # We can predict who will complain about 36% of the time


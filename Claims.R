library(tidyverse)
library(dplyr)
library(ggplot2)
library(skimr)


claims_df <- readRDS(url('https://gmubusinessanalytics.netlify.app/data/claims_df.rds'))

### Raw Data
claims_df
summary(claims_df)
skim(claims_df)

# Exploratory Data Analysis

## Question 1

# **Question**: Which coverage is taken by most number of customers?

# **Answer**: There are 3 types of coverage Basic, Extended and Premium. Most people prefer taking the Basic coverage.

number_of_policy <- claims_df %>% 
  group_by(coverage) %>%
  count(coverage, sort = TRUE, name= 'number_of_people') 

ggplot(number_of_policy, aes(x = coverage, y = number_of_people, fill = coverage ))  +
  geom_col()


number_of_policy


# Question 2



# **Question**: Highest number of claims used by which residence_type? What are the most number of claims that people claim?
  
# **Answer**: The majority of suburban residents opt for the policy. The majority of residences take two claims. The majority of the residences in suburban area opt for 2 claims.

Residence_affect <- claims_df %>% 
  group_by(residence_type, total_claims) %>%
  count(residence_type, sort = TRUE, name= 'No_residence_who_have_claimed') %>%
  arrange(total_claims, residence_type) 

sum(Residence_affect$No_residence_who_have_claimed)
Residence_affect

ggplot(Residence_affect, aes(x = total_claims, y = No_residence_who_have_claimed, size = total_claims ))  +
  geom_point() +
  facet_wrap(~residence_type)

## Question 3



# **Question**: What is the amount that the policy has been active and then claimed?
  
  
# **Answer**: According to the analysis many customers have claimed within 12 months of investment.

cust_12_months <- claims_df %>%  
  filter(months_policy_active <= 12) %>%
  summarise(total_investment = months_policy_active * monthly_premium, total_claims_amount, customer_lifetime_value,                                months_policy_active )  %>% 
  arrange(customer_lifetime_value, months_policy_active)

cust_12_months


## Question 4

# **Question**: Does customer_state affect the number of claims?
#  **Answer**: Customers from California have done the majority of claims.

claims_state <- claims_df %>% 
  group_by(customer_state, total_claims) %>%
  count(total_claims, sort = TRUE, name= 'Number_of_people')%>%
  arrange(desc(Number_of_people), desc(total_claims))

claims_state

ggplot(claims_state, aes(x = customer_state , y = Number_of_people , color = total_claims, fill = total_claims)) +
  geom_bar(stat = "identity", alpha=.9, width=.3) + xlab("") +
  theme_bw() +
  coord_flip()


## Question 5

## **Question**:  Does income affect the number of claims?
  
## **Answer**: By the visualization we can see that a customer having more income have claimed more.

cust_income <- claims_df %>%
  group_by(customer_state) %>%
  summarise(mean(income))

cust_income


ggplot(claims_df, aes(x = income, y = total_claims_amount	)) +
  geom_smooth(color = "red") +
  theme_bw()


## Question 6

## **Question**: Which type of policy and coverage has taken by most of the customers?
  
##  **Answer**: Personal policy with basic coverage has been taken by most of the customers.

most_policy_coverage <- claims_df %>%  group_by(policy, coverage) %>%
  count(policy,coverage, sort = TRUE, name= 'no_of_cust_in_policy_coverage') %>%
  group_by(policy,coverage)





most_policy_coverage


ggplot(most_policy_coverage, aes(x = policy , y = coverage, color= no_of_cust_in_policy_coverage, size = no_of_cust_in_policy_coverage )) +
  geom_point() 


## Question 7

## **Question**: Which channel has sold which type of coverage the most?
##  **Answer**: According to my analysis the Agent channel has sold Basic coverage the most.

Most_sold_channel <- claims_df %>%
  group_by(sales_channel, coverage) %>%
  count(sales_channel, sort = TRUE, name= 'No_of_Sales_done') %>%
  arrange(sales_channel)


Most_sold_channel


ggplot(Most_sold_channel, aes(x = sales_channel, y = No_of_Sales_done, fill = coverage)) +
  geom_col() +
  scale_x_discrete(limits = c()) +coord_polar("y")

## Question 8

## **Question**: What specific policies have a higher number of claims ?
  
## **Answer**: 2 claims have been made by 2773 customers who have taken the Personal policy, which is the maximum , and 3 claims have been made by 1469 customers who have the same policy.
## Special policies have fewer claims.


high_claims <- claims_df %>% 
  group_by(total_claims) %>%
  count(policy, sort = TRUE, name= 'number_of_customers_taking_policy') %>%
  arrange(desc(number_of_customers_taking_policy)) 

high_claims



ggplot(high_claims, aes(x = total_claims  , y = number_of_customers_taking_policy, fill= policy)) +
  geom_tile(stat = "identity", alpha=.9, width=.3) + 
  theme_bw() 

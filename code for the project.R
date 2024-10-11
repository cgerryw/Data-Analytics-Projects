library(tidyverse)
library(glmnet)

colnames(Amazon_Sale_Report)[which(colnames(Amazon_Sale_Report) == "ship-state")] <- "ship_state"
colnames(Amazon_Sale_Report)[which(colnames(Amazon_Sale_Report) == "ship-city")] <- "ship_city"
colnames(Amazon_Sale_Report)[which(colnames(Amazon_Sale_Report) == "promotion-ids")] <- "promotion_ids"
colnames(Amazon_Sale_Report)[which(colnames(Amazon_Sale_Report) == "ship-service-level")] <- "ship_service_level"
colnames(Amazon_Sale_Report)[which(colnames(Amazon_Sale_Report) == "ship-postal-code")] <- 'ship_postal_code'
colnames(Amazon_Sale_Report)[which(colnames(Amazon_Sale_Report) == "fulfilled-by")] <- 'fulfilled_by'
colnames(Amazon_Sale_Report)[which(colnames(Amazon_Sale_Report) == "Unnamed: 22")] <- 'Unnamed'
colnames(Amazon_Sale_Report)[which(colnames(Amazon_Sale_Report) == "Amount")] <- "sales"
summary(Amazon_Sale_Report)
str(Amazon_Sale_Report)

#Remove NAs
data_clean <- Amazon_Sale_Report %>% 
  mutate(promotion_ids = ifelse(is.na(promotion_ids), "No Promotion", promotion_ids)) %>% #Assume that NA means there were no promotions used.
  mutate(fulfilled_by = ifelse(is.na(fulfilled_by), "No Data", fulfilled_by)) 

data_clean <- data_clean %>% filter(!is.na(ship_postal_code))

#Since we are focused on calculating Amount, we filter out the cancelled orders, unshipped and cancelled courier status.
#Also filter out amounts that are still 0 and consider that the data is unavailable.
data_clean <- data_clean %>%
  filter(Status != "Cancelled" & !is.na(Status)) %>% 
  filter(`Courier Status` != "Cancelled") %>% 
  filter(`Courier Status` != "Unshipped") %>% 
  filter(sales != 0) %>% 
  filter(`Sales Channel`== 'Amazon.in')

data_clean <- data_clean %>% 
  mutate(price = sales/Qty)
# Remove Unnamed Column
data_clean <- data_clean %>%
  select(-Unnamed)


library(stringr)
# Replace patterns in the promotion_ids column of data_clean
data_clean$promotion_ids <- data_clean$promotion_ids %>%
  # Remove any trailing characters for Amazon PLCC after "Amazon PLCC Free-Financing Universal Merchant"
  str_replace_all("Amazon PLCC Free-Financing Universal Merchant.*", "Amazon PLCC Free-Financing Universal Merchant") %>%
  
  # Remove trailing date strings and numbers after "N Core Free Shipping"
  str_replace_all("N Core Free Shipping.*", "N Core Free Shipping") %>%
  
  # Replace "VPC-*" with just "VPC"
  str_replace_all("VPC-[0-9]+-[0-9]+ Coupon", "VPC")



summary(data_clean)
View(data_clean)
#Change the required data into factors.
data_clean <- data_clean %>% 
  mutate(
    Fulfilment = factor(Fulfilment),
    ship_city  = factor(ship_city),
    ship_state = factor(ship_state),
    ship_service_level = factor(ship_service_level),
    B2B = factor(B2B),
    Status = factor(Status),
    promotion_ids = factor(promotion_ids),
    Category = factor(Category)
  ) %>% 
  filter(!is.na(sales))


# Use sub() to remove the trailing characters after 'Amazon PLCC Free-Financing Universal Merchant'

write.csv(data_clean, "C:/Files from Analytics Projects/data_clean.csv")

#Regression using glmnet

#y <- data_clean$sales
#x <- as.matrix(data_clean[, c("Qty", "Category", "Fulfilment", "ship_city", "ship_state", "ship_service_level", "B2B", "Status", "promotion_ids")])

#model <- glmnet(x, y, alpha = 0)
#cv_model <- cv.glmnet(x, y ,alpha = 0)
#best_lambda <- cv_model$lambda.min
#final_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
#print(coef(final_model))

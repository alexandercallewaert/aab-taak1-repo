setwd("~/Desktop/HIR - FASE 4/SEMESTER 2 /ADVANCED /ASSIGNMENT 1")
rm(list=ls())

library(lubridate)  # to convert to date
library(zoo)        # to convert to date
library(tidyr)      # to separate date columns 
library(tidyverse)  # to separate date columns 

train <- read_delim("test.csv", ";", escape_double = FALSE, trim_ws = TRUE)
attach(train)

## Make date variables ##

### year, month & day 
train$claim_date_registered <- ymd(train$claim_date_registered)
train$claim_date_occured <- ymd(train$claim_date_occured)

### only year & month
train$claim_vehicle_date_inuse <- format(lubridate::parse_date_time(train$claim_vehicle_date_inuse, 
                                                                    orders = c("Y/m")), "%Y-%m") 
train$claim_vehicle_date_inuse <- as.Date(as.yearmon(train$claim_vehicle_date_inuse, "%Y-%m"))

train$policy_date_start <- format(lubridate::parse_date_time(train$policy_date_start, 
                                                             orders = c("Y/m")), "%Y-%m") 
train$policy_date_start <- as.Date(as.yearmon(train$policy_date_start, "%Y-%m"))

train$policy_date_next_expiry <- format(lubridate::parse_date_time(train$policy_date_next_expiry, 
                                                                   orders = c("Y/m")), "%Y-%m")
train$policy_date_next_expiry <- as.Date(as.yearmon(train$policy_date_next_expiry, "%Y-%m"))

train$policy_date_last_renewed <- format(lubridate::parse_date_time(train$policy_date_last_renewed, 
                                                                    orders = c("Y/m")), "%Y-%m")
train$policy_date_last_renewed <- as.Date(as.yearmon(train$policy_date_last_renewed, "%Y-%m"))

str(train)


## Separate date columns ## 

### Year, Month & Day

train <- separate(train, claim_date_registered, c("claim_date_registered_Year", 
                                                  "claim_date_registered_Month", 
                                                  "claim_date_registered_Day"), sep = "-")
train <- separate(train, claim_date_occured, c("claim_date_occured_Year", 
                                               "claim_date_occured_Month", 
                                               "claim_date_occured_Day"), sep = "-")

### Year & Month

train <- separate(train, claim_vehicle_date_inuse, 
                  c("claim_vehicle_date_inuse_Year", "claim_vehicle_date_inuse_Month"),
                  sep = "-") 

train <- separate(train, policy_date_start, 
                  c("policy_date_start_Year","policy_date_start_Month"),
                  sep = "-")

train <- separate(train, policy_date_next_expiry, 
                  c("policy_date_next_expiry_Year", "policy_date_next_expiry_Month"),
                  sep = "-")

train <- separate(train, policy_date_last_renewed, 
                  c("policy_date_last_renewed_Year", "policy_date_last_renewed_Month"),
                  sep = "-")

str(train) # hier zie je dat alle data geconverteerd is naar characters, dus deze nog omzetten 
# naar numerieke waarden en we hebben compatibele waarden voor Random Forrest  


## Factorizing all variables & making Numeric ## 
               
train$fraud <- as.factor(train$fraud)                   # does not exist for test data!!                     
train$claim_amount <- as.numeric(train$claim_amount)    # does not exist for test data!!

train$claim_date_registered_Year  <- as.numeric(train$claim_date_registered_Year)    
train$claim_date_registered_Month <- as.numeric(train$claim_date_registered_Month)  
train$claim_date_registered_Day   <- as.numeric(train$claim_date_registered_Day) 
train$claim_date_occured_Year     <- as.numeric(train$claim_date_occured_Year)       
train$claim_date_occured_Month    <- as.numeric(train$claim_date_occured_Month)      
train$claim_date_occured_Day      <- as.numeric(train$claim_date_occured_Day) 

train$claim_time_occured  <- as.numeric(train$claim_time_occured)            
train$claim_postal_code   <- as.numeric(train$claim_postal_code)                 
train$claim_cause         <- as.factor(train$claim_cause)          
train$claim_liable        <- as.factor(train$claim_liable)          
train$claim_num_injured   <- as.numeric(train$claim_num_injured)                
train$claim_num_third_parties <- as.numeric(train$claim_num_third_parties)           
train$claim_num_vehicles  <- as.numeric(train$claim_num_vehicles)                
train$claim_police        <- as.factor(train$claim_police)            
train$claim_alcohol       <- as.factor(train$claim_alcohol)             
train$claim_language      <- as.factor(train$claim_language)

train$claim_vehicle_id        <- as.factor(train$claim_vehicle_id)           
train$claim_vehicle_brand     <- as.factor(train$claim_vehicle_brand)          
train$claim_vehicle_type      <- as.factor(train$claim_vehicle_type)          
train$claim_vehicle_date_inuse_Year <- as.numeric(train$claim_vehicle_date_inuse_Year)    
train$claim_vehicle_date_inuse_Month <- as.numeric(train$claim_vehicle_date_inuse_Month)    
train$claim_vehicle_cyl       <- as.numeric(train$claim_vehicle_cyl)                 
train$claim_vehicle_load      <- as.numeric(train$claim_vehicle_load)               
train$claim_vehicle_fuel_type <- as.factor(train$claim_vehicle_fuel_type)       
train$claim_vehicle_power     <- as.numeric(train$claim_vehicle_power)       

train$policy_holder_id          <- as.factor(train$policy_holder_id)      
train$policy_holder_postal_code <- as.numeric(train$policy_holder_postal_code)        
train$policy_holder_form        <- as.factor(train$policy_holder_form)    
train$policy_holder_year_birth  <- as.numeric(train$policy_holder_year_birth)          
train$policy_holder_country     <- as.factor(train$policy_holder_country )    
train$policy_holder_expert_id   <- as.factor(train$policy_holder_expert_id) 

train$driver_id           <- as.factor(train$driver_id)          
train$driver_postal_code  <- as.numeric(train$driver_postal_code )                
train$driver_form         <- as.factor(train$driver_form)          
train$driver_year_birth   <- as.numeric(train$driver_year_birth )                
train$driver_country      <- as.factor(train$driver_country)          
train$driver_expert_id    <- as.factor(train$driver_expert_id)          
train$driver_injured      <- as.factor(train$driver_injured )          
train$driver_vehicle_id   <- as.factor(train$driver_vehicle_id)          

train$third_party_1_id          <- as.factor(train$third_party_1_id)             
train$third_party_1_postal_code <- as.numeric(train$third_party_1_postal_code)         
train$third_party_1_injured     <- as.factor(train$third_party_1_injured)    
train$third_party_1_vehicle_type <- as.factor(train$third_party_1_vehicle_type)    
train$third_party_1_form        <- as.factor(train$third_party_1_form)    
train$third_party_1_year_birth  <- as.numeric(train$third_party_1_year_birth)      
train$third_party_1_country     <- as.factor(train$third_party_1_country)    
train$third_party_1_vehicle_id  <- as.factor(train$third_party_1_vehicle_id)    
train$third_party_1_expert_id   <- as.factor(train$third_party_1_expert_id) 

train$third_party_2_id          <- as.factor(train$third_party_2_id)             
train$third_party_2_postal_code <- as.numeric(train$third_party_2_postal_code)         
train$third_party_2_injured     <- as.factor(train$third_party_2_injured)    
train$third_party_2_vehicle_type <- as.factor(train$third_party_2_vehicle_type)    
train$third_party_2_form        <- as.factor(train$third_party_2_form)    
train$third_party_2_year_birth  <- as.numeric(train$third_party_2_year_birth)      
train$third_party_2_country     <- as.factor(train$third_party_2_country)    
train$third_party_2_vehicle_id  <- as.factor(train$third_party_2_vehicle_id)    
train$third_party_2_expert_id   <- as.factor(train$third_party_2_expert_id) 

train$third_party_3_id          <- as.factor(train$third_party_3_id)             
train$third_party_3_postal_code <- as.numeric(train$third_party_3_postal_code)         
train$third_party_3_injured     <- as.factor(train$third_party_3_injured)    
train$third_party_3_vehicle_type <- as.factor(train$third_party_3_vehicle_type)    
train$third_party_3_form        <- as.factor(train$third_party_3_form)    
train$third_party_3_year_birth  <- as.numeric(train$third_party_3_year_birth)      
train$third_party_3_country     <- as.factor(train$third_party_3_country)    
train$third_party_3_vehicle_id  <- as.factor(train$third_party_3_vehicle_id)    
train$third_party_3_expert_id   <- as.factor(train$third_party_3_expert_id) 

train$repair_id           <- as.factor(train$repair_id)          
train$repair_postal_code  <- as.numeric(train$repair_postal_code)               
train$repair_form         <- as.factor(train$repair_form )           
train$repair_year_birth   <- as.numeric(train$repair_year_birth)          
train$repair_country      <- as.factor(train$repair_country)          
train$repair_sla          <- as.factor(train$repair_sla)

train$policy_date_start_Year  <- as.numeric(train$policy_date_start_Year)          
train$policy_date_start_Month <- as.numeric(train$policy_date_start_Month)           
train$policy_date_next_expiry_Year  <- as.numeric(train$policy_date_next_expiry_Year)    
train$policy_date_next_expiry_Month <- as.numeric(train$policy_date_next_expiry_Month)    
train$policy_date_last_renewed_Year <- as.numeric(train$policy_date_last_renewed_Year)    
train$policy_date_last_renewed_Month <- as.numeric(train$policy_date_last_renewed_Month)    
train$policy_num_changes      <- as.numeric(train$policy_num_changes)            
train$policy_num_claims       <- as.numeric(train$policy_num_claims)                
train$policy_premium_100      <- as.numeric(train$policy_premium_100)           
train$policy_coverage_1000    <- as.numeric(train$policy_coverage_1000)             
train$policy_coverage_type    <- as.factor(train$policy_coverage_type)

str(train)

save(train, file = "train.RData")
save(train, file = "test.RData") # change line 9 to "test.csv" instead of "train.csv" and run


setwd("~/github/aab-taak1-repo/data")
rm(list=ls())

library(lubridate)  # to convert to date
library(zoo)        # to convert to date
library(tidyr)      # to separate date columns 
library(tidyverse)  # to separate date columns 

train <- read_delim("train.csv", ";", escape_double = FALSE, trim_ws = TRUE)
test <- read_delim("test.csv", ";", escape_double = FALSE, trim_ws = TRUE)

#############
### TRAIN ###
#############

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
train$policy_coverage_type    <- gsub("#","",as.character(train$policy_coverage_type))
train$policy_coverage_type    <- as.numeric(train$policy_coverage_type)


#### Overbodige kolommen! #### 

# als je de data goed analyseert, dan merk je op dat kolommen "policy_date_last_renewed_Year", 
# "policy_date_last_renewed_Month" helemaal hetzelfde zijn als 

table(train$policy_date_last_renewed_Month == train$policy_date_next_expiry_Month)
table(train$policy_date_last_renewed_Year == train$policy_date_next_renewed_Year)

# Die twee kolommen zijn dus afhankelijk van een andere kolom, dus kan je zonder verlies 
# van algemeenheid verwijderen uit de dataset!

deleteCol <- which(colnames(train) == c("policy_date_last_renewed_Month", 
                                        "policy_date_last_renewed_Year"))

train <- train[,-deleteCol]

#### Overbodige rijen! #### 

train <- train %>% 
  filter(as.character(train$driver_vehicle_id) == as.character(train$claim_vehicle_id))

str(train)
detach(train)

save(train, file = "train.RData")

############
### TEST ###
############

attach(test)

## Make date variables ##

### year, month & day 
test$claim_date_registered <- ymd(test$claim_date_registered)
test$claim_date_occured <- ymd(test$claim_date_occured)

### only year & month
test$claim_vehicle_date_inuse <- format(lubridate::parse_date_time(test$claim_vehicle_date_inuse, 
                                                                    orders = c("Y/m")), "%Y-%m") 
test$claim_vehicle_date_inuse <- as.Date(as.yearmon(test$claim_vehicle_date_inuse, "%Y-%m"))

test$policy_date_start <- format(lubridate::parse_date_time(test$policy_date_start, 
                                                             orders = c("Y/m")), "%Y-%m") 
test$policy_date_start <- as.Date(as.yearmon(test$policy_date_start, "%Y-%m"))

test$policy_date_next_expiry <- format(lubridate::parse_date_time(test$policy_date_next_expiry, 
                                                                   orders = c("Y/m")), "%Y-%m")
test$policy_date_next_expiry <- as.Date(as.yearmon(test$policy_date_next_expiry, "%Y-%m"))

test$policy_date_last_renewed <- format(lubridate::parse_date_time(test$policy_date_last_renewed, 
                                                                    orders = c("Y/m")), "%Y-%m")
test$policy_date_last_renewed <- as.Date(as.yearmon(test$policy_date_last_renewed, "%Y-%m"))

str(test)


## Separate date columns ## 

### Year, Month & Day

test <- separate(test, claim_date_registered, c("claim_date_registered_Year", 
                                                  "claim_date_registered_Month", 
                                                  "claim_date_registered_Day"), sep = "-")
test <- separate(test, claim_date_occured, c("claim_date_occured_Year", 
                                               "claim_date_occured_Month", 
                                               "claim_date_occured_Day"), sep = "-")

### Year & Month

test <- separate(test, claim_vehicle_date_inuse, 
                  c("claim_vehicle_date_inuse_Year", "claim_vehicle_date_inuse_Month"),
                  sep = "-") 

test <- separate(test, policy_date_start, 
                  c("policy_date_start_Year","policy_date_start_Month"),
                  sep = "-")

test <- separate(test, policy_date_next_expiry, 
                  c("policy_date_next_expiry_Year", "policy_date_next_expiry_Month"),
                  sep = "-")

test <- separate(test, policy_date_last_renewed, 
                  c("policy_date_last_renewed_Year", "policy_date_last_renewed_Month"),
                  sep = "-")

str(test) # hier zie je dat alle data geconverteerd is naar characters, dus deze nog omzetten 
# naar numerieke waarden en we hebben compatibele waarden voor Random Forest  


## Factorizing all variables & making Numeric ## 

test$claim_date_registered_Year  <- as.numeric(test$claim_date_registered_Year)    
test$claim_date_registered_Month <- as.numeric(test$claim_date_registered_Month)  
test$claim_date_registered_Day   <- as.numeric(test$claim_date_registered_Day) 
test$claim_date_occured_Year     <- as.numeric(test$claim_date_occured_Year)       
test$claim_date_occured_Month    <- as.numeric(test$claim_date_occured_Month)      
test$claim_date_occured_Day      <- as.numeric(test$claim_date_occured_Day) 

test$claim_time_occured  <- as.numeric(test$claim_time_occured)            
test$claim_postal_code   <- as.numeric(test$claim_postal_code)                 
test$claim_cause         <- as.factor(test$claim_cause)          
test$claim_liable        <- as.factor(test$claim_liable)          
test$claim_num_injured   <- as.numeric(test$claim_num_injured)                
test$claim_num_third_parties <- as.numeric(test$claim_num_third_parties)           
test$claim_num_vehicles  <- as.numeric(test$claim_num_vehicles)                
test$claim_police        <- as.factor(test$claim_police)            
test$claim_alcohol       <- as.factor(test$claim_alcohol)             
test$claim_language      <- as.factor(test$claim_language)

test$claim_vehicle_id        <- as.factor(test$claim_vehicle_id)           
test$claim_vehicle_brand     <- as.factor(test$claim_vehicle_brand)          
test$claim_vehicle_type      <- as.factor(test$claim_vehicle_type)          
test$claim_vehicle_date_inuse_Year <- as.numeric(test$claim_vehicle_date_inuse_Year)    
test$claim_vehicle_date_inuse_Month <- as.numeric(test$claim_vehicle_date_inuse_Month)    
test$claim_vehicle_cyl       <- as.numeric(test$claim_vehicle_cyl)                 
test$claim_vehicle_load      <- as.numeric(test$claim_vehicle_load)               
test$claim_vehicle_fuel_type <- as.factor(test$claim_vehicle_fuel_type)       
test$claim_vehicle_power     <- as.numeric(test$claim_vehicle_power)       

test$policy_holder_id          <- as.factor(test$policy_holder_id)      
test$policy_holder_postal_code <- as.numeric(test$policy_holder_postal_code)        
test$policy_holder_form        <- as.factor(test$policy_holder_form)    
test$policy_holder_year_birth  <- as.numeric(test$policy_holder_year_birth)          
test$policy_holder_country     <- as.factor(test$policy_holder_country )    
test$policy_holder_expert_id   <- as.factor(test$policy_holder_expert_id) 

test$driver_id           <- as.factor(test$driver_id)          
test$driver_postal_code  <- as.numeric(test$driver_postal_code )                
test$driver_form         <- as.factor(test$driver_form)          
test$driver_year_birth   <- as.numeric(test$driver_year_birth )                
test$driver_country      <- as.factor(test$driver_country)          
test$driver_expert_id    <- as.factor(test$driver_expert_id)          
test$driver_injured      <- as.factor(test$driver_injured )          
test$driver_vehicle_id   <- as.factor(test$driver_vehicle_id)          

test$third_party_1_id          <- as.factor(test$third_party_1_id)             
test$third_party_1_postal_code <- as.numeric(test$third_party_1_postal_code)         
test$third_party_1_injured     <- as.factor(test$third_party_1_injured)    
test$third_party_1_vehicle_type <- as.factor(test$third_party_1_vehicle_type)    
test$third_party_1_form        <- as.factor(test$third_party_1_form)    
test$third_party_1_year_birth  <- as.numeric(test$third_party_1_year_birth)      
test$third_party_1_country     <- as.factor(test$third_party_1_country)    
test$third_party_1_vehicle_id  <- as.factor(test$third_party_1_vehicle_id)    
test$third_party_1_expert_id   <- as.factor(test$third_party_1_expert_id) 

test$third_party_2_id          <- as.factor(test$third_party_2_id)             
test$third_party_2_postal_code <- as.numeric(test$third_party_2_postal_code)         
test$third_party_2_injured     <- as.factor(test$third_party_2_injured)    
test$third_party_2_vehicle_type <- as.factor(test$third_party_2_vehicle_type)    
test$third_party_2_form        <- as.factor(test$third_party_2_form)    
test$third_party_2_year_birth  <- as.numeric(test$third_party_2_year_birth)      
test$third_party_2_country     <- as.factor(test$third_party_2_country)    
test$third_party_2_vehicle_id  <- as.factor(test$third_party_2_vehicle_id)    
test$third_party_2_expert_id   <- as.factor(test$third_party_2_expert_id) 

test$third_party_3_id          <- as.factor(test$third_party_3_id)             
test$third_party_3_postal_code <- as.numeric(test$third_party_3_postal_code)         
test$third_party_3_injured     <- as.factor(test$third_party_3_injured)    
test$third_party_3_vehicle_type <- as.factor(test$third_party_3_vehicle_type)    
test$third_party_3_form        <- as.factor(test$third_party_3_form)    
test$third_party_3_year_birth  <- as.numeric(test$third_party_3_year_birth)      
test$third_party_3_country     <- as.factor(test$third_party_3_country)    
test$third_party_3_vehicle_id  <- as.factor(test$third_party_3_vehicle_id)    
test$third_party_3_expert_id   <- as.factor(test$third_party_3_expert_id) 

test$repair_id           <- as.factor(test$repair_id)          
test$repair_postal_code  <- as.numeric(test$repair_postal_code)               
test$repair_form         <- as.factor(test$repair_form )           
test$repair_year_birth   <- as.numeric(test$repair_year_birth)          
test$repair_country      <- as.factor(test$repair_country)          
test$repair_sla          <- as.factor(test$repair_sla)

test$policy_date_start_Year  <- as.numeric(test$policy_date_start_Year)          
test$policy_date_start_Month <- as.numeric(test$policy_date_start_Month)           
test$policy_date_next_expiry_Year  <- as.numeric(test$policy_date_next_expiry_Year)    
test$policy_date_next_expiry_Month <- as.numeric(test$policy_date_next_expiry_Month)    
test$policy_date_last_renewed_Year <- as.numeric(test$policy_date_last_renewed_Year)    
test$policy_date_last_renewed_Month <- as.numeric(test$policy_date_last_renewed_Month)    
test$policy_num_changes      <- as.numeric(test$policy_num_changes)            
test$policy_num_claims       <- as.numeric(test$policy_num_claims)                
test$policy_premium_100      <- as.numeric(test$policy_premium_100)           
test$policy_coverage_1000    <- as.numeric(test$policy_coverage_1000)    

test$policy_coverage_type    <- as.factor(test$policy_coverage_type)
test$policy_coverage_type    <- gsub("#","",as.character(test$policy_coverage_type))
test$policy_coverage_type    <- as.numeric(test$policy_coverage_type)

#### Overbodige kolommen! #### 

# als je de data goed analyseert, dan merk je op dat kolommen "policy_date_last_renewed_Year", 
# "policy_date_last_renewed_Month" helemaal hetzelfde zijn als 

table(test$policy_date_last_renewed_Month == test$policy_date_next_expiry_Month)
table(test$policy_date_last_renewed_Year == test$policy_date_next_renewed_Year)

# Die twee kolommen zijn dus afhankelijk van een andere kolom, dus kan je zonder verlies 
# van algemeenheid verwijderen uit de dataset!

deleteCol <- which(colnames(test) == c("policy_date_last_renewed_Month", 
                                       "policy_date_last_renewed_Year"))

test <- test[,-deleteCol]

str(test)
detach(test)
save(test, file = "test.RData")


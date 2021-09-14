################################################################################
##############################################################################
###########################################################################
### Project: Predicting Electricity Consumption in Kosovo
### Methodology: Machine Learning Regression Model
### Data Scientist: Taulant Elshani
### Place: Vienna, Austria
### Date: 17/09/2021
##############################################################################
###############################################################################
################################################################################

### Exploring and Preparing Data

## Loading dataset

library(readxl)

electricity <- read_excel("Electricity_Generation 1953-2021.xlsx")

View(electricity)

## Data Wrangling

library(dplyr)

electricity_d <- select(electricity, Electricity_Consumption_GWh, 
                        
                        Coal_Production_in_Tons, Electricity_Production_GWh, 
                        
                        Electricity_Network_Coverage_%, GDP_per_Capita, Population)

# Performing some descriptive statistical operations

str(electricity)

summary(electricity$Electricity_Consumption_GWh)

summary(electricity$Electricity_Production_GWh)

# Graphic representation of the variable we are predicting

hist(electricity$Electricity_Consumption_GWh, 
     
     main = "Electricity Consumption in Kosovo 1953 - 2020", 
     
     xlab = "Electricity Consumption in GWh", 
     
     col = "pink")

table(electricity$Electricity_Consumption_GWh)

### Exploring Relationships Between Features

cor(electricity[c("Coal_Production_ in_Tons", "Electricity_Production_GWh", 
                  
                  "Electricity_Consumption_GWh", 
                  
                  "Electricity_Network_Coverage_%", 
                  
                  "GDP_per_Capita", "Population")])

### Visualizing Relationships Among Features

pairs(electricity[c("Coal_Production_ in_Tons", "Electricity_Production_GWh", 
                    
                    "Electricity_Consumption_GWh", 
                    
                    "Electricity_Network_Coverage_%", 
                    
                    "GDP_per_Capita", "Population")])

## Adding more information by downloading and using tha package "Psych"

install.packages("psych")

library(psych)

# Analyzing correlation by using the "Psych" package

pairs.panels(electricity[c("Coal_Production_ in_Tons", "Electricity_Production_GWh", 
                           
                           "Electricity_Consumption_GWh", 
                           
                           "Electricity_Network_Coverage_%", 
                           
                           "GDP_per_Capita", "Population")])

### Training a Model on the Data

electricity_model <- lm(electricity$Electricity_Consumption_GWh ~ electricity$`Coal_Production_ in_Tons` +
                          
                          electricity$Electricity_Production_GWh + 
                          
                          electricity$`Electricity_Network_Coverage_%` +
                          
                          electricity$GDP_per_Capita + electricity$Population,
                        
                        data = electricity)

print(electricity_model)

### Evaluating and Interpreting Model Performance

summary(electricity_model)

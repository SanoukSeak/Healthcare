getwd()
py <- read.csv("PY_Financial_and_Quality_Results_2023.csv", header = TRUE, sep = ",")
py

----------------------
  
-- O1.1: Cluster Analysis of Average Risk Scores and Per Capita Expenditures
  
  # Load necessary libraries 
  
  library(dplyr) 

library(ggplot2) 

library(cluster) 

library(scales) 



# Convert relevant columns to numeric (ensure no non-numeric data) 

data_cleaned <- data %>% 
  
  mutate(across(starts_with("CMS_HCC_RiskScore"), as.numeric, .names = "clean_{.col}"), 
         
         across(starts_with("Per_Capita_Exp"), as.numeric, .names = "clean_{.col}")) 



# Aggregate the data to get the average risk scores and per capita expenditures across all years 

data_avg <- data_cleaned %>% 
  
  select(starts_with("clean_CMS_HCC_RiskScore"),  
         
         starts_with("clean_Per_Capita_Exp")) %>% 
  
  mutate(Average_Risk_Score_All_Years = rowMeans(select(., starts_with("clean_CMS_HCC_RiskScore")),  
                                                 
                                                 na.rm = TRUE), 
         
         Average_Per_Capita_Exp_All_Years = rowMeans(select(., starts_with("clean_Per_Capita_Exp")),  
                                                     
                                                     na.rm = TRUE)) 



# Select the relevant variables for clustering 

data_cluster <- data_avg %>% 
  
  select(Average_Risk_Score_All_Years, Average_Per_Capita_Exp_All_Years) 



# Handle missing data if any 

data_cluster <- na.omit(data_cluster) 



# Perform K-means clustering (set 3 clusters as an example) 

set.seed(123)  # Set seed for reproducibility 

kmeans_result <- kmeans(data_cluster, centers = 3) 



# Add the cluster results back to the data 

data_cluster$Cluster <- as.factor(kmeans_result$cluster) 



# Visualize the clustering results using ggplot2 

ggplot(data_cluster, aes(x = Average_Risk_Score_All_Years,  
                         
                         y = Average_Per_Capita_Exp_All_Years,  
                         
                         color = Cluster)) + 
  
  geom_point(size = 3) + 
  
  labs(title = "Cluster Analysis of Average Risk Scores and Per Capita Expenditures", 
       
       x = "Average Risk Score (All Years)",  
       
       y = "Average Per Capita Expenditure (All Years)", 
       
       color = "Year") +  # Change legend title to "Year" 
  
  theme_minimal() + 
  
  scale_color_manual(values = c("red", "blue", "darkgreen")) + 
  
  scale_y_continuous(breaks = seq(0, max(data_cluster$Average_Per_Capita_Exp_All_Years), by = 5000), 
                     
                     labels = scales::comma) +  # Format y-axis labels with commas 
  
  scale_x_continuous(breaks = seq(0, max(data_cluster$Average_Risk_Score_All_Years), by = 0.1)) +  # Set x-axis major breaks to 0.1 
  
  theme(legend.position = "top",  # Place the legend on top 
        
        axis.title.x = element_text(margin = margin(t = 10)),  # Add space above x-axis title 
        
        axis.title.y = element_text(margin = margin(r = 10)),  # Add space to the right of y-axis title 
        
        plot.title = element_text(hjust = 0.5))  # Center the plot title 

  
----------------------
  
-- O1.2: Impact of risk model and assignment type of saving

# Load necessary libraries
library(tidyverse)

# Analyze the impact of Risk_Model and Assign_Type on GenSaveLoss
performance_analysis <- data %>%
  group_by(Risk_Model, Assign_Type) %>%
  summarize(Average_Savings = mean(GenSaveLoss, na.rm = TRUE),
            Total_Savings = sum(GenSaveLoss, na.rm = TRUE))

# Visualize the results
ggplot(performance_analysis, aes(x = Assign_Type, y = Average_Savings, fill = Risk_Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Impact of Risk Model and Assignment Type on Savings",
       x = "Assignment Type", y = "Average Generated Savings") +
  theme_minimal()


---------------------
  
O1.3: Distribution of Per Capita Expenditures by Risk Model


  # Filter and prepare data for visualization 
  
  data <- data %>% 
  
  mutate(Per_Capita_Exp_TOTAL_PY = as.numeric(Per_Capita_Exp_TOTAL_PY)) 



# Box plot 

ggplot(data, aes(x = Risk_Model, y = Per_Capita_Exp_TOTAL_PY, fill = Risk_Model)) + 
  
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  
  labs(title = "Distribution of Per Capita Expenditures by Risk Model", 
       
       x = "Risk Model", y = "Per Capita Expenditures (Total, PY)") + 
  
  theme_minimal() + 
  
  scale_fill_brewer(palette = "Pastel1") 


-------------------
  
O2.1: Hospital Discharges vs Inpatient Expenditures
  
  
  # Analyze the correlation between ADM and CapAnn_INP_All 
  
  hospital_utilization <- data %>% 
  
  select(ADM, CapAnn_INP_All) %>% 
  
  filter(!is.na(ADM), !is.na(CapAnn_INP_All)) 



# Correlation analysis 

correlation <- cor(hospital_utilization$ADM, hospital_utilization$CapAnn_INP_All, use = "complete.obs") 

print(paste("Correlation between hospital discharges and inpatient expenditures:", correlation)) 



# Scatter plot 

ggplot(hospital_utilization, aes(x = ADM, y = CapAnn_INP_All)) + 
  
  geom_point(alpha = 0.5) + 
  
  geom_smooth(method = "lm", color = "blue") + 
  
  labs(title = "Hospital Discharges vs Inpatient Expenditures", 
       
       x = "Inpatient Hospital Discharges", y = "Inpatient Expenditures") + 
  
  theme_minimal() 
  
  
-------------------

O2.2: Comparison of PerCapita ESRD vs Disabled Expenditures by Ethnic Group
  
  # Load necessary libraries 

library(ggalt) 

library(readr) 


# Extract the relevant columns for the plot 

# Assuming the columns for Per_Capita_Exp_ALL_ESRD_PY and Per_Capita_Exp_ALL_DIS_PY 

# correspond to rows 1 and 2, and ethnic group names to rows 3-9. 



# Create a new data frame for the categories and values 

ethnic_groups <- c( 
  
  "Non-Hispanic White", "Black", "Asian", "Hispanic",  
  
  "North American Native", "Other", "Unknown" 
  
) 



# Make sure the data is numeric and handle missing values by replacing NAs with 0 

plot_data <- data.frame( 
  
  Ethnic_Group = ethnic_groups, 
  
  ESRD_Expenditure = as.numeric(data$Per_Capita_Exp_ALL_ESRD_PY[1:7]), 
  
  Disabled_Expenditure = as.numeric(data$Per_Capita_Exp_ALL_DIS_PY[1:7]) 
  
) 



# Replace any NAs with 0 (or you can use na.omit() to remove rows with NAs) 

plot_data[is.na(plot_data)] <- 0 



# Plot the Dumbbell chart with visible color distinction and values on the axis 

ggplot(plot_data) + 
  
  # Grey lines between the red and blue dots 
  
  geom_segment(aes(x = Ethnic_Group, xend = Ethnic_Group,  
                   
                   y = ESRD_Expenditure, yend = Disabled_Expenditure),  
               
               color = "gray", size = 1) + 
  
  # Adding points for ESRD and Disabled expenditures with color 
  
  geom_point(aes(x = Ethnic_Group, y = ESRD_Expenditure, color = "ESRD Expenditure"), size = 4) +  
  
  geom_point(aes(x = Ethnic_Group, y = Disabled_Expenditure, color = "Disabled Expenditure"), size = 4) + 
  
  # Customizing the colors for the dots 
  
  scale_color_manual(values = c("ESRD Expenditure" = "blue", "Disabled Expenditure" = "red")) + 
  
  # Adding text labels to show the values for both expenditures, formatted with commas 
  
  geom_text(aes(x = Ethnic_Group, y = ESRD_Expenditure, label = scales::comma(round(ESRD_Expenditure, 2))),  
            
            color = "blue", vjust = -0.5, size = 3) +  # Label for ESRD Expenditure 
  
  geom_text(aes(x = Ethnic_Group, y = Disabled_Expenditure, label = scales::comma(round(Disabled_Expenditure, 2))),  
            
            color = "red", vjust = 1.5, size = 3) +   # Label for Disabled Expenditure (below the dots) 
  
  labs( 
    
    title = "Comparison of Per Capita ESRD vs Disabled Expenditures by Ethnic Group", 
    
    x = "Ethnic Group", 
    
    y = "Expenditure Amount", 
    
    color = "Expenditure Type" 
    
  ) + 
  
  theme_minimal() + 
  
  theme( 
    
    legend.position = "top", 
    
    axis.text.x = element_text(angle = 90, hjust = 1)  # Tilt ethnic group names by 90 degrees 
    
  ) 
  
-------------------
  
O2.3: Healthcare Expenditures Treemap
  
  # Load necessary libraries 

library(treemap)


# Select the relevant expenditure columns 

expenditures <- data %>% 
  
  select(CapAnn_INP_S_trm, CapAnn_INP_L_trm, CapAnn_INP_Rehab,  
         
         CapAnn_INP_Psych, CapAnn_HSP, CapAnn_SNF, CapAnn_OPD, CapAnn_PB,  
         
         CapAnn_AmbPay, CapAnn_HHA, CapAnn_DME) 



# Convert all columns to numeric to avoid non-numeric issues 

expenditures <- expenditures %>% 
  
  mutate(across(everything(), ~ as.numeric(.))) 



# Gather the data into long format for easier plotting 

expenditures_long <- expenditures %>% 
  
  gather(key = "Expense_Type", value = "Cost_Value") 



# Remove rows where Cost_Value is NA (if any) 

expenditures_long <- expenditures_long %>% 
  
  filter(!is.na(Cost_Value)) 



# Define descriptions for the expenditures without the word 'expenditures' 

expense_descriptions <- c( 
  
  "Short-term acute care hospital (IPPS/CAH)", 
  
  "Long-term care hospital (LTCH)", 
  
  "Inpatient rehabilitation facility (IRF)", 
  
  "Inpatient psychiatric facility (IPF)", 
  
  "Hospice", 
  
  "SNF", 
  
  "Outpatient", 
  
  "Physician/supplier", 
  
  "Ambulance", 
  
  "Home health", 
  
  "Durable medical equipment (DME)" 
  
) 



# Map the descriptions to the expense types 

expenditures_long$Expense_Description <- factor(expenditures_long$Expense_Type,  
                                                
                                                levels = names(expenditures)[1:11],  
                                                
                                                labels = expense_descriptions) 



# Summarize total cost for each expense type 

expense_sums <- expenditures_long %>% 
  
  group_by(Expense_Description) %>% 
  
  summarise(Total_Cost = sum(Cost_Value, na.rm = TRUE)) 



# Add a combined label for display in the treemap 

expense_sums <- expense_sums %>% 
  
  mutate(Label = paste0(Expense_Description, "\n$", format(Total_Cost, big.mark = ",", scientific = FALSE))) 



# Create the treemap 

treemap(expense_sums, 
        
        index = c("Label"),                # Use the combined labels 
        
        vSize = "Total_Cost",              # Size of the blocks based on the total cost 
        
        vColor = "Total_Cost",             # Color blocks based on total cost 
        
        title = "Healthcare Expenditures Treemap",  # Title of the treemap 
        
        fontsize.labels = 12,              # Font size for labels 
        
        fontcolor.labels = "white",        # Label font color 
        
        bg.labels = "transparent",         # Transparent label background 
        
        border.col = "black",              # Border color for blocks 
        
        position.legend = "none",          # Remove legend 
        
        align.labels = list(c("center", "center")))  # Center align the labels 

-------------------
  
O3.1: 
  
  # Load necessary libraries 

library(tidyr) 

library(fmsb) 


# Select the relevant CAHPS columns and their descriptions 

cahps_data <- data %>% 
  
  select(CAHPS_1, CAHPS_2, CAHPS_3, CAHPS_4, CAHPS_5, CAHPS_6, CAHPS_7, CAHPS_8, CAHPS_9, CAHPS_11) 



# Ensure the 'Score' columns are numeric (convert if necessary) 

cahps_data <- cahps_data %>% 
  
  mutate(across(everything(), ~ as.numeric(as.character(.)))) 



# Calculate the average score for each CAHPS attribute 

average_scores <- cahps_data %>% 
  
  summarise(across(everything(), mean, na.rm = TRUE)) 



# Create a data frame for radar chart with max and min values 

max_min_values <- data.frame( 
  
  CAHPS_1 = c(100, 0),  
  
  CAHPS_2 = c(100, 0),  
  
  CAHPS_3 = c(100, 0),  
  
  CAHPS_4 = c(100, 0),  
  
  CAHPS_5 = c(100, 0),  
  
  CAHPS_6 = c(100, 0),  
  
  CAHPS_7 = c(100, 0),  
  
  CAHPS_8 = c(100, 0),  
  
  CAHPS_9 = c(100, 0),  
  
  CAHPS_11 = c(100, 0) 
  
) 



# Bind average scores and max-min values 

radar_data <- rbind(max_min_values, average_scores) 



# Define row names for radar chart 

rownames(radar_data) <- c("Max", "Min", "Average") 



# Define meaningful attribute descriptions for the chart 

attribute_descriptions <- c( 
  
  "Getting Timely Care, Appointments, and Information", 
  
  "How Well Providers Communicate", 
  
  "Patients’ Rating of Provider", 
  
  "Access to Specialists", 
  
  "Health Promotion and Education", 
  
  "Shared Decision Making", 
  
  "Health Status/Functional Status", 
  
  "Stewardship of Patient Resources", 
  
  "Courteous and Helpful Office Staff", 
  
  "Care Coordination" 
  
) 



# Plot radar chart with more visible axis numbers and grid lines 

radarchart(radar_data,  
           
           axistype = 1,  
           
           pcol = "blue",  
           
           pfcol = rgb(0.53, 0.81, 0.98, 0.5),  # Transparent sky blue 
           
           plwd = 4,  
           
           cglcol = "black",  # Darker grid lines 
           
           cglty = 1,  
           
           axislabcol = "black",  # Darker axis labels 
           
           caxislabels = seq(0, 100, 20),  
           
           cglwd = 1.5,  # Thicker grid lines 
           
           vlcex = 0.9,  # Larger value label size 
           
           vlabels = attribute_descriptions, 
           
           title = "Average Rating Score Percentage")

-------------------
  
O3.2: Correlation Matrix Provider Rating Score


# Select the relevant CAHPS columns and their descriptions 

cahps_data <- data %>% 
  
  select(CAHPS_1, CAHPS_2, CAHPS_3, CAHPS_4, CAHPS_5, CAHPS_6, CAHPS_7, CAHPS_8, CAHPS_9, CAHPS_11) 



# Ensure the 'Score' columns are numeric (convert if necessary) 

cahps_data <- cahps_data %>% 
  
  mutate(across(everything(), ~ as.numeric(as.character(.)))) 



# Calculate the correlation matrix 

correlation_matrix <- cor(cahps_data, use = "complete.obs") 



# Define meaningful attribute descriptions for the chart 

attribute_descriptions <- c( 
  
  "Timely Care, Appointments, Info", 
  
  "Provider Communication", 
  
  "Patients’ Rating of Provider", 
  
  "Access to Specialists", 
  
  "Health Promotion and Education", 
  
  "Shared Decision Making", 
  
  "Health Status/Functional Status", 
  
  "Stewardship of Patient Resources", 
  
  "Courteous and Helpful Office Staff", 
  
  "Care Coordination" 
  
) 



# Set the column names and row names to be the attribute descriptions 

colnames(correlation_matrix) <- attribute_descriptions 

rownames(correlation_matrix) <- attribute_descriptions 



# Load required package for visualization 

library(corrplot) 



# Plot the correlation matrix 

corrplot(correlation_matrix, method = "color", type = "upper",  
         
         tl.col = "black", tl.srt = 45, title = "Correlation Matrix of CAHPS Attributes",  
         
         mar = c(0,0,1,0)) 
  
-------------------


rm(list = ls())
# Load necessary and relevant libraries
library(skimr)
library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(caret)
library(scales)

# Set working directory
setwd("C:/Users/user/Desktop/Maven_Challenge/Bank_Customer_Churn")

# Import and read data file
data <- read.csv("Bank_Churn.csv")

str(data)
skim(data)
summary(data)

# Data transformation
# Exited variable
data$Exited <- ifelse(data$Exited == 1, "Churned", "Retention")

# Convert back to a factor for proper categorical handling
data$Exited <- factor(data$Exited, levels = c("Retention", "Churned"))

# Verify the changes
table(data$Exited)

#Transform numeric variables into categorical 
# Categorize 'Age' into age groups

#data$AgeGroup <- cut(data$Age, breaks = c(18, 30, 40, 50, 60, 100),
                     #labels = c("18-30", "31-40", "41-50", "51-60", "60+"), right = FALSE)
data <- data %>%
  mutate(AgeGroup = case_when(
    Age <= 30 ~ "18-30",
    Age > 30 & Age <= 45 ~ "31-45",
    Age > 45 & Age <= 60 ~ "46-60",
    Age > 60 ~ "60+"
  ))

# Categorize CreditScore into Low, Medium, and High based on specified ranges
data$CreditScoreCategory <- cut(data$CreditScore,
                                breaks = c(-Inf, 584, 718, Inf),
                                labels = c("Low", "Medium", "High"),
                                right = TRUE)  # Ensure <=584, >584 & <=718, >718
table(data$CreditScoreCategory)


# Categorize 'Balance' into bins
data$BalanceCategory <- cut(data$Balance, breaks = c(-1, 0, 50000, 100000, 200000, max(data$Balance)),
                            labels = c("Zero", "Low", "Medium", "High", "Very High"), right = FALSE)
table(data$BalanceCategory)

#####################################
# Statistical Tests
# Fit a logistic regression model
##############################################
model <- glm(Exited ~ Age + Gender + Geography + CreditScore + Balance + NumOfProducts, 
             data = data, family = binomial)

# Summary of the model
summary(model)

# Extract significant variables based on p-values
significant_vars <- summary(model)$coefficients
significant_vars <- significant_vars[significant_vars[, 4] < 0.05, ]
significant_vars
class(significant_vars)
#stat_vars <- writexl::write_xlsx(significant_vars, "significant_vars.xlsx")
significant_vars <- as.data.frame(significant_vars)

#######################################################################
#Visualizations
# Focusing on statistically Significant Predictors of Customer Churn
#######################################################################

# Assuming 'significant_vars' contains columns Estimate, Std. Error, and Pr(>|z|)
significant_vars <- significant_vars %>%
  mutate(
    Predictor = rownames(significant_vars),  # Add row names as a column for predictors
    LowerCI = Estimate - 1.96 * `Std. Error`,  # Calculate 95% confidence intervals
    UpperCI = Estimate + 1.96 * `Std. Error`
  )

# Create the forest plot
ggplot(significant_vars, aes(x = Estimate, y = reorder(Predictor, Estimate))) +
  geom_point(size = 3, color = "blue") +  # Point for estimates
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.2, color = "black") +  # Horizontal error bars
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Vertical line at 0
  labs(
    title = "Customer Churn Predictors",
    x = "Estimate (with 95% CI)",
    y = "Predictors"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10, face = "bold")
  )


##################### Customer churn distribution ################################## 
# Calculate percentages for each category
churn_percent <- data %>%
  group_by(Exited) %>%
  summarise(Percentage = n() / nrow(data) * 100)

# Create churn distribution with percentages and labels
ggplot(data, aes(x = Exited, fill = Exited)) +
  geom_bar(aes(y = (..count..) / sum(..count..) * 100), width = 0.5) +  # Reduce bar width
  geom_text(data = churn_percent, aes(x = Exited, y = Percentage, label = sprintf("%.1f%%", Percentage)), 
            vjust = -0.5, size = 3.8) +  # Add percentage labels above bars
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Format y-axis as percentages
  scale_fill_manual(values = c("Retention" = "lightgreen", "Churned" = "red")) +  # Set colors for Retention and Churned
  labs(title = "Customer Churn Distribution (%)", x = "Customer Status", y = "Percentage") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove gridlines
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),  # Center and style title
    axis.text.y = element_blank(),  # Remove y-axis text
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    axis.title.y = element_blank(),  # Remove y-axis title
    axis.text.x = element_text(face = "bold"),
    axis.title.x = element_text(),
    legend.position = "none"  # Remove legend
  )

############################# Customer Churn by Gender ################################
# Prepare data for Gender and churn proportions
gender_data <- data %>%
  group_by(Gender, Exited) %>%
  summarise(Count = n()) %>%
  group_by(Gender) %>%
  mutate(Proportion = Count / sum(Count) * 100)

# Plot stacked bar chart for Gender with percentage labels
ggplot(gender_data, aes(x = Gender, y = Proportion, fill = as.factor(Exited))) +
  geom_bar(stat = "identity", width = 0.5) +  # Stacked proportional bars without border
  geom_text(aes(label = paste0(round(Proportion, 1), "%")),  # Add percentage labels
            position = position_stack(vjust = 0.5), size = 4, color = "black") +
  scale_fill_manual(values = c("lightgreen", "red"), labels = c("Retention", "Churned")) +
  labs(title = "Customer Churn by Gender (%)",
       x = "Gender", y = NULL, fill = "Customer Status") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
    axis.text.y = element_blank(),       # Remove y-axis values
    axis.ticks.y = element_blank(),      # Remove y-axis ticks
    panel.grid = element_blank(),        # Remove gridlines
    legend.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

################################## Customer Churn Rate by Age Group #############################
# Churn by age group
age_churn_summary <- data %>%
  group_by(AgeGroup, Exited) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Visualize churn by age group
# Stacked bar chart for churn by age group
# Update chart to flip coordinates and rank churned customers
ggplot(age_churn_summary %>% arrange(desc(Exited), desc(Percentage)), 
       aes(x = reorder(AgeGroup, -Percentage * as.numeric(as.factor(Exited))), 
           y = Percentage, fill = as.factor(Exited))) +
  geom_bar(stat = "identity", width = 0.5) +
  coord_flip() +  # Flip the chart
  scale_fill_manual(values = c("lightgreen", "red"), labels = c("Retention", "Churned")) +
  labs(
    title = "Customer Churn by Age Distribution (%)",
    x = "Age Group",
    y = "Percentage",
    fill = "Customer Status"
  ) +
  geom_text(
    aes(label = sprintf("%.1f%%", Percentage)),
    position = position_stack(vjust = 0.5), size = 4, color = "black"
  ) +  # Add percentage labels inside bars
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),  # Center title
    axis.text = element_text(size = 10),  # Adjust axis text size
    axis.title = element_text(size = 12),  # Adjust axis title size
    legend.position = "bottom",
    panel.grid = element_blank()  # Remove gridlines
  )

################################# Customer Churn Rate by Geography ####################################

# Calculate percentages for churn by geography
geo_churn_percent <- data %>%
  group_by(Geography, Exited) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create Churn by Geography with facet_wrap
# Flip chart and rank churned customers
ggplot(geo_churn_percent %>% arrange(desc(Percentage)), 
       aes(x = reorder(Geography, -Percentage), y = Percentage, fill = Exited)) +
  geom_bar(stat = "identity", width = 0.5) +  # Horizontal bar chart
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5), size = 3, color = "black") +  # Add percentage labels
  coord_flip() +  # Flip the chart
  scale_fill_manual(values = c("Retention" = "lightgreen", "Churned" = "red")) +  # Set custom colors
  labs(
    title = "Customer Churn by Geography (%)",
    x = "Geography",
    y = "Percentage",
    fill = "Customer Status"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove gridlines
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),  # Center and style title
    axis.text = element_text(size = 10),  # Adjust axis text size
    axis.title = element_text(size = 12),  # Adjust axis title size
    strip.text = element_text(size = 13, face = "bold"),  # Style facet labels
    legend.title = element_blank(),  # Remove legend title
    panel.border = element_blank(),
    legend.position = "bottom" # Add borders around facets
  )

########################### Customer Churn Rate by BalanceCategory ################
# Prepare data for BalanceCategory and churn proportions
balance_data <- data %>%
  filter(!is.na(BalanceCategory)) %>%
  group_by(BalanceCategory, Exited) %>%
  summarise(Count = n()) %>%
  group_by(BalanceCategory) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Plot stacked bar chart for BalanceCategory with percentage labels
ggplot(balance_data %>% arrange(desc(Percentage)), 
       aes(x = reorder(BalanceCategory, -Percentage), y = Percentage, fill = Exited)) +
  geom_bar(stat = "identity", width = 0.5) +  
  geom_text(aes(label = sprintf("%.f%%", Percentage)),  # Add percentage labels
            position = position_stack(vjust = 0.5), size = 3, color = "black") +
  coord_flip() +
  scale_fill_manual(values = c("Retention" = "lightgreen", "Churned" = "red")) +
  labs(
    title = "Customer Churn by Account Balance (%)",
       x = "Balance Category", 
       y = "Percentage", 
       fill = "Customer Status") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 13, face = "bold"),
    legend.title = element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom"
  )

################################################################################### FOUR
# Prepare data for visualizing churn by Geography and another key factor
geo_drivers_data <- data %>%
  filter(!is.na(Geography), !is.na(Gender)) %>%
  group_by(Geography, Gender, Exited) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Geography, Gender) %>%
  mutate(Proportion = Count / sum(Count) * 100)

# Plot stacked bar chart for Geography with key drivers (e.g Gender)
ggplot(geo_drivers_data, aes(x = Gender, y = Proportion, fill = as.factor(Exited))) +
  geom_bar(stat = "identity", width = 0.6) +  # Stacked proportional bars
  geom_text(aes(label = paste0(round(Proportion, 1), "%")),  # Add percentage labels
            position = position_stack(vjust = 0.5), size = 3, color = "black") +
  scale_fill_manual(values = c("lightgreen", "red"), labels = c("Retention", "Churned")) +
  facet_wrap(~ Geography) +  # Separate charts for each geography
  labs(
    title = "Customer Churn by Geography and Gender (%)",
    x = "Gender",
    y = NULL,
    fill = "Customer Status"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.y = element_blank(),       # Remove y-axis values
    axis.ticks.y = element_blank(),      # Remove y-axis ticks
    panel.grid = element_blank(),        # Remove gridlines
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )
 
################################# Customer Churn Balance by Geography ####################
data <- data %>%
  filter(!is.na(Balance)) %>%  # Remove NA values from Balance before categorization
  mutate(BalanceCategory = cut(Balance, 
                               breaks = c(0, 1, 50000, 100000, 200000, max(Balance, na.rm = TRUE) + 1),  # Add 1 to max
                               labels = c("Zero", "Low", "Medium", "High", "Very High"), 
                               right = FALSE, include.lowest = TRUE))  # Include 0 and adjust intervals

# Plot stacked bar chart with facets for Geography
ggplot(data, aes(x = BalanceCategory, fill = as.factor(Exited))) +
  geom_bar(position = "fill", width = 0.6) +  # Stacked proportional bars
  facet_wrap(~ Geography) +  # Separate charts for each Geography
  scale_fill_manual(values = c("darkgreen", "red"), labels = c("Retention", "Churned")) +
  scale_y_continuous(labels = percent_format()) +  # Convert y-axis to percentage
  labs(title = "Customer Churn by Geography and Balance",
       x = "Balance Category", y = "Percentage", fill = "Customer Status") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA)
  )




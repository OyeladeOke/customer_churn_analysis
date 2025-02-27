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
#####################################
# Chi-Square test for Geography and Churn
geo_chi <- chisq.test(table(data$Geography, data$Exited))
geo_chi$p.value

# Chi-Square test for Gender and Churn
gender_chi <- chisq.test(table(data$Gender, data$Exited))
gender_chi$p.value

# T-test for CreditScore
t_test_credit <- t.test(CreditScore ~ Exited, data = data)
t_test_credit$p.value

# ANOVA for Age
anova_age <- aov(Age ~ Exited, data = data)
summary(anova_age)

##############################################
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

##############################
#Visualizations
##############################

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

#Continue

# Prepare data for visualizing churn by Geography and another key factor
geo_drivers_data <- data %>%
  filter(!is.na(Geography), !is.na(BalanceCategory)) %>%
  group_by(Geography, BalanceCategory, Exited) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Geography, BalanceCategory) %>%
  mutate(Proportion = Count / sum(Count) * 100)

# Plot stacked bar chart for Geography with key drivers (e.g., BalanceCategory)
ggplot(geo_drivers_data, aes(x = BalanceCategory, y = Proportion, fill = as.factor(Exited))) +
  geom_bar(stat = "identity", width = 0.6) +  # Stacked proportional bars
  geom_text(aes(label = paste0(round(Proportion, 1), "%")),  # Add percentage labels
            position = position_stack(vjust = 0.5), size = 3, color = "black") +
  scale_fill_manual(values = c("lightgreen", "red"), labels = c("Retention", "Churned")) +
  facet_wrap(~ Geography) +  # Separate charts for each geography
  labs(
    title = "Customer Churn by Geography and Balance (%)",
    x = "Balance Category",
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

################################################################################## TWO
# Prepare data for visualizing churn by Geography and another key factor
geo_drivers_data <- data %>%
  filter(!is.na(Geography), !is.na(AgeGroup)) %>%
  group_by(Geography, AgeGroup, Exited) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Geography, AgeGroup) %>%
  mutate(Proportion = Count / sum(Count) * 100)

# Plot stacked bar chart for Geography with key drivers (e.g., AgeGroup)
ggplot(geo_drivers_data, aes(x = AgeGroup, y = Proportion, fill = as.factor(Exited))) +
  geom_bar(stat = "identity", width = 0.6) +  # Stacked proportional bars
  geom_text(aes(label = paste0(round(Proportion, 1), "%")),  # Add percentage labels
            position = position_stack(vjust = 0.5), size = 3, color = "black") +
  scale_fill_manual(values = c("lightgreen", "red"), labels = c("Retention", "Churned")) +
  facet_wrap(~ Geography) +  # Separate charts for each Geography
  labs(
    title = "Customer Churn by Geography and Age Distribution (%)",
    x = "Age Distribution",
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

################################################################################# THREE
# Prepare data for visualizing churn by Geography and another key factor
geo_drivers_data <- data %>%
  filter(!is.na(Geography), !is.na(CreditScoreCategory)) %>%
  group_by(Geography, CreditScoreCategory, Exited) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Geography, CreditScoreCategory) %>%
  mutate(Proportion = Count / sum(Count) * 100)

# Plot stacked bar chart for Geography with key drivers (e.g., CreditScoreCategory)
ggplot(geo_drivers_data, aes(x = CreditScoreCategory, y = Proportion, fill = as.factor(Exited))) +
  geom_bar(stat = "identity", width = 0.6) +  # Stacked proportional bars
  geom_text(aes(label = paste0(round(Proportion, 1), "%")),  # Add percentage labels
            position = position_stack(vjust = 0.5), size = 3, color = "black") +
  scale_fill_manual(values = c("lightgreen", "red"), labels = c("Retention", "Churned")) +
  facet_wrap(~ Geography) +  # Separate charts for each geography
  labs(
    title = "Customer Churn by Geography and CreditScoreCategory (%)",
    x = "Credit Score Category",
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
 # FILTER FOR GERMANY ONLY
################################################################################# Five
# Filter data for Germany
germany_data <- data %>%
  filter(Geography == "Germany", !is.na(BalanceCategory)) %>%
  group_by(BalanceCategory, Exited) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(BalanceCategory) %>%
  mutate(Proportion = Count / sum(Count) * 100) %>%
  ungroup()

# Rank BalanceCategory by churn proportion
ranked_categories <- germany_data %>%
  filter(Exited == 1) %>%
  arrange(desc(Proportion)) %>%
  pull(BalanceCategory)

# Reorder BalanceCategory factor levels based on ranking
germany_data <- germany_data %>%
  mutate(BalanceCategory = factor(BalanceCategory, levels = ranked_categories))

# Plot the data
ggplot(germany_data, aes(x = BalanceCategory, y = Proportion, fill = as.factor(Exited))) +
  geom_bar(stat = "identity", width = 0.6) +  # Stacked proportional bars
  geom_text(
    aes(label = paste0(round(Proportion, 1), "%")),  # Add percentage labels
    position = position_stack(vjust = 0.5), size = 3, color = "black"
  ) +
  scale_fill_manual(
    values = c("red", "darkgreen"),
    labels = c("Churned", "Retention")
  ) +
  labs(
    title = "Customer Churn in Germany by Balance Category (%)",
    x = "Balance Category (Ranked by Churn)",
    y = "Proportion of Customers (%)",
    fill = "Customer Status"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    panel.grid = element_blank(),                      # Remove gridlines
    legend.position = "bottom"                         # Position legend below the chart
  )

##################################################################################### SIX

germany_data <- data %>%
  filter(Geography == "Germany", !is.na(AgeGroup)) %>%
  group_by(AgeGroup, Exited) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(AgeGroup) %>%
  mutate(Proportion = Count / sum(Count) * 100) %>%
  ungroup() %>%
  filter(Exited == 1) %>%  # Focus only on churned customers for ranking
  arrange(desc(Proportion)) %>%
  mutate(AgeGroup = factor(AgeGroup, levels = unique(AgeGroup)))

# Plot stacked bar chart for Germany with ranked AgeGroup
ggplot(germany_data, aes(x = AgeGroup, y = Proportion, fill = as.factor(Exited))) +
  geom_bar(stat = "identity", width = 0.6) +  # Stacked proportional bars
  geom_text(aes(label = paste0(round(Proportion, 1), "%")),  # Add percentage labels
            position = position_stack(vjust = 0.5), size = 3, color = "black") +
  scale_fill_manual(values = c("lightgreen", "red"), labels = c("Churned", "Retention")) +
  labs(
    title = "Customer Churn in Germany by Age Distribution (%)",
    x = "Age Distribution",
    y = "Proportion of Customers (%)",
    fill = "Customer Status"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    panel.grid = element_blank(),                      # Remove gridlines
    legend.position = "bottom"                         # Position legend below the chart
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
################################ Customer Churn by Age Group and Geography ##########################

# Plot stacked bar chart with facet wrap for Geography
ggplot(age_geo_data, aes(x = AgeGroup, y = Proportion, fill = as.factor(Exited))) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +  # Stacked proportional bars
  scale_fill_manual(values = c("darkgreen", "red"), labels = c("Retention", "Churned")) +
  facet_wrap(~ Geography) +
  labs(title = "Customer Churn by Age Group and Geography",
       x = "Age Group", y = "Proportion (%)", fill = "Customer Status") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(color = "grey80"),  # Retain y-axis gridlines for better readability
    panel.grid.minor = element_blank(),                  # Remove minor gridlines
    panel.border = element_rect(color = "black", fill = NA),
    strip.text = element_text(face = "bold")
  )



#########################################
# Statistically Significant Coefficients
#########################################

# Bar plot of significant coefficients
coef_data <- data.frame(Variable = rownames(significant_vars), 
                        Coefficient = significant_vars[, 1])

ggplot(coef_data, aes(x = reorder(Variable, Coefficient), y = Coefficient, fill = Coefficient)) +
  geom_bar(stat = "identity", width = 0.5) +
  coord_flip() +
  scale_fill_gradient2(low = "darkgreen", mid = "grey", high = "red", midpoint = 0) +  # Color gradient
  labs(title = "Statistically Significant Customer Churn Predictors", subtitle =  "(Logistic Regression)", x = "Variable", y = "Coefficient") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

#######################################################################
# Focusing on statistically Significant Predictors of Customer Churn
#######################################################################
# Subset for Germany (Geography)
# Filter data for Germany
germany_data <- data %>% filter(Geography == "Germany")

# Summary of churn within Germany
germany_summary <- germany_data %>%
  group_by(Exited) %>%
  summarise(
    Avg_Balance = mean(Balance, na.rm = TRUE),
    Avg_Age = mean(Age, na.rm = TRUE),
    Avg_CreditScore = mean(CreditScore, na.rm = TRUE),
    Avg_Tenure = mean(Tenure, na.rm = TRUE),
    Count = n()
  )
print(germany_summary)


########################################################

#################################### Interaction of Age and Balance on Churn ###########################
ggplot(data, aes(x = Age, y = Balance, color = as.factor(Exited))) +
  geom_point(alpha = 0.5) +
  facet_wrap(~Geography) +
  scale_color_manual(values = c("darkgreen", "red"), labels = c("Retention", "Churned")) +
  labs(title = "Interaction of Age and Balance on Churn by Geography", x = "Age", y = "Balance") +
  theme_minimal()

###################################### Germany Only ###############################
# Filter data for Germany
germany_data <- data %>%
  filter(Geography == "Germany")

# Prepare data for absolute labels
germany_bar_data <- germany_data %>%
  group_by(BalanceCategory, Exited) %>%
  summarise(Count = n()) %>%
  group_by(BalanceCategory) %>%
  mutate(Proportion = Count / sum(Count) * 100)

# Plot stacked bar chart for Germany with absolute labels
ggplot(germany_bar_data, aes(x = BalanceCategory, y = Proportion, fill = as.factor(Exited))) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +  # Stacked proportional bars
  geom_text(aes(label = paste0(round(Proportion, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3) +  # Add percentage labels
  scale_fill_manual(values = c("green", "red"), labels = c("Retention", "Churned")) +
  labs(title = "Customer Churn by Balance (Germany Only)",
       x = "Balance Category", y = NULL, fill = "Customer Status") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),  # Remove y-axis values
    panel.grid = element_blank(),   # Remove gridlines
    panel.border = element_blank(),
    legend.position = "bottom"
  )

#############################################

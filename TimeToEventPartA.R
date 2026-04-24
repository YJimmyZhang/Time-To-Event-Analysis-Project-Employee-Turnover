library("dplyr")     # Data manipulation
library("ggplot2")   # Plotting graphics
library("forcats")   # Handling categorical factors and lumping
library("tidyr")     # Transforming data from wide to long format
library("moments")   # Calculating statistical moments like skewness


# 1. Data loading 

data <- read.csv("turnover.csv")

# Notice the category Finan\xf1e. This is an encoding error 
# (likely a special character that didn't translate well when the CSV was created, meant to be "Finance").
# We fix this before doing any frequency tables or modeling.
data <- data %>%
  mutate(profession = ifelse(profession == "Finan\xf1e", "Finance", profession))

# Survival models require categorical data to be explicitly recognized as 'factors'.
# This checks every column; if it's text (character), it converts it to a factor.
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)

# Sanity check to ensure data loaded correctly
head(data)
str(data)

# 2. Diagonostics for categorical variables

# We have variables with too many levels. If a level has too few events, it won't be reliable to build a model later.
# We create a function to check 'event counts' per category for categorical variables with many levels.
check_category_health <- function(df, col_name) {
  df %>%
    group_by(!!sym(col_name)) %>%
    summarise(
      Total_Employees = n(),
      Events = sum(event),
      Event_Rate = round(sum(event) / n(), 2)
    ) %>%
    arrange(desc(Total_Employees)) # Sort largest to smallest to spot the drop-off
}

# Run health checks for variables with many levels
industry_health   <- check_category_health(data, "industry")
profession_health <- check_category_health(data, "profession")
traffic_health    <- check_category_health(data, "traffic")

# Visualizing categorical variables to check levels with few events

# Check Industry
ggplot(industry_health, aes(x = reorder(industry, -Total_Employees), y = Total_Employees)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_bar(aes(y = Events), stat = "identity", fill = "salmon") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Employee Turnover by Industry",
       subtitle = "Comparing total headcount and observed events",
       x = "Industry Sector", 
       y = "Number of Employees")

# Check Profession 
ggplot(profession_health, aes(x = reorder(profession, -Total_Employees), y = Total_Employees)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_bar(aes(y = Events), stat = "identity", fill = "salmon") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 9)) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 1)) + 
  labs(title = "Turnover Frequencies across Professions",
       subtitle = "Distribution of active and departed employees",
       x = "Profession", 
       y = "Number of Employees")

# Check Traffic Source (Recruitment channel)
ggplot(traffic_health, aes(x = reorder(traffic, -Total_Employees), y = Total_Employees)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_bar(aes(y = Events), stat = "identity", fill = "salmon") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Employee Distribution by Recruitment Channel",
       subtitle = "Total observations and event occurrences per channel",
       x = "Recruitment Channel (Traffic)", 
       y = "Number of Employees")



# 3. Level reduction of categorical variables "Industry" and "Profession", while "Traffic" seems fine

# we map related, sparse categories into "Medium-Sized Buckets". 
# This preserves interpretability while ensuring no category is too small for the models in Part C.

data <- data %>%
  mutate(
    # Grouping related job families
    profession_grouped = fct_collapse(profession,
                                      HR_and_Training = c("HR", "Teaching"),
                                      Tech_and_Engineering = c("IT", "Engineer"),
                                      Finance_and_Accounting = c("Finance", "Accounting"),
                                      Revenue_Client = c("Sales", "Marketing", "Commercial", "BusinessDevelopment", "PR"),
                                      Management_Legal_Consulting = c("manage", "Consult", "Law"),
                                      Other = c("etc")
    ),
    
    # Grouping by labor market dynamics
    industry_grouped = fct_collapse(industry,
                                    Finance_and_Consulting = c("Banks", "Consult"),
                                    Tech_and_Telecom = c("IT", "Telecom"),
                                    Retail_and_Service = c("Retail", "HoReCa", "RealEstate"),
                                    Heavy_Industry = c("manufacture", "Building", "PowerGeneration", "Mining", "Agriculture"),
                                    Public_and_Essential = c("State", "Pharma", "transport"),
                                    Other = c("etc")
    )
  )

# Plot new Profession categories
data %>%
  group_by(profession_grouped) %>%
  summarise(
    Total_Employees = n(),
    Events = sum(event)
  ) %>%
  ggplot(aes(x = reorder(profession_grouped, -Total_Employees), y = Total_Employees)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_bar(aes(y = Events), stat = "identity", fill = "salmon") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
  labs(title = "Turnover by Profession Family",
       subtitle = "Post-transformation aggregated categories",
       x = "Profession Group",
       y = "Number of Employees")

# Plot new Industry categories
data %>%
  group_by(industry_grouped) %>%
  summarise(
    Total_Employees = n(),
    Events = sum(event)
  ) %>%
  ggplot(aes(x = reorder(industry_grouped, -Total_Employees), y = Total_Employees)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_bar(aes(y = Events), stat = "identity", fill = "salmon") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
  labs(title = "Turnover by Industry Sector",
       subtitle = "Post-transformation aggregated categories",
       x = "Industry Group",
       y = "Number of Employees")


# 4. Response variable and censoring analysis

# event = 1 means they left (the "event")
# event = 0 means they stayed (they are "censored")

# Count occurrences of each event status
event_counts <- table(data$event)
event_percentages <- prop.table(event_counts) * 100

# Overlapping histogram maps time but colors whether they left or stayed.
# This prevents us from needing multiple plots for censoring.
ggplot(data, aes(x = stag, fill = as.factor(event))) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30, color = "white") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "salmon"), 
                    labels = c("0 = Censored (Active)", "1 = Event (Departed)")) +
  labs(title = "Distribution of Follow-up Time",
       subtitle = "Stratified by censoring status",
       x = "Follow-up Time (stag)",
       y = "Frequency",
       fill = "Employment Status") +
  theme_minimal()



# 5. Continuous explanatory variables

# Define the list of continuous columns
cont_vars <- c("age", "extraversion", "independ", "selfcontrol", "anxiety", "novator")

# Basic descriptives
summary_stats <- summary(data[cont_vars])
print(summary_stats)

# Standard deviation 
sapply(data[cont_vars], sd)

# Mathematical check of skewness
# Rule of thumb: skewness > 1 or < -1 indicates a highly skewed distribution
skewness_values <- sapply(data[cont_vars], skewness)
print(skewness_values)

# To save space for the page limit, we condense all 6 boxplots into one graphic 
# using facet_wrap. This shows how covariates relate to the target variable (censoring).
data %>%
  select(event, all_of(cont_vars)) %>%
  pivot_longer(cols = -event, names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = as.factor(event), y = value, fill = as.factor(event))) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.alpha = 0.5) +
  facet_wrap(~variable, scales = "free_y", ncol = 3) + 
  scale_fill_manual(values = c("0" = "skyblue", "1" = "salmon"), 
                    labels = c("0 = Censored", "1 = Event")) +
  theme_minimal() +
  labs(title = "Distribution of Continuous Covariates by Event Status",
       subtitle = "Evaluating baseline differences between active and departed employees",
       x = "Employment Status (Event)",
       y = "Covariate Value",
       fill = "Status") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom") 



# 6. Categorical explanatory variables summary

# We now summarize the categorical variables, replacing the raw profession/industry 
# with our new, transformed groupings so the rest of the team can use them.
cat_vars <- c("gender", "industry_grouped", "profession_grouped", "traffic", "coach", "head_gender", "greywage", "way")

# Loop to print a summary for each variable
for (var in cat_vars) {
  cat("\n==========================================\n")
  cat("Frequency Table for:", var, "\n")
  
  tbl <- data %>%
    count(!!sym(var)) %>%                 
    mutate(percentage = round(n / sum(n) * 100, 2)) 
  
  print(tbl)
}

# Plot the variables with 2-4 levels
# These variables are the potential candidates for the initial survival curves in Part B.
selected_plot_vars <- c("way", "coach", "greywage", "gender", "head_gender")

data %>%
  select(all_of(selected_plot_vars)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "level") %>%
  ggplot(aes(x = level, fill = variable)) +
  geom_bar(color = "white") +
  facet_wrap(~variable, scales = "free", ncol = 2) + 
  theme_minimal() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Frequencies of Categorical Covariates",
       x = "Factor Level",
       y = "Frequency")

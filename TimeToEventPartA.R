library("dplyr") #data manipulation
library("ggplot2") #plots
library("forcats") #to handle categorical factors, lump tiny categories together of categorical variables
library("tidyr") #to transform data from wide to long format so we can plot multiple variables at once
library("moments") #to calculate statistical moments like skewness

#load data
data <- read.csv("turnover.csv")


#sanity check
head(data)
str(data)


# survival models categorical data to be explicitly recognized as 'factors'
# checks every column if it's text(character), it converts it to a factor
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)

## Diagnostics for categorical variables with many levels

# we have variables with too many levels. If a level has too few events it won't be reliable to build a model
# create a function to check 'event vounts' per category for categorical variables with many levels
check_category_health <- function(df, col_name) {
  df %>%
    group_by(!!sym(col_name)) %>%
    summarise(
      Total_Employees = n(),
      Events = sum(event),
      Event_Rate = round(sum(event) / n(), 2)
    ) %>%
    arrange(desc(Total_Employees)) # sort largest to smallest to spot drop-off
}

# run health checkc for variables with many levels
industry_health   <- check_category_health(data, "industry")
profession_health <- check_category_health(data, "profession")
traffic_health    <- check_category_health(data, "traffic")

print(industry_health)
print(profession_health)
print(traffic_health)

# looking for the 'elbow' where categories start having very few events (naive approach)
# check Industry
ggplot(industry_health, aes(x = reorder(industry, -Total_Employees), y = Total_Employees)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_bar(aes(y = Events), stat = "identity", fill = "salmon") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Employee Turnover by Industry",
       subtitle = "Comparing total headcount and observed events",
       x = "Industry Sector", 
       y = "Number of Employees")

# check Profession
ggplot(profession_health, aes(x = reorder(profession, -Total_Employees), y = Total_Employees)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_bar(aes(y = Events), stat = "identity", fill = "salmon") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Turnover Frequencies across Professions",
       subtitle = "Distribution of active and departed employees",
       x = "Profession", 
       y = "Number of Employees")


# check Traffic Source (recruitment channel)
ggplot(traffic_health, aes(x = reorder(traffic, -Total_Employees), y = Total_Employees)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_bar(aes(y = Events), stat = "identity", fill = "salmon") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Employee Distribution by Recruitment Channel",
       subtitle = "Total observations and event occurrences per channel",
       x = "Recruitment Channel (Traffic)", 
       y = "Number of Employees")

# applying cutoffs based on previous plots
# keep the most frequent levels and group the tiny ones into "Other" using fct_lump() of forcats library
data$industry <- fct_lump(data$industry, n = 6)
data$profession <- fct_lump(data$profession, n = 4)
data$traffic <- fct_lump(data$traffic, n = 5)

## Reponse Variable & Censoring Analysis

# calculate the Censoring Rate
# event = 1 means they left (the "event")
# event = 0 means they stayed (they are "censored")

# count occurrences of each event status
event_counts <- table(data$event)
print(event_counts)

# calculate percentages for each status
event_percentages <- prop.table(event_counts) * 100
print(event_percentages)

# look at the spread of employment durations
summary(data$stag)

# histogram maps time but colors whether left or stayed
# overlapping bars helps to see if people tend to quit really early or much later
ggplot(data, aes(x = stag, fill = as.factor(event))) +
  # alpha = 0.5 makes the bars semi-transparent so you can see where they overlap
  # position = "identity" prevents the bars from stacking on top of each other
  geom_histogram(alpha = 0.5, position = "identity", bins = 30, color = "white") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "salmon"), 
                    labels = c("0 = Censored (Active)", "1 = Event (Departed)")) +
  
  labs(title = "Distribution of Follow-up Time",
       subtitle = "Stratified by censoring status",
       x = "Follow-up Time (stag)",
       y = "Frequency",
       fill = "Employment Status") +
  theme_minimal()



# side-by-side histogram 
ggplot(data, aes(x = stag, fill = as.factor(event))) +
  geom_histogram(bins = 30, show.legend = FALSE) +
  facet_wrap(~event, labeller = as_labeller(c("0" = "Stayed", "1" = "Left"))) +
  theme_minimal() +
  labs(x = "Time (stag)", y = "Count")

## Continuous explanatory variables

# define the list of continuous columns
cont_vars <- c("age", "extraversion", "independ", "selfcontrol", "anxiety", "novator")

# basic descriptives
summary_stats <- summary(data[cont_vars])
print(summary_stats)

# standard deviation 
sapply(data[cont_vars], sd)


# plotting the distributions.
# pivot_longer stacks the data so ggplot can use facet_wrap to draw all 6 histograms in one go
data %>%
  select(all_of(cont_vars)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  facet_wrap(~variable, scales = "free") + 
  theme_minimal() +
  labs(title = "Distributions of Continuous Covariates",
       x = "Value",
       y = "Frequency")

# mathematical check of skewness
# rule of thumb: skewness > 1 or < -1 indicates a highly skewed distribution
skewness_values <- sapply(data[cont_vars], skewness)
print(skewness_values)


## Categorical explanatory variables


# list of all categorical variables
cat_vars <- c("gender", "industry", "profession", "traffic", "coach", "head_gender", "greywage", "way")


# loop to print a summary for each variable
for (var in cat_vars) {
  cat("\n==========================================\n")
  cat("Frequency Table for:", var, "\n")
  
  tbl <- data %>%
    count(!!sym(var)) %>%                 # count occurrences
    mutate(percentage = round(n / sum(n) * 100, 2)) # calculate %
  
  print(tbl)
}

# plot the variables with 2-4 levels
# variables with lower levels are best candidates for the initial survival curves
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

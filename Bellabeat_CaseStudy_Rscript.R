hourly_steps <- read.csv("C:/Users/Lucia/Documents/Data Analysis/Google Analytics Certificate 1/Bellabeat Capstone Proyect/Datasets/hourly_steps.csv")
daily_activity <- read.csv("C:/Users/Lucia/Documents/Data Analysis/Google Analytics Certificate 1/Bellabeat Capstone Proyect/Datasets/daily_activity.csv")
library(ggplot2)
library(dplyr)

# creating scatter plot for Calories and TotalSteps
ggplot(data = daily_activity, aes(x = Calories, y = TotalSteps)) + 
  geom_point(aes(color = TotalDistance), alpha = 0.8) +  
  geom_smooth(method = "lm", formula = y ~ x, color = "lightblue", se = FALSE) + 
  labs(title = "Total Steps vs. Calories Burned",
       x = "Calories Burned",
       y = "Total Steps") +
  scale_color_gradient(low = "lightpink", high = "deeppink") +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, family = "serif"),
    axis.title = element_text(size = 14, family = "serif"), 
    axis.text = element_text(size = 14, family = "serif")  
  )


# formating ActivityHour as date type
hourly_steps$ActivityHour <- as.POSIXct(hourly_steps$ActivityHour, format = "%m/%d/%Y %H:%M")

#getting only the hour
hourly_steps$Hour <- as.numeric(format(hourly_steps$ActivityHour, "%H"))

# grouping by hour and getting the avg
hourly_summary <- hourly_steps %>%
  group_by(Hour) %>%
  summarize(AverageSteps = mean(StepTotal, na.rm = TRUE))

# identifying highest and lowest points
max_point <- hourly_summary %>% filter(AverageSteps == max(AverageSteps))
min_point <- hourly_summary %>% filter(AverageSteps == min(AverageSteps))

# creating the plot
ggplot(hourly_summary, aes(x = Hour, y = AverageSteps)) +
  geom_line(color = "lightblue", size=1.5) +
  geom_point(data=max_point, aes(x=Hour, y=AverageSteps), color="purple", size= 3) +
  geom_point(data=min_point, aes(x=Hour, y=AverageSteps), color="orange", size= 3) +
  geom_text(data = max_point, aes(x = Hour, y = AverageSteps, label = round(AverageSteps, 1)), vjust = -1, color = "purple") +
  geom_text(data = min_point, aes(x = Hour, y = AverageSteps, label = round(AverageSteps, 1)), vjust = -1, color = "orange") +
  labs(x = "Hour of Day", y = "Average Steps", title = "Average Steps by Hour of Day") +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, family = "serif"),
    axis.title = element_text(size = 14, family = "serif"), 
    axis.text = element_text(size = 14, family = "serif")  
  )

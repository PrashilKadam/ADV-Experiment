library(ggplot2)
library(wordcloud)

data <- housing

head(data)

ocean_counts <- table(data$ocean_proximity)
wordcloud(names(ocean_counts), freq = ocean_counts, min.freq = 1, max.words = 100, random.order = FALSE)

ggplot(data, aes(x = ocean_proximity, y = median_house_value)) +
  geom_boxplot() +
  labs(title = "Box and Whisker Plot of Median House Value by Ocean Proximity",
   	x = "Ocean Proximity",
   	y = "Median House Value") +
  theme_minimal()

ggplot(data, aes(x = ocean_proximity, y = median_house_value)) +
  geom_violin(fill = "lightblue") +
  labs(title = "Violin Plot of Median House Value by Ocean Proximity",
   	x = "Ocean Proximity",
   	y = "Median House Value") +
  theme_minimal()

ggplot(data, aes(x = median_income, y = median_house_value)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Linear Regression Plot: Median Income vs Median House Value",
   	x = "Median Income",
   	y = "Median House Value") +
  theme_minimal()

ggplot(data, aes(x = median_income, y = median_house_value)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Non-linear Regression Plot: Median Income vs Median House Value",
   	x = "Median Income",
   	y = "Median House Value") +
  theme_minimal()

ggplot(data, aes(x = ocean_proximity, y = median_house_value)) +
  geom_jitter(alpha = 0.5, width = 0.2) +
  labs(title = "Jitter Plot of Median House Value by Ocean Proximity",
   	x = "Ocean Proximity",
   	y = "Median House Value") +
  theme_minimal()

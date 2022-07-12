#### Load in csv file ####

student_data <- read.csv("data/students_performance.csv")
View(student_data)

## Practice plotting with your dataset

ggplot(data = student_data, aes(x = gender, y = math.score)) +
  geom_bar(stat = "summary",
             fun = "mean") +
  labs(title = "Average Math Score vs Gender",
       x = "Gender",
       y = "Math Score")

ggplot(data = student_data, aes(x = math.score, y = reading.score, color = gender)) +
  geom_point() +
  labs(title = "Reading Scores vs Math Scores of Students",
       x = "Math Score",
       y = "Reading Score",
       color = "Sex")

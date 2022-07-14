## Practice plotting with your dataset

ggplot(data = student_data, aes(x = sex, y = math.score)) +
  geom_bar(stat = "summary",
           fun = "mean") +
  labs(title = "Average Math Score vs Sex",
       x = "Sex",
       y = "Math Score")

ggplot(data = student_data, aes(x = math.score, y = reading.score, color = sex)) +
  geom_point() +
  labs(title = "Reading Scores vs Math Scores of Students",
       x = "Math Score",
       y = "Reading Score",
       color = "Sex")

## Practice subsetting data
# use a combination of 
# filter, select, rename, mutate, arrange, summarize, group_by, sample, and/or slice
# create a visualization using your new subset of data

View(student_data)
names(student_data)

student_data <- rename(student_data, sex = gender)
student_data <- select(student_data, sex, math.score:writing.score, race.ethnicity, parental.level.of.education, test.preparation.course, lunch)

student_data_sample100 <- student_data %>%
  filter(lunch == "standard") %>%
  sample_n(100) %>%
  ggplot(aes(x = parental.level.of.education, y = reading.score, fill = sex)) +
  geom_violin() +
  labs(title = "Reading Score of Students And Their Parental Level of Education",
       x = "Parental Level of Education",
       y = "Reading Score",
       fill = "Sex")

student_data_sample100
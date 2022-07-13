#### Load in csv file ####

student_data <- read.csv("data/students_performance.csv")
View(student_data)

#### Save R Object as a file ####
saveRDS(student_data, "data/student_data.RDS")
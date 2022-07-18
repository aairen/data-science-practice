sleep_hours <- c(5,5,5,6,6,7,7,7,8,9)
mean(sleep_hours)
sd(sleep_hours)                 

t.test(sleep_hours, mu = 7, alternative = "less")

# wrong direction
t.test(sleep_hours, mu = 7, alternative = "greater")

## Using iris dataset
pop_mean <- mean(iris$Sepal.Length)

setosa <- filter(iris, Species == "setosa")

t.test(setosa$Sepal.Length, mu = pop_mean)

# If the sample mean (of setosa sepal lengths) is 5.84 (the same as the pop mean),
# then .00000000000000002% of the time, this difference (or more)
# will happen by chance

### Activity
Choose a different numeric variable (sepal width, petal lenght, petal width) and
compare any of the 3 species to the population mean (setosa, versicolor, or virgina).
Calculate the p-value using a one-sample t-test.

petal_len_pop_mean = mean(iris$Petal.Length)
versicolor = filter(iris, Species == "versicolor")

t.test(versicolor$Petal.Length, mu = petal_len_pop_mean)

# the p-value of 9.243e-10 is less than 0.05, so there is a significant difference
# and this data supports the alternative hypothesis

### two-sample t-test
versicolor <- filter(iris, Species == 'versicolor')
setosa <- filter(iris, Species == "setosa")

t.test(versicolor$Sepal.Length, setosa$Sepal.Length)

# (mean of setosa sepal lengths) - (mean of versicolor sepal lengths) != 0
# mean of setosa sepal lengths != mean of versicolor sepal lengths

## Compare versicolor and virginica sepal lenghts. What is the p-value? Is it significant?
versicolor <- filter(iris, Species == "versicolor")
virginica <- filter(iris, Species == "virginica")

t.test(versicolor$Sepal.Length, virginica$Sepal.Length)

## paired t-test
install.packages("datarium")
library(datarium)
?mice2
mice2

t.test(mice2$before, mice2$after, x = T)

# p-value of 1.039e-09 is less than 0.05, so there is a significant difference

### ANOVA
sepal_len_anova <- aov(data = iris, Sepal.Length ~ Species)

# Are any categories different?
summary(sepal_len_anova)

# Which groups are significantly different?
TukeyHSD(sepal_len_anova)

# Answer: all of them!

sepal_width_anova <- aov(data = iris, Sepal.Width ~ Species)
summary(sepal_width_anova)
TukeyHSD(sepal_width_anova)

# Let's look at the diamonds dataset
View(diamonds)
diamond_price_color <- aov(data = diamonds, price ~ color)
summary(diamond_price_color)

# to save results, use $cat_var
signif_results <- TukeyHSD(diamond_price_color)$color

# convert to dataframe so we can use dplyr functions
arrange(as.data.frame(signif_results), `p adj`)

### Practice with your own dataset
male = filter(student_data, gender == "male")
female = filter(student_data, gender == "female")
t.test(male$math.score, female$math.score)

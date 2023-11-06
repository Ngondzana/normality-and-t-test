                   # Hypothesis

# To compare if there is a difference in marks awarded by Dr.Filarnami abd Dr.Adebola
# Null Hypothesis: There is no difference in the mean of marks awarded by the two proffessors
# Alternative Hypothesis:There is a significant difference in the mean.

# Importing libraries

library("tidyverse") 
library("rstatix")
library("PairedData")
library("ggpubr")


#Importing our Grade dataset
data <- read_csv("Grade Data.csv")
head(data, 3)

# Converting character to factor
data <- data %>% mutate_if(is.character,as.factor)

glimpse(data)

#Normality test with the shapiro-wilk
# Compute the difference

d <- with(data, TestGrades[Class == "Dr.Filarnami"] - TestGrades[Class == "Dr.Adebola"])

# Shapiro-Wilk normality test for the differences

shapiro.test(d)

 #Descriptive statistics of dataset
group_by(data, Class) %>% 
  summarise(
    count = n(),
    mean = mean(TestGrades, na.rm = TRUE),
    sd = sd(TestGrades, na.rm = TRUE)
  )

#Using the rstatix library

stat.test <- data %>% 
  t_test(TestGrades ~ Class, paired = TRUE, detailed = TRUE) %>% add_significance
stat.test

#Data Visualization using box plot
# Plot Class by group and color by Test Grades

bxp <- ggpaired(data, x = "Class", y =  "TestGrades", color =
                  "Class", palette = c("#00AFBB", "#E7B800"),
                order = c("Dr.Filarnami", "Dr.Adebola"),
                xlab ="Class", ylab ="TestGrades")
stat.test <- stat.test %>% add_xy_position(x = "Class")
bxp +
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))

#Plot the paired data

# Subset test grades data by Dr.Filarnami
Dr.Filarnami <- subset(data, Class == "Dr.Filarnami", TestGrades, drop = TRUE)

# Subset test grades data by Dr.Adebola
Dr.Adebola <- subset(data, Class == "Dr.Adebola", TestGrades, drop = TRUE)

# Check if subsets have rows before proceeding
if (nrow(Dr.Filarnami) > 0 && nrow(Dr.Adebola) > 0) {
  # Plot paired data
  pd <- paired(Dr.Filarnami, Dr.Adebola)
  plot(pd, type = "profile") + theme_bw()
} else {
  print("No data available for one or both subsets.")
}

#Compute the paired t-test
res <- t.test(TestGrades ~ Class, data = data, paired = TRUE)
res



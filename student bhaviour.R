DF= read.csv("D:\\Statistics\\stats_data.csv", header=T, na.strings='')
head(DF)

dim(DF)colnames(DF)

#handling missing values
DF_clean <- na.omit(DF)
DF_clean

#remove duplicate values
DF_clean <- DF[!duplicated(DF), ]
DF_clean

summary(DF)


Percentage_mean<- mean(DF$GraduationMarks, na.rm = TRUE)
cat("Percentage Mean =", Percentage_mean, "\n")

Percentage_median<- median(DF$GraduationMarks, na.rm = TRUE)
cat("Percentage Median =", Percentage_median, "\n")

Percentage_sd <- sd(DF$GraduationMarks, na.rm = TRUE)
cat("Percentage standard deviation: ", Percentage_sd, "\n")

Percentage_var <- var(DF$GraduationMarks, na.rm = TRUE)
cat("Percentage variance: ", Percentage_var, "\n")

cv_vals <- Percentage_sd / Percentage_mean
cat("Coefficient of Variation:\n", cv_vals, "\n")

percentage_iqr <- IQR(DF$GraduationMarks, na.rm = TRUE)
cat("Percentage interquartile range: ", percentage_iqr, "\n")

unique(DF$Department)
 
library(ggplot2)


ggplot(DF, aes(x = When.do.you.prefer.to.study, fill = When.do.you.prefer.to.study)) +
  geom_bar() +
  labs(title = "Bar Chart of Prefered Study Time",
       x = "Prefered Study time",
       y = "Count") +
  scale_fill_manual(values = c("Morning" = "blue", "night" = "red", "Anytime"="green")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar plot for Gender
ggplot(DF, aes(x = Gender)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribution of Gender")

# Bar plot for Department
ggplot(DF, aes(x = Department)) +
  geom_bar(fill = "lightgreen") +
  labs(title = "Distribution of Departments")

# Histogram for Height
ggplot(DF, aes(x = Height)) +
  geom_histogram(fill = "orange", color = "black") +
  labs(title = "Distribution of Height")

# Box plot for Weight across Gender
ggplot(DF, aes(x = Gender, y = Weight)) +
  geom_boxplot(fill = "pink") +
  labs(title = "Distribution of Weight by Gender")



# Customize further as needed (e.g., add axis labels, change colors, etc.)

#1) CHI SQUARE TEST  

# Create the contingency table
contingency_table <- table(DF_clean$CertificationCourses , DF_clean$Willingness.to.Pursue.a.Career.Based.on.Your.Degree)

# Perform the chi-square test
chi_square_test <- chisq.test(contingency_table)

# Print the null and alternative hypotheses
cat("Null Hypothesis (H0): There is no significant association between Certification Courses and Willingness to Pursue a Career Based on Your Degree.\n")
cat("Alternative Hypothesis (H1): There is a significant association between Certification Courses and Willingness to Pursue a Career Based on Your Degree.\n\n")

# Print the results
print(chi_square_test)
#p-value (0.7489) is greater than 0.05 which means we accept the null hypothesis
#conclusion their is no significant difference between the given variables Certification and pursue degree


#2) T-TEST
# Subset your data frame to create two groups
group1 <- DF_clean$What.is.your.salary.expectation.for.a.potential.job[DF_clean$Gender == "Male"]
group2 <- DF_clean$What.is.your.salary.expectation.for.a.potential.job[DF_clean$Gender == "Female"]

# Perform a two-sample t-test
t_test_result <- t.test(group1, group2)


# Print the results
print(t_test_result)



#5) Kruskal-Wallis Test: for one continuous and one categorical with more than 1 values
kruskal_test_result <- kruskal.test(DF$GraduationMarks ~ DF$Department)
print(kruskal_test_result)
#p value is 0.007105 hence we reject the null hypothesis
#conclusion: Their is a significant difference in GraduationMarks and Department


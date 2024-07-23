getwd()
setwd("C:/Users/archa/OneDrive/Desktop/Data Visualization")
dir()
HR<-read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv", header = T)
dim(HR)
# 1. BAR PLOT
# Filter the columns of interest
df <- HR[, c("MonthlyIncome", "Age")]

# Create a factor variable for age groups
df$AgeGroup <- cut(df$Age, breaks = c(18, 25, 35, 45, 55, 65), labels = c("18-25", "26-35", "36-45", "46-55", "56-65"))

# Calculate the mean monthly income by age group
df <- aggregate(MonthlyIncome ~ AgeGroup, data = df, mean)

# Create a bar plot using barplot()
barplot(df$MonthlyIncome, names.arg = df$AgeGroup,ylim = c(0,15000), xlab = "Age Group", ylab = "Mean Monthly Income")

#2. BOX PLOT

# Create a box plot using boxplot() to determine relationship between Attrition and Job satisfaction

boxplot(HR$JobSatisfaction ~ HR$Attrition, main = "Job Satisfaction by Attrition", ylim = c(0,5),xlab = "Attrition", ylab = "Job Satisfaction")

#3. Histogram

# Filter the columns of interest
df <- HR[, c("PercentSalaryHike", "Attrition")]

# Create a factor variable for attrition status
df$Attrition <- factor(df$Attrition, labels = c("No", "Yes"))

# Create a histogram using hist()
hist(df$PercentSalaryHike, main = "Histogram of Percent Salary Hike vs Attrition", xlim = c(10,30),ylim = c(0,500),xlab = "Percent Salary Hike", col = "lightblue", border = "black")

# Filter the columns of interest
df <- HR[, c("MonthlyIncome", "YearsAtCompany", "Attrition")]

# Create a factor variable for attrition status
df$Attrition <- factor(df$Attrition, labels = c("No", "Yes"))

# Fit a linear model
model <- lm(YearsAtCompany ~ MonthlyIncome + Attrition, data = df)

# Create a scatter plot using plot()
plot(df$MonthlyIncome, df$YearsAtCompany, col = df$Attrition, pch = as.numeric(df$Attrition), xlab = "Monthly Income", ylab = "Years at Company", main = "Scatter Plot of Monthly Income vs Years at Company")
abline(model, col = "green")

# Box Plot
boxplot(HR$DistanceFromHome ~ HR$Attrition, main = "Distance from home by Attrition", xlab = "Attrition", ylab = "Distance from home")

#Pie Chart

par(mfrow = c(1, 1))
# Count the frequency of EducationField
freq <- table(HR$JobRole)

# Plot the frequency as a pie chart
pie(freq, main = "Count of Job Role", col = rainbow(6))


# Histogram with Density Plot
# Split the data by Attrition
data_yes <- subset(HR, Attrition == "Yes")
data_no <- subset(HR, Attrition == "No")

# Plot Percentage Salary Hike by Attrition
hist(data_yes$PercentSalaryHike, prob = TRUE, col = "red", main = "Percentage Salary Hike by Attrition", xlab = "Percentage Salary Hike", xlim = c(0, 30), ylim = c(0, 0.8))
lines(density(data_yes$PercentSalaryHike), col = "red")
hist(data_no$PercentSalaryHike, prob = TRUE, col = "blue", add = TRUE)
lines(density(data_no$PercentSalaryHike), col = "blue")
legend("topright", legend = c("Yes", "No"), fill = c("red", "blue"))



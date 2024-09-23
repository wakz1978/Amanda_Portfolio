# Import Libraries
library(writexl)
suppressMessages(library(pROC))

filepath1 <- "EmployeeRetentionPredictiveAnalytics(R)\\employees2.csv"

filepath2 <- "EmployeeRetentionPredictiveAnalytics(R)\\dictionary.csv"

#Import data files
employees <- read.csv(filepath1, stringsAsFactors = TRUE)
dictionary <- read.csv(filepath2, header = FALSE, col.names = c('SurveyQuestion', 'QuestionDetail'))

# Write Dictionary to an excel file for report
write_xlsx(dictionary, 'Dictionary.xlsx')

# Set parameters for plots
par(
  # Specify the margins: bottom, left, top, right
  mar = c(6, 10, 10, 6),
  cex.axis = 0.9
)


# Create a barplot of employees that have left and stayed
mids <- barplot(table(employees$Attrition),
  main = "Number of Employees Left Globex \n Since 2023 Employee Survey",
  ylim = c(0, 1000),
  col = c("lightblue", "indianred1"),
  las = 1,
  names.arg = c("Stayed", "Left")
)

# Place count data of number employees who haveleft and stayed on the bars
text(
  x = mids,
  y = table(employees$Attrition),
  labels = table(employees$Attrition),
  pos = 3
)

# Specify the margins: bottom, left, top, right
par(
  mar = c(6, 11, 4.1, 2.1)
)


# Create a barplot of Overtime in the organisaton
mids2 <- barplot(
  table(employees$OverTime),
  main = "Overtime at Globex",
  ylim = c(0, 1000),
  col = c("lightblue", "indianred1"),
  las = 1,
  names.arg = c("No", "Yes")
)

# create labels of number of employees on bars
text(
  x = mids2,
  y = table(employees$OverTime),
  labels = table(employees$OverTime),
  pos = 3
)

#Create data of employees Working Years less than 3 years
junior_employees <- aggregate(Department ~ TotalWorkingYears,
  data = employees[employees$TotalWorkingYears < 3, ],
  FUN = length
)

# Create Plot of number of mployees in Years 1 - 3
mids6 <- barplot(
  junior_employees$Department ~ junior_employees$TotalWorkingYears,
  main = "WorkingYears <3 @ Globex \n",
  col = c("lightblue2", "dodgerblue","cadetblue"),
  names.arg = c("First Year", "Second Year", "Third Year"),
  ylab = "Number of Employees",
  xlab = "",
  las = 1
)

# Create labels
text(
  x = mids6,
  y = junior_employees$Department,
  labels = junior_employees$Department,
  pos = 3
)

# Create data for Attrition By Role Employees less than 3 years
junior_employees2 <- prop.table(
  table(junior_employees$Attrition, junior_employees$JobRole),
  margin = 2
)

junior_employees2 <- junior_employees2[,colnames(junior_employees2) %in% c("Human Resources", "Sales Representative", "Laboratory Technician", "Research Scientist")]
junior_employees2 <- junior_employees2[,order(junior_employees2[2,])]

par(
  mar = c(6, 2, 10, 6)
)

# create a barplot of the Working Years Table 
mids3 <- barplot(
  junior_employees2,
  main = "Total Working Years Less Than 3 Years \n Attrition By Role",
  xlab = "",
  yaxt= "n",
  col = c("lightblue2", "indianred1"),
  las = 1,
  beside = T,
  ylim = c(0, 0.8),
  cex.lab = 1
)

# Create labels for plot
text(
  x = mids3,
  y = junior_employees2,
  labels = paste(round(junior_employees2,2) * 100, "%"),
  pos = 3
)

# Create a legend
legend(
  # Put at the top
  "topright",
  xpd = TRUE,
  inset = c(-.1, 0),
  # Specify the labels in the legend
  legend = c("Stayed", "Left"),
  # Specify the colours in the legend
  fill = c("cadetblue1","indianred1"),
  # Remove box from legend
  bty = "n"
)

median(employees$MonthlyIncome)

junior_wages <- subset(employees, employees$TotalWorkingYears < 3)

median(junior_wages$MonthlyIncome)

# Create data for Attrition By Business Treval
travel_vs_leavers <- prop.table(table(employees$Attrition, employees$BusinessTravel), margin = 2)
travel_vs_leavers <- travel_vs_leavers[,order(travel_vs_leavers[2,])]

par(
  mar = c(4,4,4,4)
)

# Create Barplot
mids4 <- barplot(
  travel_vs_leavers[,order(travel_vs_leavers[2,])],
  las = 1,
  beside = T,
  yaxt = "n",
  width = c(10,9),
  col = c("lightblue", "indianred1"),
  ylim = c(0,1),
  main = "Percentage of Employees Left By Business Travel"
)

# Create labels for plot
text(
  y = travel_vs_leavers[,order(travel_vs_leavers[2,])],
  x = mids4,
  labels = paste(round(travel_vs_leavers,2) * 100, "%"),
  pos = 3
  )

# Create a legend
legend(
  # Put at the top
  "topright",
  # Specify the labels in the legend
  legend = c("Stayed", "Left"),
  # Specify the colours in the legend
  fill = c("cadetblue1","indianred1"),
  # Remove box from legend
  bty = "n"
)


par(
  mar = c(8,4,4,4)
)

# Get data for employees over $8164 Monthly Income
highincome_leavers <- subset(employees, employees$MonthlyIncome > 8164)
nostock_vs_leavers <- prop.table(
  table(highincome_leavers$Attrition, highincome_leavers$StockOptionLevel), 
        margin = 2)

# Create a plot Attrition By Stock Option leve, Monthly Income $8164
mids5 <- barplot(
  nostock_vs_leavers,
  las = 1,
  beside = T,
  yaxt = "n",
  width = c(10,9),
  col = c("lightblue", "indianred1"),
  ylim = c(0,1),
  names.arg = c("None", "Some", "High", "Very High"),
  main = "Percentage of Leavers \n Income > $8164 vs. Stock Option Level",
)

# create labels for plot
text(
  y = nostock_vs_leavers,
  x = mids5,
  labels = paste(round(nostock_vs_leavers,2) * 100, "%"),
  pos = 3
)

# Create a legend
legend(
  # Put at the top
  "topright",
  xpd = T,
  inset = c(-.1, 0),
  # Specify the labels in the legend
  legend = c("Stayed", "Left"),
  # Specify the colours in the legend
  fill = c("cadetblue1","indianred1"),
  # Remove box from legend
  bty = "n"
)
 


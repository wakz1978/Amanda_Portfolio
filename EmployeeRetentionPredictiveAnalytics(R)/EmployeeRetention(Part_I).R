# R-Code 
# Import employees csv file, treat any columns with strings as factors
employees_df <- read.csv('employees.csv', stringsAsFactors = T)
survey_dict <- read.csv('Dictionary.csv', header = FALSE, col.names = c('SurveyQuestion', 'QuestionDetail'))


# RESEARCH QUESTION ONE - HOW DOES INCOME VARY BY JOB?
# Set Plot parameters
par(
  # Specify the margins: bottom, left, top, right
  mar = c(3, 11, 3, 3),
  cex.axis = 0.7
)

# Calculate the median income for each job role
income_by_job <- aggregate(
  MonthlyIncome ~ JobRole,
  employees_df,
  FUN = median
)

# Plot data for distribution of income by Job Role
boxplot(
  MonthlyIncome ~ JobRole,
  data = employees_df,
  horizontal = TRUE,
  las = 1,
  ylab = "",
  col = c(rep("cadetblue",5),rep("indianred",1),rep("cadetblue",3)),
  main = "Income Distribution By Job Role",
  xlab = "Monthly Income $"
)

# Filter the data to "Research Director" Job Role Only
rdirector_data <- subset(employees_df,employees_df$JobRole == 'Research Director')

# Create a summary of the data for Research Directors
summary(rdirector_data$MonthlyIncome)

par(
  # Specify the margins: bottom, left, top, right
  mar = c(3, 6, 3, 3),
  cex.axis = 0.7
)
#Plot Distribution of Gender amongst Research Directors
plot(
  rdirector_data$Gender,
  ylab = "Number of Employees",
  col = c("hotpink","lightblue2"),
  main = "Research Directors Gender Distribution"
)


# Create 1 x 2 parameters for next plots
par(
  mfrow = c(1,2),
  mar = c(5,4,4,1),
  cex.axis = 0.6,
  las = 1,
  cex.main = 0.9
)

# Create a histogram of distribution of Monthly Income for Female R. Directors
hist(
  rdirector_data$MonthlyIncome[rdirector_data$Gender == "Female"],
  main = "Female Research Director Wage",
  xlim = c(10000, 20000),
  ylim = c(0,12),
  breaks = 8,
  col = "hotpink",
  xlab = "Monthly Income $",
  ylab = "Frequency"
  )

# Create a histogram of distribution of Monthly Income for Male R. Directors
hist(
  rdirector_data$MonthlyIncome[rdirector_data$Gender == "Male"],
  main = "Male Research Director Wage",
  xlim = c(10000, 20000),
  ylim = c(0,12),
  breaks = 8,
  col = "lightblue1",
  xlab = "Monthly Income $",
  ylab = "Frequency"
)

# Calculate the median years in current role and age by Gender for Research Directors
years_by_gender <-  aggregate(
  cbind(YearsInCurrentRole, Age) ~ Gender,
  data = rdirector_data,
  FUN = median
)

# Bar Plot of Median Years in Current Role By Gender
barplot(
  YearsInCurrentRole ~ Gender,
  data = years_by_gender,
  col = c("hotpink","lightblue"),
  main = "Years In Role",
  ylab = "",
  xlab = ""
)

#Bar Plot of Median Age in Current Role By Gender
barplot(
  Age ~ Gender,
  data = years_by_gender,
  ylim = c(0,50),
  col = c("hotpink","lightblue"),
  main = "Age",
  ylab = "",
  xlab = ""
)

# RESEARCH QUESTION 2 - HOW DO STOCK OPTION LEVELS GET AWARDED?
# Proportions of Employees With Stock Options
prop_stockoption <-prop.table(
  table(employees_df$StockOptionLevel)
)

# Set Up Pie Piece Labels with Stock Option Level and % of Employees from table above
pie_labels <- paste(c("None", "Some", "High", "Very High"), paste(prop_stockoption * 100, "%"))

# Reset parameters for plots and margin
par(
  mfrow = c(1, 1),
  mar = c(6, 6, 3, 6),
  cex.main = 0.9
)

# Create Pie Chart % employees at each stock option level
pie(
  # Use the data from the proportion table
  prop_stockoption, 
  # Use the Pie Labels set up earlier
  labels = pie_labels,
  # Set up title and resize 
  main = "Percentage Employees By Stock Option Level",
  # Start wedge at 0 degrees
  clockwise = TRUE,
  # Colours of Wedges
  col = c('indianred1', 'seagreen2', 'cyan','mediumorchid2')
)

# Grab all employees who have stock level > "None"/1
stockholders <- employees_df[employees_df$StockOptionLevel >=1,]

# Count the number of employees who have stock option levels >"None"/1 in each role, rename columns
stockholder_frequencies <- setNames(aggregate(
  stockholders$StockOptionLevel ~ stockholders$JobRole,
  data = stockholders,
  FUN = length),
  c('JobRole', 'NumWithStock')
)
stockholder_frequencies

#O rder Median Income Highest to Lowest
income_by_job <- income_by_job[order(income_by_job$MonthlyIncome),]
income_by_job

# Merge Number Employees Stock Options and Median Monthly Income By Role
merged_data1 <- merge(stockholder_frequencies, income_by_job, by = "JobRole")

# Grab all employees who have No Stock Options
no_stock_data <- employees_df[employees_df$StockOptionLevel == 0,]

# Count the number of employees who have No Stock Option By Role, rename columns
nostock_frequencies <- setNames(aggregate(
  no_stock_data$StockOptionLevel ~ no_stock_data$JobRole,
  data = no_stock_data,
  FUN = length),
  c('JobRole', 'NumWithNoStock')
)
nostock_frequencies

# Make a dataframe of Number of Employees By Role who have stock > "None", and who have "None"
# Ordered By Median Monthly Income highest to lowest
merged_data2 <- merge(merged_data1, nostock_frequencies, by = "JobRole")
merged_data2
stockoption_byrole <- merged_data2[order(merged_data2$MonthlyIncome),]

par(
  mar = c(6, 10, 3, 10),
  cex.main = 0.9,
  cex.axis = 0.7
)

# Generate Bar plot show Num EMployees with and without stock, each job role
barplot(
  # Grab the two columns stock and no stock for the y axis, by Job Role on x axis
  cbind(NumWithNoStock, NumWithStock) ~ JobRole,
  data = stockoption_byrole,
  # side by side bars, horiztonal bars
  beside = TRUE,
  horiz = TRUE,
  # Vertical labels on x axis, Label for y axis, remove z axis label
  las = 1,
  xlab = "Number of Employees",
  ylab = "",
  # Colour the bars
  col = c("indianred1", "cadetblue1"),
  # Change the Y axis to be clearer
  xlim = c(0,140),
  # Title for the Plot
  main = "Stock Options By Job Role, Highest to Lowest Earners",
  cex.names = 0.7
)

legend(
  # Put at the top
  "right",
  # Outside plot margin, set position
  xpd = TRUE,
  inset = c(-.65, 0),
  # Specify the labels in the legend
  legend = c("Stock Options", "No Stock Options" ),
  # Specify the colours in the legend
  fill = c("cadetblue1","indianred1"),
  # Change size text in legend
  cex = .7,
  # Remove box from legend
  bty = "n"
)
# income_by_job <- income_by_job[order(income_by_job$MonthlyIncome),]
# income_by_job

# Create Bar Plot of Job Role By Income Hight to Low
barplot(
  income_by_job$MonthlyIncome,
  names.arg = income_by_job$JobRole,
  main = "Median Monthly Income By Job Role",
  horiz = TRUE,
  las = 1,
  cex.names = 0.7,
  xlab = "Monthly Income $",
  ylab = "",
  xlim = c(0,20000),
  col = ifelse(income_by_job$MonthlyIncome < 3500, "#c6ffff",
               ifelse(income_by_job$MonthlyIncome  >= 8000, "darkblue", "#86c6ff")),
  cex.axis = 0.9
)
legend = legend(
  "right",
  xpd = TRUE,
  inset = c(-.95, 0),
  cex = 0.7,
  bty = "n",
  legend = c("Income > $8000", "$3500 <= Income < $8000","Income < $3500"),
  fill = c("darkblue","#86c6ff","#c6ffff"),
  
)

par(
  # Specify the margins: bottom, left, top, right
  mar = c(6, 4, 3, 3),
  cex.axis = 0.8
)

# Distrubtion of Incomes By Stock Option Level Boxplot
boxplot(
  employees_df$MonthlyIncome ~ employees_df$StockOptionLevel ,
  data = employees_df,
  # data = employees_df[employees_df$StockOptionLevel == 0,],
  xlab = "Stock Option Level",
  names = c("None", "Some", "High", "Very High"),
  ylab = "Monthly Income $",
  col = "lightblue",
  main = "Distribution of Incomes By Stock Option Level"
  )

# Filter the outliers from boxplot, income >15000 and no stock option level
highincome_nostock <- employees_df[employees_df$StockOptionLevel == 0 & employees_df$MonthlyIncome > 15000,]

# Group Outliers by Dept and Name and count frequencies
highincome_nostock <- setNames(aggregate(
  MonthlyIncome ~ Department + JobRole,
  data = highincome_nostock,
  FUN = length),
  c('Dept', 'JobRole', 'NumNoStockHighIncome')
)

# Write to an Excel Table
library(writexl)
write_xlsx(highincome_nostock,'HighIncomeNoStock.xlsx')
print(highincome_nostock)


### RESEARCH QUESTION THREE WORKLIFE BALANCE = BAD

par(
  # Specify the margins: bottom, left, top, right
  mar = c(6, 4, 3, 3),
  cex.axis = 0.8
)

# Show distribution of Monthly Income by Work Life Balance Rating 1-4
boxplot(
  MonthlyIncome ~ WorkLifeBalance,
  data = employees_df,
  # Format colours, axis text size and titles
  col = "lightblue",
  cex.axis = .7,
  las = 1,
  main = "Distribution of Income By Work Life Balance Satisfaction",
  xlab = "Work Life Balance Rating",
  ylab = "Monthly Income $",
  # Names for each boxplot
  names = c('Bad', 'Satisfactory', 'Good', 'Very Good')
)

# Create a table of breakdown of work life balance rating for each job role
worklife_by_job <- table(
  employees_df$WorkLifeBalance, 
  employees_df$JobRole
)

# Calculate the median income for each job role
income_by_job <- aggregate(
  MonthlyIncome ~ JobRole,
  employees_df,
  FUN = median
)

# Order highest to lowest income
income_by_job <- income_by_job[order(-income_by_job$MonthlyIncome),]
income_by_job

# Calculate the Median Monthly Income By Work Life Balance Level
income_by_workbalance <- aggregate(
  MonthlyIncome ~ WorkLifeBalance,
  data = employees_df,
  FUN = median
)
# Rename Columns appropriately
colnames(income_by_workbalance) <- c("WorkLifeBalance","MedianMonthlyIncome")

#Order the Table of work_life_by_job from highest to lowest income
worklife_by_job <- worklife_by_job[,income_by_job$JobRole[order(income_by_job$MonthlyIncome)]]
worklife_by_job

# Create parameters for plots
par(
  # Specify the margins: bottom, left, top, right
  mar = c(8, 10, 2, 8)
)

# Generate  bar plot 
barplot(
  # Calculate the  proportion of Work Life Balance rating in each Job Role
  prop.table(worklife_by_job, margin = 2),
  # Labels in line with the axis
  las = 1,
  # Change axis text size and name size
  cex.axis = 0.7,
  cex.names = 0.7,
  # Horizontal Bar Plot not vertical
  horiz = TRUE,
  # Colours for each Work Life Balance Rating Bad - Very Good
  col = c('indianred3', 'lightgoldenrod3', 'cornflowerblue', 'royalblue3'),
  # Labels and Title
  xlab = " Proportion of Employees",
  main = "Work Life Balance Survey Rating By Role"
)

# Create a legend
legend(
  "right",
  # Put legend outside the plot area
  xpd = TRUE,
  # Inset legend
  inset = c(-.55, 0),
  # Change size text
  cex = 0.85,
  # Remove border from legend
  bty = "n",
  # Labels
  legend = c('Bad', 'Satisfactory', 'Good', 'Very Good'),
  # Colours for each label
  fill = c('indianred3', 'lightgoldenrod3', 'cornflowerblue', 'royalblue3')
)

# Create new parameters for plot 
par(
  # Specify the margins: bottom, left, top, right
  mar = c(6, 6, 3, 6)
)

# Generate barplot median monthly income by rating of work life balance rating
barplot(
  MedianMonthlyIncome ~ WorkLifeBalance,
  data = income_by_workbalance,
  # Labels, fontsize, titles
  names.arg = c('Bad', 'Satisfactory', 'Good', 'Very Good'),
  cex.names = 0.7,
  cex.axis = 0.7,
  yaxt = "n",
  ylab = "Monthly Income $",
  xlab = 'Survey Rating for Work Life Balance',
  ylim = c(0,6000),
  main = 'Pay vs. Work Life Balance Satisfaction?',
  col = c('indianred2', 'lightblue2', 'lightblue2', 'lightblue2'),
)

# Generate own ticks for Y-axis
axis(
  # Y axis
  side = 2, 
  # Tick marks
  at = c(0, 2000, 4000, 6000), 
  # labels and axis size
  las = 1, 
  cex.axis = 0.65, 
  labels = c(0, "$2000", "$4000", "$6000")
)

# Create frequency of "Bad" Work Life Balance Rating for each role
frequencybad_byrole <- aggregate(
  Department ~ JobRole,
  data = employees_df[employees_df$WorkLifeBalance == 1,],
  FUN = length
)

# Rename Columns 
colnames(frequencybad_byrole)[colnames(frequencybad_byrole) == 'Department'] <- "NumEmployees"

# Parameters for plots
par(
  # Specify the margins: bottom, left, top, right
  mar = c(10, 11, 2, 12)
)
# Order By Frequency lowest to highest
frequencybad_byrole <- frequencybad_byrole[order(frequencybad_byrole$NumEmployees),]
frequencybad_byrole

# Merge data of frequency bad ratings and median monthly incomes for each job role
merged_data <- merge(frequencybad_byrole, income_by_job)
# Order by income level
merged_data <- merged_data[order(merged_data$MonthlyIncome),]
merged_data

par(
  # Specify the margins: bottom, left, top, right
  mar = c(3, 11, 3, 6)
)

# Bar Plot 
barplot(
  # Frequency of Employees Bad Work Life Balance by Job Role
  merged_data$NumEmployees,
  names.arg = merged_data$JobRole,
  # Horizontal Plot, not vertical
  horiz = TRUE,
  # Colour bar less than $3500 lightblue, medium blue middle income, dark blue
  # for high income
  col = ifelse(merged_data$MonthlyIncome < 3500, "#c6ffff",
               ifelse(merged_data$MonthlyIncome  > 8000, "darkblue", "#86c6ff")
  ),
  # Format labels, axis, fontsize, titles
  las = 1,
  xlim = c(0,20),
  ylab = "",
  cex.names = .7,
  cex.axis = .7,
  main = "Work Life Balance Survey Rating = 'Bad'",
  xlab = "Number of Employees By Role"
  
)

# Create and format legend
legend = legend(
  "right",
  xpd = TRUE,
  inset = c(-.35, 0),
  cex = 0.85,
  bty = "n",
  legend = c("High Income Role", "Medium Income Role", "Low Income Role"),
  fill = c( "darkblue","#c6ffff", "#86c6ff")
)

# Calculate the number of employees in each job role
num_by_role <- setNames(
  aggregate(
    Department ~ JobRole, 
    data = employees_df,
    FUN = length 
  ),c('JobRole', 'NumEmployees'))

# Order number of employees in each job role
num_by_role <- num_by_role[order(num_by_role$NumEmployees),]


# BarPlot
barplot(
  # Frequency of employees in each job role
  num_by_role$NumEmployees,
  names.arg = num_by_role$JobRole,
  # data = num_by_role,
  horiz = TRUE,
  # Format labels, axis, colours and title
  las = 1,
  cex.axis = 0.7,
  cex.names = 0.7,
  xlim = c(0, 250),
  col = ifelse(num_by_role$NumEmployees > 100, "orange", "cadetblue"),
  main = "Number Of Employees By Role"
)


# Create and format legend
legend = legend(
  "right",
  xpd = TRUE,
  inset = c(-.45, 0),
  cex = 0.85,
  bty = "n",
  legend = c("Employee Headcount > 100", "Employees Headcount < 100"),
  fill = c( "orange", "cadetblue")
)


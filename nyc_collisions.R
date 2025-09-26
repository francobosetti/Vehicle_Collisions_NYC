library(readr)
nyc <- read_csv("Motor_Vehicle_Collisions_-_Crashes.csv")

nyc$CRASH.DATE <- as.Date(nyc$`CRASH DATE`, format = "%m/%d/%Y")
nyc$BOROUGH <- factor(nyc$BOROUGH)
nyc$YEAR_MONTH <- format(nyc$CRASH.DATE, "%Y-%m")
nyc$YEAR <- format(nyc$CRASH.DATE, "%Y")

str(nyc)

# Contingency Table: Borough VS Injury Severity

nyc$injury_severity <- ifelse(nyc$`NUMBER OF PERSONS KILLED` > 0, "FATAL/DIED",
                       ifelse(nyc$`NUMBER OF PERSONS INJURED` > 0, "INJURY", "NO INJURY"))

# remove NA's
borough_injury_data <- nyc[!is.na(nyc$BOROUGH) & !is.na(nyc$injury_severity), ]
(borough_injury_table <- table(borough_injury_data$BOROUGH, borough_injury_data$injury_severity))

# Chi-square test
(chi_test1 <- chisq.test(borough_injury_table))
# p = 0.0001 < alpha = 0.05, we reject the null hypothesis

# Row proportions (by borough) 
(prop_table1 <- prop.table(borough_injury_table, 
                           main = "Borough vs. Injury Severity", ylab = "Proportions by Borough", 
                           margin = 1))
barplot(prop_table1)

# Cramer's v
library(rcompanion)
cramerV(borough_injury_table)

# Scatterplot: Total Persons Injured vs Killed
injury_data <- nyc[nyc$`NUMBER OF PERSONS INJURED` > 0 | nyc$`NUMBER OF PERSONS KILLED` > 0, ]

plot(injury_data$`NUMBER OF PERSONS INJURED`, 
     injury_data$`NUMBER OF PERSONS KILLED`,
     main = "Relationship Between Injuries and Fatalities in NYC Crashes",
     xlab = "Number of Persons Injured",
     ylab = "Number of Persons Killed",
     pch = 16,
     col = "darkblue",
     cex = 0.6)

abline(lm(injury_data$`NUMBER OF PERSONS KILLED` ~ injury_data$`NUMBER OF PERSONS INJURED`), 
       col = "red", lwd = 2)

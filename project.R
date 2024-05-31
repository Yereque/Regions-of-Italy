#Group 1
#Torebek Yerassyl
#Sailau Dinara
#Kanatbekov Iliyas


sdgs <- read.csv2('sdgs.csv')
View(sdgs)

#A)  Firstly we retrieved each region seperatly
#Lombardia
lombardia_row <- sdgs[sdgs$region == "Lombardia", ]
lombardia_gdi <- unlist(lombardia_row[, grep("GDI_pc", names(lombardia_row))])
gdi_2009_lombardia <- sdgs[sdgs$region == "Lombardia", "GDI_pc_2009"]
fixed_base_index_lombardia <- (lombardia_gdi / gdi_2009_lombardia) * 100
fixed_base_index_lombardia

#Trento
trento_row <- sdgs[sdgs$region=="Trento",]
trento_gdi <- unlist(trento_row[,grep("GDI_pc",names(trento_row))])
gdi_2009_trento <- sdgs[sdgs$region == "Trento", "GDI_pc_2009"]
fixed_base_index_trento <- (trento_gdi / gdi_2009_trento) * 100
fixed_base_index_trento

#Lazio
lazio_row <- sdgs[sdgs$region=="Lazio",] 
lazio_gdi <- unlist(lazio_row[,grep("GDI_pc",names(lazio_row))]) 
gdi_2009_lazio <- sdgs[sdgs$region == "Lazio", "GDI_pc_2009"] 
fixed_base_index_lazio <- (lazio_gdi / gdi_2009_lazio) * 100 
fixed_base_index_lazio

#Calabria
calabria_row <- sdgs[sdgs$region=="Calabria",]
calabria_gdi <- unlist(calabria_row[,grep("GDI_pc",names(calabria_row))])
gdi_2009_calabria <- sdgs[sdgs$region == "Calabria", "GDI_pc_2009"]
fixed_base_index_calabria <- (calabria_gdi / gdi_2009_calabria) * 100
fixed_base_index_calabria

#Sardegna
sardegna_row <- sdgs[sdgs$region=="Sardegna",]
sardegna_gdi <- unlist(sardegna_row[,grep("GDI_pc",names(sardegna_row))])
gdi_2009_sardegna <- sdgs[sdgs$region == "Sardegna", "GDI_pc_2009"]
fixed_base_index_sardegna <- (sardegna_gdi / gdi_2009_sardegna) * 100
fixed_base_index_sardegna

#b. Line Graphs
#Lombardia
years <- 2009:2019
plot(years, fixed_base_index_lombardia, type = "o", col = "blue", ylim = c(97, max(fixed_base_index_lombardia) + 0.5), 
main = "Fixed Base Index for Lombardia", xlab = "Year", ylab = "Fixed Base Index")
lines(years, fixed_base_index_lombardia, col = "blue", type = "o")
grid()
legend("topright", legend = "Lombardia", col = "blue", pch = 1)

#Trento
years <- 2009:2019
plot(years, fixed_base_index_trento, type="o", col="red", ylim=c(97, max(fixed_base_index_trento) + 0.5), 
main="Fixed Base Index for Trento", xlab="Year", ylab="Fixed Base Index")
lines(fixed_base_index_trento, col="red", type="o")
grid()
legend("topright", legend="Trento", col="red", pch=1)

#Lazio
years <- 2009:2019
plot(years, fixed_base_index_lazio, type="o", col="green", ylim=c(95, max(fixed_base_index_lazio) + 0.5), 
main="Fixed Base Index for Lazio", xlab="Year", ylab="Fixed Base Index")
lines(fixed_base_index_lazio, col="green", type="o")
grid()
legend("topright", legend="Lazio", col="green", pch=1)

#Calabria
years <- 2009:2019
plot(years, fixed_base_index_calabria, type="o", col="purple", ylim=c(97, max(fixed_base_index_calabria) + 0.5), 
main="Fixed Base Index for Calabria", xlab="Year", ylab="Fixed Base Index")
lines(fixed_base_index_calabria, col="purple", type="o")
grid()
legend("topright", legend="Calabria", col="purple", pch=1)

#Sardegna
years <- 2009:2019
plot(years, fixed_base_index_sardegna, type="o", col="orange", ylim=c(99, max(fixed_base_index_sardegna) + 0.5), 
main="Fixed Base Index for Sardegna", xlab="Year", ylab="Fixed Base Index")
lines(fixed_base_index_sardegna, col="orange", type="o")
grid()
legend("topright", legend="Sardegna", col="orange", pch=1)

# Создание пустого графика
plot(years, fixed_base_index_lombardia, type = "o", col = "blue", 
     ylim = c(95, max(fixed_base_index_lombardia, fixed_base_index_trento, fixed_base_index_lazio, fixed_base_index_calabria, fixed_base_index_sardegna) + 0.5), 
     main = "Fixed Base Index for Multiple Regions", xlab = "Year", ylab = "Fixed Base Index")

lines(years, fixed_base_index_trento, col = "red", type = "o")
lines(years, fixed_base_index_lazio, col = "green", type = "o")
lines(years, fixed_base_index_calabria, col = "purple", type = "o")
lines(years, fixed_base_index_sardegna, col = "orange", type = "o")

legend("topright", legend = c("Lombardia", "Trento", "Lazio", "Calabria", "Sardegna"), 
       col = c("blue", "red", "green", "purple", "orange"), pch = 1, cex = 0.8)


#c. Best perfomed Region
selected_regions <- sdgs[sdgs$region %in% c('Lombardia', 'Trento', 'Lazio', 'Calabria', 'Sardegna'), ]

selected_regions$percentage_change <- ((selected_regions$GDI_pc_2019 - selected_regions$GDI_pc_2014) / selected_regions$GDI_pc_2014) * 100

best_region <- selected_regions[which.max(selected_regions$percentage_change), ]

print(selected_regions)
print(paste("Region with the highest percentage change:", best_region$region))
print(paste("Percentage change of the best performing region:", round(best_region$percentage_change, 2), "%"))

#Question 2

#A
install.packages("moments")
library(moments)
sdgs <- read.csv2('sdgs.csv')
sdgs$GDI_pc_2018 <- as.numeric(gsub(",", "\\.", as.character(sdgs$GDI_pc_2018)))

gdi_2018_data <- sdgs$GDI_pc_2018
gdi_2018_data

boxplot(gdi_2018_data, main = "Distribution of GDI_pc_2018 among Italian Regions/Provinces", ylab = "GDI_pc_2018")

skewness(gdi_2018_data)

outliers <- boxplot.stats(gdi_2018_data)
outliers

#B
quartiles <- quantile(gdi_2018_data)
mean_value <- mean(gdi_2018_data)
median_value <- median(gdi_2018_data)
std_deviation <- sd(gdi_2018_data)
lower_quartile <- quartiles[2]
upper_quartile <- quartiles[4]

gdi_2018_data
mean_value
median_value
std_deviation
lower_quartile
upper_quartile
#C
sdgs$GDI_pc_2018 <- as.numeric(gsub(",", "\\.", as.character(sdgs$GDI_pc_2018)))

# Boxplot
boxplot(GDI_pc_2018 ~ division, data = sdgs, 
        col = c("red", "green", "blue"),  # Задаем цвета для каждой категории
        main = "Distribution of GDI_pc_2018 among Italian Regions/Provinces",
        xlab = "Division", ylab = "GDI_pc_2018")

northern_outliers <- sdgs[sdgs$division %in% c("North"), ]
northern_outliers <- northern_outliers[sdgs$GDI_pc_2018 > quantile(sdgs$GDI_pc_2018, 0.75) + 1.5 * IQR(sdgs$GDI_pc_2018) | 
                                         sdgs$GDI_pc_2018 < quantile(sdgs$GDI_pc_2018, 0.25) - 1.5 * IQR(sdgs$GDI_pc_2018), ]
print(northern_outliers)

iqr_by_division <- tapply(sdgs$GDI_pc_2018, sdgs$division, IQR)
print(iqr_by_division)

#D
income_2019 <- data$GDI_pc_2019

boxplot(income_2019, col = "skyblue", main = "Boxplot of 2019 Gross Disposable Income")

sorted_income <- sort(income_2019)

total_income <- sum(sorted_income)
cumulative_income <- cumsum(sorted_income) / total_income
cumulative_population <- seq(1, length(cumulative_income)) / length(cumulative_income)

plot(cumulative_population, cumulative_income, type = "l", col = "green", 
     xlab = "Cumulative proportion of population", ylab = "Cumulative share of income",
     main = "Lorenz Curve for Total 2019 Gross Disposable Income")
abline(0, 1, col = "red", lty = 2)
grid()

#E Compute also the related Gini index.

lorenz_curve <- function(x) {
  approx_x <- approxfun(cumulative_population, cumulative_income)
  y <- approx_x(x)
  ifelse(is.finite(y), y, 0)  # Replace non-finite values with 0
}

area_under_curve <- integrate(lorenz_curve, lower = 0, upper = 1)$value

gini_index <- 1 - 2 * area_under_curve

print(gini_index)


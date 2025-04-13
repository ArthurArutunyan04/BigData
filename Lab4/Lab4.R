#1
library(rvest)

extract_year_data <- function(year) {
  url <- paste0("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=", year)
  webpage <- read_html(url)
  
  rows <- html_nodes(webpage, "tbody tr")
  
  data <- lapply(rows, function(row) {
    cells <- html_nodes(row, "td")
    list(
      Country = html_text(cells[2]),
      Quality_of_Life_Index = html_text(cells[3]),
      Purchasing_Power_Index = html_text(cells[4]),
      Safety_Index = html_text(cells[5]),
      Health_Care_Index = html_text(cells[6]),
      Cost_of_Living_Index = html_text(cells[7]),
      Property_Price_to_Income_Ratio = html_text(cells[8]),
      Traffic_Commute_Time_Index = html_text(cells[9]),
      Pollution_Index = html_text(cells[10]),
      Climate_Index = html_text(cells[11]),
      Year = year)
  })
  
  Filter(Negate(is.null), data)
}

years <- 2014:2021
all_data <- do.call(rbind, lapply(years, function(year) {
  year_data <- extract_year_data(year)
  do.call(rbind, lapply(year_data, as.data.frame, stringsAsFactors = FALSE))
}))


#2
countries <- c("Russia", "Germany", "Sweden", "France", "Finland")
filtered_data <- subset(all_data, Country %in% countries)


#3
russia_data <- subset(filtered_data, Country == "Russia")
germany_data <- subset(filtered_data, Country == "Germany")
sweden_data <- subset(filtered_data, Country == "Sweden") 
france_data <- subset(filtered_data, Country == "France")
finland_data <- subset(filtered_data, Country == "Finland")


convert_impute_round <- function(df) {
  numeric_cols <- c(
    "Quality_of_Life_Index", "Purchasing_Power_Index", "Safety_Index",
    "Health_Care_Index", "Cost_of_Living_Index", "Property_Price_to_Income_Ratio",
    "Traffic_Commute_Time_Index", "Pollution_Index", "Climate_Index")
  
  df[numeric_cols] <- lapply(df[numeric_cols], as.numeric)
  
  for(col in numeric_cols) {
    mean_val <- mean(df[[col]], na.rm = TRUE)
    if(!is.na(mean_val)) {
      df[[col]][is.na(df[[col]])] <- round(mean_val, 1)
    }
    df[[col]] <- round(df[[col]], 1)
  }
  
  return(df)
}

russia_data <- convert_impute_round(russia_data)
germany_data <- convert_impute_round(germany_data)
sweden_data <- convert_impute_round(sweden_data)
france_data <- convert_impute_round(france_data)
finland_data <- convert_impute_round(finland_data)


plot(
  NA, 
  xlim = c(2014, 2021), 
  ylim = c(0, 200), 
  xlab = "Год", 
  ylab = "Индекс качества жизни", 
  main = "Динамика Quality of Life Index (2014–2021)")

lines(russia_data$Year, russia_data$Quality_of_Life_Index, col = "red", type = "b", pch = 16)
lines(germany_data$Year, germany_data$Quality_of_Life_Index, col = "blue", type = "b", pch = 16)
lines(sweden_data$Year, sweden_data$Quality_of_Life_Index, col = "green", type = "b", pch = 16)
lines(france_data$Year, france_data$Quality_of_Life_Index, col = "purple", type = "b", pch = 16)
lines(finland_data$Year, finland_data$Quality_of_Life_Index, col = "orange", type = "b", pch = 16)

legend(
  "bottomright", 
  legend = c("Россия", "Германия", "Швеция", "Франция", "Финляндия"),
  col = c("red", "blue", "green", "purple", "orange"),
  lty = 1,
  pch = 16,
  y.intersp = 0.8)


indicators <- c(
  "Quality_of_Life_Index",
  "Purchasing_Power_Index", 
  "Safety_Index",
  "Health_Care_Index",
  "Cost_of_Living_Index",
  "Property_Price_to_Income_Ratio",
  "Traffic_Commute_Time_Index",
  "Pollution_Index",
  "Climate_Index"
)

country_colors <- c("Russia" = "#E41A1C",
                    "Germany" = "#377EB8",
                    "Sweden" = "#4DAF4A",
                    "France" = "#984EA3",
                    "Finland" = "#FF7F00")

plot_indicator <- function(indicator) {
  y_range <- range(
    russia_data[[indicator]],
    germany_data[[indicator]],
    sweden_data[[indicator]],
    france_data[[indicator]],
    finland_data[[indicator]]
  )
  
  xlim <- c(2014, 2023) 
  
  plot(NA, xlim = xlim, 
       ylim = y_range,
       xlab = "Год", 
       ylab = indicator,
       main = paste("Динамика:", indicator),
       xaxt = "n")
  axis(1, at = 2014:2021)
  
  lines(russia_data$Year, russia_data[[indicator]], 
        col = country_colors["Russia"], lwd = 2, type = "o", pch = 19)
  lines(germany_data$Year, germany_data[[indicator]], 
        col = country_colors["Germany"], lwd = 2, type = "o", pch = 19)
  lines(sweden_data$Year, sweden_data[[indicator]], 
        col = country_colors["Sweden"], lwd = 2, type = "o", pch = 19)
  lines(france_data$Year, france_data[[indicator]], 
        col = country_colors["France"], lwd = 2, type = "o", pch = 19)
  lines(finland_data$Year, finland_data[[indicator]], 
        col = country_colors["Finland"], lwd = 2, type = "o", pch = 19)
  
  legend(x = 2021.3, y = mean(y_range), 
         legend = names(country_colors),
         col = country_colors,
         lwd = 2, pch = 19, 
         cex = 0.8,
         bty = "o")
}

for (indicator in indicators) {
  plot_indicator(indicator)
}

#4
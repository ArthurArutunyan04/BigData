#1
dataset <- read.csv("/home/artur/Documents/R/summer.csv")

korea_archery <- dataset[
  (grepl("archery", dataset$Sport, ignore.case = TRUE) | 
     grepl("archery", dataset$Discipline, ignore.case = TRUE)) & 
    grepl("^KOR", dataset$Country),
]

#2
places <- table(korea_archery$Medal)
places_1_to_8 <- places[as.character(1:8)]
places_1_to_8[is.na(places_1_to_8)] <- 0  
names(places_1_to_8) <- paste("Место", 1:8)

cat("Результаты Южной Кореи в стрельбе из лука:\n\n")
cat("Занятые места (1-8):\n")
places_1_to_8


#2.1 Столбчатая диаграммма 
medal_table <- table(korea_archery$Year, korea_archery$Medal)
medal_table <- medal_table[, c("Gold", "Silver", "Bronze")] 

colors <- c("#FFD700", "#D3D3D3", "#FFB90F")

barplot(t(medal_table), 
        beside = TRUE,
        col = colors,
        main = "Медали Южной Кореи в стрельбе из лука",
        xlab = "Год Олимпиады",
        ylab = "Количество медалей")


#2.2 Круговая диаграмма
gold_medals <- korea_archery[korea_archery$Medal == "Gold", ]
gold_by_year <- table(gold_medals$Year)
year_city <- unique(korea_archery[korea_archery$Medal == "Gold", c("Year", "City")])
year_city <- year_city[order(year_city$Year), ]

pie(gold_by_year,
    main = "Распределение золотых медалей Южной Кореи\nв стрельбе из лука",
    col = rainbow(length(gold_by_year)),
    labels = gold_by_year,  
    cex = 0.8,  
    radius = 2)

legend_labels <- paste(year_city$Year, year_city$City)

legend("topright", 
       legend = legend_labels,  
       fill = rainbow(length(gold_by_year)),
       title = "Год - Город проведения",
       cex = 0.7,
       bty = "o", 
       box.lwd = 1, 
       x.intersp = 0.2,  
       y.intersp = 0.6,  
       text.width = max(strwidth(legend_labels)))


#2.3 График тенденций по полу
recent_data <- korea_archery[korea_archery$Year >= 1995, ]

male_data <- recent_data[recent_data$Gender == "Men", ]
female_data <- recent_data[recent_data$Gender == "Women", ]

count_medals_by_year <- function(data) {
  years <- unique(recent_data$Year)
  counts <- sapply(years, function(y) sum(data$Year == y))
  names(counts) <- years
  return(counts)
}

male_counts <- count_medals_by_year(male_data)
female_counts <- count_medals_by_year(female_data)

years <- as.numeric(names(male_counts))
x_range <- range(years)
y_range <- c(0, max(c(male_counts, female_counts)) + 1)
  
plot(years, male_counts, type = "o", col = "blue", 
     main = "Тенденции призовых мест Южной Кореи\nв стрельбе из лука (1995-2025)",
     xlab = "Год", ylab = "Количество призовых мест",
     ylim = y_range,
     xlim = x_range,
     xaxt = "n",
     pch = 19,  
     lwd = 2)  

axis(1, at = years, labels = years)

lines(years, female_counts, type = "o", col = "red", pch = 19, lwd = 2)

legend("bottomleft", 
       legend = c("Мужчины", "Женщины"), 
       col = c("blue", "red"), 
       lty = 1, 
       pch = 19,
       lwd = 2,
       bty = "n",  
       x.intersp = 0.5,    
       y.intersp = 0.5,     
       text.width = 1,
       cex = 0.5,           
       inset = c(0.02, 0.02), 
)


#3

last_6_olympics <- tail(sort(unique(dataset$Year)), 6)

recent_data <- dataset[dataset$Year %in% last_6_olympics, ]


#3.1 Золотые медали по всем странам
gold_medals <- recent_data[recent_data$Medal == "Gold", ]
top_countries_gold <- names(sort(table(gold_medals$Country), decreasing = TRUE))[1:7]

gold_summary <- aggregate(Medal ~ Year + Country, 
                          data = gold_medals[gold_medals$Country %in% top_countries_gold, ], 
                          FUN = length)

max_y <- max(gold_summary$Medal)
max_y <- ceiling(max_y/5)*5

x_padding <- diff(range(last_6_olympics)) * 0.25
plot(NULL, 
     xlim = c(min(last_6_olympics), max(last_6_olympics) + x_padding),
     ylim = c(0, max_y),
     main = "Золотые медали по всем видам спорта (последние 6 Олимпиад)",
     xlab = "Год Олимпиады",
     ylab = "Количество золотых медалей",
     xaxt = "n",
     yaxt = "n")

axis(1, at = last_6_olympics, labels = last_6_olympics)
axis(2, at = seq(0, max_y, by = 5), las = 1)  


colors <- rainbow(length(top_countries_gold))
for (i in 1:length(top_countries_gold)) {
  country <- top_countries_gold[i]
  country_data <- gold_summary[gold_summary$Country == country, ]
  lines(country_data$Year, country_data$Medal, 
        type = "o", 
        col = colors[i], 
        pch = 16, 
        lwd = 2)
}

legend("topright",
       legend = c("Страны",top_countries_gold), 
       col = c(NA, colors),
       lty = 1, 
       pch = 16,
       cex = 0.8,
       x.intersp = 0.2,    
       y.intersp = 0.25,  
       bty = "o")


# 3.2 Призовые места (1-3) 
prize_medals <- recent_data[recent_data$Medal %in% c("Gold", "Silver", "Bronze"), ]

top_countries_prize <- names(sort(table(prize_medals$Country), decreasing = TRUE))[1:7]

prize_summary <- aggregate(Medal ~ Year + Country, 
                           data = prize_medals[prize_medals$Country %in% top_countries_prize, ], 
                           FUN = length)

max_y <- ceiling(max(prize_summary$Medal)/50)*50

x_padding <- diff(range(last_6_olympics)) * 0.3
plot(NULL, 
     xlim = c(min(last_6_olympics), max(last_6_olympics) + x_padding),
     ylim = c(0, max_y),
     main = "Призовые места (1-3) по всем видам спорта (последние 6 Олимпиад)",
     xlab = "Год Олимпиады",
     ylab = "Количество призовых мест",
     xaxt = "n",
     yaxt = "n")

axis(1, at = last_6_olympics, labels = last_6_olympics)
axis(2, at = seq(0, max_y, by = 50), las = 1) 

colors <- rainbow(length(top_countries_prize))
for (i in 1:length(top_countries_prize)) {
  country <- top_countries_prize[i]
  country_data <- prize_summary[prize_summary$Country == country, ]
  lines(country_data$Year, country_data$Medal, 
        type = "o", 
        col = colors[i], 
        pch = 17,  
        lwd = 2)
}

legend("topright",
       legend = c("Страны", top_countries_prize), 
       col = c(NA, colors),
       lty = 1,
       pch = 17,
       x.intersp = 0.2,
       y.intersp = 0.2,
       bty = "o")


#4
archery_data <- dataset[
  (grepl("archery", dataset$Sport, ignore.case = TRUE) | 
     grepl("archery", dataset$Discipline, ignore.case = TRUE)) & 
    dataset$Year %in% last_6_olympics,
]

count_medals_by_year_gender <- function(data) {
  years <- sort(unique(data$Year))
  counts <- sapply(years, function(y) sum(data$Year == y))
  names(counts) <- years
  return(counts)
}

male_data <- archery_data[archery_data$Gender == "Men", ]
female_data <- archery_data[archery_data$Gender == "Women", ]

male_counts <- count_medals_by_year_gender(male_data)
female_counts <- count_medals_by_year_gender(female_data)

male_counts
female_counts


#4.1
years <- as.numeric(names(male_counts))
x_range <- range(years)
y_range <- c(0, max(c(male_counts, female_counts), na.rm = TRUE) + 1)

plot(years, male_counts, type = "o", col = "blue", 
     main = "Медали в стрельбе из лука за последние 6 Олимпиад\nпо полу",
     xlab = "Год Олимпиады", 
     ylab = "Количество медалей",
     ylim = y_range,
     xlim = x_range,
     xaxt = "n",
     pch = 19,  
     lwd = 2)

axis(1, at = years, labels = years)

lines(years, female_counts, type = "o", col = "red", pch = 19, lwd = 2)

legend("topright", 
       legend = c("Мужчины и Женщины"), 
       col = c("red"), 
       lty = 1, 
       pch = 19,
       lwd = 2,
       bty = "n",
       cex = 0.8,
       x.intersp = 0.5,
       y.intersp = 0.5)


#4.2 Столбчатая диаграмма
medal_matrix <- rbind(male_counts, female_counts)
rownames(medal_matrix) <- c("Мужчины", "Женщины")

colors <- c("blue", "red")


par(mar = c(5, 4, 4, 6) + 0.1)
barplot(medal_matrix, 
        beside = TRUE,
        col = colors,
        main = "Медали в стрельбе из лука за последние 6 Олимпиад\nпо полу",
        xlab = "Год Олимпиады",
        ylab = "Количество медалей",
        names.arg = last_6_olympics,
        ylim = c(0, max(medal_matrix, na.rm = TRUE) + 1))

legend("topright",
       legend = c("Мужчины", "Женщины"),
       fill = colors,
       bty = "o",
       cex = 0.8,
       x.intersp = 0.2,
       y.intersp = 0.6,
       xpd = TRUE,  
       inset = c(-0.25, 0))


#4.3 Круговая диаграмма
gender_counts <- c(sum(male_counts), sum(female_counts))
names(gender_counts) <- c("Мужчины", "Женщины")

colors <- c("blue", "red")

pie(gender_counts,
    labels = gender_counts,
    col = colors,
    radius = 1,
    main = "Распределение медалей в стрельбе из лука\nпо полу (последние 6 Олимпиад)")

legend("topright", 
       legend = names(gender_counts),
       fill = colors,
       title = "Пол",
       x.intersp = 0.3,
       cex = 0.8)


write.csv(korea_archery, file = "/home/artur/Documents/R/Lab3/korean_archery.csv", row.names = FALSE)

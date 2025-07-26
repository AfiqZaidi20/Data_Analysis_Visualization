library(readxl)
library(dplyr)
library(ggplot2)

dfhealthcare <- read.csv("HealthCareData2024.csv", stringsAsFactors = FALSE)

str(df)
summary(df)
head(df)

colSums(is.na(dfs))
df <- na.omit(df)

df$column_name[is.na(df$column_name)] <- mean(df$column_name, na.rm = TRUE)  # Replace with mean
df$column_name[is.na(df$column_name)] <- "Unknown"  # Replace with a placeholder

df <- df[!duplicated(df), ]



library(funModeling)
df_status(dfs)

df_status(dfhealthcare)

df <- na.omit(df)

dfs$County.Parish <- NULL

dfs <- na.omit(dfs)
colSums(is.na(dfs))


df[is.na(df$A), "A"] <- mean(df$A, na.rm = TRUE)

#untuk tukar yes no klau no 1,2
dfs <- dfs %>% mutate(Patients.who.reported.that.staff.did.not.give.care.in.a.professional.way.or.the.facility.was.not.clean = ifelse(Patients.who.reported.that.staff.did.not.give.care.in.a.professional.way.or.the.facility.was.not.clean == 1, 'Yes', 'No'))


 dfhealthcare <- dfhealthcare %>% sample_n(250)
 
 colSums(is.na(dfhealthcare))

 
 
 ggplot(dfhealthcare, aes(x = NetworkEventType, y = TransactionsPerSession)) +
   geom_boxplot(fill = "purple") +
   labs(title = "Transactions per Session by Network Event Type", x = "Network Event Type", y = "Transactions Per Session")

 ggplot(dfhealthcare, aes(x = DataTransferVolume_IN, y = DataTransferVolume_OUT)) +
   geom_point(color = "blue") +
   labs(title = "Inbound vs Outbound Data Transfer", x = "Data Transfer IN", y = "Data Transfer OUT")

 
 library(ggplot2)
 ggplot(dfhealthcare, aes(x = DataTransferVolume_OUT)) +
   geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
   labs(title = "Distribution of Outbound Data Transfer", x = "Data Transfer Volume (OUT)", y = "Count")
 
 library(reshape2)
 cor_matrix <- cor(dfhealthcare[, c("SystemAccessRate", "NetworkAccessFrequency", "UserActivityLevel")], use = "complete.obs")
 melted_cor <- melt(cor_matrix)
 
 
 ggplot(dfhealthcare, aes(x = SystemAccessRate)) +
   geom_bar(fill = "red", color = "black") +
   labs(title = "Count of Each Alert Category", x = "Alert Category", y = "Count")
 
 
 ggplot(dfhealthcare, aes(y = UserActivityLevel)) +
   geom_boxplot(fill = "green") +
   labs(title = "Box Plot of User Activity Level", y = "User Activity Level")
 
 

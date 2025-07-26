library(readxl)
library(dplyr)
library(ggplot2)
library(funModeling)
library(scales)

df <- read.csv("HealthCareData2024.csv", stringsAsFactors = FALSE)
df_status(df)

df <- df %>% sample_n(250)

df_clean <- df %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) %>%  # Replace NAs with 0
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), 0, .)))  # Replace Inf with 0

print(df_clean)
df_status(df)


sesnion_sort <- df %>% count(SessionIntegrityCheck, sort = TRUE)
resource_sort<- df %>% count(ResourceUtilizationFlag, sort = TRUE)
net_inter_sort<- clean_df %>% count(NetworkInteractionType, sort = TRUE)
sys_sort <- df %>% count(SystemAccessRate, sort = TRUE)

# Count occurrences of AlertCategory and NetworkInteractionType together
df_count <- df %>%
  count(AlertCategory, NetworkInteractionType) %>%
  rename(Count = n)  # Rename column to "Count"

# View the result
print(df_count)

clean_df <-na.omit(df)

clean_df$NetworkEventType <- gsub("_", "", clean_df$NetworkEventType)

df_status(clean_df)


#univariate
#1
# Calculate percentage distribution
df_percentage <- clean_df %>%
  count(NetworkInteractionType) %>%
  mutate(Percentage = (n / sum(n)) * 100)

# Enhanced Pie Chart
ggplot(df_percentage, aes(x = "", y = Percentage, fill = NetworkInteractionType)) +
  geom_bar(stat = "identity", width = 1, color = "white") +  # Add white borders for clarity
  coord_polar(theta = "y", start = 0) +  # Start from top
  labs(title = "Distribution of Network Interaction Types", fill = "Network Interaction Type") +
  theme_void() +  # Removes grid and axes for a clean pie chart
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5), size = 4, fontface = "bold", color = "white") +  # Bigger text
  scale_fill_manual(values = c("#E74C3C", "#1ABC9C", "#F1C40F", "#3498DB")) +  # Custom colors
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Center title
    legend.position = "right",  # Place legend on right side
    legend.text = element_text(size = 12)  # Increase legend text size
  )

#2
# Create color palette
custom_colors <- colorRampPalette(c("#2E86C1", "#3498DB", "#5DADE2", "#85C1E9"))(4)
# Create the enhanced plot
ggplot(clean_df, aes(x = reorder(NetworkEventType, -table(NetworkEventType)[NetworkEventType]))) +
  geom_bar(aes(fill = after_stat(count)), color = "white", width = 0.7) +  # Updated this line
  scale_fill_gradientn(colors = custom_colors) +
  labs(
    title = "Distribution of Network Event Types",
    subtitle = "Overview of Network Events Frequency",
    x = "Event Type",
    y = "Frequency",
    fill = "Count"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 12, margin = margin(b = 20)),
    axis.title.x = element_text(margin = margin(t = 10), face = "bold"),
    axis.title.y = element_text(margin = margin(r = 10), face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.text = element_text(color = "gray40"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    legend.position = "right",
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  ) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(expand=c(0,0))

#bivariate
#1
ggplot(clean_df, aes(x = NetworkEventType, fill = SessionIntegrityCheck)) +
  geom_bar(position = "fill", color = "black") +  # Adds black borders for clarity
  labs(title = "Session Integrity Check Distribution by Network Event Types", 
       x = "Network Event", y = "Proportion", fill = "Session Integrity") +
  scale_fill_manual(values = c("FALSE" = "red", "TRUE" = "lightgreen")) +  # Improved color contrast
  theme_bw() +  # Cleaner theme
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Centered title
    axis.text.x = element_text(angle = 20, vjust = 1, hjust = 1, size = 12),  # Rotate x-axis labels
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "top",  # Move legend to top
    legend.text = element_text(size = 12)
  ) +
  geom_text(stat = "count", aes(label = ..count..), position = position_fill(vjust = 0.5), size = 5, color = "black")  # Add data labels

#2
# Create a custom color palette
custom_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b")
# Create enhanced plot
ggplot(clean_df, aes(x = AlertCategory, fill = NetworkInteractionType)) +
  geom_bar(position = "fill", width = 0.7, color = "white") +
  scale_fill_manual(values = custom_colors) +
  labs(
    title = "Distribution of Network Interaction Types",
    subtitle = "Breakdown by Alert Category",
    x = "Alert Category",
    y = "Proportion",
    fill = "Network Interaction"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 10)),
    plot.subtitle = element_text(color = "gray40", size = 12, margin = margin(b = 20)),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  ) +
  scale_y_continuous(labels = scales::percent) # Convert to percentages

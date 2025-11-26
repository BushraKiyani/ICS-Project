library(ggplot2)
library(ggpubr)
library(dplyr)
library(gridExtra)
library(rstatix)

# Open the file
babies_data <- read.csv("babies.csv")
# Selecting only the "id", wt" and "smoke" columns from the dataset
babies_data <- babies_data[, c("id", "wt", "smoke")]
head(babies_data, 5)

# Check the data types of columns in the dataset
str(babies_data)

# check duplicate values based on the 'id' column
duplicates <- babies_data[duplicated(babies_data$id) | duplicated(babies_data$id, fromLast = TRUE),]
count <- length(unique(duplicates$id))
count

sort(unique(duplicates$id))

# Filter the data to keep unique 'id' values where 'wt' is not NA
babies_data <- babies_data %>%
  group_by(id) %>%
  filter(!is.na(wt)) %>%
  distinct(id, .keep_all = TRUE)

filtered_rows <- subset(babies_data, smoke == 9)

# Display the results
filtered_rows


# Remove unknown values
babies_data_updated <- babies_data %>%
  filter(smoke != 9)

# Ordering the data based on categories in smoke
babies_data_updated <- babies_data_updated[order(babies_data_updated$smoke),]
# Checking total number of observations in each group the data based on categories
babies_data_updated %>% group_by(smoke) %>%tally()

# Check the number of NA values in the 'wt' column
sum(is.na(babies_data_updated$wt))
any(babies_data_updated$wt == 999)

# Summary wt
wt_summary <- babies_data_updated %>%
  group_by(smoke) %>%
  summarize(mean_wt = mean(wt),
            median_wt = median(wt),
            sd_wt = sd(wt),
            min_wt = min(wt),
            max_wt = max(wt),
            var_wt = var(wt))

# Display summary statistics for the 'wt' variable
wt_summary

# Frequency distribution of weight
# Create a histogram for the 'wt' variable, grouped by the 'smoke' variable
plot_freq <- ggplot(data = babies_data_updated, aes(x = wt, fill = as.factor(smoke))) +
  geom_histogram(col = "black", position = "identity", alpha = 0.9, bins = 50) +
  scale_fill_manual(values = c("#0072B2", "#E69F00", "#56B4E9", "#D55E00",
                               "#CC79A7"),
                    name = "Smoke Category",
                    labels = c("never", "smokes now", "until current pregnancy", "once did, not now")) +
  labs(title = "Frequency Distribution of Babies' Weight by Maternal Smoke Category",
       x = "Babies' Weight (ounces)",
       y = "Frequency") +
  theme_minimal()

plot_freq2 <- plot_freq + theme(plot.title = element_text(face = "bold"),
                                axis.text = element_text(size = 11),
                                axis.title = element_text(size = 12),
                                legend.text = element_text(size = 11),
                                legend.title = element_text(size = 12))
ggsave("freq.pdf", plot = plot_freq2, width = 8.5, height = 4, units = "in")
plot_freq2

# Verifying the Assumptions

## 1. Homogeneity of variance assumption

# Box plot to compare finishing time in the different categories to find the homogeneity in variance
box_plot <- ggboxplot(babies_data_updated, x = "smoke", y = "wt", color = "black", fill = "smoke",
                      ylab = "Babies' Weight (ounces)", xlab = "Smoking Status") +
  labs(title = "Babies' Weights Distribution by Smoking Categories") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        legend.position = "none") +
  scale_x_discrete(name = "Smoke Categories",
                   labels = c("never", "smokes now", "until current pregnancy", "once did, not now"))

ggsave("box_plot2.pdf", plot = box_plot, width = 8.5, height = 4, units = "in")
box_plot

# 2. Normality assumption
Never <- babies_data_updated %>% filter(smoke == 0)
Smokes_now <- babies_data_updated %>% filter(smoke == 1)
Until_cur_pregnancy <- babies_data_updated %>% filter(smoke == 2)
Once_did <- babies_data_updated %>% filter(smoke == 3)

plot4 <- ggplot(Never) + stat_qq(aes(sample = wt), color= "green")+
  stat_qq_line(aes(sample = wt)) +  scale_x_continuous(name = "Theoretical Quantiles") +
  scale_y_continuous(name = "Sample Quantiles") + ggtitle("a) never") + theme_minimal()
plot4 <- plot4 + theme(panel.background = element_rect(fill = "White", color = "black"), 
                       plot.title = element_text(face = "bold",hjust = 0.5, size = 12),
                       axis.text=element_text(size=10),
                       axis.title=element_text(size=12), legend.text = element_text(size = 12))


plot5 <- ggplot(Smokes_now) + stat_qq(aes(sample = wt), color= "red")+
  stat_qq_line(aes(sample = wt)) +  scale_x_continuous(name = "Theoretical Quantiles") +
  scale_y_continuous(name = "Sample Quantiles") + ggtitle("b) smokes now")  + theme_minimal()
plot5 <- plot5 + theme(panel.background = element_rect(fill = "White", color = "black"),
                       plot.title = element_text(face = "bold",hjust = 0.5, size = 12),
                       axis.text=element_text(size=10), axis.title=element_text(size=12),
                       legend.text = element_text(size = 12))

plot6 <- ggplot(Until_cur_pregnancy) + stat_qq(aes(sample = wt), color= "darkgreen")+
  stat_qq_line(aes(sample = wt)) +  scale_x_continuous(name = "Theoretical Quantiles") +
  scale_y_continuous(name = "Sample Quantiles") + ggtitle("c) until current pregnancy") + theme_minimal()
plot6 <- plot6 +  theme(panel.background = element_rect(fill = "White", color = "black"),
                        plot.title = element_text(face = "bold",hjust = 0.5, size = 12),
                        axis.text=element_text(size=10), axis.title=element_text(size=12),
                        legend.text = element_text(size = 12))

plot7 <- ggplot(Once_did) + stat_qq(aes(sample = wt), color= "steelblue")+
  stat_qq_line(aes(sample = wt)) +  scale_x_continuous(name = "Theoretical Quantiles") +
  scale_y_continuous(name = "Sample Quantiles") + ggtitle("d) once did, not now") + theme_minimal()
plot7 <- plot7 +  theme(panel.background = element_rect(fill = "White", color = "black"),
                        plot.title = element_text(face = "bold",hjust = 0.5, size = 12),
                        axis.text=element_text(size=10), axis.title=element_text(size=12),
                        legend.text = element_text(size = 12))

final_plot1 <- grid.arrange(plot4, plot5, plot6, plot7, ncol=2, nrow = 2)
ggsave("QQplots.pdf",plot = final_plot1, width = 8.5, height = 4.5, units = "in")
final_plot1

# Task 2
# Do the babies birth weights differ between the categories? Conduct a global test.

# Convert 'wt' to numeric 
babies_data_updated$wt <- as.numeric(babies_data_updated$wt)

# Convert 'smoke' to factor 
babies_data_updated$smoke <- as.factor(babies_data_updated$smoke)

# Perform one-way ANOVA test
anova_result <- aov(wt ~ smoke, data = babies_data_updated)
summary(anova_result)

# Extract p-value from ANOVA result
anova_p_value <- summary(anova_result)[[1]]["smoke", "Pr(>F)"]

# Check if the p-value is less than the significance level (0.05)
if (anova_p_value < 0.05) {
  cat("There is a significant difference in babies' birth weights between the smoking categories.\n")
} else {
  cat("There is no significant difference in babies' birth weights between the smoking categories.\n")
}



# Task 3
# Multiple T-Tests


#0=never", "1=smokes now", "2=until current pregnancy", "3=once did, not now", "9=unknown"
# List of pairs made of the 5 Categories
pair_category  <- c("never_smokesNow","never_untilCurrentPregnancy","never_onceDidNotNow",
                    "smokesNow_untilCurrentPregnancy","smokesNow_onceDidNotNow",
                    "untilCurrentPregnancy_onceDidNotNow")
                    
# Filtering data for pairwise t-test

never_smokesNow  <- babies_data_updated  %>% filter(smoke %in% c(0,1)) 
never_untilCurrentPregnancy  <- babies_data_updated  %>% filter(smoke %in% c(0,2))  
never_onceDidNotNow  <- babies_data_updated  %>% filter(smoke %in% c(0,2))  
smokesNow_untilCurrentPregnancy  <- babies_data_updated  %>% filter(smoke %in% c(1,2))  
smokesNow_onceDidNotNow <- babies_data_updated  %>% filter(smoke %in% c(1,3))  
untilCurrentPregnancy_onceDidNotNow  <- babies_data_updated  %>% filter(smoke %in% c(2,3)) 

#t-tests
test_1  <- t.test(wt ~ factor(smoke), data = never_smokesNow, var.equal = TRUE)
test_2  <- t.test(wt ~ factor(smoke), data = never_untilCurrentPregnancy, var.equal = TRUE)
test_3  <- t.test(wt ~ factor(smoke), data = never_onceDidNotNow, var.equal = TRUE)
test_4  <- t.test(wt ~ factor(smoke), data = smokesNow_untilCurrentPregnancy, var.equal = TRUE)
test_5  <- t.test(wt ~ factor(smoke), data = smokesNow_onceDidNotNow, var.equal = TRUE)
test_6  <- t.test(wt ~ factor(smoke), data = untilCurrentPregnancy_onceDidNotNow, var.equal = TRUE)

#p-values from the t-tests
p_values  <- c(test_1$p.value,test_2$p.value,test_3$p.value,test_4$p.value,test_5$p.value,
               test_6$p.value)
p_values

#Tabulating the P-value
df1 <- data.frame(data.frame(pair_category),data.frame(p_values))
names(df1)[1] <- "Categories pair"
names(df1)[2] <- "p-values"

df1["Reject Yes/No"] <- with(df1, ifelse(df1$`p-values` < 0.05, "Yes", "No"))
df1

# Multiple Tests Adjustment Method: Bonferroni’s Correction

#Adjusting method: Bonferroni
p_values_bonferroni <- p.adjust(p = p_values, method = "bonferroni", n = 6)

#Tabulating the P-value after bonferroni correction method
df2 <- data.frame(data.frame(pair_category),data.frame(p_values_bonferroni))
names(df2)[1] <- "Categories pair"
names(df2)[2] <- "Adjusted p-values"
df2["Reject Yes/No"] <- with(df2, ifelse(df2$`Adjusted p-values` < 0.05, "Yes", "No"))
df2

# Tukey-Kramer test
#TukeyHSD() function adjust for unequal sizes as well (Performs Tukey-Kramer when sizes are unequal across the categories)

# Perform Tukey-Kramer test
tukey_hsd <- TukeyHSD(anova_result)
tukey_hsd

# Extract p-values and convert them into a data frame
upr <- tukey_hsd[[1]][, "upr"]
lwr <- tukey_hsd[[1]][, "lwr"]
diff <- tukey_hsd[[1]][, "diff"]
confi <- data.frame(
  Categories_pair = pair_category,
  diff = diff,
  lwr = lwr,
  upr = upr
)

confi

# Extract p-values and convert them into a data frame
p_val_t <- tukey_hsd[[1]][, "p adj"]
p_val_t_df <- data.frame(
  Categories_pair = pair_category,
  adj_p_value = p_val_t
)
# Add a column to indicate rejection or acceptance of the null hypothesis
p_val_t_df["Reject Yes/No"] <- with(p_val_t_df, ifelse(p_val_t_df$adj_p_value < 0.05, "Yes", "No"))

p_val_t_df
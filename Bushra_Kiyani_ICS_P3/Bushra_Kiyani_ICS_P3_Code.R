# Load the required packages
library(ggplot2)
library(GGally)
library(ggpubr)
library(dplyr)
library(reshape2)
library(RColorBrewer)
library(MASS)
library(car)

# load the dataset
input_data <- read.csv("Bikedata.csv")
head(input_data)

#Check  NA Values
colSums(is.na(input_data))

# Convert Seasons and Holiday to factor variables
input_data$Seasons <- as.factor(input_data$Seasons)
input_data$Holiday <- as.factor(input_data$Holiday)

# Summary
summary(input_data)

#Summarise all continuous variables
standard_deviations <- rbind.data.frame(lapply(input_data  %>% dplyr::
                                                 select(-c("Seasons","Holiday")),sd))
summary_table  <- rbind.data.frame(lapply(input_data  %>% dplyr::
                                            select(-c("Seasons","Holiday")),summary))
summary_table  <-  rbind(summary_table, standard_deviations)
row.names(summary_table) <- c("Min", "Q1", "Median", "Mean", "Q3", "Max", "SD")
summary_table  <- t(round(summary_table,3))
summary_table

#Histogram to see the frequency distribution of the variable log.Rented.Bike.Count 
FD = ggplot(input_data, aes(x=log.Rented.Bike.Count))+ 
  geom_histogram(color="black", fill="#0002AF", alpha = 0.4, bins = 25)+
  xlab("log.Rented.Bike.Count") + ylab("Frequency") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16 , face = "bold"))
ggsave("FD.pdf", plot = FD)
FD

#Correlation Heatmap
corr_mat <- round(cor(input_data  %>% dplyr::select(-c("Seasons","Holiday"))),2)
melted_corr_mat <- melt(corr_mat) %>% dplyr::filter(Var1 == "log.Rented.Bike.Count")
out  <- ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                           fill=value)) + geom_tile(aes(width = 1.5, height=1)) +
  xlab("Response") + ylab("Covariates") + guides(fill=guide_legend(title="")) +
  theme(legend.text = element_text(size=14),  axis.text=element_text(size=16),
        axis.title=element_text(size=16 , face = "bold"))+ coord_fixed(ratio = 2/4) +
  geom_text(aes(label = value),
            color = "white", size = 5)
ggsave("out.pdf", plot = out)
out

#Correlation scatter plot for log.Rented.Bike.Count vs all variables
input_corr <- input_data  %>% dplyr::select(-c("Seasons","Holiday")) %>% 
  tidyr::gather(key = "Covariates", value = "value",-log.Rented.Bike.Count)

corr_plot  <- ggplot(input_corr, aes(x = log.Rented.Bike.Count, y = value, col = Covariates)) +
  geom_point(size = 0.3) +
  facet_wrap(~Covariates, scales = "free") + 
  xlab("log count of bikes rented ") + ylab("Covariate")  + 
  theme(axis.text=element_text(size=10), 
        axis.title=element_text(size=12 , face = "bold"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10),
        legend.position = "none",
        legend.direction = "horizontal",
        strip.text = element_text(size = 12)  # Add this line
  )
ggsave("Cor_plot.pdf", plot = corr_plot, width = 8.5, height = 4, units = "in")
corr_plot


# Create a color palette
my_palette <- brewer.pal(4, "Dark2")

# Create box plots for log.Rented.Bike.Count by Seasons
sea = ggplot(input_data, aes(x = Seasons, y = log.Rented.Bike.Count, fill = Seasons)) +
  geom_boxplot() +
  scale_fill_manual(values = my_palette) +
  xlab("Seasons") +
  ylab("log.Rented.Bike.Count") +
  ggtitle("Distribution of log.Rented.Bike.Count by Seasons") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(size = 10),
        legend.position = "none")

# Create box plots for log.Rented.Bike.Count by Holiday
hol = ggplot(input_data, aes(x = Holiday, y = log.Rented.Bike.Count, fill = Holiday)) +
  geom_boxplot() +
  scale_fill_manual(values = my_palette[c(1, 3)]) +
  xlab("Holiday") +
  ylab("log.Rented.Bike.Count") +
  ggtitle("Distribution of log.Rented.Bike.Count by Holiday") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(size = 10),
        legend.position = "none")
ggsave("hol.pdf", plot = hol, width = 8.5, height = 4, units = "in")
ggsave("sea.pdf", plot = sea, width = 8.5, height = 4, units = "in")
sea 
hol


# Selecting "Winter" as reference category for Seasons
input_data$Seasons <- relevel(factor(input_data$Seasons), ref = "Winter")

# Fit the linear regression model
model <- lm(log.Rented.Bike.Count ~ ., data = input_data)

# Display the model summary
summary(model)

#Finding the Optimal set of Covariates using Backward Selection Method and AIC

# Apply stepAIC() with direction='backward'
AIC_model <- stepAIC(model, direction='backward')

# Print the summary of the reduced model
summary(AIC_model)


#Finding the Optimal set of Covariates using Backward Selection Method and BIC
BIC_model <- stepAIC(model, direction = "backward", k = log(nrow(input_data)))
summary(BIC_model)

# Compute 95% confidence intervals for the coefficients
CI <- confint(BIC_model, level = 0.95)
CI

# Print AIC of Models
print(paste("AIC of Full Model: ", AIC(model)))
print(paste("AIC of Backward Selection Model with AIC: ", AIC(AIC_model)))
print(paste("AIC of Backward Selection Model with BIC: ", AIC(BIC_model)))


# Print BIC of Models
print(paste("BIC of Full Model: ", BIC(model)))
print(paste("BIC of Backward Selection Model with AIC: ", BIC(AIC_model)))
print(paste("BIC of Backward Selection Model with BIC: ", BIC(BIC_model)))


#Residual plot for standardized residuals vs Fitted best subset model
res = ggplot(BIC_model, aes(x = .fitted, y = BIC_model$residuals)) +
  geom_point(size = 1.5, color = "#0002AF", alpha = 0.7) +
  geom_hline(yintercept = mean(BIC_model$residuals), color = "black", linewidth = 0.5) + 
  xlab("Fitted values") + ylab("Residuals") + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14 , face = "bold"))

ggsave("res_plot.pdf", plot = res)
res


# QQ plot
nn = ggplot(input_data, aes(sample = BIC_model$residuals)) +
  stat_qq(distribution = stats::qnorm, color = "#0002AF", alpha = 0.5) +
  stat_qq_line() +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  theme(plot.title = element_text(face = "bold",hjust = 0.5, size = 12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14, face = "bold"),
        legend.text = element_text(size = 12))

ggsave("nn_plot.pdf", plot = nn)
nn


# Calculate VIF for each predictor variable
vif_values <- vif(BIC_model)

# Print VIF values
vif_values

library('GGally')
library('dplyr')
library('ggplot2')
library('gridExtra')
library('tidyverse')

#Loading the data
census_data <- read.csv('census2002_2022.csv')[,-1]

#Changing Column Names for better readability
colnames(census_data) <- c("Country","Subregion","Region","Year",
                           "Life_exp_both","Life_exp_male","Life_exp_female",
                           "Mortality_rate_both", "Mortality_rate_male",
                           "Mortality_rate_female")

# Fill missing values in the "Region" and "Subregion" columns:

country_mapping <- tibble(
  Country = c("Curaçao", "Côte d'Ivoire"),
  Region = c("Americas", "Africa"),
  Subregion = c("South America", "Western Africa")
)

census_data <- census_data %>%
  left_join(country_mapping, by = "Country", suffix = c("", "_mapped")) %>%
  mutate(
    Region = coalesce(Region, Region_mapped),
    Subregion = coalesce(Subregion, Subregion_mapped)
  ) %>%
  select(-Region_mapped, -Subregion_mapped)

# Replace NaN values with the median
census_data <- census_data %>%
  mutate(across(c("Life_exp_both", "Life_exp_male", "Life_exp_female",
                  "Mortality_rate_both", "Mortality_rate_male",
                  "Mortality_rate_female"),
                ~ replace(., is.na(.), median(., na.rm = TRUE))))

#Ordering the Data according Region and Subregion
census_data <- census_data[order(census_data$Region,census_data$Subregion),]

#Factoring with the Sub regions
census_data$Subregion <- factor(census_data$Subregion,
                                levels = unique(census_data$Subregion[order(census_data$Region)]))

#Split the Data Based on the year
census_data_2022 <- census_data %>% filter(Year == 2022)
census_data_2002 <- census_data %>% filter(Year == 2002)

#Summary of data 2022
census_data_2022   %>% summary()

# Task 1: Frequency Distributions of Different Variables
custom_theme <- theme(plot.title = element_text(hjust = 0.5, size = 13, face="bold"), 
                      axis.text=element_text(size=12),
                      axis.title = element_text(size = 14),
                      axis.title.y=element_text(size=12), # Change the size of the y-axis title
                      legend.text=element_text(size=12),
                      legend.title = element_text(size=14))

binwidth <- 1
Life_exp_both  <- ggplot(census_data_2022, aes(x = Life_exp_both)) +
  geom_histogram(aes(fill = after_stat(count)), col = "black",
                 binwidth = binwidth, linewidth = 0.1) +
  scale_x_continuous(name = "Life expectancy of both sexes in years") +
  scale_y_continuous(name = "Frequency") +
  scale_fill_viridis_c() +
  ggtitle("c) Frequency Distribution of\nLife expectancy of both sexes") +
  custom_theme+
  theme(legend.position = c(0, 1), # Set legend position to upper right corner
        legend.justification = c(0, 1)) # Align legend to the upper right corner
binwidth <- 3

Mortality_rate_both <- ggplot(census_data_2022, aes(x = Mortality_rate_both)) +
  geom_histogram(aes(fill = after_stat(count)), binwidth = binwidth, color = "black",
                 linewidth = 0.1) +
  scale_x_continuous(name = "Mortality rate of both sexes in years") +
  scale_y_continuous(name = "Frequency") +
  scale_fill_viridis_c() +
  ggtitle("d) Frequency Distribution of\nMortality rate of both sexes") +
  custom_theme+
  theme(legend.position = c(1, 1), # Set legend position to upper right corner
        legend.justification = c(1, 1)) # Align legend to the upper right corner

# Create custom theme for plot
custom_theme <- theme(plot.title = element_text(hjust = 0.5, size = 13, face="bold"), 
                      axis.text=element_text(size=12),
                      axis.title = element_text(size = 14),
                      axis.title.y=element_text(size=12), # Change the size of the y-axis title
                      legend.text=element_text(size=12),
                      legend.title = element_text(size=14))
# Define custom color palette for male and female
custom_palette <- c("#E69F00", "#56B4E9")
# Create ggplot object
combined_life_plot <- ggplot() +
  geom_histogram(aes(x=census_data_2022$Life_exp_male, fill="Male"),
                 data=data.frame(census_data_2022$Life_exp_male),col = "black",
                 size = 0.1, alpha=0.7, binwidth=1) +
  geom_histogram(aes(x=census_data_2022$Life_exp_female, fill="Female"),
                 data=data.frame(census_data_2022$Life_exp_female), col = "black",
                 size = 0.1, alpha=0.7, binwidth=1) +
  labs(title="a) Frequency Distribution\nof life expectancy in female and male",
       x="Life expectancy in years", y="Frequency") +
  scale_fill_manual(values = custom_palette) +
  labs(fill = "Gender") +
  custom_theme +
  theme(legend.position = c(0, 1), # Set legend position to upper right corner
        legend.justification = c(0, 1), # Align legend to the upper right corner
        legend.title = element_text(size = 11), # Reduce legend title size
        legend.text = element_text(size = 10)) # Reduce legend text size

# Create custom theme for plot
custom_theme <- theme(plot.title = element_text(hjust = 0.5, size = 13, face="bold"), 
                      axis.text=element_text(size=12),
                      axis.title = element_text(size = 14),
                      axis.title.y=element_text(size=12), # Change the size of the y-axis title
                      legend.text=element_text(size=12),
                      legend.title = element_text(size=14))
# Define custom color palette for male and female
custom_palette <- c("#E69F00", "#56B4E9","darkgreen")
# Create ggplot object
combined_mortality_plot <- ggplot() +
  geom_histogram(aes(x=census_data_2022$Mortality_rate_male, fill="Male"),
                 data=data.frame(census_data_2022$Mortality_rate_male),col = "black",
                 size = 0.1, alpha=0.7, binwidth=3) +
  geom_histogram(aes(x=census_data_2022$Mortality_rate_female, fill="Female"),
                 data=data.frame(census_data_2022$Mortality_rate_female), col = "black",
                 size = 0.1, alpha=0.7, binwidth=3) +
  labs(title="b) Frequency Distribution\nof Mortality rate in female and male",
       x="Mortality rate in years", y="Frequency") +
  scale_fill_manual(values = custom_palette) +
  labs(fill = "Gender") +
  custom_theme +
  theme(legend.position = c(1, 1), # Set legend position to upper right corner
        legend.justification = c(1, 1), # Align legend to the upper right corner
        legend.title = element_text(size = 11), # Reduce legend title size
        legend.text = element_text(size = 10)) # Reduce legend text size

# Plot the ggplot object
#print(combined_mortality_plot)

# Combine the plots into a grid with adjusted heights
final_plot_all_n <- grid.arrange(combined_life_plot, combined_mortality_plot,
                                 Life_exp_both, Mortality_rate_both, ncol = 2, nrow = 2,
                                 widths = unit(c(4, 4), "in"), heights = unit(c(3.2,3.2), "in"))

# Save the final plot to a file with adjusted dimensions
ggsave("histograms_all_n.pdf", plot = final_plot_all_n, width = 8.5, height = 6.5, units = "in")
final_plot_all_n

# life expectancy by 'Region'
boxplot_life_expectancy <- ggplot(census_data_2022, aes(x = Region, y = Life_exp_both,
                                                        fill = Region)) +
  geom_boxplot() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face="bold"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  xlab("Region") +
  ylab("Life Expectancy (Years)") +
  ggtitle("Life Expectancy by Region") +
  scale_fill_brewer(palette="Dark2")

print(boxplot_life_expectancy)
ggsave("boxplots_life_expectancy_by_region.pdf", plot = boxplot_life_expectancy)

# Mortality rate by 'Region'
boxplot_mortality_rate <- ggplot(census_data_2022, aes(x = Region, y = Mortality_rate_both,
                                                       fill = Region)) +
  geom_boxplot() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face="bold"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  xlab("Region") +
  ylab("Under age 5 Mortality Rate") +
  ggtitle("Under age 5 Mortality Rate by Region") +
  scale_fill_brewer(palette="Dark2")

print(boxplot_mortality_rate)
ggsave("boxplots_mortality_by_region.pdf", plot = boxplot_mortality_rate)

install.packages("patchwork")
library(patchwork)
boxplot_life_expectancy <- ggplot(census_data_2022, aes(x = Region, y = Life_exp_both)) +
  geom_boxplot(aes(fill = Region), show.legend = FALSE) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face="bold"),
        axis.text=element_text(size=11),
        axis.title=element_text(size=14),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "lightgrey"))  +
  xlab("Region") +
  ylab("Life Expectancy (Years)") +
  ggtitle("a) Life Expectancy by Region") +
  scale_fill_brewer(palette="Set1", guide = FALSE)

boxplot_mortality_rate <- ggplot(census_data_2022, aes(x = Region, y = Mortality_rate_both)) +
  geom_boxplot(aes(fill = Region), show.legend = FALSE) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face="bold"),
        axis.text=element_text(size=11),
        axis.title=element_text(size=14),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "lightgrey")) +
  xlab("Region") +
  ylab("Under age 5 Mortality Rate") +
  ggtitle("b) Under age 5 Mortality Rate by Region") +
  scale_fill_brewer(palette="Set1")
combined_plot <- boxplot_life_expectancy + boxplot_mortality_rate
ggsave(filename = "combined_plot.pdf", plot = combined_plot, width = 8.5, height = 4, units = "in")
print(combined_plot)


# Task 2: Analysis of variability within and between subregions.

#Summary for box plot for life expectancy of both sexes in regions and subregions
census_data_2022 %>%                               # Summary by group using dplyr
  group_by(Region) %>% 
  dplyr::summarize(min = min(Life_exp_both),
                   q1 = quantile(Life_exp_both, 0.25),
                   median = median(Life_exp_both),
                   q3 = quantile(Life_exp_both, 0.75),
                   max = max(Life_exp_both))

census_data_2022 %>%                               # Summary by group using dplyr
  group_by(Region) %>% 
  dplyr::summarize(min = min(Mortality_rate_both),
                   q1 = quantile(Mortality_rate_both, 0.25),
                   median = median(Mortality_rate_both),
                   q3 = quantile(Mortality_rate_both, 0.75),
                   max = max(Mortality_rate_both))

census_data_2022 %>%                               # Summary by group using dplyr
  group_by(Subregion) %>% 
  dplyr::summarize(min = min(Life_exp_both),
                   q1 = quantile(Life_exp_both, 0.25),
                   median = median(Life_exp_both),
                   q3 = quantile(Life_exp_both, 0.75),
                   max = max(Life_exp_both))

census_data_2022 %>%                               # Summary by group using dplyr
  group_by(Subregion) %>% 
  dplyr::summarize(min = min(Mortality_rate_both),
                   q1 = quantile(Mortality_rate_both, 0.25),
                   median = median(Mortality_rate_both),
                   q3 = quantile(Mortality_rate_both, 0.75),
                   max = max(Mortality_rate_both))

# Filter data to only include the region named Europe
europe_data <- census_data_2022 %>%
  filter(Region == "Europe") %>%
  # Reshape data to have a single column for mortality rates and a new column to indicate gender
  gather(Variable, value, Mortality_rate_male, Mortality_rate_female, Mortality_rate_both)

# Create box plots for mortality_rate_male and mortality_rate_female by subregion
mortality_europe <- europe_data %>%
  ggplot(aes(x = Subregion, y = value, fill = Variable)) +
  geom_boxplot() +
  #scale_fill_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5, size = 13, face="bold"),
        axis.text=element_text(size=11),
        axis.title=element_text(size=13),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "lightgrey"))+
  xlab("Subregions") + ylab("Under age 5 Mortality Rate")

ggsave('mortality_EU.pdf', plot = mortality_europe, width = 8.4, height = 3, units = "in")

europe_data <- census_data_2022 %>%
  filter(Region == "Europe") %>%
  # Reshape data to have a single column for mortality rates and a new column to indicate gender
  gather(Variable, value, Life_exp_male, Life_exp_female, Life_exp_both)

# Create box plots for Life expectancy_male and Life expectancy female by subregion

life_europe <- europe_data %>%
  ggplot(aes(x = Subregion, y = value, fill = Variable)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5, size = 13, face="bold"),
        axis.text=element_text(size=11),
        axis.title=element_text(size=13),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "lightgrey")) +
  xlab("Subregions") + ylab("Life Expectancy")

ggsave('life_EU.pdf', plot = life_europe, width = 8.4 , height = 3, units = "in")

# Task 3: Bivariate correlations between the variables

#Pairplot
scat_plot  <- ggpairs(census_data_2022, columns = 5:10, 
                      upper = list(continuous = GGally::wrap(ggally_cor, stars = F)),
                      diag = list(continuous = wrap("barDiag", alpha = 0.8, color="grey")), 
                      lower = list(continuous = wrap("points", alpha = 0.8,size=0.4), 
                                   combo = wrap("dot", alpha = 0.8,size=0.2) ),
                      mapping=ggplot2::aes(colour = Region)) +
  theme(axis.text=element_text(size=9),
        axis.title=element_text(size=11)) 
ggsave("corr_plot.pdf",plot = scat_plot, width = 8.5,height = 8.5, units = "in")


# Task 4: comparison of 2002 with 2022

countries  <- census_data[which(is.na(census_data$Mortality_rate_both)),]$Country
census_data_2002  <- census_data_2002  %>% filter(!Country %in% countries)
census_data_2022  <- census_data_2022  %>% filter(!Country %in% countries)

scat_plot1 <- ggplot(data = NULL, aes(x = census_data_2002$Life_exp_both, 
                                      y = census_data_2022$Life_exp_both, 
                                      color = census_data_2002$Region)) + 
  geom_point(size = 2.5) +
  guides(colour = guide_legend(title = "Subregion", size = 16, override.aes = list(shape = 15))) +  
  geom_abline(intercept =0 , slope = 1)+ xlim(40,90)+ylim(40,90) +
  xlab("Life expectancy of both sexes in 2002") + ylab("Life expectancy of both sexes in 2022") + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face="bold"),
        legend.position = c(0.15, 0.83),legend.background = element_rect(fill = "transparent"), 
        legend.text = element_text(size = 10), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  ggtitle("a) Life expectancy 2002 vs 2022") +
  theme(plot.title = element_text(size = 14))

scat_plot2  <- ggplot(data = NULL, aes(x = census_data_2002$Mortality_rate_both, 
                                       y = census_data_2022$Mortality_rate_both, 
                                       color = census_data_2002$Region)) + geom_abline(intercept =0 , slope = 1) +
  xlim(0,150)+ylim(0,150) +
  geom_point(size = 2.5) +
  guides(colour = guide_legend(title = "Subregion", size = 16, override.aes = list(shape = 15))) +  
  xlab("Under age 5 mortality rate in 2002") + ylab("Under age 5 mortality rate in 2022") + 
  theme(plot.title = element_text(hjust = 0.5, size = 24, face="bold"),
        legend.position = c(0.15, 0.83),legend.background = element_rect(fill = "transparent"), 
        legend.text = element_text(size = 10), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  ggtitle("b) Mortality rate 2002 Vs 2022") +
  theme(plot.title = element_text(size = 14))

scat_plot1
scat_plot2

combined_plot2 <- scat_plot1 + scat_plot2 + plot_layout(ncol = 2)

ggsave("combined_plot2.pdf", combined_plot2, width = 8.4, height = 5, units = "in")

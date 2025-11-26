library('GGally')
library('dplyr')
library('ggplot2')
library('gridExtra')
#Loading the data
census_data <- read.csv('census2001_2021.csv')

#View the Data
head(census_data)

#Changing Column Names for better readability
colnames(census_data) <- c("Country","Subregion","Region","Year",
                           "Life_exp_both","Life_exp_male","Life_exp_female","Mortality_rate")

#Ordering the Data according Region and Subregion
census_data <- census_data[order(census_data$Region,census_data$Subregion),]

#Factoring with the Sub regions
census_data$Subregion <- factor(census_data$Subregion,
                                levels = unique(census_data$Subregion[order(census_data$Region)]))

head(census_data)

#Check is there any missing values in Data
colSums(is.na(census_data))

#Check the missing value records #Mortality rate
census_data[which(is.na(census_data$Mortality_rate)),]

#Split the Data Based on the year
census_data_2021 <- census_data %>% filter(Year == 2021)
census_data_2001 <- census_data %>% filter(Year == 2001)

#Summary of data
census_data_2021   %>% summary()

#Get the difference between the Life expectancy of female and male
census_data_2001$Life_exp_diff_btw_sexes <- census_data_2001$
  Life_exp_female - census_data_2001$Life_exp_male
census_data_2021$Life_exp_diff_btw_sexes <- census_data_2021$
  Life_exp_female - census_data_2021$Life_exp_male

# Histograms
plot1  <- ggplot(census_data_2021, aes(x = Life_exp_female)) +
  geom_histogram(aes(fill = ..count..), col = "black")+
  scale_x_continuous(name = "Life expectancy of female in years ") +
  scale_y_continuous(name = "Count") +
  ggtitle("a) Frequency of life expectancy of female") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face="bold"), 
        axis.text=element_text(size=9),
        axis.title=element_text(size=11))

plot2  <- ggplot(census_data_2021, aes(x = Life_exp_male)) +
  geom_histogram(aes(fill = ..count..), col = "black")+
  scale_x_continuous(name = "Life expectancy of male in years ") +
  scale_y_continuous(name = "Count") +
  ggtitle("b) Frequency of life expectancy of male") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face="bold"), 
        axis.text=element_text(size=9),
        axis.title=element_text(size=11))

plot3  <- ggplot(census_data_2021, aes(x = Life_exp_both)) +
  geom_histogram(aes(fill = ..count..), col = "black")+
  scale_x_continuous(name = "Life expectancy of both sexes") +
  scale_y_continuous(name = "Count") +
  ggtitle("c) Frequency of Life expectancy of both sexes") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face="bold"), 
        axis.text=element_text(size=9),
        axis.title=element_text(size=11))

plot4  <- ggplot(census_data_2021, aes(x = Mortality_rate)) +
  geom_histogram(aes(fill = ..count..), col = "black")+
  scale_x_continuous(name = "Infant mortality rate") +
  scale_y_continuous(name = "Count") +
  ggtitle("d) Frequency of infant mortality rate") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face="bold"), 
        axis.text=element_text(size=9),
        axis.title=element_text(size=11))



final_plot1 <- grid.arrange(plot1, plot2, plot3, plot4, ncol=2, nrow = 2)
ggsave("histograms.pdf",plot = final_plot1)
final_plot1
```
```{r}
plot5  <- ggplot(census_data_2021, aes(x = Life_exp_diff_btw_sexes)) +
  geom_histogram(aes(fill = ..count..), col = "black")+
  scale_x_continuous(name = "Life expectancy difference between sexes") +
  scale_y_continuous(name = "Count") +
  ggtitle("Frequency of the diference of life expectancy") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face="bold"), 
        axis.text=element_text(size=9),
        axis.title=element_text(size=11))

final_plot2 <- grid.arrange(plot5, ncol=1, nrow = 1)
ggsave("histogram-d.pdf",plot = final_plot2)
final_plot2

# Task 2: Bivariate correlations between the variables


#Pairplot
scat_plot  <- ggpairs(census_data_2021, columns = 5:8, 
                      upper = list(continuous = GGally::wrap(ggally_cor, stars = F)),
                      diag = list(continuous = wrap("barDiag", alpha = 0.8, color="grey")), 
                      lower = list(continuous = wrap("points", alpha = 0.8,size=0.4), 
                                   combo = wrap("dot", alpha = 0.8,size=0.2) ),
                      mapping=ggplot2::aes(colour = Region)) + 
  theme(axis.text=element_text(size=9),
        axis.title=element_text(size=11)) 
ggsave("corr_plot.pdf",plot = scat_plot)
scat_plot

# Task 3: Analysis of variability within and between subregions.

```{r}
#Summary for box plot for life expectancy of both sexes in regions and subregions
census_data_2021 %>%                               # Summary by group using dplyr
  group_by(Region) %>% 
  dplyr::summarize(min = min(Life_exp_both),
                   q1 = quantile(Life_exp_both, 0.25),
                   median = median(Life_exp_both),
                   q3 = quantile(Life_exp_both, 0.75),
                   max = max(Life_exp_both))

census_data_2021 %>%                               # Summary by group using dplyr
  group_by(Region) %>% 
  dplyr::summarize(min = min(Mortality_rate),
                   q1 = quantile(Mortality_rate, 0.25),
                   median = median(Mortality_rate),
                   q3 = quantile(Mortality_rate, 0.75),
                   max = max(Mortality_rate))

census_data_2021 %>%                               # Summary by group using dplyr
  group_by(Subregion) %>% 
  dplyr::summarize(min = min(Life_exp_both),
                   q1 = quantile(Life_exp_both, 0.25),
                   median = median(Life_exp_both),
                   q3 = quantile(Life_exp_both, 0.75),
                   max = max(Life_exp_both))

census_data_2021 %>%                               # Summary by group using dplyr
  group_by(Subregion) %>% 
  dplyr::summarize(min = min(Mortality_rate),
                   q1 = quantile(Mortality_rate, 0.25),
                   median = median(Mortality_rate),
                   q3 = quantile(Mortality_rate, 0.75),
                   max = max(Mortality_rate))

#Comparing the Life Expectancies of Male in Sub Regions
box_plot1 <- census_data_2021 %>%
  ggplot(aes(x=Subregion, y=Life_exp_both, fill=Region)) +
  geom_boxplot() +
  coord_flip()+ scale_fill_brewer(palette="Accent") +
  theme(legend.position="top", 
        axis.text = element_text(vjust = 0.5, size = 9),
        legend.text = element_text(size = 11), 
        axis.title=element_text(size=11)
  )+
  xlab("Sub regions") + ylab("Life expectancy of both genders") 
box_plot1
ggsave('Boxplot1.pdf', plot = box_plot1)


#Comparing the infant mortality rate in sub regions
box_plot2 <-  census_data_2021 %>%
  ggplot(aes(x=Subregion, y=Mortality_rate, fill=Region)) +
  geom_boxplot() + scale_fill_brewer(palette="Accent") +
  coord_flip()+
  theme(legend.position="top", 
        axis.text = element_text(vjust = 0.5, size = 9),
        legend.text = element_text(size = 11), 
        axis.title=element_text(size=11)
  )+
  xlab("Sub regions") + ylab("Infant mortality rate") 
box_plot2
ggsave('Boxplot2.pdf', plot = box_plot2)



# Task 4: comparison of 2001 with 2021

countries  <- census_data[which(is.na(census_data$Mortality_rate)),]$Country

census_data_2001  <- census_data_2001  %>% filter(!Country %in% countries)
census_data_2021  <- census_data_2021  %>% filter(!Country %in% countries)

scat_plot1 <- ggplot(data = NULL, aes(x = census_data_2001$Life_exp_both, 
                                      y = census_data_2021$Life_exp_both, 
                                      color = census_data_2001$Region)) + 
  geom_point(size = 2.5) + guides(colour = guide_legend(title = "Subregion", size = 16)) + 
  geom_abline(intercept =0 , slope = 1)+ xlim(40,90)+ylim(40,90) +
  xlab("Life expectancy of both sexes in 2001") + ylab("Life expectancy of both sexes in 2021") + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face="bold"),
        legend.position = c(0.15, 0.85),legend.background = element_rect(fill = "transparent"), 
        legend.text = element_text(size = 14), 
        axis.text=element_text(size=14),
        axis.title=element_text(size=18))

scat_plot2  <- ggplot(data = NULL, aes(x = census_data_2001$Mortality_rate, 
                                       y = census_data_2021$Mortality_rate, 
                                       color = census_data_2001$Region)) + geom_abline(intercept =0 , slope = 1) +
  xlim(0,150)+ylim(0,150) +
  geom_point(size = 2.5) + guides(colour = guide_legend(title = "Subregion", size = 16)) + 
  xlab("Infant mortality rate in 2001") + ylab("Infant mortality rate in 2021") + 
  theme(plot.title = element_text(hjust = 0.5, size = 24, face="bold"),
        legend.position = c(0.15, 0.85),legend.background = element_rect(fill = "transparent"), 
        legend.text = element_text(size = 14), 
        axis.text=element_text(size=14),
        axis.title=element_text(size=18))

ggsave("final_plot1.pdf",plot = scat_plot1)
ggsave("final_plot2.pdf",plot = scat_plot2)
scat_plot1
scat_plot2
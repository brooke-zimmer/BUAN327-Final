library(dplyr)
library(gglpot2)
library(tidyverse)
library(maps)
library(sf)

nursing= read.csv("2021_CostReport.csv")

colnames(nursing)

##Using R dplyr, calculate the average revenue per nursing home
nursing %>% summarise(mean(Gross.Revenue, na.rm= TRUE))

#Using R dplyr, calculate the average net income per nursing home
nursing %>% summarise(mean(Net.Income, na.rm= TRUE))

#   Using R dplyr, calculate the profit margin per county.
profit_by_county <- nursing %>% 
  group_by(County) %>%
  summarise(mean_profit_margin= mean(Net.Income / Gross.Revenue, na.rm = TRUE))

# Using R dplyr, find the top 10 counties that have the highest profit margin?

top_10_profit_margins <- profit_by_county %>%
  arrange(desc(mean_profit_margin)) %>%
  slice_head(n = 10)

#Using R dplyr, find the counties with the lowest number of beds available.

counties_lowest_beds <- nursing %>%
  arrange(SNF.Number.of.Beds) %>%
  slice_head(n=10)

#Create 2 maps to show which counties could be better for investing in nursing homes
#This one is given
bed_data <- nursing %>%
  select(County, Number.of.Beds)

counties <- map_data("county")

bed_data <- bed_data %>%
  mutate(County = tolower(gsub(" county", "", County))) %>%
  group_by(County) %>%
  summarise(Total_Beds = sum(Number.of.Beds, na.rm = TRUE))

counties <- counties %>%
  mutate(subregion = tolower(subregion)) %>%
  left_join(bed_data, by = c("subregion" = "County"))

ggplot(data = counties, aes(x = long, y = lat, group = group, fill = Total_Beds)) +
  geom_polygon(color = "white") +
  coord_quickmap() +
  scale_fill_viridis_c(option = "plasma", name = "Total Beds") +
  labs(title = "Total Number of Beds by County") +
  theme_minimal()   
##########################################################MAP1
revenue_data <- nursing %>%
  select(County, Gross.Revenue)
counties <- map_data("county")
revenue_data <- revenue_data %>%
  mutate(County = tolower(gsub(" county", "", County))) %>%
  group_by(County) %>%
  summarise(Total_Revenue = sum(Gross.Revenue, na.rm = TRUE))
counties <- counties %>%
  mutate(subregion = tolower(subregion)) %>%
  left_join(revenue_data, by = c("subregion" = "County"))

ggplot(data = counties, aes(x = long, y = lat, group = group, fill = Total_Revenue)) +
  geom_polygon(color = "orange") +
  coord_quickmap() +
  scale_fill_viridis_c(option = "plasma", name = "Total Revenue") +
  labs(title = "Total Revenue by County") +
  theme_minimal() 

###################MAP2
admissions_data <- nursing %>%
  select(County,SNF.Admissions.Total)
counties <- map_data("county")
admissions_data <- admissions_data %>%
  mutate(County = tolower(gsub(" county", "", County))) %>%
  group_by(County) %>%
  summarise(SNF.Admissions.Total = sum(SNF.Admissions.Total, na.rm = TRUE))

counties <- counties %>%
  mutate(subregion = tolower(subregion)) %>%
  left_join(admissions_data, by = c("subregion" = "County"))

ggplot(data = counties, aes(x = long, y = lat, group = group, fill = SNF.Admissions.Total)) +
  geom_polygon(color = "salmon") +
  coord_quickmap() +
  scale_fill_viridis_c(option = "plasma", name = "Total Admissions") +
  labs(title = "Total Admissions by County") +
  theme_minimal()



#Create a graph (you decide which kind) to show the relationship between length of stay and ratings.
# Box Plot
provider = read.csv("ProviderInfo_2021.csv")

new_nursing  <- merge(nursing, provider, by.x = "Facility.Name", by.y = "Provider.Name")

stay.vs.rating <- new_nursing[c("SNF.Average.Length.of.Stay.Total", "Facility.Name", "Overall.Rating")]
stay.vs.rating <- as.data.frame(stay.vs.rating)

stay_vs_rating <- stay.vs.rating %>%
  filter(!is.na(Overall.Rating), !is.na(SNF.Average.Length.of.Stay.Total))

ggplot(stay_vs_rating, aes(x = factor(Overall.Rating), y = SNF.Average.Length.of.Stay.Total)) +
  geom_boxplot() +
  labs(x = "Overall Rating", y = "Length of Stay (log scale)") +
  ggtitle("Distribution of Length of Stay by Overall Rating (log scale)") +
  scale_y_continuous(trans = "log10")


#Create a pie chart ( you decide of what) 
#Pie chart of  Types of Control variables 1-13 are defined as  1= Voluntary NonprofitChurch,
# 2= Voluntary Nonprofit-Other, 3 = ProprietaryIndividual, 4 = Proprietary-Corporation, 
#5 = ProprietaryPartnership, 6 = Proprietary-Other, 7 = Governmental-Federal,
#8 = Governmental-City-County, 9 = Governmental-County, 10 = Governmental-State, 
#11 = Governmental-facility District, 12 = Governmental-City, 13 = Governmental-Other

control_counts <- table(nursing$Type.of.Control)
control_df <- as.data.frame(control_counts)
colnames(control_df) <- c("Type_of_Control", "Count")

ggplot(control_df, aes(x = "", y = Count, fill = factor(Type_of_Control))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_viridis_d() +  
  labs(title = "Distribution of Nursing Homes by Type of Control") +
  theme_void() +
  theme(legend.position = "right")




#Run a hypothesis test ANOVA to compare the revenues in three different states 
#( you decide which states). Explain your findings
selected_states <- c("NY", "FL", "CA")
states_data <- nursing %>% 
  filter(State.Code %in% selected_states)

anova_result <- aov(Gross.Revenue ~ State.Code, data= states_data)
summary(anova_result)


#Create a regression analysis to identify the most significant influential factors 
#impacting nursing homes performance.

dependent_variable <- "Gross.Revenue"  
independent_variables <- c("SNF.Admissions.Total", "Number.of.Beds", "SNF.Average.Length.of.Stay.Total")

regression_data <- nursing %>%
  select(all_of(c(dependent_variable, independent_variables))) %>%
  drop_na()  

regression_model <- lm(formula = paste(dependent_variable, "~", paste(independent_variables, collapse = "+")), data = regression_data)

summary(regression_model)








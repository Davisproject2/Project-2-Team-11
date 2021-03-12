
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggthemes)
library(patchwork)
library(gganimate)

# SET UP

covid <- read_csv("../Data/https://covid19.who.int/WHO-COVID-19-global-data.csv")
c_h_index <- read_csv("../Data/covid-containment-and-health-index.csv")
pop_index <- read_csv("../Data/population.csv")

colnames(covid)[which(names(covid) == "Date_reported")] <- "Date"
colnames(covid)[which(names(covid) == "Country_code")] <- "Code"

colnames(c_h_index)[which(names(c_h_index) == "Entity")] <- "Country"

covid <- covid[!(covid$Date == "2021-03-03" | covid$Date == "2021-03-02"),]
covid$Date_of_Restrictions <- covid$Date - 28

covid_index <- merge(covid, c_h_index, by.x = c("Date_of_Restrictions", "Country"), by.y = c("Date", "Country"))

pop <- pop_index[c("Country Name","2019")]
pop <- pop[!is.na(pop["2019"]),]

covid_index_pop = merge(covid_index, pop, by.x = c("Country"), by.y = c("Country Name"))
colnames(covid_index_pop)[which(names(covid_index_pop) == "2019")] <- "Population"
covid_index_pop$new_cases_div_by_pop <- covid_index_pop$New_cases / covid_index_pop$Population

# total cases worldwide to date

total.cc = sum(covid$New_cases)
a <- covid %>% group_by(Country) %>% 
  summarise(sum_newcase = sum(New_cases), max_newcase_perday_bycountry = max(New_cases)) %>% 
  arrange(desc(sum_newcase))
# total deaths worldwide to date
total.cd = sum(covid$New_deaths)
b <- covid %>% 
  group_by(Country) %>% 
  summarise(sum_newdeath = sum(New_deaths), max_newdeath_perday_bycountry = max(New_deaths)) %>% 
  arrange(desc(sum_newdeath))

plot1 <- ggplot(data=covid, mapping = aes(x=Date, y=New_cases)) + 
  geom_line(color = "steelblue2", size = 0.1) +
  labs(title = "Worldwide Total New Covid Cases Per day",
       subtitle = "How is Covid spreading around the world?",
       x = "Date Reported (Daily)",
       y = "# of New Covid Cases") +
  theme_fivethirtyeight() + 
  theme(axis.title = element_text())

plot2 <- ggplot(data=covid, mapping = aes(x=Date, y=New_deaths)) + 
  geom_line(color = "sienna2", size = 0.1) +
  labs(title = "Worldwide Total Covid Death Cases Per day",
       subtitle = "How many people died from Covid?",
       x = "Date Reported (Daily)",
       y = "# of New Covid Deaths") +
  theme_fivethirtyeight() + 
  theme(axis.title = element_text()) 

plot1 / plot2 + plot_layout(ncol = 1, heights=c(4,4))

Italy <- covid[covid$Country == "Italy",]

plot_Italy_c <- Italy %>% ggplot(aes(x=Date, y=New_cases)) + 
  geom_line(color = "darkorange", size = 0.5)+
   labs(title = "Total Covid New Cases in Italy (Daily)",
       subtitle = "How covid cases increase in the Italy?",
       x = "Date Reported (Daily)",
       y = "# of New Cases") +
  theme_fivethirtyeight() + 
  theme(axis.title = element_text()) 

plot_Italy_d <- Italy %>% ggplot(aes(x=Date, y=New_deaths)) + 
  geom_line(color = "firebrick4", size = 0.5)+
   labs(title = "Total New Covid Deaths in Italy (Daily)",
       subtitle = "How covid deaths increase in the Italy?",
       x = "Date Reported (Daily)",
       y = "# of New Deaths",
       color = "Country") +
  theme_fivethirtyeight() + 
  theme(axis.title = element_text()) 

plot_Italy_c / plot_Italy_d + plot_layout(ncol = 1, heights=c(4,4))

Italy2 <- covid_index[covid_index$Country == "Italy",]

plot_Italy_ci <- Italy2 %>% ggplot(aes(x=Date, y=containment_index)) + 
  geom_line(color = "lightpink4", size = 0.5)+
   labs(title = "Containment Index score in Italy (Daily)",
       subtitle = "How the containment inbdex changes in the Italy?",
       x = "Date Reported (Daily)",
       y = "containment index (0-100)") +
  theme_fivethirtyeight() + 
  theme(axis.title = element_text()) 

plot_Italy_d <- Italy %>% ggplot(aes(x=Date, y=New_deaths)) + 
  geom_line(color = "firebrick4", size = 0.5)+
   labs(title = "Total New Covid Deaths in Italy (Daily)",
       subtitle = "How covid deaths increase in the Italy?",
       x = "Date Reported (Daily)",
       y = "# of New Deaths") +
  theme_fivethirtyeight() + 
  theme(axis.title = element_text())

plot_Italy_d / plot_Italy_ci + plot_layout(ncol = 1, heights=c(4,4))

US <- covid[covid$Country == c("United States of America","Brazil","India"),] 

plot_top3 <- US %>% ggplot(aes(x=Date, y=New_cases,color = Country)) + 
  geom_line(size = 0.5)+
   labs(title = "Total Covid New Cases in Top Three Countries (Daily)",
       subtitle = "How covid increases in Top Three Countries?",
       x = "Date Reported (Daily)",
       y = "# of New Cases") + 
  theme(axis.title = element_text()) + 
  theme_fivethirtyeight() + 
  theme(axis.title = element_text()) + 
  theme(plot.title = element_text(size=16))
  
plot_top3

covid$Date_of_Restrictions <- covid$Date - 28

covid_index <- merge(covid, c_h_index, by.x = c("Date_of_Restrictions", "Country"), by.y = c("Date", "Country"))

pop <- pop_index[c("Country Name","2019")]
pop <- pop[!is.na(pop["2019"]),]

covid_index_pop = merge(covid_index, pop, by.x = c("Country"), by.y = c("Country Name"))

colnames(covid_index_pop)[which(names(covid_index_pop) == "2019")] <- "Population"

covid_index_pop$new_cases_div_by_pop <- covid_index_pop$New_cases / covid_index_pop$Population

plot_01_03_2020 <- covid_index_pop %>% ggplot() + 
  geom_histogram(mapping = aes(x = containment_index), fill = "slategray3", size = 0.5, binwidth = 1)+
  labs(title = "Containment Index Distribution starting on 01/03/2020",
       x = "Containment Index",
       y = "Frequency") +
  theme_fivethirtyeight() + 
  theme(axis.title = element_text()) +
  theme(plot.title = element_text(size=16))
plot_01_03_2020

covid_post_0130 <- with(covid_index_pop, covid_index_pop[Date > "2020-01-29",])
covid_post_0130

plot_01_30_2020 <- covid_post_0130 %>% ggplot() + 
  geom_histogram(mapping = aes(x = containment_index), fill = "slategray3", size = 0.5, binwidth = 1)+
  labs(title = "Containment Index Distribution starting on 01/30/2020",
       x = "Containment Index",
       y = "Frequency") +
  theme_fivethirtyeight() + 
  theme(axis.title = element_text()) +
  theme(plot.title = element_text(size=16))
plot_01_30_2020

covid_post_0311 <- with(covid_index_pop, covid_index_pop[Date > "2020-03-10",])
covid_post_0311

plot_03_11_2020 <- covid_post_0311 %>% ggplot() + 
  geom_histogram(mapping = aes(x = containment_index), fill = "slategray3", size = 0.5, binwidth = 1)+
  labs(title = "Containment Index Distribution starting on 03/11/2020",
       x = "Containment Index",
       y = "Frequency") +
  theme_fivethirtyeight() + 
  theme(axis.title = element_text()) +
  theme(plot.title = element_text(size=16))
plot_03_11_2020

covid_post_01300310 <- with(covid_index_pop, covid_index_pop[Date > "2020-01-29" & Date < "2020-03-10",])
covid_post_01300310

plot_01_30_thru_03_11_2020 <- covid_post_01300310 %>% ggplot() + 
  geom_histogram(mapping = aes(x = containment_index), fill = "slategray3", size = 0.5, binwidth = 1)+
  labs(title = "Containment Index Distribution 01/30/2020 thru 03/11/2020",
       x = "Containment Index",
       y = "Frequency") +
  theme_fivethirtyeight() + 
  theme(axis.title = element_text()) +
  theme(plot.title = element_text(size=16))
plot_01_30_thru_03_11_2020

quantile(covid_post_0311$containment_index, c(0.2, 0.4, 0.6, 0.8))

covid_index_pop$containment_index_leveled <- cut(covid_index_pop$containment_index,
                                                 breaks = c(-Inf, 38.46, 51.92, 61.54, 69.23 , Inf),
                                                 labels = c("Lenient", "Lenient_Mod", "Moderate", "Mod_Strict", "Strict"))

two_way_anova <- aov(new_cases_div_by_pop~containment_index_leveled+Country, data = covid_index_pop)
summary(two_way_anova)

TukeyHSD(two_way_anova, "containment_index_leveled")

plot(two_way_anova)

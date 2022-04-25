##
## Author: Krissy McHugh
## Date: 4/24/2021
## Midterm 2 | STA 309  
##
#####################
#  Build a dashboard comparing the impact of the three COVID-19 surges

library(tidyverse)
library(stringr)
# Loading COVID-19 data
ohioCovid <- read_csv("https://coronavirus.ohio.gov/static/dashboards/COVIDDeathData_CountyOfResidence.csv")
ohioVax <- read_csv("https://coronavirus.ohio.gov/static/dashboards/vaccine_data.csv")
ohioPop <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-39.csv") 

install.packages("readxl")

library(readxl)
election_data <- read_excel("/Users/krissymchugh/Desktop/STA 309/STA 309//ohioelectiondata.xlsx")
election_data <- load(url("https://github.com/krissymchugh/Data/blob/main/ohioelectiondata.RData"))

# ref for data: https://www.politico.com/2020-election/results/ohio/
# data was copy & pasted into excel sheet
# added to public github site
# note: load from github site was not working due to problem with my personal github site, which is why I am 
# showing my alternative method using excel. data file will need to be downloaded and loaded for code to work.

# Converting decimal to percent for Trump vote
election_data <- election_data %>% 
  select(c(County, `Trump Pct`)) %>%
  mutate(`Trump Vote` = (`Trump Pct`)*100,
         County = str_remove_all(County, " County"))

election_data <- election_data %>%
  select(-c(`Trump Pct`))

# Adding date information for surges
vax_compare <- ohioVax %>%
  mutate(Timing = case_when(date < as.Date("2021-07-01") ~ "Pre Delta",
                            date < as.Date("2021-12-01") ~ "Delta to Omicron",
                            TRUE ~ "Vaccines since Omicron start"),
         Timing = factor(Timing,
                         levels=c("Pre Delta", "Delta to Omicron", "Vaccines since Omicron start"))) %>%
  group_by(county, Timing) %>%
  summarize(Vaccines = sum(vaccines_completed) ) %>%    ## Sums in each surge
  mutate(Cumulative_Vaccines = cumsum(Vaccines),        ## Accumulate the vaccines, cumulative sum
         Timing = case_when(Timing == "Pre Delta" ~ "before Delta",
                            Timing == "Delta to Omicron" ~ "before Omicron",
                            TRUE ~ "Total"))

vax_compare <- vax_compare[vax_compare$Timing == 'Total', ]

# Population data set: getting rid of unecessary information
ohio_populations <- ohioPop %>%
  filter(YEAR==12) %>%
  select(County=CTYNAME, Population=POPESTIMATE) %>%
  mutate(County = str_remove_all(County, " County"))

# Getting rid of unecessary columns
vax_compare <-subset(vax_compare, county!="Unknown")
vax_compare <-subset(vax_compare, county!="Out of State")

# Changing column name to match
names(vax_compare)[names(vax_compare) == 'county'] <- 'County'

# Getting rid of unecessary columns
vax_compare <- vax_compare %>%
  select(-c(Timing, Vaccines))

# Mering Vax and population datasets
Pop_Vax_OH <- merge(ohio_populations, vax_compare, by="County")

# Finding vax rate for population of counties
Pct_Vax <- Pop_Vax_OH %>%
  group_by(County) %>%
  mutate(`% Vaccinated` = (Cumulative_Vaccines/Population)*100)

# Merging vax rate dataset with election dataset
election_vax <- merge(election_data, Pct_Vax, by="County")

# Generating plot comparing vax rate to election data
election_vax_plot <- ggplot(election_vax, aes(x=`Trump Vote`, y=`% Vaccinated`, text= County)) +
  geom_point(aes(size=Population)) +
  theme(title = element_text(face="bold"), size=1) +
  labs(x= "2020 Trump Vote %",
       legend.position = "none",
       title='COVID-19 Fully Vaccinated Levels out of Total Population',
       subtitle='Dots Representing Relative Population Size of County') +
  theme_minimal() 
  

## PLOT 2

# Defining surges in covid death data
covid_deaths <- ohioCovid %>%
  mutate(Surge = case_when(`Date Of Death` %in% seq.Date(as.Date("2020-01-01"), as.Date("2020-05-31"), by="day") ~ "Initial Cases", 
                           `Date Of Death` %in% seq.Date(as.Date("2020-10-01"), as.Date("2021-02-28"), by="day") ~ "Alpha Surge",
                           `Date Of Death` %in% seq.Date(as.Date("2021-07-01"), as.Date("2021-11-30"), by="day") ~ "Delta Surge",
                           `Date Of Death` %in% seq.Date(as.Date("2021-12-01"), as.Date("2022-02-28"), by="day") ~ "Omicron Surge",
                           `Date Of Death` >= as.Date("2021-03-01") ~ "Other, Post Vaccine",
                           TRUE ~ "Other, Pre Vaccine") ) %>%
  group_by(County, Surge) %>%
  summarize(Deaths = sum(`Death Due To Illness Count - County Of Residence`, na.rm=TRUE) ) 

# Filtering only Alpha surge data
alpha_deaths <- covid_deaths %>%
  filter(Surge == "Alpha Surge")

alpha_pop <- merge(alpha_deaths, ohio_populations)

# Finding death rate for alpha surge 
alpha_death_rate <- alpha_pop %>%
  group_by(County) %>%
  mutate("Death Rate" = (Deaths/Population)*100)

# Repeating process for Delta surge
delta_deaths <- covid_deaths %>%
  filter(Surge == "Delta Surge")

delta_pop <- merge(delta_deaths, ohio_populations)

delta_death_rate <- delta_pop %>%
  group_by(County) %>%
  mutate("Death Rate" = (Deaths/Population)*100)

# Repeating process again for Omicron surge
omicron_deaths <- covid_deaths %>%
  filter(Surge == "Omicron Surge")

omicron_pop <- merge(omicron_deaths, ohio_populations)

omicron_death_rate <- omicron_pop %>%
  group_by(County) %>%
  mutate("Death Rate" = (Deaths/Population)*100)

# Binding three surge datasets together
Surge_Death_Rate <- rbind(alpha_death_rate, delta_death_rate, omicron_death_rate)

# Deteerming which counties have the highest/lowest vaccination rate
MostVax <- ohio_county_covid_data %>%
  arrange(desc(`Vax Percent Total`)) 
head(top3Vax)
tail(top3Vax)

# Most vax: Delaware, Lake, Warren
# Least vax: Darke, Mercer, Highland

top3vax <- Surge_Death_Rate %>%
  filter(County == "Delaware" | County == "Lake" | County == "Warren") 
view(top3vax)

# Plot of death rate for most vaccinated counties 
top3_vax <- ggplot(top3vax, aes(x=Surge, y=`Death Rate`, fill=County)) +
  geom_bar(position="dodge", stat="identity") +
  ylim(0, .17) +
  labs(title="Death Rate for the Three Most Vaccinated Counties in Ohio") +
  theme(legend.position="bottom",
        axis.title.y = element_blank(),
        legend.title= element_blank()) 

bottom3vax <- Surge_Death_Rate %>%
  filter(County == "Darke" | County == "Mercer" | County == "Highland") 

# Plot of death rate for least vaccinated counties
bottom3_vax <- ggplot(bottom3vax, aes(x=Surge, y=`Death Rate`, fill=County)) +
  geom_bar(position="dodge", stat="identity") +
  ylim(0, .17) +
  labs(title="Death Rate for Three Least Vaccinated Counties in Ohio") +
  theme(legend.position="bottom",
        axis.title.y = element_blank(),
        legend.title= element_blank()) 

# PLOT 3

# Reading in Ohio Covid data frame

covidDF <- read_csv(paste0(OhioURL,"COVIDDeathData_CountyOfResidence.csv"),
                    col_types = "cccDDDiii")

# Specifying surges 
county_covid_bydate <- covidDF %>%
  group_by(Date=`Onset Date`) %>%
  summarize(Cases = sum(`Case Count`)) %>%
  mutate(Surge = case_when(Date >= as.Date("2020-10-01") & Date <= as.Date("2021-02-28") ~ "Alpha",
                           Date >= as.Date("2021-07-01") & Date <= as.Date("2021-10-31") ~ "Delta",
                           Date >= as.Date("2021-12-01") ~ "Omicron",
                           Date <  as.Date("2020-10-01") ~ "Initial Cases",
                           TRUE ~ "Gaps")) %>%
  mutate(Surge = factor(Surge, levels=c("Initial Cases", "Alpha", "Delta", "Omicron", "Gaps") ) ) 

# Generating plot showing cases over time
timeline <- ggplot(county_covid_bydate, aes(x=Date, y=Cases, group=1)) +
  geom_line() + 
  geom_rect(aes(xmin = as.Date("2020-10-01"),xmax = as.Date("2021-02-28"),ymin = 0, ymax = Inf),
            fill="#56B4E9", 
            alpha = .002)+
  coord_cartesian(expand=FALSE) +
  geom_rect(aes(xmin = as.Date("2021-07-01"),xmax = as.Date("2021-10-31"),ymin = 0, ymax = Inf),
            fill="#009E73", 
            alpha = .002) +
  geom_rect(aes(xmin = as.Date("2021-12-01"),xmax = as.Date("2022-02-02"),ymin = 0, ymax = Inf),
            fill="#F0E442", 
            alpha = .002) +
  xlim(as.Date("2020-10-01"), as.Date("2022-02-02")) +
  coord_cartesian(expand=FALSE) +
  labs(title= "COVID-19 Cases in Ohio",
       subtitle="Shaded by the Alpha, Delta, and Omicron Surges") +
  theme_minimal()


library(patchwork)

# Creating dashboard
covid_dashboard <- (timeline + election_vax_plot)/(top3_vax + bottom3_vax) 
covid_dashboard


ggsave("covid_dashboard.pdf", width = 15, height = 10)

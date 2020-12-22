# If completing this assignment as a team,
# make sure I have set up your team in Canvas (or it won't be able to award all team members credit).
# Additionally, please list team members' names
# and usernames on the lines just below:
# 1. 
# 2. 
# 3. 

# Run the three lines of code below to install the packages needed. ####
# As always, installing the Tidyverse packages will take a few minutes.
# ggthemes and ggrepel should go much faster.

install.packages("tidyverse")
install.packages("ggthemes")
install.packages("ggrepel")

# Once installation completes, find: ####
# dplyr (*not* dbplyr),
# ggplot2,
# ggthemes, and
# ggrepel
# in the Packages list, and check their boxes.

# Import the necessary data by running the line of code below. ####
DoingBusiness <- read.csv("https://iu.box.com/shared/static/fv0l9h35p76qzqiocnxe9ju8pcige1cw.csv")

# FOR EACH PROBLEM, LOCATE *ALL* WORK FOR THAT PROBLEM JUST BELOW ITS PROMPT.

# Do not make any modifications to the Doing Business object,
# since you will submit only your R script file. We will run your code on *our* imported dataset.

# Part 1 ####

# *Here and elsewhere on this project*, you may need to filter
# or otherwise structure the data before creating a plot.
# When you do this, write your code in the area designated for that problem,
# assign results to a variable, naming the variable as indicated.

# Problem 1. Days to start a business, African subregions, 2004 versus 2018. ####
# See the sketch for plot 1.
# Color-code the outlines, but not the interiors, of the boxes.

# First, create the data source for the chart.
# Name the data source plot_1_data
plot_1_data <- DoingBusiness %>% 
  filter(WorldRegion == "Africa", SurveyYear == 2004 | SurveyYear == 2018)

# Then, create the chart.
ggplot(plot_1_data, aes(x = SubRegion,
                        y = StartBus.Days,
                        color = SubRegion)) +
  geom_boxplot()+
  facet_wrap(~SurveyYear)+
  labs(title = "Plot 1: Days to start a business, Africa, 2004 vs. 2018",
       x = "Subregion",
       y = "Days to start a business")+
  theme(axis.text.x = element_text(angle=90))+
  theme(legend.position = "none")+
  scale_colour_brewer(palette = "Accent")

# Part 2. OECD: 2018 makeup by region; growth in membership over time. ####
# OECD: Organisation for Economic Cooperation and Development

# Problem 2.1 Percent makeup of OECD countries by region, 2018 data. ####
# See the sketch for table 1.
# Store results as a variable named table_1

table_1 <- DoingBusiness %>% 
  group_by(WorldRegion) %>% 
  summarize(Total = sum(OECDMember)) %>% 
  mutate(Pct = Total / sum(Total) * 100)

# Problem 2.2 Growth in OECD membership over time, by region, ####
# limited to regions with OECD members.
# See the sketch for table 2.
# Store results as a variable named table_2

table_2 <- DoingBusiness %>% 
  group_by(SurveyYear, WorldRegion) %>% 
  summarize(Total = sum(OECDMember)) %>% 
  filter(Total > 0)
  

# Problem 2.3 Graph the table_2 results. ####
# See the sketch for plot 2.
ggplot(table_2, aes(x = SurveyYear, y = Total, color = WorldRegion)) + 
  geom_line() + 
  labs(title = "Plot 2: Total OECD Members by Region",
       x = "DB Survey Year",
       y = "OECD Members") +
  scale_colour_discrete("Region")+
  theme_tufte()



# Part 3. Starting a business vs. enforcing contracts, OECD (and non-OECD). ####

# Problem 3.1 Days to start a business versus days to enforce a contract, ####
# OECD countries only,
# in 2004 and 2018.
# See the sketch for plot 3.
# Name the data source plot_3_data

plot_3_data <- DoingBusiness %>% 
  group_by(EconomyName) %>% 
  filter(OECDMember > 0, SurveyYear == "2004" | SurveyYear == "2018") %>% 
  select(SurveyYear, EnforceContract.Days, StartBus.Days, WorldRegion)
  
ggplot(plot_3_data, aes(x = StartBus.Days, y = EnforceContract.Days, color = WorldRegion, label = EconomyName))+
  geom_point()+
  facet_wrap(~SurveyYear)+
  theme_tufte()+
  theme(legend.position = "none")+
  labs(title = "Plot 3: Enforcing a contract vs. starting a business, OECD Countries, 2004 vs. 2018",
       x = "Days to start a business",
       y = "Days to enforce a contract")+
  geom_text_repel()+
  theme(strip.background = element_blank())+
  theme(strip.text = element_blank())

# Problem 3.2 Days to enforce contracts in OECD countries, by economy and year ####
# Look over plot 3 to identify four countries that showed a lot of movement from 2004 to 2018
# in the "days to enforce" dimension, and plot only those.
# See the sketch for plot 4.
# Name the data source plot_4_data

plot_4_data <- DoingBusiness %>% 
  group_by(EconomyName) %>% 
  filter(OECDMember > 0, EconomyName %in% c("Greece","Poland","Finland","Canada")) %>% 
  select(EconomyName, SurveyYear, EnforceContract.Days)

ggplot(plot_4_data, aes(x=SurveyYear,y=EnforceContract.Days))+
  geom_line()+
  facet_wrap(~EconomyName)+
  theme_fivethirtyeight()+
  labs(title = "Plot 4: Days to enforce contracts, select OECD countries")+
  theme(plot.title = element_text(size=14))

# Problem 3.3 Days to start a business, non-OECD and OECD countries, 2004 and 2018 ####
# See the sketch for plot 5.
# !!! Note: To use OECD membership as the x, you will need to make it a factor.
# In your ggplot code, rather than x = OECDMember, use x = factor(OECDMember)
# Name the data source plot_5_data

plot_5_data <- DoingBusiness %>%  
  group_by(WorldRegion) %>% 
  filter(SurveyYear == 2004 | SurveyYear == 2018) %>% 
  select(WorldRegion, OECDMember, SurveyYear, StartBus.Days)

ggplot(plot_5_data, aes(x=factor(OECDMember),y=StartBus.Days,color = WorldRegion))+
  geom_boxplot()+
  facet_grid(SurveyYear~WorldRegion)+
  labs(title = "Plot 5: Days to start a business, non-OECD vs. OECD, 2004 vs. 2018",
       x = "1 = OECD",
       y = "Days to start a business")+
  theme_gdocs()+
  theme(legend.position = "none")+
  theme(plot.title = element_text(size=14))

# Part 4. Key starting efficiency measures: Select EU economies, all-Europe, world regions. ####

# Problem 4.1 Belgium, France, Germany, Ireland, Luxembourg, and Netherlands are countries that have become/are expected
# to become the home of new headquarters and other operations for a number of companies planning to leave the UK post-Brexit.
# Look at combined days to start a business and obtain electricity in those six countries, over the years of the Doing Business study.
# Since the Doing Business study did not contain the "Elec.Days" question prior to 2010,
# only years from 2010 and after should be included.
# See the sketch for plot 6.
# Name the data source plot_6_data

plot_6_data <- DoingBusiness %>% 
  filter(EconomyName %in% c("Belgium", "France", "Germany", "Ireland", "Luxembourg", "Netherlands"), SurveyYear >= 2010) %>% 
  mutate(StartBusANDElecCombine = StartBus.Days + Electricity.Days) %>% 
  select(EconomyName, SurveyYear, StartBusANDElecCombine, StartBus.Days, Electricity.Days)

ggplot(plot_6_data, aes(x = SurveyYear, y = StartBusANDElecCombine, color = EconomyName))+
  geom_line()+
  theme_tufte()+
  scale_colour_brewer(palette = "Dark2")+
  labs(title = "Plot 6: Days to start a business and get electricity, 2010 - present",
       x = "DB Survey Year",
       y = "Days to start + get electricity")+
  scale_colour_discrete("Economy")

# Problem 4.2 Days that it has taken to start a business and obtain electricity over the years,
# in Europe. ####
# Only the years 2010 and after should be included, for the reason given in the previous problem.
# See the sketch for plot 7.
# Name the data source plot_7_data

plot_7_data <- DoingBusiness%>% 
  filter(WorldRegion == "Europe", SurveyYear >= 2010) %>% 
  mutate(StartBusANDElecCombine = StartBus.Days + Electricity.Days) %>% 
  select(EconomyName, EconomyCode, SurveyYear, StartBusANDElecCombine, StartBus.Days, Electricity.Days, IncomeGroup)

ggplot(plot_7_data, aes(x=SurveyYear, y=StartBusANDElecCombine, color = IncomeGroup))+
  geom_line()+
  facet_wrap(~EconomyCode)+
  theme_tufte()+
  scale_colour_brewer(palette = "Dark2")+
  labs(title = "Plot 7: Days to start a business and get electricity, 2010 - present",
       x = "DB Survey Year",
       y = "Days, start + get electricity")+
  scale_colour_discrete("Income Group")+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
  
# Problem 4.3 Number of records, P25, P50, and P75 of combined days to start a business + obtain a building permit, ####
# by region, year, and OECD status.
# See the sketch for table 3.
# Store results as a variable named table_3

table_3 <- DoingBusiness %>% 
  mutate(StartBldg = StartBus.Days + BldgPermits.Days, na.rm = TRUE) %>% 
  group_by(WorldRegion, SurveyYear, OECDMember) %>% 
  summarize(Countries = n(),
            P25StartAndConstr = quantile(StartBldg, 0.25, na.rm = TRUE),
            P50StartAndConstr = quantile(StartBldg, 0.5, na.rm = TRUE),
            P75StartAndConstr = quantile(StartBldg, 0.75, na.rm = TRUE))%>%
  na.exclude
   
# Problem 4.4 Graph the P75 values from table 3 so that within each region we can explore ####
# progress in this downside measure over the years.
# See the sketch for plot 8.
ggplot(table_3, aes(x = SurveyYear, y = P75StartAndConstr, color = WorldRegion))+
  geom_line()+
  facet_wrap(~OECDMember)+
  theme_economist()+
  labs(title = "Plot 8: P75 of days to start a business and obtain construction permits, 2006 - present, non-OECD (0) vs. OECD",
       x = "DB Survey Year",
       y = "Days, start + construction permits")+
  scale_colour_discrete("Region")+
  theme(plot.title = element_text(size=14))

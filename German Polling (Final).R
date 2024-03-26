##################
# German Polling #
##################
library(tidyverse)
library(rvest)
library(plyr)
library(ggplot2)
library(ggthemes)


# Extracting polls from Wiki
data <-
  read_html("https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_German_federal_election")
data_tables <- data %>% html_table(header=TRUE, fill=TRUE)


# Bind polls together
polls <- data_tables[1:4]
all_polls <- do.call(rbind.fill, polls)


# Convert chr type cols to doubles
# (FW and BSW are these)
all_polls <- all_polls %>%
  mutate(FW = as.numeric(FW),
         BSW = as.numeric(BSW))


# Drop junk rows that wikipedia tables create
all_polls <- all_polls %>%
  filter(`Polling firm` != "Polling firm" & `Polling firm` != "2021 federal election")


# Data Cleaning
# Function cleans from wiki format to date format
clean_wiki <- function(polls) {

  # Split the Fieldwork date column into separate columns,
  # using this I create final D/M/Y cols
  temp <- polls %>%
    separate(`Fieldwork date`, into = c("d", "m", "y", "d2", "m2", "y2"), sep = " ") %>%
    mutate(cond = is.na(d2)) %>%
    mutate(
      DAY = ifelse(cond, d, d2),
      MONTH = ifelse(cond, m, m2),
      YEAR = ifelse(cond, y, y2)
    )
  temp <- subset(temp, select = -c(d, m, y, d2, m2, y2, cond))
  
  
  # Making them dates
  temp <- temp %>%
    unite(polldate, c("YEAR", "MONTH", "DAY"), sep = '-') %>%
    mutate(polldate = as.Date(polldate, "%Y-%b-%d"))
  temp
  temp <- subset(temp, select = -c(Abs.))
  
  return(temp)
}


# Creating clean data
clean_polls <- clean_wiki(all_polls)

clean_polls
# Arrange by polldate, rename some parties for English speakers
clean_polls <- clean_polls %>%
  arrange(polldate)


# Define party colors
party_colors <- c(AfD = '#009EE0', Union = '#161A1D', FDP = '#FFED00',
                  SPD = '#E3010F', Grüne = '#64A12D', Linke = '#BE3075',
                  FW = '#FFA500', BSW = '#784072')


# Graph
poll_graph <- ggplot(clean_polls, aes(x=polldate)) +
  
  geom_point(alpha=0.55, aes(y=AfD, color="AfD")) +
  geom_smooth(aes(y=AfD, color="AfD"), size=1.2, method='loess', span=0.1, se=FALSE) +
  
  geom_point(alpha=0.55, aes(y=Union, color="Union")) +
  geom_smooth(aes(y=Union, color = "Union"), size=1.2, method='loess', span=0.1, se=FALSE) +
  
  geom_point(alpha=0.55, aes(y=FDP, color="FDP")) +
  geom_smooth(aes(y=FDP, color="FDP"), size=1.2, method='loess', span=0.1, se=FALSE) +
  
  geom_point(alpha=0.55, aes(y=SPD, color="SPD")) +
  geom_smooth(aes(y=SPD, color="SPD"), size=1.2, method='loess', span=0.1, se=FALSE) +
  
  geom_point(alpha=0.55, aes(y=Grüne, color="Grüne")) +
  geom_smooth(aes(y=Grüne, color="Grüne"), size=1.2, method='loess', span=0.1, se=FALSE) +
  
  geom_point(alpha=0.55, aes(y=Linke, color="Linke")) +
  geom_smooth(aes(y=Linke, color="Linke"), size=1.2, method='loess', span=0.1, se=FALSE) +
  
  geom_point(alpha=0.55, aes(y=FW, color="FW")) +
  geom_smooth(aes(y=FW, color="FW"), size=1.2, method='loess', span=0.1, se=FALSE) +
  
  geom_point(alpha=0.55, aes(y=BSW, color="BSW")) +
  geom_smooth(aes(y=BSW, color="BSW"), size=1.2, method='loess', span=0.8, se=FALSE) +
  
  scale_y_continuous(name = "Opinion", breaks=c(0, 10, 20, 30)) +
  scale_x_date(name = "Date", date_breaks = "4 month") +
  
  ggtitle('Opinion polling for the next German federal election') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = party_colors, name = 'Party') +
  theme_economist(dkpanel=TRUE)
poll_graph
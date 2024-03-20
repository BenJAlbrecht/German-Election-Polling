##################
# German Polling #
##################


library(rvest)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(ggthemes)


# Extracting polls from Wiki
data <-
  read_html("https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_German_federal_election")
data_tables <- data %>% html_table(header=TRUE, fill=TRUE)

# List of polls 2021, 2022, 2023
polls <- data_tables[1:3]

# Data cleaning
# function cleans from wiki format to date format
clean_wiki <- function(poll_list) {
  new_poll_list <- lapply(poll_list, function(poll) {
    test <- poll %>%
      slice(-c(1,n()))
    
    test <- test %>%
      separate(`Fieldwork date`, into = c("d", "m", "y", "d2", "m2", "y2"), sep = " ") %>%
      mutate(cond = is.na(d2)) %>%
      mutate(
        DAY = ifelse(cond, d, d2),
        MONTH = ifelse(cond, m, m2),
        YEAR = ifelse(cond, y, y2)
      )
    test <- subset(test, select = -c(d,m,y,d2,m2,y2,cond))
    
    test <- test %>%
      separate(DAY, into = c('first', 'second'), sep='–') %>%
      mutate(cond = is.na(second)) %>%
      mutate(DAY = ifelse(cond, first, second))
    test <- subset(test, select = -c(first, second, cond))
    
    test <- test %>%
      unite(polldate, c("YEAR", "MONTH", "DAY"), sep='-') %>%
      mutate(polldate = as.Date(polldate, "%Y-%b-%d"))
    test <- subset(test, select = -c(Abs.))
    
    return(test)
  })
  
  return(new_poll_list)
}

clean_polls <- clean_wiki(polls)

all_polls <- bind_rows(lapply(clean_polls, identity))
all_polls <- all_polls %>%
  arrange(polldate)

p <- ggplot(all_polls, aes(x=polldate)) +
  geom_point(alpha = 0.55, aes(y = AfD), color = '#009EE0') +
  geom_point(alpha = 0.55, aes(y = Union), color = '#161A1D') +
  geom_point(alpha = 0.55, aes(y = FDP), color = '#FFED00') +
  geom_point(alpha = 0.55, aes(y = SPD), color = '#E3010F') +
  geom_point(alpha = 0.55, aes(y = Grüne), color = '#64A12D') +
  geom_point(alpha = 0.55, aes(y = Linke), color = '#BE3075') +
  scale_y_continuous(name = "Opinion", breaks=c(0, 10, 20, 30)) +
  scale_x_date(date_breaks = "3 month", )
p

write.csv(all_polls,"C:\\Users\\benjo\\Downloads\\all_polls.csv", row.names = FALSE)

View(all_polls)
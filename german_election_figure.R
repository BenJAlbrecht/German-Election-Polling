# MSE computation with German election data
# Repl. old project with some new curiosity


# Libs
#-------------------------------------------------------------------------------
library(tidyverse)
library(ggthemes)
library(rvest)
library(ggrepel)
#-------------------------------------------------------------------------------


# Scrape wikipedia polling, clean resulting tibble
#-------------------------------------------------------------------------------

# Scrape and select tables
data <- read_html("https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_German_federal_election")

data_tables <- data %>% 
  html_table(header = TRUE, fill = TRUE)

# Bind polls together
polls <- data_tables[1:4]
all_polls <- do.call(rbind.fill, polls)

# Basic cleaning
all_polls <- all_polls %>% 
  as_tibble() %>%
  filter(
    `Polling firm` != "Polling firm",
    `Polling firm` != "2021 federal election",
    `Polling firm` != "2024 EU Parliament Election"
  ) %>% 
  mutate(
    FW = as.numeric(FW),
    BSW = as.numeric(BSW),
    Linke = as.numeric(Linke)
  ) %>% 
  dplyr::rename(
    polling_firm = `Polling firm`,
    fieldwork_date = `Fieldwork date`,
    samplesize = Samplesize,
    Green = Grüne
  ) %>% 
  select(
    -Abs., -Others, -Lead, -samplesize
  )

# Pivot longer
all_polls_long <- all_polls %>% 
  pivot_longer(cols = -c(polling_firm, fieldwork_date),
               names_to = "party",
               values_to = "supp")

# Make fieldwork_date a date
clean_polls <- all_polls_long %>% 
  dplyr::rename(date = fieldwork_date) %>% 
  separate(date,
           into = c("d", "m", "y", "d2", "m2", "y2"), sep = " ") %>% 
  mutate(
    dd = case_when(
      is.na(d2) ~ d,
      !is.na(d2) ~ d2),
    
    mm = case_when(
      is.na(m2) ~ m,
      !is.na(m2) ~ m2),
    
    yy = case_when(
      is.na(y2) ~ y,
      !is.na(y2) ~ y2)
    ) %>% 
  select(
    -d, -m, -y, -d2, -m2, -y2
  ) %>% 
  mutate(
    dd = case_when(
      str_detect(dd, "–") ~ sub(".*–", "", dd),
      TRUE ~ dd
    )
  ) %>% 
  mutate(
    mm = match(mm, month.abb)
  ) %>% 
  mutate(
    date = ymd(paste0(yy, "-", mm, "-", dd))
  ) %>% 
  select(
    -c(dd:yy)
  ) %>% 
  relocate(date)
#-------------------------------------------------------------------------------


# MSE analysis for best spans
#-------------------------------------------------------------------------------

party_optimal_spans <- tibble(
  party = character(),
  span = numeric()
)

# MSE analysis for every party
parties <- unique(clean_polls$party)
for(p in parties){
  
  curr_party <- clean_polls %>% 
    filter(party == p) %>% 
    mutate(date = as.numeric(date)) %>% 
    na.omit()
  
  span_range <- seq(0.1, .99, by = .01)
  
  # Result tibble
  loess_tib <- tibble(
    span = numeric(),
    mse = numeric()
  )
  
  for(i in span_range){
    loess_temp <- loess(supp ~ date, data = curr_party, span = i)
    
    pred <- predict(loess_temp, newdata = curr_party)
    
    mse <- mean((curr_party$supp - pred)^2)
    
    loess_tib <- loess_tib %>% 
      add_row(span = i, mse = mse)
  }
  
  best_span <- loess_tib %>% 
    slice_min(mse, n = 1) %>% 
    pull(span)
  
  party_optimal_spans <- party_optimal_spans %>% 
    add_row(party = p, span = best_span)
}

# Best span: 0.1 for every party
#-------------------------------------------------------------------------------


# Get last LOESS predicted values for ggrepel
#-------------------------------------------------------------------------------
party_loess_vals <- tibble(
  party = character(),
  date = as.Date(character()),
  last_pred = numeric()
)

# Optimal span setting
optimal_span <- 0.1

parties <- unique(clean_polls$party)
for(p in parties){
  curr_ <- clean_polls %>% 
    filter(party == p) %>% 
    mutate(date_num = as.numeric(date)) %>% 
    na.omit()
  
  loess_temp <- loess(supp ~ date_num, data = curr_, span = optimal_span)
  
  y_hat <- predict(loess_temp, newdata = curr_)
  
  party <- p
  date <- as.Date(max(curr_$date))
  last_pred <- y_hat[1]
  
  party_loess_vals <- party_loess_vals %>% 
    add_row(party = party,
            date = date,
            last_pred = last_pred)
}

party_loess_vals <- party_loess_vals %>% 
  filter(party != "FW")

#-------------------------------------------------------------------------------


# Final plot
#-------------------------------------------------------------------------------
final_polls <- clean_polls %>% 
  filter(party != "FW")

# Custom party colors
party_colors <- c(AfD = '#009EE0', Union = '#161A1D', FDP = '#FFED00',
                  SPD = '#E3010F', Green = '#64A12D', Linke = '#BE3075',
                  BSW = '#784072')

# Label data for ggrepel
label_data <- final_polls %>%
  group_by(party) %>%
  filter(date == max(date, na.rm = TRUE)) %>% 
  filter(polling_firm == "INSA") %>% 
  select(-polling_firm)


ggplot(final_polls,
       aes(x = date, y = supp, color = party)) +
  geom_point(alpha = 0.2, na.rm = TRUE) +
  geom_smooth(span = .1, se = FALSE, lwd = 1.2, na.rm = TRUE) +
  scale_color_manual(values = party_colors) +
  theme_minimal() +
  scale_y_continuous(breaks = c(0, 10, 20, 30),
                     minor_breaks = c(5, 15, 25),
                     labels = scales::percent_format(scale = 1)) +
  ggthemes::geom_rangeframe(sides = "b", color = "black") +
  geom_vline(xintercept = as.Date("2022-02-24"), color = "red", lwd = 1,
             linetype = "dashed") +
  geom_vline(xintercept = as.Date("2022-08-22"), color = "red", lwd = 1,
             linetype = "dashed") +
  geom_vline(xintercept = as.Date("2023-10-07"), color = "red", lwd = 1,
             linetype = "dashed") +
  annotate("text", x = as.Date("2021-11-19"), y = 33,
           label = expression(italic("Russia invades ->\nUkraine")),
           size = 2.75, color = "red") +
  annotate("text", x = as.Date("2022-05-17"), y = 33,
           label = expression(italic("Peak electricity ->\nspot price")),
           size = 2.75, color = "red") +
  annotate("text", x = as.Date("2023-07-25"), y = 33,
           label = expression(italic("Gaza War ->\nbegins")),
           size = 2.75, color = "red") +
  labs(
    title = " Opinion polling for the next German federal election",
    x = "", y = "", color = "",
    caption = "Chart by Ben Albrecht\nSource: Wikipedia"
  ) +
  theme(
    axis.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x = element_line(size = 0.7),
    panel.grid.minor.y = element_blank(),
    legend.position = "none"
  ) +
  geom_text_repel(data = party_loess_vals, aes(x = date, y = last_pred, label = party),
                  nudge_x = 30, hjust = 0, direction = "y", show.legend = FALSE,
                  size = 4, segment.size = 1, segment.linetype = "dotted",
                  segment.curvature = -.1, segment.angle = 20, fontface = "bold")
  ggsave("german_polling.png")
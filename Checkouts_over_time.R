#Chart 1: Agatha Christie Book Checkouts Over Time
library("tidyverse")
library("dplyr")
library("ggplot2")

#Sort for Agatha Christie books
christie_overall <- spl_data %>%
  filter(str_detect(Creator, "Christie") == T) %>%
  filter(str_detect(Creator, "Agatha") == T) %>%
  mutate(total_date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))

#Convert date to date format
christie_overall$total_date <- as.Date(christie_overall$total_date, format = "%Y-%m-%d")

christie_titles <- christie_overall %>%
  
  #consolidate multiple editions
  mutate(Title = gsub(" /.*", "", christie_overall$Title)) %>%
  
  #find only standard physical books
  filter(str_detect(MaterialType, "^BOOK") == T) %>%
  filter(str_detect(Subjects, "Large type") == F) %>%
  
  #Find four specific titles
  filter(str_detect(Title, "Styles$") == T |
         str_detect(Title, "Orient Express$") == T |
         str_detect(Title, "none$") == T |
         str_detect(Title, "table$"))
  
  
#Line graph of selected book checkouts over time
ggplot(christie_titles) +
  geom_line(aes(x = total_date, y = Checkouts, color = Title)) +
  labs(title = "Agatha Christie Book Checkouts",
       subtitle = "2022-Jan 2023",
       x = "Month",
       y = "Number of Checkouts",
       color = "Book Title")
  
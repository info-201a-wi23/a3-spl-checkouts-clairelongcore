#Chart 1: Agatha Christie Book Checkouts Over Time
library("tidyverse")
library("dplyr")
library("ggplot2")

#Sort for Agatha Christie books
christie_overall <- spl_data %>%
  filter(str_detect(Creator, "Christie") == TRUE) %>%
  filter(str_detect(Creator, "Agatha") == TRUE) %>%
  mutate(total_date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))

#Convert date to date format
christie_overall$total_date <- as.Date(christie_overall$total_date,
                                       format = "%Y-%m-%d")

christie_titles <- christie_overall %>%

  #consolidate multiple editions
  mutate(Title = gsub(" /.*", "", christie_overall$Title)) %>%

  #find only standard physical books
  filter(str_detect(MaterialType, "^BOOK") == TRUE) %>%
  filter(str_detect(Subjects, "Large type") == FALSE) %>%

  #Find four specific titles
  filter(str_detect(Title, "Styles$") == TRUE |
         str_detect(Title, "Orient Express$") == TRUE |
         str_detect(Title, "none$") == TRUE |
         str_detect(Title, "table$"))

#Line graph of selected book checkouts over time
ggplot(christie_titles) +
  geom_line(aes(x = total_date, y = Checkouts, color = Title)) +
  labs(title = "Agatha Christie Book Checkouts",
       subtitle = "2022-Jan 2023",
       x = "Month",
       y = "Number of Checkouts",
       color = "Book Title")

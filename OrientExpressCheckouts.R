#Chart 2: "Murder on the Orient Express" Over Time
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

#Consolidate multiple editions
christie_spec_titles <- christie_overall %>%
  mutate(Title = gsub(": .*", "", christie_overall$Title))
christie_spec_titles <- christie_spec_titles %>%
  mutate(Title = gsub(" /.*", "", christie_spec_titles$Title))

  
#Find only unabridged versions of "Murder on the Orient Express"
orient_express <- christie_spec_titles %>%
  filter(Title %in% "Murder on the Orient Express") %>%
  filter(str_detect(Subjects, "Drama") == F)


#Graph checkouts of "Orient Express" over time
ggplot(orient_express) +
  geom_line(aes(x = total_date, y = Checkouts, color = MaterialType)) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Murder on the Orient Express Over Time",
         subtitle = "2022", 
    	   x = "Month",
         y = "Number of Checkouts",
    	   color = "Type of Material")

  




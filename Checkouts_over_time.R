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
       x = "Month",
       y = "Number of Checkouts",
       color = "Book Title")
  

#christie_by_title <- christie_spec_titles %>%
#  group_by(title) %>%
 # group_by(total_date) %>%
 # summarize(by_title = sum(Checkouts))


 # filter(Title %in% c("The Mysterious Affair at Styles", "And Then There Were None", "Murder on the Orient Express"))

#title_list <- c("The Mysterious Affair at Styles", "And Then There Were None", "Murder on the Orient Express", "The Murder of Roger Ackroyd")

# mutate(Title = gsub("Murder on the Orient Express", "Murder on the Orient Express", christie_spec_titles$Title)) %>%
#christie_spec_titles <- lapply(christie_spec_titles$Title, str_trim)




 # filter(tolower(christie_spec_titles$Title) %in% c("The Mysterious Affair at Styles", "And Then There Were None", "Murder on the Orient Express", "The Murder of Roger Ackroyd"))
  
#mutate(total_date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
#christie_data$total_date <- as.Date(christie_data$total_date, format = "%Y-%m-%d")





#ggplot(date_spl_data) +
#  geom_point(aes(x = total_date, y = Checkouts, color = UsageClass)) +
#  jitter(x = total_date, factor = 1, amount = .1)



#christie_spec_titles <- christie_overall %>%
#  mutate(Title = gsub(": .*", "", christie_overall$Title))
#christie_spec_titles <- christie_spec_titles %>%
# mutate(Title = gsub("/.*", "", christie_spec_titles$Title))
#christie_spec_titles <- christie_spec_titles %>%
#  mutate(Title = gsub("\\(.*", "", christie_spec_titles$Title))
#christie_spec_titles <- christie_spec_titles %>%
#  mutate(Title = gsub("mysterious affair", "Mysterious Affair", christie_spec_titles$Title))
#christie_spec_titles <- christie_spec_titles %>%
#  mutate(Title = gsub("And then there were none", "And Then There Were None", christie_spec_titles$Title))
#christie_spec_titles <- christie_spec_titles %>%
#  mutate(Title = gsub("murder", "Murder", christie_spec_titles$Title))
#christie_spec_titles <- christie_spec_titles %>%
#  mutate(Title = gsub(" $", "", christie_spec_titles$Title)) %>%

#author_df <- spl_df %>% 
 # filter(str_detect(Creator, "James")) %>% 
 # filter(str_detect(Creator, "Baldwin")) 
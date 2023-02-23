library("dplyr")
library("tidyverse")
library("ggplot2")


#Choose specific material types
selected_types <- spl_data %>%
  filter(MaterialType %in% c("BOOK", "AUDIOBOOK", "VIDEODISC", "EBOOK", "VISUAL", "ER, SOUNDREC", "ATLAS", "SOUNDDISC", "NOTATEDMUSIC"))

#Calculate average checkouts per item by medium
avg_checkout_by_type <- selected_types %>%
  group_by(MaterialType) %>%
  summarize(by_type = mean(Checkouts, na.rm = T))


#Bar chart of average checkouts per item by medium
ggplot(avg_checkout_by_type) +
  geom_col(mapping = aes(x = by_type, y = MaterialType, fill = MaterialType)) +
  scale_color_brewer(palette = "Paired") +
  labs(title = "Average Checkouts By Item Type",
       subtitle = "2022", 
       x = "Average Checkouts",
       y = "Type of Material", 
       fill = "Type of Material")
#Chart 3: Average Checkouts by Material Type
library("dplyr")
library("ggplot2")

#Choose specific material types
selected_types <- spl_data %>%
  filter(MaterialType %in% c("BOOK", "AUDIOBOOK", "VIDEODISC", "EBOOK",
                             "VISUAL", "ATLAS", "SOUNDDISC", "NOTATEDMUSIC"))

#Calculate average checkouts per item by medium
avg_checkout_by_type <- selected_types %>%
  group_by(MaterialType) %>%
  summarize(by_type = mean(Checkouts, na.rm = TRUE))

#Bar chart of average checkouts per item by medium
ggplot(avg_checkout_by_type) +
  geom_col(mapping = aes(x = by_type, y = MaterialType, fill = MaterialType)) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Average Checkouts By Item Type",
       subtitle = "2022-Jan 2023",
       x = "Average Checkouts",
       y = "Type of Material",
       fill = "Type of Material")

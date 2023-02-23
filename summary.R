rm(list = ls())
spl_data <- read.csv("2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

library("dplyr")
library("tidyverse")
library("ggplot2")


summary_info <- list()

summary_info$total_checkouts <- spl_data %>%
  summarize(sum_all_checkouts = sum(spl_data$Checkouts)) %>%
  select(sum_all_checkouts)

summary_info$max_checkouts <- spl_data %>%
  filter(Checkouts == max(Checkouts, na.rm = T)) %>%
  select(Title)

summary_info$max_checkout_month <- spl_data %>%
  group_by(CheckoutMonth) %>%
  summarize(checkout_by_month = sum(Checkouts)) %>%
  filter(checkout_by_month == max(checkout_by_month, na.rm = T)) %>%
  select(CheckoutMonth)

summary_info$average_audiobook <- spl_data %>%
  group_by(MaterialType) %>%
  summarize(avg_checkouts = mean(Checkouts, na.rm = T)) %>%
  filter(MaterialType == "AUDIOBOOK") %>%
  select(avg_checkouts)

summary_info$average_physical_book <- spl_data %>%
  group_by(MaterialType) %>%
  summarize(avg_checkouts = mean(Checkouts, na.rm = T)) %>%
  filter(MaterialType == "BOOK") %>%
  select(avg_checkouts)

summary_info$christie_max_checkouts <- spl_data %>%
  filter(str_detect(Creator, "Christie") == T) %>%
  filter(str_detect(Creator, "Agatha") == T) %>%
  filter(Checkouts == max(Checkouts)) %>%
  select(Checkouts)

summary_info$christie_max_title <- spl_data %>%
  filter(str_detect(Creator, "Agatha") == T) %>%
  group_by(Title) %>%
  summarize(total_both_years = sum(Checkouts)) %>%
  filter(total_both_years == max(total_both_years)) %>%
  select(Title)

summary_info$christie_max_format <- spl_data %>%
  filter(str_detect(Creator, "Agatha") == T) %>%
  group_by(MaterialType) %>%
  summarize(total_both_years = sum(Checkouts)) %>%
  filter(total_both_years == max(total_both_years)) %>%
  select(MaterialType)

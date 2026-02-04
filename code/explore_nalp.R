library(tidyverse)
library(janitor)
library(ggplot2)

load(file = "data/nalp/DS0001/26302-0001-Data.rda")

raw_data <- da26302.0001 %>% clean_names()

clean_data <- raw_data %>%
    select(
        gender = aq75,
        n_offers_public = aq39_1,
        n_offers_private = aq39_2,
        class_rank = aq61,
        usnews_rank = ausnews03
    ) %>%
    mutate(total_offers = n_offers_public + n_offers_private) %>%
    pivot_longer(names_to = "type", values_to = "n_offers", cols = c(n_offers_public, n_offers_private, total_offers)) %>%
    filter(!is.na(gender), !is.na(class_rank), !is.na(usnews_rank))

# histogram of offer distribution
ggplot(clean_data, aes(x = n_offers, fill = gender)) +
    geom_density(alpha = 0.5, adjust = 1.5) +
    facet_grid(rows = vars(type), scales = "free_y") +
    theme_classic() +
    scale_fill_brewer(palette = "Set2")

ggplot(clean_data %>% filter(type == "total_offers"), aes(x = n_offers, fill = gender)) +
    geom_density(alpha = 0.5, adjust = 1.5) +
    facet_grid(rows = vars(class_rank), scales = "free_y") +
    theme_classic() +
    scale_fill_brewer(palette = "Set2")


ggplot(clean_data %>% filter(type == "total_offers", str_detect(usnews_rank, "top")), aes(x = n_offers, fill = gender)) +
    geom_histogram(alpha = 0.5, position = "identity", bins = 10) +
    facet_grid(rows = vars(class_rank), scales = "free_y") +
    theme_classic() +
    scale_fill_brewer(palette = "Set2")

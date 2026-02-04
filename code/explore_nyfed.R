# ==============================================================================
# Explore NYFED Survey of Consumer Expectations (SCE) Job Search Data
# ==============================================================================
#
# Motivation: The "pigeonholing" hypothesis posits that employers sort workers
# into narrow subgroups based on observable characteristics (gender, education,
# region, industry) and that job offer arrival rates differ systematically
# across these subgroups. If pigeonholing operates, then comparing raw gender
# gaps in offer counts is misleading: the gap may reflect compositional
# differences rather than discrimination per se. We therefore condition on
# observables — education, region, industry — and examine whether gender gaps
# in offer counts persist *within* finely defined cells.
#
# This script performs exploratory analysis of the SCE data:
#   1. Loads and cleans the SCE 2013-2021 microdata
#   2. Filters to job searchers (active search or received an offer)
#   3. Tabulates subgroup sample sizes to assess feasibility
#   4. Produces proof-of-concept plots: weighted density of offer distributions
#      by gender, faceted by education x region and education x industry
#   5. Single-panel plots for Bachelor's+ in finance/prof services and
#      education/health sectors
# ==============================================================================

library(tidyverse)
library(janitor)
library(readxl)

# ---------- Step 1: Load and clean data ----------

raw_data <- read_xlsx("data/nyfed_sce/sce_13_21.xlsx", sheet = "Data") %>%
    clean_names()

# Select relevant variables with readable names
data <- raw_data %>%
    select(
        offers       = js19_offers_last4wks,
        weight       = survey_weight,
        male         = male,
        education    = education,
        region       = current_region,
        hh_income    = hh_income,
        race         = race,
        hispanic     = hispanic,
        industry     = ec1f_cps_job_industry_rc,
        looked_ne    = l5_ne_looked_for_work,
        looked_emp   = l6_emp_looked_for_work,
        search_days  = l7_days_spent_searching,
        offers_any   = js18_offers_any
    )

# ---------- Step 2: Filter to job searchers ----------

# Keep respondents who actively searched OR received at least one offer
searchers <- data %>%
    filter(
        looked_ne == 1 | looked_emp %in% c(1, 2) | (!is.na(offers) & offers > 0)
    ) %>%
    filter(!is.na(offers), !is.na(male), !is.na(education)) %>%
    filter(offers < 20)

# Recode gender for plotting
searchers <- searchers %>%
    mutate(
        gender = factor(ifelse(male == 1, "Men", "Women")),
        edu_label = factor(case_when(
            education %in% c(1, 2) ~ "HS or less",
            education %in% c(3, 4) ~ "Some college / Assoc.",
            education == 5 ~ "Bachelor's",
            education >= 6 ~ "Graduate+"
        ), levels = c("HS or less", "Some college / Assoc.", "Bachelor's", "Graduate+")),
        industry_macro = factor(case_when(
            industry %in% c(1, 2, 3, 4) ~ "Primary / Construction",
            industry == 5 ~ "Manufacturing",
            industry %in% c(6, 7, 8) ~ "Trade / Transport",
            industry %in% c(9, 10, 11, 12) ~ "Finance / Prof. Services",
            industry %in% c(13, 14) ~ "Education / Health",
            industry %in% c(15, 16, 17, 18) ~ "Leisure / Other Services",
            TRUE ~ NA_character_
        ))
    )

cat("\n=== Job searcher sample ===\n")
cat("Total job searchers:", nrow(searchers), "\n")
cat("Men:", sum(searchers$male == 1), " Women:", sum(searchers$male == 0), "\n\n")

# ---------- Step 3: Subgroup sample size analysis ----------

cat("=== Subgroup sample sizes ===\n\n")

# Education x Gender
edu_gender <- searchers %>%
    count(edu_label, gender) %>%
    pivot_wider(names_from = gender, values_from = n, values_fill = 0)
cat("--- Education x Gender ---\n")
print(as.data.frame(edu_gender))

# Education x Region x Gender
edu_region_gender <- searchers %>%
    filter(!is.na(region)) %>%
    count(edu_label, region, gender) %>%
    pivot_wider(names_from = gender, values_from = n, values_fill = 0)
cat("\n--- Education x Region x Gender ---\n")
print(as.data.frame(edu_region_gender))

# Education x Industry (macro) x Gender
edu_ind_gender <- searchers %>%
    filter(!is.na(industry_macro)) %>%
    count(edu_label, industry_macro, gender) %>%
    pivot_wider(names_from = gender, values_from = n, values_fill = 0)
cat("\n--- Education x Industry (macro) x Gender ---\n")
print(as.data.frame(edu_ind_gender))

# Save combined table
all_sizes <- bind_rows(
    edu_gender %>% mutate(facet = "edu_x_gender", subgroup = edu_label) %>%
        select(facet, subgroup, Men, Women),
    edu_region_gender %>% mutate(facet = "edu_x_region", subgroup = paste(edu_label, region, sep = " | ")) %>%
        select(facet, subgroup, Men, Women),
    edu_ind_gender %>% mutate(facet = "edu_x_industry", subgroup = paste(edu_label, industry_macro, sep = " | ")) %>%
        select(facet, subgroup, Men, Women)
)

write_csv(all_sizes, "figures/subgroup_sample_sizes.csv")
cat("\nSaved subgroup sample sizes to figures/subgroup_sample_sizes.csv\n")

# Flag cells below threshold
threshold <- 25
small_cells <- all_sizes %>% filter(Men < threshold | Women < threshold)
if (nrow(small_cells) > 0) {
    cat("\nWARNING: Cells with n <", threshold, ":\n")
    print(as.data.frame(small_cells))
} else {
    cat("\nAll cells have n >=", threshold, "\n")
}

# ---------- Step 4: Plot — Education x Region ----------

plot_edu_region <- searchers %>%
    filter(!is.na(region)) %>%
    ggplot(aes(x = offers, fill = gender, weight = weight)) +
    geom_density(alpha = 0.5, adjust = 1.5) +
    facet_grid(rows = vars(edu_label), cols = vars(region), scales = "free_y") +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal(base_family = "Palatino") +
    labs(
        title = "Job offers in last 4 weeks: Men vs. Women",
        subtitle = "By education level and region (NYFED SCE 2013-2021, weighted)",
        x = "Number of offers",
        y = "Density",
        fill = "Gender"
    ) +
    theme(
        strip.text = element_text(size = 8),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom"
    )

ggsave("figures/offers_by_edu_region.pdf", plot_edu_region, width = 12, height = 8)
cat("\nSaved figures/offers_by_edu_region.pdf\n")

# ---------- Step 5: Plot — Education x Industry ----------

plot_edu_industry <- searchers %>%
    filter(!is.na(industry_macro)) %>%
    ggplot(aes(x = offers, fill = gender, weight = weight)) +
    geom_density(alpha = 0.5, adjust = 1.5) +
    facet_grid(rows = vars(edu_label), cols = vars(industry_macro), scales = "free_y") +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal(base_family = "Palatino") +
    labs(
        title = "Job offers in last 4 weeks: Men vs. Women",
        subtitle = "By education level and industry (NYFED SCE 2013-2021, weighted)",
        x = "Number of offers",
        y = "Density",
        fill = "Gender"
    ) +
    theme(
        strip.text = element_text(size = 7),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom"
    )

ggsave("figures/offers_by_edu_industry.pdf", plot_edu_industry, width = 14, height = 8)
cat("Saved figures/offers_by_edu_industry.pdf\n")

# ---------- Step 6: Single-panel plots for Bachelor's+ subgroups ----------

ba_plus <- searchers %>%
    filter(education >= 5, !is.na(industry_macro))

plot_sector <- function(df, sector, filename) {
    plot_data <- df %>% filter(industry_macro == sector, offers > 0)
    n_men <- sum(plot_data$gender == "Men")
    n_women <- sum(plot_data$gender == "Women")

    p <- plot_data %>%
        ggplot(aes(x = offers, fill = gender, weight = weight)) +
        geom_density(alpha = 0.5, adjust = 1.5) +
        scale_fill_brewer(palette = "Set2") +
        scale_x_continuous(breaks = seq(0, 15, by = 1)) +
        theme_minimal(base_family = "Palatino") +
        labs(
            title = "Job offers: Men vs. Women",
            subtitle = paste0("Bachelor's+, ", sector, " (NYFED SCE 2013-2021, weighted)"),
            x = "Number of offers",
            y = "Density",
            fill = "Gender",
            caption = paste0("N = ", n_men + n_women, " (Men: ", n_men, ", Women: ", n_women, ")")
        ) +
        theme(
            plot.title = element_text(face = "bold"),
            legend.position = "bottom"
        )

    ggsave(filename, p, width = 7, height = 5)
    cat("Saved", filename, "\n")
}

plot_sector(ba_plus, "Finance / Prof. Services", "figures/offers_ba_finance.pdf")
plot_sector(ba_plus, "Education / Health", "figures/offers_ba_eduhealth.pdf")

cat("\nDone.\n")

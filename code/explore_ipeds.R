# ==============================================================================
# Explore IPEDS Admissions Data: Male vs. Female Admissions Rates and Yields
# ==============================================================================
#
# Motivation: The "pigeonholing" hypothesis predicts that while admissions rates
# may be similar for men and women, yield rates are systematically lower for
# women at selective universities — suggesting women receive offers but choose
# not to enroll (possibly due to fewer/worse options). If this pattern holds in
# higher education admissions, it provides an institutional-level analog to
# labor market pigeonholing: women face narrower effective choice sets even when
# raw acceptance rates appear comparable.
#
# This script performs exploratory analysis of IPEDS admissions data:
#   1. Loads and cleans IPEDS admissions microdata for 2024 and 2016
#   2. Produces scatter plots comparing male vs. female admissions rates and
#      yield rates across institutions, with a 45-degree reference line
#   3. Labels a set of elite/selective institutions for context
# ==============================================================================

library(tidyverse)
library(ggrepel)
library(patchwork)

# ---------- Labeled colleges ----------

label_colleges <- c(
    "Harvard University",
    "Yale University",
    "Princeton University",
    "Columbia University in the City of New York",
    "University of Pennsylvania",
    "Brown University",
    "Dartmouth College",
    "Cornell University",
    "Stanford University",
    "Massachusetts Institute of Technology",
    "Duke University",
    "California Institute of Technology",
    "University of Chicago",
    "Georgetown University",
    "Northwestern University"
)

# ---------- Helper: load IPEDS CSV ----------

load_ipeds <- function(filepath, year_label) {
    raw <- read_csv(filepath, show_col_types = FALSE)

    # Column name prefixes differ by year
    if (year_label == "2024") {
        adm_prefix <- "DRVADM2024"
        app_prefix <- "ADM2024"
    } else {
        adm_prefix <- "DRVADM2016_RV"
        app_prefix <- "ADM2016_RV"
    }

    df <- raw %>%
        select(
            institution = `institution name`,
            admit_rate_men = !!sym(paste0(adm_prefix, ".Percent admitted - men")),
            admit_rate_women = !!sym(paste0(adm_prefix, ".Percent admitted - women")),
            yield_men = !!sym(paste0(adm_prefix, ".Admissions yield - men")),
            yield_women = !!sym(paste0(adm_prefix, ".Admissions yield - women")),
            apps_men = !!sym(paste0(app_prefix, ".Applicants men")),
            apps_women = !!sym(paste0(app_prefix, ".Applicants women")),
            admitted_men = !!sym(paste0(app_prefix, ".Admissions men")),
            admitted_women = !!sym(paste0(app_prefix, ".Admissions women")),
            enrolled_men = !!sym(paste0(app_prefix, ".Enrolled  men")),
            enrolled_women = !!sym(paste0(app_prefix, ".Enrolled  women"))
        ) %>%
        mutate(across(admit_rate_men:enrolled_women, as.numeric)) %>%
        filter(
            !is.na(admit_rate_men), !is.na(admit_rate_women),
            !is.na(yield_men), !is.na(yield_women),
            !is.na(apps_men), !is.na(apps_women),
            !is.na(admitted_men), !is.na(admitted_women),
            !is.na(enrolled_men), !is.na(enrolled_women)
        ) %>%
        mutate(
            total_apps = apps_men + apps_women,
            pct_admitted_women = admitted_women / (admitted_men + admitted_women) * 100,
            pct_enrolled_women = enrolled_women / (enrolled_men + enrolled_women) * 100
        )

    cat(paste0("\nLoaded ", filepath, ": ", nrow(df), " institutions\n"))
    df
}

# ---------- Helper: plot admissions rate & yield scatter ----------

plot_ipeds <- function(df, year_label, label_colleges) {
    # Keep only selective institutions (sub-25% admissions rate)
    df <- df %>%
        filter(admit_rate_men < 25 & admit_rate_women < 25) %>%
        filter(total_apps > 5000) %>% # Focus on larger institutions
        mutate(
            label = ifelse(institution %in% label_colleges, institution, NA_character_),
            turndown_men = 100 - yield_men,
            turndown_women = 100 - yield_women
        )

    # Shared size scale so patchwork can merge the legend
    apps_range <- range(df$total_apps)

    p_admit <- ggplot(df, aes(x = admit_rate_men, y = admit_rate_women)) +
        geom_point(aes(size = total_apps), alpha = 0.5, color = "#2b8cbe") +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
        geom_smooth(method = "lm", se = TRUE, color = "#2b8cbe", linewidth = 0.7) +
        geom_text_repel(aes(label = label), size = 2.5, max.overlaps = 20) +
        scale_size_continuous(
            labels = scales::comma, range = c(1, 6), limits = apps_range,
            guide = guide_legend(override.aes = list(color = "grey40"))
        ) +
        expand_limits(x = 0, y = 0) +
        labs(
            x = "Male admissions rate (%)",
            y = "Female admissions rate (%)",
            size = "Total applicants"
        ) +
        theme_minimal(base_family = "Palatino") +
        theme(
            plot.title = element_text(face = "bold"),
            legend.position = "bottom"
        )

    p_turndown <- ggplot(df, aes(x = turndown_men, y = turndown_women)) +
        geom_point(aes(size = total_apps), alpha = 0.5, color = "#e34a33") +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
        geom_smooth(method = "lm", se = TRUE, color = "#e34a33", linewidth = 0.7) +
        geom_text_repel(aes(label = label), size = 2.5, max.overlaps = 20) +
        scale_size_continuous(
            labels = scales::comma, range = c(1, 6), limits = apps_range,
            guide = guide_legend(override.aes = list(color = "grey40"))
        ) +
        expand_limits(x = 0, y = 0) +
        labs(
            x = "Male turndown rate (%)",
            y = "Female turndown rate (%)",
            size = "Total applicants"
        ) +
        theme_minimal(base_family = "Palatino") +
        theme(
            plot.title = element_text(face = "bold"),
            legend.position = "bottom"
        )

    p_admit + p_turndown +
        plot_annotation(
            title = "Admissions rates and turndown rates: Men vs. Women",
            subtitle = paste0("IPEDS ", year_label, ", by institution"),
            theme = theme(
                plot.title = element_text(face = "bold", family = "Palatino"),
                plot.subtitle = element_text(family = "Palatino")
            )
        ) +
        plot_layout(guides = "collect") &
        theme(legend.position = "bottom")
}

# ---------- Helper: single-panel admissions vs enrollment share ----------

plot_ipeds_shares <- function(df, year_label, label_colleges) {
    # Keep only selective institutions (sub-10% admissions rate)
    df <- df %>%
        filter(admit_rate_men < 10 & admit_rate_women < 10) %>%
        filter(total_apps > 5000) %>% # Focus on larger institutions
        mutate(label = ifelse(institution %in% label_colleges, institution, NA_character_))

    ggplot(df, aes(x = pct_admitted_women, y = pct_enrolled_women)) +
        geom_point(aes(size = total_apps), alpha = 0.5, color = "#7570b3") +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
        geom_smooth(method = "lm", se = FALSE, color = "#7570b3", linewidth = 0.7) +
        geom_text_repel(aes(label = label), size = 2.5, max.overlaps = 20) +
        scale_size_continuous(
            labels = scales::comma,
            guide = guide_legend(override.aes = list(color = "grey40"))
        ) +
        labs(
            x = "Women as % of admitted students",
            y = "Women as % of enrolled students",
            size = "Total applicants"
        ) +
        theme_minimal(base_family = "Palatino") +
        theme(
            plot.title = element_text(face = "bold"),
            legend.position = "bottom"
        )
}

# ---------- Generate figures ----------

ipeds_24 <- load_ipeds("data/ipeds/adm24.csv", "2024")
ipeds_16 <- load_ipeds("data/ipeds/adm16.csv", "2016")

p24 <- plot_ipeds(ipeds_24, "2024", label_colleges)
ggsave("figures/ipeds_admit_yield_2024.pdf", p24, width = 8, height = 5)
cat("Saved figures/ipeds_admit_yield_2024.pdf\n")

p16 <- plot_ipeds(ipeds_16, "2016", label_colleges)
ggsave("figures/ipeds_admit_yield_2016.pdf", p16, width = 14, height = 7)
cat("Saved figures/ipeds_admit_yield_2016.pdf\n")

s24 <- plot_ipeds_shares(ipeds_24, "2024", label_colleges)
ggsave("figures/ipeds_shares_2024.pdf", s24, width = 8, height = 5)
cat("Saved figures/ipeds_shares_2024.pdf\n")

s16 <- plot_ipeds_shares(ipeds_16, "2016", label_colleges)
ggsave("figures/ipeds_shares_2016.pdf", s16, width = 9, height = 7)
cat("Saved figures/ipeds_shares_2016.pdf\n")

# ---------- Generate LaTeX table for labeled colleges ----------

# Short display names for the table
short_names <- c(
    "Harvard University" = "Harvard",
    "Yale University" = "Yale",
    "Princeton University" = "Princeton",
    "Columbia University in the City of New York" = "Columbia",
    "University of Pennsylvania" = "UPenn",
    "Brown University" = "Brown",
    "Dartmouth College" = "Dartmouth",
    "Cornell University" = "Cornell",
    "Stanford University" = "Stanford",
    "Massachusetts Institute of Technology" = "MIT",
    "Duke University" = "Duke",
    "California Institute of Technology" = "Caltech",
    "University of Chicago" = "UChicago",
    "Georgetown University" = "Georgetown",
    "Northwestern University" = "Northwestern"
)

make_table_df <- function(df, year_label) {
    df %>%
        filter(institution %in% label_colleges) %>%
        mutate(
            short_name = short_names[institution],
            turndown_men = 100 - yield_men,
            turndown_women = 100 - yield_women
        ) %>%
        select(
            short_name, admit_rate_men, admit_rate_women,
            turndown_men, turndown_women, total_apps
        ) %>%
        arrange(admit_rate_men) %>%
        mutate(year = year_label) %>%
        head(10)
}

tab <- bind_rows(
    make_table_df(ipeds_24, "2024")
)

# Build LaTeX file
tex_lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\caption{Admissions and turndown rates at selective institutions}",
    "\\label{tab:ipeds_selective}",
    "\\small",
    "\\begin{tabular}{l rr rr r}",
    "\\toprule",
    " & \\multicolumn{2}{c}{Admit Rate (\\%)} & \\multicolumn{2}{c}{Turndown Rate (\\%)} & \\\\",
    "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
    "Institution & Men & Women & Men & Women & Total Apps \\\\",
    "\\midrule"
)

for (yr in c("2024")) {
    tex_lines <- c(
        tex_lines, paste0("\\addlinespace[4pt]"),
        paste0("\\multicolumn{6}{l}{\\textbf{IPEDS ", yr, "}} \\\\"),
        paste0("\\addlinespace[2pt]")
    )
    yr_tab <- tab %>% filter(year == yr)
    for (i in seq_len(nrow(yr_tab))) {
        r <- yr_tab[i, ]
        tex_lines <- c(tex_lines, sprintf(
            "%s & %g & %g & %g & %g & %s \\\\",
            r$short_name, r$admit_rate_men, r$admit_rate_women,
            r$turndown_men, r$turndown_women,
            formatC(r$total_apps, format = "d", big.mark = ",")
        ))
    }
}

tex_lines <- c(
    tex_lines,
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}"
)

writeLines(tex_lines, "figures/ipeds_selective_table.tex")
cat("Saved figures/ipeds_selective_table.tex\n")

cat("\nDone.\n")


## Testing
ipeds_16 %>%
    filter(admit_rate_men < 10 & admit_rate_women < 10) %>%
    View()

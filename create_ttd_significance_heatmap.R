library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(meta)
library(ragg)

xlsx <- "C:/Users/86156/Nutstore/1/QoL_ICI/TTD meta analysis/BMC Medcine返修/2026.6.27/整合版数据6.23_TTD_新增n_event.xlsx"
out_dir <- "C:/Users/86156/Documents/Codex/2026-06-21/new-chat/work/significance_heatmap"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

dimension_map <- tibble::tribble(
  ~sheet, ~dimension,
  "QOL", "Global QoL",
  "PF", "Physical functioning",
  "EF", "Emotional functioning",
  "Fatigue", "Fatigue",
  "Pain", "Pain",
  "Appetite Loss", "Appetite loss",
  "NV", "Nausea/vomiting",
  "Dyspnea", "Dyspnea",
  "Couging", "Cough",
  "Hemoptysis", "Hemoptysis",
  "DY CO AND Pain in chest", "Composite symptom endpoint"
)

original_dimension_levels <- dimension_map$dimension
analysis_levels <- c("Overall ICI-based", "Immunochemotherapy", "ICI alone")
analysis_labels <- c(
  "Overall ICI-based" = "Overall\nICI-based",
  "ICI alone" = "ICI\nalone",
  "Immunochemotherapy" = "Immuno-\nchemotherapy"
)

read_dimension <- function(sheet_name) {
  df <- read_excel(xlsx, sheet = sheet_name)

  needed <- c("Study.ID", "Trial.name", "HR", "LCI", "UCI", "control_type", "treatment_type")
  for (nm in needed) {
    if (!nm %in% names(df)) df[[nm]] <- NA
  }

  dim_name <- dimension_map$dimension[match(sheet_name, dimension_map$sheet)]

  df %>%
    transmute(
      sheet = sheet_name,
      dimension = dim_name,
      Study.ID = as.character(Study.ID),
      Trial.name = as.character(Trial.name),
      treatment_type = as.character(treatment_type),
      control_type = as.character(control_type),
      HR = suppressWarnings(as.numeric(HR)),
      LCI = suppressWarnings(as.numeric(LCI)),
      UCI = suppressWarnings(as.numeric(UCI))
    ) %>%
    filter(control_type == "CT") %>%
    filter(!is.na(HR), !is.na(LCI), !is.na(UCI), HR > 0, LCI > 0, UCI > 0) %>%
    mutate(
      TE = log(HR),
      seTE = (log(UCI) - log(LCI)) / (2 * 1.96)
    ) %>%
    filter(is.finite(TE), is.finite(seTE), seTE > 0)
}

summarise_meta <- function(dat, analysis_name) {
  k <- nrow(dat)

  if (k == 0) {
    return(tibble(
      analysis = analysis_name, k = 0,
      HR = NA_real_, LCI = NA_real_, UCI = NA_real_,
      p_value = NA_real_, I2 = NA_real_,
      status = "No data"
    ))
  }

  if (k == 1) {
    z <- dat$TE[1] / dat$seTE[1]
    p <- 2 * pnorm(-abs(z))
    return(tibble(
      analysis = analysis_name, k = 1,
      HR = dat$HR[1], LCI = dat$LCI[1], UCI = dat$UCI[1],
      p_value = p, I2 = NA_real_,
      status = case_when(
        p < 0.05 & HR < 1 ~ "Significant benefit",
        p < 0.05 & HR > 1 ~ "Significant harm",
        TRUE ~ "Not significant"
      )
    ))
  }

  m <- meta::metagen(
    TE = TE,
    seTE = seTE,
    studlab = Trial.name,
    sm = "HR",
    method.tau = "DL",
    hakn = TRUE,
    fixed = FALSE,
    prediction = FALSE,
    data = dat
  )

  hr <- exp(m$TE.random)
  lci <- exp(m$lower.random)
  uci <- exp(m$upper.random)
  p <- m$pval.random

  tibble(
    analysis = analysis_name, k = k,
    HR = hr, LCI = lci, UCI = uci,
    p_value = p, I2 = m$I2 * 100,
    status = case_when(
      p < 0.05 & hr < 1 ~ "Significant benefit",
      p < 0.05 & hr > 1 ~ "Significant harm",
      TRUE ~ "Not significant"
    )
  )
}

all_rows <- lapply(dimension_map$sheet, read_dimension) %>%
  bind_rows()

results <- all_rows %>%
  group_by(dimension) %>%
  group_modify(function(.x, .y) {
    bind_rows(
      summarise_meta(.x, "Overall ICI-based"),
      summarise_meta(filter(.x, treatment_type == "IO"), "ICI alone"),
      summarise_meta(filter(.x, treatment_type == "IO+CT"), "Immunochemotherapy")
    )
  }) %>%
  ungroup()

plot_df <- expand_grid(
  dimension = original_dimension_levels,
  analysis = analysis_levels
) %>%
  left_join(results, by = c("dimension", "analysis")) %>%
  group_by(dimension) %>%
  mutate(n_significant_benefit = sum(status == "Significant benefit", na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    status = replace_na(status, "No data"),
    original_order = match(dimension, original_dimension_levels),
    sig_group = case_when(
      n_significant_benefit == 3 ~ "3 significant subgroups",
      n_significant_benefit == 2 ~ "2 significant subgroups",
      n_significant_benefit == 1 ~ "1 significant subgroup",
      TRUE ~ "0 significant subgroups"
    )
  ) %>%
  arrange(desc(n_significant_benefit), original_order) %>%
  mutate(
    dimension = factor(dimension, levels = rev(unique(dimension))),
    sig_group = factor(
      sig_group,
      levels = c("3 significant subgroups", "2 significant subgroups",
                 "1 significant subgroup", "0 significant subgroups")
    ),
    analysis = factor(analysis, levels = analysis_levels),
    status = factor(status, levels = c("Significant benefit", "Not significant", "Significant harm", "No data")),
    label = case_when(
      is.na(HR) ~ "",
      p_value < 0.001 ~ sprintf("%.2f***", HR),
      p_value < 0.01 ~ sprintf("%.2f**", HR),
      p_value < 0.05 ~ sprintf("%.2f*", HR),
      TRUE ~ sprintf("%.2f", HR)
    ),
    label_colour = if_else(status == "Significant benefit", "white", "#2B2B2B")
  )

write.csv(results %>% arrange(match(dimension, original_dimension_levels), match(analysis, analysis_levels)),
          file.path(out_dir, "ttd_significance_heatmap_results.csv"),
          row.names = FALSE,
          fileEncoding = "UTF-8")
write.csv(plot_df,
          file.path(out_dir, "ttd_significance_heatmap_matrix.csv"),
          row.names = FALSE,
          fileEncoding = "UTF-8")

palette_status <- c(
  "Significant benefit" = "#2B7FB8",
  "Not significant" = "#E8E8E8",
  "Significant harm" = "#C63D3D",
  "No data" = "#F7F7F7"
)

p <- ggplot(plot_df, aes(x = analysis, y = dimension, fill = status)) +
  geom_tile(color = "white", linewidth = 0.75, width = 0.94, height = 0.86) +
  geom_text(aes(label = label, colour = label_colour), size = 3.0, family = "Arial", fontface = "bold") +
  scale_colour_identity() +
  facet_grid(sig_group ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_fill_manual(
    values = palette_status,
    drop = TRUE,
    name = NULL,
    labels = c(
      "Significant benefit" = "Significant benefit",
      "Not significant" = "Not significant"
    )
  ) +
  scale_x_discrete(position = "top", expand = c(0, 0), labels = analysis_labels) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    x = NULL,
    y = NULL,
    title = "TTD treatment effects by PRO domain",
    subtitle = "Rows are ordered by the number of treatment subgroups with statistically significant delayed deterioration",
    caption = "Cell values are pooled HRs. *P < 0.05; **P < 0.01; ***P < 0.001.\nHR < 1 favors delayed deterioration with ICI-based therapy."
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 8, base_family = "Arial") +
  theme(
    plot.title = element_text(face = "bold", size = 10.2, hjust = 0, margin = margin(b = 2)),
    plot.subtitle = element_text(size = 6.5, color = "#4A4A4A", hjust = 0, margin = margin(b = 8)),
    plot.caption = element_text(size = 6.2, color = "#4A4A4A", hjust = 0, lineheight = 0.95, margin = margin(t = 8)),
    axis.text.x = element_text(size = 7.0, color = "#222222", face = "bold", lineheight = 0.9),
    axis.text.y = element_text(size = 7.0, color = "#222222"),
    panel.grid = element_blank(),
    panel.spacing.y = unit(1.8, "mm"),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 0, size = 6.3, face = "bold", color = "#555555", hjust = 0),
    legend.position = "right",
    legend.title = element_text(size = 7.2, face = "bold"),
    legend.text = element_text(size = 6.7),
    plot.margin = margin(8, 8, 8, 8)
  )

base <- file.path(out_dir, "ttd_significance_heatmap")

grDevices::svg(paste0(base, ".svg"), width = 7.8, height = 5.2, family = "Arial")
print(p)
dev.off()

grDevices::cairo_pdf(paste0(base, ".pdf"), width = 7.8, height = 5.2, family = "Arial")
print(p)
dev.off()

ragg::agg_tiff(paste0(base, ".tiff"), width = 7.8, height = 5.2, units = "in", res = 600, compression = "lzw")
print(p)
dev.off()

ragg::agg_png(paste0(base, ".png"), width = 7.8, height = 5.2, units = "in", res = 300)
print(p)
dev.off()

cat("Saved significance heatmap outputs to:", out_dir, "\n")

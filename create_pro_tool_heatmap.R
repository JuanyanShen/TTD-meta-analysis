library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ragg)

xlsx <- "C:/Users/86156/Nutstore/1/QoL_ICI/TTD meta analysis/BMC Medcine返修/2026.6.27/整合版数据6.23_TTD_新增n_event.xlsx"
out_dir <- "C:/Users/86156/Documents/Codex/2026-06-21/new-chat/work/pro_tool_heatmap"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

dimension_map <- tibble::tribble(
  ~sheet, ~dimension, ~default_tool,
  "QOL", "Global QoL", NA_character_,
  "PF", "Physical functioning", NA_character_,
  "EF", "Emotional functioning", "QLQ-C30",
  "Fatigue", "Fatigue", NA_character_,
  "Pain", "Pain", NA_character_,
  "Appetite Loss", "Appetite loss", NA_character_,
  "NV", "Nausea/vomiting", "QLQ-C30",
  "Dyspnea", "Dyspnea", NA_character_,
  "Couging", "Cough", NA_character_,
  "Hemoptysis", "Hemoptysis", NA_character_,
  "DY CO AND Pain in chest", "Composite symptom endpoint", "QLQ-C30/QLQ-LC13"
)

dimension_levels <- dimension_map$dimension

read_dimension <- function(sheet_name) {
  df <- read_excel(xlsx, sheet = sheet_name)

  needed <- c("Study.ID", "Trial.name", "HR", "LCI", "UCI", "control_type", "Tool")
  for (nm in needed) {
    if (!nm %in% names(df)) df[[nm]] <- NA
  }

  dim_info <- dimension_map %>% filter(sheet == sheet_name)

  df %>%
    transmute(
      sheet = sheet_name,
      dimension = dim_info$dimension,
      Study.ID = as.character(Study.ID),
      Trial.name = as.character(Trial.name),
      HR = suppressWarnings(as.numeric(HR)),
      LCI = suppressWarnings(as.numeric(LCI)),
      UCI = suppressWarnings(as.numeric(UCI)),
      control_type = as.character(control_type),
      Tool = as.character(Tool)
    ) %>%
    mutate(
      parent_trial_id = case_when(
        str_detect(Study.ID, "^No_5") ~ "No_5",
        str_detect(Study.ID, "^No_6") ~ "No_6",
        TRUE ~ Study.ID
      ),
      parent_trial_name = case_when(
        parent_trial_id == "No_5" ~ "POSEIDON",
        parent_trial_id == "No_6" ~ "MYSTIC",
        TRUE ~ Trial.name
      )
    ) %>%
    filter(!is.na(Study.ID), !is.na(Trial.name), control_type == "CT") %>%
    filter(!is.na(HR), !is.na(LCI), !is.na(UCI), HR > 0, LCI > 0, UCI > 0) %>%
    mutate(
      Tool = if_else(is.na(Tool) | Tool == "", dim_info$default_tool, Tool),
      Tool = case_when(
        is.na(Tool) | Tool == "" ~ "Not specified",
        Tool %in% c("EQ-5D-3L", "EQ-5D VAS") ~ "EQ-5D",
        TRUE ~ Tool
      )
    )
}

plot_df <- dimension_map$sheet %>%
  lapply(read_dimension) %>%
  bind_rows() %>%
  group_by(parent_trial_id, dimension) %>%
  summarise(
    Trial.name = parent_trial_name[which.max(nchar(parent_trial_name))],
    Study.ID = parent_trial_id[1],
    Tool = paste(sort(unique(Tool)), collapse = " / "),
    .groups = "drop"
  ) %>%
  mutate(
    row_key = parent_trial_id,
    Tool = if_else(str_detect(Tool, " / "), "Multiple instruments", Tool),
    tool_label = case_when(
      Tool == "QLQ-C30" ~ "C30",
      Tool == "QLQ-LC13" ~ "LC13",
      Tool == "LCSS" ~ "LCSS",
      Tool == "EQ-5D" ~ "EQ-5D",
      Tool == "QLQ-C30/QLQ-LC13" ~ "Mix",
      Tool == "Multiple instruments" ~ "Mixed",
      TRUE ~ ""
    )
  )

study_order <- plot_df %>%
  distinct(Study.ID, row_key) %>%
  mutate(
    study_num = suppressWarnings(as.numeric(str_extract(Study.ID, "\\d+"))),
    study_suffix = str_extract(Study.ID, "[A-Za-z]$")
  ) %>%
  arrange(study_num, study_suffix) %>%
  pull(row_key)

row_labels <- plot_df %>%
  group_by(row_key) %>%
  summarise(
    Trial.name = Trial.name[which.max(nchar(Trial.name))],
    .groups = "drop"
  ) %>%
  tibble::deframe()

grid_df <- expand_grid(
  row_key = study_order,
  dimension = dimension_levels
) %>%
  left_join(plot_df %>% select(row_key, dimension, Tool, tool_label),
            by = c("row_key", "dimension")) %>%
  mutate(
    dimension = factor(dimension, levels = dimension_levels),
    row_key = factor(row_key, levels = rev(study_order)),
    Tool = factor(
      replace_na(Tool, "Not reported/extracted"),
      levels = c("QLQ-C30", "QLQ-LC13", "LCSS", "EQ-5D", "QLQ-C30/QLQ-LC13",
                 "Multiple instruments", "Not specified", "Not reported/extracted")
    ),
    tool_label = replace_na(tool_label, "")
  )

source_data <- plot_df %>%
  arrange(match(row_key, study_order), match(dimension, dimension_levels))

write.csv(source_data,
          file.path(out_dir, "pro_tool_heatmap_source_data_long.csv"),
          row.names = FALSE,
          fileEncoding = "UTF-8")
write.csv(grid_df,
          file.path(out_dir, "pro_tool_heatmap_matrix_data.csv"),
          row.names = FALSE,
          fileEncoding = "UTF-8")

palette_tools <- c(
  "QLQ-C30" = "#6BAED6",
  "QLQ-LC13" = "#74C476",
  "LCSS" = "#FDAE6B",
  "EQ-5D" = "#9E9AC8",
  "QLQ-C30/QLQ-LC13" = "#9ECAE1",
  "Multiple instruments" = "#BDBDBD",
  "Not specified" = "#FDD0A2",
  "Not reported/extracted" = "#F3F3F3"
)

p <- ggplot(grid_df, aes(x = dimension, y = row_key, fill = Tool)) +
  geom_tile(color = "white", linewidth = 0.35, width = 0.96, height = 0.9) +
  geom_text(aes(label = tool_label), size = 2.05, color = "#1F1F1F", family = "Arial") +
  scale_fill_manual(values = palette_tools, drop = TRUE, name = "PRO instrument") +
  scale_x_discrete(position = "top", expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0), labels = row_labels) +
  labs(
    x = NULL,
    y = NULL,
    title = "Coverage of PRO time-to-deterioration dimensions by study and instrument"
  ) +
  coord_fixed(ratio = 0.7, clip = "off") +
  theme_minimal(base_size = 7, base_family = "Arial") +
  theme(
    plot.title = element_text(face = "bold", size = 8.5, hjust = 0, margin = margin(b = 8)),
    axis.text.x = element_text(angle = 45, hjust = 0, vjust = 0, size = 6.2, color = "#222222"),
    axis.text.y = element_text(size = 6.2, color = "#222222"),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 6.8, face = "bold"),
    legend.text = element_text(size = 6.2),
    plot.margin = margin(8, 8, 8, 8)
  )

base <- file.path(out_dir, "pro_tool_dimension_heatmap")

grDevices::svg(paste0(base, ".svg"), width = 9.2, height = 5.8, family = "Arial")
print(p)
dev.off()

grDevices::cairo_pdf(paste0(base, ".pdf"), width = 9.2, height = 5.8, family = "Arial")
print(p)
dev.off()

ragg::agg_tiff(paste0(base, ".tiff"), width = 9.2, height = 5.8, units = "in", res = 600, compression = "lzw")
print(p)
dev.off()

ragg::agg_png(paste0(base, ".png"), width = 9.2, height = 5.8, units = "in", res = 300)
print(p)
dev.off()

cat("Saved heatmap outputs to:", out_dir, "\n")
cat("Studies:", length(study_order), "Dimensions:", length(dimension_levels), "\n")

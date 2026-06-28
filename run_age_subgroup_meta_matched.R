library(readxl)
library(openxlsx)
library(dplyr)
library(metafor)

base_dir <- "C:/Users/86156/Documents/Codex/2026-06-21/new-chat/work/age_subgroup_matched"
input_xlsx <- file.path(base_dir, "matched_age_input.xlsx")
output_xlsx <- file.path(base_dir, "age_subgroup_meta_table.xlsx")
output_clean_xlsx <- file.path(base_dir, "age_subgroup_meta_table_clean.xlsx")

normalize_age_group <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("≤", "<=", x, fixed = TRUE)
  x <- gsub("≥", ">=", x, fixed = TRUE)
  x <- gsub("\\s+", "", x)
  dplyr::case_when(
    x %in% c("<65", "＜65") ~ "<65",
    x %in% c(">=65", "≥65", "=>65", "65+") ~ ">=65",
    suppressWarnings(as.numeric(x)) < 65 ~ "<65",
    suppressWarnings(as.numeric(x)) >= 65 ~ ">=65",
    TRUE ~ NA_character_
  )
}

clean_names_from_first_row <- function(dat) {
  names(dat) <- as.character(unlist(dat[1, ]))
  dat <- dat[-1, , drop = FALSE]
  names(dat) <- make.names(names(dat), unique = TRUE)
  dat
}

pool_one <- function(dat) {
  dat <- dat %>% filter(!is.na(yi), !is.na(vi), vi > 0)
  k <- nrow(dat)
  if (k == 0) {
    return(data.frame(k = 0, HR = NA_real_, LCI = NA_real_, UCI = NA_real_,
                      p_value = NA_real_, I2 = NA_real_, tau2 = NA_real_))
  }
  if (k == 1) {
    se <- sqrt(dat$vi[1])
    z <- dat$yi[1] / se
    return(data.frame(
      k = 1,
      HR = exp(dat$yi[1]),
      LCI = exp(dat$yi[1] - 1.96 * se),
      UCI = exp(dat$yi[1] + 1.96 * se),
      p_value = 2 * pnorm(abs(z), lower.tail = FALSE),
      I2 = NA_real_,
      tau2 = NA_real_
    ))
  }
  fit <- rma.uni(yi = yi, vi = vi, data = dat, method = "REML")
  data.frame(
    k = k,
    HR = exp(as.numeric(fit$b)),
    LCI = exp(fit$ci.lb),
    UCI = exp(fit$ci.ub),
    p_value = fit$pval,
    I2 = fit$I2,
    tau2 = fit$tau2
  )
}

subgroup_p <- function(dat) {
  dat <- dat %>% filter(!is.na(yi), !is.na(vi), vi > 0, !is.na(age_group))
  if (n_distinct(dat$age_group) < 2 || nrow(dat) < 3) return(NA_real_)
  dat$age_group <- factor(dat$age_group, levels = c("<65", ">=65"))
  fit <- tryCatch(
    rma.uni(yi = yi, vi = vi, mods = ~ age_group, data = dat, method = "REML"),
    error = function(e) NULL
  )
  if (is.null(fit) || length(fit$pval) < 2) return(NA_real_)
  fit$pval[2]
}

fmt_p <- function(p) {
  ifelse(is.na(p), NA_character_, ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
}

sheets <- excel_sheets(input_xlsx)
summary_list <- list()
used_list <- list()
excluded_list <- list()

domain_labels <- c(
  "Dyspnea" = "Dyspnea",
  "Couging" = "Cough",
  "Hemoptysis" = "Hemoptysis",
  "DY CO AND Pain in chest" = "Composite symptom endpoint",
  "Appetite Loss" = "Appetite loss",
  "Pain" = "Pain",
  "PF" = "Physical functioning",
  "Fatigue" = "Fatigue",
  "NV" = "Nausea/vomiting",
  "QOL" = "Global HRQoL",
  "EF" = "Emotional functioning"
)

for (sheet in sheets) {
  raw <- read_excel(input_xlsx, sheet = sheet, col_names = FALSE, .name_repair = "minimal")
  if (nrow(raw) < 2) next
  dat <- clean_names_from_first_row(raw)

  if (!all(c("HR", "LCI", "UCI", "Age") %in% names(dat))) {
    warning("Skipping sheet without HR/LCI/UCI/Age: ", sheet)
    next
  }

  dat <- dat %>%
    mutate(
      outcome = sheet,
      HR = suppressWarnings(as.numeric(HR)),
      LCI = suppressWarnings(as.numeric(LCI)),
      UCI = suppressWarnings(as.numeric(UCI)),
      age_group = normalize_age_group(Age),
      yi = log(HR),
      sei = (log(UCI) - log(LCI)) / (2 * 1.96),
      vi = sei^2
    )

  valid <- dat %>%
    filter(!is.na(HR), !is.na(LCI), !is.na(UCI), HR > 0, LCI > 0, UCI > 0, !is.na(age_group))

  excluded_list[[sheet]] <- dat %>%
    filter(is.na(HR) | is.na(LCI) | is.na(UCI) | is.na(age_group)) %>%
    select(any_of(c("outcome", "Trial.name", "Study.ID", "First.Author", "Year",
                    "HR", "LCI", "UCI", "Age", "age_group")))

  used_list[[sheet]] <- valid %>%
    select(any_of(c("outcome", "Trial.name", "Study.ID", "First.Author", "Year",
                    "treatment_type", "treatment_type1", "E", "C", "HR", "LCI", "UCI",
                    "Age", "age_group", "yi", "sei", "vi")))

  p_sub <- subgroup_p(valid)
  for (grp in c("<65", ">=65")) {
    pooled <- pool_one(valid %>% filter(age_group == grp))
    summary_list[[length(summary_list) + 1]] <- cbind(
      data.frame(PRO_domain = sheet, Age_subgroup = grp),
      pooled,
      data.frame(P_for_subgroup_difference = p_sub)
    )
  }
}

summary_tbl <- bind_rows(summary_list) %>%
  mutate(
    `PRO domain` = unname(ifelse(PRO_domain %in% names(domain_labels), domain_labels[PRO_domain], PRO_domain)),
    `Pooled HR (95% CI)` = ifelse(
      is.na(HR), NA_character_, sprintf("%.2f (%.2f-%.2f)", HR, LCI, UCI)
    ),
    `P value` = fmt_p(p_value),
    `I2 (%)` = ifelse(is.na(I2), NA_character_, sprintf("%.1f", I2)),
    `P for subgroup difference` = fmt_p(P_for_subgroup_difference)
  ) %>%
  select(
    `PRO domain`, PRO_domain, Age_subgroup, k, `Pooled HR (95% CI)`,
    `P value`, `I2 (%)`, `P for subgroup difference`,
    HR, LCI, UCI, p_value, I2, tau2, P_for_subgroup_difference
  )

used_tbl <- bind_rows(used_list)
excluded_tbl <- bind_rows(excluded_list)

wide_tbl <- summary_tbl %>%
  select(`PRO domain`, PRO_domain, Age_subgroup, k, `Pooled HR (95% CI)`, `P value`, `I2 (%)`, `P for subgroup difference`) %>%
  tidyr::pivot_wider(
    names_from = Age_subgroup,
    values_from = c(k, `Pooled HR (95% CI)`, `P value`, `I2 (%)`),
    names_glue = "{.value} {Age_subgroup}"
  ) %>%
  select(`PRO domain`, PRO_domain, `k <65`, `Pooled HR (95% CI) <65`, `P value <65`, `I2 (%) <65`,
         `k >=65`, `Pooled HR (95% CI) >=65`, `P value >=65`, `I2 (%) >=65`,
         `P for subgroup difference`)

wb <- createWorkbook()
addWorksheet(wb, "Age subgroup table")
writeData(wb, "Age subgroup table", wide_tbl)
addWorksheet(wb, "Long format results")
writeData(wb, "Long format results", summary_tbl)
addWorksheet(wb, "Data used")
writeData(wb, "Data used", used_tbl)
addWorksheet(wb, "Excluded rows")
writeData(wb, "Excluded rows", excluded_tbl)

header_style <- createStyle(textDecoration = "bold", fgFill = "#D9EAF7", border = "Bottom",
                            halign = "center", valign = "center")
body_style <- createStyle(valign = "center", border = "Bottom", borderColour = "#E6E6E6")
note_style <- createStyle(fontColour = "#666666", textDecoration = "italic")

for (sh in names(wb)) {
  addStyle(wb, sh, header_style, rows = 1, cols = 1:50, gridExpand = TRUE)
  freezePane(wb, sh, firstRow = TRUE)
  setColWidths(wb, sh, cols = 1:50, widths = "auto")
}
addStyle(wb, "Age subgroup table", body_style, rows = 2:(nrow(wide_tbl) + 1), cols = 1:ncol(wide_tbl), gridExpand = TRUE)
setColWidths(wb, "Age subgroup table", cols = 1, widths = 28)
setColWidths(wb, "Age subgroup table", cols = 2, widths = 24)
setColWidths(wb, "Age subgroup table", cols = 3:ncol(wide_tbl), widths = 18)

addWorksheet(wb, "Notes")
notes <- data.frame(
  Item = c("Age grouping", "Model", "Interpretation"),
  Description = c(
    "Age was classified according to the Age column in the source workbook: <65 vs >=65.",
    "Log HRs were pooled within each subgroup using a random-effects model with REML estimation. For subgroup difference, age group was fitted as a study-level moderator.",
    "This is an exploratory comparison-level subgroup meta-analysis based on aggregate trial/comparison data, not an individual patient-level age subgroup analysis."
  )
)
writeData(wb, "Notes", notes)
addStyle(wb, "Notes", header_style, rows = 1, cols = 1:2, gridExpand = TRUE)
addStyle(wb, "Notes", note_style, rows = 2:4, cols = 1:2, gridExpand = TRUE)
setColWidths(wb, "Notes", cols = 1, widths = 22)
setColWidths(wb, "Notes", cols = 2, widths = 110)

saveWorkbook(wb, output_xlsx, overwrite = TRUE)
writexl::write_xlsx(
  list(
    "Age subgroup table" = wide_tbl,
    "Long format results" = summary_tbl,
    "Data used" = used_tbl,
    "Excluded rows" = excluded_tbl,
    "Notes" = notes
  ),
  path = output_clean_xlsx
)
cat("Saved:", output_xlsx, "\n")
cat("Saved clean:", output_clean_xlsx, "\n")
print(wide_tbl)

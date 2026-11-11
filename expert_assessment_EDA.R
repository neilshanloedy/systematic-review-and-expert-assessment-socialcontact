# ── 0. Setup ───────────────────────────────────────────────────────────────────
rm(list = ls())
set.seed(123)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  data.table, dplyr, tidyr, stringr, purrr, tibble, forcats,
  ggplot2, ggrepel, scales, rstudioapi, patchwork, polycor
)
select <- dplyr::select
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

cfg <- list(
  col_pal  = c("#ff0a12","#ff7300","#FFCC00","#90fe00",
               "#00a8e8","#0466c8","#8400ff","#ff00a1"),
  fig_dir  = "figures",
  dpi      = 450
)

# Category order — single source of truth used everywhere
category_map <- list(
  "Demographic factors"        = as.character(1:6),
  "SES factors"                = as.character(7:10),
  "Household composition"      = as.character(11:18),
  "Health status"              = as.character(19:23),
  "Other (participant) questions" = as.character(24:28),
  "Contact features"           = as.character(29:41),
  "Additional contacts"        = c("42","42a","42b","42c","42d"),
  "Info. about the survey day" = as.character(43:50)
)
cat_levels <- names(category_map)

# ── 1. Helper functions ────────────────────────────────────────────────────────
save_fig <- function(plot, filename, width, height,
                     dir = cfg$fig_dir, dpi = cfg$dpi) {
  path <- file.path(dir, filename)
  if (!file.exists(path)) {
    ggsave(path, plot = plot, width = width, height = height,
           units = "in", dpi = dpi)
    message("Saved: ", path)
  } else {
    message("Skipped (exists): ", path)
  }
}

nature_theme <- function(base_size = 10) {
  theme_classic(base_size = base_size, base_family = "Helvetica") +
    theme(
      strip.text         = element_text(face = "bold", size = base_size * 0.95,
                                        hjust = 0),
      strip.background   = element_blank(),
      axis.line          = element_line(color = "grey50", linewidth = 0.35),
      axis.ticks         = element_line(color = "grey50", linewidth = 0.25),
      panel.grid.major.x = element_line(color = "grey93", linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      plot.margin        = margin(10, 15, 10, 10)
    )
}

summarise_metric <- function(data, metric_name, n_experts = 26, B = 10000) {
  data %>%
    group_by(questions, Category) %>%
    summarise(
      vals = list(!!sym(metric_name)),
      p3   = round(mean(!!sym(metric_name) == 3, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) %>%
    mutate(ci = map(vals, ci_bootstrap, B = B)) %>%
    mutate(
      val_mean  = round(map_dbl(ci, "mean"),  2),
      ci_lower  = round(map_dbl(ci, "lower"), 2),
      ci_upper  = round(map_dbl(ci, "upper"), 2),
      q_wrapped = str_wrap(questions, width = 28)
    ) %>%
    select(-vals, -ci) %>%
    arrange(Category, val_mean) %>%
    mutate(q_wrapped = fct_inorder(q_wrapped))
}

analyze_metric_priority <- function(data, metric_name, n_experts = 26) {
  avg <- summarise_metric(data, metric_name, n_experts)
  cat("\n---", toupper(metric_name), "---\n")
  print(avg %>% select(Category, questions, val_mean, ci_lower, ci_upper))
  invisible(avg)
}

analyze_metric_priority <- function(data,
                                    metric_name = "Importance",
                                    n_experts   = 26,
                                    cat_order   = NULL) {

  avg_data <- data %>%
    group_by(questions, Category) %>%
    summarise(
      val_mean = round(mean(!!sym(metric_name), na.rm = TRUE), 2),
      val_sd   = round(sd(!!sym(metric_name),   na.rm = TRUE), 2),
      .groups  = "drop"
    ) %>%
    mutate(
      se         = val_sd / sqrt(n_experts),
      t_critical = qt(0.975, df = n_experts - 1),
      ci_lower   = round(val_mean - t_critical * se, 2),
      ci_upper   = round(val_mean + t_critical * se, 2),
      questions_wrapped = str_wrap(questions, width = 28)
    ) %>%
    arrange(Category, val_mean) %>%
    mutate(questions_wrapped = fct_inorder(questions_wrapped))

  if (!is.null(cat_order)) {
    avg_data$Category <- factor(avg_data$Category, levels = cat_order)
  }

  cat(paste0("\n--- ", toupper(metric_name), " ANALYSIS ---\n"))
  print(avg_data %>% select(Category, questions, val_mean, ci_lower, ci_upper))

  p <- ggplot(avg_data,
              aes(x = val_mean,
                  y = questions_wrapped)) +
    geom_segment(
      aes(x    = 0.5,
          xend = val_mean,
          y    = questions_wrapped,
          yend = questions_wrapped),
      color     = "grey88",
      linewidth = 0.3
    ) +
    geom_errorbarh(
      aes(xmin = ci_lower, xmax = ci_upper),
      height    = 0.25,
      linewidth = 0.45,
      color     = "grey55"
    ) +
    geom_point(size = 2.6, color = "#2166AC", alpha = 0.92) +
    geom_text(
      aes(x     = ci_upper,
          label = sprintf("%.2f [%.2f\u2013%.2f]",
                          val_mean, ci_lower, ci_upper)),
      hjust    = -0.12,
      size     = 2.2,
      color    = "grey10",
      family   = "Helvetica",
      fontface = "plain"
    ) +
    scale_x_continuous(
      limits = c(0.5, 4),
      breaks = seq(0.5, 4, 0.5),
      expand = expansion(mult = c(0, 0.18))
    ) +
    facet_wrap(~ Category, scales = "free_y", ncol = 3) +
    labs(
      x       = paste0("Mean ", tolower(metric_name), " score (95% CI)"),
      y       = NULL
    ) +
    theme_classic(base_size = 12, base_family = "Helvetica") +
    theme(
      strip.text         = element_text(face = "bold", size = 9.5, hjust = 0),
      strip.background   = element_blank(),
      axis.text.y        = element_text(size = 8.5, color = "grey20", lineheight = 0.9),
      axis.text.x        = element_text(size = 9,   color = "grey20"),
      axis.title.x       = element_text(size = 10,   margin = margin(t = 6)),
      axis.line          = element_line(color = "grey50", linewidth = 0.35),
      axis.ticks         = element_line(color = "grey50", linewidth = 0.25),
      axis.ticks.y       = element_blank(),
      panel.grid.major.x = element_line(color = "grey93", linewidth = 0.3),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.spacing      = unit(1.4, "lines"),
      plot.caption       = element_text(size = 7, color = "grey50", hjust = 0,
                                        lineheight = 1.35, margin = margin(t = 10)),
      plot.margin        = margin(10, 15, 10, 10)
    )

  n_q    <- n_distinct(avg_data$questions)
  n_cats <- n_distinct(avg_data$Category)
  n_rows <- ceiling(n_cats / 3)
  fig_h  <- max(100, (n_q / n_cats) * n_rows * 6 + 35)

  filename <- paste0("figures/figure_", tolower(metric_name))

  ggsave(paste0(filename, ".png"),
         plot = p, width = 350, height = fig_h,
         units = "mm", dpi = 300)

  message("Saved: ", filename, ".pdf / .png  (", round(fig_h), " mm tall)")

  return(list(summary = avg_data, plot = p))
}

make_forest_plot <- function(data, metric_name, x_label) {
  summary_df <- summarise_metric(data, metric_name) %>%
    mutate(
      label = sprintf("%.2f [%.2f–%.2f]", val_mean, ci_lower, ci_upper),
      q_display = str_to_sentence(
        str_replace_all(as.character(q_wrapped), "_", " ")
      ),
      q_display = factor(
        q_display,
        levels = q_display[order(val_mean)]
      )
    )

  summary_df <- summary_df %>%
    mutate(label = sprintf("%.2f [%.2f–%.2f]", val_mean, ci_lower, ci_upper))

  ggplot(summary_df, aes(x = val_mean, y = q_display)) +
    geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper),
                   height = 0, linewidth = 0.5, color = "grey30") +
    geom_point(size = 2, color = "#2166AC") +
    geom_text(aes(label = label, x = ci_upper + 0.25), hjust = 0, size = 2.8, color = "grey20") +
    scale_x_continuous(limits = c(0.5, 4.55), breaks = c(0.5,1,1.5,2,2.5,3,3.5,4)) +
    facet_wrap(~ Category, scales = "free_y", ncol = 2) +
    labs(x = x_label, y = NULL) +
    nature_theme(base_size = 10) +
    theme(
      strip.text         = element_text(face = "bold", size = 11, hjust = 0),
      axis.text.y        = element_text(size = 9),
      axis.text.x        = element_text(size = 9),
      axis.title.x       = element_text(size = 12, margin = margin(t = 6)),
      panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3)
    )
}

build_supplementary_table <- function(data, expert_imp_avg) {
  proportions <- data %>%
    group_by(questions, Category) %>%
    summarise(
      pct_medium = round(mean(Importance == 2, na.rm = TRUE) * 100, 1),
      pct_high   = round(mean(Importance == 3, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    )

  medians <- data %>%
    group_by(questions, Category) %>%
    summarise(val_median = median(Importance, na.rm = TRUE), .groups = "drop")

  proportions %>%
    left_join(
      expert_imp_avg %>% select(questions, Category, priority_status, val_mean = mean_Importance),
      by = c("questions", "Category")
    ) %>%
    left_join(medians, by = c("questions", "Category")) %>%
    mutate(
      # stage-2-only median reclassification, restricted to retained items
      class_median_stage2 = case_when(
        priority_status == "Optional" ~ NA_character_,   # stage 1 unaffected by median
        val_median >= 3                 ~ "Core",
        TRUE                             ~ "Secondary"
      )
    ) %>%
    select(-val_mean) %>%
    rename(
      Item                       = questions,
      `% rating Medium`          = pct_medium,
      `% rating High`            = pct_high,
      `Classification`           = priority_status,
      `Classification (median)`  = class_median_stage2
    ) %>%
    arrange(Category, desc(`% rating High`))
}

# nonparametric percentile bootstrap ─────────────────────────────
ci_bootstrap <- function(x, B = 10000) {
  x <- x[!is.na(x)]
  n <- length(x)
  boot_means <- replicate(B, mean(sample(x, n, replace = TRUE)))
  m <- mean(x)
  qs <- quantile(boot_means, c(0.025, 0.975))
  c(mean = m, lower = unname(qs[1]), upper = unname(qs[2]), n = n)
}

expert_average_cat_bootstrap <- function(data, cat_levels) {
  data %>%
    group_by(Category) %>%
    summarise(
      n = n(),
      imp_ci  = list(ci_bootstrap(Importance)),
      burd_ci = list(ci_bootstrap(Burden)),
      acc_ci  = list(ci_bootstrap(Accuracy)),
      mean_pct_underexplored = round(mean(Underexplored, na.rm = TRUE) * 100, 2),
      .groups = "drop"
    ) %>%
    mutate(
      mean_Importance = round(map_dbl(imp_ci, "mean"), 2),
      ci_lo_Importance = round(map_dbl(imp_ci, "lower"), 2),
      ci_hi_Importance = round(map_dbl(imp_ci, "upper"), 2),
      mean_Burden = round(map_dbl(burd_ci, "mean"), 2),
      ci_lo_Burden = round(map_dbl(burd_ci, "lower"), 2),
      ci_hi_Burden = round(map_dbl(burd_ci, "upper"), 2),
      mean_Accuracy = round(map_dbl(acc_ci, "mean"), 2),
      ci_lo_Accuracy = round(map_dbl(acc_ci, "lower"), 2),
      ci_hi_Accuracy = round(map_dbl(acc_ci, "upper"), 2)
    ) %>%
    select(-imp_ci, -burd_ci, -acc_ci, -n) %>%
    mutate(Category = factor(Category, levels = cat_levels)) %>%
    arrange(mean_Importance)
}

ci_bootstrap_binary <- function(x, B = 10000) {
  x <- x[!is.na(x)]
  n <- length(x)
  p_hat <- mean(x)
  boot_p <- replicate(B, mean(sample(x, n, replace = TRUE)))
  qs <- quantile(boot_p, c(0.025, 0.975))
  c(p = p_hat, lower = unname(qs[1]), upper = unname(qs[2]))
}

# ── 2. Data ingestion & cleaning ───────────────────────────────────────────────
expert_raw <- read.csv(
  "data/response_of_expert_assessment_27042026.csv",
  na.strings = ""
)

drop_cols   <- c("Informed_consent", "Email_address",
                 "Additional_comments", "Additional_locations")
questions_row <- expert_raw[1, ] %>% select(-all_of(drop_cols))
expert_data   <- expert_raw[-1, ] %>% select(-all_of(drop_cols))

N_experts <- nrow(expert_data)

# ── 3. Category dictionary ────────────────────────────────────────────────────
lookup_df <- enframe(category_map, name = "Category", value = "Q_Suffix") %>%
  unnest(Q_Suffix) %>%
  mutate(Q_Prefix = paste0("Q", Q_Suffix, ":"))

question_dictionary <- as.data.frame(t(questions_row)) %>%
  rownames_to_column("Column_Name") %>%
  rename(Full_Text = 2) %>%
  mutate(Q_ID = str_extract(Full_Text, "Q\\d+[a-z]*:")) %>%
  right_join(lookup_df, by = c("Q_ID" = "Q_Prefix")) %>%
  mutate(clean_var = str_remove(Column_Name,
                                "^(Importance_|Burden_|Accuracy_|Underexplored_)")) %>%
  select(clean_var, Category) %>%
  distinct()

# ── 4. Reshape & recode ───────────────────────────────────────────────────────
likert_recode <- c("High" = 3, "Medium" = 2, "Low" = 1)

expert_numeric <- expert_data %>%
  pivot_longer(
    cols         = starts_with(c("Importance","Burden","Accuracy","Underexplored")),
    names_pattern = "^([^_]+)_(.*)",
    names_to      = c(".value", "questions")
  ) %>%
  left_join(question_dictionary, by = c("questions" = "clean_var")) %>%
  mutate(
    across(c(Importance, Burden, Accuracy),
           ~ likert_recode[.]),
    Underexplored = as.integer(Underexplored == "Yes"),
    Category      = factor(Category, levels = cat_levels)
  )

# ── 5. Correlation checks ─────────────────────────────────────────────────────
polychor_boot <- function(a, b, B = 1000) {
  est <- polychor(expert_numeric[[a]], expert_numeric[[b]])
  bs  <- replicate(B, tryCatch({
    d <- dplyr::bind_rows(sample(by_id, replace = TRUE))
    polychor(d[[a]], d[[b]])
  }, error = function(e) NA_real_))
  ci <- quantile(bs, c(.025, .975), na.rm = TRUE)
  message(a, " vs ", b, ": rho = ", round(est, 3),
          ", 95% cluster-bootstrap CI [", round(ci[1], 3), ", ", round(ci[2], 3), "]")
}


metric_pairs <- list(
  c("Importance", "Burden"),
  c("Importance", "Accuracy"),
  c("Burden",     "Accuracy"),
  c("Importance", "Underexplored")
)

# Asymptotic-test
walk(metric_pairs, ~ {
  r <- cor.test(expert_numeric[[.x[1]]], expert_numeric[[.x[2]]],
                method = "spearman", exact = FALSE)
  message(.x[1], " vs ", .x[2], ": rho = ", round(r$estimate, 3),
          ", p = ", signif(r$p.value, 3))
})

# Permutation-test
by_id <- split(expert_numeric, expert_numeric$Participant_id)
walk(metric_pairs, ~ polychor_boot(.x[1], .x[2], B = 10000))

# ── 6. Summary tables ─────────────────────────────────────────────────────────
expert_average <- expert_numeric %>%
  group_by(questions, Category) %>%
  summarise(
    across(c(Importance, Burden, Accuracy),
           list(mean = ~ round(mean(.x, na.rm = TRUE), 2),
                sd   = ~ round(sd(.x,   na.rm = TRUE), 2)),
           .names = "{fn}_{col}"),
    mean_pct_underexplored = round(mean(Underexplored, na.rm = TRUE) * 100, 2),
    sd_pct_underexplored   = round(sd(Underexplored,   na.rm = TRUE) * 100, 2),
    .groups = "drop"
  ) %>%
  mutate(Category = factor(Category, levels = cat_levels)) %>%
  arrange(Category) %>%
  rename_with(~ sub("mean_", "mean_", .x))

expert_average_cat_bootstrap <- expert_average_cat_bootstrap(expert_numeric, cat_levels)

expert_imp_avg <- expert_numeric %>%
  group_by(questions, Category) %>%
  summarise(
    n               = n(),
    imp_ci          = list(ci_bootstrap(Importance)),
    mean_Burden     = mean(Burden,   na.rm = TRUE),
    mean_Accuracy   = mean(Accuracy, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mean_Importance      = round(map_dbl(imp_ci, "mean"),  2),
    lower_ci_importance  = round(map_dbl(imp_ci, "lower"), 2),
    upper_ci_importance  = round(map_dbl(imp_ci, "upper"), 2),
    priority_status = factor(
      case_when(
        lower_ci_importance < 2    ~ "Optional",
        mean_Importance     >= 2.5 ~ "Core",
        TRUE                       ~ "Secondary"
      ),
      levels = c("Core", "Secondary", "Optional")
    ),
    Category = factor(Category, levels = cat_levels)
  ) %>%
  select(-imp_ci)

table(expert_imp_avg$priority_status)

# ── 8. Plot helpers ───────────────────────────────────────────────────────────

scatter_base <- function(data, zones = FALSE) {
  p <- ggplot(data, aes(x = mean_Burden, y = mean_Importance))

  # Zones go first so everything else renders on top
  if (zones) {
    p <- p +
      annotate("rect", xmin=1, xmax=3, ymin=2.5, ymax=3,
               fill="#D5E8D4", alpha=0.6) +
      annotate("rect", xmin=1, xmax=3, ymin=2.0, ymax=2.5,
               fill="#FFF2CC", alpha=0.6) +
      annotate("rect", xmin=1, xmax=3, ymin=1.0, ymax=2.0,
               fill="#F8CECC", alpha=0.6) +
      annotate("text", x=2.95, y=2.75, label="Core",
               size=3.5, fontface="italic", color="grey40", angle=-90) +
      annotate("text", x=2.95, y=2.25, label="Secondary",
               size=3.5, fontface="italic", color="grey40", angle=-90) +
      annotate("text", x=2.95, y=1.55, label="Optional",
               size=3.5, fontface="italic", color="grey40", angle=-90)
  }

  p +
    geom_text_repel(aes(label = questions), size = 3, max.overlaps = 15) +
    scale_color_manual(name = "Thematic category", values = cfg$col_pal) +
    scale_size_continuous(name = "Average accuracy") +
    labs(
      x = "Average participant burden (1 = Low, 3 = High)",
      y = "Average importance (1 = Low, 3 = High)"
    ) +
    coord_cartesian(xlim = c(1, 3), ylim = c(1, 3)) +
    nature_theme(base_size = 11) +
    theme(legend.position = "top")
}

scatter_base2 <- function(data, zones = FALSE) {

  data <- data %>%
    mutate(label_display = str_to_sentence(str_replace_all(questions, "_", " ")))

  p <- ggplot(data, aes(x = mean_Burden, y = mean_Importance))

  if (zones) {
    p <- p +
      annotate("rect", xmin = 1, xmax = 3, ymin = 2.5, ymax = 3,
               fill = "#D5E8D4", alpha = 0.6) +
      annotate("rect", xmin = 1, xmax = 3, ymin = 2, ymax = 2.5,
               fill = "#FFF2CC", alpha = 0.6) +
      annotate("rect", xmin = 1, xmax = 3, ymin = 1, ymax = 2,
               fill = "#F8CECC", alpha = 0.6) +
      annotate("text", x = 2.95, y = 2.75, label = "Core",
               size = 3.5, fontface = "italic",
               color = "grey40", angle = -90) +
      annotate("text", x = 2.95, y = 2.25, label = "Secondary",
               size = 3.5, fontface = "italic",
               color = "grey40", angle = -90) +
      annotate("text", x = 2.95, y = 1.55, label = "Optional",
               size = 3.5, fontface = "italic",
               color = "grey40", angle = -90)
  }

  p +
    geom_text_repel(aes(label = label_display), size = 3, max.overlaps = 15) +
    scale_color_manual(name = "Thematic category", values = cfg$col_pal) +
    scale_size_continuous(name = "Average accuracy") +
    labs(
      x = "Average participant burden (1 = Low, 3 = High)",
      y = "Average importance (1 = Low, 3 = High)"
    ) +
    coord_cartesian(xlim = c(1, 3), ylim = c(1, 3)) +
    nature_theme(base_size = 11) +
    theme(legend.position = "top")
}

# ── 9. Figures ────────────────────────────────────────────────────────────────

# 9a. Expert geographical distribution
expert_geo <- data.frame(
  Continent    = c("Africa","Asia","Europe","North America","Oceania","South America"),
  Invited      = c(3, 13, 22, 9, 3, 0),
  Participated = c(1,  4, 14, 6, 1, 0)
) %>%
  pivot_longer(c(Invited, Participated),
               names_to  = "Group",
               values_to = "n") %>%
  mutate(Group = factor(Group, levels = c("Invited","Participated")))

p_geo <- ggplot(expert_geo, aes(x = Continent, y = n, fill = Group)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Invited" = "dodgerblue3",
                               "Participated" = "steelblue1")) +
  labs(x = "Geographical area", y = "Number of experts", fill = NULL) +
  nature_theme(base_size = 14) +
  theme(legend.position = "top")

save_fig(p_geo, "Expert_assessment_geographical.png", width = 10, height = 6)

# 9b. Importance × burden scatter — with shape encoding priority
p_scatter_shape <- scatter_base(expert_imp_avg) +
  geom_point(aes(color = Category, size = mean_Accuracy,
                 shape = priority_status), alpha = 0.9, stroke = 0.75) +
  scale_shape_manual(name   = "Classification",
                     values = c("Core"=19, "Secondary"=8, "Optional"=1)) +
  guides(shape=guide_legend(order=1),
         size =guide_legend(order=2),
         color=guide_legend(order=3)) +
  theme(legend.position = "right")

p_scatter_shape2 <- scatter_base2(expert_imp_avg) +
  geom_point(aes(color = Category, size = mean_Accuracy,
                 shape = priority_status), alpha = 0.9, stroke = 0.75) +
  scale_shape_manual(name   = "Classification",
                     values = c("Core"=19, "Secondary"=8, "Optional"=1)) +
  guides(shape=guide_legend(order=1),
         size =guide_legend(order=2),
         color=guide_legend(order=3)) +
  theme(legend.position = "right")

save_fig(p_scatter_shape, "expert_average.png", width = 12, height = 6.5)

ggsave(
  "figures/expert_importance_burden.svg",
  plot = p_scatter_shape2,
  width = 12,
  height = 7,
  device = "svg"
)

# 9c. Importance × burden — with zone shading
p_scatter_zones <- scatter_base(expert_imp_avg, zones = TRUE) +
  geom_point(aes(color = Category, size = mean_Accuracy), alpha = 0.9) +
  theme(legend.position = "right")

save_fig(p_scatter_zones, "expert_average_rev.png", width = 12, height = 6.5)

# 9d. Burden × accuracy matrix
p_burden_acc <- ggplot(expert_average,
                       aes(x = mean_Burden, y = mean_Accuracy)) +
  geom_point(aes(color = Category), alpha = 0.7) +
  geom_abline(intercept = 4, slope = -1,
              linetype = "dashed", color = "red",
              linewidth = 0.8, alpha = 0.3) +
  geom_text_repel(aes(label = str_to_sentence(str_replace_all(questions, "_", " "))),
                  size = 3.5, max.overlaps = 15) +
  scale_color_manual(values = cfg$col_pal) +
  coord_cartesian(xlim = c(1,3), ylim = c(1,3)) +
  labs(x = "Average burden (1 = Low, 3 = High)",
       y = "Average accuracy (1 = Low, 3 = High)") +
  nature_theme(base_size = 13) +
  theme(legend.position = "top")

save_fig(p_burden_acc, "accuracy_burden_plot.png", width = 13, height = 7.5)

p_importance_acc <- ggplot(expert_average,
                       aes(x = mean_Importance, y = mean_Accuracy)) +
  geom_point(aes(color = Category), alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1,
              linetype = "dashed", color = "red",
              linewidth = 0.8, alpha = 0.3) +
  geom_text_repel(aes(label = str_to_sentence(str_replace_all(questions, "_", " "))),
                  size = 3.5, max.overlaps = 15) +
  scale_color_manual(values = cfg$col_pal) +
  coord_cartesian(xlim = c(1,3), ylim = c(1,3)) +
  labs(x = "Average importance (1 = Low, 3 = High)",
       y = "Average accuracy (1 = Low, 3 = High)") +
  nature_theme(base_size = 13) +
  theme(legend.position = "top")

save_fig(p_importance_acc, "importance_acc_plot.png", width = 13, height = 7.5)

# 9e. % Underexplored bar chart
underexplored_ci <- expert_numeric %>%
  group_by(questions, Category) %>%
  summarise(
    ci = list(ci_bootstrap_binary(Underexplored)),
    .groups = "drop"
  ) %>%
  mutate(
    mean_pct_underexplored = round(map_dbl(ci, "p") * 100, 1),
    ci_lo_underexplored    = round(map_dbl(ci, "lower") * 100, 1),
    ci_hi_underexplored    = round(map_dbl(ci, "upper") * 100, 1)
  ) %>%
  select(-ci)

expert_numeric %>%
  group_by(Category) %>%
  summarise(
    ci = list(ci_bootstrap_binary(Underexplored)),
    .groups = "drop"
  ) %>%
  mutate(
    mean_pct_underexplored = round(map_dbl(ci, "p") * 100, 1),
    ci_lo_underexplored    = round(map_dbl(ci, "lower") * 100, 1),
    ci_hi_underexplored    = round(map_dbl(ci, "upper") * 100, 1)
  ) %>%
  select(-ci)

expert_plot_data <- underexplored_ci %>%
  left_join(
    expert_imp_avg %>% select(questions, Category, priority_status),
    by = c("questions", "Category")
  ) %>%
  mutate(
    q_wrapped = str_wrap(questions, width = 25),
    Category  = factor(Category, levels = cat_levels)
  )

p_underexplored <- ggplot(expert_plot_data,
                          aes(x    = reorder(str_to_sentence(str_replace_all(q_wrapped, "_", " ")), mean_pct_underexplored),
                              y    = mean_pct_underexplored,
                              fill = priority_status)) +
  geom_col(color = "grey30", linewidth = 0.25, width = 0.75) +
  geom_errorbar(aes(ymin = ci_lo_underexplored, ymax = ci_hi_underexplored),
                width = 0.3, linewidth = 0.4, color = "grey20") +
  geom_hline(yintercept = 50, linetype = "dashed",
             color = "#B2182B", linewidth = 0.6) +
  scale_fill_manual(values = c("Core"      = "#2166AC",
                               "Secondary" = "#92C5DE",
                               "Optional"  = "#D9D9D9")) +
  scale_y_continuous(limits = c(0,100), breaks = c(0,25,50,75,100),
                     expand = expansion(mult = c(0, 0.02))) +
  coord_flip() +
  facet_wrap(~ Category, scales = "free_y", ncol = 2) +
  labs(x = NULL, y = "Average % underexplored (95% bootstrap CI)", fill = NULL) +
  nature_theme(base_size = 10) +
  theme(
    strip.text           = element_text(face="bold", size=11, hjust=0),
    axis.text.y          = element_text(size=9),
    axis.text.x          = element_text(size=9),
    axis.title.x         = element_text(size=12, margin=margin(t=6)),
    legend.position      = "top",
    legend.justification = "center",
    legend.text          = element_text(size=10),
    panel.grid.major.x   = element_line(color="grey90", linewidth=0.3)
  )

save_fig(p_underexplored, "underexplored_importance.png",
         width = 10.5, height = 8.5)

# # 9f.
# imp_summary <- analyze_metric_priority(expert_numeric, "Importance")
#
# # Priority filtering (Importance only)
# core      <- imp_summary %>% filter(ci_lower >= 2.0, val_mean >= 2.5)
# secondary <- imp_summary %>% filter(ci_lower >= 2.0, val_mean <  2.5)
# optional <- imp_summary %>% filter(ci_lower < 2.0)

# message("\nCore items (n=",      nrow(core),      "): ", paste(core$questions,      collapse=", "))
# message("Secondary items (n=",   nrow(secondary),  "): ", paste(secondary$questions, collapse=", "))
# message("Optional items (n=",   nrow(optional),  "): ", paste(optional$questions, collapse=", "))

p_importance <- make_forest_plot(expert_numeric, "Importance",
                                 "Mean importance score (95% bootstrap CI)")

p_burden <- make_forest_plot(expert_numeric, "Burden",
                                 "Mean burden score (95% bootstrap CI)")

p_accuracy <- make_forest_plot(expert_numeric, "Accuracy",
                                 "Mean accuracy score (95% bootstrap CI)")

save_fig(p_importance, "figure_importance.png", width = 10.5, height = 8.5)
save_fig(p_burden, "figure_burden.png", width = 10.5, height = 8.5)
save_fig(p_accuracy, "figure_accuracy.png", width = 10.5, height = 8.5)

# .......
response_dist_data <- expert_numeric %>%
  group_by(questions, Category) %>%
  summarise(
    Low    = mean(Importance == 1, na.rm = TRUE) * 100,
    Medium = mean(Importance == 2, na.rm = TRUE) * 100,
    High   = mean(Importance == 3, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(Low, Medium, High), names_to = "Response", values_to = "pct") %>%
  mutate(
    Response  = factor(Response, levels = c("Low", "Medium", "High")),
    q_wrapped = str_wrap(questions, width = 25),
    Category  = factor(Category, levels = cat_levels)
  )

# Order items within each category by % High, for a cleaner visual gradient
order_by_high <- response_dist_data %>%
  filter(Response == "High") %>%
  arrange(Category, pct) %>%
  pull(q_wrapped) %>%
  unique()

response_dist_data <- response_dist_data %>%
  mutate(
    q_wrapped = factor(q_wrapped, levels = order_by_high),
    q_display = factor(
      q_display,
      levels = unique(q_display[match(order_by_high, q_wrapped)])
    )
  )

p_response_dist <- ggplot(response_dist_data,
                          aes(x = pct, y = q_display, fill = Response)) +
  geom_col(position = "stack", color = "grey30", linewidth = 0.2, width = 0.75) +
  scale_fill_manual(values = c("Low" = "#D9D9D9", "Medium" = "#92C5DE", "High" = "#2166AC")) +
  facet_wrap(~ Category, scales = "free_y", ncol = 2) +
  labs(x = "% of experts", y = NULL, fill = "Importance rating") +
  nature_theme(base_size = 10) +
  theme(
    strip.text           = element_text(face = "bold", size = 11, hjust = 0),
    axis.text.y          = element_text(size = 9),
    axis.text.x          = element_text(size = 9),
    axis.title.x         = element_text(size = 12, margin = margin(t = 6)),
    legend.position      = "top",
    legend.justification = "center",
    legend.text          = element_text(size = 10),
    panel.grid.major.x   = element_line(color = "grey90", linewidth = 0.3)
  )

save_fig(p_response_dist, "p_distribution.png", width = 10.5, height = 8.5)

# .......
supplementary_table <- build_supplementary_table(expert_numeric, expert_imp_avg)

supplementary_table %>%
  filter(Classification != `Classification (median)`)


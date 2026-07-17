# Shared ggplot2 theme applied to every plot infeR produces, so that all
# diagnostic figures look consistent and are ready to drop directly into a
# manuscript or slide deck without further styling.
theme_publication <- function(base_size = 13) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "grey90", linewidth = 0.3),
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.7),
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(color = "black", size = ggplot2::rel(0.9)),
      axis.title = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.05), hjust = 0),
      plot.subtitle = ggplot2::element_text(color = "grey30", size = ggplot2::rel(0.8)),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(0.85)),
      strip.background = ggplot2::element_rect(fill = "grey95", color = "black"),
      strip.text = ggplot2::element_text(face = "bold"),
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )
}

# A small, colorblind-friendly qualitative palette used for grouped/faceted
# plots (post-hoc contrasts, multivariate scores, etc.).
publication_palette <- c(
  "#2C7FB8", "#D95F02", "#1B9E77", "#7570B3", "#E7298A",
  "#66A61E", "#E6AB02", "#A6761D", "#666666"
)

theme_custom <- function(base_size = 12, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.3),
      axis.ticks = element_line(color = "black", linewidth = 0.3),
      
      axis.title = element_text(size = base_size + 1, face = "plain"),
      axis.text = element_text(size = base_size, color = "black"),
      axis.text.x = element_text(margin = margin(t = 5), angle = 0, hjust = 0.5),
      axis.text.y = element_text(margin = margin(r = 5)),
      
      plot.title = element_text(size = base_size + 2, face = "bold", hjust = 0, vjust = 1),
      plot.title.position = "plot",
      plot.subtitle = element_text(size = base_size + 1, hjust = 0, color = "grey30"),
      
      legend.title = element_text(size = base_size, face = "plain"),
      legend.text = element_text(size = base_size),
      legend.position = "top",
      legend.key = element_blank(),
      legend.background = element_blank(),
      
      strip.text = element_text(size = base_size + 1, face = "bold"),
      strip.background = element_rect(fill = "gray97", color = "gray40", linewidth = 0.6),
      
      plot.margin = margin(10, 10, 10, 10)
    )
}
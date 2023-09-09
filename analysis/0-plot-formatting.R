# Define theme that works well with substack ----
theme_substack <- function() {
  theme_bw() +
    theme(axis.text.x.bottom = element_text(color = "black", size = 14),
          
          axis.text.y = element_text(size = 14, color = "black"),
          
          axis.title.x.bottom   = element_text(size = 18),
          axis.title.y.left = element_text(size = 18),
          
          
          plot.title = element_text(hjust = .5, size = 20),
          plot.subtitle = element_text(hjust = .5, size = 18), 
          plot.caption = element_text(size = 13),
          
          # Facets
          strip.text = element_text(size = 16), strip.background = element_rect(fill = "#F5F5F5")
    )
}


# Define color scheme ----
fpi_colors <- c("#5F6D73", "#B4B4AA", "#BF8C6F", "#D9CBBF")

# Load plot formatting
source("./analysis/0-plot-formatting.R")

# Load data
fpi_comparison <- readr::read_csv(file = "./data/cfb-fpi-comparison-pre-post-labor-day.csv")

# Basic summaries
summary(fpi_comparison$rank_change)
sd(fpi_comparison$rank_change)
summary(fpi_comparison$fpi_change)

# Histograms
hist(fpi_comparison$rank_change)
hist(fpi_comparison$fpi_change)

# Anything interesting by conference?
ggplot(data = fpi_comparison, aes(x = conference_before, y = fpi_change, fill = conference_before)) +
geom_boxplot()

dplyr::group_by(fpi_comparison, conference_before) %>%
dplyr::summarize(min = min(fpi_change),
                 max = max(fpi_change),
                 mean = mean(fpi_change),
                 sd = sd(fpi_change),
                 n = n()
                 ) %>%
dplyr::arrange(desc(sd))

# Nicer histogram
fpi_comparison %<>% dplyr::mutate(team_highlight = case_when(rank_change > 30 | rank_change < -30 ~ paste(season, team),
                                                            rank_change <= 30 | rank_change >= -30 ~ "One team"
                                 
                                 ))

# Histogram x labels
bottom <- seq(-34, 39, 5)
top <- seq(-30, 45, 5)
x_labs <- c(paste(bottom, top, sep = "-"), "")


# Define in-plot labels
plot_labels = data.frame(label = c("The FPI ranking of 93 teams across the 2021-2023\nseasons increased by between 1 and 5 spots\nafter their Labor Day Weekend Games",
                                   "2021 Oklahoma State Cowboys (-35)",
                                   "2023 Colorado Buffaloes (+31)",
                                   "2023 Texas State Bobcats (+43)"
                                   ),
                          x = c(27, -24.5, 30, 35),
                          y = c(80, 35, 26, 35),
                          xstart = c(7, -33, 31, 43),
                          xend = c(3, -33, 31, 43),
                          ystart = c(80, 32, 22, 32),
                          yend = c(80, 2, 2, 2)
                         )

# Main plot
rank_change_hist <-  ggplot(data = fpi_comparison) +
                     geom_histogram(aes(x = rank_change, fill = team_highlight), color = "black", binwidth = 5, boundary = 0) +
                     scale_y_continuous( limits = c(0, 120), breaks = seq(0, 120, 20), expand = c(0,0)) +
                     scale_x_continuous(limits = c(-35, 45), breaks = seq(-35, 45, 5), labels = x_labs) +
                     scale_fill_manual(values = c("blue", "green", "green", "gray")) +
                        
                     labs(x = "Change in Ranking",
                          y = "Number of Teams",
                          title = "Change in College Football FPI Rankings After Labor Day Weekend Games",
                          subtitle = "2021-2023 Seasons"
                          ) +
                        
                      # In-plot labels
                      geom_label(data = plot_labels, aes(x = x, y = y, label = label), size = 6) +
                      geom_segment(data = plot_labels, aes(x = xstart, y = ystart, xend = xend, yend = yend), size = .4, arrow = arrow(length = unit(0.1, "cm"))) +  
                        
                      # Themes  
                      theme_substack() +
                      theme(axis.text.x.bottom = element_text(angle = 90, vjust = 2.4),
                            legend.position = "none"
                            )

# Save
ggsave(plot = rank_change_hist, filename = "./analysis/plots/fpi-rank-change-histogram.png", width = 13.5, height = 11)


# Probabilities assuming an approximately normal distribution ----
mu <- mean(fpi_comparison$rank_change)
sigma2 <- var(fpi_comparison$rank_change)

# Greater than +/- 30?
2 * (1-pnorm(mean = mu, sd = sqrt(sigma2), 30))




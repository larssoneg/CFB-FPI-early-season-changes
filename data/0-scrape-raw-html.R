# Scrape FPI tables from archived ESPN FPI sites
scrape_raw_fpi <- function(html_file) {

  # Time stamp
  time_stamp <- stringr::str_extract(string = html_file, pattern = "\\d{14}")
  
  html_table <- rvest::read_html(html_file)
  schedule <- rvest::html_table(html_table, header = T)

teams <- schedule[[2]]
teams <- teams[2:nrow(teams),]
names(teams) <- "team"

fpi <- schedule[[3]]
fpi <- fpi[2:nrow(fpi),1:4]
names(fpi) <- c("record", "fpi", "rank", "trend")

fpi <- bind_cols(teams, fpi) %>%
       dplyr::mutate_at(c("fpi", "rank", "trend"), as.numeric) %>%
       dplyr::mutate(trend = tidyr::replace_na(trend, 0),
                     wayback_timestamp = time_stamp,
                     season = as.numeric(substring(time_stamp, 1, 4))
                     )

return(fpi)

}


# File names
before_games_files <- list.files("./data/raw", pattern = "*before-games.html", full.names = T)
after_games_files <- list.files("./data/raw", pattern = "*after-games.html", full.names = T)

# Before labor day
before_games <- lapply(X = before_games_files, FUN = function(x) scrape_raw_fpi(x) %>% dplyr::arrange(team)) %>%
                dplyr::bind_rows() %>%
                dplyr::rename(record_before = record, fpi_before = fpi, rank_before = rank, trend_before = trend, conference_before = `...2`,
                              wayback_timestamp_before = wayback_timestamp) %>%
                dplyr::group_by(team) %>%
                dplyr::arrange(team) %>% 
                tidyr::fill(conference_before, .direction = "up")

# After labor day
after_games <- lapply(X = after_games_files, FUN = function(x) scrape_raw_fpi(x) %>% dplyr::arrange(team)) %>%
  dplyr::bind_rows() %>%
  dplyr::rename(record_after = record, fpi_after = fpi, rank_after = rank, trend_after = trend, conference_after = `...2`,
                wayback_timestamp_after = wayback_timestamp
                ) %>%
  dplyr::group_by(team) %>%
  dplyr::arrange(team) %>% 
  tidyr::fill(conference_after, .direction = "up")


# Join by team and season
fpi_comparison <- dplyr::inner_join(x = before_games, y = after_games, by = c("team", "season")) %>%
                  dplyr::mutate(rank_change = rank_before - rank_after,
                                fpi_change = fpi_after - fpi_before
                               ) %>%
                  dplyr::select(season, team, rank_change, fpi_change, dplyr::everything()) %>%
                  dplyr::arrange(desc(rank_change))



# Save
readr::write_csv(x = fpi_comparison, file = "./data/cfb-fpi-comparison-pre-post-labor-day.csv")





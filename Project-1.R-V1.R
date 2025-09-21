library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(readr)

lines <- readLines("C:/MSinDSSemester1/DATA607-Projects/Project-1/data/tournamentinfo.txt")

drop <- str_detect(lines, r"(^\s*$)") |          
  str_detect(lines, r"(^-{2,}$)") |       
  str_detect(lines, r"(^\s*Pair\s*\|)")   

drop[is.na(drop)] <- FALSE

clean_lines <- lines[!drop]
stopifnot(exists("clean_lines"), is.character(clean_lines))

top_idx <- which(str_detect(clean_lines, "^\\s*\\d+\\s*\\|"))
row1 <- clean_lines[top_idx]        
row2 <- clean_lines[top_idx + 1]   

# split each row on "|", returned as a matrix; trim spaces
m1 <- str_split(row1, "\\|", simplify = TRUE)
m2 <- str_split(row2, "\\|", simplify = TRUE)

# build the frame with just the requested columns
player_info <- tibble(
  Player_Num = as.integer(str_squish(m1[, 1])),
  Name       = str_squish(m1[, 2]),
  TotalPts   = as.numeric(str_squish(m1[, 3])),
  State      = str_squish(m2[, 1]),
  PreRating  = as.integer(str_match(row2, "R:\\s*(\\d+)")[, 2]),
)

# player_rating_matx columns (R1..R7)
stopifnot(ncol(m1) >= 10)
player_rating_matx <- m1[, 4:10, drop = FALSE]

# keep matrix shape by applying over columns
opps_mat <- apply(player_rating_matx, 2, function(col) stringr::str_sub(col, 3, 4))

# now opps_mat is a matrix again → safe to apply over rows
opp_list <- apply(opps_mat, 1, function(r) {
  vals <- as.integer(trimws(r))  # " 5" -> 5, "12" -> 12, "H " -> NA
  vals[!is.na(vals)]             # drop H/B/U/etc.
})

# Ensure Player_Num is present (pull from m1[,1] if needed)
if (!"Player_Num" %in% names(player_info)) {
  player_info <- player_info %>%
    mutate(Player_Num = as.integer(stringr::str_squish(m1[, 1])))
}

# Key + lookup (player number -> PreRating)
players_key <- player_info %>% select(Player_Num, Name, PreRating)
pre_lookup  <- stats::setNames(players_key$PreRating, players_key$Player_Num)

# Convert opponent IDs -> opponent ratings, padded to equal length
max_opps <- max(lengths(opp_list), na.rm = TRUE)

ratings_mat <- vapply(
  opp_list,
  FUN.VALUE = numeric(max_opps),
  function(ids) {
    if (length(ids) == 0) return(rep(NA_real_, max_opps))
    vals <- unname(pre_lookup[as.character(ids)])
    c(vals, rep(NA_real_, max_opps - length(vals)))   # pad with NA
  }
)

# Orient so rows = players, cols = Opp1..OppK
ratings_mat <- t(ratings_mat)
colnames(ratings_mat) <- paste0("Opp", seq_len(max_opps), "_Rating")

# Final tibble
opp_ratings_tbl <- dplyr::bind_cols(
  players_key %>% select(Player_Num, Name),
  tibble::as_tibble(ratings_mat)
)

# Peek
dplyr::glimpse(opp_ratings_tbl)
head(opp_ratings_tbl, 5)



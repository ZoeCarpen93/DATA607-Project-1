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

# opp_matx columns (R1..R7)
stopifnot(ncol(m1) >= 10)
opp_matx <- m1[, 4:10, drop = FALSE]

m <- stringr::str_match(as.vector(opp_matx), "[WLD]\\s*(\\d+)")
# m[,1] is the full match like "W  39"; m[,2] is just the digits "39"

opp_ids <- matrix(
  as.integer(m[, 2]),
  nrow = nrow(opp_matx),
  ncol = ncol(opp_matx)
)

# named lookup: player number -> PreRating
pre_lookup <- stats::setNames(player_info$PreRating, player_info$Player_Num)

ratings_vec <- unname(pre_lookup[as.character(opp_ids)])
ratings_mat <- matrix(ratings_vec, nrow = nrow(opp_matx), ncol = ncol(opp_matx))

# Average opponent pre-rating per player (ignore NAs for byes, etc.)
AvgOppPre <- rowMeans(ratings_mat, na.rm = TRUE)
AvgOppPre <- ifelse(is.nan(AvgOppPre), NA_real_, AvgOppPre)  # convert NaN (all-NA rows) to NA
AvgOppPre <- round(AvgOppPre)

# as a new column on the matrix:
ratings_plus <- cbind(ratings_mat, AvgOppPre = AvgOppPre)

# Add it to player_info tibble and keep only required cols:
stopifnot(nrow(player_info) == nrow(ratings_mat))
player_info <- player_info %>%
  dplyr::mutate(AvgOppPre = AvgOppPre) %>%
  dplyr::select(Name, State, TotalPts, PreRating, AvgOppPre)

readr::write_csv(player_info, file.path("C:/MSinDSSemester1/DATA607-Projects/Project-1/outputs/tournament_players.csv")
                 





library(ffscrapr)
library(dplyr)
library(data.table)
library(tidyr)
library(purrr)
library(stringr)
library(piggyback)
library(cli)

options(dplyr.summarise.inform = FALSE,
        piggyback.verbose = FALSE)

mfl_user_agent <- function(league_id){
  paste0(Sys.getenv("MFL_USER_AGENT"),league_id)
}

# leagues <- mfl_getendpoint(mfl_connect(2022),"leagueSearch",SEARCH="#SFB12") |>
#   pluck("content","leagues","league") |>
#   rbindlist() |>
#   filter(str_detect(name,"\\#SFB12 [0-9]+")) |>
# select(-homeURL, league_name = name, league_id = id)
#
# fwrite(leagues,"mfl_league_ids.csv",quote = TRUE)

leagues <- fread("league_ids_mfl.csv")

get_draft <- function(league_id){
  cli::cli_alert("League ID: {league_id}")
  conn <- mfl_connect(2022, league_id, user_agent = mfl_user_agent(league_id), rate_limit = F)
  ff_draft(conn)
}

drafts <- leagues |>
  mutate(drafts = map(league_id, possibly(get_draft, otherwise = tibble()))) |>
  unnest(drafts) |>
  mutate(
    division_name = case_when(
      league_id == 58678 & division == "02" ~ "ATLANTIS, ATLANTIS",
      TRUE ~ division_name
    )
  )

fwrite(drafts,"output/draft_picks_mfl.csv",quote = TRUE)
update_time <- format(Sys.time(), tz = "America/Toronto", usetz = TRUE)
writeLines(update_time, "output/timestamp.txt")
pb_upload("output/draft_picks_mfl.csv", repo = "dynastyprocess/data-sfb12", tag = "data-mfl")
pb_upload("output/timestamp.txt", repo = "dynastyprocess/data-sfb12", tag = "data-mfl")

drafts <- fread("output/draft_picks_mfl.csv")

adp <- drafts |>
  filter(!is.na(player_id)) |>
  group_by(league_id,division,pos) |>
  mutate(pos_rank = rank(overall)) |>
  group_by(player_id, player_name, pos, team) |>
  summarise(
    n = n(),
    overall_avg = mean(overall, na.rm = TRUE) |> round(2),
    overall_sd = sd(overall, na.rm = TRUE) |> round(2),
    pos_avg = mean(pos_rank, na.rm = TRUE) |> round(2),
    pos_sd = sd(pos_rank, na.rm = TRUE) |> round(2),
    overall_min = min(overall, na.rm = TRUE),
    overall_max = max(overall, na.rm = TRUE),
    pos_min = min(pos_rank, na.rm = TRUE),
    pos_max = max(pos_rank, na.rm = TRUE)
  ) |>
  ungroup() |>
  arrange(overall_avg,-n)

fwrite(adp,"output/adp_mfl.csv")
update_time <- format(Sys.time(), tz = "America/Toronto", usetz = TRUE)
writeLines(update_time, "output/timestamp.txt")
pb_upload("output/adp_mfl.csv", repo = "dynastyprocess/data-sfb12", tag = "data-mfl")
pb_upload("output/timestamp.txt", repo = "dynastyprocess/data-sfb12", tag = "data-mfl")

cli::cli_alert_success("Successfully got all picks and ADP!")

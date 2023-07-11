library(ffscrapr)
library(dplyr)
library(data.table)
library(tidyr)
library(purrr)
library(stringr)
library(piggyback)
library(cli)

leagues <- sleeper_userleagues("ScottFishBowl", 2022) |>
select(league_id, league_name) |>
  filter(str_detect(league_name,"\\#SFB12")) |> 
  mutate(league_id = as.character(league_id))

fwrite(leagues,"league_ids_sleeper.csv")

get_draft <- function(league_id){
  cli::cli_alert("League ID: {league_id}")
  conn <- sleeper_connect(2022, league_id, rate_limit = F)
  suppressWarnings(ff_draft(conn))
}

drafts <- leagues |>
  select(league_id, league_name) |>
  mutate(drafts = map(league_id, possibly(get_draft, otherwise = tibble()))) |>
  unnest(drafts) |>
  group_by(league_id) |>
  mutate(overall = row_number()) |>
  ungroup()


adp <- drafts |>
  filter(!is.na(player_id)) |>
  group_by(league_id) |>
  mutate(overall = row_number()) |>
  group_by(league_id,pos) |>
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

fwrite(drafts,"output/draft_picks_sleeper.csv",quote = TRUE)
fwrite(adp,"output/adp_sleeper.csv",quote = TRUE)
update_time <- format(Sys.time(), tz = "America/Toronto", usetz = TRUE)
writeLines(update_time, "output/timestamp.txt")
pb_upload("output/draft_picks_sleeper.csv", repo = "dynastyprocess/data-sfb12", tag = "data-sleeper")
pb_upload("output/adp_sleeper.csv", repo = "dynastyprocess/data-sfb12", tag = "data-sleeper")
pb_upload("output/timestamp.txt", repo = "dynastyprocess/data-sfb12", tag = "data-sleeper")

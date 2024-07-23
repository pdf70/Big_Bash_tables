# Filename: "Womens_Big_Bash_tables.R"

# Reads in data from wikipedia of history of all Women's Big Bash League (WBBL) tables
# Note that the format of the input data may change as people change wikipedia entries

# Team colours sourced from https://sportsfancovers.com/bbl-color-codes/
# Could also replicate HTML format from https://imagecolorpicker.com/en.

# Retrieve previous work from:
#setwd(output_path) 
#load(file = "wbbl_tables_raw.Rdata")   # list - "tables"
#load(file="wbbl_tables.Rdata")         # data frame 


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Libraries & directories

# Set directory paths
path = "C:/Users/fallo/OneDrive/Documents/Pete/R-files"
input_path = paste(path, "/input", sep="")
output_path = paste(path, "/R_output", sep="")
setwd(path)

# Specify packages (libraries) that are used
library(lubridate)
library(tidyverse)
library(scales)
library(rvest)    # Reading tables from a web page


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Parameters 

# From first season in 2015-16 to 2023-24
end_yr = seq(2016, 2024, by = 1)
start_yr = end_yr - 1
seasons = paste(start_yr, "-", substr(end_yr,3,4), sep = "")

# Using template as location for wikipedia table
# Note: need to amend values if on wikipedia a new table is inserted above the league table
wiki_table_no = c(2, rep(4,length(seasons)-1))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Functions 
make_graph_wbbl = function(team_abbrev) {
  data_for_graph = wbbl_tables %>% 
    filter(abbrev == team_abbrev)
  
  max_teams_in_season = max(data_for_graph$count_teams)
  start_yr = min(data_for_graph$season)
  end_yr = max(data_for_graph$season)
  min_yr = min(data_for_graph$yr_end)
  max_yr = max(data_for_graph$yr_end)
  discont_yr = 2099
  league_name = "WBBL"
  
  #Breaks for background rectangles, other formatting
  x_intercepts = data_for_graph$yr_end[(data_for_graph$yr_end %% 5) == 0]
  x_intercepts = x_intercepts[!(x_intercepts ==max_yr)]
  
  # Graph of league position
  graph_1 = ggplot(data_for_graph, aes(x = yr_end, y = Pos, group=yr_end<=discont_yr)) +
    geom_line(linewidth=1.15, colour = data_for_graph$team_colours[1]) +
    geom_point(aes(colour=as.factor(champion), size = as.factor(champion))) +
    scale_colour_manual(values = c(data_for_graph$second_colour[1], data_for_graph$champ_colour[1])) +
    scale_size_manual(values = c(2,4)) +

    # axes
    scale_y_continuous(trans = "reverse", expand = c(0,0.1), breaks= pretty_breaks()) +
    scale_x_continuous(breaks= pretty_breaks()) +
    coord_cartesian(xlim = c(min_yr, max_yr), ylim = c(max_teams_in_season, 1)) +
    geom_vline(xintercept=x_intercepts,  linetype="dotted") +
    theme(panel.border = element_rect(fill=NA)) +
    
    # titles
    ggtitle(paste("Position of", data_for_graph$current_name[1], "in", league_name, "from", start_yr, "to", end_yr)) + 
    theme(plot.title = element_text(lineheight=1.0, face="bold", hjust = 0.5)) +
    labs(x="Year", y="Position") +
    theme(axis.title = element_text(face = "bold")) +
    theme(plot.margin=unit(c(0.5,1,1.5,1.2),"cm")) +
    theme(legend.position = "none") +
    
    # horizontal lines for number of finals teams
    geom_segment(aes(x = min(yr_end), xend = max(yr_end), y = 4.5, yend = 4.5), linetype="dotted", colour = "black", linewidth = 1)
  
  graph_1
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Read in external data
# read all points tables in one loop
tables = list()

for (j in 1:length(seasons)) {
  table = read_html(paste("https://en.wikipedia.org/wiki/", seasons[j], "_Women's_Big_Bash_League_season", sep = ""))
  tables_wiki <- table %>%
    html_nodes(".wikitable") %>%
    html_table(fill = TRUE)
  
  tables[[j]] <- tables_wiki[[wiki_table_no[j]]] %>% # added to my list
    mutate(season_no = j, season = seasons[j])
  
  if (j%%5==0) print(paste("season = ", seasons[j])) 
}

# Review headers in each of the tables - need consistency of names for combining tables
headers_all = c()
for (j in 1) {
  header_fmt1 = colnames(tables[[j]])
  headers_all = rbind(header_fmt1, headers_all)
}
for (j in 2) {                                     # exception - season 2 has points deductions in points table
  header_fmt2 = colnames(tables[[j]])
  headers_all = rbind(header_fmt2, headers_all)
}

header_fmt1 = colnames(tables[[1]]) %>%
  str_replace("\\.mw-parser.*","") 
header_fmt2 = colnames(tables[[2]]) %>%
  str_replace("\\.mw-parser.*","") 

for (j in 1:length(seasons)) {  
  colnames(tables[[j]]) = header_fmt1
}
for (j in 2) {
  colnames(tables[[j]]) = header_fmt2             
}

# convert from list to data frame
tables_all_fmt1 = do.call(rbind, lapply(tables[c(1, 3:length(seasons))], as.data.frame))
tables_all_fmt2 = do.call(rbind, lapply(tables[2], as.data.frame))

tables_all_fmt1_adj = tables_all_fmt1 %>%
  mutate(Ded = 0) %>%
  select(Pos:NR, Ded, Pts:season)

tables_all = rbind(tables_all_fmt1_adj, tables_all_fmt2) %>%
  arrange(season_no, Pos)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Read in input files
setwd(input_path)
wbbl_teams = read_csv("bbl_teams.csv")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Select relevant data, and then data manipulations
wbbl_tables = tables_all %>% 
  mutate(Team = str_replace(Team, "\\[.*\\]", ""),            # remove text inside square brackets
         champion = ifelse(substr(Team, nchar(Team) - 2, nchar(Team)) == "(C)", 1, 0),
         runners_up = ifelse(substr(Team, nchar(Team) - 3, nchar(Team)) == "(RU)", 1, 0),
         grand_finalist = champion + runners_up,
         premiers = ifelse(Pos == 1, 1, 0),
         finals = ifelse(season_no <= 8, ifelse(Pos <= 4, 1, 0), ifelse(Pos <= 5, 1, 0)),
         pts_per_win = 2,
         pts_per_draw = 1,
         pts_deducted = Pts - (pts_per_win * W + pts_per_draw * NR),
         max_avail_pts = Pld * pts_per_win,
         pts_achieved_perc = Pts / max_avail_pts,
         NRR_value = ifelse(nchar(NRR) == 6, as.numeric(substr(NRR,2,6)) * -1, as.numeric(NRR)),
         yr_end = as.numeric(substr(season, 1, 4)) + 1) %>%
  group_by(season) %>%
  mutate(count_teams = n(),
         wooden_spoon = ifelse(Pos == max(Pos), 1, 0)) %>%
  ungroup() %>%
  select(Pos:finals, count_teams:wooden_spoon, pts_per_win:yr_end)

wbbl_tables$Team = gsub(" \\s*\\([^\\)]+\\)","",as.character(wbbl_tables$Team)) # to get consistency in team name

# Create a table of team names, including history & past team name changes (if any)
teams = as_tibble(unique(wbbl_tables$Team))
colnames(teams) = c("original_name")
teams = teams %>% 
  mutate(current_name = original_name)
#teams$current_name = ifelse(teams$original_name == "Melbourne Heart", "Melbourne City", teams$current_name)

teams_all = left_join(teams, wbbl_teams, by = c("current_name" = "current_name"))

wbbl_tables_all = left_join(wbbl_tables, teams_all, by = c("Team" = "original_name"))

# Add additional information of previous season's finishing position
wbbl_tables = wbbl_tables_all %>%
  arrange(current_name, season_no) %>%
  mutate(prev_pos = ifelse(current_name == lag(current_name), lag(Pos), NA)) %>%
  mutate(next_pos = ifelse(current_name == lead(current_name), lead(Pos), NA)) %>%
  arrange(season_no, Pos) %>%
  mutate(pos_diff = ifelse(is.na(prev_pos), NA, -(Pos - prev_pos)),
         pos_abs_diff = abs(pos_diff)) %>%
  group_by(current_name) %>%
  mutate(cum_champions = cumsum(champion),
         streak_champion = c(ave(c(0, champion), cumsum(c(0, champion) == 0), FUN = seq_along) - 1)[-1],
         streak_missed_champion = c(ave(c(0, champion), cumsum(c(0, champion) > 0), FUN = seq_along) - 1)[-1],
         cum_runners_up = cumsum(runners_up),
         streak_runners_up = c(ave(c(0, runners_up), cumsum(c(0, runners_up) == 0), FUN = seq_along) - 1)[-1],
         cum_premiers = cumsum(premiers),
         streak_premiers = c(ave(c(0, premiers), cumsum(c(0, premiers) == 0), FUN = seq_along) - 1)[-1],
         streak_missed_premiers = c(ave(c(0, premiers), cumsum(c(0, premiers) > 0), FUN = seq_along) - 1)[-1],
         cum_finals = cumsum(finals),
         streak_finals = c(ave(c(0, finals), cumsum(c(0, finals) == 0), FUN = seq_along) - 1)[-1],
         streak_missed_finals = c(ave(c(0, finals), cumsum(c(0, finals) > 0), FUN = seq_along) - 1)[-1],
         cum_grand_finals = cumsum(grand_finalist),
         streak_grand_finals = c(ave(c(0, grand_finalist), cumsum(c(0, grand_finalist) == 0), FUN = seq_along) - 1)[-1],
         streak_missed_grand_finals = c(ave(c(0, grand_finalist), cumsum(c(0, grand_finalist) > 0), FUN = seq_along) - 1)[-1]) %>%
  ungroup() %>%
  mutate(row_number = row_number())


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Analysis of Women's Big Bash League tables data
# Make all-time points table
wbbl_all_time_points_table = group_by(wbbl_tables, current_name) %>%
  summarise(count = n(),
            Total_Pld = sum(Pld),
            Total_W = sum(W),
            Total_L = sum(L),
            Total_NR = sum(NR),
            Total_Ded = sum(pts_deducted),
            Total_Pts = sum(Pts),
            win_perc = round(Total_W / (Total_W + Total_L) * 100, 2),
            result_perc = round((Total_W + 0.5 * Total_NR) / Total_Pld * 100, 2),
            count_champions = sum(champion),
            count_runners_up = sum(runners_up),
            count_premiers = sum(premiers),
            count_finals = sum(finals),
            count_1st = sum(Pos == 1),
            count_2nd = sum(Pos == 2),
            count_3rd = sum(Pos == 3),
            best = min(Pos), 
            count_spoon = sum(wooden_spoon),
            count_gf = sum(grand_finalist),
            first_season = min(season),
            last_season = max(season)) %>%
  arrange(desc(win_perc), desc(count_champions), desc(count_runners_up))

# champions by final position
champions = filter(wbbl_tables, champion == 1)
champions_by_Pos = group_by(champions, Pos) %>%
  summarise(count = n())

# totals by season
season_totals = group_by(wbbl_tables, season, yr_end) %>%
  summarise(count = n(),
            Total_Pld = sum(Pld),
            Total_W = sum(W),
            Total_L = sum(L),
            Total_NR = sum(NR),
            Total_Ded = sum(Ded),
            Total_Pts = sum(Pts))

title_race_totals = group_by(wbbl_tables, season, yr_end) %>%
  summarise(count = n(),
            Total_Pts_1 = sum(Pts[Pos == 1]),
            Total_Pts_2 = sum(Pts[Pos == 2]),
            Total_NRR_1 = sum(NRR_value[Pos == 1]),
            Total_NRR_2 = sum(NRR_value[Pos == 2])) %>%
  mutate(margin_pts = Total_Pts_1 - Total_Pts_2,
         margin_NRR = Total_NRR_1 - Total_NRR_2)

# totals by club
team_streaks = group_by(wbbl_tables, current_name) %>%
  summarise(count = n(),
            max_streak_champion = max(streak_champion),
            max_streak_missed_champion = max(streak_missed_champion),
            max_streak_runners_up = max(streak_runners_up),
            streak_premiers = max(streak_premiers),
            max_streak_missed_premiers = max(streak_missed_premiers),
            max_streak_finals = max(streak_finals),
            max_streak_missed_finals = max(streak_missed_finals),
            max_streak_grand_finals = max(streak_grand_finals),
            max_streak_missed_grand_finals = max(streak_missed_grand_finals)) %>%
  arrange(current_name)

# Records for a single season
# most & least points - not adjusted for no. of games or points system
most_pts_season = arrange(wbbl_tables, desc(Pts)) %>%
  select(season, Team, Pld, Pts)
head(most_pts_season, 5)

least_pts_season = arrange(wbbl_tables, Pts) %>%
  select(season, Team, Pld, Pts)
head(least_pts_season, 5)

# most & least wins
most_wins_season = arrange(wbbl_tables, desc(W)) %>%
  select(season, Team, Pld, W)
head(most_wins_season, 5)

least_wins_season = arrange(wbbl_tables, W) %>%
  select(season, Team, Pld, W)
head(least_wins_season, 5)

# most & least losses
most_losses_season = arrange(wbbl_tables, desc(L)) %>%
  select(season, Team, Pld, L)
head(most_losses_season, 5)

least_losses_season = arrange(wbbl_tables, L) %>%
  select(season, Team, Pld, L)
head(least_losses_season, 5)

# highest & lowest points achieved percentage
highest_pts_perc_season = arrange(wbbl_tables, desc(pts_achieved_perc)) %>%
  select(season, Team, Pld, Pts, max_avail_pts, pts_achieved_perc)
head(highest_pts_perc_season, 5)

lowest_pts_perc_season = arrange(wbbl_tables, pts_achieved_perc) %>%
  select(season, Team, Pld, Pts, max_avail_pts, pts_achieved_perc)
head(lowest_pts_perc_season, 5)

# most points to not win the league
most_pts_not_premiers_season = arrange(wbbl_tables, desc(Pts)) %>%
  filter(premiers == 0) %>%
  select(season, Team, Pld, Pts) 
head(most_pts_not_premiers_season, 5)

# least points to win the league
least_pts_premiers_season = arrange(wbbl_tables, Pts) %>%
  filter(premiers == 1) %>%
  select(season, Team, Pld, Pts)
head(least_pts_premiers_season, 5)

# biggest & smallest winning margin in league
most_winning_margin_season = title_race_totals %>%
  arrange(desc(margin_pts), desc(margin_NRR)) %>%
  left_join(wbbl_tables, by = c("season" = "season")) %>%
  filter(Pos == 1) %>%
  select(season, Team, margin_pts, margin_NRR)
head(most_winning_margin_season, 5)

least_winning_margin_season = title_race_totals %>%
  arrange(margin_pts, margin_NRR) %>%
  left_join(wbbl_tables, by = c("season" = "season")) %>%
  filter(Pos == 1) %>%
  select(season, Team, margin_pts, margin_NRR)
head(least_winning_margin_season, 5)


# best & worst NRR
best_nrr_season = arrange(wbbl_tables, desc(NRR_value)) %>%
  select(season, Team, Pld, NRR_value)
head(best_nrr_season, 5)

worst_nrr_season = arrange(wbbl_tables, NRR_value) %>%
  select(season, Team, Pld, NRR_value)
head(worst_nrr_season, 5)


# highest movement in final position
highest_mvmt_up_season = arrange(wbbl_tables, desc(pos_diff)) %>%
  select(season, Team, Pos, prev_pos, pos_diff)
head(highest_mvmt_up_season, 5)

highest_mvmt_down_season = arrange(wbbl_tables, pos_diff) %>%
  select(season, Team, Pos, prev_pos, pos_diff)
head(highest_mvmt_down_season, 5)

# lowest position to champion in one season
prev_pos_champion = wbbl_tables %>%
  filter(champion == 1) %>%
  select(season, Team, prev_pos) %>%
  arrange(desc(prev_pos), season)
colnames(prev_pos_champion)[1] = "champion_season"
head(prev_pos_champion, 5)

# lowest position after being champion in one season
next_pos_champion = wbbl_tables %>%
  filter(champion == 1) %>%
  select(season, Team, next_pos) %>%
  arrange(desc(next_pos), season)
colnames(next_pos_champion)[1] = "champion_season"
head(next_pos_champion, 5)

# volatility of position from year to year
pos_changes = wbbl_tables %>%
  group_by(current_name) %>%
  summarise(count_seasons = n(),
            total_pos_diff = sum(pos_abs_diff, na.rm = TRUE)) %>%
  mutate(ave_mvmt = total_pos_diff / (count_seasons - 1)) %>%
  arrange(desc(ave_mvmt))
pos_changes


# Longest streaks
longest_streaks_champion = arrange(wbbl_tables, desc(streak_champion)) %>%
  select(season, Team, streak_champion)
head(longest_streaks_champion, 5)

longest_streaks_missed_champion = arrange(wbbl_tables, desc(streak_missed_champion)) %>%
  select(season, Team, streak_missed_champion)
head(longest_streaks_missed_champion, 5)

longest_streaks_runners_up = arrange(wbbl_tables, desc(streak_runners_up)) %>%
  select(season, Team, streak_runners_up)
head(longest_streaks_runners_up, 5)

longest_streaks_premiers = arrange(wbbl_tables, desc(streak_premiers)) %>%
  select(season, Team, streak_premiers)
head(longest_streaks_premiers, 5)

longest_streaks_missed_premiers = arrange(wbbl_tables, desc(streak_missed_premiers)) %>%
  select(season, Team, streak_missed_premiers)
head(longest_streaks_missed_premiers, 5)

longest_streaks_finals = arrange(wbbl_tables, desc(streak_finals)) %>%
  select(season, Team, streak_finals)
head(longest_streaks_finals, 5)

longest_streaks_missed_finals = arrange(wbbl_tables, desc(streak_missed_finals)) %>%
  select(season, Team, streak_missed_finals)
head(longest_streaks_missed_finals, 5)

longest_streaks_grand_finals = arrange(wbbl_tables, desc(streak_grand_finals)) %>%
  select(season, Team, streak_grand_finals)
head(longest_streaks_grand_finals, 5)

longest_streaks_missed_grand_finals = arrange(wbbl_tables, desc(streak_missed_grand_finals)) %>%
  select(season, Team, streak_missed_grand_finals)
head(longest_streaks_missed_grand_finals, 5)


# no. of teams in finals
finals_teams = wbbl_tables %>% 
  filter(finals == 1) %>% 
  group_by(season, yr_end) %>% 
  summarise(finals_teams = max(Pos))

# list of all team abbreviations
teams_unique = unique(wbbl_tables$abbrev)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# checks on data for consistency
error_check_pts = wbbl_tables %>% 
  filter(!Pts == (pts_per_win * W + pts_per_draw * NR - Ded))

error_check_pld = wbbl_tables %>%
  filter(!Pld == (W + L + NR))

error_check_results = season_totals %>%
  filter(!Total_W == Total_L)

error_check_pos = group_by(wbbl_tables, season) %>%
  summarise(count = n(),
            sum_pos = sum(Pos)) %>%
  mutate(exp_sum_pos = count * (count + 1) / 2,
         pos_diff = sum_pos - exp_sum_pos) %>%   # error if calculated difference (pos_diff) is not zero
  filter(!(pos_diff == 0))

error_sorted_pos = wbbl_tables %>%
  arrange(season_no, desc(Pts), desc(NRR_value)) %>%
  mutate(sorted_row_number = row_number(),
         row_no_diff = row_number - sorted_row_number) %>%
  filter(!(row_no_diff == 0))

check_identical_pos = wbbl_tables %>%
  group_by(season_no, Pts, NRR_value) %>%
  summarise(count_seasons = n()) %>%
  filter(count_seasons > 1)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# run function to produce graph for a specific team
make_graph_wbbl("STR")   # Adelaide Strikers 
make_graph_wbbl("HEA")   # Brisbane Heat
make_graph_wbbl("HUR")   # Hobart Hurricanes
make_graph_wbbl("REN")   # Melbourne Renegades
make_graph_wbbl("STA")   # Melbourne Stars
make_graph_wbbl("SCO")   # Perth Scorchers
make_graph_wbbl("SIX")   # Sydney Sixers
make_graph_wbbl("THU")   # Sydney Thunder


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# export file to csv format
names(wbbl_all_time_points_table) <- gsub(x = names(wbbl_all_time_points_table), pattern = "_", replacement = " ") 

setwd(output_path)
save(tables, file = "wbbl_tables_raw.Rdata")
save(wbbl_tables, file = "wbbl_tables.Rdata")
write.csv(wbbl_tables, file = "wbbl_tables_full.csv")
write.csv(wbbl_all_time_points_table, file = "wbbl_all_time_points_table.csv")
setwd(path) 

# export single graph
#setwd(output_path)
#ggsave("graph_ggsave.svg")
#setwd(path)

# export multiple graphs
for (i in 1:length(teams_unique)) {
  make_graph_wbbl(teams_unique[i])
  setwd(output_path)
  #  ggsave(paste("graph_wbbl_", teams_unique[i], ".pdf", sep=""))
  ggsave(paste("performance_chart_wbbl_", teams_unique[i], ".png", sep=""))
  ggsave(paste("performance_chart_wbbl_", teams_unique[i], ".svg", sep=""))
}
setwd(path)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# End


# To do:
# Validate wikipedia data against another source.
# consider different coloured point on chart for champions - take from AFL



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Test
# read one league table manually
table = read_html("https://en.wikipedia.org/wiki/2023-24_Women's_Big_Bash_League_season")
tables_all <- table %>%
  html_nodes(".wikitable") %>%
  html_table(fill = TRUE)
table_yyyymm = tables_all[[4]]
table_yyyymm


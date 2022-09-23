transform_team_log_fd <- function(file_path) {
    team_log_table <- readr::read_csv(file_path)
}


# testing

file_path <- "data/team_logs/fuck_yeah_tom_wilson_fd_log_2018.csv"

team_log_table <- readr::read_csv(file_path)
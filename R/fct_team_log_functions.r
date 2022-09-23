transform_team_log_fd <- function(file_path) {
    team_log_table <- readr::read_csv(file_path)
}


# testing

file_path <- "data/team_logs/fuck_yeah_tom_wilson_fd_log_2018.csv"

team_log_table <- readr::read_csv(file_path, col_names = F)

for (i in seq_along(team_log_table)) {
    if (i == 0) {
        test <- team_log_table[1:11, ]
        test_t <- test %>%
            tidyr::pivot_wider(
                names_from = X1
            )
    } else if (i %% 11 == 0) {

    }
}

test <- team_log_table[1:11, ]
test_t <- t(test)


test <- team_log_table[1:11, ]
test_t <- test %>%
    tidyr::pivot_wider(
        names_from = X1
    )

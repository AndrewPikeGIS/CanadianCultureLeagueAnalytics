transform_team_log_fd <- function(file_path) {
    team_log_table <- readr::read_csv(file_path, col_names = F)
    # testing

    for (i in seq_len(nrow(team_log_table))) {
        if (i == 1) {
            team_log_header <- team_log_table[1:10, ]
            team_log_header_t <- t(team_log_header) %>%
                tibble::as_tibble(
                    .name_repair = "unique"
                )
            name_vector <- team_log_header_t %>%
                dplyr::slice(1) %>%
                as.character()

            names(team_log_header_t) <- name_vector

            team_log_table_clean <- team_log_header_t[-1, ]
        } else if (i %% 12 == 0) {
            table_selection <- team_log_table[(i + 1):(i + 10), ]
            table_selection_t <- t(table_selection) %>%
                tibble::as_tibble(
                    .name_repair = "unique"
                )
            names(table_selection_t) <- name_vector

            team_log_table_clean <- dplyr::bind_rows(
                team_log_table_clean,
                table_selection_t
            )
        }
    }
    team_log_table_clean <- head(team_log_table_clean, -1)

    team_log_table_clean <- re_class_cols(team_log_table_clean)

    team_log_table_clean <- team_log_table_clean %>%
        # janitor::tabyl() %>%
        janitor::adorn_totals("row")

    return(team_log_table_clean)
}

re_class_cols <- function(table_in) {
    table_out <- table_in %>%
        dplyr::rename(
            GP = "GP*",
            plus_minus = "+/-"
        ) %>%
        dplyr::mutate(
            GP = as.numeric(GP),
            G = as.numeric(G),
            A = as.numeric(A),
            plus_minus = as.numeric(plus_minus),
            PIM = as.numeric(PIM),
            PPP = as.numeric(PPP),
            GWG = as.numeric(GWG),
            HIT = as.numeric(HIT),
            BLK = as.numeric(BLK)
        )

    return(table_out)
}

file_path <- "data/team_logs/fuck_yeah_tom_wilson_fd_log_2018.csv"

clean_team_log <- transform_team_log_fd(file_path)
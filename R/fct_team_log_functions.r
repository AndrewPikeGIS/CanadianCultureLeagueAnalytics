read_in_table <- function(table_path) {
    team_log_table <- readr::read_csv(table_path, col_names = F)
    return(team_log_table)
}

transform_team_log_fd <- function(team_log_table) {
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
        } else if (i %% 10 == 0) {
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

    team_log_table_clean <- tidyr::drop_na(team_log_table_clean)

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
        dplyr::mutate_at(
            dplyr::vars(-Name),
            as.numeric
        )

    return(table_out)
}

add_year_and_name <- function(tib_in,
                              table_in) {
    table_name <- basename(table_in)
    team_list <- stringr::str_split(
        table_name,
        "_fd_log_"
    )
    team_name <- team_list[[1]][1]
    team_year <- team_list[[1]][2]

    team_year <- sub(".csv", "", team_year)

    table_out <- tib_in %>%
        dplyr::mutate(
            year = team_year,
            team_name = team_name
        )

    return(table_out)
}

return_team_list <- function(file_path) {
    file_list <- list.files(
        file_path,
        pattern = "*.csv"
    )
    return(file_list)
}

run_clean_table <- function(file_path,
                            file_name) {
    file_full_path <- paste0(
        file_path,
        "/",
        file_name
    )
    raw_table <- read_in_table(file_full_path)

    pre_processed_table <- team_log_pre_process(raw_table)

    clean_team_log <- transform_team_log_fd(pre_processed_table) %>%
        add_year_and_name(table_in = file_full_path)

    return(clean_team_log)
}

bind_all_tables <- function(table_list_in) {
    full_table <- dplyr::bind_rows(
        table_list_in
    )

    return(full_table)
}

write_to_csv <- function(table, file_path) {
    readr::write_csv(
        table,
        file = file_path
    )
}

team_log_pre_process <- function(team_log_table) {
    table_out <- team_log_table %>%
        dplyr::mutate(
            X1 = dplyr::na_if(X1, "?"),
            X1 = dplyr::na_if(X1, "No new player Notes")
        ) %>%
        tidyr::drop_na()
}

join_manager_table <- function(table_in) {
    manager_table <- readr::read_csv(
        file = "data/Team_Managers.csv"
    )

    table_out <- dplyr::full_join(
        table_in,
        manager_table,
        by = c("Name = Team")
    )

    return(table_out)
}
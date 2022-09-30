pkgload::load_all()

file_path <- "data/team_logs"

file_out <- "data/full_team_logs.csv"

team_list <- return_team_list(file_path)

team_logs <- lapply(team_list, FUN = run_clean_table, file_path = file_path)

full_table <- bind_all_tables(team_logs)

write_to_csv(full_table, file_out)


full_table %>%
    dplyr::filter(
        Name == "Total"
    ) %>%
    plotly::plot_ly(
        x = ~team_name,
        y = ~GP,
        type = "bar",
        color = ~year
    )
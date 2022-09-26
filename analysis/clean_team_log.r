pkgload::load_all()

file_path <- "data/team_logs"

team_list <- return_team_list(file_path)

team_logs <- lapply(team_list, FUN = run_clean_table, file_path = file_path)

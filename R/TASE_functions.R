
#' This function imports capital issuance data
#'
#' @import readxl
#'
#' @param file_path
#'
import_tase_issue_data = function(file_path = NULL){

  if(is.null(file_path)){

    file_path = paste0(
      file.path(Sys.getenv("USERPROFILE")),
      "\\OneDrive - Bank Of Israel\\Data\\",
      "TASE\\Stat_201_l01_2020.xlsx")
  }

  categories_table = read_csv(paste0(
    file.path(Sys.getenv("USERPROFILE")),
    "\\Documents\\LearningMaterials\\data",
    "\\tase_capital_issue_vars_table.csv"))

  temp_df = read_xlsx(file_path,
                     range = cell_limits(c(5, 1),c(NA_integer_,10)),
                     col_names = categories_table$tase_name_eng)


  annual_df = temp_df %>%
    filter(str_detect(date,"^\\d{4}$")) %>%
    filter(complete.cases(.)) %>%
    mutate(across(-date,as.numeric)) %>%
    pivot_longer(-date, names_to = "category")


  return(annual_df)
}

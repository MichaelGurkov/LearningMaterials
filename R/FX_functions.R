#' @description  This function imports FX rates from BOI data files
#' @title Import FX rates
#'
#' @import purrr
#'
#' @import dplyr
#'
#' @import readxl
#'
#' @import stringr
#'
#' @import lubridate
#'
#' @import tidyr

import_fx_rates = function(files_dir = NULL,
                           old_files_vec = NULL){

  if(is.null(files_dir)){

    files_dir = paste0(
      file.path(Sys.getenv("USERPROFILE")),
      "\\OneDrive - Bank Of Israel\\Data\\BoI\\FX\\FXRate\\")

  }

  if(is.null(old_files_vec)){

    old_files_vec = c("Yaz7781","Yaz8286","Yaz9296","Yaz9700",
                      "YAZIG","yazig_0712","Yaz8791")

  }

  fx_df = map_dfr(old_files_vec,function(temp_name){

    temp = read_xls(paste0(files_dir,temp_name,".xls"))

    names_vec = paste(names(temp), temp[1,]) %>%
      tolower() %>%
      str_remove_all(pattern = "[()\\.]") %>%
      str_replace_all(pattern = " ", "_")

    temp_df = temp[-1,] %>%
      setNames(names_vec) %>%
      rename_at(vars(starts_with("date")), ~"date") %>%
      mutate(across(-date, as.numeric)) %>%
      mutate(date = ymd(date))

    return(temp_df)
                  })

  fx_df = fx_df %>%
    pivot_longer(-date, names_to = "currency")

  temp = read_xlsx(paste0(files_dir,"yazigmizt",".xlsx")) %>%
    rename_all(tolower) %>%
    rename_all(~str_replace_all(.," ","_")) %>%
    mutate(across(-date, as.numeric)) %>%
    mutate(date = ymd(date))%>%
    pivot_longer(-date, names_to = "currency")

  fx_df = fx_df %>%
    rbind(temp)


  return(fx_df)

}



#' This function imports trade volume in fx
#'
#' @details The files are in different formats
#'  - In the period 2001 - 2008 the data format includes data
#'   about the activity of three groups : domestic banks,
#'    foreign financial institutions and other clients.

import_fx_trade_volume = function(dir_path = NULL){

  if(is.null(dir_path)){

    dir_path = paste0(
      file.path(Sys.getenv("USERPROFILE")),
      "\\OneDrive - Bank Of Israel\\Data\\BoI\\FX\\TradeVolume\\")

  }


  old_format_data = map_dfr(paste0("0",c(1:8,"91")), function(temp_ind){

    temp_df = import_fx_volume_workbook_old_format(
      file_path = paste0(dir_path,"trdvol",temp_ind,".xls")
      )


  })

  old_format_data = old_format_data %>%
    mutate(value = as.numeric(value))


  new_format_list = map(c("09",11:16), function(temp_ind){

    temp_list = import_fx_volume_workbook_new_format(
      file_path = paste0(dir_path,"trdvol",temp_ind,".xls")
      )
 })

  daily_df = map_dfr(new_format_list, function(temp_list){

    return(temp_list[["daily_df"]])

  })

  weekly_df = map_dfr(new_format_list, function(temp_list){

    return(temp_list[["weekly_df"]])

  })


  return(list(old_format_data = old_format_data,
              daily_df = daily_df, weekly_df = weekly_df))









}


#' This is a helper (internal) function that imports volume data for
#' each player category based on old format
#'
#'

import_fx_volume_workbook_old_format = function(file_path){

  import_fx_volume_sheet_data = function(file_path, sheet_ind){

    temp_df = suppressMessages(
      read_xls(
      path = file_path,
      sheet = sheet_ind,
      range = cell_cols("A:U"))
      )


      data_df  = temp_df %>%
      select(c(1,3,4,5,9,10,17,18)) %>%
      set_names(
        "date",
        "min_col",
        "foreign_inst-inc_swaps",
        "foreign_inst-no_swaps",
        "other_clients-inc_swaps",
        "other_clients-no_swaps",
        "domestic_banks-inc_swaps",
        "domestic_banks-no_swaps") %>%
      mutate(date = as.numeric(date)) %>%
      filter(is.na(min_col) & !is.na(date)) %>%
      select(-min_col) %>%
      mutate(date = as.Date(date, origin = "1899-12-30")) %>%
      pivot_longer(-date,names_to = "cat_type") %>%
      separate(col = cat_type,into = c("category","type"), sep = "-")

    return(data_df)

  }


  volume_df = file_path %>%
    excel_sheets() %>%
    set_names() %>%
    map_dfr(import_fx_volume_sheet_data, file_path = file_path)

  return(volume_df)

}


#' This is a helper (internal) function that imports volume data for
#' each player category based on old format
#'
#'

import_fx_volume_workbook_new_format = function(file_path){

  existing_sheets = excel_sheets(file_path)

  daily_df = read_xls(path = file_path,
                      sheet = existing_sheets[1],
                      range = cell_cols("A:D"),
                      col_names = c(
                        "date",
                        "conversion",
                        "swap",
                        "otc")) %>%
    mutate(date = dmy(date)) %>%
    filter(complete.cases(.)) %>%
    mutate(across(-date, as.numeric))


  weekly_df = read_xls(path = file_path,
                       sheet = existing_sheets[2],
                       range = cell_cols("A:J"),
                       col_names = c(
                         "date",
                         "foreign-conversion",
                         "domestic-conversion",
                         "total-conversion",
                         "foreign-swap",
                         "domestic-swap",
                         "total-swap",
                         "foreign-otc",
                         "domestic-otc",
                         "total-otc"
                       )) %>%
    mutate(date = dmy(date)) %>%
    filter(complete.cases(.)) %>%
    mutate(across(-date, as.numeric)) %>%
    pivot_longer(-date, names_to = "cat_type") %>%
    separate("cat_type",into = c("category","type"),sep = "-") %>%
    filter(!category == "total")

  # monthly_for_df = read_xls(path = file_path,
  #                           sheet = "חודשי - תושבי חוץ",
  #                           range = cell_cols("A:I"),
  #                           col_names = c(
  #                             "date",
  #                             "foreign_inst-conversion",
  #                             "foreign_inst-swap",
  #                             "foreign_inst-otc",
  #                             "foreign_inst-total",
  #                             "other_foreigners-conversion",
  #                             "other_foreigners-swap",
  #                             "other_foreigners-otc",
  #                             "other_foreigners-total"
  #                           )) %>%
  #   filter(complete.cases(.)) %>%
  #   mutate(across(-date, as.numeric)) %>%
  #   pivot_longer(-date, names_to = "cat_type") %>%
  #   separate("cat_type",into = c("category","type"),sep = "-") %>%
  #   filter(!category == "total")

  return(list(daily_df = daily_df, weekly_df = weekly_df))

}


#' This is a helper function that imports band data
#'
#' @import readxl
#'
import_band_data = function(dir_path = NULL){

  if(is.null(dir_path)){

    dir_path = paste0(
      file.path(Sys.getenv("USERPROFILE")),
      "\\OneDrive - Bank Of Israel\\Data\\BoI\\FX\\Band\\")
  }

  band_df = map_dfr(c("band2000h",
        "band2001h",
        "band2003h",
        "band2004h",
        "band2005h",
        "BAND8999"), function(temp_name){

          temp_df = read_xls(
            path = paste0(dir_path, temp_name,".xls"),
            range = cell_cols("A:C"),
            col_names = c("date","lower_bound","upper_bound")) %>%
            mutate(date = as.numeric(date)) %>%
            filter(complete.cases(.)) %>%
            mutate(date = as.Date(date, origin = "1899-12-30"))

        }) %>%
    mutate(across(-date,as.numeric))

  return(band_df)



}

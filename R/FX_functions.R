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

import_fx_trade_volume = function(){}


#' This is a helper (internal) function that imports volume data for
#' each player category based on column position
#'
#'

import_fx_volume_workbook = function(file_path,start_row){

  import_fx_volume_sheet_data = function(file_path, sheet_ind,
                                         start_row){

    dates_df = read_xls(
      path = file_path,
      sheet = sheet_ind,
      range = cell_limits(ul = c(start_row,1),lr = c(NA_integer_,1)),
      col_names = "date") %>%
      mutate(date = as_date(date)) %>%
      mutate(date = update(date, year = 2001))

    foreign_fin_inst_df = read_xls(
      path = file_path,
      sheet = sheet_ind,
      range = cell_limits(ul = c(start_row,4),lr = c(NA_integer_,5)),
      col_names = c("vol_net_swaps","vol_including_swaps")) %>%
      mutate(category = "foreign_fin_inst")

    other_clients_df = read_xls(
      path = file_path,
      sheet = sheet_ind,
      range = cell_limits(ul = c(start_row,9),lr = c(NA_integer_,10)),
      col_names = c("vol_net_swaps","vol_including_swaps")) %>%
      mutate(category = "other_clients")

    domestic_banks_df = read_xls(
      path = file_path,
      sheet = sheet_ind,
      range = cell_limits(ul = c(start_row,17),lr = c(NA_integer_,18)),
      col_names = c("vol_net_swaps","vol_including_swaps")) %>%
      mutate(category = "domestic_banks")


    df = map_dfr(list(foreign_fin_inst_df,
                      domestic_banks_df,
                      other_clients_df),
                 function(temp_df){

                   temp_df = temp_df %>%
                     cbind(dates_df) %>%
                     pivot_longer(
                       c("vol_net_swaps","vol_including_swaps"),
                       names_to = "volume_type")

                   return(temp_df)


                 })


  }


  volume_df = file_path %>%
    excel_sheets() %>%
    set_names() %>%
    map_dfr(import_fx_volume_sheet_data,
            start_row = start_row, file_path = file_path)

  return(volume_df)

}

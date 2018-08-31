#' getAE_data
#'
#' @param update_data whether to download files afresh from ISD Scotland website (TRUE)
#' or use existing downloaded files (FALSE)
#' @param directory directory to find existing downloaded files, and to save new downloads
#' @param url_list list of urls (as strings) for the pages to scrape for data files
#' @param use_filename_date if TRUE, take dates from the Excel file's name, if FALSE,
#' take from the date specified inside the sheet
#'
#' @return A data frame containing all the monthly A&E data from the ISD Scotland website.
#' @export
#'
#' @examples
#' AE_data <- getAE_data(directory = file.path('nhsAEscraperScotland','sitreps'))
#' str(AE_data)
getAE_data <- function(update_data = TRUE, directory = file.path('data-raw','sitreps'),
                       url_list = NULL, use_filename_date = FALSE) {
  
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  
  if(update_data) {
    urls <- getAEdata_urls_monthly(url_list = url_list)
    download_AE_files(urls, directory = directory)
  }
  rawDataList <- load_AE_files(directory = directory, use_filename_date = use_filename_date)
  
  rawDataList <- lapply(rawDataList, delete_extra_columns)
  
 # if(!all(unlist(lapply(rawDataList, check_format)))) {
 #   stop('There is a problem with the format of the data in one or more of the files')
 # }
  
  cleanDataList <- lapply(rawDataList, clean_AE_data)
  
  AE_data <- dplyr::bind_rows(cleanDataList)
  
  AE_data
  
}

###actually gives weekly data. This wil be sorted out in the app itself 
#' getAEdata_urls_monthly
#'
#' @param url_list list of urls (as strings) for the pages to scrape for data files
#'
#' @return the urls for ISD Scotland A&E data *.csv files from pages in url_list
#' yielding addresses for weekly data from June 2015 to (in principle) present.
#' @export
#'
#' @examples
#' urls <- getAEdata_urls_monthly()
#' head(urls, n = 3)
getAEdata_urls_monthly <- function(url_list = NULL) {
  
  if(is.null(url_list)) {
    
    url_15_18 <- "http://www.isdscotland.org/Health-Topics/Emergency-Care/Publications/data-tables2017.asp?id"
    url_list <- list(url_15_18)
    
  }
  
  #Scotish data : each csv file contains previous data already so only need most recent file
  #but also downloads board and whole of Scotland data
  url_vect <- unlist(lapply(url_list,function(x) getAEdata_page_urls_monthly(x)))
  url_vect
}


#' getAEdata_page_urls_monthly
#'
#' @param index_url the url of the page to scrape data files from
#'
#' @return character vector of the urls for ISD Scotland A&E data *.csv files
#' from one of the weekly data index pages
#'
#' @export
#'
#' @examples
#' urls <- getAEdata_page_urls_monthly(paste0('https://www.england.nhs.uk/statistics/',
#' 'statistical-work-areas/ae-waiting-times-and-activity/',
#' 'ae-attendances-and-emergency-admissions-2017-18/'))
#' head(urls, n = 3)
getAEdata_page_urls_monthly <- function(index_url) {
  
  #Get the html from the index website. n=3350 argument stops it from reading last line of webpage which has a error in it thus avoiding a warning message.
  con <- url(index_url, "r")
  html_lines <- readLines(con, n = 3350)
  
  #Close connection
  close(con)

  hosp_data_url_lines <- grep("ED-Weekly-Hospital-Data",html_lines)
  board_data_url_lines <- grep("ED-Weekly-NHSBoard-Data",html_lines)
  scot_data_url_lines <- grep("ED-Weekly-NHSScotland-Data",html_lines)
  NHSS_csvdata_lines_hosp <- html_lines[hosp_data_url_lines]
  NHSS_csvdata_lines_board <- html_lines[board_data_url_lines]
  NHSS_csvdata_lines_scot <- html_lines[scot_data_url_lines]
  
  #Extract urls from html lines
  #starts <- regexpr("http",NHSS_csvdata_lines)
  #ends <- regexpr(".csv",NHSS_csvdata_lines) + 3
  urls_hosp <- substr(NHSS_csvdata_lines_hosp, regexpr("http",NHSS_csvdata_lines_hosp), regexpr(".csv",NHSS_csvdata_lines_hosp) + 3)
  urls_board <- substr(NHSS_csvdata_lines_board, regexpr("http",NHSS_csvdata_lines_board), regexpr(".csv",NHSS_csvdata_lines_board) + 3)
  urls_scot <- substr(NHSS_csvdata_lines_scot, regexpr("http",NHSS_csvdata_lines_scot), regexpr(".csv",NHSS_csvdata_lines_scot) + 3)

  urls <- c(urls_hosp[1], urls_board[1], urls_scot[1])
  
  #Return urls
  return(urls)

}


#' download_AE_files
#'
#' @param file_urls list of urls of files to download
#' @param directory location to save files to
#'
#' @return vector of download.file return values
#' @export
#'
#' @examples
#' urls <- getAEdata_page_urls_monthly(paste0('https://www.england.nhs.uk/statistics/',
#' 'statistical-work-areas/ae-waiting-times-and-activity/',
#' 'ae-attendances-and-emergency-admissions-2017-18/'))
#' download_AE_files(urls[1], directory = file.path('nhsAEscraper','sitreps'))
download_AE_files <- function(file_urls, directory) {
  
  file.remove(
    dir(directory,
        pattern = "*",
        full.names = TRUE)
  )
  
  f_name_regex <- '/([^/]+)$'
  
  lapply(file_urls, function(x) {
    fn <- file.path(directory, stringr::str_match(x, f_name_regex)[,2])
    httr::GET(x, httr::write_disk(fn, overwrite = TRUE))
  })
  
}


#' load_AE_files
#'
#' @param directory path of the directory to load files from
#'
#' @return a list of data frames containing data loaded from files in directory
#' whose name is of the form '\*-Data\*.csv'
#' @export
#'
#' @examples
#' dataList <- load_AE_files(directory = file.path('nhsAEscraperScotland','sitreps'))
#'
load_AE_files <- function(directory = file.path('data-raw','sitreps'), use_filename_date = TRUE) {
  
  #fileNames <- Sys.glob(file.path(directory,'*AE-by-provider*.xls'))
  fileNames <- Sys.glob(file.path(directory,'*-Data*.csv'))
  dataList <- NULL
  dataList <- lapply(fileNames, function(x) {
    cat(file=stderr(), "Loading: ", x, "\n")
    df <- utils::read.csv(x)
    cat(file=stderr(), "Success loaded: ", x, "\n")
    #if(use_filename_date) {
    #  dt_chr <- stringr::str_replace(
    #    stringr::str_match(x, '/(([0-9A-Za-z]|-)*)-ED-Weekly-Hospital-Data')[,2], '-', ' '
    #  )
    #  df <- df %>% dplyr::mutate(X__2 = ifelse(X__1 == 'Period:', dt_chr, X__2))
    #}
    df
  })
  dataList
}

##do I still need this?
# Tell codetools not to worry about no visible binding for default imported data column names
#if(getRversion() >= "2.15.1") {
#  utils::globalVariables(c("X__1", "X__2", "X__3", "X__4", "X__5", "X__6", "X__7", "X__8",
#                           "X__9", "X__10", "X__11", "X__12", "X__13", "X__14", "X__15", "X__16",
#                           "X__17", "X__18", "X__19", "X__20", "X__21"))
#}


#' clean_AE_data
#'
#' @param raw_data dataframe containing a ISD Scotland A&E weekly report
#' with a standardised set of columns
#'
#' @return the same data as raw_data, as a rectangular table with header removed,
#' new column names, and correct numerical data types for numerical columns
#' @importFrom magrittr %>%
#'
#'
clean_AE_data <- function(raw_data) {
  
  #data_date <- get_date(raw_data)
  
  #clean_data <- raw_data %>% dplyr::filter(grepl("^[A-Z0-9]+$",X__1))
  clean_data <- raw_data
  
  clean_data <- clean_data %>% dplyr::select(X__1:X__12) %>%
    dplyr::rename(Week_End = X__1,
                  Board_Code = X__2,
                  Board_Name = X__3,
                  Prov_Code = X__4,
                  Prov_Name = X__5,
                  Att_All = X__6,
                  Att_4hr_Br = X__7,
                  Perf_4hr = X__8,
                  Att_8hr_Br = X__9,
                  Perf_8hr = X__10,
                  Att_12hr_Br = X__11,
                  Perf_12hr = X__12
                  )
  
  
  # Explicitly replace Excel 'N/A' with NA_character_
  #clean_data <- clean_data %>%
  #  dplyr::mutate(Perf_4hr = dplyr::case_when(as.character(Perf_4hr) == 'N/A' ~ NA_character_,
  #                                             as.character(Perf_4hr) == '-' ~ NA_character_,
  #                                             as.character(Perf_4hr) != 'N/A' ~ Perf_4hr))
  #clean_data <- clean_data %>%
  #  dplyr::mutate(Perf_8hr = dplyr::case_when(Perf_8hr == 'N/A' ~ NA_character_,
  #                                            Perf_8hr == '-' ~ NA_character_,
  #                                            Perf_8hr != 'N/A' ~ Perf_8hr))
  #clean_data <- clean_data %>%
  #  dplyr::mutate(Perf_12hr = dplyr::case_when(Perf_12hr == 'N/A' ~ NA_character_,
  #                                            Perf_12hr == '-' ~ NA_character_,
  #                                            Perf_12hr != 'N/A' ~ Perf_12hr))
  
  clean_data <- clean_data %>% 
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("Att_")), dplyr::funs(as.numeric)) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("Perf_")), dplyr::funs(as.numeric)) %>%
    dplyr::mutate(Week_End = as.Date(Week_End)) %>%
    dplyr::mutate(Board_Code = as.character(Board_Code)) %>%
    dplyr::mutate(Board_Name = as.character(Board_Name)) %>%
    dplyr::mutate(Prov_Name = as.character(Prov_Name)) %>%
    dplyr::mutate(Prov_Code = as.character(Prov_Code)) %>%
    dplyr::mutate(Prov_Name = ifelse(startsWith(Prov_Name, "NHS"), sub("NHS","Board:",Prov_Name), Prov_Name)) %>%
    dplyr::mutate(Prov_Name = ifelse(endsWith(Board_Name, "Scotland"), "Whole of Scotland", Prov_Name))
  

  clean_data
}


#' check_format
#'
#' @param raw_data a data frame containing A&E provider data
#' for one month, from the NHS England website
#' @param verbose control level of detail returned
#'
#' @return boolean indicating whether data frame is in correct format
#' for analysis. Length 1 if verbose = FALSE, length 6 if not - in this case
#' each element pertains to a specific aspect of raw_data that is checked, respectively:
#' 1 - column 1 contains the heading Code
#' 2 - column 2 contains the heading Region
#' 3 - column 3 contains the heading Name
#' 4 - column 4 contains the heading A&E attendances
#' 5 - column 8 contains the heading A&E attendances > 4 hours from arrival to admission
#' 6 - column 14 contains the heading Emergency Admissions
#' Note that the 5th element allows the words greater than as well as the symbol.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   dataList <- load_AE_files(directory = 'nhsAEscraper/sitreps/')
#'   check_format(dataList[[1]], verbose = TRUE)
#' }
check_format <- function(raw_data, verbose = FALSE) {
  
  format_status <- logical()
  
  format_status[1] <- nrow(raw_data %>% dplyr::filter(X__1 == "Code")) == 1
  format_status[2] <- nrow(raw_data %>% dplyr::filter(X__2 == "Region")) == 1
  format_status[3] <- nrow(raw_data %>% dplyr::filter(X__3 == "Name")) == 1
  format_status[4] <- nrow(raw_data %>% dplyr::filter(X__4 == "A&E attendances")) == 1
  format_status[5] <- nrow(raw_data %>% dplyr::filter(grepl("A&E attendances > 4 hours from arrival to admission",X__8)|grepl("A&E attendances greater than 4 hours from arrival to admission",X__8))) == 1
  format_status[6] <- nrow(raw_data %>% dplyr::filter(X__14 == "Emergency Admissions")) == 1
  
  
  if (verbose) {
    format_status
  } else {
    all(format_status)
  }
}


#' get_date
#'
#' @param raw_data a data frame containing A&E provider data
#' for one month, from the NHS England website
#'
#' @return the period (month) that this data pertains to
#' @export
#'
#' @examples
#' dataList <- load_AE_files(directory = 'nhsAEscraper/sitreps/')
#' get_date(dataList[[1]])
#'
get_date <- function(raw_data) {
  #Find the cell specifying the period and extract the text
  date_chr <- raw_data %>% dplyr::filter(X__1 == "Period:") %>%
    dplyr::pull(X__2)
  lubridate::myd(paste(date_chr,'1st',sep=' '), tz = "Europe/London")
  
}

#' delete_extra_columns
#'
#' @param df a data frame containing A&E provider data
#' for one month, from the ISD Scotland website
#'
#' @return df with superfluous columns removed
#'
delete_extra_columns <- function(df) {
  #format_type_x <- nrow(df %>% dplyr::filter(grepl("A&E attendances less than 4 hours from arrival to admission",X__8))) == 1
  #if(!format_type_x) return(df)
  df <- dplyr::select(df, -c(data_source))
  colnames(df) <- paste('X__',c(1:ncol(df)),sep='')
  df
}

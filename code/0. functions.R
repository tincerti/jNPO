# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# DESCRIPTION ----
# ______________________________________________________________________________

# Last updated 6 April, 2022 by Trevor Incerti

# This file contains functions used throughout code cleaning

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# FUNCTION TO IMPORT AND APPEND ALL FILES FROM A DIRECTORY ----
# ______________________________________________________________________________

# Import/define pipe operator from magrittr ------------------------------------
`%>%` <- magrittr::`%>%`

# Helper functions -------------------------------------------------------------
read_flnm <- function(flnm, delim = NULL, skip = NULL) {
  read_delim(flnm, delim = delim, skip = skip, 
             col_types = cols(.default = "c")) %>% 
    mutate(filename = tools::file_path_sans_ext(fs::path_file(flnm)))
}

read_flnm_xl <- function(flnm, sheet = NULL, skip = NULL, col_types = NULL) {
  readxl::read_excel(flnm, sheet = sheet, skip = skip, col_types = col_types) %>% 
    mutate(filename = tools::file_path_sans_ext(fs::path_file(flnm)))
}

# Main function: read in and append all files in a directory ------------------ 
# Function arguments:
# Path = filepath of directory where data files are located.
# Extension = data files extension. Currently accepts:
# all extensions compatible with readr::read_delim and "xlsx" for Excel.
# delim = Single character used to separate fields within a record, e.g. ",".
# sheet = Sheet to import if importing from Excel. 
# skip = Number of rows to skip when importing each file.

read_dir = function(path, extension, delim, filename, sheet = NULL, skip = 0,
                    col_types = NULL) {
  
  # Stop and display errors if conflicting arguments are entered
  if (!missing(sheet) & extension != "xlsx") {
    stop("Error: Argument 'sheet' only applies to Excel files")
    
    # Read in delimited text data files
  } else if (filename == FALSE & extension != "xlsx") {
    list.files(path = path,
               pattern = paste0("*.", extension),
               full.names = T) %>%
      purrr::map_df(~read_delim(., delim = delim, skip = skip, 
                                col_types = cols(.default = "c")))
    
  } else if (filename == TRUE & extension != "xlsx") {
    list.files(path = path,
               pattern = paste0("*.", extension),
               full.names = T) %>%
      purrr::map_df(~read_flnm(., delim = delim, skip = skip))
    
    # Read in Excel data files  
  } else if (extension == "xlsx" & filename == F) {
    list.files(path = path,
               pattern = paste0("*.", extension),
               full.names = T) %>%
      purrr::map_df(~readxl::read_excel(., sheet = sheet, skip = skip,
                                        col_types = col_types))
    
  } else if (extension == "xlsx" & filename == T) {
    list.files(path = path,
               pattern = paste0("*.", extension),
               full.names = T) %>%
      purrr::map_df(~read_flnm_xl(., sheet = sheet, skip = skip,
                                  col_types = col_types))
  }
}

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# FUNCTIONS FOR CONVERSION OF JAPANESE DATA FORMATTING ----
# ______________________________________________________________________________

#### Function to convert from full-width to half-width characters ####
zen2han <- function(s){
    if(any(Encoding(s) != "UTF-8"))  s <- iconv(s, from = "", to = "UTF-8")
    s <- paste(s, sep='', collapse='')
    y <- sapply(unlist(strsplit(s, split = "")), function(x){
        i <- utf8ToInt(x)
        if(i >= 65281 && i <= 65374){
            return(intToUtf8(i - 65248))
        }else{
            return(x)
        }
    })
    return(paste(y, collapse = ""))
}

sanitizeZenkaku <-function(s){
  stopifnot(is.character(s))
  if(Encoding(s[1]) != "UTF-8")  s <- iconv(s, from = "", to = "UTF-8")
  zenEisu <- paste0(intToUtf8(65295 + 1:10), intToUtf8(65312 + 1:26),
                    intToUtf8(65344 + 1:26))
  zenKigo <- c(65281, 65283, 65284, 65285, 65286, 65290, 65291,
               65292, 12540, 65294, 65295, 65306, 65307, 65308,
               65309, 65310, 65311, 65312, 65342, 65343, 65372,
               65374)
  s <- chartr(zenEisu,"0-9A-Za-z", s)
  s <- chartr(intToUtf8(zenKigo), '!#$%&*+,-./:;<=>?@^_|~', s)
  s <- gsub(intToUtf8(12288), "", s)
  return(s)
}

#### Function to convert Kanji to Romaji ####
kakasi <- function(x, kakasi.option="-Ha -Ka -Ja -Ea -ka",
                   ITAIJIDICTPATH=Sys.getenv("ITAIJIDICTPATH", unset = NA),
                   KANWADICTPATH=Sys.getenv("KANWADICTPATH", unset = NA)){
  if(is.na(ITAIJIDICTPATH)){
    Sys.setenv(ITAIJIDICTPATH=.set.dict("itaijidict"))
  }else{
    stopifnot(file.exists(ITAIJIDICTPATH))
    Sys.setenv(ITAIJIDICTPATH=ITAIJIDICTPATH)
  }
  if(is.na(KANWADICTPATH)){
    Sys.setenv(KANWADICTPATH=.set.dict("kanwadict"))
  }else{
    stopifnot(file.exists(KANWADICTPATH))
    Sys.setenv(KANWADICTPATH=KANWADICTPATH)
  }
  stopifnot(is.character(x))
  ops <- strsplit(kakasi.option, " ")[[1]]
  ## This is a trick to work correctly
  ops <- c(" ", ops)
  stopifnot(length(ops) != 0)
  lc.ctype <- Sys.getlocale("LC_CTYPE")
  if (lc.ctype != "ja_JP.UTF-8" || lc.ctype != ""){
    if (Sys.getlocale("LC_CTYPE") == "Japanese_Japan.932") x <- sjis2utf8(x)
  }else{
    warning("kakasi() assumes \"ja_JP.UTF-8\" for LC_TYPE")
  }
  u <- sapply(x, function(i){
    .Call("rkakasi", x = i, k = ops, PACKAGE = "Nippon")
  })
  return(u)
}

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# COALESCE JOIN FUNCTION (A LA SQL) ----
# ______________________________________________________________________________

coalesce_join <- function(x, y, 
                          by = NULL, suffix = c(".x", ".y"), 
                          join = dplyr::left_join, ...) {
  joined <- join(x, y, by = by, suffix = suffix, ...)
  # names of desired output
  cols <- union(names(x), names(y))
  
  to_coalesce <- names(joined)[!names(joined) %in% cols]
  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
  # remove suffixes and deduplicate
  to_coalesce <- unique(substr(
    to_coalesce, 
    1, 
    nchar(to_coalesce) - nchar(suffix_used)
  ))
  
  coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
    joined[[paste0(.x, suffix[1])]], 
    joined[[paste0(.x, suffix[2])]]
  ))
  names(coalesced) <- to_coalesce
  
  dplyr::bind_cols(joined, coalesced)[cols]
}

## Import libraries
library(httr)
library(purrr)
library(tm)
library(tidyverse)
library(pdftools)

map_null_chr <- function(.x, .f) {
  map(.x, .f) %>%
    map_if(is_empty, ~ NA_character_) %>%
    flatten_chr()
}

batch_upload <- c("Lietuvos-sveikatos-mokslų-studentų-ir-jaunųjų-tyrėjų-konferencija-2004.pdf",
           "Lietuvos-sveikatos-mokslų-studentų-ir-jaunųjų-tyrėjų-konferencija-2005-I-d..pdf",
           "Lietuvos-sveikatos-mokslų-studentų-ir-jaunųjų-tyrėjų-konferencija-2005-II-d..pdf",
           "Lietuvos-sveikatos-mokslų-studentų-ir-jaunųjų-tyrėjų-konferencija-2006.pdf",
           "Lietuvos-sveikatos-mokslų-studentų-ir-jaunųjų-tyrėjų-konferencija-2007.pdf",
           "Lietuvos-sveikatos-mokslų-studentų-ir-jaunųjų-tyrėjų-konferencija-2008.pdf",
           "Lietuvos-sveikatos-mokslų-studentų-ir-jaunųjų-tyrėjų-konferencija-2009.pdf",
           "Lietuvos-sveikatos-mokslų-studentų-ir-jaunųjų-tyrėjų-konferencija-2010.pdf",
           "Lietuvos-sveikatos-mokslų-studentų-ir-jaunųjų-tyrėjų-konferencija-2011.pdf",
           "Jaunųjų-mokslininkų-ir-tyrėjų-konferencija-2012-m.-I-knyga.pdf",
           "Jaunųjų-mokslininkų-ir-tyrėjų-konferencija-2012-m.-II-knyga.pdf",
           "Jaunųjų-mokslininkų-ir-tyrėjų-konferencija-2013-m..pdf",
           "Jaunųjų-mokslininkų-ir-tyrėjų-konferencija-2014-m.-86-MB-.pdf",
           "Jaunųjų-mokslininkų-ir-tyrėjų-konferencija-2015-m.-compressed.pdf",
           "JMTK16-tezių-knyga.pdf")

not_batch_names <- c("Tezių-knyga-2017.pdf", "Tezių-knyga-2019.pdf")
not_batch <- c("https://smd.lt/wp-content/uploads/2017/09/Tezių-knyga-2017.pdf",
               "https://smd.lt/download/2019-jaunuju-mokslininku-ir-tyreju-konferencija/?wpdmdl=19446")

url_batch_upload <- paste0("https://smd.lt/wp-content/uploads/2016/12/", batch_upload)
pdf_links <- c(url_batch_upload,not_batch)
pdf_names <- c(batch_upload,not_batch_names)

dir.create("../pdf_download")

purrr::map2(pdf_links, pdf_names, function(x,y) GET(x, write_disk(glue::glue("../pdf_download/{y}"), overwrite = TRUE)))


year <- c(2004,2005,2005.2,2006:2011,2012,2012.2,2013:2017,2019)



scrapePvals <- function(x,y) {

  doc <- pdf_text(glue::glue("../pdf_download/{x}"))
  abstract_split <- ifelse(y == "2019","[Aa]utor.{1,10}?:","[Vv]adov.{1,10}?:")
  doc_col <- paste(doc, collapse = "\r\n")
  abstracts <- strsplit(doc_col, split = abstract_split)[[1]]
  results <- strsplit(abstracts, split = "\\n.{0,10}?[Rr]ezultatai.{0,1}?\\r") %>%
    map_null_chr(., 2) %>%
    strsplit(., split = "Išvad.+?\\r") %>%
    map_null_chr(., 1)
  methods <- strsplit(abstracts, split = "\\n.{0,10}?[Mm]etod.{0,4}?\\r") %>%
    map_null_chr(., 2) %>%
    strsplit(., split = "\\n.{0,10}?[Rr]ezultatai.{0,1}?\\r") %>%
    map_null_chr(., 1)

  parsed_abstract <- tibble(Year = y,
                            ID = 1:length(results),
                            Results = results,
                            Methods = methods)

  # Contains n elements for each abstract with truncated p values. If there are no p values, NA is returned and stored as a list element.

  # For truncated supposedly significant p values
  temp <- strsplit(results, split = "[[:space:](][Pp][[:space:]\\r\\n]{0,4}?[<≤0]") %>%
    map(., function(x) str_extract_all(x, "([\\.\\,][[:space:]]{0,1}?[0-9]+)")) %>%
    map(., function(x) map(x, 1))

  data_tr <- tibble(ID = 1:length(temp), Year = y, pval = temp, truncated = 1) %>%
    unnest(pval) %>% group_by(ID) %>% slice(-1) %>% ungroup()

  data_tr$pval <- gsub(",", ".", data_tr$pval)
  data_tr$pval <- gsub("\\s", "", data_tr$pval)
  data_tr$pval <- paste0(0,data_tr$pval) %>% as.numeric()
  data_tr$significant <- ifelse(data_tr$pval < 0.05,1,0)


  # For untruncated p values
  temp <- strsplit(results, split = "[[:space:](][Pp][[:space:]\\r\\n]{0,4}?[=]") %>%
    map(., function(x) str_extract_all(x, "([\\.\\,][[:space:]]{0,1}?[0-9]+)")) %>%
    map(., function(x) map(x, 1))

  data_untr <- tibble(ID = 1:length(temp), Year = y, pval = temp, truncated = 0) %>%
    unnest(pval) %>% group_by(ID) %>% slice(-1) %>% ungroup()

  data_untr$pval <- gsub(",", ".", data_untr$pval)
  data_untr$pval <- gsub("\\s", "", data_untr$pval)
  data_untr$pval <- ifelse(data_untr$pval == "NULL", 1, paste0(0,data_untr$pval)) %>% as.numeric()
  data_untr$significant <- ifelse(data_untr$pval < 0.05,1,0)

  #For truncated supposedly nonsignificant p values
  temp <- strsplit(results, split = "[[:space:](][Pp][[:space:]\\r\\n]{0,4}?[≥>]") %>%
    map(., function(x) str_extract_all(x, "([\\.\\,][[:space:]]{0,1}?[0-9]+)")) %>%
    map(., function(x) map(x, 1))

  data_more <- tibble(ID = 1:length(temp), Year = y, pval = temp, truncated = 1) %>%
    unnest(pval) %>% group_by(ID) %>% slice(-1) %>% ungroup()

  data_more$pval <- gsub(",", ".", data_more$pval)
  data_more$pval <- gsub("\\s", "", data_more$pval)
  data_more$pval <- ifelse(data_more$pval == "NULL", 1, paste0(0,data_more$pval)) %>% as.numeric()
  data_more$significant <- ifelse(data_more$pval < 0.05,1,0)

  # Combine all the lists into one
  data_full <- bind_rows(data_untr,data_tr,data_more)

  # Build a list of abstracts and parsed p values
  abstracts_list <- list(Abstracts = parsed_abstract, pvals = data_full)

  return(abstracts_list)

}

# Run the function over the list of abstract books and years
pvals_year <- map2(pdf_names,year,scrapePvals)

#Have two working datasets for ease of access
abstract_data <- map_df(pvals_year,1)
pvalue_data <- map_df(pvals_year,2)

# Getting the number of abstracts when the abstracts are divided into two books.
#Adding the number of abstracts from the first volume to the second volume essentially connects them.

length_of_year <- function(set,x,y) {
  set[set$Year == paste0(y,".2"),] %>%
    mutate(ID = ID + nrow(x[x$Year == y,])) %>%
    mutate(Year = y)
}

# I know I should be writing another function but assigning to multiple objects is a pain
abstract_data[abstract_data$Year == 2005.2,] <- length_of_year(abstract_data,abstract_data,2005)
pvalue_data[pvalue_data$Year == 2005.2,] <- length_of_year(pvalue_data,abstract_data,2005)
abstract_data[abstract_data$Year == 2012.2,] <- length_of_year(abstract_data,abstract_data,2012)
pvalue_data[pvalue_data$Year == 2012.2,] <- length_of_year(pvalue_data,abstract_data,2012)

# Save both of the datasets into one .rda file
save(abstract_data,pvalue_data, file = "pvalue_data.rda")









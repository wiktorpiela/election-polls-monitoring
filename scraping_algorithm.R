library("tidyverse")
library("shiny")
library("rvest")
library("googlesheets4")


url <- "https://ewybory.eu/sondaze/"

polls_scraping <- function(url){
  
  content <- read_html(url) %>% 
    html_nodes(".section_polls_row")
  
  #header
  header <- content[[1]]
  
  
  header <- header %>% 
    html_nodes(".section_polls_party_border") %>% 
    html_text() %>% 
    as_tibble() %>% 
    mutate(value = fct_recode(value,
                              LEWICA="LEWICALEW",
                              `POLSKA 2050`="POLSKA 2050P50",
                              KONFEDERACJA ="KONFEDERACJAKON"))
  
  
  #procenty
  headers <- rep(list(header),length(content))
  
  
  procenty <- list()
  temp <- list()
  
  start <- Sys.time()
  
  for(x in 2:length(content)){
    
    if(nrow(as_tibble(html_text(html_nodes(content[[x]],".section_polls_data")))) > 0){
      
      temp[[x]] <- content[[x]] %>%
        html_nodes(".section_polls_data") %>%
        html_text() %>%
        as_tibble()
      
    } else {
      
      next
      
    }
    
    procenty[[x]] <- bind_cols(headers[[x]],temp[[x]]) %>% 
      pivot_wider(names_from = value...1,
                  values_from = value...2)
    
    
    stop <- Sys.time()
    
  }
  
  stop-start
  
  procenty <- reduce(procenty, bind_rows) %>% 
    bind_rows(content[[length(content)]] %>% 
                html_nodes(".section_polls_avg") %>% 
                html_text() %>% 
                as_tibble() %>% 
                bind_cols(header) %>%
                pivot_wider(names_from = value...2,
                            values_from = value...1))
  
  
  #kolumna z nazwami
  creator <- character()
  
  for(x in seq_along(content)){
    
    creator[x] <- content[[x]] %>% 
      html_nodes(".section_polls_name") %>% 
      html_text()
    
  }
  
  creator <- tibble(creator) 
  
  
  #kolumna z terminami
  dates <- character()
  
  for(x in seq_along(content)){
    
    dates[x] <- content[[x]] %>% 
      html_nodes(".section_polls_term") %>% 
      html_text()
    
  }
  
  dates <- tibble(dates) 
  
  
  #zlaczenie w jedno + obrĂłbka
  
  polls_data <- bind_cols(creator,dates)[-c(1,2),] %>% 
    bind_cols(procenty) %>% 
    filter(!is.na(dates)) %>% 
    mutate(across(c("LEWICA","KO","POLSKA 2050","PSL","PiS","KONFEDERACJA"), as.numeric),
           dates = as.Date(str_sub(dates,-10,-1),format = "%d.%m.%Y"),
           creator = fct_collapse(creator,
                                  Pollster = c("Pollster","Pollster  / SE.pl "),
                                  Kantar = c("Kantar (CAPI)","Kantar (CATI)"),
                                  `United Surveys` = c("United Surveys","United Surveys  / WP ","United Surveys  / DGP,RMF "),
                                  Estymator = c("Estymator","Estymator  / dorzeczy.pl "),
                                  IBRiS = c("IBRiS","IBRiS  / Onet ","IBRiS  / radiozet.pl "),
                                  `CBM Indicator` = c("Indicator","CBM Indicator"),
                                  `Research Partner` = c("Research Partner","Research Partner "),
                                  CBOS = c("CBOS","CBOS "),
                                  IPSOS = c("IPSOS", "IPSOS  / OKO.press "))) %>% 
    pivot_longer(cols = LEWICA:KONFEDERACJA,
                 names_to = "party",
                 values_to = "value")
  
  return(polls_data)
}


get_poll_hrefs_in_table <- function(){
  
  #get href
  
  links <- read_html("https://ewybory.eu/sondaze/") %>% 
    html_nodes(".section_polls_name > a") %>% 
    html_attr("href") 
  
  #get dates
  
  dates <- character()
  
  xx <- read_html("https://ewybory.eu/sondaze/") %>% 
    html_nodes(".section_polls_row")
  
  for(x in seq_along(xx)){
    
    dates[x] <- xx[x] %>% 
      html_nodes(".section_polls_term") %>% 
      html_text()
    
  }
  
  where_last_na <- max(which(is.na(daty)))
  
  dates <- dates[where_last_na+1:length(links)]
  
  #get polls
  
  poll <- character()
  
  for(x in seq_along(xx)){
    
    poll[x] <- xx[x] %>% 
      html_nodes(".section_polls_name") %>% 
      html_text()
    
  }
  
  poll <- poll[where_last_na+1:length(links)]
  
  table <- tibble(dates,poll,links) %>% 
    mutate(dates = str_sub(dates, -10,-1))
  
  return(table)
  
}


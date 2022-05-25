library("tidyverse")
library("shiny")
library("rvest")
library("googlesheets4")

get_avg_polls <- function(df){
  
  df <- df %>% 
    group_by(dates,party) %>%
    summarise(avg = mean(value, na.rm=TRUE)) %>% 
    ungroup() 
  
  return(df)
}

get_last_election_results_plot <- function(df){
  
  df <- df %>%
    filter(str_detect(creator,"ybo")==TRUE,
           !is.na(value)) %>% 
    mutate(party = fct_relevel(party,
                               c("PiS","KO","LEWICA","PSL","KONFEDERACJA")),
           value = value/100) %>% 
    rename(`Share of votes` = "value") %>% 
    ggplot(aes(party, `Share of votes`, fill=party))+
    geom_col()+
    scale_y_continuous(labels = scales::percent)+
    scale_fill_manual(values = c("PiS"="#1f77b4",
                                 "KONFEDERACJA"="#9467bd",
                                 "KO"="#ff7f0e",
                                 "LEWICA"="#d62728",
                                 "PSL"="#2ca02c"))+
    ggtitle("Last parliamentary election result - 13.10.2019")+
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          plot.background = element_rect(fill="#FEF6E4"))

  return(df)
}

draw_party_plot <- function(df){
  
  df <- df %>% 
    ggplot(aes(date, result, col=party))+
    geom_line()+
    scale_x_date(breaks = "1 month")+
    scale_y_continuous(labels = scales::percent)+
    facet_wrap(vars(creator))+
    labs(x = "Date", y = "Result", col = "Party")+
    scale_color_manual(values = c("PiS"="#1f77b4",
                                  "KONFEDERACJA"="#9467bd",
                                  "KO"="#ff7f0e",
                                  "LEWICA"="#d62728",
                                  "POLSKA 2050"="#bcbd22",
                                  "PSL"="#2ca02c"))+
    theme(axis.text.x = element_text(angle = 90),
          plot.background = element_rect(fill="#FEF6E4"),
          legend.background = element_rect(fill="#FEF6E4"))
  
  return(df)
  
}

draw_avg_party_plot <- function(df){
  
  df <- df %>% 
    ggplot(aes(date, result, col=party))+
    geom_line()+
    scale_x_date(breaks = "1 month")+
    scale_y_continuous(labels = scales::percent)+
    labs(x = "Date", y = "Result", col = "Party")+
    scale_color_manual(values = c("PiS"="#1f77b4",
                                  "KONFEDERACJA"="#9467bd",
                                  "KO"="#ff7f0e",
                                  "LEWICA"="#d62728",
                                  "POLSKA 2050"="#bcbd22",
                                  "PSL"="#2ca02c"))+
    theme(axis.text.x = element_text(angle = 90),
          plot.background = element_rect(fill="#FEF6E4"),
          legend.background = element_rect(fill="#FEF6E4"))
  
  return(df)
  
}


draw_avg_party_plot_smooth <- function(df){
  
  df <- df %>% 
    ggplot(aes(date, result, col=party))+
    geom_smooth()+
    scale_x_date(breaks = "1 month")+
    scale_y_continuous(labels = scales::percent)+
    labs(x = "Date", y = "Result", col = "Party")+
    scale_color_manual(values = c("PiS"="#1f77b4",
                                  "KONFEDERACJA"="#9467bd",
                                  "KO"="#ff7f0e",
                                  "LEWICA"="#d62728",
                                  "POLSKA 2050"="#bcbd22",
                                  "PSL"="#2ca02c"))+
    theme(axis.text.x = element_text(angle = 90),
          plot.background = element_rect(fill="#FEF6E4"),
          legend.background = element_rect(fill="#FEF6E4"))
  
  return(df)
  
}


draw_avg_party_plot_fit <- function(df){
  
  df <- df %>% 
    ggplot(aes(date, result, col=party))+
    geom_line()+
    geom_smooth()+
    scale_x_date(breaks = "1 month")+
    scale_y_continuous(labels = scales::percent)+
    labs(x = "Date", y = "Result", col = "Party")+
    scale_color_manual(values = c("PiS"="#1f77b4",
                                  "KONFEDERACJA"="#9467bd",
                                  "KO"="#ff7f0e",
                                  "LEWICA"="#d62728",
                                  "POLSKA 2050"="#bcbd22",
                                  "PSL"="#2ca02c"))+
    theme(axis.text.x = element_text(angle = 90),
          plot.background = element_rect(fill="#FEF6E4"),
          legend.background = element_rect(fill="#FEF6E4"))
  
  return(df)
  
}

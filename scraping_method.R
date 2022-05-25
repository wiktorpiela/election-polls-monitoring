library("tidyverse")
library("shiny")
library("rvest")


content <- read_html("https://ewybory.eu/sondaze/") %>% 
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


#zlaczenie w jedno + obróbka

polls_data <- bind_cols(creator,dates)[-c(1,2),] %>% 
  bind_cols(procenty) %>% 
  filter(!is.na(dates)) %>% 
  mutate(across(c("LEWICA","KO","POLSKA 2050","PSL","PiS","KONFEDERACJA"), as.numeric),
         dates = as.Date(str_sub(dates,-10,-1),format = "%d.%m.%Y"),
         creator = fct_collapse(creator,
                                Pollster = c("Pollster","Pollster  / SE.pl "),
                                Kantar = c("Kantar (CAPI)","Kantar (CATI)"),
                                `United Surveys` = c("United Surveys","United Surveys  / WP "),
                                Estymator = c("Estymator","Estymator  / dorzeczy.pl "),
                                IBRiS = c("IBRiS","IBRiS  / Onet ","IBRiS  / radiozet.pl "),
                                `CBM Indicator` = c("Indicator","CBM Indicator"))) 
  

# poll_data %>% 
#   group_by(dates) %>% 
#   summarise(across(where(is.numeric),mean,na.rm=TRUE))

# procedura ---------------------------------------------------------------

# 1.biore te rekordy ktore maja klase .section_polls_row - jest ich dosc duzo 338

# content <- read_html("https://ewybory.eu/sondaze/") %>%
#   html_nodes(".section_polls_row")
 
# 2.pierwszy jest header, ktory trzeba wyczyœcic

# header <- content[[1]]
# 
# header <- header %>%
#   html_nodes(".section_polls_party_border") %>%
#   html_text() %>%
#   as_tibble() %>%
#   mutate(value = fct_recode(value,
#                             LEWICA="LEWICALEW",
#                             `POLSKA 2050`="POLSKA 2050P50",
#                             KONFEDERACJA ="KONFEDERACJAKON"))

# 3.bierzemy na razie tylko procenty z tabeli
# czyli z contentu wybieramy wszystko co to ma klase ".section_polls_data" i przejezdzamy to wszystkich liniach

#header trzeba powrotrzyc tyle razy aby w srodku polaczyc z kazdym elementem listy zawierajacym procenty

# headers <- rep(list(header),length(content))

#niekotre elementy listy sa puste dlatego trzeba je pominac stad warunek if else


# procenty <- list()
# temp <- list()
# 
# for(x in 2:length(content)){
# 
#   if(nrow(as_tibble(html_text(html_nodes(content[[x]],".section_polls_data")))) > 0){
# 
#     temp[[x]] <- content[[x]] %>%
#       html_nodes(".section_polls_data") %>%
#       html_text() %>%
#       as_tibble()
# 
#   } else{
# 
#     next
# 
#   }
# 
#   procenty[[x]] <- bind_cols(headers[[x]],temp[[x]]) %>%
#     pivot_wider(names_from = value...1,
#                 values_from = value...2)
# 
# }

# w srodku przerabiamy na jednokolumnowe tibble, laczymy z naglowkami i pivotujemy


#na koniec zbijamy wszystkie jednowierszowe elementy listy w tabele i dodatkowo na koncu dodajemy jeden rekord z inna klasa (na ktorym zrobilismy taka operacje jak z innymi rekordami)

# procenty <- reduce(procenty, bind_rows) %>%
#   bind_rows(content[[338]] %>%
#               html_nodes(".section_polls_avg") %>%
#               html_text() %>%
#               as_tibble() %>%
#               bind_cols(header) %>%
#               pivot_wider(names_from = value...2,
#                           values_from = value...1))



# 4. bierzemy kolumne z sondazownia, czyli wszystko to co ma klase ".section_polls_name"
# i caly wektor stringow zbijamy w 1 kolumnowy tibble


# creator <- character()
# 
# for(x in seq_along(content)){
# 
#   creator[x] <- content[[x]] %>%
#     html_nodes(".section_polls_name") %>%
#     html_text()
# 
# }
# 
# creator <- tibble(creator)


# 5.to samo robimy z danymi o dacie przeprowadzenia sondazu czyli z klasa ".section_polls_term"


# dates <- character()
# 
# for(x in seq_along(content)){
# 
#   dates[x] <- content[[x]] %>%
#     html_nodes(".section_polls_term") %>%
#     html_text()
# 
# }
# 
# dates <- tibble(dates)


# 6. na koniec laczymy wszystko w jedna tabele pamietajac ze creator i dates za dluzsze ucinamy w gory 2 niepotrzebne rekordy

# polls_table <- bind_cols(creator,dates)[-c(1,2),] %>%
#   bind_cols(procenty)

content[[100]] %>% 
  html_nodes(".section_polls_name") %>% 
  html_text()

content[[100]] %>%
  html_nodes(".section_polls_data") %>%
  html_text() %>%
  as_tibble() %>% 
  nrow()

content[[100]] %>% 
  html_nodes(".section_polls_term") %>% 
  html_text()

content[[100]] %>% 
  html_nodes(".section_polls_avg") %>% 
  html_text()

# proba z wykresem
library("plotly")
poll_data <- read_rds("scraped_poll_data.rds")
avg_poll_data <- read_rds("scraped_poll_data_avg.rds")



avg_poll_data
plot_ly(avg_poll_data, x = ~dates, y = ~avg, mode = 'lines', split = ~party)



ui <- fluidPage(
  
  tableOutput("tab")
)

server <- function(input, output, session){
  
  output$tab <- renderTable({
    
    
    urls <- get_poll_hrefs_in_table()$links
    poll <- get_poll_hrefs_in_table()$poll
    dates <- get_poll_hrefs_in_table()$dates

    refs <- paste0("<a href='",  urls, "' target='_blank'>click to open article</a>")
    
    data.frame(dates,poll,refs)
    
  }, sanitize.text.function = function(x) x)

  
}

shinyApp(ui,server)


x <- read_html("https://ewybory.eu/sondaze/") %>% 
  html_nodes(".section_polls_name > a") %>% 
  html_attr("href") 


y <- read_html("https://ewybory.eu/sondaze/") %>% 
  html_nodes(".section_polls_term") %>% 
  html_text()
  
  
y <- y[1:length(x)]

tibble(x,y)


library("googlesheets4")


  
ss <- read_sheet("https://docs.google.com/spreadsheets/d/1OXT4l8OrCn5Jr0wqyfrYW3R3rtPrrzLAxe04Jdcz3NQ/edit#gid=0")

gs4_create("polls")
write_sheet(ss,ss="https://docs.google.com/spreadsheets/d/1e9lxTT76_CYmYmLbnfMH4upzul0zD0Ak8UEgqgFjkXs/edit#gid=0",
            sheet="data")

#googlesheet api
library("httr")
library("jsonlite")

res <- GET("https://sheet.best/api/sheets/30ff97c4-1d73-4fd8-868d-9aaf058808dd")

fromJSON(rawToChar(res$content))

jsonlite::fromJSON(httr::GET("https://sheet.best/api/sheets/30ff97c4-1d73-4fd8-868d-9aaf058808dd"))


# od ktorego indeksu koncza sie NA w datach ? od tego zalezy ile bedziemy brac reszty
xx <- read_html("https://ewybory.eu/sondaze/") %>% 
  html_nodes(".section_polls_row")

daty <- character()

for(x in seq_along(xx)){
  
  daty[x] <- xx[x] %>% 
    html_nodes(".section_polls_term") %>% 
    html_text()
  
}

max(which(is.na(daty)))
daty[14]


#proba z tabami w shiny 

plot <- tibble(x = 1:10,
               y = 1:10) 

plot %>% 
  ggplot(aes(x,y))+
  geom_line()+
  theme(plot.background = element_rect(fill="red"))

ui <- fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
      
      tabsetPanel(
        
        id="tabsecik",
        type = "hidden",
        tabPanel("1",
                 dateInput("date",NULL),
                 numericInput("num",NULL, value=0),
                 textInput("text",NULL)),
        
        tabPanel("2",
                 dateInput("date",NULL),
                 textInput("text",NULL)),
        
        tabPanel("3",
                 dateInput("date",NULL),
                 numericInput("text",NULL,value=0),
                 numericInput("text",NULL,value=0),
                 numericInput("text",NULL,value=0))
        
      )

    ),
    
    mainPanel(
      
      tabsetPanel(
        
        id="tabsecik2",
        
        tabPanel("1",
                 plotlyOutput("plocik")),
        
        tabPanel("2",
                 plotlyOutput("plocik2")),
        
        tabPanel("3",
                plotlyOutput("plocik3"))
      
    )
  )
)
)

server <- function(input, output, session){
  
  observe({
    print(input$tabsecik)
  })
  
  
  observeEvent(input$tabsecik2, {
    updateTabsetPanel(inputId = "tabsecik",
                      selected = input$tabsecik2)
  }) 
  
  to_plot <- reactive({
    
    x <- plot %>% 
      filter(x >= input$num) %>% 
      ggplot(aes(x, y))+
      geom_line()
    
    ggplotly(x)
    
    
  })
  
  output$plocik <- renderPlotly(to_plot())
  

  
}

shinyApp(ui, server)



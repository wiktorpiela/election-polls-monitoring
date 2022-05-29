library("tidyverse")
library("shiny")
library("plotly")
library("shinyBS")
library("googlesheets4")
library("kableExtra")
library("shinysurveys")
setwd("~/Shiny/my_apps/election_poll_monitoring")
source("func.R")

gs4_auth("rprogrammer97@gmail.com")

poll_data <- read_sheet("https://docs.google.com/spreadsheets/d/1e9lxTT76_CYmYmLbnfMH4upzul0zD0Ak8UEgqgFjkXs/edit#gid=1360866738") %>%
  mutate(dates = as.Date(dates))

hrefs <- read_sheet("https://docs.google.com/spreadsheets/d/1o9m_HwPWGm4OwXS7w0wkVN_hBV4Lr60Uk0jl6oy0-fA/edit#gid=1915104960") %>%
  mutate(dates = as.Date(dates, format = "%d.%m.%Y"))

partys_list <- unique(poll_data$party)
creators_list <- sort(unique(poll_data$creator))[-length(unique(poll_data$creator))]

ui <- fluidPage(
  
  tags$style('.container-fluid {
                             background-color: #FEF6E4;
              }'),
  
  tags$head(tags$style(
    HTML('
         #sidebar {
            background-color: #f3d2c1;
        }')
  )),
  
  navbarPage(
    
    header = tags$style(HTML(" 
        .navbar-default .navbar-brand {color: #001858;}
        .navbar-default .navbar-brand:hover {color: #001858;}
        .navbar { background-color: #001858;}
        .navbar-default .navbar-nav > li > a {color:#001858;}
        .navbar-default .navbar-nav > li > a {color:black;}
        .navbar-default .navbar-nav > .active > a,
        .navbar-default .navbar-nav > .active > a:focus,
        .navbar-default .navbar-nav > .active > a:hover {color: #FEF6E4;background-color: #001858;}")), 

  title = div("Election polls monitoring",
              img(src = "flaga.jpg",
                  height = "35px",
                  style = "position: relative;
                                    top: -3px;
                                    left: 10px;")),

  theme = shinythemes::shinytheme("journal"),
                 
                 tabPanel("Main menu",
                        
                        sidebarLayout(
                          
                          sidebarPanel(
                            id="sidebar",
                            
                            tabsetPanel(
                              id = "tabset2",
                              type = "hidden",
                              
                              tabPanel("Polls",
                                       
                                       sliderInput("date", "Select date range",
                                                   min = min(poll_data$dates),
                                                   max = max(poll_data$dates),
                                                   value = c(min(poll_data$dates)+150, max(poll_data$dates)-150)),
                                       br(),
                                       
                                       fluidRow(
                                         
                                         column(6,checkboxGroupInput("partys", "Select party's",
                                                                     choices = partys_list,
                                                                     selected = partys_list),
                                                
                                                actionButton("select_all", "Select/unselect all")),
                                         
                                         column(6,selectizeInput("creators", "Select poll's creator",
                                                                 choices = creators_list,
                                                                 selected = "CBOS",
                                                                 multiple = TRUE,
                                                                 options = list(maxItems = 3)))
                                         )
                                       ),
                              
                              tabPanel("Last parliamentary election result",
                                       
                                       
                                       
                                       ),
                              
                              tabPanel("Average polls results",
                                      
                                       sliderInput("date1", "Select date range",
                                                   min = min(poll_data$dates),
                                                   max = max(poll_data$dates),
                                                   value = c(min(poll_data$dates)+150, max(poll_data$dates)-150)), 
                                       br(),
                                       
                                       fluidRow(
                                         
                                         column(6,checkboxGroupInput("partys1", "Select party's",
                                                                     choices = partys_list,
                                                                     selected = partys_list),
                                                
                                                actionButton("select_all1", "Select/unselect all")),
                                         
                                         column(6, radioButtons("smooth", "Choose plot option",
                                                                choices = c("regular line plot" = "regular",
                                                                            "fit trend" = "fit",
                                                                            "smooth plot" = "smooth"),
                                                                selected = "regular"))
                                         
                                         )
                                       )
                              )
                            ),
                          
                          mainPanel(
                            
                            tabsetPanel(
                              
                              id = "tabset1",
                              
                              tabPanel("Polls", plotlyOutput("party_plot")),
                              
                              tabPanel("Average polls results", plotlyOutput("avg_poll_results")),
                              
                              tabPanel("Last parliamentary election result",plotlyOutput("election_results",
                                                                       width = "50%",
                                                                       height="400px")),
                            )
                            
                          )
                        ),

                        h4(strong("Additions", style = "color: #666666; font-family: Lato")), 
                        
                        sidebarLayout(
                          
                          sidebarPanel(
                            id="sidebar",
                            
                            fluidRow(
                              
                              column(6,radioButtons("format", "Select data format",
                                                    choices = c("excel" = "xlsx",
                                                                "csv" = "csv",
                                                                "txt" = "txt"),
                                                    inline=TRUE),
                                     downloadButton("save", "Download polls data")),
                              
                              column(6, 
                                     downloadButton("save_plot", "Save plot"),
                                     br(),
                                     br(),
                                     actionButton("show_ref_to_article","Show reference to articles"),
                                     
                                     bsModal("show_articles",
                                             "Articles",
                                             "show_ref_to_article",
                                             tableOutput("urls")))
                            )
                          ),
                          
                          mainPanel(
                            
                            tabsetPanel(
                              
                              id = "tabset3",
                              type = "hidden",
                              
                              tabPanel("Polls", tableOutput("poll_table")),
                              
                              tabPanel("Average polls results", tableOutput("poll_table2")),
                              
                              tabPanel("Last parliamentary election result")
                              
                            )
                            
                            
                          ))
               ),
  
              tabPanel("Survey",
                       
                       surveyOutput(df = bind_rows(first_q,second_q,third_q,fourth_q),
                                    survey_title = "Own election poll",
                                    survey_description = "Share your vote!")
                       
                       ),
               
               navbarMenu("More",
                          
                          tabPanel("About",
                                   
                                   h3(strong("How it works?"),
                                      style = "color: #666666; font-family: Lato"),
                                   
                                   sidebarLayout(

                                     sidebarPanel(
                                       
                                       htmlOutput("info")
                                       
                                     ),
                                     
                                     mainPanel(
                                       
                                       img(src = "schemat.png",
                                           height = "400px",
                                           width = "1000px")
                                       
                                     )
                                     
                                   ),
                                   
                                   h3(strong("Covered topics and used packages"),
                                      style = "color: #666666; font-family: Lato"),
                                   
                                   sidebarLayout(
                                     
                                     sidebarPanel(

                                       htmlOutput("info2")
                                       
                                     ),
                                     
                                     mainPanel(
                                       
                                       img(src = "packages_pic.png",
                                           height = "250px",
                                           width = "450px")
                                       
                                     )
                                     )
                                   ),
                          
                          
                          tabPanel("Source",
                                   
                                   h2(strong("All data scraped from: ",
                                             tags$a(href="https://ewybory.eu/sondaze/", "ewybory.eu"),
                                             style = "color: #666666; font-family: Lato"))
                                   
                                   ))
               
    )

)

server <- function(input, output, session){
  
  observeEvent(input$select_all, {
    
    if(input$select_all%%2 == 0){
    
      updateCheckboxGroupInput(session, "partys", 
                               choices = partys_list,
                               selected = partys_list)
    } else {
      
      updateCheckboxGroupInput(session, "partys", 
                               choices = partys_list)
      
    }
    
  })
  
  
  observeEvent(input$select_all1, {
    
    if(input$select_all1%%2 == 0){
      
      updateCheckboxGroupInput(session, "partys1", 
                               choices = partys_list,
                               selected = partys_list)
    } else {
      
      updateCheckboxGroupInput(session, "partys1", 
                               choices = partys_list)
      
    }
    
  })
  
  
  observeEvent(input$tabset1, {
    
    updateTabsetPanel(inputId = "tabset2",
                      selected = input$tabset1)
    
    })
  
  observeEvent(input$tabset1, {
    
    updateTabsetPanel(inputId = "tabset3",
                      selected = input$tabset1)
    
  })
  
  
  party_plot <- reactive({
    
    req(input$partys)
    req(input$creators)
    
    poll_data %>% 
      rename(date = "dates",
             result = "value") %>% 
      filter(date>=min(input$date) & date<= max(input$date),
             party%in%input$partys,
             creator%in%input$creators) %>%
      mutate(result = result/100) %>% 
      draw_party_plot()

  })
  
  party_plotly <- reactive({
    
    x <- party_plot() + 
      theme(legend.position = "none")
    
    x <- ggplotly(x) %>% 
      config(displayModeBar=FALSE)

  })

  avg_party_plot <- reactive({
    
    req(input$partys1)
    
    if(input$smooth== "fit"){
      
      get_avg_polls(poll_data) %>%
        rename(date = "dates",
               result = "avg") %>%
        filter(date>=min(input$date1) & date<= max(input$date1),
               party%in%input$partys1) %>%
        mutate(result = result/100) %>%
        draw_avg_party_plot_fit()
      
    } else if(input$smooth == "smooth"){
      
      get_avg_polls(poll_data) %>%
        rename(date = "dates",
               result = "avg") %>%
        filter(date>=min(input$date1) & date<= max(input$date1),
               party%in%input$partys1) %>%
        mutate(result = result/100) %>%
        draw_avg_party_plot_smooth()
      
      
    } else {
      
        get_avg_polls(poll_data) %>%
          rename(date = "dates",
                 result = "avg") %>%
          filter(date>=min(input$date1) & date<= max(input$date1),
                 party%in%input$partys1) %>%
          mutate(result = result/100) %>%
          draw_avg_party_plot()
      
      }
    
    
    
  })
  
  avg_party_plotly <- reactive({
    
    x <- avg_party_plot() +
      theme(legend.position = "none")
    
    x <- ggplotly(x) %>% 
      config(displayModeBar=FALSE)

  })
  
  last_election <- reactive({
    
    get_last_election_results_plot(poll_data)
    
  })
  
  last_election_plotly <- reactive({
    
    ggplotly(last_election(),
             tooltip = c("Share of votes")) %>% 
      config(displayModeBar=FALSE)
    
    
  })
  
  react_poll_table <- reactive({
    
    req(input$creators)
    req(input$partys)

    x <- poll_data %>% 
      filter(dates<=max(input$date) & dates>=min(input$date),
             creator%in%input$creators,
             party%in%input$partys) %>%
      mutate(value = replace(value, is.na(value),"no data")) %>% 
      pivot_wider(names_from = party,
                  values_from = value,
                  values_fn = list)
    
    x %>% 
      unnest(cols = 2:ncol(x)) %>% 
      arrange(desc(dates)) %>% 
      mutate(dates = as.character(dates),
             across(3:ncol(x), ~ ifelse(. == "no data","",paste(.,"%")))) %>% 
      rename(date = "dates")
  })
  
  
  react_poll_table2 <- reactive({
    
    req(input$creators)
    req(input$partys)

     x <- get_avg_polls(poll_data) %>%
       arrange(match(party, c("LEWICA","KO","POLSKA 2050","PSL","PiS","KONFEDERACJA"))) %>% 
       filter(dates<=max(input$date) & dates>=min(input$date),
              party%in%input$partys1) %>%
       mutate(avg = round(avg,2),
              avg = replace(avg, is.na(avg), "no data")) %>% 
       pivot_wider(names_from = party,
                   values_from = avg,
                   values_fn = list) %>% 
       arrange(desc(dates))
     
     x %>% 
       mutate(dates = as.character(dates),
              across(2:ncol(x), ~ ifelse(.=="no data","",paste(.,"%")))) %>%
       rename(date = "dates")

  })
  
  data_save <- reactive({
    
    x <- poll_data
    
  })
  
  output$save <- downloadHandler(
    
    filename = function() paste0("polls.",input$format),
    
    content = function(file) if(input$format=="xlsx"){
      
      writexl::write_xlsx(data_save(), file) 
      
    } else if(input$format=="csv"){
        
      write_csv(data_save(), file)
      
    } else {write.table(data_save(), file, sep = "\t")
        
  })
  
  output$save_plot <- downloadHandler(
    
    filename = "plot.png",
    
    content = function(file){
      
      if(input$tabset1 == "Polls"){
        
        png(file, width = 1000, height = 600)
        print(party_plot())
        dev.off()
        
      } else if(input$tabset1=="Average polls results"){
        
        png(file, width = 1000, height = 600)
        print(avg_party_plot())
        dev.off()
        
        
      } else {
        
        png(file, width = 1000, height = 600)
        print(last_election())
        dev.off()
        
      }
      
      

  })
  
  output$info <- renderText({
  
    str1 <- "1. The host of ewybory.eu stores poll results on the website as HTML table with data"
    str2 <- "2. Scraping algorithm takes all data from HTML table (including hyperlinks) and converts
    into readable by R data frame in R-environment"
    str3 <- "3. Then some data cleaning and processing is performing"
    str4 <- "4. In the next step the same algorithm stores prepared data in Google Sheets"
    str5 <- "5. The Election poll monitoring app imports information from source and presents the results
    on plots and files available to download by user"
    str6 <- "6. In case of any updates on hosts website - the algorithm just repeates steps for 1 to 4 and overwrites dataset in sheets"

    HTML(paste(str1, str2,str3,str4,str5,str6 ,sep = '</br></br>'))
    
    

  })
  
  output$info2 <- renderText({
    
    str1 <- "web scraping, R programming, Shiny web app development, data processing, data storage, automation"
    str2 <- "packages: Shiny, rvest, tidyverse family packages, googlesheets4, plotly, shinyBS, kableExta"
    str3 <- "some functionalitites exhanced by pure HTML, CSS and JS"
   
    HTML(paste(str1, str2,str3,sep = '</br></br>'))
    
    
    
  })
  
  output$poll_table <- function(){
    
    react_poll_table() %>% 
      kable() %>% 
      kable_styling(c("striped","hover")) %>% 
      scroll_box(height = "300px")
    
  }
  
  output$poll_table2 <- function(){
    
    react_poll_table2() %>% 
      kable() %>% 
      kable_styling(c("striped","hover")) %>% 
      scroll_box(height = "300px")

  }
                         
  output$party_plot <- renderPlotly(party_plotly())
  
  output$election_results <- renderPlotly(last_election_plotly())

  output$avg_poll_results <- renderPlotly(avg_party_plotly())
  
  output$urls <- renderTable({
    
    
    urls <- hrefs$links
    poll <- hrefs$poll
    dates <- hrefs$dates
    
    refs <- paste0("<a href='",  urls, "' target='_blank'>click to open article</a>")
    
    data.frame(dates,poll,refs) %>% 
      mutate(dates = as.character(dates))
    
  }, sanitize.text.function = function(x) x)
  
  renderSurvey()
  
  observeEvent(input$submit, {
    showModal(modalDialog(
      title = "Thanks for your time and providing the answers!",
      "Your input has been gathered and stored!"
    ))
  })
    
}

shinyApp(ui, server) 

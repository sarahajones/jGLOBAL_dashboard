#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
if (!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if (!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if (!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if (!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if (!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if (!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if (!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if (!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if (!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if (!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if (!require(qualtRics)) install.packages("qualtRics", repos = "http://cran.us.r-project.org")
if (!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if (!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if (!require(fresh)) install.packages("fresh", repos = "http://cran.us.r-project.org")
if (!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")
if (!require(formattable)) install.packages("formattable", repos = "http://cran.us.r-project.org")
if (!require(sysfonts)) install.packages("sysfonts", repos = "http://cran.us.r-project.org")
if (!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if (!require(googlesheets4)) install.packages("googlesheets4", repos = "http://cran.us.r-project.org")

options(shiny.maxRequestSize = 160*1024^2)

font_add_google("Poppins", "poppins")

# set data
Sys.setenv(QUALTRICS_API_KEY = "	X53Q5r3xOUUUPgs1SMDd3IryEYAYMG7Kg4fhwBDz")
Sys.setenv(QUALTRICS_BASE_URL = "cambridge.eu.qualtrics.com")



fetch_and_process_data <- function() {
    raw_data <- fetch_survey(
        surveyID = "SV_6RqrZpLmxPmHbL0",
        label = FALSE,
        convert = FALSE,
        include_display_order = FALSE
    )



#raw_data <- raw_data[!raw_data$ResponseId %in% response_ids_to_remove, ]


raw_data$filter <- NA
# REMOVAL CRITERIA
# 1. duration less than 3 minutes (180 seconds)
raw_data$filter[raw_data$`Duration (in seconds)` < 180] <- 'too_quick'

# 2. age less than 18 years old
#raw_data$filter[raw_data$age < 18] <- 'underaged'

# 3. attention check failed (not 5)
#raw_data$filter[raw_data$attention != 5] <- 'attention_failed'

# 4. bot
#raw_data$filter[!is.na(raw_data$source)] <- 'bot'

# 5. no consent
#raw_data$filter[raw_data$consent != 1] <- 'no_consent'

# Remaining is valid
raw_data$filter[is.na(raw_data$filter)] <- "valid"


samples <- raw_data %>%
    group_by(filter) %>%
    summarize(n = n(), .groups = 'drop')

samples_wide <- samples %>%
    pivot_wider(names_from = filter, values_from = n, values_fill = list(n = 0))



if(!"no_consent" %in% colnames(samples_wide)) {
    samples_wide$no_consent <- NA
}
if(!"underaged" %in% colnames(samples_wide)) {
    samples_wide$underaged <- NA
}

if(!"attention_failed" %in% colnames(samples_wide)) {
    samples_wide$attention_failed <- NA
}

if(!"too_quick" %in% colnames(samples_wide)) {
    samples_wide$too_quick <- NA
}

if(!"valid" %in% colnames(samples_wide)) {
    samples_wide$valid <- NA
}

if(!"bot" %in% colnames(samples_wide)) {
    samples_wide$bot <- NA
}

gs4_deauth()
countries <- read_sheet("https://docs.google.com/spreadsheets/d/1DfTKlUGul1dFCOOJujc0Kwab2m4aML8g94zREn4xtFs/edit?usp=sharing", na = "")
countries$latitude <- as.numeric(countries$latitude)
countries$longitude <- as.numeric(countries$longitude)

#samples_wide <- merge(samples_wide, countries, by = "Q_Language", all = TRUE)

samples_wide <- samples_wide %>%
    mutate(
        attention_failed = ifelse(is.na(attention_failed), 0, attention_failed),
        underaged = ifelse(is.na(underaged), 0, underaged),
        too_quick = ifelse(is.na(too_quick), 0, too_quick),
        bot = ifelse(is.na(bot), 0, bot),
        no_consent = ifelse(is.na(no_consent), 0, no_consent),
        valid = ifelse(is.na(valid), 0, valid)
    )

samples_wide$total <- rowSums(samples_wide[, c("valid", "underaged", "attention_failed", "too_quick", "no_consent", "bot")], na.rm = TRUE)

list(samples_wide = samples_wide, raw_data = raw_data)
}

worldcountry <- geojson_read("50m.geojson", what = "sp")


result <- fetch_and_process_data()

samples_wide <- result$samples_wide
raw_data <- result$raw_data

total <- sum(samples_wide$valid, na.rm = TRUE)
Sys.setenv(TZ = "Europe/London")
current_datetime <- Sys.time()

# Create plotting parameters for map
bins <- c(0, 50, 100, 200, 300, 400, 500, 600, 700, 2000)
custom_colors <- c("#C4CBCF", "#CAF3FB","#A5FDFE", "#65FCFE", "#09DEE1", "#169C5E", "#087B46", "#005F33", "#005F53")
cv_pal <- colorBin(palette = custom_colors, domain = samples_wide$valid, bins = bins)
plot_map <- worldcountry[worldcountry$ADM0_A3 %in% samples_wide$ADM0_A3, ]

# Create base map
basemap <- leaflet(plot_map) %>%
    addTiles() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    fitBounds(-100, -60, 60, 70) %>%
    addLegend("bottomright", pal = cv_pal, values = samples_wide$valid, 
              title = paste0(
                  "<small>Valid: ", total, 
                             "<br>", format(current_datetime, "%Y-%m-%d %H:%M:%S"), "</small><br>Sample")
              )


# assign colours to countries to ensure consistency between plots 
cls <- rep(c(brewer.pal(8, "Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8, "Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")), 4)
cls_names <- c(as.character(unique(samples_wide$country)))
country_cols <- cls[1:length(cls_names)]
names(country_cols) <- cls_names

covid_col <- "#cc4c02"

# Shiny UI
ui <- bootstrapPage(
    tags$head(
        tags$style(
            HTML('

         @import url("https://fonts.googleapis.com/css2?family=Outfit:wght@100..900&family=Poppins:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap");'
            )
        )),
    navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
               
               HTML('<a style="text-decoration:none; cursor:default; color:#ffffff; font-weight: 500; font-style: italic;" class="active" href="#">jGLOBAL</a>
'),
windowTitle = "jGLOBAL",
tabPanel("Data Collected",
         div(class = "outer",
             tags$head(includeCSS("styles.css")),
             leafletOutput("mymap", width = "100%", height = "100%"),
             absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 200, fixed = TRUE, draggable = FALSE, height = "auto",
                           tags$a(href = 'https://jrp.pscholars.org/', tags$img(src = 'https://github.com/sarahajones/jGLOBAL_dashboard/blob/main/ICON.png', height = 'auto', width = '60%'))
             )
         )
),
tabPanel("More Country Information",
         sidebarLayout(
             fluidRow(
                 sidebarPanel(
                     pickerInput(
                         "region_select",
                         HTML("<span style='background-color: #bfcfe4; font-style: italic; font-weight: 700; family-font='Outfit; margin-bottom: 10px; font-size: 25px;'>Country:</span>"),
                         choices = str_sort(unique(samples_wide$Q_Language)),
                         options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                         multiple = FALSE
                     ),
                     HTML("<br><i>This displayed data only refers to valid participants.</i></br>")
                 )
             ),
             mainPanel(
                 uiOutput("filtered_content")
             )
         )
)
    )
)

# Shiny server
server <- function(input, output, session) {
    
    timer <- reactiveTimer(300000)  # Set timer for 5 minutes
    
    data_with_timestamp <- reactive({
        timer()
        result <- fetch_and_process_data()
        list(
            samples_wide = result$samples_wide,
            raw_data = result$raw_data,
            current_datetime = Sys.time()
        )
    })

    reactive_db <- reactive({
        samples_wide
    })
    
    reactive_db_large <- reactive({
        large_countries <- samples_wide %>% filter(ADM0_A3 %in% worldcountry$ADM0_A3)
        worldcountry_subset <- worldcountry[worldcountry$ADM0_A3 %in% large_countries$ADM0_A3, ]
        large_countries <- large_countries[match(worldcountry_subset$ADM0_A3, large_countries$ADM0_A3), ]
        large_countries
    })
    
    reactive_polygons <- reactive({
        worldcountry[worldcountry$ADM0_A3 %in% reactive_db_large()$ADM0_A3, ]
    })
    
    output$mymap <- renderLeaflet({
        basemap
    })
    
    observe({

        leafletProxy("mymap") %>%
            clearMarkers() %>%
            clearShapes() %>%
            addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.4, fillColor = ~cv_pal(reactive_db_large()$valid)) %>%
            addCircleMarkers(data = samples_wide, lat = ~latitude, lng = ~longitude,
                             radius = 5,
                             fillColor = "#30a4f7",
                             color = "#30a4f7",
                             fillOpacity = 0.3, stroke = TRUE,weight = 1,
                             label = ~paste(
                                 "<b>", country, "<br>",
                                 "Total valid:", valid, "</b><br>",
                                 "Total:", total, "<br>",
                                 "No consent:", no_consent, "<br>",
                                 "Underaged:", underaged, "<br>",
                                 "Too quick:", too_quick, "<br>",
                                 "Attention check:", attention_failed, "<br>",
                                 "Bot:", bot, "<br>") %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px"),
                                 textsize = "15px", direction = "auto"))
    })
    
    observeEvent(input$region_select, {
        site_data <- raw_data %>% filter(Q_Language == input$region_select, filter=="valid", Finished==1)
        
        output$filtered_content <- renderUI({
            tagList(
                fluidRow(style = "margin-left:10px; margin-top: 20px; margin-bottom: 20px",
                         column(6, plotlyOutput("sex_distribution")),
                         column(6, plotlyOutput("age_distribution", width="700px"))),
                fluidRow(style = "margin-left:10px; margin-top: 20px; margin-bottom: 20px",
                         fluidRow(style = "margin-left:10px; margin-top: 20px; margin-bottom: 20px",
                                  column(6, plotlyOutput("duration", width="1000px")))
                ),
                fluidRow(style = "margin-left:10px; margin-top: 20px; margin-bottom: 20px",
                         fluidRow(style = "margin-left:10px; margin-top: 20px; margin-bottom: 20px",
                                  column(6, plotlyOutput("employment_distribution", width="1000px")))
                ),
                fluidRow(style = "margin-left:10px; margin-top: 20px; margin-bottom: 20px",
                         fluidRow(style = "margin-left:10px; margin-top: 20px; margin-bottom: 20px",
                                  column(6, plotlyOutput("political_distribution", width="1000px")))
                ),
                fluidRow(
                    column(12, DTOutput("zip_table"))
                )
            )
            
        })
        
        output$sex_distribution <- renderPlotly({
            gender <- site_data %>% 
                group_by(gender) %>% 
                summarize(n = n()) %>%
                mutate(gender = case_when(
                    gender == 2 ~ "female",
                    gender == 1 ~ "male",
                    gender == 3 ~ "prefer not to answer",
                    gender == 4 ~ "other",
                    TRUE ~ as.character(gender)
                ))
            
            plot_ly(gender, x = ~gender, y = ~n, type = 'bar', color = ~as.factor(gender),
                    colors = c("#f1948a", "#f1c40f")) %>% 
                layout(title = list(text = "Sex distribution"),
                       showlegend = FALSE,
                       xaxis = list( 
                           zerolinecolor = '#D1ECE5', 
                           zerolinewidth = 2, 
                           gridcolor = '#D1ECE5'), 
                       yaxis = list( 
                           zerolinecolor = '#D1ECE5', 
                           zerolinewidth = 2, 
                           gridcolor = '#D1ECE5'))
        })
        
        output$age_distribution <- renderPlotly({
            age <- site_data %>% 
                group_by(age) %>% 
                summarize(n = n())
            
            plot_ly(age, x = ~age, y = ~n, type = 'bar') %>% 
                layout(title = list(text = "Age distribution"),
                       showlegend = FALSE,
                       xaxis = list(title = "Age"),
                       xaxis = list( 
                           zerolinecolor = '#D1ECE5', 
                           zerolinewidth = 2, 
                           gridcolor = '#D1ECE5'), 
                       yaxis = list( 
                           zerolinecolor = '#D1ECE5', 
                           zerolinewidth = 2, 
                           gridcolor = '#D1ECE5'))
        })
        
        output$duration <- renderPlotly({
            site_data <- site_data %>%
                mutate(Duration_in_minutes = `Duration (in seconds)` / 60)
            
            plot_ly(site_data, x = ~Duration_in_minutes, type = "box", boxpoints = "all", jitter = 0.3, pointpos = -1.8)%>% 
                layout(title = list(text = "Duration (in minutes)"),
                       showlegend = FALSE,
                       xaxis = list( title="Minutes",
                                     zerolinecolor = '#D1ECE5', 
                                     zerolinewidth = 2, 
                                     gridcolor = '#D1ECE5'), 
                       yaxis = list(title="",
                                    zerolinecolor = '#D1ECE5', showticklabels=F,
                                    zerolinewidth = 2, 
                                    gridcolor = '#D1ECE5')) %>% hide_colorbar()
        })
        
        output$employment_distribution <- renderPlotly({
            employment <- site_data %>% 
                group_by(employment) %>% 
                summarize(n = n()) %>%
                mutate(employment = case_when(
                    employment == 1 ~ "Full-time employed",
                    employment == 2 ~ "Full-time student",
                    employment == 3 ~ "Employed part-time\n(hourly or only\npartial wage and limited benefits)",
                    employment == 4 ~ "I own my own business",
                    employment == 5 ~ "Not in paid\nemployment (by choice)",
                    employment == 6 ~ "Not in paid\nemployment (looking for work)",
                    employment == 7 ~ "Not in paid\nemployment (unable to/nwork due to health/personal reasons)",
                    employment == 8 ~ "Retired",
                    TRUE ~ as.character(employment)
                ))
            
            plot_ly(employment, x = ~n, y = ~employment, type = 'bar', color = ~as.factor(employment),
                    colors = c("#c39bd3")) %>% 
                layout(title = list(text = "Employment distribution"),
                       showlegend = FALSE,
                       xaxis = list( 
                           zerolinecolor = '#D1ECE5', 
                           zerolinewidth = 2, 
                           gridcolor = '#D1ECE5'), 
                       yaxis = list( 
                           zerolinecolor = '#D1ECE5', 
                           zerolinewidth = 2, 
                           gridcolor = '#D1ECE5',
                           title= ""))
        })
        
        output$political_distribution <- renderPlotly({
            political <- site_data %>% 
                group_by(ideology_1) %>% 
                summarize(n = n())
            
            plot_ly(political, x = ~ideology_1, y = ~n, type = 'bar', color = ~as.factor(ideology_1),
                    colors = c("#DD4124")) %>% 
                layout(title = list(text = "Political orientation distribution"),
                       showlegend = FALSE,
                       xaxis = list( 
                           zerolinecolor = '#D1ECE5', 
                           zerolinewidth = 2, 
                           gridcolor = '#D1ECE5',
                           title= ""), 
                       yaxis = list( 
                           zerolinecolor = '#D1ECE5', 
                           zerolinewidth = 2, 
                           gridcolor = '#D1ECE5'))
        })
        
        output$zip_table <- renderDT({
            datatable(site_data %>% select(zip), 
                      options = list(pageLength = 50, 
                                     autoWidth = TRUE, 
                                     columnDefs = list(list(className = 'dt-center', targets = '_all')),
                                     dom = 't<"dt-buttons"Bp>', 
                                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                      ), rownames = FALSE
            )
        })
        
    })
    
    # Schedule shutdown after 5 minutes (300 seconds)
    later::later(function() { 
        stopApp() 
    }, 300)
}

shinyApp(ui = ui, server = server)

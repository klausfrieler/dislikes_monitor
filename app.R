#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinythemes)
library(DT)
library(sjPlot)
# library(pcalg)
# library(networkD3)
# library(igraph)

source("analysis.R")
source("plot_util.R")

on_server <- grepl("shiny-server", getwd())
if(on_server){
    #result_dir <- "../earworms/output/results"
  result_dir <- "../dislikes/output/results/"
  all_styles <<- readxl::read_xlsx("../dislikes/data_raw/SMP_AUS_styles.xlsx")
} else{
    result_dir <- "data/from_server/v2"
    all_styles <<- readxl::read_xlsx("SMP_AUS_styles.xlsx")

}

setup_workspace(result_dir)

var_choices <- setdiff(names(master_metadata), c("p_id",
                                       "time_started",
                                       "time_ended",
                                       "pilot",
                                       "complete",
                                       "REF.reflection",
                                       "DEG.second_language",
                                       "DEG.first_language",
                                       "DEG.gender", "DEG.age"))
var_types <- c("categorial", "numeric")[1 + map_lgl(var_choices, ~{(master_metadata[[.x]] %>% class())[1] == "numeric"})]
var_data <- tibble(variable = var_choices, type = var_types)
countries <- unique(master_metadata$DEG.country_of_residence)
countries <- c("--", countries[!is.na(countries)])
theme_set(get_default_theme())

get_intro_text <- function(){
  div(h3("Welcome to the  (Dis)liked Music Monitor App"),
         p("This app allows you visualize and inspect the data from a study on music (dis)likes",
           "that was carried out by the James Cook University and the Max Planck Institute for Empirical Aesthetics, Frankfurt/M., Germany"),
      p("Have fun!"),
      style = "width:50%;text-align:justify")
}

impressum <- function(){
    p(
        "(Dis)liked Music Monitor  v0.1",
        shiny::tags$br(),
        shiny::tags$br(),
        "Author: Klaus Frieler",
        shiny::tags$br(),
        shiny::a(href = "https://www.aesthetics.mpg.de/en.html",
                 "Max Planck Institute for Empirical Aesthetics, Frankfurt/M, Germany",
                 target = "_blank"),
        shiny::tags$br(),
        shiny::tags$br(),
        "PI",
        shiny::p("Amanda Krause, JCU "),
        shiny::tags$br(),
        shiny::tags$br(),
        "Powered by",
        shiny::tags$br(),
        shiny::a(href = "http://www.music-psychology.de/",
                 "Deutsche Gesellschaft für Musikspsychologie", target = "_blank"),
        shiny::tags$br(),
        shiny::tags$br(),
        shiny::a(href = "https://github.com/klausfrieler/dislikes_monitor", "On Github", target = "_blank"),
        style = "font-size: 10pt; display: block"
    )

}

input_width <- 300

ui_new <-
    shiny::shinyUI(
        navbarPage(
            title = "Exploring (Dis)liked Music",
            theme = shinytheme("spacelab"),
            id = "tabs",
            tabPanel(
                "Home",
                sidebarLayout(
                    sidebarPanel(
                      selectizeInput("country_filter",
                                     "Filter:",
                                     countries,
                                     multiple = F),
                      impressum(),
                      #downloadButton("download_all_data_csv", "Download data"),
                      #checkboxInput("dec", label = "Use German Format", value = 0),

                        width = 2
                    ),

                    # Main panel for displaying outputs ----
                    mainPanel(
                        htmlOutput("introduction"),
                        h4("Summary"),
                        tableOutput("overall_stats")
                    )

                )
            ),
            tabPanel(
                "Univariate",
                sidebarLayout(
                    sidebarPanel(
                        selectizeInput("uv_variable", "Variable:", var_choices, multiple = F),
                        selectizeInput("uv_country_filter",
                                       "Filter:",
                                       choices = countries, multiple = F),

                        impressum(),
                        width = 2
                    ),

                    # Main panel for displaying outputs ----
                    mainPanel(
                        plotOutput("univariate_plot", width = "800px")
                        )

                )
            ),
            tabPanel(
                "Bivariate",
                sidebarLayout(
                    sidebarPanel(
                        selectizeInput("bv_variable1", "Variable X:", var_choices, selected = "JAJ.ability", multiple = F),
                        selectizeInput("bv_variable2", "Variable y:", var_choices, selected = "IMI.earworm_frequency", multiple = F),
                        actionButton("switch_axes",
                                     label = "Switch axes", style = "margin-bottom: 10px"),
                        selectizeInput("bv_country_filter",
                                       "Filter:",
                                       countries,
                                       multiple = F),

                        impressum(),
                        width = 2
                    ),

                    # Main panel for displaying outputs ----
                    mainPanel(
                        plotOutput("bivariate_plot", width = "800px"),
                        tableOutput("corr_tab")
                    )

                )
            ),

            tabPanel(
                "Data",
                sidebarLayout(
                    sidebarPanel(
                        impressum(),
                        width = 2
                    ),

                    # Main panel for displaying outputs ----
                    mainPanel(
                        DT::DTOutput("raw_data")
                    )

                )
            )))

apply_filters <- function(data, input){
  tabs <- input$tabs
  #browser()
  if(tabs == "Home"){
    filter_val <- input$country_filter
  }
  else if(tabs == "Univariate"){
    filter_val <- input$uv_country_filter
  }
  else if(tabs == "Bivariate"){
    filter_val <- input$bv_country_filter
  }
  else{
    return(data)
  }
  if(filter_val == "--") return(data)
  #print(input$pc_study_filter)
  data <- data %>% filter(DEG.country_of_residence == filter_val)

  data
}
# Define server logic required to draw a plot
server <- function(input, output, session) {
   message("*** STARTING APP***")
   check_data <- reactiveFileReader(1000, session, result_dir, setup_workspace)

   shiny::observeEvent(input$switch_axes, {
       x <- input$bv_variable1
       y <- input$bv_variable2
       updateSelectizeInput(session, inputId = "bv_variable1",
                            selected = y)
       updateSelectizeInput(session, inputId = "bv_variable2",
                            selected = x)

   })
   output$introduction <- renderUI({
     get_intro_text()
   })
   output$overall_stats <- renderTable({
      check_data()
      data <- apply_filters(master, input)
      #data <- master
      p_id_stats <- data %>%
         distinct(p_id, gender, age, GMS.active_engagement, complete, DEG.country_of_residence) %>%
         summarise(n_female = sum(gender == "female", na.rm = T),
                   n_male = sum(gender == "male", na.rm = T),
                   n_other = sum(gender == "other", na.rm = T),
                   n_not_say = sum(gender == "not_say", na.rm = T),
                   n_countries = n_distinct(na.omit(DEG.country_of_residence)),
                   mean_age = mean(age, na.rm = T),
                   n_unique = n(),
                   n_complete = sum(complete, na.rm = T),
                   .groups = "drop")
      p_id_stats %>%
         select(n_unique, n_complete, starts_with("n"), mean_age, n_countries, everything()) %>%
          set_names("Total N", "Completed", "Females", "Males", "Other", "Rather not say",  "Number of countries", "Mean Age")
      })

    output$raw_data <- renderDataTable({
      check_data()
      data <- apply_filters(master, input)
      data %>%
        mutate(p_id = as.integer(factor(p_id) %>% fct_reorder(time_started))) %>%
        select(-DEG.age) %>%
        mutate_if(is.numeric, round, 2) %>%
        select(p_id, time_started, complete, age, gender, DEG.country_of_residence, everything())
   }, options = list(lengthMenu = list(c(25, 50,  -1), c("25", "50",  "All"))))

  output$univariate_plot <- renderPlot({
    check_data()
    data <- apply_filters(master_metadata, input)
    #data <- master
    var_info <- var_data %>% filter(variable == input$uv_variable)
    if(var_info$type == "numeric"){
      q <- univariate_plot_numeric(data, input$uv_variable, remove_na = T)
      }
    else if (var_info$type == "categorial"){
      data <- data %>% prepare_dislikes_categorials(input$uv_variable, var_data)
      coord_flip <- n_distinct(data[[input$uv_variable]]) > 3
      q <- univariate_plot_categorial(data, input$uv_variable,  remove_na = T, coord_flip = coord_flip)
      }
    else {
      return()
      }
    q
   })

  output$bivariate_plot <- renderPlot({
    check_data()
    data <- apply_filters(master_metadata, input)
    #data <- master

    #browser()
    if(input$bv_variable1 == input$bv_variable2){
      return()
    }
    data1 <- prepare_dislikes_categorials(data, input$bv_variable1, var_data)
    data2 <- prepare_dislikes_categorials(data, input$bv_variable2, var_data)
    data <- data1 %>% left_join(data2, by = "p_id")
    bivariate_plot_auto(data, input, var_data, remove_na = T)
  })



    output$corr_tab <- renderTable({
      check_data()
      data <- apply_filters(master_metadata, input)
      vars <- get_parameters(data, input, var_data = var_data)
      if(vars$sub_type == "num-num" && input$bv_variable1 != input$bv_variable2) {
        get_correlations(data, input$bv_variable1, input$bv_variable2)
      }
    },  caption = "Correlations", caption.placement = getOption("xtable.caption.placement", "top"))


    output$download_all_data_csv <- downloadHandler(
      filename = "dislikes_data.csv",
      content = function(file) {
        dec <- ifelse(input$dec, ",", ".")
        write.table(master %>% mutate_if(is.logical, as.integer),
                    file,
                    row.names = FALSE,
                    dec = dec,
                    sep = ";",
                    quote = T,
                    fileEncoding = "utf-8")
      }
    )

}

# Run the application
shinyApp(ui = ui_new, server = server)


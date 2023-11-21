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

source("utils.R")
source("analysis.R")
source("plot_util.R")
analysis_mode <- T

on_server <- grepl("shiny-server", getwd())
if(on_server & !analysis_mode){
    #result_dir <- "../earworms/output/results"
  result_dir <- "../dislikes/output/results/"
  #all_styles <<- readxl::read_xlsx("../dislikes/data_raw/SMP_AUS_styles.xlsx")
  all_styles_path <- "data/SMP_AUS_styles.xlsx"
} else{
    result_dir <- "data"
    #all_styles <<- readxl::read_xlsx("data/SMP_AUS_styles.xlsx")
    all_styles_path <- "data/SMP_AUS_styles.xlsx"

}

setup_workspace(result_dir, all_styles_path = all_styles_path, reload = F)

var_choices <- setdiff(names(metadata), c("p_id",
                                       "time_started",
                                       "time_ended",
                                       "pilot",
                                       "complete",
                                       "REF.reflection",
                                       "DEG.second_language",
                                       "DEG.first_language",
                                       "DEG.gender", "DEG.age"))
var_types <- c("categorial", "numeric")[1 + map_lgl(var_choices, ~{(metadata[[.x]] %>% class())[1] == "numeric"})]
var_data <- tibble(variable = var_choices, type = var_types)
countries <- unique(metadata$DEG.country_of_residence)
countries <- c(countries[!is.na(countries)])
age_groups <-  c( "17-34", "35-53", "54+")
gender <- levels(metadata$gender)
most_liked <- unique(master$REF.most_disliked) %>% sort()
most_disliked <- unique(master$REF.most_liked) %>% sort()
styles <- union(most_liked, most_disliked) %>% sort()
metadata <- metadata %>% mutate(p_id = sprintf("%04d", p_id %>% as.factor() %>% as.integer()))
p_ids <- unique(metadata$p_id) %>% sort()

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
                                     "Countries:",
                                     countries,
                                     selected = countries,
                                     multiple = T),
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
                                       "Countries:",
                                       selected = countries,
                                       choices = countries,
                                       multiple = T),

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
                                       "Countries:",
                                       countries,
                                       selected = countries,
                                       multiple = T),

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
              "Reflections",
              sidebarLayout(
                sidebarPanel(
                  selectizeInput("ref_age_groups", "Age groups:", age_groups, selected = age_groups, multiple = T),
                  selectizeInput("ref_gender", "Gender:", gender, selected = gender, multiple = T),
                  selectizeInput("ref_p_id", "ID:", p_ids, selected = p_ids[1], multiple = F),
                  selectizeInput("ref_most_liked", "Most liked:", most_liked, selected = most_disliked, multiple = T),
                  selectizeInput("ref_most_disliked", "Most disliked:", most_disliked, selected = most_disliked, multiple = T),
                  selectizeInput("ref_country",
                                 "Countries:",
                                 countries,
                                 selected = countries,
                                 multiple = T),
                  impressum(),
                  width = 2
                ),

                # Main panel for displaying outputs ----
                mainPanel(
                  actionButton("previous_id", "Previous"),
                  actionButton("next_id", "Next"),
                  uiOutput("ref_reader_demographics"),
                  uiOutput("ref_reader_reflections"),
                  uiOutput("ref_reader_personality_headline"),
                  plotOutput("ref_reader_personality_plot", width = 600)
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

apply_filters <- function(data, input, no_id = F){
  tabs <- input$tabs
  #browser()
  country_filter <- countries
  age_filter <- make_age_range(age_groups)
  gender_filter <- gender
  most_liked_filter <- most_liked
  most_disliked_filter <- most_disliked
  filter_styles <- F
  if(tabs == "Home"){
    country_filter <- input$country_filter
  }
  else if(tabs == "Univariate"){
    country_filter <- input$uv_country_filter
  }
  else if(tabs == "Bivariate"){
    country_filter <- input$bv_country_filter
  }
  else if(tabs == "Reflections"){
    country_filter <- input$ref_country
    age_filter <- make_age_range(input$ref_age_groups)
    gender_filter <- input$ref_gender
    most_liked_filter <- input$ref_most_liked
    most_disliked_filter <- input$ref_most_disliked
    p_id_filter <- input$ref_p_id
    filter_styles <- T
  }
  else{
    return(data)
  }
  # print(country_filter)
  # print(age_filter)
  # print(gender_filter)
  #print(input$pc_study_filter)
  #print(nrow(data))
  data <- data %>% filter(DEG.country_of_residence %in% country_filter)
  #print(nrow(data))
  data <- data %>% filter(is.na(age) | age %in% age_filter)
  #print(nrow(data))
  data <- data %>% filter(as.character(gender) %in% gender_filter)
  if(filter_styles){
    data <- data %>%
      filter(REF.most_liked %in% most_liked_filter) %>%
      filter(REF.most_disliked %in% most_disliked_filter) %>%
      filter(!is.na(REF.reflection), nzchar(REF.reflection))
    if(!no_id){
      data <- data %>%
      filter(p_id == p_id_filter)
    }
  }
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
   shiny::observeEvent(input$ref_age_groups, {
     data <- apply_filters(metadata, input, no_id = T)
     updateSelectizeInput(session, inputId = "ref_most_liked",
                          choices = unique(data$REF.most_liked),
                          selected = unique(data$REF.most_liked)
     )
     updateSelectizeInput(session, inputId = "ref_most_disliked",
                          choices = unique(data$REF.most_disliked),
                          selected = unique(data$REF.most_disliked))

     updateSelectizeInput(session, inputId = "ref_p_id",
                          choices = sort(unique(data$p_id)),
                          selected = sort(unique(data$p_id))[1])
   })
   shiny::observeEvent(input$ref_gender, {
     data <- apply_filters(metadata, input, no_id = T)
     updateSelectizeInput(session, inputId = "ref_most_liked",
                          choices = unique(data$REF.most_liked),
                          selected = unique(data$REF.most_liked)
     )
     updateSelectizeInput(session, inputId = "ref_most_disliked",
                          choices = unique(data$REF.most_disliked),
                          selected = unique(data$REF.most_disliked))

     updateSelectizeInput(session, inputId = "ref_p_id",
                          choices = unique(data$p_id)  %>% sort(),
                          selected = sort(unique(data$p_id))[1])
   })
   shiny::observeEvent(input$ref_country, {
     data <- apply_filters(metadata, input, no_id = T)
     updateSelectizeInput(session, inputId = "ref_most_liked",
                          choices = unique(data$REF.most_liked),
                          selected = unique(data$REF.most_liked)
                          )
     updateSelectizeInput(session, inputId = "ref_most_disliked",
                          choices = unique(data$REF.most_disliked),
                          selected = unique(data$REF.most_disliked))
     updateSelectizeInput(session, inputId = "ref_p_id",
                          choices = unique(data$p_id) %>% sort(),
                          selected = sort(unique(data$p_id))[1])

   })

   shiny::observeEvent(input$next_id, {
     data <- apply_filters(metadata, input, no_id = T)
     #browser()
     selected <- input$ref_p_id
     choices <- unique(data$p_id) %>% sort()
     idx <- which(selected == choices) + 1
     if(idx > length(choices)){
       idx <- 1
     }
     updateSelectizeInput(session, inputId = "ref_p_id",
                          choices = choices,
                          selected = choices[idx])

   })

   shiny::observeEvent(input$previous_id, {
     data <- apply_filters(metadata, input, no_id = T)
     #browser()
     selected <- input$ref_p_id
     choices <- unique(data$p_id) %>% sort()
     idx <- which(selected == choices) - 1
     if(idx < 1){
       idx <- length(choices)
     }

     updateSelectizeInput(session, inputId = "ref_p_id",
                          choices = choices,
                          selected = choices[idx])

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
    data <- apply_filters(metadata, input)
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
    data <- apply_filters(metadata, input)
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
    data <- apply_filters(metadata, input)
    vars <- get_parameters(data, input, var_data = var_data)
    if(vars$sub_type == "num-num" && input$bv_variable1 != input$bv_variable2) {
      get_correlations(data, input$bv_variable1, input$bv_variable2)
    }
  },  caption = "Correlations", caption.placement = getOption("xtable.caption.placement", "top"))

  output$ref_reader_demographics <- renderUI({
    check_data()
    data <- metadata %>% mutate(
                                SES = scale(scale(as.integer(factor(DEG.financial))) +
                                                    scale(as.integer(as.factor(DEG.life_circumstances)))) %>%
                                  as.numeric())
    data <- apply_filters(data, input) %>%
      filter(!is.na(REF.reflection), nzchar(REF.reflection))
    if(nrow(data) == 0){
      return(shiny::p("Empty data..."))
    }
    shiny::h3(sprintf("Participant %s: %s (%s),  %s",
                               data$p_id[1],
                               str_to_title(data$gender[1]),
                               data$age[1],
                               data$DEG.country_of_residence[1]))


  })

  output$ref_reader_personality_headline <- renderUI({
    shiny::h4("Personality Profile", style = "margin-top:50px;margin-left:40px")

  })
  output$ref_reader_personality <- renderUI({
    check_data()
    data <- apply_filters(metadata, input) %>%
      filter(!is.na(REF.reflection), nzchar(REF.reflection))

    # if(nrow(data) = 0){
    #   return(shiny::p("Empty data...",
    #          style = "text-color:red"))
    # }
    data %>% mutate(MET.music_engagement = .2*(MET.physical +
                                           MET.affective +
                                           MET.social +
                                           MET.cognitive +
                                           MET.narrative)) %>%
      mutate(GMS.active_engagement_with_music = round(GMS.active_engagement/7, 2)) %>%
      mutate(across(starts_with("TPI"), function(x) round(x/7,2)),
             MET.music_enjoyment = round(MET.music_engagement/7,2)) %>%
      select(p_id,
             GMS.active_engagement_with_music,
             MET.music_enjoment,
             starts_with("TPI")) %>%
      distinct() %>%
      make_shiny_table() %>%
    shiny::div(shiny::h4("Personality Profile"), ., style = "margin-top:20px")

  })

  output$ref_reader_personality_plot <- renderPlot({
    check_data()
    md <- metadata %>%
      mutate(SES.economic_status_norm = (SES.economic_status - min(SES.economic_status))/diff(range(SES.economic_status)))
    data <- apply_filters(md, input) %>%
      filter(!is.na(REF.reflection), nzchar(REF.reflection))

    # if(nrow(data) = 0){
    #   return(shiny::p("Empty data...",
    #          style = "text-color:red"))
    # }
    data %>% mutate(MET.music_enjoyment = .2*(  MET.physical +
                                                 MET.affective +
                                                 MET.social +
                                                 MET.cognitive +
                                                 MET.narrative)
                    ) %>%
      mutate(GMS.active_engagement_with_music = round(GMS.active_engagement/7, 2),
             MET.music_enjoyment = round(MET.music_enjoyment/7,2)) %>%
      mutate(across(starts_with("TPI"), function(x) round(x/7,2))) %>%
      select(p_id,
             GMS.active_engagement_with_music,
             MET.music_enjoyment,
             SES.economic_status_norm,
             starts_with("TPI")) %>%
      distinct() %>%
      pivot_longer(-p_id)  %>%
      mutate(type = get_var_group(name)) %>%
      mutate(name = sprintf("%s:",(name %>%
                                     str_split_fixed("[.]", 2))[,2] %>%
                              str_replace_all("_", " ") %>%
                              str_to_title())) %>%
    ggplot(aes(x = fct_reorder(name, type), y = value, colour = type)) +
      geom_linerange(aes(ymax = value, ymin = 0), linewidth = 1) +
      geom_point(size = 4) +
      #geom_col(color = "black") +
      scale_colour_brewer(palette = "Set1", guide = "none") +
      coord_flip() + labs(x = "", y = "Normalized value (0-1)")

  })

  output$ref_reader_reflections <- renderUI({
    check_data()
    data <- metadata %>%
      filter(!is.na(REF.reflection), nzchar(REF.reflection))
    data <- apply_filters(data, input) %>% slice(1)
    shiny::div(
               shiny::h4(shiny::tags$b("Most liked:"),  data$REF.most_liked[1], style = "color:#4DAF4A"),
               shiny::h4(shiny::tags$b("Most disliked:"), data$REF.most_disliked[1], style = "color:#E41A1C"),
               #shiny::h4("Reflection", style = "margin-top:20px"),
               shiny::p(data$REF.reflection[1], style = "width:600px;display:block;font-size:14pt;border:1px black dotted;padding:10px;background:#eeeeee"), style = "margin:40px")

  })

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


#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lensdna)
library(tidyverse)
library(shinydashboard)
library(ggiraph)
library(shinyalert)
library(shinyjs)

# load("lensPATNing/pathways.rda")
# load("lensPATNing/all_pairs.rda")

# load("lensPATNing/reference.rda")
# load("lensPATNing/Targets_LEC.rda")
# load("pathways.rda")
# load("all_pairs.rda")
# load("ranked_data.rda")
# load("reference.rda")
# load("Targets_LEC.rda")

# ----------------------- TO PUT IN --------------------------------
# Targets genes that are unique to a particular transcription factor
# Targets_LEC %>%
#   add_count(Target) %>%
#   filter(n == 1) %>%
#   select(-n)








# Set a title and some other
header <- dashboardHeader(
  title = "lensPATNing"
)


pathway_tab <- tabItem(
  tabName = "pathway_tab",
  tabBox(
    title = "Pathway Diagrams",
    width = 12,
    tabPanel(
      "Select Pathway",
      ggiraphOutput("BareFlow"),
      tags$style(type = "text/css", "#UniProtSearch {display: inline;}"),
      tags$style(type = "text/css", "#TRToTarget {display: inline;}"),
      tags$span(
        shiny::actionButton("TRToTarget", "Get Target Genes"),
        uiOutput("UniProtSearchButton")
      )

    ),
    tabPanel(
      "Exclude Nodes",
      ggiraphOutput("ExcludeGraph"),
      shiny::actionButton("ExcludeTargetGet", "Get Target Genes")
    )
  ),
  tags$br(),
  fluidRow(
    box(
      title = "Selected Data (To Download)",
      width = 6,
      status = "primary",
      solidHeader = TRUE,
      downloadButton("DownloadPathData"),
      tags$hr(),
      DT::DTOutput("filteredDownloadView")
    ),
    box(
      title = "All Nodes",
      DT::DTOutput("TotalDataView"),
      width = 6,
      status = "primary"
    )
  )
)



targets_tab <- tabItem(
  tabName = "targets_tab",
  fluidRow(
    box(
      actionButton("BackToPathways", "Go Back to Pathways"),
      tags$hr(),
      uiOutput("TFSearchBox")
    ),
    box(
      DT::dataTableOutput("TargetGeneTable"),
      downloadButton("DownloadTargets")
    )
  ),
  fluidRow(
    box(DT::dataTableOutput("UniqueGenes"), title = "Genes Unique to a TR")
  )
)

comparison_tab <- tabItem(
  tabName = "comparison_tab",
  fluidPage(
    fluidRow(tags$br(), box(uiOutput("CompareSelection"),
      actionButton("ComparePush", "Compare Paths"),
      title = "Comparison Controls"
    )),
    fluidRow(tags$br(), tags$div(id = "Placeholder"))
  )
)




dashboard <- dashboardBody(
  useShinyalert(),
  useShinyjs(),
  # extendShinyjs(text = )
  # tags$head(tags$script(jscode)),
  tabItems(
    pathway_tab,
    targets_tab,
    comparison_tab
  )
)


sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Pathways", tabName = "pathway_tab", icon = icon("share-alt")),
    menuItem("Target Genes", tabName = "targets_tab", icon = icon("dna")),
    menuItem("Comparison", tabName = "comparison_tab", icon = icon("balance-scale"))
  ),
  uiOutput("FilterSelection"),
  uiOutput("ReceptorSearch"),

  sidebarMenu(
    id = "OtheControls",
    menuItem("Other Controls", radioButtons("SizeBy",
      label = "Size Nodes By:",
      choiceNames = c("LEC Expression", "LFC Expression", "None"),
      choiceValues = c("Hoang_LEC", "Hoang_LFC", "None"),
      selected = "Hoang_LEC"
    ), uiOutput("FilterNPaths"))
  )
)



ui <- dashboardPage(
  header,
  sidebar,
  dashboard
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  from_coords <- ranked_data %>%
    rename(from_x = rank_x, from_y = rank_y)

  to_coords <- ranked_data %>%
    rename(to_x = rank_x, to_y = rank_y)

  selected_name <- reactive({
    # # browser()
    reference %>%
      filter(Name == input$plot_selected) %>%
      select(Name, type) %>%
      slice(1)
  })

  reacVal <- reactiveValues()

  # filter
  observeEvent(input$SearchFilter, {
    # session$sendCustomMessage(type = "BareFlow_set", message = character(0))

    # input$BareFlow_selected = NULL
  })
  observeEvent(input$SearchFamily, {
    new_nodes <- node_families %>%
      filter(Family %in% input$SearchFamily) %>%
      pull(Nodes) %>%
      unique()
    session$sendCustomMessage(type = "BareFlow_set", message = character(0))

    session$sendCustomMessage(type = "SearchFilter_set", message = new_nodes)


    # input$BareFlow_selected = NULL
  })
  # Observe the event of adding a new plot
  observe({
    reacVal$current_compareID <- 0
  })

  observeEvent(input$ComparePush, {
    # browser()

    isolate({
      # browser()
      if (is.null(input$CompareSelect)) {
        shinyalert("Oops!", "You havent selected any nodes.", type = "error")
      } else {
        reacVal$current_compareID <- reacVal$current_compareID + 1

        # browser()

        p <- renderPlot({
          make_flow_plot(isolate(input$CompareSelect))
        })
        output[[glue::glue("comparePlot{reacVal$current_compareID}")]] <- p
        compareBox <- box(
          plotOutput(glue::glue("comparePlot{reacVal$current_compareID}")),
          actionButton(inputId = glue::glue("Remove_{reacVal$current_compareID}"), label = "Remove")
        )
        insertUI("#Placeholder", where = "afterEnd", ui = tags$div(compareBox, id = glue::glue("CompareID_{reacVal$current_compareID}")))
      }
    })
  })

  observe({
    map(1:reacVal$current_compareID, ~ {
      observeEvent(input[[glue::glue("Remove_{.x}")]], {
        removeUI(selector = glue::glue("#CompareID_{.x}"))
      })
    })
  })


  observeEvent(input$BareFlow_selected, {
    # browser()
    Name_Input <- input$BareFlow_selected %||% "Acvr1"

    Type <- pathways %>%
      filter(Name %in% Name_Input) %>%
      head(1) %>%
      pull(Type)

    session$sendCustomMessage(type = "SearchFilter_set", message = Name_Input)
    # session$sendCustomMessage(type = 'FilterBy_set', message = Type)
    updateRadioButtons(session = session, inputId = "FilterBy", selected = Type)
    # reacVal$filter_node <- list(Name = Name_Input %||% input$SearchFilter %||% "Acvr1", Type = Type %||% "Receptor")
  })

  # observeEvent(input$BareFlow_selected, {
  #
  #   # extenral_link <- glue::glue("Shiny.addCustomMessageHandler('mymessage', function(message) {window.open(https://www.uniprot.org/uniprot/?query={reacVal$filter_node$Name}&sort=score);});")
  #
  #   if(!exists("original_time")) {
  #     reacVal$original_time = Sys.time()
  #     return()
  #   }
  #
  #   reacVal$new_time = Sys.time()
  #
  #   time_diff = reacVal$new_time - reacVal$original_time
  #
  #   if(time_diff < 0.5) {
  #     session%
  #   }
  #
  # })

  # observeEvent(input$BareFlow_selected, {
  #   # force(input$BareFlow_selected)
  #
  #   # browser()
  #   if(!is.null(reacVal$original_time) && ((Sys.time() - isolate(reacVal$original_time)) > 0.6)) {
  #     # browser()
  #     logjs(Sys.time() - isolate(reacVal$original_time))
  #     reacVal$original_time <- Sys.time()
  #     return()
  #   }
  #
  #   if(is.null(reacVal$original_time)) {
  #     # browser()
  #     reacVal$original_time <- Sys.time()
  #     # browser()
  #     return()
  #   }
  #   # browser()
  #
  #   reacVal$new_time <- Sys.time()
  #
  #   time_diff <- isolate(reacVal$new_time) - isolate(reacVal$original_time)
  #   print(time_diff)
  #   logjs(time_diff)
  #
  #   if(time_diff < 0.5) {
  #     # browser()
  #     search_string <- glue::glue('https://www.uniprot.org/uniprot/?query={isolate(reacVal$filter_node$Name)}&sort=score')
  #     js_code <- glue::glue("window.open('{search_string}', '_blank');")
  #     runjs(js_code)
  #     # logjs(js_code)
  #     # runjs("alert('hello');")
  #     # MARK - Send the message to open a new tab
  #     # browser()
  #   }
  #   # browser()
  # })


  observeEvent(input$ExcludeGraph_selected, {
    reacVal$exclude_nodes <- input$BareFlow_selected
  })

  observe({
    # browser()
    input$searchFamily


    Name_Input <- input$BareFlow_selected



    if (is.null(Name_Input)) {
      Type <- input$FilterBy
    } else {
      Type <- pathways %>%
        filter(Name %in% Name_Input) %>%
        head(1) %>%
        pull(Type)
    }


    # reacVal$filter_node = list(Name = input$SearchFilter %||%  "Acvr1", Type = input$FilterBy %||% "Receptor")
    reacVal$filter_node <- list(Name = Name_Input %||% input$SearchFilter %||% "Acvr1", Type = Type %||% "Receptor")
  })


  # filter_node <- reactive({
  #     # # browser()
  #     list(Name = input$SearchFilter %||%  "Acvr1", Type = input$FilterBy %||% "Receptor")
  # })

  # filter_input <- reactive({
  #     # # browser()
  #     if(input$FilterBy == "Contain_R") {
  #         filter_input_ret = input$SearchFilter # Maybe assign reactive values here (or some type of reactive)
  #     } else {
  #         filter_input_ret = input$TF_Filter
  #     }
  #     return(filter_input_ret)
  # })
  #
  # observeEvent(input$SearchFilter, {
  #     # # browser()
  #     print(input$SearchFilter)
  # })

  # name_ref <- reactive({
  #     # # browser()
  #     pathways %>%
  #     filter(Type == reacVal$filter_node$Type, Name == reacVal$filter_node$Name)
  # })
  observe({
    # browser()
    proc_ref <- pathways %>%
      # filter(Type == reacVal$filter_node$Type, Name %in% reacVal$filter_node$Name) %>%
      filter(Name %in% reacVal$filter_node$Name) %>%
      pull(Process) %>%
      unique()

    reacVal$filtered_pairs <- all_pairs %>%
      filter(Process %in% proc_ref) %>% #
      count(From, To) %>%
      left_join(from_coords, c("From" = "Name")) %>%
      left_join(to_coords, c("To" = "Name"))
  })


  # filtered_pairs <- reactive({
  #     # browser()
  #
  #     # map_lgl(!!sym(input$FilterBy), ~{(filter_input() %||% "Eng") %in% .})
  #
  #     proc_ref <- reacVal$name_ref %>%
  #         pull(Process) %>%
  #         unique()
  #
  #
  #     all_pairs %>%
  #     filter(Process %in% proc_ref) %>% # TODO: This is whwere i need to use a quosured inpput for start/end filtering. Accompanied by radio buttons for direction (maybe have a look for some nicer radio buttons).
  #     count(From, To) %>%
  #     left_join(from_coords, c("From" = "Name")) %>%f
  #     left_join(to_coords, c("To" = "Name"))
  # })

  included_nodes <- reactive({
    # browser()
    c(unique(reacVal$filtered_pairs$From), unique(reacVal$filtered_pairs$To))
  })



  # # Set up sizing variable
  # observe({
  #     if(input$SizeBy != "None") {
  #         ranked_data <- reactice({reference %>%
  #             filter(Cell == input$SizeBy) %>%
  #             select(Name, Expression) %>%
  #             right_join(ranked_data)
  #         })
  #
  #         quo_size <- expr(Expression)
  #     } else {
  #         quo_size <- NULL
  #     }
  # })
  #

  observe({
    reacVal$filtered_ranks <- ranked_data %>%
      filter(Name %in% c(unique(reacVal$filtered_pairs$From), unique(reacVal$filtered_pairs$To)))
  })

  observeEvent(input$TRToTarget, {
    # browser()
    TFs <- reacVal$filtered_ranks %>%
      filter(type == "TFs") %>%
      pull(Name)

    updateTabItems(session, inputId = "tabs", selected = "targets_tab")

    updateSelectInput(session, inputId = "TFSearchBox", selected = TFs)
    session$sendCustomMessage(type = "TFSearchBox_set", message = TFs)
  })
  # observeEvent(input$UniProtSearch, {
  #   search_string <- glue::glue("https://www.uniprot.org/uniprot/?query={isolate(reacVal$filter_node$Name)}&sort=score")
  #   js_code <- glue::glue("window.open('{search_string}', '_blank');")
  #   runjs(js_code)
  # })

  output$UniProtSearchButton <- renderUI({
      search_string <- glue::glue("https://www.uniprot.org/uniprot/?query={(reacVal$filter_node$Name)}&sort=score")

    shiny::actionButton("UniProtSearch", "Search UniProt", onclick = glue::glue("window.open('{search_string}', '_blank');"))
  })


  observeEvent(input$BackToPathways, {
    updateTabItems(session, inputId = "tabs", selected = "pathway_tab")
  })


  # filtered_ranks <- reactive({
  #         # browser()
  #         ranked_data %>%
  #         filter(Name %in% included_nodes())
  #     })
  #


  output$ReceptorSearch <- renderUI({
    # # browser()
    options <- pathways %>%
      filter(Type == reacVal$filter_node$Type) %>%
      pull(Name) %>%
      unique() %>%
      sort()



    # if(input$FilterBy == "Receptor") {
    #     label <- "Receptor"
    #     selected <- "Eng"
    # } else if(input$FilterBy == "TF") {
    #     label <- "TF"
    #     selected <- "Arnt"
    # } else {
    #     print("FOR SOME REASON THE SEARCH LOGIC FAILED")
    #     print(input$FilterBy)
    # }
    selectInput("SearchFilter", glue::glue("Filtering By {reacVal$filter_node$Type}"), choices = options, selected = reacVal$filter_node$Name %||% options[[1]], multiple = TRUE)
  })


  output$FamilySearch <- renderUI({
    options <- node_families$Family %>% unique() %>% sort()


    selectInput("SearchFamily", glue::glue("Search Families"), choices = options, multiple = TRUE)
  })

  output$FilterNPaths <- renderUI({
    # browser()
    sliderInput("PathFilter", "Filter Top N Paths",
      min = 1,
      # max = nrow(reacVal$filtered_pairs)/4,
      max = nrow(reacVal$filtered_pairs %>%
        distinct(From, To, n, .keep_all = TRUE)),
      value = nrow(reacVal$filtered_pairs %>%
        distinct(From, To, n, .keep_all = TRUE)),
      step = 1
    )
  })

  output$FilterSelection <- renderUI({
    radioButtons("FilterBy",
      label = "Filter The Pathways By:",
      choices = c("Receptor", "Kinase", "TR"),
      # choiceValues = c("Contain_R", "Contain_TF"),
      selected = "Receptor"
    )
  })

  # output$TFSearch <- renderUI({
  #     options <- sort(unique(unlist(all_pairs$Contain_K)))
  #     selectInput("TF_Filter", "Filter By Receptor", choices = options, selected = "Arnt")
  # })

  output$TFSearchBox <- renderUI({
    options <- pathways %>%
      filter(Type == "TF") %>%
      pull(Name) %>%
      unique() %>%
      sort()

    selected_options <- reacVal$filtered_ranks %>%
      filter(type == "TFs") %>%
      pull(Name)

    selectInput("TFSearch", "Search TRs", choices = options, selected = selected_options %||% "Arnt", multiple = TRUE)
  })

  output$CompareSelection <- renderUI({
    options <- pathways %>%
      pull(Name) %>%
      unique() %>%
      sort()

    selectInput("CompareSelect", "Select Nodes for Comparison", choices = options, selected = options[[1]], multiple = TRUE)
  })




  p <- reactive({
    # browser()

    if (input$SizeBy != "None") {
      quo_size <- sym(input$SizeBy)
    } else {
      quo_size <- quo(3)
    }
    # quo_size <- ifelse(input$SizeBy != "None", sym(input$SizeBy), quo(3))
    # browser()


    size_data <- reacVal$filtered_ranks %>% spread(Cell, Expression)
    segment_data <- reacVal$filtered_pairs %>%
      distinct(From, To, n, from_x, from_y, to_x, to_y, .keep_all = FALSE) %>%
      top_n(input$PathFilter %||% nrow(.), n)

    p_temp <- ranked_data %>%
      ggplot(aes(x = rank_x, y = rank_y)) +
      geom_segment(aes(x = from_x, xend = to_x, y = from_y, yend = to_y, colour = n, alpha = n, size = n / 10), data = segment_data) +
      geom_point_interactive(aes(tooltip = Name, data_id = Name), colour = "grey", size = 4) +
      geom_point_interactive(aes(size = !!quo_size, tooltip = Name, data_id = Name), data = size_data) +
      geom_hline(yintercept = 0.75, colour = "red", linetype = "dashed") +
      geom_hline(yintercept = -0.75, colour = "red", linetype = "dashed") +
      annotate("label", x = 12.5, y = c(-1, 0, 1.5), label = c("TR", "K", "R"), colour = "red", size = 10) +
      xlim(c(-9.9, 12.6)) +
      ylim(c(-1, 2)) +
      ggraph::theme_graph() +
      scico::scale_color_scico(palette = "roma")
    # theme(panel.background = element_rect(fill = 'beige'))


    if (input$SizeBy != "None") {
      p_temp <- p_temp + scale_size(range = c(0, 10))
    }
    p_temp
  })

  output$BareFlow <- renderGirafe({
    gir <- girafe(code = {
      print(p())
    }, width_svg = 20, height_svg = 10)
    girafe_options(gir, opts_tooltip(offx = 20, offy = 20), opts_selection(type = "single"))
  })



  p_exclude <- reactive({
    if (input$SizeBy != "None") {
      quo_size <- sym(input$SizeBy)
    } else {
      quo_size <- quo(3)
    }
    # quo_size <- ifelse(input$SizeBy != "None", sym(input$SizeBy), quo(3))
    # # # browser()


    exclude_paths <- pathways %>%
      filter(Name %in% input$ExcludeGraph_selected) %>%
      pull(Process) %>%
      unique()

    segment_data <- reacVal$filtered_pairs %>%
      distinct(From, To, n, from_x, from_y, to_x, to_y, .keep_all = FALSE) %>%
      top_n(input$PathFilter %||% nrow(.), n)

    # browser()

    proc_ref <- pathways %>%
      # filter(Type == reacVal$filter_node$Type, Name %in% reacVal$filter_node$Name) %>%
      filter(Name %in% reacVal$filter_node$Name) %>%
      pull(Process) %>%
      unique()

    exclusions_removed <- all_pairs %>%
      filter(Process %in% proc_ref) %>%
      filter(!(Process %in% exclude_paths)) %>%
      count(From, To) %>%
      left_join(from_coords, c("From" = "Name")) %>%
      left_join(to_coords, c("To" = "Name"))
    # browser()
    #

    size_data <- reacVal$filtered_ranks %>% spread(Cell, Expression)
    size_data_filtered <- size_data %>%
      filter(Name %in% c(unique(exclusions_removed$From), unique(exclusions_removed$To)))
    size_data_selected <- size_data %>%
      filter(Name %in% input$ExcludeGraph_selected)

    # exc_filtered_pairs <- reacVal$filtered_pairs %>%
    # filter((Process %in% exc) )
    # reacVal$exclude_nodes



    p_temp <- exclusions_removed %>%
      ggplot(aes(x = rank_x, y = rank_y)) +
      geom_segment(aes(x = from_x, xend = to_x, y = from_y, yend = to_y, colour = n, alpha = n^2, size = n / 10)) +
      # geom_point_interactive(aes(tooltip = Name, data_id = Name), colour = "grey", size = 4) +
      geom_point_interactive(aes(tooltip = Name, data_id = Name), colour = "grey", size = 4, data = size_data) +
      geom_point_interactive(aes(size = !!quo_size, tooltip = Name, data_id = Name), data = size_data_filtered) +
      geom_point_interactive(aes(tooltip = Name, data_id = Name), colour = "red", size = 4, data = size_data_selected) +
      geom_hline(yintercept = 0.75, colour = "red", linetype = "dashed") +
      geom_hline(yintercept = -0.75, colour = "red", linetype = "dashed") +
      annotate("label", x = 12.5, y = c(-1, 0, 1.5), label = c("TR", "K", "R"), colour = "red", size = 10) +
      xlim(c(-9.9, 12.6)) +
      ylim(c(-1, 2)) +
      ggraph::theme_graph()

    if (input$SizeBy != "None") {
      p_temp <- p_temp + scale_size(range = c(0, 10))
    }
    p_temp
  })



  output$ExcludeGraph <- renderGirafe({
    gir <- girafe(code = {
      print(p_exclude())
    }, width_svg = 20, height_svg = 10)
    girafe_options(gir, opts_tooltip(offx = 20, offy = 20), opts_selection(type = "multiple"))
  })



  output$AnnotatedFlow <- renderPlot({
    # filtered_pairs <- all_pairs %>%
    #     filter(Start_R == (input$ReceptorFilter %||% "Eng")) %>%
    #     count(From, To) %>%
    #     left_join(from_coords, c("From" = "Name")) %>%
    #     left_join(to_coords, c("To" = "Name"))
    #
    # p <- ranked_data %>%
    #     ggplot(aes(x = rank_x, y = rank_y)) +
    #     geom_point() +
    #     geom_segment(aes(x = from_x, xend = to_x, y = from_y, yend = to_y, alpha = n), data = filtered_pairs) +
    #     ggraph::theme_graph()
    #
    # p +
    #     annotate(xmin = min(ranked_data$rank_x) -0.2, xmax = max(ranked_data$rank_x) +0.2, ymin = 0.5, ymax = 1.9, colour = "red", geom = "rect", fill = "red", alpha = 0.1) +
    #     annotate(x = 0.5, y = 1.7, label = "Receptor", colour = "red", geom = "label") +
    #     annotate(xmin = min(ranked_data$rank_x) -0.2, xmax = max(ranked_data$rank_x) +0.2, ymin = -1.5, ymax = 0.5, colour = "blue", geom = "rect", fill = "blue", alpha = 0.1) +
    #     annotate(x = 0.5, y = 0.2, label = "Kinase", colour = "blue", geom = "label") +
    #     annotate(xmin = min(ranked_data$rank_x) -0.2, xmax = max(ranked_data$rank_x) +0.2, ymin = -2.1, ymax = -1.5, colour = "forestgreen", geom = "rect", fill = "forestgreen", alpha = 0.1) +
    #     annotate(x = 0.5, y = -1.7, label = "TF", colour = "forestgreen", geom = "label") +
    #     theme_minimal()
    #
  })



  output$filteredDownloadView <- DT::renderDataTable({
    reacVal$filtered_ranks %>%
      select(-starts_with("rank")) %>%
      DT::datatable(options = list(scrollX = TRUE))
  })

  output$TotalDataView <- DT::renderDataTable({
    DT::datatable(reference, options = list(scrollX = TRUE))
  })

  output$TargetGeneTable <- DT::renderDataTable({
    if (is.null(input$TFSearch) == TRUE) {
      Targets_LEC %>%
        # filter(TF == ) %>%
        DT::datatable(options = list(scrollX = TRUE))
    } else {
      Targets_LEC %>%
        mutate(TR = stringr::str_to_title(TR)) %>%
        filter(TR %in% input$TFSearch) %>%
        DT::datatable(options = list(scrollX = TRUE))
    }
  })


  output$DownloadTargets <- downloadHandler(
    filename = function() {
      paste(reacVal$filter_node$Name, "_Targets", ".csv", sep = "")
    },
    content = function(file) {
      Targets_LEC %>%
        mutate(TR = stringr::str_to_title(TR)) %>%
        filter(TR %in% input$TFSearch) %>%
        write_csv(file)
    }
  )


  output$DownloadPathData <- downloadHandler(
    filename = function() {
      paste(reacVal$filter_node$Name, "_expression", ".csv", sep = "")
    },
    content = function(file) {
      write_csv(reacVal$filtered_ranks, file)
    }
  )


  output$UniqueGenes <- DT::renderDataTable({
    Targets_LEC %>%
      add_count(Target) %>%
      filter(n == 1) %>%
      select(-n)
  })
}

make_flow_plot <- function(Target_Name) {
  # browser()
  from_coords <- ranked_data %>%
    rename(from_x = rank_x, from_y = rank_y)

  to_coords <- ranked_data %>%
    rename(to_x = rank_x, to_y = rank_y)
  quo_size <- sym("Hoang_LEC")
  proc_ref <- pathways %>%
    filter(Name %in% Target_Name) %>%
    pull(Process) %>%
    unique()

  filtered_pairs <- all_pairs %>%
    filter(Process %in% proc_ref) %>% #
    count(From, To) %>%
    left_join(from_coords, c("From" = "Name")) %>%
    left_join(to_coords, c("To" = "Name"))

  filtered_ranks <- ranked_data %>%
    filter(Name %in% c(unique(filtered_pairs$From), unique(filtered_pairs$To)))

  segment_data <- filtered_pairs %>%
    distinct(From, To, n, from_x, from_y, to_x, to_y, .keep_all = FALSE) # %>%
  # top_n(input$PathFilter %||% nrow(.), n)

  size_data <- filtered_ranks %>% spread(Cell, Expression)

  ranked_data %>%
    ggplot(aes(x = rank_x, y = rank_y)) +
    geom_segment(aes(x = from_x, xend = to_x, y = from_y, yend = to_y, colour = n, alpha = n, size = n / 10), data = segment_data) +
    geom_point_interactive(aes(tooltip = Name, data_id = Name), colour = "grey", size = 4) +
    geom_point_interactive(aes(size = !!quo_size, tooltip = Name, data_id = Name), data = size_data) +
    geom_hline(yintercept = 0.75, colour = "red", linetype = "dashed") +
    geom_hline(yintercept = -0.75, colour = "red", linetype = "dashed") +
    annotate("label", x = 12.5, y = c(-1, 0, 1.5), label = c("TR", "K", "R"), colour = "red", size = 10) +
    xlim(c(-9.9, 12.6)) +
    ylim(c(-1, 2)) +
    ggraph::theme_graph() +
    scico::scale_color_scico(palette = "roma") +
    theme(legend.position = "none")
}

# Run the application
shinyApp(ui = ui, server = server)

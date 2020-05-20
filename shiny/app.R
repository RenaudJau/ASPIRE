#################################################################################
#------------------------Script pour faire tourner Shiny------------------------#
#################################################################################

# L'architecture est basée sur le script précédent d'Eric Maldonaldo

# A faire :
# - gérer taille des graphiques
# - ajouter logo
# - ajouter notice utilisation
# - tester avec autres jeux de données..
# - possibilité de gérer les couleurs?

## --------------------- Préambule  ------------------------------------

library(shiny)
library(knitr)
library(fmsb)
library(plotrix)
source(file = "https://raw.githubusercontent.com/RenaudJau/ASPIRE/master/R/ASPIRE_functions.R")

## --------------------- server function  ------------------------------------

server <- shinyServer(function(input,output){
  
  # This reactive function will take the inputs from UI.R and use them for read.table() 
  #to read the data from the file. It returns the dataset in the form of a dataframe.
  # file$datapath -> gives the path of the file
  data1 <- reactive({
    file1 <- input$file1
    file2 <- input$file2
    file3 <- input$file3
    if(is.null(file1) || is.null(file2) || is.null(file3)) {return ()}
    #read.table(file=file1$datapath, sep=input$sep, header = input$header, 
    # stringsAsFactors = input$stringAsFactors)
    read.table(file=file1$datapath, header = T)
  })
  
  data2 <- reactive({
    file1 <- input$file1
    file2 <- input$file2
    file3 <- input$file3
    if(is.null(file1) || is.null(file2) || is.null(file3)) {return ()}
    #read.table(file=file2$datapath, sep=input$sep, header = input$header, 
    # stringsAsFactors = input$stringAsFactors)
    read.table(file=file2$datapath, header = T)
    
  })
  
  data3 <- reactive({
    file1 <- input$file1
    file2 <- input$file2
    file3 <- input$file3
    if(is.null(file1) || is.null(file2) || is.null(file3)) {return ()}
    #read.table(file=file3$datapath, sep=input$sep, header = input$header,
    #stringsAsFactors = input$stringAsFactors)
    read.table(file=file3$datapath, header = T)
    
  })
  
  
  projet <- reactive({
    Projet_Sh<-ASPIRE_all(variable_df = data3(),objective_df = data2(),project_df = data1())
    Projet_Sh
    
  })
  
  # this reactive output contains the summary of the dataset and display the summary in table format
  output$filedf <- renderTable({
    
    if(is.null(data1()) || is.null(data2()) || is.null(data3())){return ()}
    
    projet()$Summary_All$Variables
    
  })
  
  output$filedf2 <- renderTable({
    
    if(is.null(data1()) || is.null(data2()) || is.null(data3())){return ()}
    
    projet()$Summary_All$Objectives
    
  })
  
  output$filedf3 <- renderTable({
    
    if(is.null(data1()) || is.null(data2()) || is.null(data3())){return ()}
    
    projet()$Summary_All$Scores
    
  })
  
  output$Liste_variables <- renderUI({
    selectInput("Liste", "selection variables", choices = as.character(projet()$Summary_All$Variables$name_var))
  })
  
  output$Liste_variables_ref <- renderUI({
    selectInput("Liste_variables_ref",
                "est-ce qu’on trace la valeur de la référence ou pas", c("oui" = "TRUE",
                                                                         "non" = "FALSE"))
  })
  
  output$Liste_variables_transf <- renderUI({
    selectInput("Liste_variables_transf",
                "est-ce que ce sont les valeurs transformées ou brutes ?", c("oui" = "TRUE",
                                                                             "non" = "FALSE"))
  })
  output$Plot_variables <- renderPlot({
    # Render a barplot
    Var.barplot.all(variable_name = input$Liste,project_all = projet(),
                    plot.ref = input$Liste_variables_ref,
                    transf = input$Liste_variables_transf)
  })
  
  
  output$Liste_objectifs <- renderUI({
    selectInput("Liste_o", "selection objectifs",
                choices = as.character(projet()$Summary_All$Objectives$name_objective))
  })
  
  output$Liste_objectifs_graphique <- renderUI({
    selectInput("Liste_obj_graphique", "quel graphique ?", c("Barre" = "barre","Radar" = "radar"))
  })
  
  output$Liste_objectifs_plot_ref <- renderUI({
    selectInput("Liste_objectifs_plot_ref", "Plot ref ?", c("oui" = "TRUE","non" = "FALSE"))
  })
  
  output$Plot_objectifs <- renderPlot({
    # Render a barplot
    if (input$Liste_obj_graphique=="barre"){
      Obj.barplot.all(objective_name = input$Liste_o,
                      project_all = projet(),plot.ref = input$Liste_objectifs_plot_ref,
                      las.x = 2, cex.x = 0.8)
    }
    else{
      Obj.radar.plot.all(objective_name = input$Liste_o,
                         project_all = projet(),plot.ref = input$Liste_objectifs_plot_ref)		
    }
  })
  
  
  
  output$Plot_radar_objectifs <- renderPlot({
    Obj.radar.plot.all(objective_name = input$Liste_o,
                       project_all = projet(),plot.ref = input$Liste_objectifs_plot_ref)	
  })
  
  output$Liste_projet_graphique <- renderUI({
    selectInput("Liste_projet_graphique", "quel graphique ?", c("Barre" = "barre","Radar" = "radar"))
  })
  
  output$Plot_projet <- renderPlot({
    # Render a barplot
    if (input$Liste_projet_graphique=="barre"){
      Proj.complete.barplot(proj = projet(),las.x = 2,cex.x = 0.8)
    }
    else{
      Proj.all.radar.plot(proj = projet())
    }
  })
  
  output$Plot_radar_projet <- renderPlot({
    
    Proj.all.radar.plot(proj = projet())
  })
  
  output$Plot_projet_acteur <- renderPlot({
    #data1()
    Proj.stak.barplot(proj = projet())
    
  })
  
  output$Plot_projet_recov <- renderPlot({
    #data1()
    Wheelscores.all(project_all = projet())
    
  })
  
  # this reactive output contains the summary of the dataset and display the summary in table format
  output$sum <- renderTable({
    if(is.null(data1()) || is.null(data2()) || is.null(data3())){return ()}
    input$file1
    
  })
  
  # This reactive output contains the dataset and display the dataset in table format
  output$table <- renderTable({
    if(is.null(data1()) || is.null(data2()) || is.null(data3())){return ()}
    data1()
  })
  
  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. 
  # Until the file is loaded, app will not show the tabset.
  output$tb <- renderUI({
    if (is.null(data1()) || is.null(data2()) || is.null(data3())) {
      h5("Select and upload the 3 files")
    }
    else{
      
      tabsetPanel(tabPanel("Scores calculation",
                           tableOutput("filedf"), 
                           tableOutput("filedf2"),
                           tableOutput("filedf3")),
                  tabPanel("Variables plots",
                           tableOutput("Liste_variables"),
                           tableOutput("Liste_variables_ref"),
                           tableOutput("Liste_variables_transf"),
                           plotOutput("Plot_variables")),
                  tabPanel("Objectives plots",
                           tableOutput("Liste_objectifs"),
                           tableOutput("Liste_objectifs_graphique"),
                           tableOutput("Liste_objectifs_plot_ref"),
                           plotOutput("Plot_objectifs")),
                  tabPanel("Project plots",
                           tableOutput("Liste_projet_graphique"),
                           plotOutput("Plot_projet")),
                  tabPanel("Stakeholders plots",
                           plotOutput("Plot_projet_acteur")),
                  tabPanel("Recovery wheel",
                           plotOutput("Plot_projet_recov")))
      
    }
  })
  
})


## --------------------- User interface  ------------------------------------

ui <- shinyUI(fluidPage(
  img(src='logo.png', height="100", width="100",align = "right"),
  titlePanel("ASPIRE assessment"),
  tags$div(checked = NA,
           tags$a(href = "Exemple_Utilisation_ASPIRE.pdf", target="_blank","Notice d'utilisation")
  ),
  sidebarLayout(
    
    sidebarPanel(
      tags$a(href = "ASPIRE_tab_exemple_Projet.txt", target="_blank","Fichier exemple Projet"),
      fileInput('file1', 'Choose project file',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      tags$a(href = "ASPIRE_tab_exemple_Objectifs.txt", target="_blank","Fichier exemple Objectifs"),
      fileInput('file2', 'Choose objectives file',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      tags$a(href = "ASPIRE_tab_exemple_Variables.txt", target="_blank","Fichier exemple Variables"),
      fileInput('file3', 'Choose variables file',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv'))
      
    ),
    mainPanel(
      uiOutput("tb")
      
      # use below code if you want the tabset programming in the main panel. 
      # If so, then tabset will appear when the app loads for the first time.
      #       tabsetPanel(tabPanel("Summary", verbatimTextOutput("sum")),
      #                   tabPanel("Data", tableOutput("table")))
    )
  )
))

## --------------------- Launch Shiny App  ------------------------------------

shinyApp(ui = ui, server = server)


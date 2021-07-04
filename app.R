#----------------------------------------------------------------------Libraries
library(shiny)
library(echarts4r)
library(reticulate)
library(fresh)
library(plotly)
#--------------------------------------------------------Sourcing Python scripts
source_python("pdfToTxt.py")
source_python("keyword_importance.py")
source_python("keywordTable.py")
#-----------------------------------------------------------------------------UI
ui <- navbarPage("Lecture de Curriculum Vitae d'un candidat",
        header=use_theme(create_theme(
        theme = "default",
        bs_vars_navbar(
        default_bg = "darkblue",
        default_link_color = "#FFFFFF",
        default_link_active_bg = "darkblue"))),
        column(6,
                fileInput("file1","Sélectionner le premier CV"),
                fileInput("file2","Sélectionner le deuxième CV"),
                fileInput("file3","Sélectionner le troisième CV")),
        
        tabsetPanel(
                   
          tabPanel(h5(strong("Compétences requises :"),
                style='color: white;
                background-color: darkblue;
                font-size: 15px;
                padding: 15px;'),
                            
                sidebarLayout(
                    sidebarPanel(
                      textInput("keyword_1","Entrer le premier mot clé"),
                      textInput("keyword_2","Entrer le deuxième mot clé"),
                      textInput("keyword_3","Entrer le troisième mot clé")),
                    
                    mainPanel(h5(strong("Premier candidat :"),
                                   style='color: white;
                                background-color: lightslategrey;
                                font-size: 10px;
                                padding: 10px;'),
                              tableOutput("candidate1_table"),
                                h5(strong("Deuxième candidat :"),
                                   style='color: white;
                                background-color: lightslategrey;
                                font-size: 10px;
                                padding: 10px;'),
                              tableOutput("candidate2_table"),
                                h5(strong("Deuxième candidat :"),
                                   style='color: white;
                                background-color: lightslategrey;
                                font-size: 10px;
                                padding: 10px;'),
                              tableOutput("candidate3_table")))),
          
              tabPanel(h5(strong("Représentation graphique :"),
                      style='color: white;
                      background-color: darkblue;
                      font-size: 15px;
                    padding: 15px;'),
                           
                    fluidRow(h5(strong("Comparaison selon Term-Frequency :"),
                            style='color: black;
                            background-color: lemonchiffon;
                            font-size: 12px;
                            padding: 12px;'),
                          column(4,h5(strong("Premier candidat :"),
                              style='color: white;
                              background-color: lightslategrey;
                              font-size: 10px;
                              padding: 10px;'),
                              plotlyOutput("hist_candidate1_tf")),
                          column(4,h5(strong("Deuxième candidat :"),
                              style='color: white;
                              background-color: lightslategrey;
                              font-size: 10px;
                              padding: 10px;'),
                              plotlyOutput("hist_candidate2_tf")),
                          column(4,h5(strong("Troisième candidat :"),
                              style='color: white;
                              background-color: lightslategrey;
                              font-size: 10px;
                              padding: 10px;'),
                              plotlyOutput("hist_candidate3_tf"))),
                  br(),
                  br(),
                           
                  fluidRow(h5(strong("Comparaison selon l'occurrence :"),
                            style='color: black;
                            background-color: lemonchiffon;
                            font-size: 12px;
                            padding: 12px;'),
                        column(4,h5(strong("Premier candidat :"),
                            style='color: white;
                            background-color: lightslategrey;
                            font-size: 10px;
                            padding: 10px;'),
                            plotlyOutput("hist_candidate1_occurrence")),
                        column(4,h5(strong("Deuxième candidat :"),
                            style='color: white;
                            background-color: lightslategrey;
                            font-size: 10px;
                            padding: 10px;'),
                            plotlyOutput("hist_candidate2_occurrence")),
                        column(4,h5(strong("Troisième candidat :"),
                            style='color: white;
                            background-color: lightslategrey;
                            font-size: 10px;
                            padding: 10px;'),
                            plotlyOutput("hist_candidate3_occurrence")))
                   ))
)

#------------------------------------------------------------------------Server
server <- function(input, output) {
  path1<-reactive(input$file1)
  path2<-reactive(input$file2)
  path3<-reactive(input$file3)
  keyword1<-reactive(input$keyword_1)
  keyword2<-reactive(input$keyword_2)
  keyword3<-reactive(input$keyword_3)
  
  table_def1<-reactive({req(path1()$datapath,cancelOutput = TRUE)
    Text<-pdfToTxt(path1()$datapath)
    req(keyword1())
    table1<-keyword_importance(keyword1(),path1()$datapath)
    tabledef1<-data.frame(keywordTable(keyword1(),path1()$datapath))
    req(keyword2())
    table2<-keyword_importance(keyword2(),path1()$datapath)
    tabledef2<-data.frame(keywordTable(keyword2(),path1()$datapath))
    req(keyword3())
    table3<-keyword_importance(keyword3(),path1()$datapath)
    tabledef3<-data.frame(keywordTable(keyword3(),path1()$datapath))
    
    tabledef<-rbind(tabledef1, tabledef2, tabledef3)
    df<-tabledef[order(tabledef$tf),]})
  
  
  table_def2<-reactive({req(path2()$datapath,cancelOutput = TRUE)
    Text<-pdfToTxt(path2()$datapath)
    req(keyword1())
    table1<-keyword_importance(keyword1(),path2()$datapath)
    tabledef1<-data.frame(keywordTable(keyword1(),path2()$datapath))
    req(keyword2())
    table2<-keyword_importance(keyword2(),path2()$datapath)
    tabledef2<-data.frame(keywordTable(keyword2(),path2()$datapath))
    req(keyword3())
    table3<-keyword_importance(keyword3(),path2()$datapath)
    tabledef3<-data.frame(keywordTable(keyword3(),path2()$datapath))
    
    tabledef<-rbind(tabledef1, tabledef2, tabledef3)
    df<-tabledef[order(tabledef$tf),]})
  
  table_def3<-reactive({req(path3()$datapath,cancelOutput = TRUE)
    Text<-pdfToTxt(path3()$datapath)
    req(keyword1())
    table1<-keyword_importance(keyword1(),path3()$datapath)
    tabledef1<-data.frame(keywordTable(keyword1(),path3()$datapath))
    req(keyword2())
    table2<-keyword_importance(keyword2(),path3()$datapath)
    tabledef2<-data.frame(keywordTable(keyword2(),path3()$datapath))
    req(keyword3())
    table3<-keyword_importance(keyword3(),path3()$datapath)
    tabledef3<-data.frame(keywordTable(keyword3(),path3()$datapath))
    
    tabledef<-rbind(tabledef1, tabledef2, tabledef3)
    df<-tabledef[order(tabledef$tf),]})
  
  output$candidate1_table<-renderTable({table_def1()},bordered = TRUE)
  output$candidate2_table<-renderTable({table_def2()},bordered = TRUE)
  output$candidate3_table<-renderTable({table_def3()},bordered = TRUE)
  
  output$hist_candidate1_tf<-renderPlotly({
    plot_ly(table_def1(),x = ~mot_clé,
            y = ~tf,
            type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'white',
                                      width = 1)))%>%
      layout(yaxis=list(title="Pertinence de mot selon son tf"),
             xaxis = list(showticklabels = FALSE,
                          title='mots clés'))%>%
      layout(plot_bgcolor='rgb(254, 247, 234)')%>%
      layout(paper_bgcolor='rgb(254, 247, 234)')
  })
  
  
  output$hist_candidate2_tf<-renderPlotly({
    plot_ly(table_def2(),x = ~mot_clé,
            y = ~tf,
            type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'white',
                                      width = 1)))%>%
      layout(yaxis=list(title="Pertinence de mot selon son tf"),
             xaxis = list(showticklabels = FALSE,
                          title='mots clés'))%>%
      layout(plot_bgcolor='rgb(254, 247, 234)')%>%
      layout(paper_bgcolor='rgb(254, 247, 234)')
  })
  
  
  output$hist_candidate3_tf<-renderPlotly({
    plot_ly(table_def3(),x = ~mot_clé,
            y = ~tf,
            type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'white',
                                      width = 1)))%>%
      layout(yaxis=list(title="Pertinence de mot selon son tf"),
             xaxis = list(showticklabels = FALSE,
                          title='mots clés'))%>%
      layout(plot_bgcolor='rgb(254, 247, 234)')%>%
      layout(paper_bgcolor='rgb(254, 247, 234)')
  })
  
  output$hist_candidate1_occurrence<-renderPlotly({
    plot_ly(table_def1(),x = ~mot_clé,
            y = ~occurrence,
            type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'white',
                                      width = 1)))%>%
      layout(yaxis=list(title="Pertinence de mot selon son occurrence"),
             xaxis = list(showticklabels = FALSE,
                          title='mots clés'))%>%
      layout(plot_bgcolor='rgb(254, 247, 234)')%>%
      layout(paper_bgcolor='rgb(254, 247, 234)')
  })
  
  output$hist_candidate2_occurrence<-renderPlotly({
    plot_ly(table_def2(),x = ~mot_clé,
            y = ~occurrence,
            type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'white',
                                      width = 1)))%>%
      layout(yaxis=list(title="Pertinence de mot selon son occurrence"),
             xaxis = list(showticklabels = FALSE,
                          title='mots clés'))%>%
      layout(plot_bgcolor='rgb(254, 247, 234)')%>%
      layout(paper_bgcolor='rgb(254, 247, 234)')
  })
  
  output$hist_candidate3_occurrence<-renderPlotly({
    plot_ly(table_def3(),x = ~mot_clé,
            y = ~occurrence,
            type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'white',
                                      width = 1)))%>%
      layout(yaxis=list(title="Pertinence de mot selon son occurrence"),
             xaxis = list(showticklabels = FALSE,
                          title='mots clés'))%>%
      layout(plot_bgcolor='rgb(254, 247, 234)')%>%
      layout(paper_bgcolor='rgb(254, 247, 234)')
  })
  
}

shinyApp(ui = ui, server = server)


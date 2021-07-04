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
                     default_link_active_bg = "darkblue"
                   ))),
                 column(6,fileInput("file","Sélectionner un CV")),
                 tabsetPanel(
                   
                   
                   tabPanel(h5(strong("Compétences requises :"),
                               style='color: white;
                               background-color: darkblue;
                               font-size: 15px;
                    padding: 15px;'),
                            
                            sidebarLayout(
                              sidebarPanel(
                                textInput("keyword_1",
                                          "Entrer le premier mot clé"),
                                textInput("keyword_2",
                                          "Entrer le deuxième mot clé"),
                                textInput("keyword_3",
                                          "Entrer le troisième mot clé")
                              ),
                              mainPanel(
                                tableOutput("candidate_table"),
                                
                                fluidRow(column(4,
                                          plotlyOutput("hist_candidate_tf")),
                                       column(4,
                                          plotlyOutput("hist_candidate_idf")),
                                       column(4,
                                        plotlyOutput("hist_candidate_tf_idf")))
                              )
                            )))
)

#------------------------------------------------------------------------Server
server <- function(input, output) {
  path<-reactive(input$file)
  keyword1<-reactive(input$keyword_1)
  keyword2<-reactive(input$keyword_2)
  keyword3<-reactive(input$keyword_3)
  
  table_def<-reactive({req(path()$datapath,cancelOutput = TRUE)
    Text<-pdfToTxt(path()$datapath)
    req(keyword1())
    table1<-keyword_importance(keyword1(),path()$datapath)
    tabledef1<-data.frame(keywordTable(keyword1(),path()$datapath))
    req(keyword2())
    table2<-keyword_importance(keyword2(),path()$datapath)
    tabledef2<-data.frame(keywordTable(keyword2(),path()$datapath))
    req(keyword3())
    table3<-keyword_importance(keyword3(),path()$datapath)
    tabledef3<-data.frame(keywordTable(keyword3(),path()$datapath))
    
    tabledef<-rbind(tabledef1, tabledef2, tabledef3)
    df<-tabledef[order(tabledef$tf_idf),]})
  
  output$candidate_table<-renderTable({table_def()},bordered = TRUE)
  
  output$hist_candidate_tf<-renderPlotly({
    plot_ly(table_def(),x = ~mot_clé,
            y = ~tf,
            type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'white',
                                      width = 1)))%>%
      layout(yaxis=list(title="L'importance de mot selon tf"),
             xaxis = list(showticklabels = FALSE,
                          title='mots clés'))%>%
      layout(plot_bgcolor='rgb(254, 247, 234)')%>%
      layout(paper_bgcolor='rgb(254, 247, 234)')
    })
  
  output$hist_candidate_idf<-renderPlotly({
    plot_ly(table_def(),x = ~mot_clé,
            y = ~idf,
            type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'white',
                                      width = 1)))%>%
      layout(yaxis=list(title="L'importance de mot selon idf"),
             xaxis = list(showticklabels = FALSE,
                          title='mots clés'))%>%
      layout(plot_bgcolor='rgb(254, 247, 234)')%>%
      layout(paper_bgcolor='rgb(254, 247, 234)')
  })
  
  output$hist_candidate_tf_idf<-renderPlotly({
    plot_ly(table_def(),x = ~mot_clé,
            y = ~tf_idf,
            type = 'bar', text = text,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'white',
                                      width = 1)))%>%
      layout(yaxis=list(title="L'importance de mot selon tf_idf"),
             xaxis = list(showticklabels = FALSE,
                          title='mots clés'))%>%
      layout(plot_bgcolor='rgb(254, 247, 234)')%>%
      layout(paper_bgcolor='rgb(254, 247, 234)')
  })
  
}

shinyApp(ui = ui, server = server)


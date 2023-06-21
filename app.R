
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    Anisio Bule
#


#libraries
library(shiny)
library(shinycssloaders)
#internet
library(httr)

#fonts
library(extrafont)

#carpintery
library(glue)
library(scales)
library(lubridate)
library(forcats)
library(reactable)
library(htmltools)
library(fontawesome)
#maps
#library(sf)



#ggplot
library(grid)
library(ggplot2)
library(ggrepel)
library(cowplot)


#tidyverse: 
library(tidyr)
library(stringr)
library(dplyr)


#other
library(rio)
library(cli)
library(zoo)

#   devtools::install_github("jcheng5/googleCharts")
#   https://github.com/jcheng5/googleCharts

#shiny
library(DT)
library(Hmisc)
library(htmltools)

library(shinydashboardPlus)
library(shinyauthr)
library(shinymanager)

library(shinydashboard)
library(shinyWidgets)
library(highcharter)
library(rdrop2)

#library(rsconnect)

#rsconnect::deployApp(appDir = 'C:/Users/anisi/Documents/GitHub/COVID19_DASHBOARD',
#                    appName = 'Covid19_Dashboard',
#                   account = 'infoinsupdatedashboard' )



inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"


#library(esquisse)

# Menu de Autenticacao
#metodo de credenciais

credentials <- data.frame(
  user = c("anisio", "pirolita"),
  password = c("123", "321"),
  admin = c(T,F),
  # comment = c("alsace", "auvergne", "bretagne"), %>% 
  stringsAsFactors = FALSE
)



#End of Hiding Menu

#Fim de implementacao metodo de authenticacao

source(".RProfile", encoding = "UTF-8")


#deployAPI()


#Metodos auxiliares

ultima_data <- max(as.Date(BD_3_meses$Data_reporte))
PAGE_TITLE <- "INS Dashboard "

ui <- secure_app( head_auth = tags$script(inactivity),
                  
                  tags_top = tags$img(
                    src = "INS.png", width = 100
                  ),

  fluidPage( 
    theme = shinythemes::shinytheme("flatly"),#yeti,spacelab
    #color: cerulean,spacelab,journal,darkly,lumen,flatly
    tags$style(HTML(".navbar{background-color: #00879C}")),
    
    titlePanel(windowTitle = PAGE_TITLE,
               title =
                 div(
                   img(
                     src = "INS.png",
                     height = 100,
                     width = 170,
                     style = "margin:10px 10px"
                   ),
                   PAGE_TITLE
                 )
    ),
    
    navbarPage("INS",
               position = "static-top",
               collapsible = TRUE,
               tags$html(HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">INS</a>')),
               
               
               #Panel reported daily -----------------------------------------
               tabPanel("Reporte diário", verbatimTextOutput("auth_output"),
                        icon = icon("dashboard"),
                        "Última actualizacão: ", ultima_data,tags$b(uiOutput("date")),tags$br(), hr(),
                        fluidRow(
                          infoBoxOutput("progressBox", width = 3), infoBoxOutput("progressBox1", width = 3), infoBoxOutput("progressBox_taxa", width = 3),
                          infoBoxOutput("progressBox_cumulativo", width = 3),
                        ),hr(),
                        
                        #Distribuicao de testagem
                        fluidRow(
                          tabBox(
                            title = "Resultados de Notificação diária",
                            # The id lets us use input$tabset1 on the server to find the current tab
                            id = "tabset1", height = "500px",
                            tabPanel("Testagem",paste0("Data de testagem: "),
                                     plotOutput("graf_casodiario")
                            ),
                          ),
                          tabBox(
                            side = "right", height = "500px",
                            # selected = "tab_tend",
                            tabPanel("Tendencia de casos",plotOutput("tendencia_casos")),
                          ),
                        ),
                        
                        #Distribuicao de por faixa etaria e sexo
                        fluidRow(
                          tabBox(
                            height = "300px",
                            tabPanel("Distribuicao de testados por Sexo",
                                     plotOutput("graf_sexo")
                                     
                            ),
                          ),
                          tabBox(
                            side = "right", height = "300px",
                            # selected = "tab_tend",
                            tabPanel("Distribuicao de Testados por sexo e resultado",
                                     plotOutput("graf_sexo_res")
                                     
                            ),
                          ),
                        ),
                        
                        hr(),tags$br(),tags$br(),tags$br(), tags$br(),
                        #telas da secao Reporter Diario
                        tabBox(
                          side = "left",
                          width = "auto",
                          height = "auto",
                          tabPanel("BASE DE DADOS DIARIA", tags$br(), downloadButton("download1", "BAIXAR EM EXCEL"),tags$br(),
                                   tags$br(), 
                                   reactable(BD_diaria, filterable = TRUE, minRows = 10),
                                  
                                   
                          ),
                          
                          tabPanel("BASE DE DADOS - COMULATIVO",
                                   reactable(BD_3_meses, filterable = TRUE, minRows = 10),
                                   
                          )
                          
                          
                        ),
                        
                        #fim da secao das telas do Reporter
               ),
               
               #tabelas --------------------------------------------
               tabPanel("Tabelas",
                        icon = icon("table"),
                        #telas da secao Reporter Diario
                        tabBox(
                          side = "left",
                          width = "auto",
                          height = "auto",
                          tabPanel("BD", 
                                   
                                   sidebarPanel(tags$br(),selectInput("dataD", "Data de Reporte", choices = unique(r_Base$Data_reporte)),width = 2,
                                                #selectInput("provincia", "Selecionar Provincia", choices = unique(r_Base$Provincia)),width = 2,
                                                downloadButton("downloadData", "Download"),
                                                
                                   ),
                                   mainPanel(
                                     
                                     # Output: Histogram ----
                                     reactableOutput("Toda_BD"), filterable = TRUE, minRows = 10, width = "auto"
                                     
                                     
                                   )
                                   
                          ),
                          
                          tabPanel("TABELA PIVO",width="auto",
                                   
                                   fluidRow(
                                     tabBox(
                                       tabPanel(
                                         rpivotTableOutput("pivotTable"),
                                       ),height = "auto"
                                     ),
                                     
                                     
                                   ),
                                   
                          ),
                          
                        ),
                        
                        #fim da secao das telas do Reporter
                        
                        
               ),
               
               #trends ------------------------------------------------------------
               
               #Panel provincias -------------------------------------------------
               
               #Monitoria -------------------------------------------------
               tabPanel(title = "Monitoria",
                        icon = icon("chart-line"),
                        width = "auto",
                        height = "auto",
                        fluidRow(
                          tabBox(
                            tags$h2("Testagem por provincia"),br(),
                            height = "auto",
                            tabPanel("Tendencia de testagem",
                                     plotOutput("testagem_provincia")
                            ), width = "auto",
                          ),
                          
                          tabBox(
                            title = "Tendencia de testagem",
                            # The id lets us use input$tabset1 on the server to find the current tab
                            id = "tabset4", height = "500px",
                           
                            tabPanel("Tendencia de testagem de positivos",tags$br(),
                                     
                                     plotOutput("testagem_provincia2")
                            ),
                          ),
                          tabBox(
                            side = "right", height = "500px",
                            
                            tabPanel("Taxa de positividade por provincia",tags$br(),
                                     
                                     highchartOutput("taxa_pos_prov"),
                            ),
                            
                          ),
                          
                          tabBox(
                            side = "right", height = "500px",
                            
                            tabPanel("Taxa de positividade por Faixa Etaria",tags$br(),
                                     
                                     highchartOutput("FAIXA_POSI"),
                            ),
                            
                          ),
                          
                          tabBox(
                            side = "right", height = "500px",
                            
                            tabPanel("Taxa de positividade por Sexo",tags$br(),
                                     
                                     highchartOutput("FAIXA_SEXO"),
                            ),
                            
                          ),
                          
                        ),
                        
                        
                        tabBox(
                          # selected = "tab_tend",
                          tabPanel("Base de dados de Positivos",
                                   reactable(BD_positivos, filterable = TRUE, width = "auto"),
                          ),width = "auto"
                        ),
                        
                        
               ),
               
               
               #export data -------------------------------------------------------
               navbarMenu("Sobre",
                          tabPanel("Exportar dados"),
                          "----",
                          "____________________",
                          tabPanel("Ajuda")
               )
               
    ),
    tags$footer(
      # hr(),
      "Algumas informações estam disponíveis no site do INS",align="center", tags$a(href="https://ins.gov.mz/", "INS Official site", target="_blank"),
      tags$br(),tags$br(),
      print("Instituto Nacional de Saúde - @ Todos Direitos reservados - por RGDTIC",align="center")
    )
    
  ), enable_admin = TRUE, language = "en")





# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ttp<-as.numeric(BD_diaria %>% dplyr::filter(resultado_testagem=="Positivo") %>% dplyr::summarise(resultado_testagem=n()))
  tt<-as.numeric(BD_diaria %>% dplyr::summarise(resultado_testagem=n()))
  
  #Authentication metodo
  result_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(result_auth)
  })
  
  observe({
    print(input$shinymanager_where)
    print(input$shinymanager_language)
  })
  #Fim metodo Authenticacao
  
  
  #BD_diaria <- read.csv('BD_DIARIA.csv', sep=";", header=T, stringsAsFactors=FALSE, encoding = "UTF-8")
  BD_3_meses <- BD_3_meses
  #_______________________
  
  
  
  #FUNCOES DE DOWNLOAD
  output$download1 <- downloadHandler(
    filename = function() {
      paste0("BD -",ultima_data, ".csv")
    },
    content = function(file) {
      write.csv(BD_diaria, file)
    }
  )
  
  #BASE COMULATIVO DOWNLOAD POR SECAO
  
  output$downloadData <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0(input$dataD, ".csv")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(r_Base, file)
    }
  )
  
  
  #PIVOTABLE
  output$pivotTable <- renderRpivotTable({
    rpivotTable(BD_3_meses,  rows = c( "Provincia"),cols="resultado_testagem",
                vals = "NACIONALIDADE", aggregatorName = "Count", rendererName = "Bar Chart",
                width="auto")
    
  })
  
  
  
  output$Toda_BD <- renderReactable({
    #reactable(BD_3_meses[input$Provincia])
    #filtroprov<-reactable(subset(r_Base, r_Base$Data_reporte == input$dataD))
    filtroprov<-reactable(subset(r_Base, r_Base$Data_reporte == input$dataD))
  })
  
  
  
  #RENDER PROGRESSBOX IN DASHBOARD
  #num_indicad <-as.numeric(BD_diaria %>% summarise(BD_diaria$resultado_testagem=n()))
  
  output$progressBox <- renderValueBox({
    valueBox(
      
      
      value = tags$p(paste0(num_testados <-as.numeric(BD_diaria %>% dplyr::summarise(resultado_testagem=n())), "  TESTADOS"), style = "font-size: 50%;"),
      subtitle = tags$p("ULTIMAS 24H", style = "font-size: 70%;"), icon = icon("list")
      
    )
  })
  
  output$progressBox1 <- renderInfoBox({
    valueBox(
      
      value = tags$p(paste0(num_posit <-as.numeric(BD_diaria %>%dplyr::filter(resultado_testagem=="Positivo") %>% dplyr::summarise(resultado_testagem=n()))," ", "  POSITIVOS"), style = "font-size: 50%;"),
      subtitle = tags$p("ULTIMAS 24H", style = "font-size: 70%;"), icon = icon("microscope")
      
    )
  })
  
  
  output$progressBox_taxa <- renderInfoBox({
    valueBox(
      
      value = tags$p(paste0(num_taxa <-round((ttp/tt)*100,digits =2), "  TAXA DE POSITIVIDADE"), style = "font-size: 50%;"),
      subtitle = tags$p("ULTIMAS 24H", style = "font-size: 70%;"), icon = icon("chart-line")
      
    )
  })
  
  output$progressBox_cumulativo <- renderInfoBox({
    valueBox(
      
      value = tags$p(paste0("", " ", "TESTADOS CUMULATIVOS"), style = "font-size: 40%;"),
      subtitle = tags$p("CUMULATIVOS", style = "font-size: 70%;"), icon = icon("window-restore"),
     
    )
  })
  
  output$progressBox_positivos <- renderInfoBox({
    
    valueBox(
      
      
      value = tags$p(paste0(10, " ", "POSITIVOS CUMULATIVOS"), style = "font-size: 40%;"),
      subtitle = tags$p("CUMULATIVOS", style = "font-size: 70%;"), icon = icon("user-plus")
     
    )
  })
  
  
  #Secao dos Graficos ilustrativos de casos
  
  
  #Grafico por resultado
  output$graf_casodiario<-renderPlot({
    ss<-BD_diaria %>% dplyr::count(Resultado=resultado_testagem) 
    #    ggplot(data=ss, aes(x=Resultado, y=ss$n)) +
    #      geom_bar(stat="identity", fill="steelblue")+
    #      geom_text(aes(label=ss$n), vjust=-0.3, size=3.5)+
    #      theme_minimal()
    
    ggplot(data=ss, aes(x=Resultado, y=ss$n, fill=Resultado)) +
      geom_bar(stat="identity", position=position_dodge())+
      geom_text(aes(label=ss$n), vjust=1.6, color="white",
                position = position_dodge(0.9), size=3.5)+
      scale_fill_brewer(palette="Paired")+
      theme_minimal()
  })
  
  
  # Tendencia de testagem
  output$tendencia_casos<-renderPlot({
    
    r_Base %>%
      ggplot() +
      aes(x = Data_reporte) +
      geom_histogram(bins = 30L, fill = "#87CEEB") +
      labs(x = "Data de reporte", y = "Frequencia") +
      theme_minimal()
    
  }) 
  
  #Distribuicao de testados por sexo
  dis_sex<-BD_diaria %>%  dplyr::count(sexo=sexo)
  output$graf_sexo<-renderPlot({
    mycols <- c("#f56f42", "#42f55a")#"#42f5e6", "#ecf542",
    
    ggplot(dis_sex, aes(x = "", y = n, fill = sexo)) +
      geom_bar(width = 1, stat = "identity", color = "black") +
      coord_polar("y", start = 0)+
      geom_text(aes(y = n, label = n), color = "black")+
      scale_fill_manual(values = mycols) +
      theme_void()
  })
  
  
  #distribuicao de testagem por resultado e sexo
  dis_sex_res<-BD_diaria %>% group_by(Resultado=resultado_testagem) %>% dplyr::count(sexo=sexo)
  output$graf_sexo_res<-renderPlot({
    mycols <- c("#42f55a","#f56f42", "#42f5e6")#", "#ecf542",
    
    ggplot(data=dis_sex_res, aes(x=sexo, y=n, fill=Resultado)) +
      geom_bar(stat="identity", position=position_dodge())+
      geom_text(aes(label=dis_sex_res$n), vjust=1.6, color="white",
                position = position_dodge(0.9), size=3.5)+
      scale_fill_manual(values = mycols) +
      theme_minimal()
  })
  
  #esquisser(r_Base)
  #Distribuicao de testagem por provincia
  
  output$testagem_provincia<-renderPlot({
    ttprov<-r_Base %>% group_by(Provincia,Data_reporte) %>% summarise(resultado_testagem=n())
    
    ttprov %>%
      dplyr::filter(Provincia %in% c("Maputo Cidade","Maputo Provincia","Gaza","Inhambane","Manica","Sofala","Tete","Zambezia","Nampula","Cabo Delgado","Niassa")) %>% 
      ggplot(aes(x=Data_reporte,y=resultado_testagem, colour=Provincia)) +
      geom_line()+
      geom_point()
       		          
    
  })
  
  #Resultados de testagem
  output$testagem_provincia2<-renderPlot({
    ttprov_pos<-r_Base %>% group_by(Provincia,Data_reporte) %>% dplyr::filter(resultado_testagem=="Positivo") %>% summarise(resultado_testagem=n())
    
    ttprov_pos %>%
      dplyr::filter(Provincia %in% c("Maputo Cidade","Maputo Provincia","Gaza","Inhambane","Manica","Sofala","Tete","Zambezia","Nampula","Cabo Delgado","Niassa")) %>% 
      ggplot(aes(x=Data_reporte,y=resultado_testagem, colour=Provincia)) +
      geom_line()+
      geom_point()
    
    
  })
  
  
  #taxa_positividade por provincia
  output$taxa_pos_prov<-renderHighchart({
    

     #taxatprov_pos<-r_Base %>% group_by(Data_reporte,Provincia) %>% summarise(Testados=(resultado_testagem=n()))
     
     b1<-BD_diaria$Provincia <- toupper(BD_diaria$Provincia)
     
     DADOS_NEW1 <- BD_diaria %>% dplyr::filter(resultado_testagem == "Positivo") %>% dplyr::group_by(Provincia) %>% dplyr::summarise(N=n())
     
     DADOS_NEW <- BD_diaria  %>% dplyr::group_by(Provincia) %>% dplyr::summarise(N=n())
     
     DADOS_FINAL <- left_join(DADOS_NEW, DADOS_NEW1, by = "Provincia")
     
     DADOS_FINAL[, 2:3][is.na(DADOS_FINAL[, 2:3])] <- 0
     
     DADOS_FINAL <- DADOS_FINAL %>%  mutate(`Taxa de Positividade` = (DADOS_FINAL$N.y/DADOS_FINAL$N.x)*100)
     DADOS_FINAL$`Taxa de Positividade` <- round(DADOS_FINAL$`Taxa de Positividade`, digits = 1)
     
     
    
     
     highchart() %>% 
       hc_chart(type = "column") %>%
       hc_xAxis(categories = DADOS_FINAL$Provincia) %>%
       hc_add_series(name="TESTADOS",data = DADOS_FINAL$N.x, color = "#1A5276") %>%
       hc_add_series(name="TAXA DE POSITIVIDADE (%)",data = DADOS_FINAL$`Taxa de Positividade`, type="line", color = "#CB4335") %>% 
       hc_plotOptions(
         series = list(
           boderWidth = 0,
           dataLabels = list(enabled = TRUE, style = list(fontSize = "10px"))
         )) %>% hc_tooltip(crosshairs = TRUE,
                           shared = TRUE, borderWidth = 5) %>% 
       hc_title(
         text = "TESTADOS POR Provincia <i></i> vs <b>Taxas de positividades</b>",
         margin = 20,
         align = "left",
         style = list(color = "#22A884", useHTML = TRUE)
       ) %>%
       hc_add_theme(hc_theme_google()) %>%
       hc_exporting(enabled = TRUE) 
    
    
  })
  
  #Grafico por faixa etaria
  
  output$FAIXA_POSI <- renderHighchart({
    
    #B_FAIX_ET<-r_Base <- res_mod1()
    
    BD_diaria$idade <- as.numeric(BD_diaria$idade)
    
    BD_diaria$idade <- cut(BD_diaria$idade, breaks =  c(0,4,9,14,19,24,29,34,39,100), labels=c("1_0 a 4", "2_5 a 9", "3_10 a 14", "4_15 a 19", "5_20 a 24", "6_25 a 29", "7_30 a 34", "8_30 a 34", "9_Mais de 40"))
    
    
    
    
    BD_diaria$idade <- as.character(BD_diaria$idade)
    
    BD_diaria <- replace_na(BD_diaria, list(idade ="Sem imformacao"))
    
    BD_diaria$idade <- as.factor(BD_diaria$idade)
    
    BD_diaria$idade<- plyr::mapvalues(BD_diaria$idade, from= c("1_0 a 4", "2_5 a 9", "3_10 a 14", "4_15 a 19", "5_20 a 24", "6_25 a 29", "7_30 a 34", "8_30 a 34", "9_Mais de 40", "Sem imformacao"), to=c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos", "15 a 19 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos", "30 a 34 anos", "Mais de 40 anos", "Sem imformacao"))
    
    
    
    
    
    DADOS_IDADE1 <- BD_diaria %>% dplyr::filter(resultado_testagem == "Positivo") %>% dplyr::group_by(idade) %>% dplyr::summarise(N=n())
    
    DADOS_IDADE <- BD_diaria  %>% dplyr::group_by(idade) %>% dplyr::summarise(N=n())
    
    DADOS_IDADE_FINAL <- dplyr::left_join(DADOS_IDADE, DADOS_IDADE1, by = "idade")
    
    DADOS_IDADE_FINAL[, 2:3][is.na(DADOS_IDADE_FINAL[, 2:3])] <- 0
    
    
    DADOS_IDADE_FINAL <- DADOS_IDADE_FINAL %>%  dplyr::mutate(`Taxa de Positividade` = (DADOS_IDADE_FINAL$N.y/DADOS_IDADE_FINAL$N.x)*100)
    DADOS_IDADE_FINAL$`Taxa de Positividade` <- round(DADOS_IDADE_FINAL$`Taxa de Positividade`, digits = 1)
    
    DADOS_IDADE_FINAL$IDADE <- DADOS_IDADE_FINAL$IDADE
    
    
    
    
    
    highchart() %>% 
      hc_chart(type = "column") %>%
      hc_xAxis(categories = DADOS_IDADE_FINAL$idade) %>%
      hc_add_series(name="TESTADOS",data = DADOS_IDADE_FINAL$N.x, color = "#1A5276") %>%
      hc_add_series(name="TAXA DE POSITIVIDADE (%)",data = DADOS_IDADE_FINAL$`Taxa de Positividade`, type="line", color = "#CB4335") %>% 
      hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE, style = list(fontSize = "16px"))
        )) %>% hc_tooltip(crosshairs = TRUE,
                          shared = TRUE, borderWidth = 5) %>% 
      hc_title(
        text = "TESTADOS POR FAIXA ETARIA <i></i> vs <b>Taxas de positividades</b>",
        margin = 20,
        align = "left",
        style = list(color = "#22A884", useHTML = TRUE)
      )  %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_exporting(enabled = TRUE)
    
    
    
  })
  
  
  #FAIXA_SEXO
  
  #positividade por faixa etaria
  
  output$FAIXA_SEXO <- renderHighchart({
    
    #BASE_FINAL_V1 <- res_mod1()
    
    BD_diaria$sexo <-  toupper(BD_diaria$sexo)
    
    BD_diaria <- replace_na(BD_diaria, list(sexo ="Sem imformacao"))
    
    DADOS_SEXO1 <- BD_diaria %>% dplyr::filter(resultado_testagem == "Positivo") %>% dplyr::group_by(sexo) %>% dplyr::summarise(N=n())
    
    DADOS_SEXO <- BD_diaria  %>% dplyr::group_by(sexo) %>% dplyr::summarise(N=n())
    
    DADOS_SEXO_FINAL <- left_join(DADOS_SEXO, DADOS_SEXO1, by = "sexo")
    
    DADOS_SEXO_FINAL[, 2:3][is.na(DADOS_SEXO_FINAL[, 2:3])] <- 0
    
    
    DADOS_SEXO_FINAL <- DADOS_SEXO_FINAL %>%  mutate(`Taxa de Positividade` = (DADOS_SEXO_FINAL$N.y/DADOS_SEXO_FINAL$N.x)*100)
    DADOS_SEXO_FINAL$`Taxa de Positividade` <- round(DADOS_SEXO_FINAL$`Taxa de Positividade`, digits = 1)
    
    DADOS_SEXO_FINAL$sexo <- DADOS_SEXO_FINAL$sexo
    
    
    
    
    
    highchart() %>% 
      hc_chart(type = "column") %>%
      hc_xAxis(categories = DADOS_SEXO_FINAL$sexo) %>%
      hc_add_series(name="TESTADOS",data = DADOS_SEXO_FINAL$N.x, color = "#1A5276") %>%
      hc_add_series(name="TAXA DE POSITIVIDADE (%)",data = DADOS_SEXO_FINAL$`Taxa de Positividade`, type="line", color = "#CB4335") %>% 
      hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE, style = list(fontSize = "16px"))
        )) %>% hc_tooltip(crosshairs = TRUE,
                          shared = TRUE, borderWidth = 5) %>% 
      hc_title(
        text = "TESTADOS POR SEXO <i></i> vs <b>Taxas de positividades</b>",
        margin = 20,
        align = "left",
        style = list(color = "#22A884", useHTML = TRUE)
      ) %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_exporting(enabled = TRUE)
    
    
    
  })
  
  
  

  
}

# Run the application 
shinyApp(ui, server)

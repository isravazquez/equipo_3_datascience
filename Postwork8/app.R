#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#install.packages("shinydashboard")
library(shinydashboard)
#install.packages("shinythemes")
library(shinythemes)
library(dplyr)

#setwd("/home/hugomi/Documents/BEDU/Modulo2/Postworkfinal/Data-app")

#data_soccer <- read.csv("match.data.csv")
data_soccer <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2021/main/Sesion-07/Postwork/match.data.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    dashboardPage(
        dashboardHeader(title = "Postwork 8: Equipo 3"),
        dashboardSidebar(
            
            sidebarMenu(
                menuItem("Equipo", tabName = "team", icon = icon("users")),
                menuItem("Gráficas de barras", tabName = "bar_graph", icon = icon("bar-chart")),
                menuItem("Probabilidades", tabName = "prob_graph", icon = icon("signal")),
                menuItem("Data Table", tabName = "data_table", icon = icon("table")),
                menuItem("Predicciones", tabName = "img", icon = icon("file-picture-o"))
            )
        ),
            
            dashboardBody(
                
                tabItems(
                    
                    # Equipo
                    tabItem(tabName = "team", 
                            fluidRow(
                                titlePanel(h2("Equipo 3: Data Science", align = "center")),
                                htmlOutput("text0"),
                                br(),
                                textOutput("text01"),
                                br(),
                                img(src = "qr_img.png", height = 246, width = 246, style="display: block; margin-left: auto; margin-right: auto;")
                            )
                    ),
                    
                    # Gráficas de barras
                    tabItem(tabName = "bar_graph",
                            fluidRow(
                                titlePanel(h2("Gráficas de barras", align = "center")),
                                textOutput("text1"),
                                br(),
                                textOutput("text12"),
                                br(),
                                selectInput("x", "Seleccione el valor de x: cantidad de goles anotados como local o visitante",
                                            choices = names(data_soccer[, c(3,5)])),
                                
                                plotlyOutput("plot1", height = "1000", width = "1500"),
                                br(),
                                textOutput("text2")
                            )
                    ),
                    
                    # Probabilidades marginales y conjunta
                    tabItem(tabName = "prob_graph", 
                            fluidRow(
                                titlePanel(h2("Probabilidades marginales y conjuntas", align = "center")),
                                textOutput("text30"),
                                br(),
                                textOutput("text3"),
                                br(),
                                img(src = "p3-1.png", height = 400, width = 550, style="display: block; margin-left: auto; margin-right: auto;"),
                                br(),
                                textOutput("text4"),
                                br(),
                                img(src = "p3-2.png", height = 400, width = 550, style="display: block; margin-left: auto; margin-right: auto;"),
                                br(),
                                textOutput("text5"),
                                br(),
                                img(src = "p3-3.png", height = 400, width = 550, style="display: block; margin-left: auto; margin-right: auto;"),
                                br(),
                                textOutput("text6"),
                                br(),
                                img(src = "p3-4.png", height = 400, width = 550, style="display: block; margin-left: auto; margin-right: auto;")
                            )
                    ),
                    
                    
                    # Tabla de datos de la base match.data.csv
                    tabItem(tabName = "data_table",
                            fluidRow(        
                                titlePanel(h2("Data Table", align = "center")),
                                textOutput("text7"),
                                br(),
                                dataTableOutput ("data_table")
                            )
                    ), 
                    
                    # Gráficos resultantes del código momios.R
                    tabItem(tabName = "img",
                            fluidRow(
                                titlePanel(h2("Gráficas de los factores de ganancia máximo y promedio", align = "center")),
                                textOutput("text80"),
                                br(),
                                textOutput("text8"),
                                br(),
                                img(src = "pic1.png", height = 400, width = 550, style="display: block; margin-left: auto; margin-right: auto;"),
                                br(),
                                textOutput("text9"),
                                br(),
                                img(src = "pic2.png", height = 400, width = 550, style="display: block; margin-left: auto; margin-right: auto;")
                            )
                    )
                    
                )
            )
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
    library(ggplot2)
    library(plotly)
    
    # Team
    output$text0 <- renderUI({
        integrantes <- c("Integrantes:",
        "Arriaga Palma Fernando, arriaga141@gmail.com",
        "Hernández Angulo Juan de Jesus, jhernandezangulo@gmail.com",
        "Martínez Ibarra Hugo, hugomtzib@gmail.com",
        "Moreno Abrego Bryan Daniel, abre.go@outlook.com",
        "Silva Tijerina Gilberto, gilberto.silvat2812@gmail.com",
        "Vazquez Bernal Jaime Israel, israfullshot@gmail.com")
        
        HTML(paste(integrantes, collapse = '<br>'))
    })
    
    output$text01 <- renderText({
        paste("Para más información sobre postworks pasados y código de este trabajo, consultar el repositorio en el siguiente enlace (código QR)",sep="\n")
    })
    
    # Bar_graph
    attach(data_soccer)
    
    output$text1 <- renderText({
        paste("Las siguientes gráficas muestran en el eje x los goles que se han metido como equipo local/vistante y en el eje y el número de veces (partidos) que el equipo local/visitante anotó esa cantidad de goles. \n
              Mientras que cada gráfica pequeña representa el conteo de goles cuando ese equipo fue visitante/local. Además, por cada barra de cada gráfico se hace la segmentación correspondiente de la proporción de goles anotados por cada equipo local.",sep="\n")
    })
    
    output$text12 <- renderText({
        paste("Los datos corresponden a partidos de agosto de 2010 a julio de 2020. Recordar que las temporadas de la liga española, primera división, son de mayo a agosto. Además hay cierta irregularidad en la disponibilidad de datos, debido a la suspensión de partidos por la pandemia de Covid-19.")
    })
    
    output$plot1 <- renderPlotly({
         data_fill <- "away.team"
         data_wrap <- "home.team"
         x_name <- "Goles visitante"
         legend_name <- "Equipos visitante"
         
         if (input$x == "home.score") {
             data_fill <- "home.team"
             data_wrap <- "away.team"
             x_name <- "Goles local"
             legend_name <- "Equipos local"}
         g <- ggplot(data_soccer, aes(x = eval(as.name(input$x)), fill = eval(as.name(data_fill)))) + 
            geom_bar() +
            facet_wrap(data_wrap) +
            labs(x = x_name, y = "Número de veces la cantidad de goles x", fill = legend_name)
         
         ggplotly(g)
    })
    detach(data_soccer)
    
    output$text2 <- renderText({
        paste("Se puede apreciar que las barras más altas representan una mayor cantidad de partidos donde el equipo local/visitante anotó el correspondiente número de goles al respectivo equipo visitante/loca.")
    })
    
    # Probabilidades marginales y conjunta
    output$text30 <- renderText({
        paste("El siguiente análsis corresponde a datos de partidos de agosto de 2017 a julio de 2020. Recordar que las temporadas de la liga española, primera división, son de mayo a agosto.")
    })
    
    output$text3 <- renderText({
        paste("La siguiente gráfica muestra la probabilidad marginal de x: número de goles como equipo local. Es la probobabilidad de que el equipo local anote x número de goles.")
    })
    
    output$text4 <- renderText({
        paste("La siguiente gráfica muestra la probabilidad marginal de y: número de goles como equipo visitante. Es la probobabilidad de que el equipo visitante anote y número de goles.")
    })
    
    output$text5 <- renderText({
        paste("Mientras que la siguiente gráfica muestra la probabilidad conjunta de x e y; es decir la probabilidad de que el equipo local anote x goles y el equipo visitante anote y goles.")
    })
    
    output$text6 <- renderText({
        paste("La siguiente gráfica representa un análisis de probabilidades por cada gol como local al fijar el número de goles de visitante.")
    })
    
    # Data table
    output$text7 <- renderText({
        paste("En la siguiente tabla se pueden visualizar todos los datos de fecha de partidos (date), equipo local (home.team), puntuación de equipo local (home.score), equipo local (home.team) y puntuación de equipo visitante (away.score).",
              "Son datos de partidos de agosto de 2010 a julio de 2019. Recordar que las temporadas de la liga española, primera división, son de mayo a agosto. Además hay cierta irregularidad en la disponibilidad de datos, debido a la suspensión de partidos por la pandemia de Covid-19.", sep = "\n")
    })
    
    output$data_table <- renderDataTable( {data_soccer}, options = list(aLengthMenu = c(5,25,50), iDisplayLength = 5))
    
    # Predicciones
    output$text80 <- renderText({
        paste("Se mostraŕan unos escenarios de predicción basados en datos de partidos de agosto de 2010 a julio de 2020. Recordar que las temporadas de la liga española, primera división, son de mayo a agosto.",
              "Además hay cierta irregularidad en la disponibilidad de datos, debido a la suspensión de partidos por la pandemia de Covid-19.", sep = "\n")
    })
    
    output$text8 <- renderText({
        paste("A continuación se muestra la gráfica del escenario con el factor de ganacia máximo para 600 partidos jugados y un capital inicial de $50,000.")
    })
    
    output$text9 <- renderText({
        paste("Enseguida se muestra la gráfica del escenario con el factor de ganacia promedio para 600 partidos jugados y un capital inicial de $50,000.")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

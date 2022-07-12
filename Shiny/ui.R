
library(shinydashboard)
library(shiny)
library(shinyWidgets)

# Encabezado

dbHeader <- dashboardHeader(title = "Covid-19",
                            tags$li(a(href = 'https://www.colmex.mx/',
                                      img(src = 'colmexpng.png',
                                          title = "El Colegio de México", height = "30px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"),
                            tags$li(a(href = 'https://cee.colmex.mx/',
                                      img(src = 'cee.png',
                                          title = "Centro de Estudios Económicos", height = "30px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"))




dbSidebar <- dashboardSidebar(
  sidebarMenu(
    h4("Secciones", align = "center", style="color:white"),
    menuItem("Inicio", tabName = "inicio", icon = icon("home")),
    menuItem("Glosario", tabName = "glosario", icon = icon("book")),
    menuItem("Covid y el mundo", tabName = "mundial", icon = icon("globe"), startExpanded =  F,
             menuSubItem("Panorama General", tabName = "panoramam"),
             menuSubItem("Regresión cuantil", tabName = "regcuanm"),
             menuSubItem("Clusterización", tabName = "clusterm")
    ),
    menuItem("Covid y Latinoamérica", tabName = "latinoamerica", icon = icon("globe-americas"), startExpanded =  F,
             menuSubItem("Panorama General", tabName = "panoramala"),
             menuSubItem("Regresión cuantil", tabName = "regcuanla"),
             menuSubItem("Clusterización", tabName = "clusterla")
    ),
    menuItem("Covid y México", tabName = "mexico", icon = icon("pepper-hot"), startExpanded =  F,
             menuSubItem("Panorama General", tabName = "panoramamx"),
             menuSubItem("Control sintético", tabName = "consin"),
             menuSubItem("DFL", tabName = "dfl"),
             menuSubItem("Regresión cuantil", tabName = "regcuanmx")
    ),
    menuItem("Covid en los estados", tabName = "impactoef", icon = icon("chart-area"), startExpanded =  F,
             menuSubItem("Panorama general", tabName = "panoramaef"),
             menuSubItem("Modelo logit/probit", tabName = "logit"),
             menuSubItem("Regresión cuantil", tabName = "regcuanef")
    ),
    menuItem("Covid en los municipios", tabName = "municipios", icon = icon("map-pin")),
    menuItem("Sobre nosotrxs", tabName = "about", icon = icon("users"))
  )
)

shinyUI(
  dashboardPage(skin = "yellow",
    dbHeader,
    dbSidebar,
    dashboardBody(
      tabItems(
        tabItem(tabName = "inicio",
                h1("Bienvenidxs", 
                   align = "center", 
                   style="color:black"),
                tags$hr(style="border-color: #F39C12;"),
                tags$head(
                  tags$style(HTML("hr {border-top: 3px solid #000000;}"))
                ),
                h3("A dos años de la confirmación del primer caso de Covid-19 de un hombre de 55 años en Wuhan, China, la enfermedad ha causado la muerte de casi 5 millones de personas y ha infectado a más de 240 millones. Las medidas de prevención y confinamiento tomadas por diferentes gobiernos en el mundo han alterado las interacciones sociales, golpeado el crecimiento económico de las naciones y dañado irreversiblemente los ingresos de muchas personas. ",
                   align = "center",
                   style = "color:#7B7B7B"),
                br(),
                div(style="text-align: center;",img(src="imagenintro.png", height=350, width=500)),
                h3("Sin embargo, el Covid-19 no ha golpeado de igual manera a los países, regiones y personas. En muchas partes del mundo, la gente no tiene el mismo riesgo, ni miedo, de enfermar o morir por Covid-19. Existen ciudades donde la adquisición de una vacuna no representa una preocupación importante. En otras locaciones, el nivel de prevalencia de otras afecciones no provoca que los individuos busquen cambiar sus estilos de vida por temor al Covid-19.", align = "center"),
                linebreaks(1),
                fluidRow(
                  column(12, align = "center", offset = 3,
                box(title = "Sobre este sitio", status = "warning", width = 6, #status: primary, success, info, warning, danger
                  solidHeader = TRUE, collapsible = F, style = "font-size:20px;",
                  p("A través de este proyecto de visualización de datos, se busca cumplir el objetivo de ilustrar, de manera interactiva e intuitiva, la relación entre las tendencias de las variables más relevantes de la pandemia de Covid-19 y la relación que guardan con variables económicas.", align = "center"))
                        )
                        ),
                linebreaks(1),
                h4(style="text-align: justify;", "Así, el sitio se divide de la siguiente manera:"),
                h4(style="text-align: justify;", "La sección de Glosario muestra algunos términos relevantes para la correcta interpretación de la información presentada en este sitio, tanto en términos económicos, como epidemiológicos."),
                h4(style="text-align: justify;", "Después, se presenta el panorama de la situación de la pandemia de Covid-19 a nivel mundial, ilustrando la evolución de las variables más relevantes como el número de casos, muertes, vacunas aplicadas, etc. Además, esta primera sección se presentan análisis econométricos cuantiles y de clusterización para evaluar la relación que guardan algunas de estas variables con indicadores económicos relevantes."),
                h4(style="text-align: justify;", "En tercer lugar, en la sección Covid y Latinoamérica, se presenta la misma información que en la primera sección, pero acotándola al caso de los países latinoamericanos. En el caso de los análisis econométricos, también se realizan los mismos que en la sección Mundial."),
                h4(style="text-align: justify;", "En la cuarta sección, el análisis se estrecha aún más para revisar el caso mexicano. Además de presentar visualizaciones de las variables más relevantes y su evolución en nuestro país, se realiza un procedimiento de control sintético para evaluar si la cuarentena anunciada por el Gobierno Federal tuvo un efecto sobre algunos indicadores de la pandemia; además, se hace uso de una descomposición DFL con el objetivo de comparar las distribuciones de casos y muertes entre México y Nueva Zelanda; finalmente, se replica una regresión cuantil como en las secciones anteriores."),
                h4(style="text-align: justify;", "Seguidamente, se ilustra la situación al interior de los estados de la República de manera similar a las secciones de arriba, con la diferencia de que se incluye un modelo probit que tiene el objetivo de determinar la probabilidad de morir por causa de Covid-19 dadas ciertas condiciones de salud y comorbilidades."),
                h4(style="text-align: justify;", "Finalmente, en la sección de Municipios se exhiben algunos indicadores generales sobre la evolución de la enfermedad al nivel municipal mexicano.")
                ),
        
        #######################################
        
        tabItem(tabName = "glosario",
                  h2("Glosario de términos"),
                  tabsetPanel(
                    tabPanel("Términos económicos"),
                    tabPanel("Términos epidemiológicos",
                             fluidRow(
                               box(
                                 title = "Glosario", status = "info", width = 12, #status: primary, success, info, warning, danger
                                 solidHeader = TRUE,collapsible = F,
                                 helpText("Seleccione un concepto"),
                                 selectInput("select_conceptos_mundial", label = "",
                                             choices = glosario_total$concepto, selected = 1)
                                 
                               )
                             ),
                             fluidRow(
                               box(
                                 title = "Definición", background  = "navy", width = 12,
                                 solidHeader = TRUE,collapsible = F,
                                 textOutput("descriptivas_mundial")
                                 
                               )
                             )
                             ),
                    tabPanel("Otros")
                              )
                ),
        
        ###############TAB INTERNACIONAL########################
        
        tabItem(tabName = "panoramam",
                h2("Información general y descriptiva sobre la situación del Covid-19 a nivel mundial", align = "center"),
                tabsetPanel(
                  tabPanel("Tendencias generales",
                tags$hr(style="border-color: #000000;"),
                h3("Tendencias de las variables más importantes a nivel mundial.", align = "center"),
                tags$hr(style="border-color: #000000;"),
                br(),
                h4(style = "text-align:justify;", "En esta sección se encuentra información sobre la evolución de los casos, muertes y personas vacunadas a nivel mundial desde enero de 2020, mes en el que el virus del Covid-19 comenzó a propagarse sustancialmente alrededor del planeta. Específicamente, se puede encontrar el número de casos totales y nuevos, por millón de habitantes y con un promedio móvil de 7 días. Además, el número de personas vacunads según su esquema."),
                # Casos #
                linebreaks(3),
                fluidRow(
                  box(
                    title = "Variables de casos", 
                    status = "primary",
                    helpText("Seleccione una variable para mostrar. Fuente: Our World in Data."),
                    collapsible = T,
                    selectInput("var_inter_casos",
                                label = "Variable",
                                choices = unique(owid_m_casos$Variable1),
                                selected = 1)
                  ),
                
                # Muertes #
                  box(
                    title = "Variables de muertes", 
                    status = "primary",
                    helpText("Seleccione una variable para mostrar. Fuente: Our World in Data."),
                    collapsible = T,
                    selectInput("var_inter_muertes",
                                label = "Variable",
                                choices = unique(owid_m_muertes$Variable2),
                                selected = 1)
                  )
                        ),
                br(),
                fluidRow(column(6,plotlyOutput('plot_inter_casos')),
                         column(6,plotlyOutput('plot_inter_muertes'))
                ),
                # Vacunas #
                linebreaks(3),
                fluidRow(
                  box(
                    title = "Personas vacunadas a nivel mundial",
                    status = "primary",
                    helpText("Fuente: Our World in Data."),
                    width = 12,
                    collapsible = T,
                    plotlyOutput("plot_inter_vacunas")
                  )
                ),
                # Continentes #
                linebreaks(2),
                tags$hr(style="border-color: #000000;"),
                h3("Tendencias de las variables más importantes por continente.", align = "center"),
                tags$hr(style="border-color: #000000;"),
                br(),
                h4(style = "text-align:justify;", "A continuación se ilustran casos, muertes y personas vacunadas en diferentes métricas por continente."),
                linebreaks(2),
                fluidRow(
                  box(
                    title = "Variables", 
                    status = "primary",
                    helpText("Seleccione una variable para mostrar. Fuente: Our World in Data."),
                    collapsible = T,
                    width = 12,
                    selectInput("var_inter_continentes",
                                label = "Variable",
                                choices = unique(owid_cont$Variable3),
                                selected = 1)
                  ),
                  box(
                    title = "Datos por continente",
                    status = "primary",
                    width = 12,
                    collapsible = T,
                    plotlyOutput("plot_inter_continentes")
                  )
                ),
                # Casos #
                linebreaks(2),
                tags$hr(style="border-color: #000000;"),
                h3("Casos nuevos por continente.", align = "center"),
                tags$hr(style="border-color: #000000;"),
                br(),
                h4(style = "text-align:justify;", "Se muestran los casos nuevos por contienente desde el inicio de la pandemia."),
                linebreaks(2),
                fluidRow(
                  box(
                    title = "Continentes", 
                    status = "primary",
                    helpText("Seleccione un continente para mostrar. Fuente: Our World in Data."),
                    collapsible = T,
                    width = 12,
                    selectInput("var_cont_casos",
                                label = "Continente",
                                choices = unique(owid_barras_casos$Continente),
                                selected = 1)
                  ),
                  box(
                    title = "Casos nuevos por continente",
                    status = "primary",
                    width = 12,
                    collapsible = T,
                    plotlyOutput("plot_casos_continentes")
                  )
                ),
                # Muertes #
                linebreaks(2),
                tags$hr(style="border-color: #000000;"),
                h3("Muertes nuevas por continente.", align = "center"),
                tags$hr(style="border-color: #000000;"),
                br(),
                h4(style = "text-align:justify;", "Se muestran los muertes nuevas por contienente desde el inicio de la pandemia."),
                linebreaks(2),
                fluidRow(
                  box(
                    title = "Continentes", 
                    status = "primary",
                    helpText("Seleccione un continente para mostrar. Fuente: Our World in Data."),
                    collapsible = T,
                    width = 12,
                    selectInput("var_cont_muertes",
                                label = "Continente",
                                choices = unique(owid_barras_muertes$Continente),
                                selected = 1)
                  ),
                  box(
                    title = "Muertes nuevas por continente",
                    status = "primary",
                    width = 12,
                    collapsible = T,
                    plotlyOutput("plot_muertes_continentes")
                  )
                ),
                  ),
                # Mapas #
              tabPanel("Mapas casos",
                 tags$hr(style="border-color: #000000;"),
                 h3("Casos totales por millón de habitantes.", align = "center"),
                 tags$hr(style="border-color: #000000;"),
                 br(),
                 h4(style = "text-align:justify;", "Los siguientes mapas exhiben los casos totales de Covid-19 por millón de habitantes en el mundo."),
                 linebreaks(2),
                 tabsetPanel(
                   tabPanel("Mundial",
                            div(plotlyOutput("mapamundial1", height = "100%"), align = "center")
                            ),
                   tabPanel("Norteamérica",
                            div(plotlyOutput("mapaNA1", height = "100%"), align = "center")
                            ),
                   tabPanel("Sudamérica",
                            div(plotlyOutput("mapaSA1", height = "100%"), align = "center")
                            ),
                   tabPanel("Europa",
                            div(plotlyOutput("mapaEUR1", height = "100%"), align = "center")
                            ),
                   tabPanel("Asia",
                            div(plotlyOutput("mapaASIA1", height = "100%"), align = "center")
                            ),
                   tabPanel("África",
                            div(plotlyOutput("mapaAFR1", height = "100%"), align = "center")
                            )
                            )
                        ),
              tabPanel("Mapas muertes",
                       tags$hr(style="border-color: #000000;"),
                       h3("Muertes totales por millón de habitantes.", align = "center"),
                       tags$hr(style="border-color: #000000;"),
                       br(),
                       h4(style = "text-align:justify;", "Los siguientes mapas exhiben las muertes totales de Covid-19 por millón de habitantes en el mundo."),
                       linebreaks(2),
                       tabsetPanel(
                         tabPanel("Mundial",
                                  div(plotlyOutput("mapamundial2", height = "100%"), align = "center")
                         ),
                         tabPanel("Norteamérica",
                                  div(plotlyOutput("mapaNA2", height = "100%"), align = "center")
                         ),
                         tabPanel("Sudamérica",
                                  div(plotlyOutput("mapaSA2", height = "100%"), align = "center")
                         ),
                         tabPanel("Europa",
                                  div(plotlyOutput("mapaEUR2", height = "100%"), align = "center")
                         ),
                         tabPanel("Asia",
                                  div(plotlyOutput("mapaASIA2", height = "100%"), align = "center")
                         ),
                         tabPanel("África",
                                  div(plotlyOutput("mapaAFR2", height = "100%"), align = "center")
                         )
                       )
                      )
              
                )
        ),
        #######################################
        
        tabItem(tabName = "regcuanm",
                h2("Regresión cuantil de casos/muertes/vacunas y cuantiles de ingreso a nivel mundial", align = "center"),
                br(),
                div(style="text-align: center;",img(src="imagen.jpg", height=300, width=550)),
                br(),
                br(),
                sidebarLayout(
                  sidebarPanel(h4("Datos relevantes", align = "center"),
                               tableOutput("tabla1")
                  ),
                  mainPanel(h4("¿El Covid-19 ha afectado a todos por igual?", align = "center"),
                            hr(),
                            linebreaks(3),
                            plotOutput("plot1", brush = "plot_brush"),
                            tableOutput("data_brush"),
                            linebreaks(3),
                            plotlyOutput("plot1_ly")
                  )
                ),
                linebreaks(3),
                DTOutput("data_dt"),
                linebreaks(3),
                plotlyOutput("plot3"),
                linebreaks(3),
                plotlyOutput("plot4"),
                linebreaks(3),
                plotlyOutput("plot5"),
                linebreaks(3),
                plotlyOutput("plot6")
                
                ),
        tabItem(tabName = "clusterm",
                h2("Regresión clusterizada por continentes de casos/muertes/vacunas y cuantiles de ingreso a nivel mundial")        
                ),
        tabItem(tabName = "panoramala",
                h2("Información general y descriptiva sobre la situación del Covid-19 a nivel Latinoamérica", align = "center"),
                tabsetPanel(
                  tabPanel("Tendencias generales",
                           # Países LA #
                           linebreaks(2),
                           h4(style = "text-align:justify;", "A continuación se ilustran casos, muertes y personas vacunadas en diferentes métricas para países seleccionados de Latinoamérica"),
                           linebreaks(2),
                           fluidRow(
                             box(
                               title = "Variables", 
                               status = "primary",
                               helpText("Seleccione una variable para mostrar. Fuente: Our World in Data."),
                               collapsible = T,
                               width = 12,
                               selectInput("var_la_paises",
                                           label = "Variable",
                                           choices = unique(owidLA$Variable4),
                                           selected = 1)
                             ),
                             box(
                               title = "Datos por país",
                               status = "primary",
                               width = 12,
                               collapsible = T,
                               plotlyOutput("plot_la_paises")
                             )
                           ),
                           linebreaks(2),
                           tags$hr(style="border-color: #000000;"),
                           h3("Casos nuevos en países seleccionados", align = "center"),
                           tags$hr(style="border-color: #000000;"),
                           br(),
                           h4(style = "text-align:justify;", "Se muestran los casos nuevos en países selecciondados de Latinoamérica desde el inicio de la pandemia."),
                           linebreaks(2),
                           fluidRow(
                             box(
                               title = "Países", 
                               status = "primary",
                               helpText("Seleccione un país para mostrar. Fuente: Our World in Data."),
                               collapsible = T,
                               width = 12,
                               selectInput("var_la_paises_casos",
                                           label = "País",
                                           choices = unique(owid_barras_casos_la$Pais),
                                           selected = 1)
                             ),
                             box(
                               title = "Casos nuevos por país",
                               status = "primary",
                               width = 12,
                               collapsible = T,
                               plotlyOutput("plot_la_paises_casos")
                             )
                           ),
                           # Muertes #
                           linebreaks(2),
                           tags$hr(style="border-color: #000000;"),
                           h3("Muertes nuevas por país", align = "center"),
                           tags$hr(style="border-color: #000000;"),
                           br(),
                           h4(style = "text-align:justify;", "Se muestran las muertes nuevas en países seleccionados de Latinoamérica desde el inicio de la pandemia."),
                           linebreaks(2),
                           fluidRow(
                             box(
                               title = "Países", 
                               status = "primary",
                               helpText("Seleccione un país para mostrar. Fuente: Our World in Data."),
                               collapsible = T,
                               width = 12,
                               selectInput("var_la_paises_muertes",
                                           label = "País",
                                           choices = unique(owid_barras_muertes_la$Pais),
                                           selected = 1)
                             ),
                             box(
                               title = "Muertes nuevas por país",
                               status = "primary",
                               width = 12,
                               collapsible = T,
                               plotlyOutput("plot_la_paises_muertes")
                             )
                           ),
                           # Vacunas #
                           linebreaks(2),
                           tags$hr(style="border-color: #000000;"),
                           h3("Vacunas nuevas por país", align = "center"),
                           tags$hr(style="border-color: #000000;"),
                           br(),
                           h4(style = "text-align:justify;", "Se muestran las vacunas aplicadas por día en países seleccionados de Latinoamérica desde el inicio de la pandemia."),
                           linebreaks(2),
                           fluidRow(
                             box(
                               title = "Países", 
                               status = "primary",
                               helpText("Seleccione un país para mostrar. Fuente: Our World in Data."),
                               collapsible = T,
                               width = 12,
                               selectInput("var_la_paises_vacunas",
                                           label = "País",
                                           choices = unique(owid_barras_vacunas_la$Pais),
                                           selected = 1)
                             ),
                             box(
                               title = "Vacunas nuevas por país",
                               status = "primary",
                               width = 12,
                               collapsible = T,
                               plotlyOutput("plot_la_paises_vacunas")
                             )
                           )
                          ),
                  # Mapas #
                  tabPanel("Mapas casos"
                           ),
                  tabPanel("Mapas muertes"
                  )
        )
                  
                ),
        tabItem(tabName = "regcuanla",
                h2("Regresión cuantil de casos/muertes/vacunas y cuantiles de ingreso en Latinoamérica")        
                ),
        tabItem(tabName = "clusterla",
                h2("Regresión clusterizada por X de casos/muertes/vacunas y cuantiles de ingreso en Latinoamérica")        
                ),
        tabItem(tabName = "panoramamx",
                h2("Información general y descriptiva sobre la situación del Covid-19 en México")        
                ),
        tabItem(tabName = "consin",
                h2("Control sintético sobre el efecto de la cuarentena en México")        
                ),
        tabItem(tabName = "dfl",
                h2("Descomposición DFL para comparar distribuciones de México y Nueva Zelanda")        
                ),
        tabItem(tabName = "regcuanmx",
                h2("Regresión cuantil de casos/muertes/vacunas y cuantiles de ingreso en México")        
                ),
        tabItem(tabName = "logit",
                h2("Modelo logit/probit para calcular la probabilidad de morir dadas ciertas condiciones")        
                ),
        tabItem(tabName = "panoramaef",
                h2("Información general y descriptiva sobre la situación del Covid-19 en México a nivel estatal")        
                ),
        tabItem(tabName = "regcuanef",
                h2("Regresión cuantil de casos/muertes/vacunas y cuantiles de ingreso a nivel estatal")        
                ),
        tabItem(tabName = "municipios",
                h2("Información general y descriptiva sobre la situación del Covid-19 en México a nivel municipal")        
                ),
        tabItem(tabName = "about",
                h1("Proyecto final de visualización de datos", align = "center"),
                h3("Econometría Aplicada, Curso 2021",br(),"Profesor: Dr. Raymundo Miguel Campos Vázquez", align = "center"),
                br(),
                fluidRow(
                  box(h4(strong("Flor Yurivia Valdes de la Torre"),align="center"),width = 4,
                      br(),
                      HTML('<center><img src="imagenintro.png" width="100"></center>'),
                      br(),
                      a("fvaldes@colmex.mx", href = "mailto:fvaldes@colmex.mx"), align = "center"
                      ),
                  box(h4(strong("Germán Augusto Campos Ortíz "),align="center"),width = 4,
                      br(),
                      HTML('<center><img src="imagenintro.png" width="100"></center>'),
                      br(),
                      a("gacampos@colmex.mx", href = "mailto:gacampos@colmex.mx"),align="center"
                      ),
                  box(h4(strong("Marco Méndez Atienza"),align="center"),width = 4,
                      br(),
                      HTML('<center><img src="imagenintro.png" width="100"></center>'),
                      br(),
                      a("mamendez@colmex.mx", href = "mailto:mamendez@colmex.mx"), align = "center"
                      )
                  
                ),
                br(),
                HTML('<center><img src="colmexpng.png" width="100"></center>'),
                br(),
                h5("El presente proyecto de visualización de datos se realizó haciendo uso de Shiny mediante el software R. Asimismo, los datos presentados a lo largo de las secciones provienen de la página Our World in Data y la Universidad de Oxford, así como de la Secretaría de Salud del Gobierno Federal. En caso de desear conocer el código de la aplicación y sus detalles, por favor refiérase al siguiente",
              tags$a(target="_blank",href="https://github.com/marcomna/Proyecto-Shiny-Covid","repositorio de GitHub"),"Agradeceremos cualquier comentario que se quiera realizar en nuestros correos institucionales.",br(), 
              strong("¡Esperamos que les haya sido útil!"),align = "center"),
        ) 
      )
    )
  )
)

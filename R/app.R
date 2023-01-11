library(shiny)
library(leaflet)
library(shinydashboard)
library(anytime)
library(DT)
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

#greenLeafIcon <- makeIcon(fa_png(name = "map-pin"),18,18)
greenLeafIcon <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-green.png",
  iconWidth = 12.5, iconHeight = 20.5,
  iconAnchorX = 6, iconAnchorY = 20.5,
  shadowUrl = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/0.7.7/images/marker-shadow.png",
  shadowWidth = 0.5, shadowHeight = -17,
  shadowAnchorX = 20.5, shadowAnchorY = 20.5
)
ui <- dashboardPage(
  dashboardHeader(title = "Bancos Comunitários"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Seleção de Filtros", tabName = "my_selection"),
      menuItem("Mapa", tabName = "my_maps"),
      menuItem("Manual do Usuário", tabName = "my_manual")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "my_maps",
              tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
              leafletOutput("map")
      ),
      tabItem(tabName = "my_manual",
              titlePanel("Seções"),
              p("Ao acessar o site, o usuário irá se deparar com a seção de Seleção de Filtros, indicada pelo contorno no canto superior esquerdo como demonstrado na figura abaixo."),
              p("O usuário poderá a qualquer momento trocar de seção para visualizar o mapa clicando no botão “Mapa” para carregar a visualização do mapa. Vale ressaltar que o site pode levar alguns segundos para carregar os dados inicialmente e, portanto, o mapa ou a tabela podem inicialmente aparecerem vazios. Para retornar à seleção de filtros, basta clicar no botão Seleção de Filtros."),
              p("O botão que apresenta as três linhas serve para sumir ou retornar com os botões de seção retratados anteriormente."),
              img(src="manual_section.png",height = 150,width=150,align="center",style="display: block; margin-left: auto; margin-right: auto;"),
              titlePanel("Mapa"),
              p("Uma vez na seção de Mapa, com o mapa sendo visualizado, o usuário pode segurar o botão esquerdo do mouse e arrastá-lo para mover o mapa, aumentar ou diminuir o nível de ampliação com os símbolos de mais e menos ou clicar nas localizações marcadas para visualizar mais informações sobre elas."),
              img(src="manual_map.png",height = 500,width=500,align="center",style="display: block; margin-left: auto; margin-right: auto;"),
              titlePanel("Seleção de filtros"),
              p("Na seção de seleção de filtros há a possibilidade de selecionar um banco pelo seu nome, ou filtrar bancos pelo seu estado ou ano de fundação. O campo de Banco Comunitário se trata de um campo livre para escrever. O campo de Estado exige que o usuário clique na seta e escolha um dos Estados disponíveis. O campo de Ano de fundação permite que o usuário escreve um ano à esquerda e um ano à direita, determinando um intervalo de busca."),
              p("Além destes filtros convencionais, há uma barra de busca, sinalizada por “Search”, acima da tabela, que se trata de um campo livre. O programa buscará por algo compatível com o que o usuário escreveu em qualquer um dos campos disponíveis."),
              p("O indicador de contagem acima da tabela serve para sinalizar quantos resultados são retornados para esta busca."),
              p("Abaixo da tabela é possível selecionar “Next” para ir para a próxima página de resultados ou “Previous” para voltar, caso haja mais de uma página."),
              p("Por fim, os botões “Copy”, “Excel” e “PDF” servem para exportar os dados da tabela, em texto, tabela ou imagem, respectivamente."),
              img(src="manual_selection.png",height = 500,width=500,style="display: block; margin-left: auto; margin-right: auto;")
      ),
      tabItem(tabName = "my_selection",
              fluidRow(
                box(textInput("bank", label = "Banco Comunitário", placeholder = "Palmas"), width = 4),
                box(selectInput("select_state", label = "Estado", choices = c(
                  "Todos",
                  "Acre",
                  "Alagoas",
                  "Amapá",
                  "Amazonas",
                  "Bahia",
                  "Ceará",
                  "Espírito Santo",
                  "Goiás",
                  "Maranhão",
                  "Mato Grosso",
                  "Mato Grosso do Sul",
                  "Minas Gerais",
                  "Pará",
                  "Paraíba",
                  "Paraná",
                  "Pernambuco",
                  "Piauí",
                  "Rio de Janeiro",
                  "Rio Grande do Norte",
                  "Rio Grande do Sul",
                  "Rondônia",
                  "Roraima",
                  "Santa Catarina",
                  "São Paulo",
                  "Sergipe",
                  "Tocantins"
                ),selected="Todos"), width = 4),
                box(dateRangeInput("daterange", label = "Ano de Fundação",format = "yyyy",startview = "year",language = "pt-BR", start = Sys.Date() - 365*30, end = Sys.Date() + 365,
                                   min = Sys.Date() - 365*50, max = Sys.Date() + 365), width = 4)
                # box(dateRangeInput("daterange", label = "Ano de Fundação",format = "yyyy",startview = "year",language = "pt-BR", start = Sys.Date() - 365*30, end = Sys.Date() + 365,
                #      min = Sys.Date() - 365*50, max = Sys.Date() + 365), width = 4)
              ),
              verbatimTextOutput("text1"),
              DT::dataTableOutput("table")
      )
    )
    
    
  )
)


server <- function(input, output, session) {
  my_table <- read.csv("https://raw.githubusercontent.com/GabrielPonte/CompSoc/main/bancos_python.csv")
  #my_table <-  read_sheet('https://docs.google.com/spreadsheets/d/1W8XYVrhcLSCMMp3HHF8zi5kM3gPA8K-nEvi7Xy7tKNY/edit?usp=sharing')
  #my_table <- read.csv("C:\\Users\\Cliente\\Documents\\CompSoc\\meu_banco.csv",sep =";")
  
  tot_rows <- nrow(my_table)
  col_ano <-my_table$Ano.de.Fundação
  col_banco <- my_table$Banco
  div_name <- reactive({input$bank})
  div_state <- reactive({input$select_state})
  div_year <- reactive({input$daterange})
  x <- rep(TRUE,tot_rows)
  output$table <- renderDataTable({
    # name_cols_table <- list()
    # for col_name in my_colnames
    #   name_cols_table <- append(name_cols_table,list())
    if (div_name() != ""){
      for (i in 1:tot_rows){
        if (startsWith(col_banco[i],div_name())==FALSE){
          x[i] <- FALSE
        }
      }
    }
    if (div_state() != "Todos"){
      x2 <- (my_table$Estado)==div_state()
      for (i in 1:tot_rows){
        if (x2[i] == FALSE){
          x[i] = FALSE
        }
      }
    }
    if ((div_year()[1] == Sys.Date() - 365*30 &&  div_year()[2] ==Sys.Date() + 365) == FALSE){
      init_year <- strtoi(substr(anydate(div_year()[1]),1,4))
      end_year <- strtoi(substr(anydate(div_year()[2]),1,4))
      for (i in 1:tot_rows){
        year_i = (col_ano[i])
        if (is.na(year_i)){
          x[i] = FALSE  
        }else if ((year_i>= init_year && year_i <= end_year) == FALSE){
          x[i] = FALSE
        }
      }
    }
    output$text1 <- renderText(paste("Contagem:",sum(x)))
    output$map <- renderLeaflet({
      new_table <- my_table[x,]
      points <- cbind(new_table$Longitude,new_table$Latitude)
      my_popup <- paste("<b>","Banco ", new_table$Banco,"</b><br>")
      my_colnames <- gsub("."," ", colnames(new_table),fixed=TRUE)
      for (i in 4:ncol(new_table)){
        my_popup <- paste(my_popup,"<br>  - ",my_colnames[i],": ",new_table[[i]])
      }
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addMarkers(data = points,popup = my_popup
                   ,icon=greenLeafIcon)
    })
    
    datatable(my_table[x,], 
              extensions = 'Buttons',
              options = list(
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'excel', 'pdf'),
                pageLength=tot_rows, 
                lengthMenu=c(3,5,10)
                )
              ) %>% 
    formatRound(c(2,3),3) # casas decimais pra lat/long
  })
}
shinyApp(ui, server)

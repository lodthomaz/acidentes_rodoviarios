#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/   

# Carrega pacotes necessários 

library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(stringr)
library(dplyr)
library(feather)
library(leaflet)
library(plotly)
library(scales)
library(tidyr)
library(shinyalert)
library(reactable)
library(shinycssloaders)
library(readr)
library(formattable)
library(kableExtra)
library(DT)


# Upload dos dados ------------------

options(encoding = 'UTF-8')

dados_acidentes_agrupado_ano <- read_feather("dados_acidentes_agrupado_ano.feather")
dados_tipos_acidente <- read_feather("dados_tipos_acidente.feather")
dados_tipos_acidente_classificacao <- read_feather("dados_tipos_acidente_classificacao.feather")
dados_causas_acidente <- read_feather("dados_causas_acidente.feather")
dados_causas_acidente_classificacao <- read_feather("dados_causas_acidente_classificacao.feather")
shape_estados <- geobr::read_state() # read_rds("shape_estados.rds") %>% data.frame()
acidentes_dia_semana <- read_feather("acidentes_dia_semana.feather")
acidentes_dia_semana_tabela  <- read_feather("acidentes_dia_semana_tabela.feather")
dados_fase_dia <- read_feather("dados_fase_dia.feather")
dados_fase_dia_tabela  <- read_feather("dados_fase_dia_tabela.feather")
dados_uf <- read_feather("dados_uf.feather")
dados_br<- read_feather("dados_br.feather")
acidentes_condicao_met <- read_feather("acidentes_condicao_met.feather") 
colnames(acidentes_condicao_met) <- c("Condição Metereológica", "Quantidade", "prop")
dados_clima <- read_feather("dados_clima.feather")
colnames(dados_clima) <- c("classificacao_acidente", "Condição Meteorológica", "Quantidade")

# Choices ---------------------------
choices_ano <- c(2011:2020)
choices_mes <- c("Todos", "Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro", "Outubro", 
                 "Novembro", "Dezembro")

choices_uf <- dados_acidentes_agrupado_ano %>% select(uf) %>% pull() %>% unique()


# Define a interface do usuário ------------------------------

ui <- navbarPage(
  
  id = 'home',
  title = "Acidentes",
  
  # ABA HOME ------------------------------------------
  tabPanel(
    
    # tags$link(rel = "stylesheet", 
    #           type = "text/css", 
    #           href = "custom.css"), 
    
    
    icon = icon('home'), 
    title = 'Home',
    
    br(), 
    br(), 
    
    fluidRow(
      column(width = 2),
      column(width = 4, 
             box(width = 12, height = '700px', 
                 solidHeader = TRUE, background = 'teal',
                 
                 fluidRow(
                   column(width = 12, 
                          align = "center",
                          tags$u(h2("Acidentes Rodoviários")),
                          br(),
                          
                          tags$p(
                            h3("O objetivo deste dashboard é apresentar
                             a análise exploratória dos dados de acidentes que ocorreram nas rodovias brasileiras no período de 01/2011 a 03/2020.")
                          ),
                          h3("Esta base de dados foi escolhida para o meu TCC, no curso de Pós Graduação em Big Data da FIA. 
                                    Nela, os acidentes são classificados entre: sem vítimas, com vítimas feridas, e com vítimas fatais."),
                          h3("A aba resumo exibe os principais números, e as demais apresentarão a análise das principais variáriaveis."),
                          
                          br(), 
                          br(),
                          
                          fluidRow(align = "center",
                                   actionButton('resumo_button', 
                                                label = "Ver estatísticas principais!", icon = icon('car'))
                          )
                   )
                 ),
                 
                 
                 
             )
             
      )
      ,
      column(width = 2,
             # BOX CAUSAS DOS ACIDENTES ------------------------------------- 
             box(width = 12, height = '330px',
                 solidHeader = TRUE, background = 'aqua',
                 fluidRow(                
                   column(width = 12, 
                          align = "center",
                          tags$u(h3("Causas")),
                          br(),
                          h4("Análise das principais causas de cada tipo de acidente."),
                          br()
                   )
                 ),
                 
                 fluidRow(align = "center",
                          actionButton('causas_button', 
                                       label = "Ver Análise!", icon = icon('car'))
                 )
             ),
             
             # BOX CLIMA ----------------------------------
             box(width = 12, height = '330px',
                 solidHeader = TRUE, background = 'light-blue',
                 fluidRow(                
                   column(width = 12, 
                          align = "center",
                          tags$u(h3("Clima")),
                          br(),
                          h4("Análise do clima quando o acidente ocorreu. 
                             Dias de chuvas causam mais acidente? 
                             Ou os motoristas ficam mais cuidadosos?"),
                          br()
                   )
                 ),
                 
                 fluidRow(align = "center",
                          actionButton('clima_button', 
                                       label = "Ver Análise!", icon = icon('sun'))
                 )
                 
             )
      ),
      column(width = 2, 
             
             # BOX DATA -----------------------------------------------
             box(width = 12, height = '330px',
                 solidHeader = TRUE, background = 'light-blue',
                 fluidRow(                
                   column(width = 12, 
                          align = "center",
                          tags$u(h3("Data")),
                          br(),
                          h4("Análise dos acidentes ao longo do tempo. 
                             Será que o dia da semana influencia na ocorrência de um acidente?"),
                          br()
                   )
                 ),
                 
                 fluidRow(align = "center",
                          actionButton('data_button', 
                                       label = "Ver Análise!", icon = icon('calendar'))
                 )
                 
             ),
             
             # BOX MODELAGEM -----------------------------
             box(width = 12, height = '330px',
                 solidHeader = TRUE, background = 'aqua',
                 
                 fluidRow(                
                   column(width = 12, 
                          align = "center",
                          tags$u(h3("Conclusão")),
                          br(),
                          h4("Conclusão das análises"),
                          br()
                   )
                 ),
                 
                 fluidRow(align = "center",
                          actionButton('localizacao_button',  
                                       label = "Ver Conclusão", icon = icon('file-code'))
                 ))
      )
    )
    
    
    
  ),
  
  # ABA RESUMO ------------------------------------------
  tabPanel(
    value = 'id_resumo',
    icon = icon('car'),
    title = "Resumo",
    
    header = tagList(
      useShinydashboard()
    ),
    
    br(), 
    
    # h5("Os valores abaixo exibem a quantidade de acidentes dos filtros à direita."),
    # VALUE BOX --------------------------
    fluidRow(
      
      useShinydashboard(),
      br(), 
      valueBoxOutput(outputId = "total_acidentes", width = 2) %>% withSpinner()
      ,valueBoxOutput(outputId = "acidentes_sem_vitimas", width = 2) %>% withSpinner()
      ,valueBoxOutput(outputId = "acidentes_vitimas_feridas", width = 2) %>% withSpinner()
      ,valueBoxOutput(outputId = "acidentes_vitimas_fatais", width = 2) %>% withSpinner()
      
      ,column(2, 
              fluidRow(selectInput(inputId = 'ano_id', label = "Selecione o Ano:", choices = choices_ano, selected = 2020))
              ,fluidRow(selectInput(inputId = 'mes_id', label = "Selecione o Mês:", choices = choices_mes)))
      ,column(2
              ,fluidRow(
                selectInput(inputId = 'uf_id', label = "Selecione a UF:", choices = choices_uf, multiple = FALSE, selected = "Todos")
                )
              ,br()
              ,useShinyalert()
              ,fluidRow(column(width = 3), 
                        column(3,
                               actionButton(inputId = 'button_help', label = 'Como usar os filtros?')
                        )
              )
              
      )
    )
    ,
    
    fluidRow(
      box(width =  7, background = 'light-blue', height = '600px',
          # title =  "Evolução Acidentes e Classificação",
          tabBox(width = 12,
                 title = "Evolução Acidentes e Classificação",
                 # The id lets us use input$tabset1 on the server to find the current tab
                 id = "tabset1", height = "250px",
                 tabPanel("Visão Anual", 
                          plotlyOutput(outputId = 'evolucao_acidentes_visao_anual', height = '500px') %>% withSpinner()
                 ),
                 tabPanel("Visão Mensal",
                          plotlyOutput(outputId = 'evolucao_acidentes_visao_mensal', height = '500px') %>% withSpinner()
                          
                 )
          )
      )
      ,
      box(width = 5, height = '600px', background = "light-blue",
          leafletOutput(outputId = 'acidentes_mapa', height = '580px') %>% withSpinner()
      )
    ),
    fluidRow(
      box(width = 3, height = '600px',
          title = "Estados com Mais Acidentes", status = 'primary', solidHeader = TRUE,
          DT::dataTableOutput(outputId = 'dt_estados_acidentes')
      ),
      box(width = 3, height = '600px',
          title = "Rodovias com Mais Acidentes", status = 'primary', solidHeader = TRUE,
          textOutput(outputId = 'text_br_acidentes'),
          DT::dataTableOutput(outputId = 'dt_br_acidentes')
      ),
      box(width = 6, height = '600px',
          title = "Boxplots", status = 'primary', solidHeader = TRUE,
          tabBox(width = 12, height = '530px',
                 tabPanel("# Veículos",
                          column(width = 5, plotOutput(outputId = 'boxplot_veiculos')),
                          column(width = 3),
                          column(width = 4, h5("Em média, há 1,74 veículo envolvido por acidente."),
                                 h5("O máximo observado na base de dados foi um acidente que envolveu 25 veículos. "),
                                 h5("Quando observamos o boxplot da quantidade de veículos por classificação do acidente, 
                                nota-se que aparentemente não há diferença nas distribuições."),
                                 h5("Pelo 3º quartil (75% dos dados), observa-se que a maior parte dos acidentes envolve até 2 veículos."))
                          
                 ),
                 tabPanel("# Pessoas",
                          column(width = 5, plotOutput(outputId = 'boxplot_feridos')),
                          column(width = 3),
                          column(width = 4, 
                                 h5("Em média, há 2,22 pessoas envolvidas por acidente. O máximo observado na base de dados foi um acidente que envolveu 248 pessoas."),
                                 h5("Quando observamos o boxplot da quantidade de pessoas por classificação do acidente, nota-se que aparentemente a acidentes sem vítimas envolvem menos pessoas. 
                             Porém, as três classificações apresentam muitos outliers."),
                                 h5("Pelo 3º quartil (75% dos dados), observa-se que a maior parte dos acidentes envolve até 2 pessoas."))
                 )
          )
      )
    )
  ),
  
  # ABA CAUSA  ------------------------------------------
  tabPanel(
    value = 'id_causas',
    icon = icon('car-crash'),
    title = "Causa",
    
    fluidRow(
      box(width = 6, title = "Principais Causas de Acidentes", 
          status = 'primary', solidHeader = TRUE,
          reactableOutput(outputId = 'reactable_causa_acidentes')
      )
      ,tabBox(width = 6,
              tabPanel(title = "Sem Vítimas"
                       ,plotOutput(outputId = 'grafico_causas_sem_vitimas', height = '750px'))
              
              ,tabPanel(title = "Com Vítimas Feridas"
                        ,plotOutput(outputId = 'grafico_causas_vitimas_feridas', height = '750px'))
              
              
              ,tabPanel(title = "Com Vítimas Fatais"
                        ,plotOutput(outputId = 'grafico_causas_vitimas_fatais', height = '750px'))
      )
      
    ),
    
    fluidRow(
      box(width = 6, title = "Principais Tipos de Acidentes", 
          status = 'info', solidHeader = TRUE,
          reactableOutput(outputId = 'reactable_tipo_acidentes')
      )
      ,column(width = 6,
              fluidRow(box(width = 12,
                           solidHeader = TRUE,
                           title = "Sem Vítimas",
                           status = "success",
                           reactableOutput(outputId = 'reactable_sem_vitimas')))
              ,fluidRow(box(width = 12,
                            solidHeader = TRUE,
                            title = "Com Vítimas Feridas",
                            status = "primary",
                            reactableOutput(outputId = 'reactable_vitimas_feridas')))
              ,fluidRow(box(width = 12,
                            solidHeader = TRUE,
                            title = "Com Vítimas Fatais", 
                            status = "danger",
                            reactableOutput(outputId = 'reactable_vitimas_fatais')))
      )
    )
    
    
    
  ),
  
  # ABA DATA  ------------------------------------------
  tabPanel(
    value = 'id_data',
    icon = icon('calendar'),
    title = "Data",
    
    fluidRow(
      box(width = 12, 
          status = 'primary', solidHeader = TRUE, 
          title = "Classificação do Acidente",
          selectInput(inputId = 'classificacao_acidente_id', 
                      label = "Selecione a Classificação do Acidente:",
                      choices = c("Sem Vítimas", "Com Vítimas Feridas", "Com Vítimas Fatais")))
      
      
    ),
    
    fluidRow(
      box(width = 12, status = 'primary', solidHeader = TRUE, 
          title = "Dia da Semana dos Acidentes",
          column(width = 5, 
                 plotOutput(outputId = 'barplot_dia_semana')
          ),
          column(width = 7, 
                 formattableOutput(outputId = 'formattable_dia_semana')
          )
      )
      
    ),
    
    fluidRow(
      box(width = 12, status = 'primary', solidHeader = TRUE, 
          title = "Fase do Dia dos Acidentes",
          column(4, 
                 br(), 
                 br(), 
                 br(),
                 br(),
                 formattableOutput(outputId = 'formattable_fase_do_dia')
          ),
          column(4,
                 
                 br(), 
                 br(), 
                 br(),
                 
                 h3("58% dos acidentes observados ocorreram em pleno dia, como pode ser observado na tabela à esquerda."),
                 h3("No entanto, quando observamos o gráfico ao lado, a maior parte dos acidentes com vítimas fatais (48%) ocorreram em plena noite."),
          ),
          column(4,
                 plotOutput(outputId = 'barplot_fase_do_dia')
          )
      )
    )
    
  ),
  
  # ABA CLIMA  ------------------------------------------
  tabPanel(
    value = 'id_clima',
    icon = icon('sun'),
    title = "Clima",
    fluidRow(
      box(title = "Condição Climática", width = 12,
          status = 'warning', solidHeader = TRUE, 
          column(width = 6,
                 plotOutput(outputId = 'bar_clima')),
          column(width = 6,
                 tableOutput(outputId = 'kable_clima')
              )
          )
      
    )
  ),
  
  # ABA LOCALIZAÇÃO  ------------------------------------------
  tabPanel(
    value = 'id_local',
    icon = icon('file-code'),
    title = "Conclusão",
    box(title = "Objetivo", 
        width = 6, status = 'primary', solidHeader = TRUE, 
        h3("A Polícia Rodoviária Federal atende cerca de 70 mil quilômetros de rodovias federais e uma de suas responsabilidades é atender acidentes. 
           Estes são registrados no sistema BR-Brasil, que coleta informações como: estado físico dos envolvidos, informações sobre os veículos, 
           causa do acidente, etc)."),
        h3("Em atendimento às diretrizes do Programa de Dados Abertos Governamentais, estes dados são disponibilizados em arquivos de formato .csv."),
        h3("O objetivo desta análise é tentar identificar possíveis variáveis que ajudem a prever a classificação dos acidentes entre: sem vítimas, com vítimas feridas ou com vítimas fatais. 
           E com isso, identificar quais características mais contribuem para acidentes com vítimas (especialmente vítimas fatais). "),
        h3("Este resultado pode servir para orientar campanhas ou ações nas rodovias para que o número de acidentes diminua cada vez mais")
    ),
    box(title = "Conclusão", width = 6, status = 'primary', solidHeader = TRUE, 
        h3("Pela análise exploratória dos dados é possível observar que algumas variáveis podem ser importante na hora de diferenciar a classificação dos acidentes."),
        h3("- Alguns rodovias são mais perigosas (maior frequência de acidentes fatais) que outras."),
        h3("- O período do dia também é importante, pois observa-se que a fase do dia 'Plena Noite' é mais frequente em acidentes fatais que nos demais tipos de acidentes."),
        h3("- O tipo de acidente também nos mostrou que colisão frontal ocupa a terceira posição de acidentes com vítimas fatais, enquanto que para os outros, colisão traseira é mais comum."),
        h3("Ou seja, com os dados que são disponibilizados é possível ajustar um modelo para tentar prever a classificação do acidente, e esta será a próxima parte do estudo.")
        
        )
  )
)



# Define o server side -------------------------

server <- function(input, output, session) {
  
  observeEvent(input$button_help, {
    # SHINY ALERT HELP ------------
    shinyalert(
      title = "Como usar os filtros?",
      text = "Ano: O filtro de Ano funciona para os infoboxs, para o mapa, e para a evolução dos acidentes visão mensal,
            Mês: O filtro de Mês funciona para os infoboxs,
            UF: O filtro de UF funciona para os infoboxs e para os gráficos de evolução dos acidentes.",
      size = "l", 
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "info",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
  })
  
  observeEvent(input$resumo_button, {
    updateNavbarPage(session, "home", selected = "id_resumo")
  }, ignoreInit = TRUE
  )
  
  observeEvent(input$causas_button, {
    updateNavbarPage(session, "home", selected = "id_causas")
  }, ignoreInit = TRUE
  )
  
  observeEvent(input$clima_button, {
    updateNavbarPage(session, "home", selected = "id_clima")
  }, ignoreInit = TRUE
  )
  
  observeEvent(input$data_button, {
    updateNavbarPage(session, "home", selected = "id_data")
  }, ignoreInit = TRUE
  )
  
  observeEvent(input$localizacao_button, {
    updateNavbarPage(session, "home", selected = "id_local")
  }, ignoreInit = TRUE
  )
  
  # VALUEBOX TOTAL ACIDENTES  ------------------------
  output$total_acidentes <- renderValueBox({
    
    valor <- dados_acidentes_agrupado_ano %>% 
      filter(ano_ref == input$ano_id & uf == input$uf_id & mes == input$mes_id) %>% 
      select(qtde_acidentes_ano) %>% sum() %>% 
      number(big.mark = '.', decimal.mark = ',')
    
    
    valueBox(value = valor, 
             icon = icon("car-crash"),
             color = 'teal', 
             subtitle = "Total Acidentes"
    )
  })
  
  # VALUEBOX TOTAL ACIDENTES  ------------------------
  output$acidentes_sem_vitimas <- renderValueBox({
    
    valor <- dados_acidentes_agrupado_ano %>%
      filter(classificacao_acidente == "Sem Vítimas") %>% 
      filter(ano_ref == input$ano_id & uf == input$uf_id & mes == input$mes_id) %>% 
      select(qtde_acidentes_ano) %>% sum() %>% 
      number(big.mark = '.', decimal.mark = ',')
    
    valueBox(value = valor, 
             icon = icon("car"),
             color = 'aqua', 
             subtitle = "Sem Vítimas"
    )
    
  })
  
  # VALUEBOX TOTAL ACIDENTES  ------------------------
  output$acidentes_vitimas_feridas <- renderValueBox({
    
    valor <- dados_acidentes_agrupado_ano %>%
      filter(classificacao_acidente == "Com Vítimas Feridas") %>% 
      filter(ano_ref == input$ano_id & uf == input$uf_id & mes == input$mes_id) %>% 
      select(qtde_acidentes_ano) %>% sum() %>% 
      number(big.mark = '.', decimal.mark = ',')
    
    valueBox(value = valor, 
             icon = icon("hospital", lib = "font-awesome"),
             color = 'light-blue',
             subtitle = "Com Vítimas Feridas"
    )
    
  })
  
  # VALUEBOX TOTAL ACIDENTES  ------------------------
  output$acidentes_vitimas_fatais <- renderValueBox({
    
    valor <- dados_acidentes_agrupado_ano %>%
      filter(classificacao_acidente == "Com Vítimas Fatais")%>% 
      filter(ano_ref == input$ano_id & uf == input$uf_id & mes == input$mes_id) %>% 
      select(qtde_acidentes_ano) %>% sum() %>% 
      number(big.mark = '.', decimal.mark = ',')
    
    valueBox(value = valor, 
             icon = icon("ribbon"), 
             color = "blue",
             subtitle = "Com Vítimas Fatais"
    )
    
    
  })
  
  # GRÁFICO EVOLUÇÃO ACIDENTES  ------------------------
  
  output$evolucao_acidentes_visao_anual <- renderPlotly({
    
    data <- dados_acidentes_agrupado_ano %>% 
      filter(mes != 'Todos') %>% 
      filter(uf == input$uf_id) %>%
      dplyr::group_by(ano_ref, uf, classificacao_acidente) %>%
      dplyr::summarise(qtde = sum(qtde_acidentes_ano)) %>% 
      dplyr::ungroup() %>% 
      spread(key = classificacao_acidente, value = qtde)
    
    fig <- plot_ly(data, x = ~ano_ref, y = ~`Sem Vítimas`, name = 'Sem Vítimas', type = 'scatter', mode = 'lines+markers') %>% 
      add_trace(y = ~`Com Vítimas Feridas`, name = 'Com Vítimas Feridas', mode = 'lines+markers') %>% 
      add_trace(y = ~`Com Vítimas Fatais`, name = 'Com Vítimas Fatais', mode = 'lines+markers') %>% 
      layout(title = 'Total de Acidentes por Ano',
             xaxis = list(title = 'Ano',
                          zeroline = TRUE,
                          range = c(2011, 2020)),
             yaxis = list(title = 'Quantidade'))
    
  })
  
  # GRÁFICO EVOLUÇÃO ACIDENTES MENSAL ------------------------
  
  output$evolucao_acidentes_visao_mensal <- renderPlotly({
    
    data <- dados_acidentes_agrupado_ano %>% 
      filter(mes != 'Todos' & ano_ref == input$ano_id) %>% 
      filter(uf == input$uf_id) %>%
      mutate(mes = factor(mes, 
                          levels = c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
                                     "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"),
                          labels = c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
                                     "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"))) %>%  
      dplyr::group_by(ano_ref, mes, uf, classificacao_acidente) %>%
      dplyr::summarise(qtde = sum(qtde_acidentes_ano)) %>% 
      dplyr::ungroup() %>% 
      spread(key = classificacao_acidente, value = qtde)
    
    fig <- plot_ly(data, x = ~mes, y = ~`Sem Vítimas`, name = 'Sem Vítimas', type = 'scatter', mode = 'lines+markers') %>% 
      add_trace(y = ~`Com Vítimas Feridas`, name = 'Com Vítimas Feridas', mode = 'lines+markers') %>% 
      add_trace(y = ~`Com Vítimas Fatais`, name = 'Com Vítimas Fatais', mode = 'lines+markers') %>% 
      layout(title = 'Total de Acidentes por Ano',
             xaxis = list(title = 'Mês',
                          zeroline = TRUE),
             yaxis = list(title = 'Quantidade'))
    
  })
  
  output$acidentes_mapa <- renderLeaflet({
    
    data <- dados_acidentes_agrupado_ano %>% 
      filter(uf != "Todos" & mes == "Todos" & ano_ref == input$ano_id) %>% 
      dplyr::group_by(uf) %>% 
      dplyr::summarise(qtde = sum(qtde_acidentes_ano)) %>% 
      dplyr::ungroup()
    
    color_fun <- colorBin(
      "GnBu",
      domain = range(data$qtde),
      bins = 4
    )
    
    shape_estados %>%
      inner_join(data, c("abbrev_state" = "uf")) %>%
      leaflet() %>%
      addTiles() %>%
      addPolygons(
        weight = .5,
        color = "black",
        opacity = .7,
        fillOpacity = .9,
        fillColor = ~color_fun(qtde)
      ) %>% 
      addLegend("bottomright", pal = color_fun, values = ~data$qtde,
                title = str_c("Quantidade de Acidentes"),
                # labFormat = labelFormat(prefix = "$"),
                opacity = .9
      )
  })
  
  # REACTABLE TIPOS DE ACIDENTES ------------------------
  
  output$reactable_tipo_acidentes <- renderReactable({
    # Render a bar chart with a label on the left
    bar_chart <- function(label, width = "100%", height = "14px", fill = "#00bfc4", background = NULL) {
      bar <- div(style = list(background = fill, width = width, height = height))
      chart <- div(style = list(flexGrow = 1, marginLeft = "6px", background = background), bar)
      div(style = list(display = "flex", alignItems = "center"), label, chart)
    }
    
    reactable(
      defaultPageSize = 20,
      data = dados_tipos_acidente,
      columns = list(
        tipo_acidente_simpl = colDef(
          name = "Tipo de Acidente"
        ),
        qtde_acidentes = colDef(
          name = "Quantidade de Acidentes",
          defaultSortOrder = "desc",
          cell = function(value) {
            width <- paste0(value * 100 / sum(dados_tipos_acidente$qtde_acidentes), "%")
            value <- format(value, big.mark = ".")
            value <- format(value, width = 9, justify = "right")
            bar_chart(value, width = width, fill = "#3fc1c9")
          },
          align = "center",
          style = list(whiteSpace = "pre")
        ),
        prop = colDef(
          name = "(%) Acidentes",
          defaultSortOrder = "desc",
          format = colFormat(percent = TRUE, digits = 1),
          align = "center",
        )
      )
    )
  })
  
  output$reactable_sem_vitimas <- renderReactable({
    # Render a bar chart with a label on the left
    bar_chart <- function(label, width = "100%", height = "14px", fill = "#00bfc4", background = NULL) {
      bar <- div(style = list(background = fill, width = width, height = height))
      chart <- div(style = list(flexGrow = 1, marginLeft = "6px", background = background), bar)
      div(style = list(display = "flex", alignItems = "center"), label, chart)
    }
    
    dados <- dados_tipos_acidente_classificacao %>% 
      filter(classificacao_acidente == "Sem Vítimas") %>% 
      select(tipo_acidente_simpl, qtde_acidentes, prop)
    
    reactable(
      defaultPageSize = 3,  
      data = dados,
      columns = list(
        tipo_acidente_simpl = colDef(
          name = "Tipo de Acidente"
        ),
        qtde_acidentes = colDef(
          name = "Quantidade de Acidentes",
          defaultSortOrder = "desc",
          cell = function(value) {
            width <- paste0(value * 100 / sum(dados$qtde_acidentes), "%")
            value <- format(value, big.mark = ".")
            value <- format(value, width = 9, justify = "right")
            bar_chart(value, width = width, fill = "#00BA25")
          },
          align = "center",
          style = list(whiteSpace = "pre")
        ),
        prop = colDef(
          name = "(%) Acidentes",
          defaultSortOrder = "desc",
          format = colFormat(percent = TRUE, digits = 1),
          align = "center",
        )
      )
    )
  })
  
  output$reactable_vitimas_feridas <- renderReactable({
    # Render a bar chart with a label on the left
    bar_chart <- function(label, width = "100%", height = "14px", fill = "#00bfc4", background = NULL) {
      bar <- div(style = list(background = fill, width = width, height = height))
      chart <- div(style = list(flexGrow = 1, marginLeft = "6px", background = background), bar)
      div(style = list(display = "flex", alignItems = "center"), label, chart)
    }
    
    dados <- dados_tipos_acidente_classificacao %>% 
      filter(classificacao_acidente == "Com Vítimas Feridas") %>% 
      select(tipo_acidente_simpl, qtde_acidentes, prop)
    
    reactable(
      defaultPageSize = 3,  
      data = dados,
      columns = list(
        tipo_acidente_simpl = colDef(
          name = "Tipo de Acidente"
        ),
        qtde_acidentes = colDef(
          name = "Quantidade de Acidentes",
          defaultSortOrder = "desc",
          cell = function(value) {
            width <- paste0(value * 100 / sum(dados$qtde_acidentes), "%")
            value <- format(value, big.mark = ".")
            value <- format(value, width = 9, justify = "right")
            bar_chart(value, width = width, fill = "#366CB2")
          },
          align = "center",
          style = list(whiteSpace = "pre")
        ),
        prop = colDef(
          name = "(%) Acidentes",
          defaultSortOrder = "desc",
          format = colFormat(percent = TRUE, digits = 1),
          align = "center",
        )
      )
    )
  })
  
  output$reactable_vitimas_fatais <- renderReactable({
    # Render a bar chart with a label on the left
    bar_chart <- function(label, width = "100%", height = "14px", fill = "#00bfc4", background = NULL) {
      bar <- div(style = list(background = fill, width = width, height = height))
      chart <- div(style = list(flexGrow = 1, marginLeft = "6px", background = background), bar)
      div(style = list(display = "flex", alignItems = "center"), label, chart)
    }
    
    dados <- dados_tipos_acidente_classificacao %>% 
      filter(classificacao_acidente == "Com Vítimas Fatais") %>% 
      select(tipo_acidente_simpl, qtde_acidentes, prop)
    
    reactable(
      defaultPageSize = 3,  
      data = dados,
      columns = list(
        tipo_acidente_simpl = colDef(
          name = "Tipo de Acidente"
        ),
        qtde_acidentes = colDef(
          name = "Quantidade de Acidentes",
          defaultSortOrder = "desc",
          cell = function(value) {
            width <- paste0(value * 100 / sum(dados$qtde_acidentes), "%")
            value <- format(value, big.mark = ".")
            value <- format(value, width = 9, justify = "right")
            bar_chart(value, width = width, fill = "#FF0000")
          },
          align = "center",
          style = list(whiteSpace = "pre")
        ),
        prop = colDef(
          name = "(%) Acidentes",
          defaultSortOrder = "desc",
          format = colFormat(percent = TRUE, digits = 1),
          align = "center",
        )
      )
    )
    
  })
  
  
  # REACTABLE CAUSAS ACIDENTES ------------------------
  output$reactable_causa_acidentes <- renderReactable({
    # Render a bar chart with a label on the left
    bar_chart <- function(label, width = "100%", height = "14px", fill = "#00bfc4", background = NULL) {
      bar <- div(style = list(background = fill, width = width, height = height))
      chart <- div(style = list(flexGrow = 1, marginLeft = "6px", background = background), bar)
      div(style = list(display = "flex", alignItems = "center"), label, chart)
    }
    
    reactable(
      defaultPageSize = 20,
      data = dados_causas_acidente,
      columns = list(
        causa_acidente_2 = colDef(
          name = "Causa do Acidente"
        ),
        qtde_acidentes = colDef(
          name = "Quantidade de Acidentes",
          defaultSortOrder = "desc",
          cell = function(value) {
            width <- paste0(value * 100 / sum(dados_causas_acidente$qtde_acidentes), "%")
            value <- format(value, big.mark = ".")
            value <- format(value, width = 9, justify = "right")
            bar_chart(value, width = width, fill = "#3fc1c9")
          },
          align = "center",
          style = list(whiteSpace = "pre")
        ),
        prop = colDef(
          name = "(%) Acidentes",
          defaultSortOrder = "desc",
          format = colFormat(percent = TRUE, digits = 1),
          align = "center",
        )
      )
    )
    
  })
  
  # GRAFICO BARRA CAUSAS SEM VITIMAS -----------------------
  output$grafico_causas_sem_vitimas <- renderPlot({
    
    dados <- dados_causas_acidente_classificacao %>%
      filter(classificacao_acidente == 'Sem Vítimas') %>% 
      arrange(desc(qtde_acidentes)) %>% 
      head(7) %>% 
      mutate(y_prop = qtde_acidentes/total)
    
    dados %>% 
      ggplot(
        aes(y = reorder(causa_acidente_2,y_prop,sum),
            x = y_prop,
            label = prop,
            fill = reorder(causa_acidente_2,y_prop,sum)
        )
      ) +
      geom_col() + 
      geom_label() + 
      theme_minimal() + 
      scale_fill_brewer(palette = "GnBu") + 
      labs(title = "TOP 7 Causas de Acidentes Sem Vítimas", 
           x = "Causas dos Acidentes",
           y = "Proporção") + 
      theme(legend.position = 'none')
    
  })
  
  # GRAFICO BARRA CAUSAS COM VITIMAS FERIDAS -----------------------
  output$grafico_causas_vitimas_feridas <- renderPlot({
    
    dados <- dados_causas_acidente_classificacao %>%
      filter(classificacao_acidente == 'Com Vítimas Feridas') %>% 
      arrange(desc(qtde_acidentes)) %>% 
      head(7) %>% 
      mutate(y_prop = qtde_acidentes/total)
    
    dados %>% 
      ggplot(
        aes(y = reorder(causa_acidente_2,y_prop,sum),
            x = y_prop,
            label = prop,
            fill = reorder(causa_acidente_2,y_prop,sum)
        )
      ) +
      geom_col() + 
      geom_label() + 
      theme_minimal() + 
      scale_fill_brewer(palette = "YlOrRd") + 
      labs(title = "TOP 7 Causas de Acidentes Sem Vítimas", 
           x = "Causas dos Acidentes",
           y = "Proporção") + 
      theme(legend.position = 'none')
    
  })
  
  # GRAFICO BARRA CAUSAS COM VITIMAS FATAIS -----------------------
  output$grafico_causas_vitimas_fatais <- renderPlot({
    
    dados <- dados_causas_acidente_classificacao %>%
      filter(classificacao_acidente == 'Com Vítimas Fatais') %>% 
      arrange(desc(qtde_acidentes)) %>% 
      head(7) %>% 
      mutate(y_prop = qtde_acidentes/total)
    
    dados %>% 
      ggplot(
        aes(y = reorder(causa_acidente_2,y_prop,sum),
            x = y_prop,
            label = prop,
            fill = reorder(causa_acidente_2,y_prop,sum)
        )
      ) +
      geom_col() + 
      geom_label() + 
      theme_minimal() + 
      scale_fill_brewer(palette = "Reds") + 
      labs(title = "TOP 7 Causas de Acidentes Sem Vítimas", 
           x = "Causas dos Acidentes",
           y = "Proporção") + 
      theme(legend.position = 'none')
    
  })
  
  # Gráfico dia da Semana -----------------------------------------------
  output$barplot_dia_semana <- renderPlot({
    
    dados <- acidentes_dia_semana %>% 
      filter(classificacao_acidente == input$classificacao_acidente_id)
    
    ggplot(dados, mapping = aes(x = `Dia da Semana`, 
                                y = Quantidade,
                                label = number(Quantidade, 
                                               big.mark = '.', decimal.mark = ','),
                                group = 1)) +
      geom_col() +
      geom_label() + 
      theme_minimal() +
      labs(title = "Quantidade de Acidentes por Ano") +
      xlab("Ano do Acidente") + 
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  # Tabela dia da Semana -----------------------------------------------
  output$formattable_dia_semana <- renderFormattable({
    
    dados <- acidentes_dia_semana_tabela %>% 
      filter(classificacao_acidente == input$classificacao_acidente_id) %>% 
      select(`Dia da Semana` = dia_semana_padronizado, 
             everything(), -classificacao_acidente)
    
    
    dados %>% 
      formattable(list(`2011` = color_tile("#FFDFDF", "#F97E7E"),
                       `2012` = color_tile("#FFDFDF", "#F97E7E"),
                       `2013` = color_tile("#FFDFDF", "#F97E7E"),
                       `2014` = color_tile("#FFDFDF", "#F97E7E"),
                       `2015` = color_tile("#FFDFDF", "#F97E7E"),
                       `2016` = color_tile("#FFDFDF", "#F97E7E"),
                       `2017` = color_tile("#FFDFDF", "#F97E7E"),
                       `2018` = color_tile("#FFDFDF", "#F97E7E"),
                       `2019` = color_tile("#FFDFDF", "#F97E7E"),
                       `2020` = color_tile("#FFDFDF", "#F97E7E")
      ), align = c('c'))
    
    
  })
  
  # GRAFICO DE BARRAS FASE DO DIA ------------------
  output$barplot_fase_do_dia <- renderPlot({
    
    dados_fase_dia %>% 
      ggplot(aes(x = classificacao_acidente,
                 y = prop, 
                 group = `Fase do Dia`)) + 
      geom_col(aes(fill = `Fase do Dia`), position = 'fill') + 
      geom_text(aes(label = prop_text), position = position_fill(vjust = .5)) +
      theme_minimal() + 
      scale_fill_brewer("Fase do Dia", palette = "GnBu") + 
      labs(title = "Proporção por Fase do Dia", x = "Classificação do Acidente", y = "Proporção") + 
      theme(plot.title = element_text(hjust = 0.5))
    
    
  })
  
  # FORMATTABLE FASE DO DIA ------------------------
  output$formattable_fase_do_dia <- renderFormattable({
    
    dados_fase_dia_tabela %>% 
      formattable(align = c("c"),
                  list(`(%)` = percent))
    
    
  })  
  
  # DATATABLE UF ------------------------
  output$dt_estados_acidentes <- DT::renderDataTable({
    
    datatable(dados_uf, 
              rownames = FALSE)
    
    
  },  server = TRUE)  
  
  # DATATABLE BR ------------------------
  output$dt_br_acidentes <- DT::renderDataTable({
    
    datatable(dados_br %>% 
                filter(uf == input$uf_id) %>% 
                arrange(desc(Quantidade)) %>% 
                mutate(prop = number((Quantidade / sum(Quantidade)) * 100, accuracy = .1, suffix = "%"),
                       Quantidade = number(Quantidade, big.mark = '.'),
                ) %>% 
                select(Rodovia = br, Quantidade, `(%)` = prop), 
              rownames = FALSE)
    
    
  },  server = TRUE)  
  
  # TEXTO ESTADO SELECIONADO BR ------------------------
  output$text_br_acidentes <- renderText({
    
    print(str_c("UF selecionada: ", input$uf_id))
    
  })
  
  # BOX PLOT VEICULOS --------------------------------------
  output$boxplot_veiculos <- renderImage(list(src = 'boxplot_veiculos.jpg'), deleteFile = FALSE)
  
  # BOX PLOT Feridos  --------------------------------------
  output$boxplot_feridos <- renderImage(list(src = 'boxplot_pessoas.jpg'), deleteFile = FALSE)
  
  # Kable Clima  --------------------------------------
  output$kable_clima <- function(){
    
    acidentes_condicao_met %>% 
      kable() %>% 
      row_spec(c(1,4), background = "yellow") %>% 
      kable_styling() %>% 
      kable_styling(bootstrap_options = c("striped", "hover"))
    
  }
  
  # BAR CLIMA --------------------------
  output$bar_clima <- renderPlot({
    
    dados_clima %>% 
      ggplot(aes(x = `Condição Meteorológica`,
                 y = Quantidade, 
                 label = number(Quantidade, big.mark = '.'),
                 fill = `Condição Meteorológica`,
                 group = classificacao_acidente)) + 
      geom_col() + 
      geom_label(position = position_dodge(0.9), vjust = -.3) +
      facet_wrap(~classificacao_acidente) + 
      theme_minimal() + 
      scale_fill_brewer(palette = "Oranges") + 
      theme(legend.position = 'none') + 
      ylim(0, 380000)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


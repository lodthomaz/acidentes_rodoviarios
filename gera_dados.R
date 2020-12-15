library(dplyr)
library(lubridate)
library(feather)
library(scales)
library(formattable)
library(ggplot2)
library(tidyr)

df_aux_tipo_acidente <- data.frame(
  tipo_acidente = c("Atropelamento de pessoa", 
                    "Atropelamento de Pedestre",
                    "Colisão com objeto estático",
                    "Atropelamento de Animal",
                    "Colisão com objeto em movimento",
                    "Colisão Transversal",
                    "Danos Eventuais",
                    "Derramamento de Carga"),
  tipo_acidente_simpl = c("Atropelamento de Pedestre",
                          "Atropelamento de Pedestre",
                          "Colisão com objeto fixo",
                          "Atropelamento de animal",
                          "Colisão com objeto móvel",
                          "Colisão transversal",
                          "Danos eventuais",
                          "Derramamento de carga")
)



dados <- read_feather("C:/Users/le_th/OneDrive/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Dashboard/www/data/base_total.feather") %>% 
  left_join(df_aux_tipo_acidente) %>% 
  mutate(tipo_acidente_simpl = as.character(tipo_acidente_simpl),
         tipo_acidente_simpl = if_else(is.na(tipo_acidente_simpl), tipo_acidente, tipo_acidente_simpl))


# Prepara dados para dash 

dados_acidentes <- dados %>% 
  mutate(data_ref = if_else(ano == 2011, 
                            as.Date(data_inversa, format = "%d/%m/%Y"),
                            if_else(ano %in% c(2012:2015,2017:2020), as.Date(data_inversa, format = "%Y-%m-%d"),
                                    as.Date(data_inversa, format = "%d/%m/%y"))),
         dia_ref = day(data_ref), 
         mes_ref = month(data_ref)) %>% 
  filter(uf != '(null)') %>% 
  mutate(mes_texto = factor(mes_ref, levels = c(1:12),
                            labels = c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
                                       "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")))


# Agrupa dados por Ano
dados_acidentes_agrupado_ano <- dados_acidentes %>% 
  dplyr::group_by(ano_ref = ano, classificacao_acidente) %>% 
  dplyr::summarise(qtde_acidentes_ano = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    uf = "Todos",
    mes = "Todos"
  ) %>% 
  rbind(
    dados_acidentes %>% 
      dplyr::group_by(ano_ref = ano, uf, classificacao_acidente) %>% 
      dplyr::summarise(qtde_acidentes_ano = n()) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        mes = "Todos"
      ) 
  ) %>% 
  rbind(
    dados_acidentes %>% 
      dplyr::group_by(ano_ref = ano, mes = mes_texto, uf, classificacao_acidente) %>% 
      dplyr::summarise(qtde_acidentes_ano = n()) %>% 
      dplyr::ungroup()
  ) %>% 
  rbind(
    dados_acidentes %>% 
      dplyr::group_by(ano_ref = ano, mes = mes_texto, classificacao_acidente) %>% 
      dplyr::summarise(qtde_acidentes_ano = n()) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        uf = "Todos"
      )
  )


dados_acidentes_agrupado_ano %>% write_feather("C:/Users/le_th/OneDrive/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Dashboard/www/data/dados_acidentes_agrupado_ano.feather")

# Tipos de Acidente

dados_tipos_acidente <- dados_acidentes %>% 
  dplyr::group_by(tipo_acidente_simpl) %>% 
  dplyr::summarise(qtde_acidentes = n()) %>% 
  dplyr::ungroup() %>% 
  mutate(prop = percent(qtde_acidentes / sum(qtde_acidentes), accuracy = .1, suffix = '%')) %>% 
  arrange(desc(qtde_acidentes))

dados_tipos_acidente %>% write_feather("C:/Users/le_th/OneDrive/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Dashboard/www/data/dados_tipos_acidente.feather")


dados_tipos_acidente_classificacao <- dados_acidentes %>% 
  dplyr::group_by(classificacao_acidente, tipo_acidente_simpl) %>% 
  dplyr::summarise(qtde_acidentes = n()) %>% 
  dplyr::ungroup() %>% 
  mutate(total = ave(qtde_acidentes, classificacao_acidente, FUN = sum)) %>% 
  mutate(prop = percent(qtde_acidentes / total, accuracy = .1, suffix = '%')) %>% 
  arrange(desc(qtde_acidentes))

dados_tipos_acidente_classificacao %>% write_feather("C:/Users/le_th/OneDrive/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Dashboard/www/data/dados_tipos_acidente_classificacao.feather")


# Causas dos Acidente

dados_causas_acidente <- dados_acidentes %>% 
  dplyr::group_by(causa_acidente_2) %>% 
  dplyr::summarise(qtde_acidentes = n()) %>% 
  dplyr::ungroup() %>% 
  mutate(prop = percent(qtde_acidentes / sum(qtde_acidentes), accuracy = .1, suffix = '%')) %>% 
  arrange(desc(qtde_acidentes))

dados_causas_acidente %>% write_feather("C:/Users/le_th/OneDrive/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Dashboard/www/data/dados_causas_acidente.feather")

dados_causas_acidente_classificacao <- dados_acidentes %>% 
  dplyr::group_by(classificacao_acidente, causa_acidente_2) %>% 
  dplyr::summarise(qtde_acidentes = n()) %>% 
  dplyr::ungroup() %>% 
  mutate(total = ave(qtde_acidentes, classificacao_acidente, FUN = sum)) %>% 
  mutate(prop = number((qtde_acidentes / total) * 100, accuracy = .1, suffix = '%')) %>% 
  arrange(desc(qtde_acidentes))

dados_causas_acidente_classificacao %>% write_feather("C:/Users/le_th/OneDrive/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Dashboard/www/data/dados_causas_acidente_classificacao.feather")

# Dia da Semana dos Acidente

acidentes_dia_semana <- dados_acidentes %>% 
  dplyr::group_by(`Dia da Semana` = dia_semana_padronizado) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::ungroup() %>% 
  mutate(classificacao_acidente = "Geral") %>% 
  rbind(dados_acidentes %>% 
          dplyr::group_by(classificacao_acidente, `Dia da Semana` = dia_semana_padronizado) %>% 
          dplyr::summarise(Quantidade = n()) %>% 
          dplyr::ungroup()
        ) 


ggplot(acidentes_dia_semana, mapping = aes(x = `Dia da Semana`, 
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

acidentes_dia_semana %>% write_feather("C:/Users/le_th/OneDrive/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Dashboard/www/data/acidentes_dia_semana.feather")

acidentes_dia_semana_tabela <- dados_acidentes %>% 
  dplyr::group_by(classificacao_acidente, dia_semana_padronizado, ano) %>% 
  dplyr::summarise(qtde_acidentes = n()) %>% 
  dplyr::ungroup() %>% 
  spread(key = 'ano', value = 'qtde_acidentes')

acidentes_dia_semana_tabela %>% write_feather("C:/Users/le_th/OneDrive/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Dashboard/www/data/acidentes_dia_semana_tabela.feather")

acidentes_dia_semana_tabela %>% 
  filter(classificacao_acidente == 'Com Vítimas Fatais') %>% 
  select(`Dia da Semana` = dia_semana_padronizado, 
         everything(), -classificacao_acidente) %>% 
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

# Fase do Dia 

dados_fase_dia <- dados_acidentes %>% 
  dplyr::mutate(fase_dia = if_else(fase_dia == "Plena Noite", "Plena noite", fase_dia)) %>% 
  filter(fase_dia %in% c("Amanhecer", "Anoitecer", "Plena noite", "Pleno dia")) %>% 
  dplyr::group_by(classificacao_acidente, `Fase do Dia` = fase_dia) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::mutate(prop_text = number((Quantidade / sum(Quantidade)) * 100, accuracy = 1, suffix = '%'),
                prop = (Quantidade / sum(Quantidade)) * 100) %>% 
  dplyr::ungroup() 

dados_fase_dia %>% write_feather("C:/Users/le_th/OneDrive/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Dashboard/www/data/dados_fase_dia.feather")

dados_fase_dia_tabela <- dados_acidentes %>% 
  dplyr::mutate(fase_dia = if_else(fase_dia == "Plena Noite", "Plena noite", fase_dia)) %>% 
  filter(fase_dia %in% c("Amanhecer", "Anoitecer", "Plena noite", "Pleno dia")) %>% 
  dplyr::group_by(`Fase do Dia` = fase_dia) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::mutate(prop = (Quantidade / sum(Quantidade))) %>% 
  dplyr::ungroup() %>% 
  mutate(`Fase do Dia` = factor(`Fase do Dia`, levels = c("Amanhecer", "Pleno dia", "Anoitecer", "Plena noite")),
         Quantidade = number(Quantidade, big.mark = '.')) %>% 
  arrange(`Fase do Dia`) %>% 
  select(everything(), `(%)` = prop)

dados_fase_dia_tabela %>% write_feather("C:/Users/le_th/OneDrive/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Dashboard/www/data/dados_fase_dia_tabela.feather")

dados_fase_dia_tabela %>% 
  formattable(align = c("c"),
              list(`(%)` = percent))
  
# Rodovias mais perigosas 

dados_uf <- dados_acidentes %>% 
  dplyr::group_by(uf) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::mutate(prop = (Quantidade / sum(Quantidade))) %>% 
  dplyr::ungroup() %>% 
  arrange(desc(Quantidade)) %>% 
  mutate(Quantidade = number(Quantidade, big.mark = '.')) %>%  
  select(UF = uf, Quantidade, `(%)` = prop)

dados_uf %>% write_feather("C:/Users/le_th/OneDrive/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Dashboard/www/data/dados_uf.feather")


dados_uf <- dados_acidentes %>% 
  dplyr::group_by(uf) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::mutate(prop = (Quantidade / sum(Quantidade))) %>% 
  dplyr::ungroup() %>% 
  arrange(desc(Quantidade)) %>% 
  select(UF = uf, everything(), `(%)` = prop) %>% 
  mutate(Quantidade = number(Quantidade, big.mark = '.'),
         `(%)` = number(`(%)` * 100, accuracy = .1, suffix = '%'))


dados_fase_dia_tabela %>% write_feather("C:/Users/le_th/OneDrive/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Dashboard/www/data/dados_fase_dia_tabela.feather")

dados_br <- dados_acidentes %>%
  dplyr::group_by(uf, br) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::ungroup() %>% 
  rbind(dados_acidentes %>%
          dplyr::group_by(br) %>% 
          dplyr::summarise(Quantidade = n()) %>% 
          dplyr::ungroup() %>% 
          mutate(uf = 'Todos')
        )

dados_br %>% write_feather("C:/Users/le_th/OneDrive/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Dashboard/www/data/dados_br.feather")

# Boxplots
#Quantidade de Veículos ---------------------

jpeg('boxplot_veiculos.jpg')

dados_acidentes %>% 
  # select(veiculos) %>% 
  ggplot(aes(x = classificacao_acidente, 
             y = veiculos,
             fill = classificacao_acidente)) + 
  geom_boxplot() + 
  theme_minimal() + 
  labs(title = "Boxplot Quantidade de Veículos", 
       x = "Classificação do Acidente",
       y = "Quantidade de Veículos") +
  scale_fill_brewer("Classificação do Acidente", palette = "GnBu") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylim(0, 20)

dev.off()

#Quantidade de Pessoas ---------------------

jpeg('boxplot_pessoas.jpg')

dados_acidentes %>% 
  # select(veiculos) %>% 
  ggplot(aes(x = classificacao_acidente, 
             y = pessoas,
             fill = classificacao_acidente)) + 
  geom_boxplot() + 
  theme_minimal() + 
  ylim(0,20) + 
  labs(title = "Boxplot Quantidade de Pessoas", 
       x = "Classificação do Acidente",
       y = "Quantidade de Pessoas") +
  scale_fill_brewer("Classificação do Acidente", palette = "GnBu") + 
  theme(plot.title = element_text(hjust = 0.5)) 

dev.off()

# Clima

acidentes_condicao_met <- dados_acidentes %>% 
  mutate(condicao_metereologica = if_else(condicao_metereologica == "Ceu Claro", "Céu Claro", condicao_metereologica)) %>% 
  mutate(condicao_metereologica = if_else(condicao_metereologica == "Nevoeiro/neblina", "Nevoeiro/Neblina", condicao_metereologica)) %>% 
  mutate(condicao_metereologica = if_else(condicao_metereologica == "Ignorado", "Ignorada", condicao_metereologica)) %>% 
  mutate(condicao_metereologica = if_else(condicao_metereologica %in% c("(null)", ""), "Ignorada", condicao_metereologica)) %>% 
  dplyr::group_by(`Condição Metereológica` = condicao_metereologica) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::ungroup() %>% 
  arrange(desc((Quantidade))) %>% 
  mutate(prop = number(Quantidade / sum(Quantidade) * 100, accuracy = .1, suffix = "%"),
         Quantidade = number(Quantidade, big.mark = '.'))

acidentes_condicao_met %>% write_feather("C:/Users/le_th/OneDrive/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Dashboard/www/data/acidentes_condicao_met.feather")

acidentes_condicao_met %>% 
  kable() %>% 
  row_spec(c(1,4), background = "yellow") %>% 
  kable_styling() %>% 
  kable_styling(bootstrap_options = c("striped", "hover"))

dados_clima <- dados_acidentes %>% 
  mutate(condicao_metereologica = if_else(condicao_metereologica %in% c("Ceu Claro", "Céu Claro", "Sol"), "Céu Claro/Sol", condicao_metereologica)) %>% 
  mutate(condicao_metereologica = if_else(condicao_metereologica %in% c("Nublado", "Chuva"), "Nublado/Chuva", condicao_metereologica)) %>% 
  mutate(condicao_metereologica = if_else(!condicao_metereologica %in% c("Céu Claro/Sol", "Nublado/Chuva"), "Outros", condicao_metereologica)) %>% 
  dplyr::group_by(classificacao_acidente, `Condição Meteorológica` = condicao_metereologica) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::ungroup() %>% 
  arrange(classificacao_acidente, desc((Quantidade))) 

dados_clima %>% write_feather("C:/Users/le_th/OneDrive/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Dashboard/www/data/dados_clima.feather")

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


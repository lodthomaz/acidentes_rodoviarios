
# PACOTES --------------------
library(dplyr)
library(purrr)
library(readr)
library(scales)
library(knitr)
library(kableExtra)
library(formattable)
library(gdata)
library(readxl)
library(xtable)
library(lubridate)
library(feather)
library(tidyr)
library(fastDummies)
library(ggplot2)
library(janitor)

# FUNÇÕES --------------------
# Monta tabela resumo de Variáveis Numéricas
monta_tabela_descritiva <- function(base, variavel){
  tabela <- data.frame(Qtde = nrow(base),
                       `Média` = mean(base[[variavel]], na.rm = TRUE),
                       `Mín` = min(base[[variavel]], na.rm = TRUE),
                       `Máx` = max(base[[variavel]], na.rm = TRUE),
                       `1º Quartil` = quantile(base[[variavel]], probs = c(0.25)),
                       `Mediana` = median(base[[variavel]], na.rm = TRUE),
                       `3º Quartil` = quantile(base[[variavel]], probs = c(0.75)))
  
  return(tabela)
}

# Monta Gráfico Acidente por Dia da Semana 
cria_grafico_dia_semana <- function(tipo){
  
  df_acidentes_total_1 %>% 
    filter(classificacao_acidente == tipo) %>% 
    dplyr::group_by(`Dia da Semana` = dia_semana_padronizado, ano) %>% 
    dplyr::summarise(Qtde = n()) %>%
    dplyr::arrange(`Dia da Semana`) %>% 
    ggplot(aes(x = `Dia da Semana`, 
               y = Qtde)) + 
    geom_col() + 
    theme_minimal() + 
    labs(title = tipo, subtitle = "Qtde de Acidentes x Dia da Semana") + 
    theme(plot.title = element_text(hjust = 0.5))
  
}


# DADOS -----------------------
data_path <- "D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Dados"
files_path <- list.files(path = data_path, pattern = "datatran", full.names = TRUE)

df_acidentes_2011 <- read.csv2(files_path[1], header = TRUE, sep = ';', dec = ',', stringsAsFactors = FALSE)
df_acidentes_2012 <- read.csv2(files_path[2], header = TRUE, sep = ';', dec = ',', stringsAsFactors = FALSE)
df_acidentes_2013 <- read.csv2(files_path[3], header = TRUE, sep = ';', dec = ',', stringsAsFactors = FALSE)
df_acidentes_2014 <- read.csv2(files_path[4], header = TRUE, sep = ';', dec = ',', stringsAsFactors = FALSE)
df_acidentes_2015 <- read.csv2(files_path[5], header = TRUE, sep = ';', dec = ',', stringsAsFactors = FALSE)
df_acidentes_2016 <- read.csv2(files_path[6], header = TRUE, sep = ';', dec = ',', stringsAsFactors = FALSE)
df_acidentes_2017 <- read.csv2(files_path[7], header = TRUE, sep = ';', dec = ',', stringsAsFactors = FALSE)
df_acidentes_2018 <- read.csv2(files_path[8], header = TRUE, sep = ';', dec = ',', stringsAsFactors = FALSE)
df_acidentes_2019 <- read.csv2(files_path[9], header = TRUE, sep = ';', dec = ',', stringsAsFactors = FALSE)
df_acidentes_2020 <- read.csv2(files_path[10], header = TRUE, sep = ';', dec = ',', stringsAsFactors = FALSE)

# Junta os arquivos de 2011 a 2015 que estão no mesmo formato
df_acidentes_11a15 <- rbind(df_acidentes_2011,
                            df_acidentes_2012,
                            df_acidentes_2013,
                            df_acidentes_2014,
                            df_acidentes_2015) %>% 
  mutate(km = as.numeric(km))

# Cria a variável Ano para os demais data frames
df_acidentes_16 <- df_acidentes_2016 %>% 
  mutate(ano = 2016,
         br = as.character(br))

df_acidentes_17a20 <- rbind(df_acidentes_2017,
                            df_acidentes_2018,
                            df_acidentes_2019,
                            df_acidentes_2020) %>% 
  mutate(ano = year(data_inversa),
         br = as.character(br))

# PADRONIZAÇÃO  ---------------------------
# CAUSA DOS ACIDENTES
df_aux_causa_acidentes <- data.frame(
  causa_acidente = c("Ingestão de álcool e/ou substâncias psicoativas pelo pedestre", 
                     "Ingestão de Substâncias Psicoativas", 
                     "Ingestão de Álcool",
                     "Falta de atenção", 
                     "Falta de Atenção à Condução",
                     "Defeito mecânico em veículo", 
                     "Defeito Mecânico no Veículo",
                     "Dormindo", 
                     "Condutor Dormindo",
                     "Velocidade incompatível", 
                     "Velocidade Incompatível",
                     "Ultrapassagem Indevida",
                     "Ultrapassagem indevida", 
                     "Desobediência à sinalização",
                     "Desobediência às normas de trânsito pelo condutor",
                     "Falta de Atenção do Pedestre",
                     "Desobediência às normas de trânsito pelo pedestre",
                     "Defeito na via",
                     "Defeito na Via",
                     "Pista Escorregadia",
                     "Animais na Pista",
                     "Fenômenos da Natureza"
  ),
  causa_acidente_2 = c("Ingestão de álcool", 
                       "Ingestão de álcool", 
                       "Ingestão de álcool", 
                       "Falta de Atenção à Condução",
                       "Falta de Atenção à Condução",
                       "Defeito Mecânico no Veículo", 
                       "Defeito Mecânico no Veículo",
                       "Condutor Dormindo",
                       "Condutor Dormindo",
                       "Velocidade incompatível",
                       "Velocidade incompatível",
                       "Ultrapassagem Indevida",
                       "Ultrapassagem Indevida",
                       "Desobediência às normas de trânsito",
                       "Desobediência às normas de trânsito",
                       "Desobediência às normas ou Falta de Atenção do pedestre",
                       "Desobediência às normas ou Falta de Atenção do pedestre",
                       "Defeito na Via/Pista Escorregadia",
                       "Defeito na Via/Pista Escorregadia",
                       "Defeito na Via/Pista Escorregadia",
                       "Animais na Pista/Fenômenos da Natureza",
                       "Animais na Pista/Fenômenos da Natureza")
)

# TIPO DE ACIDENTE
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

# DIA DA SEMANA
de_para_dia_semana <- data.frame(
  dia_semana_n_padrao = c("Domingo", "domingo", "Segunda", "segunda-feira", "Terça", "terça-feira",
                          "Quarta", "quarta-feira", "Quinta", "quinta-feira", "Sexta", "sexta-feira",
                          "Sábado", "sábado"),
  dia_semana_padronizado = c("Domingo", "Domingo", "Segunda", "Segunda", "Terça", "Terça",
                             "Quarta", "Quarta", "Quinta", "Quinta", "Sexta", "Sexta",
                             "Sábado", "Sábado"))

# CONDICAO METEOROLÓGICA 
de_para_clima <- data.frame(
  condicao_metereologica = c("Ceu Claro", "Sol", "Nublado", "Chuva", "Ignorada", "Vento", "Nevoeiro/neblina",
                             "Neve", "Granizo", "(null)", "", "Garoa/Chuvisco", "Céu Claro", "Ignorado", "Nevoeiro/Neblina"),
  condicao_metereologica_padronizado = c("Céu Claro", "Sol", "Nublado", "Chuva", "Ignorada", "Vento", "Nevoeiro/Neblina",
                                         "Neve", "Granizo", "Ignorada", "Ignorada", "Garoa/Chuvisco", "Céu Claro", "Ignorada","Nevoeiro/Neblina")
)

# CONDICAO METEOROLÓGICA AGRUPADA
de_para_clima_agrupado <- data.frame(
  condicao_metereologica = c("Ceu Claro", "Sol", "Nublado", "Chuva", "Ignorada", "Vento", "Nevoeiro/neblina",
                             "Neve", "Granizo", "(null)", "", "Garoa/Chuvisco", "Céu Claro", "Ignorado", "Nevoeiro/Neblina"),
  condicao_metereologica_padronizado_agrupado = c("Céu Claro/Sol", "Céu Claro/Sol", "Nublado/Chuva", "Nublado/Chuva", "Outros", "Outros", "Outros",
                                                  "Outros", "Outros", "Outros", "Outros", "Outros", "Céu Claro/Sol", "Outros", "Outros")
)

# Junta todos os dataframes e cruza com o de para de causas, tipo de acidente e dia da semana 
df_acidentes_total_0 <- bind_rows(df_acidentes_11a15,
                                  df_acidentes_16,
                                  df_acidentes_17a20) %>% 
  left_join(df_aux_causa_acidentes) %>%
  mutate(causa_acidente_2 = as.character(causa_acidente_2),
         causa_acidente_2 = if_else(is.na(causa_acidente_2), causa_acidente, causa_acidente_2)) %>% 
  left_join(df_aux_tipo_acidente) %>% 
  mutate(tipo_acidente_simpl = as.character(tipo_acidente_simpl),
         tipo_acidente_simpl = if_else(is.na(tipo_acidente_simpl), tipo_acidente, tipo_acidente_simpl)) %>% 
  left_join(de_para_dia_semana, by = c("dia_semana" = "dia_semana_n_padrao")) %>% 
  left_join(de_para_clima) %>% 
  left_join(de_para_clima_agrupado)


df_acidentes_total_0$dia_semana_padronizado <- reorder.factor(df_acidentes_total_0$dia_semana_padronizado,
                                                              new.order=c("Segunda",
                                                                          "Terça",
                                                                          "Quarta",
                                                                          "Quinta",
                                                                          "Sexta",
                                                                          "Sábado",
                                                                          "Domingo"))

# Apaga os dfs que não serão mais utilizados
rm(df_acidentes_2011,
   df_acidentes_2012,
   df_acidentes_2013,
   df_acidentes_2014,
   df_acidentes_2015,
   df_acidentes_2016, 
   df_acidentes_2017, 
   df_acidentes_2018, 
   df_acidentes_2019, 
   df_acidentes_2020,
   df_acidentes_11a15,
   df_acidentes_16,
   df_acidentes_17a20)

# TRATAMENTO ---------------
# Variável Resposta Base Total 
df_acidentes_total_0 %>% 
  dplyr::group_by(`Classificação do Acidente` = classificacao_acidente) %>% 
  dplyr::summarise(Qtde = n())

# Exlcui missing e 'ignorado' da variável de interesse
df_acidentes_total_1 <- df_acidentes_total_0 %>% 
  filter(classificacao_acidente %in% c("Com Vítimas Fatais",
                                       "Com Vítimas Feridas",
                                       "Sem Vítimas")) %>% 
  dplyr::mutate(fase_dia_padronizada = if_else(fase_dia == "Plena Noite", "Plena noite", fase_dia),
                fase_dia_padronizada = if_else(fase_dia_padronizada %in% c("Amanhecer", "Anoitecer", "Plena noite", "Pleno dia"),
                                               fase_dia_padronizada, "Não informada"))


#########################################################################################
############################ ANÁLISE EXPLORATÓRIA #######################################
#########################################################################################

# CLASSIFICAÇÃO DOS ACIDENTES -----------------
total_obs <- nrow(df_acidentes_total_1)

tabela_classificacao_acidente <- df_acidentes_total_1 %>% 
  dplyr::group_by(`Classificação do Acidente` = classificacao_acidente) %>% 
  dplyr::summarise(Qtde = n()) %>% 
  dplyr::ungroup()

tabela_classificacao_acidente %>% write.csv2("D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/tabelas_brutas/tabela_classificacao_acidentes.csv")

# CAUSA DOS ACIDENTES -----------------
# GERAL
tabela_causas_geral <- df_acidentes_total_1 %>% 
  dplyr::group_by(`Causa do Acidente` = causa_acidente_2) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(Quantidade))

tabela_causas_geral %>% write.csv2("D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/tabelas_brutas/tabela_causas_acidentes_geral.csv")

# SEM VÍTIMAS
tabela_causas_sem_vitimas <- df_acidentes_total_1 %>% 
  dplyr::filter(classificacao_acidente == 'Sem Vítimas') %>% 
  dplyr::group_by(`Causa do Acidente` = causa_acidente_2) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(Quantidade))

tabela_causas_sem_vitimas %>% write.csv2("D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/tabelas_brutas/tabela_causas_acidentes_sem_vitimas.csv")

# COM VÍTIMAS FERIDAS
tabela_causas_com_vitimas_feridas <- df_acidentes_total_1 %>% 
  dplyr::filter(classificacao_acidente == 'Com Vítimas Feridas') %>% 
  dplyr::group_by(`Causa do Acidente` = causa_acidente_2) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(Quantidade))

tabela_causas_com_vitimas_feridas %>% write.csv2("D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/tabelas_brutas/tabela_causas_acidentes_com_vitimas_feridas.csv")

# COM VÍTIMAS FATAIS
tabela_causas_com_vitimas_fatais <- df_acidentes_total_1 %>% 
  dplyr::filter(classificacao_acidente == 'Com Vítimas Fatais') %>% 
  dplyr::group_by(`Causa do Acidente` = causa_acidente_2) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(Quantidade))

tabela_causas_com_vitimas_fatais %>% write.csv2("D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/tabelas_brutas/tabela_causas_acidentes_com_vitimas_fatais.csv")

# TIPO DE ACIDENTE ----------------------------
# GERAL 
tabela_tipo_acidente_geral <- df_acidentes_total_1 %>% 
  dplyr::group_by(`Tipo do Acidente` = tipo_acidente_simpl) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(Quantidade))

tabela_tipo_acidente_geral %>% write.csv2("D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/tabelas_brutas/tabela_tipo_acidente_geral.csv")

# SEM VÍTIMAS 
tabela_tipo_acidente_sem_vitimas <- df_acidentes_total_1 %>% 
  dplyr::filter(classificacao_acidente == 'Sem Vítimas') %>% 
  dplyr::group_by(`Tipo do Acidente` = tipo_acidente_simpl) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(Quantidade))

tabela_tipo_acidente_sem_vitimas %>% write.csv2("D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/tabelas_brutas/tabela_tipo_acidente_sem_vitimas.csv")

# COM VÍTIMAS FERIDAS
tabela_tipo_acidente_com_vitimas_feridas <- df_acidentes_total_1 %>% 
  dplyr::filter(classificacao_acidente == 'Com Vítimas Feridas') %>% 
  dplyr::group_by(`Tipo do Acidente` = tipo_acidente_simpl) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(Quantidade))

tabela_tipo_acidente_com_vitimas_feridas %>% write.csv2("D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/tabelas_brutas/tabela_tipo_acidente_com_vitimas_feridas.csv")

# COM VÍTIMAS FATAIS 
tabela_tipo_acidente_com_vitimas_fatais <- df_acidentes_total_1 %>% 
  dplyr::filter(classificacao_acidente == 'Com Vítimas Fatais') %>% 
  dplyr::group_by(`Tipo do Acidente` = tipo_acidente_simpl) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(Quantidade))

tabela_tipo_acidente_com_vitimas_fatais %>% write.csv2("D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/tabelas_brutas/tabela_tipo_acidente_com_vitimas_fatais.csv")

# Gráfico de Barras
# Com Vítimas Fatais
top_6_tipo_fatal <- df_acidentes_total_1 %>% 
  filter(classificacao_acidente == "Com Vítimas Fatais") %>% 
  filter(tipo_acidente_simpl %in% c("Colisão frontal","Atropelamento de Pedestre","Colisão traseira",
                                    "Colisão transversal","Saída de Pista","Colisão lateral")
  )

top_6_tipo_fatal %>% 
  dplyr::group_by(classificacao_acidente, tipo_acidente_simpl) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(Quantidade)) %>% 
  ggplot(aes(x = factor(tipo_acidente_simpl, levels = c("Colisão lateral", 
                                                        "Saída de Pista", "Colisão transversal",
                                                        "Colisão traseira", "Atropelamento de Pedestre",
                                                        "Colisão frontal")), 
             y = Quantidade,
             label =  number(Quantidade, big.mark = '.'))) + 
  geom_col(aes(
    fill =factor(tipo_acidente_simpl, levels = c("Colisão lateral", 
                                                 "Saída de Pista", "Colisão transversal",
                                                 "Colisão traseira", "Atropelamento de Pedestre",
                                                 "Colisão frontal")))) + 
  geom_label() +
  theme_minimal() + 
  scale_fill_brewer("Tipo de Acidente", palette = "GnBu") + 
  labs(title = "Quantidade por Tipo de Acidente",
       subtitle = "Vítimas Fatais", x = " ") + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

ggsave(
  "grafico_barras_tipo_acidente_vitimas_fatais.png",
  plot = last_plot(),
  device = "png",
  path = "D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/graficos/",
  width = 10, height = 7
)


# QUANTIDADE DE VEÍCULOS ---------------------
tabela_descritiva_veiculos <- monta_tabela_descritiva(base = df_acidentes_total_1, variavel = "veiculos")

tabela_descritiva_veiculos %>% write.csv2("D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/tabelas_brutas/tabela_medidas_resumo_veiculos.csv")

df_acidentes_total_1 %>% 
  ggplot(aes(x = classificacao_acidente, 
             y = veiculos,
             fill = classificacao_acidente)) + 
  geom_boxplot() + 
  theme_minimal() + 
  labs(title = "Boxplot Quantidade de Veículos", 
       x = "Classificação do Acidente",
       y = "Quantidade de Veículos") +
  scale_fill_brewer("Classificação do Acidente", palette = "GnBu") + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave(
  "grafico_boxplot_veiculos_por_classificacao_acidente.png",
  plot = last_plot(),
  device = "png",
  path = "D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/graficos/"
)

# QUANTIDADE DE PESSOAS ---------------------

tabela_descritiva_pessoas <- monta_tabela_descritiva(base = df_acidentes_total_1, variavel = "pessoas")

tabela_descritiva_pessoas %>% write.csv2("D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/tabelas_brutas/tabela_medidas_resumo_pessoas.csv")

df_acidentes_total_1 %>% 
  ggplot(aes(x = classificacao_acidente, 
             y = pessoas,
             fill = classificacao_acidente)) + 
  geom_boxplot() + 
  theme_minimal() + 
  labs(title = "Boxplot Quantidade de Pessoas", 
       x = "Classificação do Acidente",
       y = "Quantidade de Pessoas") +
  scale_fill_brewer("Classificação do Acidente", palette = "GnBu") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylim(0,50)

ggsave(
  "grafico_boxplot_pessoas_por_classificacao_acidente.png",
  plot = last_plot(),
  device = "png",
  path = "D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/graficos/"
)

# QUANTIDADE DE MORTOS ---------------------

tabela_descritiva_mortos <- monta_tabela_descritiva(base = df_acidentes_total_1, variavel = "mortos")

tabela_descritiva_mortos %>% write.csv2("D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/tabelas_brutas/tabela_medidas_resumo_mortos.csv")

# QUANTIDADE DE ILESOS ---------------------

tabela_descritiva_ilesos <- monta_tabela_descritiva(base = df_acidentes_total_1, variavel = "ilesos")

tabela_descritiva_ilesos %>% write.csv2("D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/tabelas_brutas/tabela_medidas_resumo_ilesos.csv")

# ANO DO ACIDENTE --------------------------

df_acidentes_total_1 %>% 
  dplyr::group_by(classificacao_acidente, `Ano do Acidente` = ano) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::ungroup() %>% 
  ggplot(aes(x = as.factor(`Ano do Acidente`), 
             y = Quantidade, 
             group = classificacao_acidente,
             color = classificacao_acidente)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() +
  scale_color_discrete("Classificação do Acidente") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") + 
  labs(title = "Quantidade de Acidentes por Ano", x = "Ano do Acidente")

ggsave(
  "grafico_linha_quantidade_acidentes_ano.png",
  plot = last_plot(),
  device = "png",
  path = "D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/graficos/"
)

# DIA DA SEMANA ----------------------------
# GERAL
tabela_dia_semana_geral <- df_acidentes_total_1 %>% 
  dplyr::group_by(`Dia da Semana` = dia_semana_padronizado, ano) %>% 
  dplyr::summarise(Qtde = n()) %>%
  dplyr::arrange(`Dia da Semana`) %>% 
  tidyr::spread(key = ano, value = Qtde) 

tabela_dia_semana_geral %>% write.csv2("D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/tabelas_brutas/tabela_dia_semana_geral.csv")

# COM VÍTIMAS FATAIS 
tabela_dia_semana_vitimas_fatais <- df_acidentes_total_1 %>% 
  filter(classificacao_acidente == "Com Vítimas Fatais") %>% 
  dplyr::group_by(`Dia da Semana` = dia_semana_padronizado, ano) %>% 
  dplyr::summarise(Qtde = n()) %>%
  dplyr::arrange(`Dia da Semana`) %>% 
  tidyr::spread(key = ano, value = Qtde)

tabela_dia_semana_vitimas_fatais %>% write.csv2("D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/tabelas_brutas/tabela_dia_semana_vitimas_fatais.csv")

cria_grafico_dia_semana("Com Vítimas Fatais")

ggsave(
  "grafico_barras_quantidade_acidentes_semana_vitimas_fatais.png",
  plot = last_plot(),
  device = "png",
  path = "D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/graficos/"
)

# SEM VÍTIMAS
tabela_dia_semana_sem_vitimas <- df_acidentes_total_1 %>% 
  filter(classificacao_acidente == "Sem Vítimas") %>% 
  dplyr::group_by(`Dia da Semana` = dia_semana_padronizado, ano) %>% 
  dplyr::summarise(Qtde = n()) %>%
  dplyr::arrange(`Dia da Semana`) %>% 
  tidyr::spread(key = ano, value = Qtde)

tabela_dia_semana_sem_vitimas %>% write.csv2("D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/tabelas_brutas/tabela_dia_semana_sem_vitimas.csv")

cria_grafico_dia_semana("Sem Vítimas")

ggsave(
  "grafico_barras_quantidade_acidentes_semana_sem_vitimas.png",
  plot = last_plot(),
  device = "png",
  path = "D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/graficos/"
)

# COM VÍTIMAS FERIDAS 
tabela_dia_semana_vitimas_feridas <- df_acidentes_total_1 %>% 
  filter(classificacao_acidente == "Com Vítimas Feridas") %>% 
  dplyr::group_by(`Dia da Semana` = dia_semana_padronizado, ano) %>% 
  dplyr::summarise(Qtde = n()) %>%
  dplyr::arrange(`Dia da Semana`) %>% 
  tidyr::spread(key = ano, value = Qtde)

tabela_dia_semana_vitimas_feridas %>% write.csv2("D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/tabelas_brutas/tabela_dia_semana_com_vitimas_feridas.csv")

cria_grafico_dia_semana("Com Vítimas Feridas")

ggsave(
  "grafico_barras_quantidade_acidentes_semana_vitimas_feridas.png",
  plot = last_plot(),
  device = "png",
  path = "D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/graficos/"
)

# FASE DIA -----------------
tabela_fase_dia <- df_acidentes_total_1 %>% 
  dplyr::group_by(`Fase do Dia` = fase_dia_padronizada) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::ungroup() %>% 
  arrange(desc((Quantidade))) 

tabela_fase_dia %>% write.csv2("D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/tabelas_brutas/tabela_fase_dia_sem_classificacao_acidente.csv")

tabela_fase_dia_acidentes <- df_acidentes_total_1 %>% 
  dplyr::group_by(`Fase do Dia` = fase_dia_padronizada, classificacao_acidente) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::ungroup() %>% 
  arrange(desc((Quantidade))) 

tabela_fase_dia_acidentes %>% write.csv2("D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/tabelas_brutas/tabela_fase_dia_com_classificacao_acidente.csv")

df_acidentes_total_1 %>% 
  dplyr::filter(fase_dia_padronizada %in% c("Amanhecer", "Anoitecer", "Plena noite", "Pleno dia")) %>% 
  dplyr::group_by(classificacao_acidente, `Fase do Dia` = fase_dia_padronizada) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::mutate(prop_text = number((Quantidade / sum(Quantidade)) * 100, accuracy = 1, suffix = '%'),
                prop = (Quantidade / sum(Quantidade)) * 100) %>% 
  dplyr::ungroup() %>% 
  ggplot(aes(x = classificacao_acidente,
             y = prop, 
             group = `Fase do Dia`)) + 
  geom_col(aes(fill = `Fase do Dia`), position = 'fill') + 
  geom_text(aes(label = prop_text), position = position_fill(vjust = .5)) +
  theme_minimal() + 
  scale_fill_brewer("Fase do Dia", palette = "GnBu") + 
  labs(title = "Proporção por Fase do Dia", x = "Classificação do Acidente", y = "Proporção") + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave(
  "grafico_barras_fase_dia_por_acidente.png",
  plot = last_plot(),
  device = "png",
  path = "D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/graficos/"
)

# UF -----
# GERAL 
tabela_acidentes_uf <- df_acidentes_total_1 %>% 
  dplyr::group_by(`UF` = uf) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(Quantidade))

tabela_acidentes_uf %>% write.csv2("D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/tabelas_brutas/tabela_acidentes_uf_geral.csv")

# VÍTIMAS FATAIS
tabela_acidentes_uf_fatal <- df_acidentes_total_1 %>% 
  dplyr::filter(classificacao_acidente == "Com Vítimas Fatais") %>% 
  dplyr::group_by(`UF` = uf) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(Quantidade))

tabela_acidentes_uf_fatal %>% write.csv2("D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/tabelas_brutas/tabela_acidentes_uf_vitimas_fatais.csv")

# SEM VÍTIMAS
tabela_acidentes_uf_sem_vit <- df_acidentes_total_1 %>% 
  dplyr::filter(classificacao_acidente == "Sem Vítimas") %>% 
  dplyr::group_by(`UF` = uf) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(Quantidade))

tabela_acidentes_uf_sem_vit %>% write.csv2("D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/tabelas_brutas/tabela_acidentes_uf_sem_vitimas.csv")

# VÍTIMAS FERIDAS
tabela_acidentes_uf_com_vit <- df_acidentes_total_1 %>% 
  dplyr::filter(classificacao_acidente == "Com Vítimas Feridas") %>% 
  dplyr::group_by(`UF` = uf) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(Quantidade))

tabela_acidentes_uf_com_vit %>% write.csv2("D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/tabelas_brutas/tabela_acidentes_uf_vitimas_feridas.csv")

# PRINCIPAIS RODOVIAS UF'S COM MAIS ACIDENTES
tabela_acidentes_uf_br <- df_acidentes_total_1 %>% 
  dplyr::filter(uf %in% c("MG", "PR", "SC")) %>% 
  dplyr::group_by(`UF` = uf, br) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(`UF`, desc(Quantidade))

tabela_acidentes_uf_br %>% write.csv2("D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/tabelas_brutas/tabela_rodovias_uf_mais_acidentes.csv")

# TIPO DE PISTA -----------------------------
tabela_acidentes_tipo_pista <- df_acidentes_total_1 %>% 
  dplyr::group_by(`Tipo da Pista` = tipo_pista) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::ungroup() %>% 
  arrange(desc((Quantidade)))

tabela_acidentes_tipo_pista %>% write.csv2("D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/tabelas_brutas/tabela_acidentes_tipo_pista.csv")

df_acidentes_total_1 %>% 
  dplyr::filter(!tipo_pista == "(null)") %>% 
  dplyr::group_by(classificacao_acidente, tipo_pista) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::mutate(prop_text = number((Quantidade / sum(Quantidade)) * 100, accuracy = 1, suffix = '%'),
                prop = (Quantidade / sum(Quantidade)) * 100) %>% 
  dplyr::ungroup() %>% 
  ggplot(aes(x = classificacao_acidente,
             y = prop, 
             group = tipo_pista)) + 
  geom_col(aes(fill = tipo_pista), position = 'fill') + 
  geom_text(aes(label = prop_text), position = position_fill(vjust = .5)) +
  theme_minimal() + 
  scale_fill_brewer("Tipo de Pista", palette = "GnBu") + 
  labs(title = "Proporção por Tipo de Pista", 
       x = "Classificação do Acidente", 
       y = "Proporção") + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave(
  "grafico_barras_tipo_pista_por_acidente.png",
  plot = last_plot(),
  device = "png",
  path = "D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/graficos/"
)

# TRAÇADO VIA ----------------------------
tabela_acidentes_tracado_via <- df_acidentes_total_1 %>% 
  dplyr::group_by(`Traçado da Via` = tracado_via) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::ungroup() %>% 
  arrange(desc((Quantidade))) 

tabela_acidentes_tracado_via %>% write.csv2("D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/tabelas_brutas/tabela_acidentes_tracado_via.csv")

df_acidentes_total_1 %>% 
  dplyr::filter(tracado_via %in% c("Reta", "Curva", "Cruzamento")) %>% 
  dplyr::group_by(classificacao_acidente, tracado_via) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::ungroup() %>% 
  ggplot(aes(x = classificacao_acidente,
             y = Quantidade, 
             group = tracado_via)) + 
  geom_col(aes(fill = tracado_via), position = 'fill') + 
  # geom_text(aes(label = prop_text), position = position_fill(vjust = .5)) +
  theme_minimal() + 
  scale_fill_brewer("Traçado da Via", palette = "GnBu") + 
  labs(title = "Proporção por Traçado da Via", 
       x = "Classificação do Acidente", 
       y = "Proporção") + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave(
  "grafico_barras_tracado_via_por_acidente.png",
  plot = last_plot(),
  device = "png",
  path = "D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/graficos/"
)

# CONDIÇÃO METEOROLÓGICA ---------------------

tabela_acidentes_condicao_met_sem_acidentes <- df_acidentes_total_1 %>% 
  dplyr::group_by(`Condição Metereológica` = condicao_metereologica_padronizado) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::ungroup() %>% 
  arrange(desc((Quantidade)))

tabela_acidentes_condicao_met_sem_acidentes %>% write.csv2("D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/tabelas_brutas/tabela_acidentes_condicao_met_sem_acidentes.csv")

tabela_acidentes_condicao_met_com_acidentes <- df_acidentes_total_1 %>% 
  dplyr::group_by(classificacao_acidente, `Condição Metereológica` = condicao_metereologica_padronizado) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::ungroup() %>% 
  arrange(classificacao_acidente, desc((Quantidade)))

tabela_acidentes_condicao_met_com_acidentes %>% write.csv2("D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/tabelas_brutas/tabela_acidentes_condicao_met_com_acidentes.csv")

df_acidentes_total_1 %>% 
  dplyr::group_by(classificacao_acidente, `Condição Meteorológica` = condicao_metereologica_padronizado_agrupado) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::ungroup() %>% 
  arrange(classificacao_acidente, desc((Quantidade))) %>% 
  ggplot(aes(x = `Condição Meteorológica`,
             y = Quantidade, 
             group = classificacao_acidente)) + 
  geom_col() + 
  facet_wrap(~classificacao_acidente) + 
  theme_minimal()

ggsave(
  "grafico_barras_condicao_meteorologica_acidente.png",
  plot = last_plot(),
  device = "png",
  path = "D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/graficos/",
  width = 10, height = 5
)

# USO SOLO -----------------
tabela_acidentes_uso_solo <- df_acidentes_total_1 %>% 
  dplyr::group_by(`Solo` = uso_solo) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::ungroup() %>% 
  arrange(desc((Quantidade))) 


# SENTIDO DA VIA ---------------------

tabela_acidentes_sentido_via <- df_acidentes_total_1 %>% 
  dplyr::group_by(classificacao_acidente, sentido_via) %>% 
  dplyr::summarise(Quantidade = n()) %>% 
  dplyr::ungroup() %>% 
  arrange(classificacao_acidente, desc((Quantidade))) 


#########################################################################################
################################ CRIAÇÃO DA ABT #########################################
#########################################################################################

# Principais UFs
uf_top_11 <- c("MG","PR","SC","RJ","RS","SP","BA","GO","ES","PE","MT")

# Principais Acidentes 
vetor_tipo_acidente <- c(
  "Colisão frontal",
  "Atropelamento de Pedestre",
  "Colisão traseira",
  "Colisão transversal",
  "Saída de Pista",
  "Colisão lateral",
  "Queda de motocicleta / bicicleta / veículo",
  "Capotamento",
  "Colisão com objeto fixo","Tombamento"
)

# Criando dummies 

abt_modelagem <- df_acidentes_total_1 %>% 
  mutate(uf_simpl = if_else(uf %in% uf_top_11, uf, "Outros"),
         tipo_acidente_resumo = if_else(tipo_acidente_simpl %in% vetor_tipo_acidente, tipo_acidente_simpl, "Outros")) %>% 
  dummy_cols('dia_semana_padronizado') %>% 
  dummy_cols('uf_simpl') %>% 
  dummy_cols('causa_acidente_2') %>% 
  dummy_cols('tipo_acidente_resumo') %>% 
  dummy_cols('condicao_metereologica_padronizado_agrupado') %>% 
  dummy_cols('fase_dia_padronizada') %>% 
  dummy_cols('sentido_via') %>% 
  dummy_cols('tipo_pista') %>% 
  dummy_cols('tracado_via') %>% 
  dummy_cols('uso_solo') %>% 
  # dummy_cols('ano') %>% 
  select(everything(), -c(id, data_inversa, dia_semana, horario, uf, br, km, municipio, 
                          causa_acidente, tipo_acidente, fase_dia, sentido_via, 
                          condicao_metereologica, condicao_metereologica_padronizado_agrupado, 
                          tipo_pista, `tipo_pista_(null)`, tracado_via, `tracado_via_(null)`, 
                          uso_solo, `uso_solo_(null)`, ano, pessoas, mortos, feridos, ilesos, ignorados, 
                          feridos_leves, feridos_graves, veiculos, latitude, longitude, regional, uop, 
                          dia_semana_padronizado, causa_acidente_2,tipo_acidente_simpl, 
                          tipo_acidente_resumo, delegacia, fase_dia_padronizada, condicao_metereologica_padronizado,
                          uf_simpl, `causa_acidente_2_(null)`)) %>% 
  clean_names()

abt_modelagem %>% write.csv2("D:/Documentos/Leticia/FIA/Pós - Análise em Big Data/TCC/Entrega 3/abt/abt_modelagem.csv", 
                                    fileEncoding = "UTF-8")





rm(list = ls())
library(sidrar)
library(dplyr)
library(gtsummary)
library(stringr)
library(ggplot2)
library(tidyr)
library(knitr)
library(scales)


# ...existing code...
info_sidra(1209)

# Código do Pará é 15
cod_uf_para <- 15

# Buscar dados de 2022 para o Pará
para_idade_2022 <-   get_sidra(
    x = 9514,
    period = "2022", # anos do censo
    geo = "State",
    geo.filter = list(State = cod_uf_para)
  )

para_idade2022_filtrado <- para_idade_2022 |> 
  select(-"Nível Territorial", -"Unidade de Medida")

# Olhar os dados da primeira tabela como exemplo
str(para_idade2022_filtrado)

info_sidra(3145)

# Buscar dados de 2010 para o Pará
para_idade_2010 <-   get_sidra(
    x = 1378,
    period = "2010", # ano do censo
    geo = "State",
    geo.filter = list(State = cod_uf_para)
  )
para_idade2010_filtrado <- para_idade_2010 |> 
  select(-"Nível Territorial", -"Unidade de Medida") |> 
  filter(grepl("Total", `Condição no domicílio e o compartilhamento da responsabilidade pelo domicílio`))

pop_total_2022 <- para_idade2022_filtrado |> 
  filter(Sexo == "Total" &
    Idade == "Total" & 
    `Forma de declaração da idade` == "Total") |> 
  select(Valor)
print(pop_total_2022)

pop_total_2010 <- para_idade2010_filtrado |> 
  filter(Sexo == "Total" & 
    Idade == "Total" & 
    `Situação do domicílio` == "Total" & 
    `Condição no domicílio e o compartilhamento da responsabilidade pelo domicílio` == "Total") |> 
  select(Valor)
print(pop_total_2010)

pop_hm_2010 <- para_idade2010_filtrado |> 
  filter(Sexo %in% c("Homens", "Mulheres") &
    Idade == "Total" &
    `Situação do domicílio` == "Total" &
    `Condição no domicílio e o compartilhamento da responsabilidade pelo domicílio` == "Total") |> 
  select(Valor, Sexo)
print(pop_hm_2010)
razao_sexo2010 <- (pop_hm_2010[1, "Valor"]/pop_hm_2010[2, "Valor"])*100
print(razao_sexo2010)

pop_hm_2022 <- para_idade2022_filtrado |> 
  filter(Sexo %in% c("Homens", "Mulheres") &
    Idade == "Total" & 
    `Forma de declaração da idade` == "Total") |> 
  select(Valor, Sexo)
print(pop_hm_2022)
razao_sexo2022 <- (pop_hm_2022[1, "Valor"]/pop_hm_2022[2, "Valor"])*100
print(razao_sexo2022)

para_idade2022_simples <- para_idade2022_filtrado |> 
  filter(grepl("^[0-9]+ anos?$", Idade)) |>   # pega "1 ano", "2 anos", ..., "99 anos"
  mutate(Idade = as.numeric(sub(" anos?$", "", Idade))) |> 
  filter(Sexo %in% c("Homens", "Mulheres"), `Forma de declaração da idade` == "Total")
digito_final <- para_idade2022_simples$Idade %% 10

# Agrupar por idade e somar valores (Homens + Mulheres)
pop_idade <- para_idade2022_simples %>%
  group_by(Idade) %>%
  summarise(Pop = sum(Valor), .groups = "drop")

# Calcular dígito final da idade
pop_idade <- pop_idade %>%
  mutate(digito_final = Idade %% 10)

# Frequência por dígito final
pop_por_digito <- tapply(pop_idade$Pop, pop_idade$digito_final, sum)

# Proporção f_i
f_i <- pop_por_digito / sum(pop_por_digito)

# Índice de Myers (0 a 180)
indice_myers <- 100 * sum(abs(f_i - 1/10))
# Resultados
print(round(f_i * 100, 2)) # Distribuição %
cat("\nÍndice de Myers:", round(indice_myers, 2), "\n")



para_idade2022_simples <- para_idade2022_filtrado |> 
  filter(grepl("^[0-9]+ anos?$", Idade) | Idade %in% c("100 anos ou mais", "Menos de 1 ano")) |> 
  mutate(
    Idade = case_when(
      Idade == "Menos de 1 ano" ~ 0,
      Idade == "100 anos ou mais" ~ 100, # Para a mediana, vamos usar 100 para essa faixa
      TRUE ~ as.numeric(str_extract(Idade, "^[0-9]+"))
    )
  ) |>
  filter(Sexo %in% c("Homens", "Mulheres"), `Forma de declaração da idade` == "Total") |> 
  select(Ano = Ano, Sexo, Idade, Populacao = Valor)

idade_mediana_para_2022 <- para_idade2022_simples |> 
  group_by(Ano, Sexo) |> 
  arrange(Idade) |> # Garantir que as idades estão em ordem crescente
  mutate(
    populacao_acumulada = cumsum(Populacao),
    populacao_total = sum(Populacao)
  ) |> 
  filter(populacao_acumulada >= populacao_total / 2) |> 
  summarise(
    idade_mediana = first(Idade),
    populacao_total = first(populacao_total)
  ) |> 
  ungroup()

print("Idade Mediana por Sexo (Pará - Censo 2022):")
print(idade_mediana_para_2022)

# O dataframe `para_idade2022_simples` já está pronto com as colunas:
# Ano, Sexo, Idade, Populacao

razao_dependencia_para_2022 <- para_idade2022_simples |> 
  # Mapear as idades para as categorias de dependência
  mutate(
    categoria_idade = case_when(
      Idade >= 0 & Idade <= 14 ~ "Jovem",
      Idade >= 15 & Idade <= 64 ~ "Ativo",
      Idade >= 65 ~ "Idoso"
    )
  ) |> 
  
  # Agrupar os dados por ano e sexo
  group_by(Ano, Sexo) |> 
  
  # Calcular a população total em cada categoria de idade
  summarise(
    pop_jovem = sum(Populacao[categoria_idade == "Jovem"], na.rm = TRUE),
    pop_ativa = sum(Populacao[categoria_idade == "Ativo"], na.rm = TRUE),
    pop_idosa = sum(Populacao[categoria_idade == "Idoso"], na.rm = TRUE),
    .groups = "drop"
  ) |> 
  
  # Calcular as razões de dependência
  mutate(
    # Razão de Dependência Jovem
    razao_dep_jovem = (pop_jovem / pop_ativa) * 100,
    
    # Razão de Dependência Idosa
    razao_dep_idosa = (pop_idosa / pop_ativa) * 100,
    
    # Razão de Dependência Total
    razao_dep_total = ((pop_jovem + pop_idosa) / pop_ativa) * 100
  )

print("Razão de Dependência por Sexo (Pará - Censo 2022):")
print(razao_dependencia_para_2022)

indice_envelhecimento_para_2022 <- para_idade2022_simples |> 
  # Mapear as idades para as categorias de envelhecimento
  mutate(
    categoria_idade = case_when(
      Idade >= 0 & Idade <= 14 ~ "Jovem",
      Idade >= 65 ~ "Idoso",
      TRUE ~ "Outros" # Categorizar o restante como "Outros"
    )
  ) |> 
  
  # Agrupar por ano e sexo
  group_by(Ano, Sexo) |> 
  
  # Calcular a população em cada categoria
  summarise(
    pop_jovem = sum(Populacao[categoria_idade == "Jovem"], na.rm = TRUE),
    pop_idosa = sum(Populacao[categoria_idade == "Idoso"], na.rm = TRUE),
    .groups = "drop"
  ) |> 
  
  # Calcular o índice de envelhecimento
  mutate(
    indice_envelhecimento = (pop_idosa / pop_jovem) * 100
  )

print("Índice de Envelhecimento por Sexo (Pará - Censo 2022):")
print(indice_envelhecimento_para_2022)

# Vamos criar faixas etárias de 5 em 5 anos
piramide_data <- para_idade2022_simples |> 
  mutate(
    faixa_etaria = cut(
      x = Idade,
      breaks = c(seq(0, 100, by = 5), Inf),
      right = FALSE, # A faixa começa em [0, 5)
      labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                 "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                 "60-64", "65-69", "70-74", "75-79", "80-84", "85-89",
                 "90-94", "95-99", "100+"),
      ordered_result = TRUE
    )
  ) |> 
  group_by(faixa_etaria, Sexo) |> 
  summarise(
    Populacao = sum(Populacao, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  # Transformar a população masculina para valores negativos para a pirâmide
  mutate(
    Populacao = if_else(Sexo == "Homens", -Populacao, Populacao)
  )

# Criar a Pirâmide Etária
ggplot(piramide_data, aes(x = faixa_etaria, y = Populacao, fill = Sexo)) +
  geom_bar(stat = "identity", position = "identity") +
  scale_y_continuous(
    labels = function(x) { scales::comma(abs(x), big.mark = ".", decimal.mark = ",") },
    name = "População"
  ) +
  coord_flip() + # Inverte os eixos para que a pirâmide seja horizontal
  labs(
    title = "Pirâmide Etária do Pará (Censo 2022)",
    subtitle = "População por sexo e faixas etárias de 5 anos",
    x = "Faixa Etária",
    fill = "Sexo"
  ) +
  scale_fill_manual(values = c("Homens" = "#ADD8E6", "Mulheres" = "#FFDAB9")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

## CENSO DE 2010 --------------------------------------------------------------------------------

para_idade2010_simples <- para_idade2010_filtrado |> 
  filter(grepl("^[0-9]+ anos?$", Idade)) |>   # pega "1 ano", "2 anos", ..., "99 anos"
  mutate(Idade = as.numeric(sub(" anos?$", "", Idade))) |> 
  filter(Sexo %in% c("Homens", "Mulheres"), `Situação do domicílio` == "Total", `Condição no domicílio e o compartilhamento da responsabilidade pelo domicílio` == "Total")
digito_final_2010 <- para_idade2010_simples$Idade %% 10

# Agrupar por idade e somar valores (Homens + Mulheres)
pop_idade_2010 <- para_idade2010_simples %>%
  group_by(Idade) %>%
  summarise(Pop = sum(Valor), .groups = "drop")

# Calcular dígito final da idade
pop_idade_2010 <- pop_idade_2010 %>%
  mutate(digito_final_2010 = Idade %% 10)

# Frequência por dígito final
pop_por_digito_2010 <- tapply(pop_idade_2010$Pop, pop_idade_2010$digito_final_2010, sum)

# Proporção f_i
f_i <- pop_por_digito_2010 / sum(pop_por_digito_2010)

# Índice de Myers (0 a 180)
indice_myers_2010 <- 100 * sum(abs(f_i - 1/10))
# Resultados
print(round(f_i * 100, 2)) # Distribuição %
cat("\nÍndice de Myers:", round(indice_myers_2010, 2), "\n")

para_idade2010_simples <- para_idade2010_filtrado |> 
  filter(grepl("^[0-9]+ anos?$", Idade) | Idade %in% c("100 anos ou mais", "Menos de 1 ano")) |> 
  mutate(
    Idade = case_when(
      Idade == "Menos de 1 ano" ~ 0,
      Idade == "100 anos ou mais" ~ 100, # Para a mediana, vamos usar 100 para essa faixa
      TRUE ~ as.numeric(str_extract(Idade, "^[0-9]+"))
    )
  ) |>
  filter(Sexo %in% c("Homens", "Mulheres"), `Situação do domicílio` == "Total", `Condição no domicílio e o compartilhamento da responsabilidade pelo domicílio` == "Total") |> 
  select(Ano = Ano, Sexo, Idade, Populacao = Valor)

idade_mediana_para_2010 <- para_idade2010_simples |> 
  group_by(Ano, Sexo) |> 
  arrange(Idade) |> # Garantir que as idades estão em ordem crescente
  mutate(
    populacao_acumulada = cumsum(Populacao),
    populacao_total = sum(Populacao)
  ) |> 
  filter(populacao_acumulada >= populacao_total / 2) |> 
  summarise(
    idade_mediana = first(Idade),
    populacao_total = first(populacao_total)
  ) |> 
  ungroup()

print("Idade Mediana por Sexo (Pará - Censo 2010):")
print(idade_mediana_para_2010)

# O dataframe `para_idade2010_simples` já está pronto com as colunas:
# Ano, Sexo, Idade, Populacao

razao_dependencia_para_2010 <- para_idade2010_simples |> 
  # Mapear as idades para as categorias de dependência
  mutate(
    categoria_idade = case_when(
      Idade >= 0 & Idade <= 14 ~ "Jovem",
      Idade >= 15 & Idade <= 64 ~ "Ativo",
      Idade >= 65 ~ "Idoso"
    )
  ) |> 
  
  # Agrupar os dados por ano e sexo
  group_by(Ano, Sexo) |> 
  
  # Calcular a população total em cada categoria de idade
  summarise(
    pop_jovem = sum(Populacao[categoria_idade == "Jovem"], na.rm = TRUE),
    pop_ativa = sum(Populacao[categoria_idade == "Ativo"], na.rm = TRUE),
    pop_idosa = sum(Populacao[categoria_idade == "Idoso"], na.rm = TRUE),
    .groups = "drop"
  ) |> 
  
  # Calcular as razões de dependência
  mutate(
    # Razão de Dependência Jovem
    razao_dep_jovem = (pop_jovem / pop_ativa) * 100,
    
    # Razão de Dependência Idosa
    razao_dep_idosa = (pop_idosa / pop_ativa) * 100,
    
    # Razão de Dependência Total
    razao_dep_total = ((pop_jovem + pop_idosa) / pop_ativa) * 100
  )

print("Razão de Dependência por Sexo (Pará - Censo 2010):")
print(razao_dependencia_para_2010)

indice_envelhecimento_para_2010 <- para_idade2010_simples |> 
  # Mapear as idades para as categorias de envelhecimento
  mutate(
    categoria_idade = case_when(
      Idade >= 0 & Idade <= 14 ~ "Jovem",
      Idade >= 65 ~ "Idoso",
      TRUE ~ "Outros" # Categorizar o restante como "Outros"
    )
  ) |>
  
  # Agrupar por ano e sexo
  group_by(Ano, Sexo) |> 
  
  # Calcular a população em cada categoria
  summarise(
    pop_jovem = sum(Populacao[categoria_idade == "Jovem"], na.rm = TRUE),
    pop_idosa = sum(Populacao[categoria_idade == "Idoso"], na.rm = TRUE),
    .groups = "drop"
  ) |> 
  
  # Calcular o índice de envelhecimento
  mutate(
    indice_envelhecimento = (pop_idosa / pop_jovem) * 100
  )

print("Índice de Envelhecimento por Sexo (Pará - Censo 2010):")
print(indice_envelhecimento_para_2010)

# Vamos criar faixas etárias de 5 em 5 anos
piramide_data <- para_idade2010_simples |> 
  mutate(
    faixa_etaria = cut(
      x = Idade,
      breaks = c(seq(0, 100, by = 5), Inf),
      right = FALSE, # A faixa começa em [0, 5)
      labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                 "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                 "60-64", "65-69", "70-74", "75-79", "80-84", "85-89",
                 "90-94", "95-99", "100+"),
      ordered_result = TRUE
    )
  ) |> 
  group_by(faixa_etaria, Sexo) |> 
  summarise(
    Populacao = sum(Populacao, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  # Transformar a população masculina para valores negativos para a pirâmide
  mutate(
    Populacao = if_else(Sexo == "Homens", -Populacao, Populacao)
  )

# Criar a Pirâmide Etária
ggplot(piramide_data, aes(x = faixa_etaria, y = Populacao, fill = Sexo)) +
  geom_bar(stat = "identity", position = "identity") +
  scale_y_continuous(
    labels = function(x) { scales::comma(abs(x), big.mark = ".", decimal.mark = ",") },
    name = "População"
  ) +
  coord_flip() + # Inverte os eixos para que a pirâmide seja horizontal
  labs(
    title = "Pirâmide Etária do Pará (Censo 2010)",
    subtitle = "População por sexo e faixas etárias de 5 anos",
    x = "Faixa Etária",
    fill = "Sexo"
  ) +
  scale_fill_manual(values = c("Homens" = "#ADD8E6", "Mulheres" = "#FFDAB9")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# --- Seção de Geração e Salvamento de Tabelas ---


# --- Criação do dataframe para o gráfico ---
dados_razao_sexo <- tibble(
  Ano = c("Censo 2010", "Censo 2022"),
  Razao_Sexo = c(razao_sexo2010, razao_sexo2022)
)

# --- Gerando o gráfico ---
ggplot(dados_razao_sexo, aes(x = Ano, y = Razao_Sexo)) +
  geom_bar(stat = "identity", fill = "#ADD8E6", width = 0.5) +
  geom_text(aes(label = sprintf("%.2f", Razao_Sexo)), vjust = -0.5, size = 5) + # Adiciona os valores nas barras
  labs(
    title = "Razão de Sexo no Pará: Comparativo 2010 vs 2022",
    subtitle = "Número de homens para cada 100 mulheres",
    x = "Ano do Censo",
    y = "Razão de Sexo"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )


#tabela 1

library(flextable)
library(dplyr)

# Dados para a tabela
tabela_comparativa <- data.frame(
  Indicador = c("População Total", 
                "Razão de Sexo (homens/100 mulheres)", 
                "Idade Mediana - Homens", 
                "Idade Mediana - Mulheres", 
                "Índice de Envelhecimento - Homens",
                "Índice de Envelhecimento - Mulheres", 
                "Razão de Dependência Total",
                "Índice de Myers"),
  `2010` = c(7581051, 101.7, 24, 24, 14.4, 16.2, 56.0, 4.42),
  `2022` = c(8120131, 99.6, 29, 30, 27.5, 31.8, 46.6, 3.38),
  Variação = c("+7,1%", "-2,1", "+5 anos", "+6 anos", "+13,1 pts", 
               "+15,6 pts", "-9,4 pts", "-1,04")
)

# Criar a tabela formatada
tabela_formatada <- tabela_comparativa %>%
  flextable() %>%
  set_header_labels(Indicador = "Indicador",
                    X2010 = "2010",
                    X2022 = "2022",
                    Variação = "Variação") %>%
  colformat_num(j = 2:3, digits = c(0, 1, 0, 0, 1, 1, 1, 2)) %>%
  theme_zebra() %>%
  align(align = "center", part = "all") %>%
  align(align = "left", part = "body", j = 1) %>%
  bold(part = "header") %>%
  fontsize(size = 11, part = "all") %>%
  width(width = c(3.5, 2, 2, 2)) %>%
  add_footer_lines("Fonte: IBGE - Censos Demográficos 2010 e 2022.")

# Visualizar a tabela
tabela_formatada

# Salvar como Word
save_as_docx(tabela_formatada, path = "tabela_comparativa_para.docx")

library(flextable)
library(dplyr)
library(officer)

# Função para criar tabelas consistentes
criar_tabela <- function(dados, titulo) {
  dados %>%
    flextable() %>%
    set_caption(caption = titulo) %>%
    theme_zebra() %>%
    align(align = "center", part = "all") %>%
    align(align = "left", part = "body", j = 1) %>%
    bold(part = "header") %>%
    fontsize(size = 11, part = "all") %>%
    padding(padding = 4, part = "all") %>%
    bg(bg = "#f8f9fa", part = "header")
}

# Tabela 1: População Total
tabela_populacao <- data.frame(
  Ano = c("2010", "2022", "Variação", "% Variação"),
  População_Total = c("7.581.051", "8.120.131", "+539.080", "+7,1%"),
  Homens = c("3.821.837", "4.051.813", "+229.976", "+6,0%"),
  Mulheres = c("3.759.214", "4.068.318", "+309.104", "+8,2%")
)

# Tabela 2: Razão de Sexo
tabela_razao_sexo <- data.frame(
  Ano = c("2010", "2022", "Variação"),
  Razão_Sexo = c("101,7", "99,6", "-2,1"),
  Classificação = c("Predomínio Masculino", "Predomínio Feminino", "Inversão do padrão")
)

# Tabela 3: Idade Mediana
tabela_idade_mediana <- data.frame(
  Sexo = c("Homens", "Mulheres", "Total*"),
  `2010` = c("24", "24", "24"),
  `2022` = c("29", "30", "29,5"),
  Variação = c("+5", "+6", "+5,5")
)

# Tabela 4: Índice de Envelhecimento
tabela_envelhecimento <- data.frame(
  Sexo = c("Homens", "Mulheres", "Total"),
  `2010` = c("14,4", "16,2", "15,3"),
  `2022` = c("27,5", "31,8", "29,7"),
  Variação = c("+13,1", "+15,6", "+14,4")
)

# Tabela 5: Razão de Dependência
tabela_dependencia <- data.frame(
  Indicador = c("Dependência Juvenil", "Dependência Idosa", "Dependência Total"),
  `2010` = c("48,4%", "7,4%", "55,8%"),
  `2022` = c("36,0%", "10,6%", "46,6%"),
  Variação = c("-12,4", "+3,2", "-9,2")
)

# Tabela 6: Índice de Myers
tabela_myers <- data.frame(
  Ano = c("2010", "2022", "Variação"),
  Índice = c("4,42", "3,38", "-1,04"),
  Classificação = c("Boa qualidade", "Muito boa qualidade", "Melhoria na declaração")
)

# Gerar todas as tabelas
t1 <- criar_tabela(tabela_populacao, "Tabela 1: População Residente do Pará (2010-2022)")
t2 <- criar_tabela(tabela_razao_sexo, "Tabela 2: Razão de Sexo no Pará (homens para cada 100 mulheres)")
t3 <- criar_tabela(tabela_idade_mediana, "Tabela 3: Evolução da Idade Mediana por Sexo (em anos)")
t4 <- criar_tabela(tabela_envelhecimento, "Tabela 4: Índice de Envelhecimento (idosos para cada 100 jovens)")
t5 <- criar_tabela(tabela_dependencia, "Tabela 5: Razões de Dependência Demográfica (%)")
t6 <- criar_tabela(tabela_myers, "Tabela 6: Qualidade da Declaração de Idade - Índice de Myers")

# Salvar em um documento Word
save_as_docx(t1, t2, t3, t4, t5, t6, 
             path = "tabelas_demograficas_para_individualizadas.docx")



#---------------------------------------------------------------------------


# --- PROJEÇÕES POPULACIONAIS PARA O PARÁ - 2050 ---

# Dados populacionais históricos
populacao_para <- data.frame(
  Ano = c(2000, 2010, 2022),
  Populacao = c(6195629, 7581051, 8120131)  # Valores dos censos
)

# 1. MÉTODO ARITMÉTICO
metodo_aritmetico <- function(ano_base, pop_base, ano_projecao, ano_anterior, pop_anterior) {
  # Taxa de crescimento absoluto anual
  taxa_crescimento_anual <- (pop_base - pop_anterior) / (ano_base - ano_anterior)
  
  # Projeção
  anos_futuros <- ano_projecao - ano_base
  pop_projetada <- pop_base + (taxa_crescimento_anual * anos_futuros)
  
  return(list(
    pop_projetada = pop_projetada,
    taxa_anual = taxa_crescimento_anual
  ))
}

# 2. MÉTODO GEOMÉTRICO
metodo_geometrico <- function(ano_base, pop_base, ano_projecao, ano_anterior, pop_anterior) {
  # Taxa de crescimento geométrico anual
  periodo <- ano_base - ano_anterior
  taxa_crescimento_anual <- ((pop_base / pop_anterior) ^ (1/periodo)) - 1
  
  # Projeção
  anos_futuros <- ano_projecao - ano_base
  pop_projetada <- pop_base * ((1 + taxa_crescimento_anual) ^ anos_futuros)
  
  return(list(
    pop_projetada = pop_projetada,
    taxa_anual = taxa_crescimento_anual
  ))
}

# 3. MÉTODO EXPONENCIAL
metodo_exponencial <- function(ano_base, pop_base, ano_projecao, ano_anterior, pop_anterior) {
  # Taxa de crescimento exponencial anual
  periodo <- ano_base - ano_anterior
  taxa_crescimento_anual <- (log(pop_base) - log(pop_anterior)) / periodo
  
  # Projeção
  anos_futuros <- ano_projecao - ano_base
  pop_projetada <- pop_base * exp(taxa_crescimento_anual * anos_futuros)
  
  return(list(
    pop_projetada = pop_projetada,
    taxa_anual = taxa_crescimento_anual
  ))
}

# 4. MÉTODO LOGÍSTICO (requer 3 pontos equidistantes)
# Primeiro, retroprojeção geométrica de 2022 para 2020
retroprojecao_2020 <- metodo_geometrico(
  ano_base = 2010,
  pop_base = populacao_para$Populacao[2],  # 2010
  ano_projecao = 2020,
  ano_anterior = 2000,
  pop_anterior = populacao_para$Populacao[1]  # 2000
)

pop_2020_estimada <- retroprojecao_2020$pop_projetada

cat("População estimada para 2020 (retroprojeção geométrica):", round(pop_2020_estimada), "\n")

metodo_logistico <- function(ano_base, pop_base, ano_projecao, dados_equidistantes) {
  # Dados equidistantes: 2000, 2010, 2020
  t1 <- dados_equidistantes$Ano[1]  # 2000
  t2 <- dados_equidistantes$Ano[2]  # 2010
  t3 <- dados_equidistantes$Ano[3]  # 2020
  
  P1 <- dados_equidistantes$Populacao[1]  # 2000
  P2 <- dados_equidistantes$Populacao[2]  # 2010
  P3 <- dados_equidistantes$Populacao[3]  # 2020
  
  # Cálculo da população de saturação (K)
  K <- (2 * P1 * P2 * P3 - P2^2 * (P1 + P3)) / (P1 * P3 - P2^2)
  
  # Cálculo do coeficiente c
  c <- (K - P2) / P2
  
  # Cálculo do coeficiente b
  b <- exp((1/(t2 - t1)) * log((P2 * (K - P1)) / (P1 * (K - P2))))
  
  # Cálculo do coeficiente a
  a <- (K - P1) / (P1 * b^t1)
  
  # Projeção para 2050
  t <- ano_projecao
  pop_projetada <- K / (1 + a * b^t)
  
  return(list(
    pop_projetada = pop_projetada,
    K = K,  # População máxima de saturação
    a = a,
    b = b,
    c = c
  ))
}

# Dados para o método logístico (equidistantes: 2000, 2010, 2020)
dados_logistico <- data.frame(
  Ano = c(2000, 2010, 2020),
  Populacao = c(populacao_para$Populacao[1],  # 2000
                populacao_para$Populacao[2],  # 2010
                pop_2020_estimada)            # 2020 (estimada)
)

# 5. EXECUTAR PROJEÇÕES PARA 2050
ano_projecao <- 2050

# Método Aritmético (usando 2010-2022)
proj_aritmetica <- metodo_aritmetico(
  ano_base = 2022,
  pop_base = populacao_para$Populacao[3],
  ano_projecao = ano_projecao,
  ano_anterior = 2010,
  pop_anterior = populacao_para$Populacao[2]
)

# Método Geométrico (usando 2010-2022)
proj_geometrica <- metodo_geometrico(
  ano_base = 2022,
  pop_base = populacao_para$Populacao[3],
  ano_projecao = ano_projecao,
  ano_anterior = 2010,
  pop_anterior = populacao_para$Populacao[2]
)

# Método Exponencial (usando 2010-2022)
proj_exponencial <- metodo_exponencial(
  ano_base = 2022,
  pop_base = populacao_para$Populacao[3],
  ano_projecao = ano_projecao,
  ano_anterior = 2010,
  pop_anterior = populacao_para$Populacao[2]
)

# Método Logístico (usando 2000, 2010, 2020)
proj_logistica <- metodo_logistico(
  ano_base = 2020,
  pop_base = pop_2020_estimada,
  ano_projecao = ano_projecao,
  dados_equidistantes = dados_logistico
)

# 6. COMPARAÇÃO DAS PROJEÇÕES
resultados_projecoes <- data.frame(
  Metodo = c("Aritmético", "Geométrico", "Exponencial", "Logístico"),
  Populacao_2050 = c(proj_aritmetica$pop_projetada,
                     proj_geometrica$pop_projetada,
                     proj_exponencial$pop_projetada,
                     proj_logistica$pop_projetada),
  Diferenca_2022 = c(proj_aritmetica$pop_projetada - populacao_para$Populacao[3],
                     proj_geometrica$pop_projetada - populacao_para$Populacao[3],
                     proj_exponencial$pop_projetada - populacao_para$Populacao[3],
                     proj_logistica$pop_projetada - populacao_para$Populacao[3])
)

# Arredondar valores
resultados_projecoes$Populacao_2050 <- round(resultados_projecoes$Populacao_2050)
resultados_projecoes$Diferenca_2022 <- round(resultados_projecoes$Diferenca_2022)

print("COMPARAÇÃO DAS PROJEÇÕES POPULACIONAIS PARA 2050:")
print(resultados_projecoes)

# 7. PROJEÇÃO POR SEXO (usando método geométrico e estrutura etária atual)
# Projeção geométrica separada para homens e mulheres

proj_homens_2050 <- metodo_geometrico(
  ano_base = 2022,
  pop_base = pop_hm_2022$Valor[pop_hm_2022$Sexo == "Homens"],
  ano_projecao = ano_projecao,
  ano_anterior = 2010,
  pop_anterior = pop_hm_2010$Valor[pop_hm_2010$Sexo == "Homens"]
)

proj_mulheres_2050 <- metodo_geometrico(
  ano_base = 2022,
  pop_base = pop_hm_2022$Valor[pop_hm_2022$Sexo == "Mulheres"],
  ano_projecao = ano_projecao,
  ano_anterior = 2010,
  pop_anterior = pop_hm_2010$Valor[pop_hm_2010$Sexo == "Mulheres"]
)

proj_sexo_2050 <- data.frame(
  Sexo = c("Homens", "Mulheres", "Total"),
  Populacao_2050 = c(proj_homens_2050$pop_projetada,
                     proj_mulheres_2050$pop_projetada,
                     proj_homens_2050$pop_projetada + proj_mulheres_2050$pop_projetada),
  Proporcao = c(NA, NA, NA)
)

proj_sexo_2050$Proporcao[1:2] <- (proj_sexo_2050$Populacao_2050[1:2] / proj_sexo_2050$Populacao_2050[3]) * 100
proj_sexo_2050$Populacao_2050 <- round(proj_sexo_2050$Populacao_2050)

print("PROJEÇÃO POR SEXO PARA 2050 (Método Geométrico):")
print(proj_sexo_2050)

# 8. ANÁLISE DE PLAUSIBILIDADE
analise_plausibilidade <- function() {
  cat("\nANÁLISE DE PLAUSIBILIDADE DAS PROJEÇÕES:\n")
  cat("==========================================\n")
  
  cat("\n1. MÉTODO ARITMÉTICO:\n")
  cat("   População 2050:", round(proj_aritmetica$pop_projetada), "\n")
  cat("   Taxa anual constante:", round(proj_aritmetica$taxa_anual), "pessoas/ano\n")
  cat("   Vantagem: Simplicidade\n")
  cat("   Limitação: Não considera desaceleração do crescimento\n")
  
  cat("\n2. MÉTODO GEOMÉTRICO:\n")
  cat("   População 2050:", round(proj_geometrica$pop_projetada), "\n")
  cat("   Taxa anual:", round(proj_geometrica$taxa_anual * 100, 2), "% ao ano\n")
  cat("   Vantagem: Considera crescimento percentual constante\n")
  cat("   Limitação: Pode superestimar crescimento a longo prazo\n")
  
  cat("\n3. MÉTODO EXPONENCIAL:\n")
  cat("   População 2050:", round(proj_exponencial$pop_projetada), "\n")
  cat("   Taxa anual:", round(proj_exponencial$taxa_anual * 100, 2), "% ao ano\n")
  cat("   Vantagem: Similar ao geométrico, base contínua\n")
  
  cat("\n4. MÉTODO LOGÍSTICO:\n")
  cat("   População 2050:", round(proj_logistica$pop_projetada), "\n")
  cat("   População máxima (K):", round(proj_logistica$K), "\n")
  cat("   Vantagem: Considera saturação populacional\n")
  cat("   Limitação: Requer dados equidistantes (2000-2010-2020)\n")
  
  cat("\nCONCLUSÃO:\n")
  cat("Considerando a transição demográfica em curso no Pará,\n")
  cat("com desaceleração do crescimento populacional e tendência\n")
  cat("de envelhecimento, o MÉTODO LOGÍSTICO parece mais plausível\n")
  cat("pois incorpora a ideia de saturação populacional.\n")
}

# Executar análise
analise_plausibilidade()

# 9. GRÁFICO COMPARATIVO DAS PROJEÇÕES (VERSÃO CORRIGIDA)
# Criar dados para o gráfico
dados_historicos <- data.frame(
  Ano = c(2000, 2010, 2022),
  Populacao = populacao_para$Populacao,
  Metodo = "Observado",
  Tipo = "Histórico"
)

dados_projecoes <- data.frame(
  Ano = rep(2050, 4),
  Populacao = c(proj_aritmetica$pop_projetada,
                proj_geometrica$pop_projetada,
                proj_exponencial$pop_projetada,
                proj_logistica$pop_projetada),
  Metodo = c("Aritmético", "Geométrico", "Exponencial", "Logístico"),
  Tipo = "Projetado"
)

# Combinar dados históricos e projeções
dados_completos <- rbind(dados_historicos, dados_projecoes)

# Criar sequência de anos para as curvas de projeção
anos_projecao <- data.frame(
  Ano = c(2022, 2050)
)

# Calcular projeções para todos os anos entre 2022 e 2050 (para as curvas)
calcular_projecoes_anuais <- function(ano_inicio, ano_fim, metodo) {
  anos <- seq(ano_inicio, ano_fim, by = 1)
  populacoes <- numeric(length(anos))
  
  for (i in 1:length(anos)) {
    if (metodo == "Aritmético") {
      result <- metodo_aritmetico(2022, populacao_para$Populacao[3], anos[i], 2010, populacao_para$Populacao[2])
    } else if (metodo == "Geométrico") {
      result <- metodo_geometrico(2022, populacao_para$Populacao[3], anos[i], 2010, populacao_para$Populacao[2])
    } else if (metodo == "Exponencial") {
      result <- metodo_exponencial(2022, populacao_para$Populacao[3], anos[i], 2010, populacao_para$Populacao[2])
    } else if (metodo == "Logístico") {
      result <- metodo_logistico(2020, pop_2020_estimada, anos[i], dados_logistico)
    }
    populacoes[i] <- result$pop_projetada
  }
  
  return(data.frame(Ano = anos, Populacao = populacoes, Metodo = metodo, Tipo = "Projetado"))
}

# Calcular curvas completas para cada método
curva_aritmetica <- calcular_projecoes_anuais(2022, 2050, "Aritmético")
curva_geometrica <- calcular_projecoes_anuais(2022, 2050, "Geométrico")
curva_exponencial <- calcular_projecoes_anuais(2022, 2050, "Exponencial")
curva_logistica <- calcular_projecoes_anuais(2022, 2050, "Logístico")

# Combinar todas as curvas
curvas_completas <- rbind(curva_aritmetica, curva_geometrica, curva_exponencial, curva_logistica)

# Criar o gráfico
ggplot() +
  # Dados históricos (pontos)
  geom_point(data = dados_historicos, aes(x = Ano, y = Populacao, color = "Observado"), size = 4) +
  geom_line(data = dados_historicos, aes(x = Ano, y = Populacao, color = "Observado"), size = 1.5) +
  
  # Curvas de projeção
  geom_line(data = curvas_completas, aes(x = Ano, y = Populacao, color = Metodo), size = 1.2, linetype = "dashed") +
  
  # Pontos finais das projeções
  geom_point(data = dados_projecoes, aes(x = Ano, y = Populacao, color = Metodo), size = 4, shape = 17) +
  
  # Configurações estéticas
  scale_y_continuous(
    labels = function(x) {scales::comma(x, big.mark = ".", decimal.mark = ",")},
    limits = c(0, 15000000),
    breaks = seq(0, 15000000, by = 2000000)
  ) +
  scale_x_continuous(breaks = c(2000, 2010, 2022, 2030, 2040, 2050)) +
  scale_color_manual(
    values = c(
      "Observado" = "black",
      "Aritmético" = "red",
      "Geométrico" = "blue",
      "Exponencial" = "green",
      "Logístico" = "orange"
    )
  ) +
  labs(
    title = "Projeções Populacionais do Pará - 2000 a 2050",
    subtitle = "Comparação entre diferentes métodos de projeção",
    x = "Ano",
    y = "População",
    color = "Método"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90")
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

# 10. GRÁFICO ADICIONAL: DETALHE DAS PROJEÇÕES EM 2050
ggplot(dados_projecoes, aes(x = reorder(Metodo, -Populacao), y = Populacao, fill = Metodo)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = scales::comma(round(Populacao), big.mark = ".", decimal.mark = ",")), 
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_y_continuous(
    labels = function(x) {scales::comma(x, big.mark = ".", decimal.mark = ",")},
    limits = c(0, max(dados_projecoes$Populacao) * 1.1)
  ) +
  scale_fill_manual(values = c("Aritmético" = "red", "Geométrico" = "blue", 
                               "Exponencial" = "green", "Logístico" = "orange")) +
  labs(
    title = "População Projectada do Pará em 2050",
    subtitle = "Comparação entre diferentes métodos de projeção",
    x = "Método de Projeção",
    y = "População"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 11. SALVAR OS GRÁFICOS
ggsave("projecoes_populacionais_para.png", width = 12, height = 8, dpi = 300)
ggsave("comparacao_projecoes_2050.png", width = 10, height = 6, dpi = 300)

cat("Gráficos gerados e salvos com sucesso!\n")

# 10. SALVAR RESULTADOS EM TABELAS FORMATADAS
tabela_projecoes <- resultados_projecoes %>%
  flextable() %>%
  set_header_labels(Metodo = "Método de Projeção",
                    Populacao_2050 = "População Projectada 2050",
                    Diferenca_2022 = "Diferença em relação a 2022") %>%
  colformat_num(j = 2:3, big.mark = ".", decimal.mark = ",", digits = 0) %>%
  theme_zebra() %>%
  align(align = "center", part = "all") %>%
  bold(part = "header")

tabela_proj_sexo <- proj_sexo_2050 %>%
  flextable() %>%
  set_header_labels(Sexo = "Sexo",
                    Populacao_2050 = "População Projectada 2050",
                    Proporcao = "Proporção (%)") %>%
  colformat_num(j = 2, big.mark = ".", decimal.mark = ",", digits = 0) %>%
  colformat_num(j = 3, big.mark = ".", decimal.mark = ",", digits = 1) %>%
  theme_zebra() %>%
  align(align = "center", part = "all") %>%
  bold(part = "header")

# Salvar tabelas
save_as_docx(tabela_projecoes, tabela_proj_sexo,
             path = "projecoes_populacionais_para_2050.docx")

cat("\nProjeções concluídas! Resultados salvos em 'projecoes_populacionais_para_2050.docx'\n")

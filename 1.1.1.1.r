library(sidrar)
library(dplyr)
library(gtsummary)
library(stringr)
library(ggplot2)
library(tidyr)
library(knitr)
library(scales)


info_sidra(1209)

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
  scale_fill_manual(values = c("Homens" = "#1f78b4", "Mulheres" = "#e31a1c")) +
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
  scale_fill_manual(values = c("Homens" = "#1f78b4", "Mulheres" = "#e31a1c")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# --- Seção de Geração e Salvamento de Tabelas ---

# Tabela 1: População Residente por Sexo e Razão de Sexo
pop_hm_2010 <- para_idade2010_simples |> group_by(Sexo) |> summarise(Populacao = sum(Populacao))
razao_sexo_2010 <- (pop_hm_2010$Populacao[pop_hm_2010$Sexo == "Homens"] / pop_hm_2010$Populacao[pop_hm_2010$Sexo == "Mulheres"]) * 100

pop_hm_2022 <- para_idade2022_simples |> group_by(Sexo) |> summarise(Populacao = sum(Populacao))
razao_sexo_2022 <- (pop_hm_2022$Populacao[pop_hm_2022$Sexo == "Homens"] / pop_hm_2022$Populacao[pop_hm_2022$Sexo == "Mulheres"]) * 100

tabela_pop_razao <- tibble(
  Ano = c(2010, 2022),
  `População Homens` = c(
    pop_hm_2010$Populacao[pop_hm_2010$Sexo == "Homens"],
    pop_hm_2022$Populacao[pop_hm_2022$Sexo == "Homens"]
  ),
  `População Mulheres` = c(
    pop_hm_2010$Populacao[pop_hm_2010$Sexo == "Mulheres"],
    pop_hm_2022$Populacao[pop_hm_2022$Sexo == "Mulheres"]
  ),
  `População Total` = c(
    sum(pop_hm_2010$Populacao),
    sum(pop_hm_2022$Populacao)
  ),
  `Razão de Sexo` = c(razao_sexo_2010, razao_sexo_2022)
)
write.csv(tabela_pop_razao, "tabelas_resultados/tabela_populacao_razao_sexo.csv", row.names = FALSE)

# Tabela 2: Idade Mediana por Sexo
idade_mediana_para_2010 <- para_idade2010_simples |>
  group_by(Ano, Sexo) |>
  arrange(Idade) |>
  mutate(
    populacao_acumulada = cumsum(Populacao),
    populacao_total = sum(Populacao)
  ) |>
  filter(populacao_acumulada >= populacao_total / 2) |>
  summarise(idade_mediana = first(Idade), .groups = "drop")

idade_mediana_para_2022 <- para_idade2022_simples |>
  group_by(Ano, Sexo) |>
  arrange(Idade) |>
  mutate(
    populacao_acumulada = cumsum(Populacao),
    populacao_total = sum(Populacao)
  ) |>
  filter(populacao_acumulada >= populacao_total / 2) |>
  summarise(idade_mediana = first(Idade), .groups = "drop")

tabela_idade_mediana <- bind_rows(idade_mediana_para_2010, idade_mediana_para_2022)
write.csv(tabela_idade_mediana, "tabelas_resultados/tabela_idade_mediana.csv", row.names = FALSE)

# Tabela 3: Razão de Dependência por Sexo
razao_dependencia_para_2010 <- para_idade2010_simples |>
  mutate(
    categoria_idade = case_when(Idade >= 0 & Idade <= 14 ~ "Jovem", Idade >= 15 & Idade <= 64 ~ "Ativo", Idade >= 65 ~ "Idoso")
  ) |>
  group_by(Ano, Sexo) |>
  summarise(
    pop_jovem = sum(Populacao[categoria_idade == "Jovem"], na.rm = TRUE),
    pop_ativa = sum(Populacao[categoria_idade == "Ativo"], na.rm = TRUE),
    pop_idosa = sum(Populacao[categoria_idade == "Idoso"], na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    razao_dep_jovem = (pop_jovem / pop_ativa) * 100,
    razao_dep_idosa = (pop_idosa / pop_ativa) * 100,
    razao_dep_total = ((pop_jovem + pop_idosa) / pop_ativa) * 100
  ) |>
  select(Ano, Sexo, `Razão Dep. Jovem` = razao_dep_jovem, `Razão Dep. Idosa` = razao_dep_idosa, `Razão Dep. Total` = razao_dep_total)

razao_dependencia_para_2022 <- para_idade2022_simples |>
  mutate(
    categoria_idade = case_when(Idade >= 0 & Idade <= 14 ~ "Jovem", Idade >= 15 & Idade <= 64 ~ "Ativo", Idade >= 65 ~ "Idoso")
  ) |>
  group_by(Ano, Sexo) |>
  summarise(
    pop_jovem = sum(Populacao[categoria_idade == "Jovem"], na.rm = TRUE),
    pop_ativa = sum(Populacao[categoria_idade == "Ativo"], na.rm = TRUE),
    pop_idosa = sum(Populacao[categoria_idade == "Idoso"], na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    razao_dep_jovem = (pop_jovem / pop_ativa) * 100,
    razao_dep_idosa = (pop_idosa / pop_ativa) * 100,
    razao_dep_total = ((pop_jovem + pop_idosa) / pop_ativa) * 100
  ) |>
  select(Ano, Sexo, `Razão Dep. Jovem` = razao_dep_jovem, `Razão Dep. Idosa` = razao_dep_idosa, `Razão Dep. Total` = razao_dep_total)

tabela_razao_dependencia <- bind_rows(razao_dependencia_para_2010, razao_dependencia_para_2022)
write.csv(tabela_razao_dependencia, "tabelas_resultados/tabela_razao_dependencia.csv", row.names = FALSE)

# Tabela 4: Índice de Envelhecimento por Sexo
indice_envelhecimento_para_2010 <- para_idade2010_simples |>
  mutate(categoria_idade = case_when(Idade >= 0 & Idade <= 14 ~ "Jovem", Idade >= 65 ~ "Idoso")) |>
  group_by(Ano, Sexo) |>
  summarise(pop_jovem = sum(Populacao[categoria_idade == "Jovem"], na.rm = TRUE),
            pop_idosa = sum(Populacao[categoria_idade == "Idoso"], na.rm = TRUE),
            .groups = "drop") |>
  mutate(indice_envelhecimento = (pop_idosa / pop_jovem) * 100) |>
  select(Ano, Sexo, `Índice de Envelhecimento` = indice_envelhecimento)

indice_envelhecimento_para_2022 <- para_idade2022_simples |>
  mutate(categoria_idade = case_when(Idade >= 0 & Idade <= 14 ~ "Jovem", Idade >= 65 ~ "Idoso")) |>
  group_by(Ano, Sexo) |>
  summarise(pop_jovem = sum(Populacao[categoria_idade == "Jovem"], na.rm = TRUE),
            pop_idosa = sum(Populacao[categoria_idade == "Idoso"], na.rm = TRUE),
            .groups = "drop") |>
  mutate(indice_envelhecimento = (pop_idosa / pop_jovem) * 100) |>
  select(Ano, Sexo, `Índice de Envelhecimento` = indice_envelhecimento)

tabela_indice_envelhecimento <- bind_rows(indice_envelhecimento_para_2010, indice_envelhecimento_para_2022)
write.csv(tabela_indice_envelhecimento, "tabelas_resultados/tabela_indice_envelhecimento.csv", row.names = FALSE)

# Tabela 5: Índice de Myers (2010 e 2022)
pop_idade_2010 <- para_idade2010_simples |>
  group_by(Idade) |>
  summarise(Pop = sum(Populacao), .groups = "drop") |>
  mutate(digito_final = Idade %% 10)
pop_por_digito_2010 <- tapply(pop_idade_2010$Pop, pop_idade_2010$digito_final, sum)
f_i_2010 <- pop_por_digito_2010 / sum(pop_por_digito_2010)
indice_myers_2010 <- 100 * sum(abs(f_i_2010 - 1/10))

pop_idade_2022 <- para_idade2022_simples |>
  group_by(Idade) |>
  summarise(Pop = sum(Populacao), .groups = "drop") |>
  mutate(digito_final = Idade %% 10)
pop_por_digito_2022 <- tapply(pop_idade_2022$Pop, pop_idade_2022$digito_final, sum)
f_i_2022 <- pop_por_digito_2022 / sum(pop_por_digito_2022)
indice_myers_2022 <- 100 * sum(abs(f_i_2022 - 1/10))

tabela_myers <- tibble(
  Ano = c(2010, 2022),
  `Índice de Myers` = c(indice_myers_2010, indice_myers_2022)
)
write.csv(tabela_myers, "tabelas_resultados/tabela_indice_myers.csv", row.names = FALSE)

# --- Criação do dataframe para o gráfico ---
dados_razao_sexo <- tibble(
  Ano = c("Censo 2010", "Censo 2022"),
  Razao_Sexo = c(razao_sexo2010, razao_sexo2022)
)

# --- Gerando o gráfico ---
ggplot(dados_razao_sexo, aes(x = Ano, y = Razao_Sexo)) +
  geom_bar(stat = "identity", fill = "#1f78b4", width = 0.5) +
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
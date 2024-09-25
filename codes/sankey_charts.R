# Carregar pacotes
library(readxl)
library(ggsankey)
library(ggplot2)
library(dplyr)
library(forcats)
library(tidyr)



############## entomology #######################

# Ler os dados
entomology <- read_excel("processed_entomology.xlsx")

# Selecionar as variáveis de interesse e remover valores ausentes usando filter do dplyr
dados_network <- entomology %>%
  select(assumptions_check, which_assumptions_checked, type_of_transformation, 
         motivation, interpretation_scala, reassessment_assumptions) %>%
  filter(
    !is.na(assumptions_check),
    !is.na(which_assumptions_checked),
    !is.na(type_of_transformation),
    !is.na(motivation),
    !is.na(interpretation_scala),
    !is.na(reassessment_assumptions)
  ) %>%
  mutate(
    assumptions_check = as.factor(assumptions_check),
    which_assumptions_checked = as.factor(which_assumptions_checked),
    type_of_transformation = as.factor(type_of_transformation),
    motivation = as.factor(motivation),
    interpretation_scala = as.factor(interpretation_scala),
    reassessment_assumptions = as.factor(reassessment_assumptions)
  )

entomology |>
  count(status)

# Aplicar a função make_long do ggsankey
df <- dados_network %>%
  make_long(assumptions_check, which_assumptions_checked, type_of_transformation, 
            motivation, interpretation_scala, reassessment_assumptions)

# Criar o data frame para contagem
dagg <- df %>%
  group_by(node) %>%
  tally()

# Juntar os dados com as contagens
df2 <- merge(df, dagg, by.x = "node", by.y = "node", all.x = TRUE)

# Definir uma paleta de cores distinta para as categorias
cores_distintas <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", 
  "#7f7f7f", "#bcbd22", "#17becf", "#393b79", "#9c9ede", "#8c6d31", "#e7ba52", 
  "#843c39", "#d6616b", "#7b4173", "#a55194", "#637939", "#ce6dbd", "#54FF9F",
  "#FFF68F")


# Criar o gráfico de Sankey
pl <- ggplot(df2, aes(x = x,
                      next_x = next_x,
                      node = node,
                      next_node = next_node,
                      fill = factor(node),
                      label = paste0(node, " n=", n)))

pl <- pl + geom_sankey(na.rm = TRUE, flow.alpha = 0.5, node.color = "black", show.legend = TRUE)
pl <- pl + theme_bw()
pl <- pl + theme_sankey(base_size = 16)
pl <- pl + theme(legend.position = "none")
pl <- pl + labs(fill = "nodes")
pl <- pl + theme(axis.title = element_blank(),
                 axis.text.y = element_blank(),
                 axis.ticks = element_blank(),
                 panel.grid = element_blank())

# Aplicar a paleta de cores distinta
pl <- pl + scale_fill_manual(values = cores_distintas)

# Exibir o gráfico
pl


##### versão com rótulos das categorias e quantidades


pl <- ggplot(df2, aes(x = x,
                      next_x = next_x,
                      node = node,
                      next_node = next_node,
                      fill = factor(node),
                      label = paste0(node, " (n=", n, ")")))

pl <- pl + geom_sankey(na.rm = TRUE, flow.alpha = 0.5, node.color = "black", show.legend = TRUE)

# Adicionar os rótulos com nomes das categorias e quantidades
pl <- pl + geom_sankey_text(size = 3, color = "black")

# Customizar o tema do gráfico
pl <- pl + theme_bw()
pl <- pl + theme_sankey(base_size = 16)
pl <- pl + theme(legend.position = "none")
pl <- pl + labs(fill = "nodes")
pl <- pl + theme(axis.title = element_blank(),
                 axis.text.y = element_blank(),
                 axis.ticks = element_blank(),
                 panel.grid = element_blank())

# Aplicar a paleta de cores distinta
pl <- pl + scale_fill_manual(values = cores_distintas)

# Exibir o gráfico
pl


# Contar as categorias para cada variável

# Criar tabelas separadas de contagens para cada variável
cont_assumptions_check <- table(dados_network$assumptions_check)
cont_which_assumptions_checked <- table(dados_network$which_assumptions_checked)
cont_type_of_transformation <- table(dados_network$type_of_transformation)
cont_motivation <- table(dados_network$motivation)
cont_interpretation_scala <- table(dados_network$interpretation_scala)
cont_reassessment_assumptions <- table(dados_network$reassessment_assumptions)

# Criar uma lista com todas as contagens
contagens_list <- list(
  `Assumptions Check` = paste0(names(cont_assumptions_check), " (", cont_assumptions_check, ")"),
  `Which Assumptions Checked` = paste0(names(cont_which_assumptions_checked), " (", cont_which_assumptions_checked, ")"),
  `Type of Transformation` = paste0(names(cont_type_of_transformation), " (", cont_type_of_transformation, ")"),
  `Motivation` = paste0(names(cont_motivation), " (", cont_motivation, ")"),
  `Interpretation Scale` = paste0(names(cont_interpretation_scala), " (", cont_interpretation_scala, ")"),
  `Reassessment Assumption` = paste0(names(cont_reassessment_assumptions), " (", cont_reassessment_assumptions, ")")
)

# Encontrar o número máximo de categorias
max_length <- max(sapply(contagens_list, length))

# Preencher com NA as colunas que têm menos categorias
tabela_contagens_entomology <- lapply(contagens_list, function(x) { length(x) <- max_length; return(x) })

# Converter para data frame
tabela_contagens_entomology <- as.data.frame(tabela_contagens_entomology)

# Exibir a tabela
tabela_contagens_entomology




############## plant #######################

# Ler os dados
plant <- read_excel("processed_plant.xlsx")

# Selecionar as variáveis de interesse e remover valores ausentes
dados_network <- plant %>%
  select(assumptions_check, which_assumptions_checked, type_of_transformation, 
         motivation, interpretation_scala, reassessment_assumptions) %>%
  filter(
    !is.na(assumptions_check),
    !is.na(which_assumptions_checked),
    !is.na(type_of_transformation),
    !is.na(motivation),
    !is.na(interpretation_scala),
    !is.na(reassessment_assumptions)
  ) %>%
  mutate(
    assumptions_check = as.factor(assumptions_check),
    which_assumptions_checked = as.factor(which_assumptions_checked),
    type_of_transformation = as.factor(type_of_transformation),
    motivation = as.factor(motivation),
    interpretation_scala = as.factor(interpretation_scala),
    reassessment_assumptions = as.factor(reassessment_assumptions)
  )

# Aplicar a função make_long
df <- dados_network %>%
  make_long(assumptions_check, which_assumptions_checked, type_of_transformation, 
            motivation, interpretation_scala, reassessment_assumptions)

# Criar o data frame para contagem
dagg <- df %>%
  group_by(node) %>%
  tally()

# Juntar os dados com as contagens
df2 <- merge(df, dagg, by.x = "node", by.y = "node", all.x = TRUE)

# Definir uma paleta de cores distinta para as categorias
cores_distintas <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", 
  "#7f7f7f", "#bcbd22", "#17becf", "#393b79", "#9c9ede", "#8c6d31", "#e7ba52", 
  "#843c39", "#d6616b", "#7b4173", "#a55194", "#637939", "#ce6dbd", "#54FF9F",
  "#FFF68F")

# Criar o gráfico Sankey sem rótulos
pl <- ggplot(df2, aes(x = x,
                      next_x = next_x,
                      node = node,
                      next_node = next_node,
                      fill = factor(node),
                      label = paste0(node, " n=", n)))

pl <- pl + geom_sankey(na.rm = TRUE, flow.alpha = 0.5, node.color = "black", show.legend = TRUE)
pl <- pl + theme_bw()
pl <- pl + theme_sankey(base_size = 16)
pl <- pl + theme(legend.position = "none")
pl <- pl + labs(fill = "nodes")
pl <- pl + theme(axis.title = element_blank(),
                 axis.text.y = element_blank(),
                 axis.ticks = element_blank(),
                 panel.grid = element_blank())

# Aplicar a paleta de cores distinta
pl <- pl + scale_fill_manual(values = cores_distintas)

# Exibir o gráfico sem rótulos
pl

##### versão com rótulos das categorias e quantidades

pl <- ggplot(df2, aes(x = x,
                      next_x = next_x,
                      node = node,
                      next_node = next_node,
                      fill = factor(node),
                      label = paste0(node, " (n=", n, ")")))

pl <- pl + geom_sankey(na.rm = TRUE, flow.alpha = 0.5, node.color = "black", show.legend = TRUE)

# Adicionar os rótulos com nomes das categorias e quantidades
pl <- pl + geom_sankey_text(size = 3, color = "black")

# Customizar o tema do gráfico
pl <- pl + theme_bw()
pl <- pl + theme_sankey(base_size = 16)
pl <- pl + theme(legend.position = "none")
pl <- pl + labs(fill = "nodes")
pl <- pl + theme(axis.title = element_blank(),
                 axis.text.y = element_blank(),
                 axis.ticks = element_blank(),
                 panel.grid = element_blank())

# Aplicar a paleta de cores distinta
pl <- pl + scale_fill_manual(values = cores_distintas)

# Exibir o gráfico com rótulos
pl


# Contar as categorias para cada variável

# Criar tabelas separadas de contagens para cada variável
cont_assumptions_check <- table(dados_network$assumptions_check)
cont_which_assumptions_checked <- table(dados_network$which_assumptions_checked)
cont_type_of_transformation <- table(dados_network$type_of_transformation)
cont_motivation <- table(dados_network$motivation)
cont_interpretation_scala <- table(dados_network$interpretation_scala)
cont_reassessment_assumptions <- table(dados_network$reassessment_assumptions)

# Criar uma lista com todas as contagens
contagens_list <- list(
  `Assumptions Check` = paste0(names(cont_assumptions_check), " (", cont_assumptions_check, ")"),
  `Which Assumptions Checked` = paste0(names(cont_which_assumptions_checked), " (", cont_which_assumptions_checked, ")"),
  `Type of Transformation` = paste0(names(cont_type_of_transformation), " (", cont_type_of_transformation, ")"),
  `Motivation` = paste0(names(cont_motivation), " (", cont_motivation, ")"),
  `Interpretation Scale` = paste0(names(cont_interpretation_scala), " (", cont_interpretation_scala, ")"),
  `Reassessment Assumption` = paste0(names(cont_reassessment_assumptions), " (", cont_reassessment_assumptions, ")")
)

# Encontrar o número máximo de categorias
max_length <- max(sapply(contagens_list, length))

# Preencher com NA as colunas que têm menos categorias
tabela_contagens_plant <- lapply(contagens_list, function(x) { length(x) <- max_length; return(x) })

# Converter para data frame
tabela_contagens_plant <- as.data.frame(tabela_contagens_plant)

# Exibir a tabela
tabela_contagens_plant




############## forestry #######################

# Ler os dados
forestry <- read_excel("processed_forestry.xlsx")

# Selecionar as variáveis de interesse e remover valores ausentes
dados_network <- forestry %>%
  select(assumptions_check, which_assumptions_checked, type_of_transformation, 
         motivation, interpretation_scala, reassessment_assumptions) %>%
  filter(
    !is.na(assumptions_check),
    !is.na(which_assumptions_checked),
    !is.na(type_of_transformation),
    !is.na(motivation),
    !is.na(interpretation_scala),
    !is.na(reassessment_assumptions)
  ) %>%
  mutate(
    assumptions_check = as.factor(assumptions_check),
    which_assumptions_checked = as.factor(which_assumptions_checked),
    type_of_transformation = as.factor(type_of_transformation),
    motivation = as.factor(motivation),
    interpretation_scala = as.factor(interpretation_scala),
    reassessment_assumptions = as.factor(reassessment_assumptions)
  )

# Aplicar a função make_long
df <- dados_network %>%
  make_long(assumptions_check, which_assumptions_checked, type_of_transformation, 
            motivation, interpretation_scala, reassessment_assumptions)

# Criar o data frame para contagem
dagg <- df %>%
  group_by(node) %>%
  tally()

# Juntar os dados com as contagens
df2 <- merge(df, dagg, by.x = "node", by.y = "node", all.x = TRUE)

# Definir uma paleta de cores distinta para as categorias
cores_distintas <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", 
  "#7f7f7f", "#bcbd22", "#17becf", "#393b79", "#9c9ede", "#8c6d31", "#e7ba52", 
  "#843c39", "#d6616b", "#7b4173", "#a55194", "#637939", "#ce6dbd", "#54FF9F",
  "#FFF68F")

# Criar o gráfico Sankey sem rótulos
pl <- ggplot(df2, aes(x = x,
                      next_x = next_x,
                      node = node,
                      next_node = next_node,
                      fill = factor(node),
                      label = paste0(node, " n=", n)))

pl <- pl + geom_sankey(na.rm = TRUE, flow.alpha = 0.5, node.color = "black", show.legend = TRUE)
pl <- pl + theme_bw()
pl <- pl + theme_sankey(base_size = 16)
pl <- pl + theme(legend.position = "none")
pl <- pl + labs(fill = "nodes")
pl <- pl + theme(axis.title = element_blank(),
                 axis.text.y = element_blank(),
                 axis.ticks = element_blank(),
                 panel.grid = element_blank())

# Aplicar a paleta de cores distinta
pl <- pl + scale_fill_manual(values = cores_distintas)

# Exibir o gráfico sem rótulos
pl

##### versão com rótulos das categorias e quantidades

pl <- ggplot(df2, aes(x = x,
                      next_x = next_x,
                      node = node,
                      next_node = next_node,
                      fill = factor(node),
                      label = paste0(node, " (n=", n, ")")))

pl <- pl + geom_sankey(na.rm = TRUE, flow.alpha = 0.5, node.color = "black", show.legend = TRUE)

# Adicionar os rótulos com nomes das categorias e quantidades
pl <- pl + geom_sankey_text(size = 3, color = "black")

# Customizar o tema do gráfico
pl <- pl + theme_bw()
pl <- pl + theme_sankey(base_size = 16)
pl <- pl + theme(legend.position = "none")
pl <- pl + labs(fill = "nodes")
pl <- pl + theme(axis.title = element_blank(),
                 axis.text.y = element_blank(),
                 axis.ticks = element_blank(),
                 panel.grid = element_blank())

# Aplicar a paleta de cores distinta
pl <- pl + scale_fill_manual(values = cores_distintas)

# Exibir o gráfico com rótulos
pl


# Contar as categorias para cada variável

# Criar tabelas separadas de contagens para cada variável
cont_assumptions_check <- table(dados_network$assumptions_check)
cont_which_assumptions_checked <- table(dados_network$which_assumptions_checked)
cont_type_of_transformation <- table(dados_network$type_of_transformation)
cont_motivation <- table(dados_network$motivation)
cont_interpretation_scala <- table(dados_network$interpretation_scala)
cont_reassessment_assumptions <- table(dados_network$reassessment_assumptions)

# Criar uma lista com todas as contagens
contagens_list <- list(
  `Assumptions Check` = paste0(names(cont_assumptions_check), " (", cont_assumptions_check, ")"),
  `Which Assumptions Checked` = paste0(names(cont_which_assumptions_checked), " (", cont_which_assumptions_checked, ")"),
  `Type of Transformation` = paste0(names(cont_type_of_transformation), " (", cont_type_of_transformation, ")"),
  `Motivation` = paste0(names(cont_motivation), " (", cont_motivation, ")"),
  `Interpretation Scale` = paste0(names(cont_interpretation_scala), " (", cont_interpretation_scala, ")"),
  `Reassessment Assumption` = paste0(names(cont_reassessment_assumptions), " (", cont_reassessment_assumptions, ")")
)

# Encontrar o número máximo de categorias
max_length <- max(sapply(contagens_list, length))

# Preencher com NA as colunas que têm menos categorias
tabela_contagens_forestry <- lapply(contagens_list, function(x) { length(x) <- max_length; return(x) })

# Converter para data frame
tabela_contagens_forestry <- as.data.frame(tabela_contagens_forestry)

# Exibir a tabela
tabela_contagens_forestry



############## soil #######################

# Ler os dados
soil <- read_excel("processed_soil.xlsx")

# Selecionar as variáveis de interesse e remover valores ausentes
dados_network <- soil %>%
  select(assumptions_check, which_assumptions_checked, type_of_transformation, 
         motivation, interpretation_scala, reassessment_assumptions) %>%
  filter(
    !is.na(assumptions_check),
    !is.na(which_assumptions_checked),
    !is.na(type_of_transformation),
    !is.na(motivation),
    !is.na(interpretation_scala),
    !is.na(reassessment_assumptions)
  ) %>%
  mutate(
    assumptions_check = as.factor(assumptions_check),
    which_assumptions_checked = as.factor(which_assumptions_checked),
    type_of_transformation = as.factor(type_of_transformation),
    motivation = as.factor(motivation),
    interpretation_scala = as.factor(interpretation_scala),
    reassessment_assumptions = as.factor(reassessment_assumptions)
  )

# Aplicar a função make_long
df <- dados_network %>%
  make_long(assumptions_check, which_assumptions_checked, type_of_transformation, 
            motivation, interpretation_scala, reassessment_assumptions)

# Criar o data frame para contagem
dagg <- df %>%
  group_by(node) %>%
  tally()

# Juntar os dados com as contagens
df2 <- merge(df, dagg, by.x = "node", by.y = "node", all.x = TRUE)

# Definir uma paleta de cores distinta para as categorias
cores_distintas <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", 
  "#7f7f7f", "#bcbd22", "#17becf", "#393b79", "#9c9ede", "#8c6d31", "#e7ba52", 
  "#843c39", "#d6616b", "#7b4173", "#a55194", "#637939", "#ce6dbd", "#54FF9F",
  "#FFF68F")

# Criar o gráfico Sankey sem rótulos
pl <- ggplot(df2, aes(x = x,
                      next_x = next_x,
                      node = node,
                      next_node = next_node,
                      fill = factor(node),
                      label = paste0(node, " n=", n)))

pl <- pl + geom_sankey(na.rm = TRUE, flow.alpha = 0.5, node.color = "black", show.legend = TRUE)
pl <- pl + theme_bw()
pl <- pl + theme_sankey(base_size = 16)
pl <- pl + theme(legend.position = "none")
pl <- pl + labs(fill = "nodes")
pl <- pl + theme(axis.title = element_blank(),
                 axis.text.y = element_blank(),
                 axis.ticks = element_blank(),
                 panel.grid = element_blank())

# Aplicar a paleta de cores distinta
pl <- pl + scale_fill_manual(values = cores_distintas)

# Exibir o gráfico sem rótulos
pl

##### versão com rótulos das categorias e quantidades

pl <- ggplot(df2, aes(x = x,
                      next_x = next_x,
                      node = node,
                      next_node = next_node,
                      fill = factor(node),
                      label = paste0(node, " (n=", n, ")")))

pl <- pl + geom_sankey(na.rm = TRUE, flow.alpha = 0.5, node.color = "black", show.legend = TRUE)

# Adicionar os rótulos com nomes das categorias e quantidades
pl <- pl + geom_sankey_text(size = 3, color = "black")

# Customizar o tema do gráfico
pl <- pl + theme_bw()
pl <- pl + theme_sankey(base_size = 16)
pl <- pl + theme(legend.position = "none")
pl <- pl + labs(fill = "nodes")
pl <- pl + theme(axis.title = element_blank(),
                 axis.text.y = element_blank(),
                 axis.ticks = element_blank(),
                 panel.grid = element_blank())

# Aplicar a paleta de cores distinta
pl <- pl + scale_fill_manual(values = cores_distintas)

# Exibir o gráfico com rótulos
pl


# Contar as categorias para cada variável

# Criar tabelas separadas de contagens para cada variável
cont_assumptions_check <- table(dados_network$assumptions_check)
cont_which_assumptions_checked <- table(dados_network$which_assumptions_checked)
cont_type_of_transformation <- table(dados_network$type_of_transformation)
cont_motivation <- table(dados_network$motivation)
cont_interpretation_scala <- table(dados_network$interpretation_scala)
cont_reassessment_assumptions <- table(dados_network$reassessment_assumptions)

# Criar uma lista com todas as contagens
contagens_list <- list(
  `Assumptions Check` = paste0(names(cont_assumptions_check), " (", cont_assumptions_check, ")"),
  `Which Assumptions Checked` = paste0(names(cont_which_assumptions_checked), " (", cont_which_assumptions_checked, ")"),
  `Type of Transformation` = paste0(names(cont_type_of_transformation), " (", cont_type_of_transformation, ")"),
  `Motivation` = paste0(names(cont_motivation), " (", cont_motivation, ")"),
  `Interpretation Scale` = paste0(names(cont_interpretation_scala), " (", cont_interpretation_scala, ")"),
  `Reassessment Assumption` = paste0(names(cont_reassessment_assumptions), " (", cont_reassessment_assumptions, ")")
)

# Encontrar o número máximo de categorias
max_length <- max(sapply(contagens_list, length))

# Preencher com NA as colunas que têm menos categorias
tabela_contagens_soil <- lapply(contagens_list, function(x) { length(x) <- max_length; return(x) })

# Converter para data frame
tabela_contagens_soil <- as.data.frame(tabela_contagens_soil)

# Exibir a tabela
tabela_contagens_soil




######################## Contagens ###########################
# Carregar as bibliotecas necessárias
library(readxl)

# Ler os arquivos 
entomology <- read_excel("processed_entomology.xlsx")
plant <- read_excel("processed_plant.xlsx")
forestry <- read_excel("processed_forestry.xlsx")
soil <- read_excel("processed_soil.xlsx")

# Função para contar total de referências e total de selecionados
count_references <- function(df) {
  total_references <- length(unique(df$reference_number))  # Contar todas as referências distintas
  selected_df <- df[df$status == 'selected', ]  # Filtrar apenas as linhas com status 'selected'
  selected_references <- length(unique(selected_df$reference_number))  # Contar referências distintas dos selecionados
  total_selected_observations <- nrow(selected_df)  # Contar número total de observações selecionadas
  return(list(total_references = total_references, selected_references = selected_references, total_selected_observations = total_selected_observations))
}


# Aplicar a função para cada conjunto de dados
entomology_info <- count_references(entomology)
plant_info <- count_references(plant)
forestry_info <- count_references(forestry)
soil_info <- count_references(soil)

# Criar um dataframe para armazenar os resultados
resultados <- data.frame(
  Área = c("Entomology", "Plant", "Forestry", "Soil"),
  Total_Artigos = c(entomology_info$total_references, plant_info$total_references, forestry_info$total_references, soil_info$total_references),
  Artigos_Selecionados = c(entomology_info$selected_references, plant_info$selected_references, forestry_info$selected_references, soil_info$selected_references),
  Total_Observações_Selecionadas = c(entomology_info$total_selected_observations, plant_info$total_selected_observations, forestry_info$total_selected_observations, soil_info$total_selected_observations)
)

# Calcular os totais e adicionar ao dataframe
totais <- data.frame(
  Área = "Total",
  Total_Artigos = sum(resultados$Total_Artigos),
  Artigos_Selecionados = sum(resultados$Artigos_Selecionados),
  Total_Observações_Selecionadas = sum(resultados$Total_Observações_Selecionadas)
)

# Adicionar a linha dos totais ao final da tabela
resultados <- rbind(resultados, totais)

# Exibir a tabela de resultados
print(resultados)


# Transpor a tabela de resultados
resultados_transposta <- as.data.frame(t(resultados))

# Ajustar os nomes das colunas corretamente após a transposição
colnames(resultados_transposta) <- resultados_transposta[1, ]
resultados_transposta <- resultados_transposta[-1, ]

# Exibir a tabela transposta
print(resultados_transposta)



# descartados

entomology <- read_excel("processed_entomology.xlsx")
plant <- read_excel("processed_plant.xlsx")
forestry <- read_excel("processed_forestry.xlsx")
soil <- read_excel("processed_soil.xlsx")

# Contar o número de artigos com status 'discarded'
num_discarded_e <- sum(entomology$status == 'discarted');num_discarded_e
num_discarded_p <- sum(plant$status == 'discarted');num_discarded_p
num_discarded_f <- sum(forestry$status == 'discarted');num_discarded_f
num_discarded_s <- sum(soil$status == 'discarted');num_discarded_s

# anos
entomology <- read_excel("processed_entomology.xlsx")
plant <- read_excel("processed_plant.xlsx")
forestry <- read_excel("processed_forestry.xlsx")
soil <- read_excel("processed_soil.xlsx")

# Encontrar o ano mais antigo e o mais recente na variável 'year'
selected_e <- entomology[entomology$status == 'selected', ]
min_year_e <- min(selected_e$year, na.rm = TRUE); min_year_e  # Ano mais antigo
max_year_e <- max(selected_e$year, na.rm = TRUE);max_year_e  # Ano mais recente

selected_p <- plant[plant$status == 'selected', ]
min_year_p <- min(selected_p$year, na.rm = TRUE);min_year_p  # Ano mais antigo
max_year_p <- max(selected_p$year, na.rm = TRUE);max_year_p  # Ano mais recente

selected_f <- forestry[forestry$status == 'selected', ]
min_year_f <- min(selected_f$year, na.rm = TRUE);min_year_f  # Ano mais antigo
max_year_f <- max(selected_f$year, na.rm = TRUE);max_year_f  # Ano mais recente

selected_s <- soil[soil$status == 'selected', ]
min_year_s <- min(selected_s$year, na.rm = TRUE);min_year_s  # Ano mais antigo
max_year_s <- max(selected_s$year, na.rm = TRUE);max_year_s  # Ano mais recente


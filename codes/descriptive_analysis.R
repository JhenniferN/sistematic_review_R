
########### análise descritiva ###########

# Carregar pacotes
library(readxl)
library(dplyr)
library(ggplot2)

# Ler os arquivos Excel
entomology <- read_excel("processed_entomology.xlsx") %>% mutate(area = "entomology")
plant <- read_excel("processed_plant.xlsx") %>% mutate(area = "plant science")
forestry <- read_excel("processed_forestry.xlsx") %>% mutate(area = "forestry")
soil <- read_excel("processed_soil.xlsx") %>% mutate(area = "soil science")

#
entomology <- entomology %>% mutate(jcr_impact_factor = as.character(jcr_impact_factor))
plant <- plant %>% mutate(jcr_impact_factor = as.character(jcr_impact_factor))
forestry <- forestry %>% mutate(jcr_impact_factor = as.character(jcr_impact_factor))
soil <- soil %>% mutate(jcr_impact_factor = as.character(jcr_impact_factor))

# Unificar os data frames
all_data <- bind_rows(entomology, plant, forestry, soil)

#head(all_data) # Visualizar os dados



######### visualização dos artigos selecionados ###########

####### grafico 1 - barras #######

# gráfico com 4 barras (uma para cada área), 
# cada barra é dividida em duas seções 
# artigos selecionados e descartados



# Criar um data frame resumido que mostra a quantidade de selecionados e descartados por área
status_summary <- all_data %>%
  group_by(area, status) %>%
  summarise(count = n()) %>%
  ungroup()

# Criar o gráfico de barras empilhadas com rótulos e tons pastéis
ggplot(status_summary, aes(x = area, y = count, fill = status)) +
  geom_bar(stat = "identity", position = "stack") +  # "stack" empilha os valores
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), size = 3) +  # Adiciona os rótulos no meio das barras
  scale_fill_manual(values = c("selected" = "#B3E5FC", "discarted" = "#EEE0E5")) +  # Cores pastéis
  labs(x = "Área",
       y = "Quantidade de Artigos",
       fill = "Status") +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # Remove a malha
        axis.title.y = element_blank(),  # Remove o título do eixo Y
        axis.text.y = element_blank(),   # Remove os textos (números) do eixo Y
        axis.ticks.y = element_blank())  # Remove os ticks (marcadores) do eixo Y



####### grafico 2- barras #######

# gráfico com 2 barras (artigos selecionados e descartados) 
# cada barra é dividida em 4 seções (uma para cada área)


# Contar quantos artigos foram selecionados e descartados por área
status_summary <- all_data %>%
  mutate(status = ifelse(status == "discarted", "discarded", status)) %>%  # Corrigir grafia
  group_by(area, status) %>%
  summarise(count = n()) %>%
  ungroup()

print(status_summary) # Exibir a tabela com a quantidade de artigos por área e status


# Criar o gráfico com duas barras estreitas, divididas por área, e adicionar rótulos
bar_plot <- ggplot(status_summary, aes(x = status, y = count, fill = area)) +
  geom_bar(stat = "identity", position = "stack", width = 0.4) +  # Barras estreitas empilhadas
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), size = 3) +  # Adiciona os rótulos no centro das barras
  scale_fill_manual(values = c("#B3E5FC", "#FFCCBC", "#C8E6C9", "#FFE082"),  # Definir cores para as áreas
                    name = "Área",
                    labels = c("entomology", "plant science", "forestry", "soil science")) +  # Etiquetas da legenda
  labs(x = "Status",
       y = "Quantidade de Artigos") +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # Remove a malha
        axis.title.y = element_blank(),  # Remove o título do eixo Y
        axis.text.y = element_blank(),   # Remove os textos (números) do eixo Y
        axis.ticks.y = element_blank())  # Remove os ticks (marcadores) do eixo Y

print(bar_plot)



####### grafico 3 - roscas #######

# gráfico roscas com os tipos de transformaçõe por área 


# Contar os tipos de transformação por área, filtrando apenas os artigos selecionados
transformation_summary <- all_data %>%
  filter(status == "selected") %>%  # Considerando apenas os artigos selecionados
  group_by(area, type_of_transformation) %>%
  summarise(count = n()) %>%
  ungroup()

#print(transformation_summary)  # transformações por área



colors <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f", "#e5c494", "#b3b3b3",
            "#1f78b4", "#33a02c", "#fb9a99", "#fdbf6f", "#cab2d6", "#6a3d9a", "#ffff99", "#b15928",
            "#B3E5FC", "#FFCCBC", "#C8E6C9", "#FFE082")


# Obter a lista de transformações únicas e mapear uma cor para cada uma
unique_transformations <- unique(transformation_summary$type_of_transformation)

# Criar um mapeamento de cores, garantindo que cada transformação tenha a mesma cor em todos os gráficos
transformation_colors <- setNames(colors[1:length(unique_transformations)], unique_transformations)

# Função para criar o gráfico de rosca para qualquer área
create_donut <- function(area_name) {
  data <- transformation_summary %>%
    filter(area == area_name) %>%
    mutate(percentage = count / sum(count) * 100)
  
  ggplot(data, aes(x = 2, y = count, fill = type_of_transformation)) +
    geom_col(width = 1) +  # Define a largura da rosca
    coord_polar(theta = "y") +  # Converte o gráfico de barras para rosca
    xlim(0.5, 2.5) +  # Controla o tamanho da rosca, deixando o centro oco
    theme_void() +  # Remove os elementos padrão do gráfico
    geom_text(aes(label = count),  # Adiciona os rótulos com as quantidades
              position = position_stack(vjust = 0.5), size = 3) +  # Posiciona os rótulos no centro de cada setor
    scale_fill_manual(values = transformation_colors) +  # Aplica as cores mapeadas para cada transformação
    labs(title = area_name) +  # Título da rosca
    theme(legend.position = "right")  # Coloca a legenda à direita
}

# Criar as roscas para cada área
donut_entomology <- create_donut("entomology")
donut_plant <- create_donut("plant science")
donut_forestry <- create_donut("forestry")
donut_soil <- create_donut("soil science")

# Exibir os gráficos de rosca
donut_entomology
donut_plant
donut_forestry
donut_soil






############### tabela com os motivos de descarte por área  ############


# Criar uma tabela com os motivos de descarte por área
discard_reasons_summary <- all_data %>%
  filter(status == "discarted") %>%  # Filtrar apenas os artigos descartados
  group_by(area, discard_reason) %>%
  summarise(count = n()) %>%
  ungroup()

discard_reasons_summary #Exibir a tabela de motivos de descarte por área

# Criar o gráfico de barras empilhadas
ggplot(discard_reasons_summary, aes(x = area, y = count, fill = discard_reason)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), color = "white") +
  labs(title = "Motivos de Descarte por Área",
       x = "Área",
       y = "Quantidade",
       fill = "Motivo de Descarte") +
  theme_minimal()





############### tabela as transformações por área  ############



transformations_by_area <- all_data %>%
  filter(status == "selected") %>%  # Filtrar apenas os artigos selecionados
  group_by(area, type_of_transformation) %>%
  summarise(count = n()) %>%
  ungroup()


transformations_by_area


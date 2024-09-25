# Carregar pacotes necessários
library(MASS)     # Para o boxcox
library(car)      # Para o teste de Levene e Johnson
library(purrr)    # Para mapear as simulações
library(tidyr)    # Para manipulação de dados

# Função para simular dados do experimento
simular_experimento <- function() {
  # 5 tratamentos, 3 blocos, e 3 repetições por tratamento dentro de cada bloco
  tratamento <- factor(rep(1:5, each = 9))  # 5 tratamentos, 9 observações (3 blocos * 3 repetições por tratamento)
  blocos <- factor(rep(1:3, each = 15))     # 3 blocos, 15 observações por bloco (5 tratamentos * 3 repetições por tratamento)
  
  # Simulação de dados com resíduos não normais e variâncias heterogêneas
  residuos <- c(rchisq(9, df = 1), rchisq(9, df = 2),
                rchisq(9, df = 3), rchisq(9, df = 4), rchisq(9, df = 5))
  
  resposta <- 5 + as.numeric(tratamento) + residuos
  dados <- data.frame(tratamento, blocos, resposta)
  
  modelo <- aov(resposta ~ tratamento + blocos, data = dados)
  residuos_modelo <- residuals(modelo)
  
  shapiro_test <- shapiro.test(residuos_modelo)
  levene_test <- leveneTest(resposta ~ tratamento, data = dados)
  
  pressupostos_rejeitados <- (shapiro_test$p.value < 0.05) | (levene_test$"Pr(>F)"[1] < 0.05)
  
  return(list(dados = dados, pressupostos_rejeitados = pressupostos_rejeitados))
}

# Função para aplicar transformações com tratamento de erros
aplicar_transformacoes <- function(dados) {
  # Corrigir valores negativos
  resposta_corrigida <- dados$resposta + abs(min(dados$resposta)) + 1 
  
  # Necessário criar um novo dados2 para rodar a função aov
  dados2 <- data.frame(resp_corrigida = resposta_corrigida,
                       trat = dados$tratamento, 
                       block =  dados$blocos)
  
  # Aplicar transformações
  log_transform <- log(resposta_corrigida)
  sqrt_transform <- sqrt(resposta_corrigida)
  arcsin_transform <- asin(sqrt((resposta_corrigida - min(resposta_corrigida)) / 
                                  (max(resposta_corrigida) - min(resposta_corrigida))))
  
  # Box-Cox
  boxcox_transform <- NULL
  johnson_transform <- NULL
  boxcox_success <- FALSE
  johnson_success <- FALSE
  
  # Tentar a transformação Box-Cox
  tryCatch({
    modelo_boxcox <- aov(resp_corrigida ~ trat + block, data = dados2)
    boxcox_trans <- boxcox(modelo_boxcox, plotit = FALSE, data = dados2)
    lambda <- boxcox_trans$x[which.max(boxcox_trans$y)]
    boxcox_transform <- ((resposta_corrigida ^ lambda) - 1) / lambda
    boxcox_success <- TRUE
  }, error = function(e) {
    message("Erro na transformação Box-Cox: ", e$message)
  })
  
  # Tentar a transformação Johnson
  tryCatch({
    johnson_transform <- as.numeric(car::powerTransform(resposta_corrigida, family = "yjPower")$y)
    johnson_success <- TRUE
  }, error = function(e) {
    message("Erro na transformação Johnson: ", e$message)
    johnson_success <- FALSE  # Marcar falha na transformação Johnson
  }, warning = function(w) {
    message("Aviso na transformação Johnson: ", w$message)
    johnson_success <- FALSE  # Marcar falha se houver um aviso de convergência
  })
  
  # Retornar lista de transformações, incluindo flags de sucesso
  return(list(
    log = log_transform, 
    sqrt = sqrt_transform, 
    arcsin = arcsin_transform, 
    boxcox = boxcox_transform, 
    johnson = johnson_transform, 
    resposta_corrigida = resposta_corrigida,
    boxcox_success = boxcox_success,
    johnson_success = johnson_success
  ))
}

# Função para testar pressupostos após transformações
testar_transformacoes <- function(transformacoes, dados) {
  resultados <- map(transformacoes[1:5], function(transf) {
    # Verificar se ambos os fatores têm ao menos dois níveis
    if (length(unique(dados$tratamento)) < 2 || length(unique(dados$blocos)) < 2) {
      message("Um dos fatores tem menos de 2 níveis. Ignorando este ajuste.")
      return(list(shapiro_p = NA, levene_p = NA))  # Retorna NA se não houver níveis suficientes
    }
    
    # Verificar se o vetor transformado (transf) tem variabilidade
    if (length(unique(transf)) < 2) {
      message("Transformação resultou em perda de variabilidade. Ignorando este ajuste.")
      return(list(shapiro_p = NA, levene_p = NA))  # Retorna NA se o vetor transformado não tiver variabilidade
    }
    
    # Ajustar modelo se houver mais de um nível em ambos os fatores e variabilidade no vetor
    modelo_transf <- tryCatch(
      {
        aov(transf ~ dados$tratamento + dados$blocos)
      },
      error = function(e) {
        message("Erro ao ajustar o modelo ANOVA: ", e$message)
        return(NULL)
      }
    )
    
    # Se o modelo não puder ser ajustado, retornar NA
    if (is.null(modelo_transf)) {
      return(list(shapiro_p = NA, levene_p = NA))
    }
    
    residuos_transf <- residuals(modelo_transf)
    
    # Teste de Shapiro-Wilk para normalidade dos resíduos
    shapiro_test <- shapiro.test(residuos_transf)
    
    # Teste de Levene para homogeneidade de variâncias
    levene_test <- tryCatch(
      {
        leveneTest(transf ~ dados$tratamento, data = dados)
      },
      error = function(e) {
        message("Erro no teste de Levene: ", e$message)
        return(NULL)
      }
    )
    
    # Se o teste de Levene falhar, retornar NA
    if (is.null(levene_test)) {
      return(list(shapiro_p = shapiro_test$p.value, levene_p = NA))
    }
    
    return(list(shapiro_p = shapiro_test$p.value, levene_p = levene_test$"Pr(>F)"[1]))
  })
  
  return(resultados)
}

# Função principal de simulação modificada
simular_transformacoes <- function(iteracoes = 1000) {
  sucesso_transformacoes <- list(log = 0, 
                                 sqrt = 0, 
                                 arcsin = 0, 
                                 boxcox = 0, 
                                 johnson = 0)
  falhas_transformacoes <- list(log = 0, 
                                sqrt = 0, 
                                arcsin = 0, 
                                boxcox = 0, 
                                johnson = 0)
  
  # Novas listas para rastrear sucessos individuais
  sucesso_normalizacao <- list(log = 0, 
                               sqrt = 0, 
                               arcsin = 0, 
                               boxcox = 0, 
                               johnson = 0)
  sucesso_homogenizacao <- list(log = 0, 
                                sqrt = 0, 
                                arcsin = 0, 
                                boxcox = 0, 
                                johnson = 0)
  
  rejeicoes_iniciais <- 0
  tempo_inicio <- Sys.time()
  
  while (rejeicoes_iniciais < iteracoes) {
    simulacao <- simular_experimento()
    
    if (simulacao$pressupostos_rejeitados) {
      rejeicoes_iniciais <- rejeicoes_iniciais + 1
      
      repeat {
        # Aplicar transformações
        transformacoes <- aplicar_transformacoes(simulacao$dados)
        
        # Verificar se as transformações Box-Cox e Johnson foram bem-sucedidas
        if (transformacoes$boxcox_success && transformacoes$johnson_success) {
          
          # Verificar se os fatores ainda têm mais de um nível
          if (length(unique(simulacao$dados$tratamento)) >= 2 && length(unique(simulacao$dados$blocos)) >= 2) {
            # Verificar se a variabilidade foi mantida nas transformações
            if (length(unique(transformacoes$log)) >= 2 &&
                length(unique(transformacoes$sqrt)) >= 2 &&
                length(unique(transformacoes$boxcox)) >= 2 &&
                length(unique(transformacoes$johnson)) >= 2) {
              break  # Se as condições forem satisfeitas, sair do loop e prosseguir
            }
          }
        }
        
        # Se alguma condição falhar, refazer a simulação
        message("Falha em Box-Cox, Johnson, ou perda de variabilidade. Re-simulando dados.")
        simulacao <- simular_experimento()
      }
      
      # Testar transformações
      testes_resultado <- testar_transformacoes(transformacoes, simulacao$dados)
      
      for (i in seq_along(testes_resultado)) {
        teste <- testes_resultado[[i]]
        nome_transformacao <- names(sucesso_transformacoes)[i]
        
        # Verificar se os testes não retornaram NA
        if (!is.na(teste$shapiro_p) && !is.na(teste$levene_p)) {
          # Verificar normalização
          if (teste$shapiro_p > 0.05) {
            sucesso_normalizacao[[nome_transformacao]] <- sucesso_normalizacao[[nome_transformacao]] + 1
          }
          
          # Verificar homogenização
          if (teste$levene_p > 0.05) {
            sucesso_homogenizacao[[nome_transformacao]] <- sucesso_homogenizacao[[nome_transformacao]] + 1
          }
          
          # Verificar se ambos passaram
          if (teste$shapiro_p > 0.05 & teste$levene_p > 0.05) {
            sucesso_transformacoes[[nome_transformacao]] <- sucesso_transformacoes[[nome_transformacao]] + 1
          } else {
            falhas_transformacoes[[nome_transformacao]] <- falhas_transformacoes[[nome_transformacao]] + 1
          }
        }
      }
    }
  }
  
  tempo_fim <- Sys.time()
  duracao <- tempo_fim - tempo_inicio
  
  print(paste("Tempo total de simulação:", duracao))
  
  print("Sucessos por transformação (Normalização e Homogenização):")
  print(sucesso_transformacoes)
  
  print("Falhas por transformação:")
  print(falhas_transformacoes)
  
  print("Sucessos por transformação (Normalização individual):")
  print(sucesso_normalizacao)
  
  print("Sucessos por transformação (Homogenização individual):")
  print(sucesso_homogenizacao)
  
  return(list(
    sucessos = sucesso_transformacoes, 
    falhas = falhas_transformacoes,
    sucessos_normalizacao = sucesso_normalizacao,
    sucessos_homogenizacao = sucesso_homogenizacao
  ))
}

# Rodar a simulação para 1000 rejeições de pressupostos
set.seed(123)
resultados_finais <- simular_transformacoes(1000)

# Taxa de sucesso de cada transformação para ambos os pressupostos
taxa_sucesso_combinado <- map_dbl(resultados_finais$sucessos, function(sucessos) sucessos / 1000)
taxa_sucesso_normalizacao <- map_dbl(resultados_finais$sucessos_normalizacao, function(sucessos) sucessos / 1000)
taxa_sucesso_homogenizacao <- map_dbl(resultados_finais$sucessos_homogenizacao, function(sucessos) sucessos / 1000)

print("Taxa de sucesso combinada (Normalização e Homogenização):")
print(taxa_sucesso_combinado)

print("Taxa de sucesso para normalização apenas:")
print(taxa_sucesso_normalizacao)

print("Taxa de sucesso para homogenização apenas:")
print(taxa_sucesso_homogenizacao)

# ============================================================================
# INSTALAÇÃO DE PACOTES
# ============================================================================
# NOTA: Mensagens de "foi compilado no R versão X.X.X" são apenas avisos
# informativos e não impedem a execução do código. Ignore-as com segurança.
# ============================================================================

# Lista de pacotes necessários
pacotes <- c("tm", "reshape2", "ggplot2", "igraph", "widyr", 
             "dplyr", "ggraph", "lexiconPT", "topicmodels", 
             "tidyverse", "stringr", "tidytext", "stm", 
             "ggridges", "udpipe", "lattice", "wordcloud",
             "writexl", "openxlsx", "SnowballC")

# Instalar pacotes que ainda não estão instalados
novos_pacotes <- pacotes[!(pacotes %in% installed.packages()[,"Package"])]
if(length(novos_pacotes)) install.packages(novos_pacotes)

# Carregar todos os pacotes
library(tm)
library(reshape2)
library(ggplot2)
library(igraph)
library(widyr)
library(dplyr)
library(ggraph)
library(lexiconPT)
library(topicmodels)
library(tidyverse)      
library(stringr)
library(tidytext)
library(stm)
library(ggridges)
library(udpipe)
library(lattice)
library(wordcloud)
library(writexl)
library(openxlsx)
library(SnowballC)

# ============================================================================
# PREPARAÇÃO DOS DADOS
# ============================================================================

# Função para criar corpus
createCorpus <- function(filepath) {
  conn <- file(filepath, "r")
  fulltext <- readLines(conn)
  close(conn)
  
  vs <- VectorSource(fulltext)
  VCorpus(vs, readerControl=list(readPlain, language="pt", load=TRUE))
}

# Criar corpus
YourCorpus <- createCorpus("SOCIEDADE_CIVIL.txt")

# Palavras a remover
x <- c("sao", "nao", "pra", "assim",
       "pra","entao","porem","ter","fazer","voce","porque","vai",
       "acho","ate","aqui","coisa","tambem","ser",
       "sei","vou", "dizer","tipo","quer","sim", "sabe","cara",
       "gente", "ne", "ai","la","ta","ja","tao","bem","que","para","uma","como",
       "com","mim", "projeto", "objetivo", "realizacao")

# ============================================================================
# FUNÇÃO PARA REMOVER PLURAIS EM PORTUGUÊS
# ============================================================================

remover_plurais <- function(texto) {
  # Converter para minúsculas
  texto <- tolower(texto)
  
  # Regras de pluralização em português (aplicadas ao contrário)
  texto <- gsub("\\b(\\w+)oes\\b", "\\1ao", texto)
  texto <- gsub("\\b(\\w+)aes\\b", "\\1ao", texto)
  texto <- gsub("\\b(\\w+)aos\\b", "\\1ao", texto)
  texto <- gsub("\\b(\\w+)eis\\b", "\\1el", texto)
  texto <- gsub("\\b(\\w{2,})is\\b", "\\1il", texto)
  texto <- gsub("\\b(\\w+)res\\b", "\\1r", texto)
  texto <- gsub("\\b(\\w+)ses\\b", "\\1s", texto)
  texto <- gsub("\\b(\\w+)zes\\b", "\\1z", texto)
  texto <- gsub("\\b(\\w{3,})as\\b", "\\1a", texto)
  texto <- gsub("\\b(\\w{3,})os\\b", "\\1o", texto)
  texto <- gsub("\\b(\\w{3,})es\\b", "\\1e", texto)
  
  return(texto)
}

# ============================================================================
# PRÉ-PROCESSAMENTO DE DADOS
# ============================================================================

Corpus_proc <- tm_map(YourCorpus, content_transformer(tolower))
Corpus_proc <- tm_map(Corpus_proc, removePunctuation)
Corpus_proc <- tm_map(Corpus_proc, removeWords, stopwords(kind="pt"))
Corpus_proc <- tm_map(Corpus_proc, removeWords, x)
Corpus_proc <- tm_map(Corpus_proc, stripWhitespace)
Corpus_proc <- tm_map(Corpus_proc, content_transformer(remover_plurais))

# ============================================================================
# ANÁLISE EXPLORATÓRIA
# ============================================================================

# Criar matriz de termos e documentos
dtm <- DocumentTermMatrix(Corpus_proc)
dtm.matrix <- as.matrix(dtm)
wordcount <- colSums(dtm.matrix)
topten <- head(sort(wordcount, decreasing=TRUE), 30)

# Nuvem de palavras
wordcloud(Corpus_proc, max.words=25, scale = c(4, 0.5),
          colors=c("blue","red","green","black"))

# Gráfico de frequência
dfplot <- as.data.frame(melt(topten))
dfplot$word <- dimnames(dfplot)[[1]]
dfplot$word <- factor(dfplot$word,
                      levels=dfplot$word[order(dfplot$value,
                                               decreasing=TRUE)])

fig <- ggplot(dfplot, aes(x=word, y=value)) + geom_bar(stat="identity")
fig <- fig + xlab("Word in Corpus")
fig <- fig + ylab("Count")
print(fig)

# ============================================================================
# PREPARAÇÃO PARA STRUCTURAL TOPIC MODELING
# ============================================================================

# Preparar stopwords
sw_pt_tm <- tm::stopwords("pt") %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
sw_pt <- c(x, sw_pt_tm)

# Converter para tidy
t_corpus <- Corpus_proc %>% tidy()
d_corpus <- t_corpus %>% select(id, text)

# Processar texto SEM stemming
proc <- stm::textProcessor(d_corpus$text, language = "portuguese",
                           customstopwords = sw_pt_tm, stem=FALSE)

# Preparar documentos
out <- stm::prepDocuments(proc$documents, proc$vocab, proc$meta,
                          lower.thresh = 3)

# Verificar dados
cat("\n========================================================\n")
cat("INFORMAÇÕES DO CORPUS\n")
cat("========================================================\n")
cat("Número de documentos:", length(out$documents), "\n")
cat("Tamanho do vocabulário:", length(out$vocab), "\n")
cat("\n")

# ============================================================================
# BUSCAR NÚMERO IDEAL DE TÓPICOS (searchK)
# ============================================================================

cat("========================================================\n")
cat("INICIANDO BUSCA DO NÚMERO IDEAL DE TÓPICOS\n")
cat("========================================================\n")
cat("Este processo pode demorar alguns minutos...\n\n")

# Executar searchK UMA VEZ
storage <- stm::searchK(
  out$documents, 
  out$vocab, 
  K = c(3:20),  # Testa K de 3 até 10
  data = out$meta,
  init.type = "Spectral",
  seed = 12345,
  verbose = TRUE
)

# Visualizar resultados
cat("\n========================================================\n")
cat("RESULTADOS DA BUSCA\n")
cat("========================================================\n")
print(storage$results)

# Criar gráfico detalhado
plot(storage)

# Salvar gráfico
png("diagnostico_searchK.png", width = 1200, height = 800, res = 120)
plot(storage)
dev.off()

cat("\n========================================================\n")
cat("INTERPRETAÇÃO DOS GRÁFICOS:\n")
cat("========================================================\n")
cat("1. Held-Out Likelihood: MAIOR é melhor (qualidade preditiva)\n")
cat("2. Residuals: MENOR é melhor (qualidade do ajuste)\n")
cat("3. Semantic Coherence: MAIOR é melhor (coerência dos tópicos)\n")
cat("4. Lower Bound: MAIOR é melhor (log-verossimilhança)\n")
cat("\n")
cat("Recomendação: Escolha K que equilibre:\n")
cat("  - Alta coerência semântica\n")
cat("  - Baixos resíduos\n")
cat("  - Interpretabilidade (nem muito poucos nem muitos tópicos)\n")
cat("\n")

# ============================================================================
# SELEÇÃO MANUAL DE K
# ============================================================================

cat("========================================================\n")
cat("SELEÇÃO DO NÚMERO DE TÓPICOS\n")
cat("========================================================\n")
cat("Analise o gráfico 'diagnostico_searchK.png' gerado.\n")
cat("\n")

# AQUI VOCÊ DEFINE O K ESCOLHIDO
# Altere este valor baseado na análise dos gráficos
K_ESCOLHIDO <- 20  # <--- ALTERE ESTE NÚMERO APÓS ANALISAR OS GRÁFICOS

cat("Número de tópicos selecionado:", K_ESCOLHIDO, "\n")
cat("\n")

# ============================================================================
# CRIAR MODELO COM K ESCOLHIDO (UMA VEZ APENAS)
# ============================================================================

cat("========================================================\n")
cat("CRIANDO MODELO STM\n")
cat("========================================================\n")
cat("K =", K_ESCOLHIDO, "\n")
cat("Aguarde...\n\n")

# Criar modelo UMA ÚNICA VEZ
fit <- stm(
  documents = out$documents, 
  vocab = out$vocab, 
  data = out$meta, 
  K = K_ESCOLHIDO,
  max.em.its = 75, 
  init.type = "Spectral",
  seed = 12345,
  verbose = TRUE
)

cat("\n✓ Modelo criado com sucesso!\n\n")

# Visualizar sumário
plot(fit, "summary")

# Extrair labels
labels <- stm::labelTopics(fit, n=7)
cat("\n=== PALAVRAS COM MAIOR PROBABILIDADE ===\n")
print(labels$prob)
cat("\n=== PALAVRAS FREX (Frequência + Exclusividade) ===\n")
print(labels$frex)

# ============================================================================
# TAXONOMIA ODS - DICIONÁRIO DE PALAVRAS-CHAVE
# ============================================================================

taxonomia_ods <- list(
  ODS_1 = list(
    nome = "Erradicação da Pobreza",
    palavras_chave = c("pobreza", "pobre", "renda", "vulneravel", "vulnerabilidade", 
                       "desigualdade", "necessidade", "basico", "minimo", "assistencia",
                       "carencia", "exclusao", "miseria", "faminto", "indigencia",
                       "precario", "necessitado", "subsistencia"),
    metas = "1.1, 1.2, 1.3, 1.4, 1.5"
  ),
  ODS_2 = list(
    nome = "Fome Zero e Agricultura Sustentável",
    palavras_chave = c("fome", "alimento", "alimentacao", "nutricao", "agricultura", 
                       "agricola", "producao", "seguranca", "alimentar", "comida",
                       "desnutricao", "cultivo", "plantio", "colheita", "safra",
                       "agricultor", "rural", "campo", "lavoura", "agrario"),
    metas = "2.1, 2.2, 2.3, 2.4"
  ),
  ODS_3 = list(
    nome = "Saúde e Bem-estar",
    palavras_chave = c("saude", "doenca", "medico", "hospital", "tratamento", 
                       "prevencao", "vacina", "mental", "fisica", "bem-estar",
                       "atendimento", "sus", "enfermeiro", "clinica", "medicina",
                       "paciente", "consulta", "exame", "diagnostico", "cuidado",
                       "terapia", "psicologico", "epidemia", "pandemia", "mortalidade"),
    metas = "3.1, 3.2, 3.3, 3.4, 3.8, 3.9"
  ),
  ODS_4 = list(
    nome = "Educação de Qualidade",
    palavras_chave = c("educacao", "escola", "ensino", "aprendizagem", "professor", 
                       "aluno", "estudante", "universidade", "conhecimento", "formacao",
                       "pedagogico", "didatico", "curriculo", "academico", "letramento",
                       "alfabetizacao", "capacitacao", "treinamento", "curso", "aula",
                       "educacional", "escolar", "docente", "discente", "aprender"),
    metas = "4.1, 4.3, 4.4, 4.5, 4.6, 4.7"
  ),
  ODS_5 = list(
    nome = "Igualdade de Gênero",
    palavras_chave = c("genero", "mulher", "feminino", "igualdade", "equidade",
                       "discriminacao", "violencia", "empoderamento", "direito",
                       "feminista", "machismo", "patriarcal", "sexismo", "feminina",
                       "masculino", "sexo", "domestica", "assedio", "lgbt", "diversidade"),
    metas = "5.1, 5.2, 5.4, 5.5, 5.6"
  ),
  ODS_6 = list(
    nome = "Água Potável e Saneamento",
    palavras_chave = c("agua", "saneamento", "esgoto", "higiene", "potavel",
                       "hidrico", "tratamento", "abastecimento", "hidrica",
                       "encanamento", "torneira", "poluicao", "contaminacao",
                       "hidrica", "aquatico", "fluvial", "manancial", "hidrologico"),
    metas = "6.1, 6.2, 6.3, 6.4, 6.6"
  ),
  ODS_7 = list(
    nome = "Energia Limpa e Acessível",
    palavras_chave = c("energia", "eletricidade", "renovavel", "solar", "eolica",
                       "eletrica", "sustentavel", "limpa", "combustivel", "geracao",
                       "eletrico", "fonte", "hidrica", "biomassa", "hidreletrica",
                       "fotovoltaica", "eolico", "energetico", "potencia"),
    metas = "7.1, 7.2, 7.3"
  ),
  ODS_8 = list(
    nome = "Trabalho Decente e Crescimento Econômico",
    palavras_chave = c("trabalho", "emprego", "economico", "crescimento", "renda",
                       "empregado", "trabalhador", "salario", "economia", "producao",
                       "produtividade", "desenvolvimento", "mercado", "empreendedor",
                       "empresa", "negocio", "empregabilidade", "profissional", "carreira",
                       "ocupacao", "empregador", "trabalhista", "desemprego", "recessao"),
    metas = "8.1, 8.2, 8.3, 8.5, 8.6, 8.8"
  ),
  ODS_9 = list(
    nome = "Indústria, Inovação e Infraestrutura",
    palavras_chave = c("industria", "inovacao", "infraestrutura", "tecnologia",
                       "pesquisa", "desenvolvimento", "industrial", "construcao",
                       "transporte", "comunicacao", "estrada", "internet", "rede",
                       "ciencia", "cientifica", "tecnico", "engenharia", "inovador",
                       "tecnologico", "modernizacao", "digitalizacao", "conectividade"),
    metas = "9.1, 9.2, 9.4, 9.5, 9.c"
  ),
  ODS_10 = list(
    nome = "Redução das Desigualdades",
    palavras_chave = c("desigualdade", "inclusao", "discriminacao", "diversidade",
                       "igualdade", "equidade", "justica", "direito", "oportunidade",
                       "acesso", "exclusao", "marginalizado", "vulneravel", "minoria",
                       "preconceito", "segregacao", "disparidade", "diferenca", "distancia"),
    metas = "10.1, 10.2, 10.3, 10.4"
  ),
  ODS_11 = list(
    nome = "Cidades e Comunidades Sustentáveis",
    palavras_chave = c("cidade", "urbano", "comunidade", "moradia", "habitacao",
                       "transporte", "mobilidade", "planejamento", "sustentavel",
                       "municipio", "bairro", "residencial", "urbana", "metropolitano",
                       "publico", "cidadao", "territorial", "espacial", "zona"),
    metas = "11.1, 11.2, 11.3, 11.6, 11.7"
  ),
  ODS_12 = list(
    nome = "Consumo e Produção Responsáveis",
    palavras_chave = c("consumo", "producao", "residuo", "lixo", "reciclagem",
                       "desperdicio", "sustentavel", "circular", "descarte",
                       "reutilizacao", "consciente", "responsavel", "reaproveitamento",
                       "compostagem", "coleta", "descartavel", "recurso"),
    metas = "12.2, 12.3, 12.4, 12.5, 12.8"
  ),
  ODS_13 = list(
    nome = "Ação Contra a Mudança Global do Clima",
    palavras_chave = c("clima", "climatica", "aquecimento", "emissao", "carbono",
                       "ambiental", "mudanca", "global", "temperatura", "gase",
                       "efeito", "estufa", "co2", "climatico", "atmosferico",
                       "meteorologico", "precipitacao", "seca", "enchente"),
    metas = "13.1, 13.2, 13.3"
  ),
  ODS_14 = list(
    nome = "Vida na Água",
    palavras_chave = c("oceano", "mar", "marinha", "maritimo", "pesca",
                       "aquatico", "costeiro", "praia", "peixe", "biodiversidade",
                       "coral", "maritima", "marinho", "pesqueiro", "litoraneo",
                       "oceanico", "recife", "mangue"),
    metas = "14.1, 14.2, 14.4, 14.5"
  ),
  ODS_15 = list(
    nome = "Vida Terrestre",
    palavras_chave = c("floresta", "desmatamento", "biodiversidade", "ecossistema",
                       "conservacao", "ambiental", "fauna", "flora", "preservacao",
                       "arvore", "solo", "terrestre", "natureza", "vegetal", "animal",
                       "silvestre", "bioma", "mata", "verde", "reflorestamento",
                       "recuperacao", "degradacao", "erosao"),
    metas = "15.1, 15.2, 15.3, 15.5"
  ),
  ODS_16 = list(
    nome = "Paz, Justiça e Instituições Eficazes",
    palavras_chave = c("justica", "paz", "violencia", "direito", "institucional",
                       "governanca", "corrupcao", "legal", "lei", "tribunal",
                       "judiciario", "crime", "seguranca", "publico", "politica",
                       "democracia", "transparencia", "participacao", "cidadania",
                       "etico", "integridade", "accountability", "estado"),
    metas = "16.1, 16.3, 16.5, 16.6, 16.7, 16.10"
  ),
  ODS_17 = list(
    nome = "Parcerias e Meios de Implementação",
    palavras_chave = c("parceria", "cooperacao", "internacional", "colaboracao",
                       "alianca", "apoio", "financiamento", "investimento",
                       "tecnologia", "capacitacao", "global", "multilateral",
                       "conjunto", "compartilhado", "rede", "articulacao",
                       "parceiro", "associacao", "convenio", "acordo", "bilateral",
                       "publico-privado", "terceiro-setor", "ong", "sociedade-civil",
                       "governamental", "intersetorial", "transnacional"),
    metas = "17.1, 17.3, 17.6, 17.9, 17.16, 17.17, 17.19"
  )
)

# ============================================================================
# FUNÇÃO PARA MAPEAR TÓPICOS A ODS
# ============================================================================

mapear_topico_ods <- function(palavras_topico, taxonomia, top_n = 3) {
  palavras_topico <- tolower(palavras_topico)
  
  scores <- sapply(names(taxonomia), function(ods) {
    palavras_ods <- taxonomia[[ods]]$palavras_chave
    matches <- sum(sapply(palavras_ods, function(palavra) {
      any(grepl(palavra, palavras_topico, fixed = TRUE))
    }))
    return(matches)
  })
  
  ods_ordenadas <- names(sort(scores, decreasing = TRUE))
  scores_ordenados <- sort(scores, decreasing = TRUE)
  
  ods_relevantes <- ods_ordenadas[scores_ordenados > 0][1:min(top_n, sum(scores_ordenados > 0))]
  
  if(length(ods_relevantes) == 0 || is.na(ods_relevantes[1])) {
    ods_relevantes <- ods_ordenadas[1:top_n]
  }
  
  return(ods_relevantes[!is.na(ods_relevantes)])
}

# ============================================================================
# MAPEAR TÓPICOS PARA ODS
# ============================================================================

num_topicos <- fit$settings$dim$K
labels <- stm::labelTopics(fit, n=10)

mapeamento_topicos_ods <- data.frame()

for(i in 1:num_topicos) {
  n_palavras_disponiveis <- ncol(labels$frex)
  n_usar <- min(10, n_palavras_disponiveis)
  
  palavras_frex <- labels$frex[i, 1:n_usar]
  palavras_prob <- labels$prob[i, 1:n_usar]
  todas_palavras <- c(palavras_frex, palavras_prob)
  
  ods_mapeadas <- mapear_topico_ods(todas_palavras, taxonomia_ods, top_n = 3)
  
  ods_principal <- as.numeric(gsub("ODS_", "", ods_mapeadas[1]))
  ods_secundarias <- if(length(ods_mapeadas) > 1) {
    paste(as.numeric(gsub("ODS_", "", ods_mapeadas[2:length(ods_mapeadas)])), collapse = ", ")
  } else {
    ""
  }
  
  nome_ods <- taxonomia_ods[[ods_mapeadas[1]]]$nome
  metas <- taxonomia_ods[[ods_mapeadas[1]]]$metas
  
  n_palavras_nome <- min(3, n_usar)
  nome_topico <- paste0("T", i, ": ", paste(palavras_frex[1:n_palavras_nome], collapse = ", "))
  
  n_palavras_chave <- min(7, n_usar)
  palavras_chave_str <- paste(palavras_frex[1:n_palavras_chave], collapse = ", ")
  
  mapeamento_topicos_ods <- rbind(mapeamento_topicos_ods, data.frame(
    topico = i,
    nome_topico = nome_topico,
    palavras_chave = palavras_chave_str,
    ods_principal = ods_principal,
    nome_ods_principal = nome_ods,
    ods_secundarias = ods_secundarias,
    metas_principais = metas,
    stringsAsFactors = FALSE
  ))
}

cat("\n=== MAPEAMENTO DE TÓPICOS PARA ODS ===\n")
print(mapeamento_topicos_ods)

# ============================================================================
# CRIAR DATAFRAME FINAL
# ============================================================================

df_resultados <- data.frame(
  id_documento = 1:length(out$documents),
  texto_original = d_corpus$text
)

for(i in 1:num_topicos) {
  df_resultados[,paste0("prob_topico_", i)] <- fit$theta[,i]
}

df_resultados$topico_principal <- apply(fit$theta, 1, which.max)
df_resultados$probabilidade_maxima <- apply(fit$theta, 1, max)

df_final <- df_resultados %>%
  left_join(mapeamento_topicos_ods, by = c("topico_principal" = "topico"))

df_final <- df_final %>%
  select(
    id_documento,
    topico_principal,
    nome_topico,
    probabilidade_maxima,
    ods_principal,
    nome_ods_principal,
    ods_secundarias,
    metas_principais,
    palavras_chave,
    texto_original,
    everything()
  )

# ============================================================================
# ANÁLISES COMPLEMENTARES
# ============================================================================

resumo_topicos <- df_final %>%
  group_by(topico_principal, nome_topico, ods_principal, 
           nome_ods_principal, ods_secundarias, metas_principais) %>%
  summarise(
    quantidade_documentos = n(),
    prob_media = mean(probabilidade_maxima),
    palavras_chave = first(palavras_chave),
    .groups = 'drop'
  ) %>%
  arrange(desc(quantidade_documentos))

resumo_ods <- df_final %>%
  group_by(ods_principal, nome_ods_principal) %>%
  summarise(
    quantidade_documentos = n(),
    topicos = paste(unique(nome_topico), collapse = "; "),
    .groups = 'drop'
  ) %>%
  arrange(desc(quantidade_documentos))

# ============================================================================
# VISUALIZAÇÕES
# ============================================================================

ggplot(resumo_ods, aes(x = reorder(paste0("ODS ", ods_principal), quantidade_documentos), 
                       y = quantidade_documentos, 
                       fill = factor(ods_principal))) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Distribuição de Documentos por ODS",
    x = "ODS",
    y = "Número de Documentos",
    fill = "ODS"
  ) +
  scale_fill_viridis_d(option = "C") +
  theme(legend.position = "none")

ggsave("grafico_distribuicao_ods.png", width = 12, height = 8, dpi = 300)

ggplot(resumo_topicos, aes(x = reorder(nome_topico, quantidade_documentos), 
                           y = quantidade_documentos,
                           fill = factor(ods_principal))) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Distribuição de Documentos por Tópico e ODS",
    x = "Tópico",
    y = "Número de Documentos",
    fill = "ODS Principal"
  ) +
  scale_fill_viridis_d(option = "C")

ggsave("grafico_topicos_ods.png", width = 14, height = 8, dpi = 300)

matriz_prob <- as.matrix(df_final[, grep("^prob_topico_", names(df_final))])
colnames(matriz_prob) <- paste0("T", 1:ncol(matriz_prob))
rownames(matriz_prob) <- paste0("Doc", 1:nrow(matriz_prob))

matriz_long <- melt(matriz_prob)
colnames(matriz_long) <- c("Documento", "Topico", "Probabilidade")

ggplot(matriz_long, aes(x = Topico, y = Documento, fill = Probabilidade)) +
  geom_tile() +
  scale_fill_viridis_c(option = "D") +
  theme_minimal() +
  labs(
    title = "Heatmap: Probabilidade de Tópicos por Documento",
    x = "Tópico",
    y = "Documento"
  )

ggsave("heatmap_topicos_documentos.png", width = 10, height = 12, dpi = 300)

# ============================================================================
# EXPORTAÇÃO PARA EXCEL
# ============================================================================

wb <- createWorkbook()

addWorksheet(wb, "Resultados Completos")
writeData(wb, "Resultados Completos", df_final)

addWorksheet(wb, "Resumo por Tópico")
writeData(wb, "Resumo por Tópico", resumo_topicos)

addWorksheet(wb, "Resumo por ODS")
writeData(wb, "Resumo por ODS", resumo_ods)

addWorksheet(wb, "Mapeamento Tópicos-ODS")
writeData(wb, "Mapeamento Tópicos-ODS", mapeamento_topicos_ods)

df_matriz_completa <- df_final %>%
  select(id_documento, topico_principal, ods_principal, starts_with("prob_topico_"))
addWorksheet(wb, "Matriz Probabilidades")
writeData(wb, "Matriz Probabilidades", df_matriz_completa)

taxonomia_df <- data.frame()
for(ods in names(taxonomia_ods)) {
  taxonomia_df <- rbind(taxonomia_df, data.frame(
    ODS = gsub("ODS_", "", ods),
    Nome = taxonomia_ods[[ods]]$nome,
    Palavras_Chave = paste(taxonomia_ods[[ods]]$palavras_chave, collapse = ", "),
    Metas = taxonomia_ods[[ods]]$metas,
    stringsAsFactors = FALSE
  ))
}
addWorksheet(wb, "Taxonomia ODS")
writeData(wb, "Taxonomia ODS", taxonomia_df)

saveWorkbook(wb, "analise_completa_topicos_ods.xlsx", overwrite = TRUE)

# ============================================================================
# EXPORTAÇÕES INDIVIDUAIS
# ============================================================================

write_xlsx(df_final, "resultados_documentos_ods.xlsx")
write_xlsx(resumo_topicos, "resumo_topicos.xlsx")
write_xlsx(resumo_ods, "resumo_ods.xlsx")
write_xlsx(mapeamento_topicos_ods, "mapeamento_topicos_ods.xlsx")

# ============================================================================
# RELATÓRIO FINAL
# ============================================================================

cat("\n")
cat("========================================================\n")
cat("           ANÁLISE CONCLUÍDA COM SUCESSO!               \n")
cat("========================================================\n")
cat("\n")
cat("K escolhido:", K_ESCOLHIDO, "\n")
cat("Total de documentos analisados:", nrow(df_final), "\n")
cat("Número de tópicos identificados:", num_topicos, "\n")
cat("\n")
cat("ODS IDENTIFICADAS:\n")
print(table(df_final$ods_principal))
cat("\n")
cat("ARQUIVOS GERADOS:\n")
cat("  1. diagnostico_searchK.png (gráficos de diagnóstico)\n")
cat("  2. analise_completa_topicos_ods.xlsx (arquivo completo)\n")
cat("  3. resultados_documentos_ods.xlsx\n")
cat("  4. resumo_topicos.xlsx\n")
cat("  5. resumo_ods.xlsx\n")
cat("  6. mapeamento_topicos_ods.xlsx\n")
cat("  7. grafico_distribuicao_ods.png\n")
cat("  8. grafico_topicos_ods.png\n")
cat("  9. heatmap_topicos_documentos.png\n")
cat("\n")
cat("========================================================\n")

cat("\nPREVIEW - Primeiros documentos:\n")
print(head(df_final %>% select(id_documento, topico_principal, ods_principal, 
                               nome_ods_principal, probabilidade_maxima), 10))

cat("\nPREVIEW - Resumo por ODS:\n")
print(resumo_ods)


####################### INSTRUÇÕES #######################

source(paste("./R/0_Library.R", sep = ""))

################## PARTE II - EMPRESAS ##################
# Carregando um dataset de empresas lista na CVM. Depois de filtrado os segmentos 
# e escolhido qual será utilizado para trabalhado, ocorrendo a exportação do arquivo 'export_cia_segmento.csv', para 
# que seja feito uma nova classificação, seguindo modelo adotado pela BOVESPA.
# Será carregado o novo arquivo 'cia_construcao.xlsx' para ser utilizado como ferramenta nos próximos tratamentos

#carregar arquivos de cadastro das empresas
dir_cadastro <- file.path("./Dados_CVM/Empresas/cad_cia_aberta.csv")
cadastro <- read.csv(dir_cadastro, sep = ";", fileEncoding = "ISO-8859-1", stringsAsFactors = FALSE)

# Exibindo nome das colunas do cabeçalho
colnames(cadastro)

# Mantendo somente as colunas necessárias
colunas_manter <- c("CNPJ_CIA",           "DENOM_SOCIAL", "DENOM_COMERC", 
                    "DT_CONST",           "SIT",          "CD_CVM", 
                    "SETOR_ATIV",         "TP_MERC",      "CATEG_REG", 
                    "CONTROLE_ACIONARIO", "MUN",          "UF", 
                    "PAIS")
cadastro <- cadastro[, colunas_manter]

head(cadastro,2)
unique(cadastro$SETOR_ATIV) # conhecer os setores do dataset
condicao_setor <- cadastro$SETOR_ATIV %in% c("Petróleo e Gás")
# filtragens para reduzir o numero de empresas: SIT = ATIVO, TP_MERC não é BALCÃO ORGANIZADO
condicao_sit <- cadastro$SIT == "ATIVO"
condicao_tp_merc <- cadastro$TP_MERC == "BOLSA"
cadastro_filtrados <- subset(cadastro, condicao_setor & condicao_sit & condicao_tp_merc)

# Imprimindo os resultados
print(cadastro_filtrados$DENOM_SOCIAL)
print(cadastro_filtrados$DENOM_COMERC)

# Criando um vetor EMPRESA e incluindo no dataframe
nome_empresa <- c("3R PETROLEUM",
                  "OCEANPACT",
                  "PETROBRAS",
                  "PETRO RIO",
                  "REFINARIA DE PETRÓLEOS DE MANGUINHOS")
cadastro_filtrados$DENOM_CIA <- nome_empresa
print(cadastro_filtrados)

# Definindo somente Petrobras como fonte de dados para filtragens futuras
empresas <- subset(cadastro_filtrados, DENOM_CIA == "PETROBRAS")

################## DEMONSTRACOES FINANCEIRAS #########
# definir os diretórios onde estão os arquivos CSV

dir_BP <- file.path("./Dados_CVM/DemonstracoesFinanceiras/BP")
dir_DFC_MD <- file.path("./Dados_CVM/DemonstracoesFinanceiras/DFC_MD")
dir_DFC_MI <- file.path("./Dados_CVM/DemonstracoesFinanceiras/DFC_MI")
dir_DMPL <- file.path("./Dados_CVM/DemonstracoesFinanceiras/DMPL")
#dir_DRA <- file.path("./Dados_CVM/DemonstracoesFinanceiras/DRA")
dir_DRE <- file.path("./Dados_CVM/DemonstracoesFinanceiras/DRE")
dir_DVA <- file.path("./Dados_CVM/DemonstracoesFinanceiras/DVA")

# obter a lista de nomes de arquivos em cada diretC3rio
arquivos_BP <- list.files(dir_BP, pattern = "\\.csv$")
arquivos_DFC_MD <- list.files(dir_DFC_MD, pattern = "\\.csv$")
arquivos_DFC_MI <- list.files(dir_DFC_MI, pattern = "\\.csv$")
arquivos_DMPL <- list.files(dir_DMPL, pattern = "\\.csv$")
#arquivos_DRA <- list.files(dir_DRA, pattern = "\\.csv$")
arquivos_DRE <- list.files(dir_DRE, pattern = "\\.csv$")
arquivos_DVA <- list.files(dir_DVA, pattern = "\\.csv$")

# inicializar listas para armazenar os data frames
lista_BP <- list()
lista_DFC_MD <- list()
lista_DFC_MI <- list()
lista_DMPL <- list()
#lista_DRA <- list()
lista_DRE <- list()
lista_DVA <- list()

# loop atravC)s dos arquivos em cada diretC3rio e ler cada um com read.csv
for (arquivo in arquivos_BP) {
  caminho_arquivo <- file.path(dir_BP, arquivo)
  df <- read.csv(caminho_arquivo, sep = ";", fileEncoding = "ISO-8859-1", stringsAsFactors = FALSE)
  lista_BP[[arquivo]] <- df
}
for (arquivo in arquivos_DFC_MD) {
  caminho_arquivo <- file.path(dir_DFC_MD, arquivo)
  df <- read.csv(caminho_arquivo, sep = ";", fileEncoding = "ISO-8859-1", stringsAsFactors = FALSE)
  lista_DFC_MD[[arquivo]] <- df
}
for (arquivo in arquivos_DFC_MI) {
  caminho_arquivo <- file.path(dir_DFC_MI, arquivo)
  df <- read.csv(caminho_arquivo, sep = ";", fileEncoding = "ISO-8859-1", stringsAsFactors = FALSE)
  lista_DFC_MI[[arquivo]] <- df
}
for (arquivo in arquivos_DMPL) {
  caminho_arquivo <- file.path(dir_DMPL, arquivo)
  df <- read.csv(caminho_arquivo, sep = ";", fileEncoding = "ISO-8859-1", stringsAsFactors = FALSE)
  lista_DMPL[[arquivo]] <- df
}
#for (arquivo in arquivos_DRA) {
#  caminho_arquivo <- file.path(dir_DRA, arquivo)
#  df <- read.csv(caminho_arquivo, sep = ";", fileEncoding = "ISO-8859-1", stringsAsFactors = FALSE)
#  lista_DRA[[arquivo]] <- df
#}
for (arquivo in arquivos_DRE) {
  caminho_arquivo <- file.path(dir_DRE, arquivo)
  df <- read.csv(caminho_arquivo, sep = ";", fileEncoding = "ISO-8859-1", stringsAsFactors = FALSE)
  lista_DRE[[arquivo]] <- df
}
for (arquivo in arquivos_DVA) {
  caminho_arquivo <- file.path(dir_DVA, arquivo)
  df <- read.csv(caminho_arquivo, sep = ";", fileEncoding = "ISO-8859-1", stringsAsFactors = FALSE)
  lista_DVA[[arquivo]] <- df
}

# combinar todos os data frames em um C:nico data frame
BP <- do.call(rbind, lista_BP)
DFC_MD <- do.call(rbind, lista_DFC_MD)
DFC_MI <- do.call(rbind, lista_DFC_MI)
#DRA <- do.call(rbind, lista_DRA)
DRE <- do.call(rbind, lista_DRE)
DVA <- do.call(rbind, lista_DVA)

       
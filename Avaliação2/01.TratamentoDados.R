library(dplyr)


# Caminho para o arquivo
set <- "./statlog+german+credit+data.zip"

# Extraindo o conteúdo em um diretório temporário
temp_dir <- tempdir()
unzip(set, exdir = temp_dir)

# Verificando arquivos extraídos
lista_arquivos <- list.files(temp_dir, full.names = TRUE)
print(lista_arquivos)

# Carregando o arquivo "DATA"
dados <- file.path(temp_dir, "german.data")
dados <- read.table(dados, header = FALSE) 

# Carregando o arquivo "DATA-NUMERIC"
data_numeric <- file.path(temp_dir, "german.data-numeric") 
data_numeric <- read.table(data_numeric, header = FALSE)  



head(dados)

# criando base de dados bruta
dataraw <- dados

#salvando base de dados original nem csv
write.csv(dados, file = "geman-data.csv", row.names = FALSE, fileEncoding = "UTF-8")


# Tratamento das colunas

## 1.1 COLUNA V1

### 1.1.1 Descritivo

# Categoria = (qualitative) 
# Descrição = Status of existing checking account 
# Valores = A11 : ... \< 0 DM 
#           A12 : 0 \<= ... \< 200 DM 
#           A13 : ... \>= 200 DM / salary assignments for at least 1 year 
#           A14 : no checking account


# verificando os valores de V1
unique(dados$V1)
# Criando coluna com transformação dos dados de V1
dados <- dados %>% 
  mutate(`Status Conta Corrente` = case_when(
    V1 == "A11" ~ "Menos de 0 DM",
    V1 == "A12" ~ "de 0 a Menos de 200 DM",
    V1 == "A13" ~ "Mais de 200 DM",
    V1 == "A14" ~ "Sem Conta Corrente",
    TRUE ~ NA_character_ 
  ))

## 1.2 COLUNA V2

### 1.2.1 Descritivo

# Categoria = (numerical)
# Descrição = Duration in month

# Criar um histograma para visualizar a distribuição da coluna V2
hist(dados$V2, 
     main = "Duração em meses", 
     xlab = "Meses",
     col = "skyblue", 
     border = "black")

# Criar uma tabela com os valores únicos e suas contagens
tabela_contagem <- as.data.frame(table(dados$V2))

# Renomear as colunas da tabela
colnames(tabela_contagem) <- c("Duração em meses", "Contagem")

# Mostrar a tabela
print(tabela_contagem)

# Criando 3 novas colunas para avaliar melhor perfomance do tempo
dados <- dados %>% mutate(`Duração (em meses)` = V2)
dados <- dados %>% 
  mutate(`Duração (em anos)` = case_when(
    V2 <= 12 ~ "Até 1 ano",
    V2 <= 24 ~ "de 1 até 2 anos",
    V2 <= 36 ~ "de 2 até 3 anos",
    V2 <= 48 ~ "de 3 até 4 anos",
    V2 <= 60 ~ "de 4 até 5 anos",
    V2 <= 72 ~ "de 5 até 6 anos",
    TRUE ~ "Mais de 6 anos" 
  ))

dados <- dados %>% 
  mutate(`Duração` = case_when(
    V2 <= 6 ~ "Até 6 meses",
    V2 <= 12 ~ "de 6 até 12 meses",
    V2 <= 18 ~ "de 12 até 18 meses",
    V2 <= 24 ~ "de 18 até 24 meses",
    V2 <= 30 ~ "de 24 até 30 meses",
    V2 <= 36 ~ "de 30 até 36 meses",
    V2 <= 42 ~ "de 36 até 42 meses",
    V2 <= 48 ~ "de 42 até 48 meses",
    V2 <= 54 ~ "de 48 até 54 meses",
    V2 <= 60 ~ "de 54 até 60 meses",
    V2 <= 66 ~ "de 60 até 66 meses",
    V2 <= 72 ~ "de 66 até 72 meses",
    TRUE ~ "Mais de 72 meses" 
  ))


## 1.3 COLUNA V3

### 1.3.1 Descritivo

# Categoria = (qualitativo)
# Descrição = Histórico de crédito
# Valores = A30 : no credits taken/ all credits paid back duly
#           A31 : all credits at this bank paid back duly
#           A32 : existing credits paid back duly till now
#           A33 : delay in paying off in the past
#           A34 : critical account/ other credits existing (not at this bank)

unique(dados$V3)

# Criando coluna com transformação dos dados de V3
dados <- dados %>% 
  mutate(`Histórico de Crédito` = case_when(
    V3 == "A30" ~ "Sem créditos / Todos os créditos pagos pontualmente",
    V3 == "A31" ~ "Todos os créditos neste banco pagos pontualmente",
    V3 == "A32" ~ "Créditos existentes pagos pontualmente até agora",
    V3 == "A33" ~ "Atrasos no pagamento no passado",
    V3 == "A34" ~ "Conta crítica / Outros créditos existentes (não neste banco)",
    TRUE ~ NA_character_ 
  ))

## 1.4 COLUNA V3

### 1.4.1 Descritivo

# Categoria = (qualitativo)
# Descrição = Histórico de crédito


unique(dados$V4)

# Criando coluna com transformação dos dados de V3
dados <- dados %>% 
  mutate(`Propósito` = case_when(
    V4 == "A40" ~ "Carro Novo",
    V4 == "A41" ~ "Carro Usado",
    V4 == "A42" ~ "Móveis",
    V4 == "A43" ~ "Radio/TV",
    V4 == "A44" ~ "Elétrodomésticos",
    V4 == "A45" ~ "Reparos",
    V4 == "A46" ~ "Educação",
    V4 == "A47" ~ "Férias",
    V4 == "A48" ~ "Treinamentos",
    V4 == "A49" ~ "Negócios",
    V4 == "A410" ~ "Outros",
    TRUE ~ NA_character_ 
  ))


## 1.5 COLUNA V5

### 1.5.1 Descritivo

# Categoria = (numerical) 
# Descrição = Credit amount

unique(dados$V5)

summary(dados$V5)

hist(dados$V5, 
     main = "Distribuição do Valor do Crédito", 
     xlab = "Valor do Crédito", 
     col = "skyblue", 
     border = "black")

boxplot(dados$V5,
        main = "Boxplot do Valor do Crédito",
        ylab = "Valor do Crédito",
        col = "skyblue")

ecdf_plot <- ecdf(dados$V5)
plot(ecdf_plot, 
     main = "Distribuição Cumulativa do Valor do Crédito",
     xlab = "Valor do Crédito", 
     ylab = "Probabilidade Cumulativa")

dados <- dados %>% mutate(`Crédito` = V5)

## 1.6 COLUNA V6

### 1.6.1 Descritivo

# Categoria = (qualitative) 
# Descrição = Savings account/bonds
# Valores = A61 :          ... <  100 DM
#           A62 :   100 <= ... <  500 DM
#           A63 :   500 <= ... < 1000 DM
#           A64 :          .. >= 1000 DM
#           A65 :   unknown/ no savings account

# verificando os valores de V1
unique(dados$V6)

# Criando coluna com transformação dos dados de V1
dados <- dados %>% 
  mutate(`Poupança` = case_when(
    V6 == "A61" ~ "Menos de 100 DM",
    V6 == "A62" ~ "de 100 a Menos de 500 DM",
    V6 == "A63" ~ "de 500 a Menos de 1000 DM",
    V6 == "A64" ~ "Mais de 1000 DM",
    V6 == "A65" ~ "Desconhecido / sem poupança",
    TRUE ~ NA_character_ 
  ))

## 1.7 COLUNA V7

### 1.7.1 Descritivo

# Categoria = (qualitative) 
# Descrição = Present employment since
# Valores = A71 : unemployed
#           A72 :       ... < 1 year
#           A73 : 1  <= ... < 4 years  
#           A74 : 4  <= ... < 7 years
#           A75 :       .. >= 7 years

# verificando os valores de V1
unique(dados$V7)

# Criando coluna com transformação dos dados de V1
dados <- dados %>% 
  mutate(`Tempo no atual emprego` = case_when(
    V7 == "A71" ~ "Desempregado",
    V7 == "A72" ~ "Menor que 1 ano",
    V7 == "A73" ~ "de 1 até 4 anos",
    V7 == "A74" ~ "de 4 até 7 anos",
    V7 == "A75" ~ "Mais que 7 anos",
    TRUE ~ NA_character_ 
  ))

## 1.8 COLUNA V8

### 1.8.1 Descritivo

Categoria = (numerical)
Descrição = Installment rate in percentage of disposable income

summary(dados$V8)

hist(dados$V8,
     main = "Distribuição da Taxa de Parcelamento", 
     xlab = "Taxa de Parcelamento (%)",
     col = "skyblue",
     border = "black")

boxplot(dados$V8,
        main = "Boxplot da Taxa de Parcelamento", 
        ylab = "Taxa de Parcelamento (%)", 
        col = "skyblue")

ecdf_plot <- ecdf(dados$V8)
plot(ecdf_plot,
     main = "Distribuição Cumulativa da Taxa de Parcelamento",
     xlab = "Taxa de Parcelamento (%)", 
     ylab = "Probabilidade Cumulativa")

dados <- dados %>% mutate(`Taxa de Parcelamento (%)` = V8)

## 1.9 COLUNA V9

### 1.9.1 Descritivo

Categoria = (qualitative)
Descrição = Personal status and sex
Valores = A91 : male   : divorced/separated
          A92 : female : divorced/separated/married
          A93 : male   : single
          A94 : male   : married/widowed
          A95 : female : single

# Coluna "Gênero"
dados <- dados %>% 
  mutate(Gênero = ifelse(
    V9 %in% c("A91", "A93", "A94"), 
    "Masculino", "Feminino"))

# Coluna "Estado Civil"
dados <- dados %>% 
  mutate(`Estado Civil` = case_when(
  V9 %in% c("A91") ~ "Divorciado/Separado",
  V9 %in% c("A92") ~ "Divorciada/Separada/Casada",
  V9 %in% c("A93", "A95") ~ "Solteiro(a)",
  V9 %in% c("A94") ~ "Casado/Viúvo"
))

## 1.10 COLUNA V10

### 1.10.1 Descritivo

Categoria = (qualitative)
Descrição = Other debtors / guarantors
Valores = A101 : none
          A102 : co-applicant
          A103 : guarantor
          
dados <- dados %>% 
  mutate(`Partes Relacionadas` = case_when(
  V10 %in% c("A101") ~ "Nenhum",
  V10 %in% c("A102") ~ "Co-requerente",
  V10 %in% c("A103") ~ "Fiador"
))          

## 1.11 COLUNA V11

### 1.11.1 Descritivo

# Categoria = (numerical)
# Descrição = Present residence since

summary(dados$V11)

hist(dados$V11,
     main = "Distribuição do Tempo de Residência Atual", 
     xlab = "Tempo de Residência Atual",
     col = "skyblue",
     border = "black")

boxplot(dados$V11,
        main = "Boxplot do Tempo de Residência Atual", 
        ylab = "Tempo de Residência Atual", 
        col = "skyblue")

ecdf_plot <- ecdf(dados$V11)
plot(ecdf_plot,
     main = "Distribuição Cumulativa do Tempo de Residência Atual",
     xlab = "Tempo de Residência Atual)", 
     ylab = "Probabilidade Cumulativa")

dados <- dados %>% mutate(`Tempo de Residência Atual` = V11)

## 1.12 COLUNA V12

### 1.12.1 Descritivo

# Categoria = (qualitative)
# Descrição = Property
# Valores = A121 : real estate
#           A122 : if not A121 : building society savings agreement/life insurance
#           A123 : if not A121/A122 : car or other, not in attribute 6
#           A124 : unknown / no property

dados <- dados %>% 
  mutate(`Propriedades` = case_when(
    V12 %in% c("A121") ~ "Imóvel",
    V12 %in% c("A122") ~ "Conta de Poupança Imobiliária/Seguro de Vida",
    V12 %in% c("A123") ~ "Carro/Outros",
    V12 %in% c("A124") ~ "Desconhecido/Sem propriedades"
  )) 

## 1.13 COLUNA V13

### 1.13.1 Descritivo
# Categoria = (numerical)
# Descrição = Age in years

summary(dados$V13)

hist(dados$V13,
     main = "Distribuição de Idade", 
     xlab = "Idade",
     col = "skyblue",
     border = "black")

boxplot(dados$V13,
        main = "Boxplot de Idade", 
        ylab = "Idade", 
        col = "skyblue")

ecdf_plot <- ecdf(dados$V13)
plot(ecdf_plot,
     main = "Distribuição Cumulativa de Idade",
     xlab = "Tempo de Idade)", 
     ylab = "Probabilidade Cumulativa")

dados <- dados %>% mutate(`Idade` = V13)



Categoria = (qualitative)
Descrição = Other installment plans 
Valores = A141 : bank
          A142 : stores
          A143 : none

Categoria = (qualitative)
Descrição = Housing
Valores = A151 : rent
          A152 : own
          A153 : for free


Categoria = (numerical)
Descrição = Number of existing credits at this bank

Categoria = (qualitative)
Descrição = Job
Valores = A171 : unemployed/ unskilled  - non-resident
          A172 : unskilled - resident
          A173 : skilled employee / official
          A174 : management/ self-employed/  highly qualified employee/ officer

Categoria = (numerical)
Descrição = Number of people being liable to provide maintenance for

Categoria = (qualitative)
Descrição = Telephone
Valores = A191 : none
          A192 : yes, registered under the customers name

Categoria = (qualitative)
Descrição = foreign worker
Valores = A201 : yes
          A202 : no









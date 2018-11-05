# SMI 2018
# MC3: Web scraping com R e potenciais aplicações na coleta de preços
# Sessão 1.1: Coleta de dados na web através de download de arquivos


# Exemplo 1: download arquivo csv -----------------------------------------

# Portal Brasileiro de Dados Abertos:
# http://dados.gov.br/dataset

# Taxas dos Títulos Ofertados pelo Tesouro Direto:
# http://dados.gov.br/dataset/taxas-dos-titulos-ofertados-pelo-tesouro-direto

# Endereço do arquivo
arquivo <- 'http://www.tesourotransparente.gov.br/ckan/dataset/df56aa42-484a-4a59-8184-7676580c81e3/resource/796d2059-14e9-44e3-80c9-2d9e30b405c1/download/PrecoTaxaTesouroDireto.csv'
# Baixar o arquivo e carregar no R
dat <- read.csv2(arquivo, stringsAsFactors = F)
# Verificar estrutura do objeto criado
str(dat)
# Verificar conteudo do objeto criado
head(dat)

# Gerar gráfico
# install.packages('tidyverse')
library(tidyverse)
dat %>% 
  mutate(Data.Base = as.Date(Data.Base, format = '%d/%m/%Y')) %>% 
  filter(
    Tipo.Titulo == 'Tesouro IPCA+',
    Data.Base > as.Date('1/1/2018', format = '%d/%m/%Y')
  ) %>% 
  ggplot(aes(x = Data.Base, y = Taxa.Venda.Manha, color = factor(Data.Vencimento))) +
  geom_line()


# Exemplo 2: download arquivo Excel (xls/xlsx) ----------------------------

# install.packages('readxl')
library(readxl)

# Endereço do arquivo
arquivo <- 'http://www.anp.gov.br/images/Precos/Semanal2013/SEMANAL_BRASIL-DESDE_2013.xlsx'
# Criar arquivo temporário (é possível também armazenar de forma permanente)
tmp <- tempfile()
# Baixar o arquivo
download.file(arquivo, tmp)
# Carregar o arquivo no R
dat <- read_excel(tmp, skip = 16)
# Apagar aquivo temporário
unlink(tmp)

# Verificar conteúdo do objeto criado
dat
print(dat, n = 20, width = Inf)

# options(tibble.print_max = 10)
# options(tibble.width = Inf)


# Exemplo 3: download arquivo zip -----------------------------------------

# Endereço do arquivo
arquivo <- 'ftp://ftp.ibge.gov.br/Precos_Indices_de_Precos_ao_Consumidor/IPCA/Serie_Historica/ipca_SerieHist.zip'
# Criar arquivo temporário
tmp <- tempfile()
# Baixar o arquivo
download.file(arquivo, tmp)
# Listar arquivos dentro do zip
unzip(tmp, list = T)
# Armazenar nome do arquivo em um objeto
arquivo <- unzip(tmp, list = T)$Name
# Descompactar arquivo
unzip(tmp)

# install.packages('readxl')
library(readxl)
# Carregar arquivo no R
dat <- 
  read_excel(
    path = arquivo, 
    range = 'B300:C350', 
    col_names = c('mes', 'indice')
  ) %>% 
  na.omit() %>% 
  mutate(data = ISOdate(rep(2014:2017, each = 12), 1:12, 1))

# Deletar arquivo temporário
unlink(tmp)
# Deletar arquivo descompactado
file.remove(arquivo)

# Gerar gráfico
# install.packages('ggplot2')
library(ggplot2)
ggplot(dat, aes(x = data, y = indice, group = 1)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5))


# Exemplo 4: download arquivo json ----------------------------------------

# install.packages('jsonlite')
library(jsonlite)

# Endereço do arquivo
arquivo <- 'https://dadosabertosapi.ufca.edu.br/service/recurso/telefones.json'
# Baixar arquivo e carregar no R
dat <- read_json(arquivo)
# Verificar tipo do objeto criado
class(dat)
# Verificar estrutura do objeto criado
str(dat, max.level = 1)
# Baixar arquivo e carregar no R como data frame
dat <- read_json(arquivo, simplifyDataFrame = T)
# Verificar conteúdo do objeto criado
head(dat)


# Exemplo 5: download arquivo xml -----------------------------------------

# install.packages('xml2')
library(xml2)

# Endereço do arquivo
arquivo <- 'http://compras.dados.gov.br/licitacoes/v1/licitacoes.xml'
# Baixar e carregar arquivo no R
dat <- read_xml(arquivo) 
# Visualizar obejto criado
View(dat)

# Extrair conteúdo
# Identificar nodes
node <- '/resource/_embedded/resource/objeto'
node <- '//nome_responsavel'
node <- '//modalidade'
nodeset <- xml_find_all(dat, node)

# Extrair conteúdo dos nodesets
xml_text(nodeset)

# Converter para lista
dat_list <- as_list(dat)
map_chr(dat_list$resource$`_embedded`, ~ .x$modalidade[[1]])

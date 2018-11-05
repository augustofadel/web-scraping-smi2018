# SMI 2018
# MC3: Web scraping com R e potenciais aplicações na coleta de preços
# Sessão 1.2: Coleta de dados na web através de APIs


# Exemplo 1: The Star Wars API --------------------------------------------
# URL baseada em diretórios

# Download JSON
# install.packages('jsonlite')
library(jsonlite)
url <- 'https://swapi.co/api/people/1'
personagem <- read_json(url, simplifyVector = T)
url <- 'https://swapi.co/api/films/5'
filme <- read_json(url, simplifyVector = T)

# Interagindo com a API: GET request
# install.packages('httr')
library(httr)
url <- 'https://swapi.co/api/people/1'
# Enviar solicitação
resposta <- GET(url)
# Verificar status da resposta recebida
http_status(resposta)
# Verificar tipo da resposta recebida
http_type(resposta)
# Extrair conteúdo
personagem <- content(resposta)
# Verificar conteúdo
personagem$name

# Compor a URL da solicitação
url_base <- 'https://swapi.co/api'
tipo <- 'people'
valor <- 1
url <- paste(url_base, tipo, valor, sep = '/')
# Enviar solicitação (com identificação)
resposta <- GET(url, user_agent('This is a test | email: augusto.fadel@ibge.gov.br'))
# Extrair conteúdo
personagem <- content(resposta)
# Verificar conteúdo
personagem$name

# Interagindo através do API client
# install.packages('rwars')
library(rwars)

# Consultar personagens
personagem1 <- get_person(1)
personagem1$name
personagem2 <- get_person(4)
personagem2$name

# Consultar filmes
filme <- get_film(3)
filme$title

# Consultar 'starships'
get_starship(9)$name

# Lista com todos os peronagens de um filme 
filme$characters

# Consultar nomes de todos os personagens de um filme
library(tidyverse)
filme$characters %>% 
  map(read_json) %>% 
  map_chr('name')

# Entradas
?all_entries
filmes <- get_all_films()
titulos <- map_chr(filmes$results, 'title')
diretores <- map_chr(filmes$results, 'director')


# Exemplo 2: API do SIDRA -------------------------------------------------
# URL baseada em diretórios

# Interagindo com a API: GET request
library(httr)

# Compor URL da solicitação
url_base <- 'http://api.sidra.ibge.gov.br/values'
tabela <- 't/1737'
periodo <- 'p/201401-201712'
nivel_territorial <- 'n1/all'
variavel <- 'v/2266'
url <- paste(url_base, tabela, periodo, nivel_territorial, variavel, sep = '/')
# Enviar solicitação
resposta <- GET(url, user_agent('SMI 2018: mini-curso web scraping | email: augusto.fadel@ibge.gov.br'))
# Verificar status da solicitação
http_status(resposta)
# Verificar tipo da solicitação
http_type(resposta)

# Formatar dados
dat <- content(resposta) %>% bind_rows()
# Verificar objeto criado
dat

# Formatar dados
dat <- 
  content(resposta)[-1] %>% 
  bind_rows() %>% 
  set_names(content(resposta)[[1]]) %>% 
  mutate(
    Data = as.Date(paste0(`Mês (Código)`, '01'), '%Y%m%d'),
    Valor = as.numeric(Valor)
  )
# Exibir variáveis selecionadas
dat %>% select(Data, Valor)


# Exemplo 3: API do Banco Central -----------------------------------------
# URL baseada em parâmetros

library(httr)
library(tidyverse)

# Cotações diárias e taxas de câmbio
url_base <- "https://olinda.bcb.gov.br/olinda/servico/PTAX/versao/v1/odata/CotacaoMoedaDia(moeda=@moeda,dataCotacao=@dataCotacao)"
parametros <- 
  list(
    `@moeda` = "'EUR'",
    `@dataCotacao` = "'01-11-2018'",
    `$top` = 100,
    `$format` = 'json'
  )
resposta <- GET(url_base, query = parametros)
http_status(resposta)
content(resposta)[[2]] %>% bind_rows()

# Moedas disponíveis
moedas <- 
  "https://olinda.bcb.gov.br/olinda/servico/PTAX/versao/v1/odata/Moedas?$top=100&$format=json" %>% 
  GET() %>% 
  content() %>% 
  `[[`(2) %>% 
  bind_rows()

# Cotações de todas as moedas
data <- '01-11-2018'
url_base <- "https://olinda.bcb.gov.br/olinda/servico/PTAX/versao/v1/odata/CotacaoMoedaDia(moeda=@moeda,dataCotacao=@dataCotacao)"
cotacao_dia <- 
  map(
    moedas$simbolo,
    function(moeda) {
      parametros <- 
        list(
          `@moeda` = paste0('\'', moeda, '\''),
          `@dataCotacao` = paste0('\'', data, '\''),
          `$top` = 100,
          `$format` = 'json'
        )
      resposta <- 
        GET(
          url_base, 
          query = parametros, 
          user_agent('SMI 2018: mini-curso web scraping | email: augusto.fadel@ibge.gov.br')
        )
      http_status(resposta)
      content(resposta)[[2]] %>% bind_rows()
    }
  ) %>% set_names(moedas$simbolo)

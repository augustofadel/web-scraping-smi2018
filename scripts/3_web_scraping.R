# SMI 2018
# MC3: Web scraping com R e potenciais aplicações na coleta de preços
# Sessão 2.1: Coleta de dados na web através de técnicas de web scraping


# Consultar robots.txt ----------------------------------------------------
library(tidyverse)

# Endereço do arquivo
url <- 'https://pt.wikipedia.org/robots.txt'
# Baixar arquivo e carregar no R
robot <- read_tsv(url, col_types = 'c')
# Visualizar objeto criado
View(robot)


# Estrutura HTML ----------------------------------------------------------
# install.packages('rvest')
library(rvest)

# URL
url <- 'https://pt.wikipedia.org/wiki/Instituto_Brasileiro_de_Geografia_e_Estat%C3%ADstica'
# Carregar URL
dat <- read_html(url)

# Extrair nome da tag
dat %>% html_node(xpath = '/html/head/title') %>% html_name()
# Extrair conteúdo da tag
dat %>% html_node(xpath = '/html/head/title') %>% html_text()

# Extrair nome da tag
dat %>% html_nodes(xpath = '//a') %>% html_name()
# Extrair atributo da tag
dat %>% html_nodes(xpath = '//a') %>% html_attr('href')
# Extrair conteúdo da tag
dat %>% html_nodes(xpath = '//a') %>% html_text()

# Extrair tabela
read_html('https://html.com/tags/') %>% 
  html_node(xpath = '//*[@id="site-content"]/div/table') %>% 
  html_table()

# Selector Gadget
vignette('selectorgadget')


# Exemplo 1: Amazon -------------------------------------------------------
# https://www.amazon.com.br/robots.txt

# install.packages(c('rvest', 'lubridate'))
library(rvest)
library(tidyverse)
library(lubridate)

# Coletar um produto por vez
url_base <- 'https://www.amazon.com.br/gp/product'
produto <- '1491910399'
url <- paste(url_base, produto, sep = '/')
resposta <- read_html(url)
titulo <- 
  resposta %>% 
  html_node(xpath = '//*[@id="productTitle"]') %>% 
  html_text()
autor <- 
  resposta %>% 
  html_node(xpath = '//*[@id="byline"]/span[1]/a') %>% 
  html_text()
valor <- 
  resposta %>% 
  html_node(xpath = '//*[@id="soldByThirdParty"]/span') %>% 
  html_text() %>% str_replace(',', '.') %>% 
  str_extract('([0-9]+\\.[0-9]+)') %>% 
  as.numeric()
disponibilidade <- 
  resposta %>% 
  html_nodes(xpath = '//*[@id="availability"]/span') %>% 
  html_text() %>% 
  str_detect('Em estoque')

# Coletar relação de produtos
url_base <- 'https://www.amazon.com.br/gp/product'
produtos <- 
  c(
    # '8525432180', 
    # '8525420816', 
    # '8537818003', 
    # '8544105378', 
    # '8516085511', 
    # '8593828620', 
    # '8501106585', 
    # '8575596543', 
    # '8520920713', 
    # '857559057X',
    '1491910399',
    '858057952X',
    '8537801550',
    '853900383X',
    '8520936873',
    '0321751043'
  )
nodes <- 
  c(
    '//*[@id="productTitle"]', 
    '//*[@id="byline"]/span[1]/a', 
    '//*[@id="soldByThirdParty"]/span', 
    '//*[@id="availability"]/span'
  )
variaveis <- 
  c(
    'titulo', 
    'autor', 
    'valor', 
    'disponibilidade'
  )

dat <- 
  map(
    produtos,
    function(produto) {
      resposta <- 
        paste(url_base, produto, sep = '/') %>% 
        read_html()
      Sys.sleep(1)
      map_chr(
        nodes, 
        ~ resposta %>% html_node(xpath = .x) %>% html_text()
      ) %>% set_names(variaveis)
    }
  ) %>%
  set_names(produtos)

# Formatar os dados coletados
dat_tb <-
  do.call(rbind, dat) %>% 
  as_tibble() %>% 
  set_names(variaveis) %>% 
  add_column(
    produto = produtos,
    data_coleta = today()
  ) %>% 
  mutate(
    valor = valor %>% 
      str_replace(',', '.') %>% 
      str_extract('([0-9]+\\.[0-9]+)') %>% 
      as.numeric(),
    disponibilidade = disponibilidade %>% 
      map_chr(
        ~ when(.x,
          str_detect(., 'a partir de') ~ str_extract(., '([0-9]{1,2}/[A-Za-z]*/[0-9]{4})'),
          str_detect(tolower(.), 'em estoque') ~ 'disponivel',
          ~ 'indisponivel'
        )
      )
  )
  
# Salvar e exportar
# Criar diretório
dir.create('dados')
# Salvar objeto do R
saveRDS(dat_tb, paste0('dados/amazon_', today(), '.rds'))
# Exportar arquivo csv
write_csv(dat_tb, paste0('dados/amazon_', today(), '.csv'))
# Exportar arquivo json
write_json(dat_tb, paste0('dados/amazon_', today(), '.json'))


# Exemplo 2: Passagens aéreas ---------------------------------------------

# Selenium Server Standalone Binary:
# http://selenium-release.storage.googleapis.com/index.html
# http://selenium-release.storage.googleapis.com/3.9/selenium-server-standalone-3.9.1.jar

# Docker:
# http://ropensci.github.io/RSelenium/articles/docker.html
# vignette("docker", package = "RSelenium")

# RSeleium::rsDriver
# library(RSelenium)
# server <- rsDriver(browser = 'chrome')

# Manualmente
# No terminal: java -jar selenium-server-standalone-3.9.1.jar
# Ou system('java -jar selenium-server-standalone-3.9.1.jar')

# install.packages('RSelenium')
library(RSelenium)

# Consutar robots.txt: https://www.voegol.com.br/robots.txt

# Especificar a sessão
server <- 
  remoteDriver(
    remoteServerAddr = 'localhost',
    port = 4444L,
    browserName = 'chrome'
  )

# Conectar ao servidor e abrir navegador
server$open(silent = T)

# Verificar status
server$getStatus()

# Acessar o site e navegar
url <- 'https://voegol.com.br/pt'
server$navigate(url)
server$getCurrentUrl()
# server$goBack()
# server$goForward()
# server$refresh()

# Localizar elementos usando XPath (ou CSS selector)
botao_comprar_passagem <- 
  server$findElement(
    using = 'xpath', 
    '//*[@id="purchase-box"]/form[2]/div[1]/div[1]/a'
  )

# Clicar em um elemento
botao_comprar_passagem$clickElement()

# Preencher o campo origem:
# Localizar campo de origem
campo_origem <- 
  server$findElement(
    'xpath', 
    '//*[@id="purchase-box"]/form[2]/div[1]/div[2]'
  )
# Clicar no campo
campo_origem$clickElement()
# Enviar comandos de teclado
campo_origem$sendKeysToActiveElement(
  list('Rio de Janeiro', key = 'enter')
)
# Enviar comandos de teclado usando represenação UTF-8
# Representação das teclas em UTF-8:
# https://github.com/SeleniumHQ/selenium/wiki/JsonWireProtocol#sessionsessionidelementidvalue
campo_origem$clickElement()
campo_origem$sendKeysToActiveElement(
  list('Rio de Janeiro', '\uE007')
)

# Enviar comandos de teclado para buscar opções
campo_origem$clickElement()
campo_origem$sendKeysToActiveElement(
  list('Rio de Janeiro', '\uE015', '\uE015', '\uE007')
)

# Preencher campo de destino
campo_destino <- 
  server$findElement(
    'xpath', 
    '//*[@id="purchase-box"]/form[2]/div[1]/div[3]/div[1]/div[1]'
  )
campo_destino$clickElement()
campo_destino$sendKeysToActiveElement(
  list('Fortaleza', '\uE007')
)

# Clicar na data de ida
server$findElement(
  'xpath', 
  '//*[@id="purchase-box"]/form[2]/div[2]/div[8]/div[2]/div[1]'
)$clickElement()
# Selecionar a data de ida
server$findElement(
  'xpath', 
  '//*[@id="ui-datepicker-div"]/div[1]/table/tbody/tr[3]/td[2]'
)$clickElement()
# Clicar na data de volta
server$findElement(
  'xpath', 
  '//*[@id="purchase-box"]/form[2]/div[2]/div[8]/div[2]/div[2]'
)$clickElement()
# Selecionar a data de volta
server$findElement(
  'xpath', 
  '//*[@id="ui-datepicker-div"]/div[1]/table/tbody/tr[4]/td[2]'
)$clickElement()

# Clicar no botão compre aqui
botao_compre_aqui <- 
  server$findElement(
    'xpath', 
    '//*[@id="btn-box-buy"]'
  )
botao_compre_aqui$clickElement()

# Aguardar carregar tabelas de tarifas
tabelas_tarifas <- 
  server$findElements(
    'xpath', 
    '//*[@id="ida"]/div[1]/div/table | //*[@id="volta"]/div[1]/div/table'
  )
tabelas_carregadas <- 
  map_lgl(tabelas_tarifas, ~ .x$isElementDisplayed() %>% unlist())
while(!all(tabelas_carregadas)) {
  Sys.sleep(1)
  tabelas_carregadas <- 
    map_lgl(tabelas_tarifas, ~ .x$isElementDisplayed() %>% unlist())
}

# Coletar valores das tarifas
# Primieira tarifa
tarifa_elemento <- 
  server$findElement(
    'xpath', 
    '//*[@id="ida"]/div[8]/div[5]/table[1]/tbody/tr/td[2]/div/label/span[1]'
  )
tarifa_elemento$getElementText()

# Todas as tarifas, usando XPath
tarifa_elementos <- 
  server$findElements(
    'xpath', 
    '//span[@class="fareValue"]'
  )
tarifas <- map_chr(tarifa_elementos, ~ .x$getElementText() %>% unlist())

# Todas as tarifas, usando classe
tarifa_elementos <- 
  server$findElements(
    'class name', 
    'fareValue'
  )
tarifas <- map_chr(tarifa_elementos, ~ .x$getElementText() %>% unlist())

# Tarifas Comfort (Max)
tarifa_elementos <- 
  server$findElements(
    'xpath', 
    '//td[@class="taxa taxaComfort"]'
  )
tarifas <- map_chr(tarifa_elementos, ~ .x$getElementText() %>% unlist())
tarifas %>% 
  str_remove('\\.') %>% 
  str_replace(',', '.') %>% 
  str_extract('([0-9]*\\.[0-9]{2})') %>% 
  as.numeric()

# Tarifas Promocionais (Light)
tarifa_elementos <- 
  server$findElements(
    'xpath', 
    '//td[@class="taxa taxaPromocional"]'
  )
tarifas <- 
  map_chr(tarifa_elementos, ~ .x$getElementText() %>% unlist()) %>% 
  str_remove('\\.') %>% 
  str_replace(',', '.') %>% 
  str_extract('([0-9]*\\.[0-9]{2})') %>% 
  as.numeric()

# Tarifas de ida promocionais (Light)
tarifa_elementos <- 
  server$findElements(
    'xpath', 
    '//*[@id="ida"]/div/div/table/tbody/tr/td[@class="taxa taxaPromocional"]'
  )
tarifas <- map_chr(tarifa_elementos, ~ .x$getElementText() %>% unlist()) %>% 
  str_remove('\\.') %>% 
  str_replace(',', '.') %>% 
  str_extract('([0-9]*\\.[0-9]{2})') %>% 
  as.numeric()

# Fechar navegador
server$close()

arq_atu <- function(
     origem,
     destino,
     hash = F,
     date_url = 'http://dados.gov.br/dataset/dominios-gov-br/resource/197a0106-c93b-42fc-bb4e-c3095baee1a0',
     date_xpath = '//*[@id="content"]/div[3]/div/section/div/table/tbody/tr[1]/td'
) {
     require(dplyr)
     require(readr)
     require(digest)
     require(tools)
     require(stringr)
     require(rvest)
     
     arquivo_tmp <- tempfile()
     download.file(origem, arquivo_tmp)
     hash_arquivo_tmp <- digest(arquivo_tmp, 'sha1', file = T)
     arquivos_destino <- list.files(destino, full.names = T)
     
     if (hash) {
          filename <- paste(hash_arquivo_tmp, file_ext(origem), sep = '.')
     } else {
          if (is.null(date_url) & is.null(date_xpath)) {
               tstamp <- 
                    Sys.Date() %>% 
                    as.character
          } else {
               tstamp <- 
                    read_html(date_url) %>% 
                    html_node(xpath = date_xpath) %>% 
                    html_text() %>% 
                    str_split('/') %>% 
                    unlist() %>% 
                    rev() %>% 
                    paste(collapse = '_')
          }
          filename <- paste(tstamp, basename(origem), sep = '_')
     }
     
     if (length(arquivos_destino) != 0) {
          arquivo_mais_recente <- 
               arquivos_destino %>% 
               '['(arquivos_destino %>% file.mtime %>% which.max)
          hash_mais_recente <- 
               digest(arquivo_mais_recente, 'sha1', file = T)
          if (hash_arquivo_tmp == hash_mais_recente) {
               cat('O arquivo de origem e igual ao arquivo mais recente encontrado no diretorio de destino.\n')
               return(arquivo_mais_recente)
          }
     }
     
     salvo <- 
          file.copy(
               arquivo_tmp, 
               file.path(destino, filename),
               overwrite = T
          )
     if (salvo) {
          cat('Um novo arquivo foi salvo no diretorio de destino.\n')
          return(file.path(destino, filename))
     } else {
          stop('Nao foi possivel salvar o novo arquivo.\n')
     }
}
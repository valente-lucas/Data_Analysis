###Baixo e instalo as bibliotecas a serem usadas
install.packages("wdman")
install.packages("remotes")
install.packages("RSelenium")
library(remotes)
install_github("meirelesff/genderBR")
install_github("remibacha/kwClustersR")
library(tidyr)
library(stringr)
library(stringi)
library(genderBR)
library(dplyr)
library(ggplot2)
library(rvest)
install.packages("xml2")
library(xml2)
library(RSelenium)
library(httr)
library(kwClustersR)
library(readxl)
library(stringdist)
library(writexl)
library(forcats)


###Abro o arquivo original
arquivo<-read.csv("/content/drive/MyDrive/Arquivo_leila/Tratamento_Leila/search_result-815643304_ORIGINAL(1).csv", encoding="UTF-8")

###Separa a coluna Autor em três partes para fazer a adequação dos dados de nomes dos autores
arquivo<-separate(arquivo,Autor.a., into=c("A","B","C"), sep=",")

### Linhas com "|" e "@", separar a partir do "|"
linhas_com_barra<-subset.data.frame(arquivo, grepl("\\|",strsplit(arquivo$B,"")),)
funcao_1<- function(x){return(strsplit(x,"|",fixed=TRUE)[[1]][1])}
arquivo$B[grepl("\\|",arquivo$B)]<-sapply(linhas_com_barra$B,funcao_1)

### Linhas com coluna B em Na 
linhas_Na<-subset.data.frame(arquivo, is.na(arquivo$B),)
row.names.data.frame(linhas_Na)
## Linhas a serem corrigidas (coluna B em Na mas não deveria)
# linha 769: ADÔNIS TAVARES DA SILVA
arquivo[769,2]='Adônis Tavares da'

### Linhas com '[]', separar a partir do primeiro '[' 
linhas_com_colchetes<-subset.data.frame(arquivo, grepl(paste0(c("\\["),collapse = "|"),strsplit(arquivo$B,"")),)
row.names.data.frame(linhas_com_colchetes)
funcao_2<- function(x){return(strsplit(x,"\\[")[[1]][1])}
arquivo$B[grepl("\\[",arquivo$B)]<-sapply(linhas_com_colchetes$B,funcao_2)

###Linhas com '()'
linhas_com_parenteses<-arquivo$B[grepl("\\(",arquivo$B)]
#Linha 19: " Alysson Gomes de (Engenheiro elétrico)"= "Alysson Gomes de"
arquivo[19,2]='Alysson Gomes de'

###Linhas com '.', substituir os nomes abreviados e por fim substituir os pontos por espaço

linhas_com_ponto<-subset.data.frame(arquivo, grepl(paste0(c("\\."),collapse = "|"),strsplit(arquivo$B,"")),)
###Nomes abreviados
#(443) L. G. NACHTIGALL == Lucas Garcia  NACHTIGALL
arquivo[443,2] = ' Lucas Garcia'
#(495) S. M. F. S. MASSRUHA == Silvia Maria Fonseca S Massruha
arquivo[495,2] ='Silvia Maria Fonseca S'
#(496) S. a. b. da CRUZ == Sergio Aparecido Braga da Cruz
arquivo[496,2]='Sergio Aparecido Braga da'
#(816) J. E. M. Freire == JOSÉ EDUARDO DE MELLO FREIRE
arquivo[816,2]='JOSÉ EDUARDO DE MELLO'
#(977) G. B. MIRANDA == GABRIEL BASTOS DE MIRANDA
arquivo[977,2]='GABRIEL BASTOS DE'
#(1073) S. A. HIRAI == SANDRO AUGUSTO HIRAI
arquivo[1073,2]='SANDRO AUGUSTO'
#(1415) L. A. LOPES == LUKERCIO DE ABREU LOPES
arquivo[1415,2]='LUKERCIO DE ABREU'
#(1433) A. C. SÉCOLO == ADELINE CECILIA SÉCOLO
arquivo[1433,2]='ADELINE CECILIA'
#(1476) R. A. FRAGOSO == RODRIGO ANDRADE FRAGOSO
arquivo[1476,2]='RODRIGO ANDRADE'
#(1614) L. A. CELIBERTO JR. == Luiz Antonio Celiberto Junior
arquivo[1614,2]='Luiz Antonio'
#(1615) D. H. PERICO ==  DANILO HERNANI PERICO
arquivo[1615,2]='DANILO HERNANI'
#(1719) N. I VERSLYPE ==  NINA IRIS VERSLYPE
arquivo[1719,2]='NINA IRIS'
#(1746) F. BINUESA == FABIO BINUESA
arquivo[1746,2]='Fabio'
#(1760) Y. M. OLIVATTI == YURI MARINHO OLIVATTI
arquivo[1760,2]='YURI MARINHO'
#(1938) I. J. SILVA == ISAAC JESUS DA SILVA
arquivo[1938,2]='ISAAC JESUS DA'
#(1939) D. R. MENEGHETTI == DOUGLAS DE RIZZO MENEGHETTI
arquivo[1939,2]='DOUGLAS DE RIZZO'
#(1940) R. CLASER == RAFFAELLO CLASER
arquivo[1940,2]='RAFFAELLO'
#(2085) M. C. P. Y. PESSOA ==  Maria Conceição Peres Young Pessoa
arquivo[2085,2]='Maria Conceição Peres Young'
#(2268) A. G. TERAMACHI == AMANDA GOMES TERAMACHI
arquivo[2268,2]='AMANDA GOMES'
#(2333) M. S. PEREIRA == MOISÉS SANTANA PEREIRA
arquivo[2333,2]='MOISÉS SANTANA'
#(3644) M. F. BOUZON == MURILLO FREITAS BOUZON
arquivo[3644,2]='MURILLO FREITAS'
#(3726) D.G. NASCIMENTO == DAVI GUARACHO NASCIMENTO
arquivo[3726,2]='DAVI GUARACHO'
#(3893) C. L MOREIRA == CRISTIANO LOPES MOREIRA
arquivo[3893,2]='CRISTIANO LOPES'
#(4179) F. H. G. CESAR == FÁBIO HENRIQUE GONÇALVES CESAR
arquivo[4179,2]='FÁBIO HENRIQUE GONÇALVES'
#(4460) A. A. MASIERO == ANDREY ARAUJO MASIERO
arquivo[4460,2]='ANDREY ARAUJO'
#(4462) F. M. E. RODRIGUES == FELIPE MARTINO ESPOSITO RODRIGUES
arquivo[4462,2]='FELIPE MARTINO ESPOSITO'
#(4676) P.A.S.O. SILVA == PEDRO AUGUSTO SANTOS ORONA SILVA
arquivo[4676,2]='PEDRO AUGUSTO SANTOS ORONA'
#(4797) R. A. L. MORETO == RODRIGO ALVES DE LIMA MORETO
arquivo[4797,2]='RODRIGO ALVES DE LIMA'
#(4832) B. T. FARIA == BRENNO TONDATO DE FARIA
arquivo[4832,2]='BRENNO TONDATO DE'
#(4898) D. A. FABBRO == DAVI ARAUJO DAL FABBRO
arquivo[4898,2]='DAVI ARAUJO DAL'
#(4899) J. A. OLIVEIRA == JULIANO ALVES DE OLIVEIRA
arquivo[4899,2]='JULIANO ALVES DE'
#(5616) F. A. CAPATI == FELIPE ALBERTO CAPATI
arquivo[5616,2]='FELIPE ALBERTO'
#(5617) A. C. SÉCOLO == ADELINE CECILIA SÉCOLO
arquivo[5617,2]='ADELINE CECILIA'
#(5618) S. R. de ARAUJO JUNIOR == SILVIO ROMERO DE ARAÚJO JÚNIOR
arquivo[5618,2]='SILVIO ROMERO DE'
#(5898) T. T. REGO == THIAGO TURCATO DO REGO
arquivo[5898,2]='THIAGO TURCATO DO'
#(5899) D. R. MENEGHETTI == DOUGLAS DE RIZZO MENEGHETTI
arquivo[5899,2]='DOUGLAS DE RIZZO'
#(5900) L. C. NEVES == LEONARDO CONTADOR NEVES
arquivo[5900,2]='LEONARDO CONTADOR'
#(5901) V. BIAZON == VICTOR BIAZON
arquivo[5901,2]='VICTOR'
#(5902) R. DE C. TECHI == RODRIGO DE CARVALHO TECHI
arquivo[5902,2]='RODRIGO DE CARVALHO'
#(5903) E. DE B. GRASSL ==  ERIC DE BAÉRE GRASSL
arquivo[5903,2]='ERIC DE BAÉRE'
#(5904) D. E. SILVA == DIEGO EDUARDO SILVA
arquivo[5904,2]='DIEGO EDUARDO'
#(6152) L. M. V. R. BITTAR == LAURA MARTINS VALENTE RAMOS BITTAR
arquivo[6152,2]='LAURA MARTINS VALENTE RAMOS'
arquivo$B[grepl("\\.",arquivo$B)]<-gsub("\\.", " ", linhas_com_ponto$B)

###Linhas com '?'
linhas_com_interrogacao<-subset.data.frame(arquivo, grepl(paste0(c("\\?"),collapse = "|"),strsplit(arquivo$B,"")),)
## Correção nomes coluna B
#(1609)"Patr?cia Morais da Matta"="Patrícia Morais da Matta"
arquivo[1609,2]='Patrícia Morais da Matta'
#(3049) "L?cia Helena de Matos"="Lúcia Helena de Matos"
arquivo[3049,2]='Lúcia Helena de Matos'
#(3056) "F?bio Rodrigues"="Fábio Rodrigues"
arquivo[3056,2]='Fábio Rodrigues'
#(3223) "Gr?gori Stefanello"="Grégori Stefanello"
arquivo[3223,2]="Grégori Stefanello"
#(3439) "Estev?o Smania"="Estevão Smania"
arquivo[3439,2]="Estevão Smania"
#(3590) "Vin?cius"="Vinícius"
arquivo[3590,2]="Vinícius"
#(3604) "André Vinícius Gonçalves" = "Andr? Vin?cius Gon?alves" (primeira coluna com erro tbm)
arquivo[3604,1]="Gonçalves"
arquivo[3604,2]="André Vinícius"
#(4747) "L?yla Advincula Candido de"="Láyla Advincula Candido de"
arquivo[4747,2]="Láyla Advincula Candido de"
#(5016) "Paulo Ant?nio Caliendo Velloso da"="Paulo Antônio Caliendo Velloso da"
arquivo[5016,2]="Paulo Antônio Caliendo Velloso da"
#(5487) "Pedro M?rcio Raposo"="Pedro Márcio Raposo"
arquivo[5487,2]="Pedro Márcio Raposo"
#(5789) "Luis Ot?vio de Colla"="Luis Otávio de Colla"
arquivo[5789,2]="Luis Otávio de Colla"
#(6232) "R?pila Rami da Silva G?da"="Rúpila Rami da Silva Gôda" (primeira coluna com erro tbm)
arquivo[6232,1]="Gôda"
arquivo[6232,2]="Rúpila Rami da Silva"
#(6340) "Everton Lu?s"="Everton Luís"
arquivo[6340,2]="Everton Luís"
#(6341) "F?bio Brandolt"="Fábio Brandolt"
arquivo[6341,2]="Fábio Brandolt"
#(6342) "Ant?nio Rodrigo Delepiane De"="Antônio Rodrigo Delepiane De"
arquivo[6342,2]="Antônio Rodrigo Delepiane De"
#(6351) "Ant?nio Barbosa J?nior"="Antônio Barbosa Júnior" (primeira coluna com erro tbm)
arquivo[6351,1]="Júnior"
arquivo[6351,2]="Antônio Barbosa"
#(6352) "?verton de Oliveira"="Éverton de Oliveira"
arquivo[6352,2]="Éverton de Oliveira"
#(6353) "J?lio C?sar da Costa"="Júlio César da Costa"
arquivo[6353,2]="Júlio César da Costa"
#(6361) "Jos? Airton Chaves"="José Airton Chaves"
arquivo[6361,2]="José Airton Chaves"
#(6405) "Maur?cio Boff de ?vila"="Maurício Boff de Ávila" (primeira coluna com erro tbm)
arquivo[6405,1]="Ávila"
arquivo[6405,2]="Maurício Boff de"
#(6406) "Maur?cio Boff de ?vila"="Maurício Boff de Ávila" (primeira coluna com erro tbm)
arquivo[6406,1]="Ávila"
arquivo[6406,2]="Maurício Boff de"
#(6449) "Elu? Ramos"="Eluã Ramos"
arquivo[6449,2]="Eluã Ramos"
#(6508) "Jos? Leonardo dos Santos"="José Leonardo dos Santos"
arquivo[6508,2]="José Leonardo dos Santos"
#(6545) "Elu? Ramos"="Eluã Ramos"
arquivo[6545,2]="Eluã Ramos"
#(6552) "Henriqueta Talita Guimar?es"="Henriqueta Talita Guimarães"
arquivo[6552,2]="Henriqueta Talita Guimarães"
#(6567) "Tha?sa de Oliveira"="Thaísa de Oliveira"
arquivo[6567,2]="Thaísa de Oliveira"
##Correção nomes coluna A

#(5915) "Bulh?es"="Bulhões"
arquivo[5915,1]="Bulhões"
#(6337) "Assun??o"="Assunção"
arquivo[6337,1]="Assunção"
#(6403) "D?res"="Dôres"
arquivo[6403,1]="Dôres"
#(6407) "Corr?a"="Corrêa"
arquivo[6407,1]="Corrêa"

###Juntando as informações das colunas de nome e fazendo uma coluna só
arquivo$Autor <- ifelse(is.na(arquivo$B), arquivo$A, paste(arquivo$B, arquivo$A))
arquivo <- arquivo[, c("Autor", setdiff(names(arquivo), c("A", "B", "C")))]
arquivo[5696,1]="Leandro Pereira"
arquivo$Autor<-str_to_title(arquivo$Autor)
arquivo$Autor <- trimws(arquivo$Autor, "both")
arquivo$Autor <- gsub("\\s+", " ", arquivo$Autor)

###Tiro os TRABALHOS DUPLICADOS do dataframe
##Obtenho a indexação das linhas duplicadas e retiro elas do dataframe 
#linhas_duplicadas <- arquivo[duplicated(arquivo[c("Autor", "Ano.de.defesa")]) | duplicated(arquivo[c("Autor", "Ano.de.defesa")], fromLast =TRUE),]
arquivo<-arquivo[-c(20,58,119,125,162,171,258,264,277,286,295,296,316,317,328,330,362,367,387,388,419,423,443,466,479,535,536,548,580,590,620,646,649,681,715,723,730,750,768,770,787,852,853,868,947,976,1047,1078,1081,1107,1120,1128,1151,1176,1222,1223,1224,1225,1226,1227,1307,1354,1355,1356,1380,1381,1384,1385,1386,1393,1395,1413,1708,1837,1914,2040,2078,2082,2085,2118,2214,2215,2303,2394,2561,2576,2594,2875,3227,3230,3244,3246,3247,3248,3296,3306,3370,3432,3434,3505,3540,3559,3581,3583,3606,3607,3746,3927,4098,4303,4327,4544,4561,4577,4596,4768,4775,4778,4782,4783,4786,4914,4915,4957,4997,5011,5040,5059,5142,5143,5242,5274,5380,5432,5433,5434,5435,5441,5442,5444,5445,5462,5463,5467,5472,5546,5646,5697,5698,5699,5700,5701,5702,5703,5704,5705,5706,5707,5711,5729,5783,5813,5823,5920,6016,6017,6018,6019,6020,6021,6022,6023,6024,6025,6026,6027,6028,6029,6030,6031,6032,6033,6034,6035,6036,6037,6038,6076,6077,6182,6223,6231,6288,6295,6315,6316,6384,6387,6410,6446,6448,6504,6518,6528,6529,6536,6551,6555,6556,6562),]
linhas_duplicadas <- arquivo[duplicated(arquivo[c("Autor", "Ano.de.defesa")]) | duplicated(arquivo[c("Autor", "Ano.de.defesa")], fromLast =TRUE),]

###Aplico a biblioteca GenderBR para obter os gêneros dos nomes
arquivo$genero<-get_gender(stri_trans_general(arquivo$Autor,"Latin-ASCII"))
###Vejo as linhas que não foram caracterizadas pela biblioteca genderBR
linhas_genero_Na<-subset.data.frame(arquivo, is.na(arquivo$genero),)
###Corrijo essas linhas
vetor_correção<-c("Male","Male","Male","Male","Male","Male","Male","Male","Male","Male","Female","Male","Male","Male","Male","Male","Male","Female","Female","Male","Male","Male","Female","Female","Male","Female","Male","Male","Female","Male","Male","Male","Female","Female","Male","Male","Male","Female","Female","Male","Male","Male","Male","Male","Female","Male","Male","Male","Male","Male","Male","Female","Male","Male","Male","Female","Male","Male","Male","Male","Male","Male","Male","Female","Male","Male","Male","Male","Male","Male","Male","Male","Female","Male","Male","Female","Female","Male","Male","Male","Male","Male","Male","Male","Male","Male","Male","Male","Female","Male","Male","Male","Male","Female","Male","Male","Male","Female","Male","Male","Male","Male","Male","Female","Male","Male","Male","Male","Female","Male","Male","Male","Female","Female","Male","Male","Male","Male","Male","Male","Male","Male","Male","Female","Male","Female","Male","Male","Male","Male","Male","Male","Male","Male","Male","Male","Female","Female","Female","Female","Male","Male","Male","Male","Female","Male","Female","Male","Male","Male","Male","Female","Male","Male","Female","Male","Male","Female","Male","Male","Female","Male","Male","Male","Female","Male","Male","Male","Male","Male","Male","Female","Female","Male","Male","Male","Male","Male","Female","Male","Male","Female","Male","Male","Male","Male","Male","Female","Male","Female","Male","Male","Female","Male","Male","Male","Female","Female","Male","Male","Female","Female","Male","Female","Female","Male","Female","Male","Female","Female","Female","Male","Female","Male","Male","Male","Female","Female","Male","Female","Male","Female","Male","Female","Female","Male","Female","Male","Female","Female","Male","Male","Male","Male","Female","Male","Male","Male","Male","Female","Male","Male","Male","Female","Female")
arquivo$genero[is.na(arquivo$genero)]<-vetor_correção

###Analise do número de autores unicos e por genero, aqui são desconsiderados autores duplicados 
#arquivo_2<-arquivo[!duplicated(arquivo$Autor),]
#dados_agrupados<-arquivo_2%>%group_by(genero)
#count(dados_agrupados)

###Analiso quais linhas não possuem assuntos preenchidos

arquivo_teste<-arquivo[arquivo$Assuntos.em.português=="Não informado pela instituição",]

###Analiso a quais instituições essas linhas pertencem e a adequação de cada linha vai depender da instituição
unique(arquivo_teste$Sigla.da.instituição.de.defesa) ###PUC_SP" "UFV" "UNB" "FIOCRUZ" "USP" "PUC_RIO" "UEL" "UFS" "INPE" "IEN" "UNIFEI" "UFCG" "UFMG" "FGV" "UFVJM" "UNISINOS" "UFSCAR" "UFRN" "INATEL"

### Correção palavras-chave da instituição PUC_SP

sequencia_links_PUC_SP<-arquivo_teste$Link.de.acesso[arquivo_teste$Sigla.da.instituição.de.defesa=="PUC_SP"]
#sequencia_links_PUC_SP
#arquivo[arquivo$Link.de.acesso=="https://tede2.pucsp.br/handle/handle/23659",]
arquivo[arquivo$Assuntos.em.português=="Não informado pela instituição" & arquivo$Sigla.da.instituição.de.defesa=="PUC_SP", "Assuntos.em.português"] = "Inteligência artificial - Aplicações no direito||Algorítmos computacionais||Ciências cognitivas"

### Correção palavras-chave da instituição UFV

### UFV [1:126] dá pra pegar com código, acho
sequencia_links_UFV<-arquivo_teste$Link.de.acesso[arquivo_teste$Sigla.da.instituição.de.defesa=="UFV"]
sequencia_links_UFV

# Função para extrair as informações da página WEB de cada uma das linhas
extract_info <- function(url) {
  # Carregar página usando rvest
  page_1 <- read_html(url)
  
  # Extrair informações da classe "metadataFieldValue dc_subject_keyword"
  info <- page_1 %>%
    html_nodes(xpath = "//tr[td[@class='metadataFieldLabel dc_subject' and contains(text(), 'Palavras-chave:')]]/td[@class='metadataFieldValue dc_subject']") %>%
    html_text2()
  info <- gsub("\n", "||", info)
  return(info)
}

# Dados de entrada, link de acesso de cada trabalho da UFV com assunto não informado
links_UFV <- sequencia_links_UFV

# Vetor para armazenar as informações extraídas
palavras_chave_UFV <- character(0)

# For para a realização do web scrapping de cada linha
for (i in seq_along(links_UFV)) {
  link <- links_UFV[i]
  tryCatch({
    palavra_chave_UFV <- extract_info(link)
    palavras_chave_UFV <- c(palavras_chave_UFV, palavra_chave_UFV)
  }, error = function(e) {
    palavra_erro_UFV<-paste("Erro ao processar", link, ":", e$message)
    palavras_chave_UFV <-c(palavras_chave_UFV,palavra_erro_UFV)
  })
}
#print(palavras_chave_UFV)
#Gero o vetor de correção e corrijo as linhas da UFV
vetor_correcao_UFV<-palavras_chave_UFV
arquivo[arquivo$Assuntos.em.português=="Não informado pela instituição" & arquivo$Sigla.da.instituição.de.defesa=="UFV", "Assuntos.em.português"] <-vetor_correcao_UFV

### Correção palavras-chave da instituição UNB

### UNB [1:234], dando erro nos links de acesso originais o devido a uma mudança no págino do repositório
sequencia_links_UNB<-arquivo_teste$Link.de.acesso[arquivo_teste$Sigla.da.instituição.de.defesa=="UNB"]

#Correção dos links dos repositórios
sequencia_links_UNB<-str_replace(sequencia_links_UNB,"https://repositorio.unb.br/handle/","http://icts.unb.br/jspui/handle/")
sequencia_links_UNB<-str_replace(sequencia_links_UNB,"http://repositorio.unb.br/handle/","http://icts.unb.br/jspui/handle/")
sequencia_links_UNB

# Função para extrair as informações da página UNB
extract_info_UNB <- function(url) {
  # Carregar página usando rvest
  page_UNB <- read_html(url)
  
  # Extrair informações da meta tag "DC.subject"
  info_UNB <- page_UNB %>%
    html_nodes(xpath = "//meta[@name='DC.subject']") %>%
    html_attr("content")
  
  # Concatenar as informações separadas por "||"
  info_UNB <- paste(info_UNB, collapse = "||")
  
  return(info_UNB)
}

# Dados de entrada,links de acesso das linhas 
links_UNB <- sequencia_links_UNB

# Vetor para armazenar as informações extraídas e os links processados
palavras_chave_UNB <- character(length(links_UNB))
links_processados <- character(0)

# Loop para processar cada link de cada linha
for (i in seq_along(links_UNB)) {
  link <- links_UNB[i]
  tryCatch({
    palavra_chave_UNB <- extract_info_UNB(link)
    palavras_chave_UNB[i] <- palavra_chave_UNB
    links_processados <- c(links_processados, link)
  }, error = function(e) {
    palavra_erro_UNB <- paste("Erro ao processar", link, ":", e$message)
    palavras_chave_UNB[i] <- palavra_erro_UNB
  })
}

# Identificar links faltantes
links_faltantes <- setdiff(links_UNB, links_processados)

#print(palavras_chave_UNB)
##Gero o vetor de correção e corrijo o dataframe 
vetor_correcao_UNB<-palavras_chave_UNB
arquivo[arquivo$Assuntos.em.português=="Não informado pela instituição" & arquivo$Sigla.da.instituição.de.defesa=="UNB", "Assuntos.em.português"]<-vetor_correcao_UNB


### Correção palavras-chave da instituição USP

### USP [1:958], codigo feito na proxima sessão, só ajustar
sequencia_links_USP<-arquivo_teste$Título[arquivo_teste$Sigla.da.instituição.de.defesa=="USP"]
sequencia_links_USP
#arquivo[arquivo$Título=="Machine Learning Tools for Bioinformatics Problems","Assuntos.em.português"]= "Biclustering||Cas proteins||CRISPR-Cas systems||Gene expression data analysis||Machine learning"
#arquivo[arquivo$Título=="Contagion in economic networks: a data-driven machine learning approach","Assuntos.em.português"]= "Complex networks||Contagion||Economic system||Systemic risk"
#arquivo[arquivo$Título=="ML4JIT- um arcabouço para pesquisa com aprendizado de máquina em compiladores JIT.","Assuntos.em.português"]= " Aprendizado computacional||Montadores e compiladores"
#arquivo[arquivo$Título=="Discretização e geração de gráficos de dados em aprendizado de máquina","Assuntos.em.português"]= " Aprendizado de máquina||Discretização||Geração de gráficos"

%%R
### Correção palavras-chave da instituição USP

### USP [1:958], codigo feito na proxima sessão, só ajustar
sequencia_links_USP<-arquivo_teste$Link.de.acesso[arquivo_teste$Sigla.da.instituição.de.defesa=="USP"]
sequencia_links_USP<-sequencia_links_USP
sequencia_links_USP
## Ponto onde deixo uma ponta para refazer o web scrapping caso ele falhe em alguma linha durante a primeira vez que o código for rodado
posicoes_anomalas<-c(2000)#indexacoes_sem_assuntos_USP

extrair_palavras_chave_USP_1 <- function(url) {
  page <- read_html(url)
  palavras_chave_USP <- page %>%
    html_nodes(xpath = "//div[contains(@class, 'DocumentoTexto')][13]") %>%
    html_text()
  palavras_chave_USP <- gsub("\\n\\n", "||", palavras_chave_USP)
  return(palavras_chave_USP)
}

extrair_palavras_chave_USP_2 <- function(url) {
  page <- read_html(url)
  palavras_chave_USP <- page %>%
    html_nodes(xpath = "//div[contains(@class, 'DocumentoTexto')][12]") %>%
    html_text()
  palavras_chave_USP <- gsub("\\n\\n", "||", palavras_chave_USP)
  return(palavras_chave_USP)
}

links_USP <- sequencia_links_USP
palavras_chaves_USP <- character(length(links_USP))  # Inicializa vetor com o mesmo comprimento

for (i in seq_along(links_USP)) {
  link <- links_USP[i]
  if (i %in% posicoes_anomalas) {
    tryCatch({
      palavra_chave_USP <- extrair_palavras_chave_USP_2(link)
      palavras_chaves_USP[i] <- palavra_chave_USP
    }, error = function(e) {
      palavra_erro_USP <- paste("Erro ao processar", link, ":", e$message)
      palavras_chaves_USP[i] <- palavra_erro_USP
    })
  } else {
    tryCatch({
      palavra_chave_USP <- extrair_palavras_chave_USP_1(link)
      palavras_chaves_USP[i] <- palavra_chave_USP
    }, error = function(e) {
      palavra_erro_USP <- paste("Erro ao processar", link, ":", e$message)
      palavras_chaves_USP[i] <- palavra_erro_USP
    })
  }
}


# Vetor de correção e corrijo o dataframe
vetor_correcao_USP <- palavras_chaves_USP
indexacoes_sem_assuntos_USP <- grep("^(?!\\|\\|)", palavras_chaves_USP, perl = TRUE)
arquivo$Assuntos.em.português[setdiff(which(arquivo$Assuntos.em.português == "Não informado pela instituição" & arquivo$Sigla.da.instituição.de.defesa == "USP"), indexacoes_sem_assuntos_USP)] <- vetor_correcao_USP[setdiff(seq_along(vetor_correcao_USP), indexacoes_sem_assuntos_USP)]

### Correção palavras-chave da instituição PUC_RIO
sequencia_links_PUC_RIO<-arquivo_teste$Link.de.acesso[arquivo_teste$Sigla.da.instituição.de.defesa=="PUC_RIO"]
#sequencia_links_PUC_RIO

##Correção feita manualmente já que os dados tem que ser obtidos dentro do pdf, gero o vetor de correção e corrijo
vetor_correcao_PUC_RIO_1<-c((" Informática||Inteligência Artificial||Entretenimento Digital Interativo||Jogos Digitais"),("Artificial Intelligence||Multi-objective Optimization||Bio-inspired Algorithms||Machine Learning||Supervised Learning"),("Direito||Direito civil||Responsabilidade civil||Tecnologia||Inteligência artificial"),("Engenharia Elétrica||Engenharia de Materiais||Aprendizado Profundo||Inteligência Artificial Explicável||Imageamento Médico||Visão Computacional||Classificação||COVID-19"),("Informática||Inteligência artificial||Aprendizado de máquina||Interpretabilidade||Explanabilidade||Algoritmos interpretáveis"),(". Informática||Aprendizado de máquina||Processamento de linguagem natural||Segmentação textual||Análise sintática superficial||Aprendizado de transformações guiado por entropia"),("Informática||Sistema Multiagentes||Engenharia de Software||Aprendizado de Máquina"),("INTELIGÊNCIA COMPUTACIONAL||MACHINE LEARNING"),("Economia||Nowcasting||Aprendizado de Máquina||Avaliação de Previsão"),(" Engenharia Mecânica||Motor Diesel||Emissões de Poluentes||Gás Natural||Inteligência Artificial||Redes Neuronais||Algoritmos Genéticos"),("Aprendizado de Máquina||Sentiment Analysis||Classificação de Texto||Classificadores Bayesianos||Support Vector Machines"),("Informática||Educação baseada na Web||Objetos de aprendizagem||Ontologia||Banco de dados para e-Learning||Aprendizado de máquina||Classificação de textos||Naive-Bayes"),("Moderação de comentários||Aprendizado de Máquina||SVM||Classificadores Bayesianos||Boosting||Processamento de Linguagem Natural||Classificação de texto"),("Aprendizado de máquina||Estratégias de balanceamento||Modelo preditivo||Resistência aos Carbapenêmicos||Bactérias Gram-negativas"),("Redes de Longa Distância de Baixa Potência||Protocolos de Controle de Acesso ao Médio||Modulação LoRa||LoRaWAN||Parâmetros de Transmissão||Algoritmos de Aprendizagem por Reforço"),("Ondas guiadas ultrassônicas||Aprendizado automático||Aprendizado supervisionado||Aprendizado não-supervisionado||estimação de tração"),("Machine learning||proxy||smart proxy||deep smart proxy||poços inteligentes||produção de petróleo"),("Redes móveis auto-organizáveis||redes heterogêneas||redes cognitivas||balanceamento de carga"),("Propensão a cancelamento||Aprendizado de máquina||Árvore de decisão||Bagging||Random Forest||Boosting||Dados desbalanceados||Seguro de vida||Under-sampling||Over-sampling||SMOTE"),("Lagartixa Pneumática||Robô Escalador||Garra de Vácuo||Sistemas Eletropneumáticos||Aprendizado Computacional||Redes Neurais Artificiais"),("Direito à explicação||decisão algorítmica||inteligência artificial||proteção de dados pessoais||direitos fundamentais"),("Garantia de escoamento||Incrustação inorgânica||Medição de pH||Redes Neurais Convolucionais||Perceptrons Multi-Camadas"),("Extração de Lignina||Peróxido de Hidrogênio||Redes Neurais Artificiais||Algoritmo Genético||ANFIS"),("Inteligência Artificial||comunicação visual||criatividade"),("Aprendizado de máquinas||Considerações éticas||Avaliação de modelos||Transparência||Model Cards||Template de Metacomunicação Estendido||Engenharia Semiótica"),("Monitoramento de saúde estrutural||Ondas guiadas||Aprendizado de máquina||Conversão de modo||Aprendizado supervisionado"),("Aprendizado de Máquina||Processamento de Linguagem Natural||Vagas de Emprego"),("Custo em Saúde||Aprendizado de Máquina||Modelos Preditivos"),("aprendizado de máquina interativo||tecnologias adaptáveis||reabilitação física||detecção de anomalias||criação de dados sintéticos"),("Dinâmica Dissipativa de Partículas||Aprendizado de Máquina||Suspensões"))
vetor_correcao_PUC_RIO_2<-c(("Selo seco a gás||Aprendizado de máquina||Dinâmica dos fluidos computacional||OpenFOAM||REFPROP"),("Permeabilidade||Aprendizado de Máquina||Petrofísica||Perfis de Imagem||Teste de Formação||Perfis de Produção"),("Machine learning||previsão||inadimplência||educação superior||credit scoring"),("Processo hierárquico analítico||fuzzy||rede neural artificial||algoritmo genético||previsão de demanda"),("Machine learning||modelos preditivos||classificação||evasão de alunos||lifetime value"),("Robôs Móveis Autônomos||Veículos Militares||Lógica Fuzzy||Controle Preditivo||Sistemas Semiautônomos"),("consumo energético||eficiência energética||ferrovia||transporte pesado||aprendizado de máquina||redes neurais artificiais||florestas aleatórias||gráfico de dependência parcial||gráfico de valores acumulados locais"),("Aprendizado de máquina||processamento de linguagem natural||classificação de textos||extração de informações||aprendizado profundo"),("Otimização combinatória||Aprendizado de máquina||Meta-heurísticas||Problema de roteamento de veículos||Cortes combinatórios de Benders"),("Dados de Treinamento||Aumento de Dados||Redes Neurais Convolucionais||Pólipos||Colonoscopia"),("Inteligência Artificial||Aprendizagem de Máquina||Visão Computacional||Arquitetura de Software||Apoio a triagem diagnóstica||Entity-component-system"),("Avaliação não destrutiva||Ondas guiadas||Ultrassom||Aprendizado de máquina||Oleoduto||Soldagem"),("Aprendizado de Máquina||Destilação de Conhecimento||Perceptron Multicamada||Classificação Multiclasse"),("SISTEMAS INTELIGENTES||Inteligência artificial||Processamento de linguagem natural||TEXTOS DA WEB"),("Método dos Elementos Finitos||Materiais Compósitos||Concreto||Fibras||Análise Multiescala"),("Processamento de Linguagem Natural||Extração de Informação||Extração de Citação||Perceptron Estruturado||Agendamento de Tarefas Ponderado"),("Aprendizado de Máquina||Processamento de Linguagem Natural||Extração de Informação||Extração de Citações||Aprendizado de Transformações Guiado por Entropia||Perceptron Estruturado||Agendamento de Tarefas Ponderado"),("Detecção de falhas||Processo Tennessee Eastman||Redes Neurais Artificiais||Rede de Elman||Echo State Network"),("Busca de Arquiteturas Neurais||Algoritmos Evolucionários de Inspiração Quântica||Classificação de Imagens"),("linguagens declarativas||inteligencia artificial"),("Previsão||Falência||Regressão logística||Logit"),("RFM||Modelos de Classificação||CRM||COVID-19||Cluster"),("genetic algorithms||neural networks||inteligencia artificial"),("Sistema de inferência fuzzy tipo-2 intervalar||Problema de classificação||Método superior e inferior||Incerteza na média||Incerteza no desvio padrão"),("Sistema de inferência fuzzy tipo-2 intervalar||Problema de classificação||Método superior e inferior||Incerteza na média||Incerteza no desvio padrão"),("Aprendizado de Máquina||Mercado de Ações||Google Trends||Predição de Séries Temporais"),("Inflação||Machine Learning||Avaliação de Previsão||Model Confidence Set||Seleção de Variáveis"),("Artificial Intelligence||Semantic||algoritmos de classificação"),("Jogos Eletrônicos||Inteligência Artificial para Jogos||Sistemas MultiAgentes||BDI||Atos de Fala||Agentes Inteligentes em Jogos"),("Simulação Militar||Modelagem de Agentes||Inteligência Artificial"))
vetor_correcao_PUC_RIO_3<-c(("Sistemas de multiagentes||Mercado de valores||Competição||Simulador||Investidores||MASSES"),("Aprendizado de Máquina||Aprendizado por Reforço||Mercados Financeiros||Algoritmos||Otimização"),("inteligência artificial||Redes Neurais||Lógica Fuzzy||Racionamento||Energia elétrica"),("Adimplente||Clusterização||Fraudulento||Inadimplente||Perda Comercial||PPH’s"),("Inteligência Artificial||Arcabouços de Software||Simulação a Eventos Discretos||Planejamento Automático||Sequenciamento em Oleodutos"),("Proveniência de Dados||Sistema Multiagente||FProvW3C||ProvBDI4JADE||Sistemas Autônomos Explicáveis||Provenência em SMA"),("Energia eólica||velocidade do vento||séries temporais||Box-Jenkins||neuro-fuzzy||previsão"),("Algoritmos||Tecnologia||Sociedade da Informação||Autonomia||Racismo"),("Resistência||discriminação||governamentalidade algorítmica||moderação||Instagram"),("Active Learning||Anotações Assistidas||Aprendizado Profundo||Segmentação Semântica||Imagens de Mamografia"),("Indução de Programas||Aprendizado Profundo||Processamento de Linguagem Natural||Modelo Transformer||Extração de Relações"),("Desagregação de Cargas||Aprendizado Profundo||Redes Neurais Inversíveis||Autoencoders Variacionais||Base de dados industrial"),("Aprendizado de Máquina||Processamento de Imagens||Redes Neurais Convolutivas||Classificação de Objetos||Mineração de Dados"),("Processamento de linguagem natural||Aprendizado automático||Extração de informação||Ontologias||Curadoria de dados"),("Sistema de Múltiplos Classificadores||Interface Cérebro Computador||Imaginação Motora||EEG||Processamento de Sinais||Técnicas de Fusão||MLP||Algoritmos Genéticos"),("Reconhecimento de Objetos em Tempo-Real||Aplicações de Vídeo Interativo||TV Digital Interativa||Visão Computacional||Análise de Imagens"),("Boosting||Filtragem Colaborativa||Sistemas de Recomendação||Aprendizado de Máquina"),("Controle Preditivo||Aprendizado por Reforço||Redes Neurais Artificiais||Proxies||Reservatórios Petrolíferos||Campos Inteligentes"),("Aprendizado de máquina||Arvore de decisão||Aprendizado sensível ao custo"),("Segmentação de notícias||Renderização de páginas web||Aprendizado de máquina"),("Segmentação de notícias||Renderização de páginas web||Aprendizado de máquina"),("Sistemas multiagentes||mineração de dados||aprendizado de máquina"),("Detecção de Duplicatas||Resolução de Entidades||Integração de dados||Deep Web"),("Extração de Relacionamentos||Supervisão à Distância||Web Semântica||Aprendizado de Máquina||Processamento Natural de Linguagens"),("Aprendizado de máquina||SVM||Processamento de Linguagem Natural||Classificação de Textos||Mercado de Ações"),("Sistema Multiagente||Auto-Organização||Autoadaptação||Internet das Coisas||Aprendizado de Máquina"),("Processamento de Linguagem Natural||Aprendizado de Máquina||Embeddings||Pergunta-Resposta Interativo"),("Aprendizado de máquina||Análise de dependência||NLP"),("Regularização de domínios e modelos||Perceptron esparso||Seleção e indução de atributos||SVM"),("Descargas parciais||para-raios||transformador de corrente||gerador||separação de fontes||classificação de padrões"))
vetor_correcao_PUC_RIO_4<-c(("inteligencia artificial||algoritmo||visão computacional"),("Policiamento preditivo||política criminal||segurança pública||prevenção criminal||direitos constitucionais individuais"),("Otimização||gás de síntese||banco de dados||inteligência artificial"),("Filtragem Colaborativa||Aprendizado de Máquina||Publicidade Direcionada||World Wide Web"),("Mineração de textos||Desambiguação||Técnicas Agrupamento"),("Previsão||Séries Temporais||Seleção de modelos||Combinação de previsões||Métodos de ensemble||Bagging||Técnicas de regularização"),("Árvores Latentes||Representação Contextual||Resolução de Correferência||BERT||Clusterização||SpanBERT"),("Redes Neurais de Aprendizado Profundo||Extração de Características||Distúrbios respiratórios do sono||Sinais EEG polissonográficos||Sono"),("Aprendizado de máquina||Séries temporais||Teste de poços||Derivada de pressão||Shapelets"),("Robustez ao Ruído||Identificação Esparsa||Corrupção Ruidosa||Engenharia de Recursos||Pesquisa em Grade"),("Seleção de pontos||Matriz de co-associação||Propagação de rótulos||Aprendizado semi-supervisionado||Aprendizado activo"),("Design ótimo||Otimização Bayesiana||Projeto de risers||Sistema offshore||Engenharia submarina"),("Sismica||Amplitude versus ângulo||Inversão Sísmica||Avaliação de incerteza||Modelo de Markov Oculto||Modelo Convolucional"),("Aprendizado de Máquina||Aprendizado Supervisionado||Classificação de Entidades||Classificação de Produtos||WEKA||Framework Aprendizado de Máquina"),("Processo para estudar mídias sociais||Ferramentas computacionais personalizadas||Mapa de Associação de Comunidades||Problema de Seleção de Conteúdo||Análise de fóruns online||Análise de comunidades online||Análise de mídias sociais||Estudo de questões de saúde||Ciência social computacional||Ciência da Web"),("Detecção de danos||dinâmica de estruturas||passarela||redes neurais artificiais||estruturas 3D||variação dos modos de vibração"),("algoritmo genético||correção de erros quântico||códigos stabilizer"),("Monitoramento da Integridade Estrutural||Identificação de Sistemas||Aprendizado de Máquina||Aprendizado Supervisionado||Aprendizado não Supervisionado"),("explicação countrafactual||visualização de dados||tree ensembles"),("COVID-19||medidas de contenção||cobertura vacinal||XGBoost||valor de Shapley"),("Algoritmos Evolucionários com Inspiração Quântica||Comitê de Classificadores||Perdas Aparentes||Fraude"),("Processamento de linguagem natural||Aprendizado profundo||Redes neurais recorrentes||Long short-term memory||Aprendizado de máquina||Conditional random fields||Extração de informação||Direito||Provisões modificatórias"),("Aprendizado de máquina||Genômica||Metilação||Sequenciamento de RNA||Modelos de classificação||Modelos de regressão||Predição da eficácia a droga||Aprendizado semi-supervisionado||Aprendizado supervisionado||Câncer"),("Mobile Marketing||Marketing baseado em localização||Geolocalização||Conteúdo de mensagem||Promoção Mobile"),("Degradação do Design||Revisão de Código Moderna||Práticas de Revisão de Código||Aspectos Influentes||Aprendizado de Máquina"),("Intraday Traders||Agendamento de Intervalos Ponderados||Finança Computacional||Aprendizado de Máquina||Markowitz"),("Aprendizado por Reforço Profundo||Caixa Delimitadora||Conjunto de Dados||Aconselhamento||Deep Q-Network||Agente Virtual||Anotações"),("ORIENTAÇÃO||DADOS||inteligencia artificial"),("Big data||Econometria em alta dimensão||LASSO||Modelos de Fatores||Previsão"),("Nowcasting||Aprendizado de máquina||Big data||Previsão||Modelos de alta dimensão||COVID-19"))
vetor_correcao_PUC_RIO_5<-c(("epilepsia refratária||IRM negativa||pós-processamento||machine learning||detecção de lesão"),("Big Data||Aprendizado de Máquina||Modelos de Fatores para Ações||Investidores Institucionais"),("Mineração de Textos||Dados Não-Estruturados||Processamento de Linguagem Natural||Aprendizado de Máquina||Recuperação de Informação"),("Fluxos de capitais||Balanço de Pagamentos||Conta Financeira||Machine learning||Regularização"),("Engenharia Semiótica||Ferramentas de Design Sociotécnico||Design Responsável||Reflexão Ética||Aprendizagem de Máquina"),("Programação Semidefinida||Otimização Convexa||Operadores Monotônicos||Algoritmos Proximais"),("Aprendizado de Máquina||Ranking"),("Sistemas Multi-Agente||Inteligência Artificial||Machine Learning||Engenharia de Software"),("Otimização de portfolios||Atualização multiplicativa de pesos||Mercado ações brasileiro"),("Valor do cliente||ação de relacionamento||varejo||modelos de clientes||Machine learning"),("Fluxo de potência||Sistemas de distribuição||Machine Learning||Redes Neurais Artificiais||Contratos de demanda"),("Prêmio de seguros||previsão||aprendizado de máquinas"),("Aprendizado de máquina||Dinâmica dos fluidos computacional||Chamas não pré-misturadas||Combustão de metano/ar"),("Fontes Renováveis||Modelagem e Simulação||Cadeias de Markov||K-means||Simulação de Monte Carlo"),("Sistemas multi-agentes||agentes||linguagem de modelagem||XMI||MDA||MAS-ML"),("Efeitos de antecipação e defasagem||Momentum de fatores||Reversão de curto prazo de ações||Previsibilidade de retornos||Fatores de risco em ações"),("Refatoração||Manutenibilidade||Customização de refatoração"),("Estruturas de aço||vigas de aço||cargas concentradas||inteligência computacional||análise paramétrica||redes neurais||lógica nebulosa||algoritmos genéticos"),("Previsibilidade de retornos||Dados em alta dimensão||Aprendizado de máquinas||Modelos não-lineares"),("Regressão por mínimos quadrados||PLS||DPLS||PPLS||MKPLS||Paralelismo||Regressão não-linear||Multi-kernel||Funções de núcleo"),("Redes Neurais||Ratings||Estatística||Investimentos||Predição de Risco||Standard & Poor’s||Seleção de Atributos"),("Previsão de Séries Temporais||Modelos Nebulosos Evolutivos||Passo de Adaptação Variável"),("Machine learning||Natural language processing||Quotation extraction||Neural networks||Deep learning"),("Deep learning||Processamento de linguagem natural||lógica fuzzy||inteligência artificial"),("Metrologia||metrologia para qualidade e inovação||sensor virtual||LiDAR||nuvem de pontos sintética||sensoriamento remoto||rastreamento de raio"),("Conjuntos de árvores||Árvores de decisão||Machine Learning interpretável||Modelos de compressão de dados"),("WWW||Algoritmo||Relevância||Máquinas de busca||Classificação baseada em hiperlink"),("Extração de Relações||DBpedia||Wikipedia||Web Semântica||Framework"),("Aspen Plus||XGBoost||simulação||polimerização do polibutadieno"),("Busca de Arquiteturas Neurais||Algoritmos de Inspiração Quântica||Neuroevolução||Redes Neurais Convolucionais"),("Processamento Digital de Imagens||Refletância de Vitrinita||Redes Neurais Convolucionais||Petrografia de Carvão||Análise de Macerais"),("Nanopartículas||Difusão||Eletroforese||Translocação||Nanoporo"))
vetor_correcao_PUC_RIO<-c(vetor_correcao_PUC_RIO_1,vetor_correcao_PUC_RIO_2,vetor_correcao_PUC_RIO_3,vetor_correcao_PUC_RIO_4,vetor_correcao_PUC_RIO_5)
arquivo[arquivo$Assuntos.em.português=="Não informado pela instituição" & arquivo$Sigla.da.instituição.de.defesa=="PUC_RIO", "Assuntos.em.português"]<- vetor_correcao_PUC_RIO

### Correção palavras-chave da instituição UEL
sequencia_links_UEL<-arquivo_teste$Link.de.acesso[arquivo_teste$Sigla.da.instituição.de.defesa=="UEL"]
#sequencia_links_UEL

#Gero o vetor de correção e corrijo
vetor_correcao_UEL<-c(("Direito||Responsabilidade (Direito)||Inteligência artificial||Tecnologia"),("Inteligência artificial||Ciência da informação"),("Sistemas de telecomunicação||Sensoriamento espectral||Rádios cognitivos"),("Redes neurais (Computação)||Processamento de sinais - Técnicas digitais||Algoritmos de computador||Pele - Doenças||Aprendizado do computador"),("Doenças pulmonares||Diagnóstico visual - Software||Pulmões $x Doenças||Redes neurais e saúde||Aprendizado de máquina"),("Engenharia elétrica||Sistemas de Telecomunicações||Inteligência artificial"),("Banco de dados - Medidas de segurança||Tecnologia da informação - Sistemas de segurança||Aprendizado do computador||Sistema de detecção de intrusão"),("Computação||Aprendizado do computador||Transtornos do espectro autista||Transtornos do espectro autista - Diagnóstico"),("Carne||Qualidade||Carne bovina||Leitão (Suíno)||Indústria||Automação"),("Lógica difusa||Reconhecimento de padrões||Aprendizado do computador||Alimentos - Qualidade||Inteligência artificial"),("Jogos por computador - Aspectos sociais||Jogos em grupo - Aspectos sociais||Multimídia interativa||Facebook (Recursos eletrônicos)||Redes sociais on-line - Aspectos sociais||Inteligência artificial"),("Abastecimento de água nas cidades - Previsão||Redes neurais (Computação)||Água - Consumo||Abastecimento de água nas cidades - Estudo de casos||Análise de séries temporais"),("Direito negocial||Criptomoeda||Bitcoin"),("Eletromiografia||Coluna lombar - Músculos||Músculos - Avaliação||Fisioterapia"),("Engenharia elétrica||Redes elétricas inteligentes||Medidor inteligente||Internet das coisas"),("Aprendizado do computador||Inteligência artificial||Ciência da computação||DSTARS||Regressão multi-target||Regressão multi-output"),("Planejamento urbano||Arquitetura e sociedade||Caminhada - Índices||Arquitetura - Fatores humanos"),("Computação||Segmentação de imagem||Algoritmos de computador||Aprendizado do computador"),("Internet das coisas||Redes de computadores||Internet||Sistemas de segurança"),("Computação||Meta-aprendizado||Aprendizado ativo||Classificação de streams||Ajuste dinâmico"),("Milho||Milho híbrido||Marcadores moleculares||Redes neurais (Computação)"),("Engenharia elétrica||mMTC||Q-learning||Acesso aleatório||NOMA"),("Engenharia elétrica||Teoria dos jogos||Aprendizado do computador||Rede definida por software (Tecnologia de rede de computador)"),("Redes de computadores - Anomalias||Telecomunicações||Assinaturas digitais||Redes de computadores||Medidas de segurança"),("Inteligência artificial||Aplicações biológicas||Homologia (Biologia)||Soja - Ácido ribonucléico||Bioinformática"),("Lúpus eritematoso sistêmico||Artrite reumatóide||Síndrome metabólica||Moléculas de adesão celular||Patologia experimental"),("Cereais||Nutrição - Avaliação||Alimentos funcionais||Cevada||Alimentos - Valor nutritivo"),("Esclerose múltipla||Estresse oxidativo||Biomarcadores inflamatórios"))
arquivo[arquivo$Assuntos.em.português=="Não informado pela instituição" & arquivo$Sigla.da.instituição.de.defesa=="UEL", "Assuntos.em.português"]<-vetor_correcao_UEL

### Correção palavras-chave da instituição UFS [1]

sequencia_links_UFS<-arquivo_teste$Link.de.acesso[arquivo_teste$Sigla.da.instituição.de.defesa=="UFS"]
#sequencia_links_UFS

#Gero o vetor de correção e corrijo
arquivo[arquivo$Assuntos.em.português=="Não informado pela instituição" & arquivo$Sigla.da.instituição.de.defesa=="UFS", "Assuntos.em.português"] = "Machine learning||Biosignal processing||Hypoglycemia||D1namo dataset||Neurokit1||1dcnn"

### Correção palavras-chave da instituição INPE
sequencia_links_INPE<-arquivo_teste$Título[arquivo_teste$Sigla.da.instituição.de.defesa=="INPE"]
#sequencia_links_INPE

#Gero o vetor de correção e corrijo
vetor_correcao_INPE<-c(("Inteligência artificial||Sistemas especialistas||Satélites||CLIPS||Design science research"),("Aprendizado de máquina||SMILES||Redes neurais||Ciência dos materiais||Propriedades moleculares"),("Lithium-ion battery||State of charge||Gradient tree boosting||Multi layer perceptron"),("Eventos extremos||Mídias sociais||Análise de sentimento||Séries Temporais||Aprendizado de Máquina"),("Ondas gravitacionais||LIGO||Ruídos||Análise de dados||Machine learning"),("Computational astrophysics||Galaxy morphology||Machine learning||Deep learning"),("Inteligência artificial||Aprendizagem de máquina||Visão computacional||Navegação autônoma||Robótica||Redes Neurais"),("Tropical biodiversity||Imaging spectroscopy||Photogrammetry||WorldView-2||Individual tree crown delineation"),("Hyperspectral Remote Sensing||Laser Scanning||Data Fusion||Tropical Forest||Secondary Successions"),("Inteligência artificial||Agentes inteligentes||Planejamento automático||Replanejamento automático||Técnicas de reparo de plano"),("ADSGS||Artificial intelligence||Ground station networks||Hungarian algorithm||Software defined radio"),("Data analysis||Data science||Lattes platform||Bibliometrics||Artificial intelligence"),("Agente de planejamento||linguagem PDDL||Automação||Plano de Operação de Vôo de satélites"),("Satellite image time series||Spatiotemporal patterns||Self-organizing maps||Land use and cover changes||Class noise"),("Spectral-temporal response surface||sugarcane pre-harvest burning||Bayesian Network||plausible reasoning||soybean mapping"),("sensor de estrelas||simulador de estrelas||fonte de luz multiespectral||otimização de projeto multidisciplinar||otimização extrema generalizado"),("Inteligência artificial||Redes bayesianas||Inferência||Planejamento territorial"),("Supernovas||Classificação automática||Inteligência artificial||Lógica nebulosa||Supernova do tipo Ia"),("Inteligência artificial||Planejamento||Escalonamento||Representação do conhecimento||Autonomia"),("mineração de dados||descargas elétricas atmosféricas||sistemas convectivos||previsão meteorológica||estimação de densidade"),("Citizen science||Data science||Cluster analysis"),("Amazon forest||Deforestation||Machine learning||Remote Sensing"),("Sistemas autoadaptativos||Veículos Aéreos Não Tripulados||Visão computacional||Estimação da posição por imagens||Aprendizado de máquina"),("Earth’s magnetosphere||Van Allen radiation belts||Van Allen Probes mission||Electromagnetic ion-cyclotron waves||Neural networks"),("Spatial context||High spatial resolution image||GEOBIA||Semantic segmentation||Convolutional Neural Network"),("Satélite||Controle||Planejamento||POV||Automação||Processo||Desenvolvimento||Ciclo de vida||Sistema||Automato||IA||UML"),("Técnica de Breeding||Redes neurais artificiais||Classificadores Neuro-Difusod||Classificadores binários hierárquico||Modelo global do CPTEC"),("inteligencia artificial||representação do conhecimento||sistemas especialistas"),("Deep learning||Machine learning||Deforestation mapping||Convolutional neural networks||Deep neural networks"),("Planejamento automático||Estados inválidos||Classificação de dados||PDDL||Sistemas de informação"),("Análise de trajetórias||Integração de dados||Uso e cobertura da terra||Séries temporais"),("Redes Neurais artificiais||Restauração de imagens||Processamento digital de imagens||Fusão de filtros"),("Control||Design||Automation||Observer-based realization||Computational intelligence||Robustness"),("Sensoriamento remoto hiperespectral||Floresta Atlântica||WorldView-3||DART"))
arquivo[arquivo$Assuntos.em.português=="Não informado pela instituição" & arquivo$Sigla.da.instituição.de.defesa=="INPE", "Assuntos.em.português"]<- vetor_correcao_INPE

### Correção palavras-chave da instituição IEN [1:2]
sequencia_links_IEN<-arquivo_teste$Título[arquivo_teste$Sigla.da.instituição.de.defesa=="IEN"]
#sequencia_links_IEN

#Gero o vetor de correção e corrijo
vetor_correcao_IEN<-c(("Inteligência Artificial||Realidade Virtual"),("Redes Neurais||Lógica Nebulosa||Controle de Processos||Ergonomia Cognitiva"))
arquivo[arquivo$Assuntos.em.português=="Não informado pela instituição" & arquivo$Sigla.da.instituição.de.defesa=="IEN", "Assuntos.em.português"] <- vetor_correcao_IEN

### Correção palavras-chave da instituição UNIFEI
sequencia_links_UNIFEI<-arquivo_teste$Link.de.acesso[arquivo_teste$Sigla.da.instituição.de.defesa=="UNIFEI"]
sequencia_links_UNIFEI

#Gero o vetor de correção e corrijo
vetor_correcao_UNIFEI<-c(("Inteligência artificial||Engenharias"),("Diagnóstico de falhas||Módulos rastreadores||Aprendizado de máquina||Extração de características"),("Inteligência artificial||Raciocínio baseado em casos||Framework para desenvolvimento"),("jogos digitais||inteligência artificial||Planejamento automático||agente inteligente||labirintos"),("aprendizado de máquina||categorização de texto||anti-spam"),(" Inteligência artificial||Colônia de formigas||Lógica paraconsistente||Redes inteligentes"),("Mineração de Dados||Big Data||Florestas Aleatórias||Previsão de Carga"),("Modelos baseados em regras||Lógica nebulosa||Conjuntos aproximados||Inteligência artificial"),("Processo Decisório de Markov||Aprendizado por Reforço||Tráfego||Veículos"),("	Módulo de rastreamento||Automação||Técnicas de Inteligência Artificial"),("Processamento de imagens||Visão computacional||Ensaios em voo||Calibração anemométrica"),("Inteligência artificiais||Robôs móveis autônomos||Algoritmos de busca||Pathfinding A∗"),("Algoritmo evolutivo||Circuito CMOS||Portas lógicas CMOS||Algoritmo genético||Python"),("Arquitetura AuRA||Navegação de Robôs Móveis||Inteligência Artificial||Planejamento Automático||PDDL"),("Sistemas flexíveis de manufatura||Redes de Petri coloridas limitadas||Processos decisórios de Markov||Tempo real"),("Valores futuros||Séries temporais financeiras||Clusterização||Cluster||Fundo de ações||SOM||SVM"),("Controle por modos deslizantes||Controle robusto||Sistemas não lineares||Conjuntos aproximados"),("Otimização por enxame||Redes neurais||Previsão||Sistemas inteligentes||Sistemas híbridos"),("Reconhecimento de padrões||Helicóptero||Dinâmica de voo"),("Compensador||Lógica Fuzzy||Técnica Internal Model Control (IMC)||Regulador automático de tensão||Estabilizador de sistema de potência"),("Farmácia hospitalar||Simulação híbrida||Aprendizado por reforço||Gestão de estoques"),("Simulação a eventos discretos||Otimização via simulação||Metaheurística||Aprendizagem de máquina||Paralelismo"))
arquivo[arquivo$Assuntos.em.português=="Não informado pela instituição" & arquivo$Sigla.da.instituição.de.defesa=="UNIFEI", "Assuntos.em.português"] <- vetor_correcao_UNIFEI

### Correção palavras-chave da instituição UFCG
sequencia_links_UFCG<-arquivo_teste$Link.de.acesso[arquivo_teste$Sigla.da.instituição.de.defesa=="UFCG"]
sequencia_links_UFCG

#Gero o vetor de correção e corrijo
vetor_correcao_UFCG<-c(("Sistemas Especialistas||Inteligência Artificial||Operação de processos"),("Sistemas de software algébrico||Computação algébrica||Operações aritméticas||Algoritmo de Risch"),("Ferramentas de programação||Sindromes Oculares"),("Redes de computadores||Redes de filas||SIM/SAVAD"),("Arquitetura de computadores||Engenharia Elétrica||Sistemas tutores inteligentes||Ambiente de aprendizagem"))
arquivo[arquivo$Assuntos.em.português=="Não informado pela instituição" & arquivo$Sigla.da.instituição.de.defesa=="UFCG", "Assuntos.em.português"] <- vetor_correcao_UFCG

### Correção palavras-chave da instituição UFMG [1:2]
sequencia_links_UFMG<-arquivo_teste$Link.de.acesso[arquivo_teste$Sigla.da.instituição.de.defesa=="UFMG"]
#sequencia_links_UFMG

#Gero o vetor de correção e corrijo
vetor_correcao_UFMG<-c(("Inteligência artificial||Narrativa interativa|| Computação||Agentes inteligentes (software)"),("Computação||Mineração de dados"))
arquivo[arquivo$Assuntos.em.português=="Não informado pela instituição" & arquivo$Sigla.da.instituição.de.defesa=="UFMG", "Assuntos.em.português"] <- vetor_correcao_UFMG


### Correção palavras-chave da instituição FGV [1:8]
sequencia_links_FGV<-arquivo_teste$Link.de.acesso[arquivo_teste$Sigla.da.instituição.de.defesa=="FGV"]
#sequencia_links_FGV

#Gero o vetor de correção e corrijo
vetor_correcao_FGV <- c(("Memória - Simulação por computador||Computadores neurais||Inteligência artificial distribuída"), ("Machine learning||Corruption||Text mining||Measurement error"), ("Cognitive modeling||Rotational Sparse Distributed Memory||Network motifs||Semantic networks||Spreading activation||Sparse distributed memory"), ("Processo decisório - Modelos matemáticos||Psicologia cognitiva"), ("Sustainable Supply Chain – Brasil||Blockchain||Sustainable innovation||Green innovation"),("Processo decisório||Contabilidade"),("Processamento da linguagem natural (Computação)||Petróleo e gás||WordNet"),("Pluralismo||Estado||Lógica||Burocracia||Sociologia organizacional||Agências reguladoras de atividades privadas"))
arquivo[arquivo$Assuntos.em.português=="Não informado pela instituição" & arquivo$Sigla.da.instituição.de.defesa=="FGV", "Assuntos.em.português"] <- vetor_correcao_FGV

### Correção palavras-chave da instituição UFVJM
sequencia_links_UFVJM<-arquivo_teste$Link.de.acesso[arquivo_teste$Sigla.da.instituição.de.defesa=="UFVJM"]
#sequencia_links_UFVJM

#Gero o vetor de correção e corrijo
vetor_correcao_UFVJM<-c(("Biomassa florestal||Floresta Natural||Inteligência artificial"),("Evasão escolar||Mineração de dados||Aprendizado de máquina"),("Aprendizado de máquina||Distúrbios metabólicos||Novilhas||Zootecnia de precisão"),(" Ciência de Dados||Educação||Avaliações em Larga Escala||Aprendizado de Máquina||PISA"),("Aprendizado de máquina||Atividade enzimática||Celulases||Espectroscopia no infravermelho próximo||Quimiometria||Xilanases"),("Estilos de aprendizagem||Sistema de Gerenciamento de Aprendizagem||Sistema Adaptativo e Inteligente para Educação||Inteligência Artificial||Modelo de Estilo de Aprendizagem Felder-Silverman’s"),("Estilos de Aprendizagem||Detecção automática de Estilos de Aprendizagem||Redes Bayesianas||Ambientes Virtuais de Aprendizagem||Educação"),("Linguagem||Sistemas Complexos||Multiagente||Pragmatismo||Enação"),("Leitura protocolada||Chatbot||Inferência||Ensino"),("Sistemas Tutores Inteligentes||Modelagem autônoma||Q-Learning||Melhoria na convergência||Metaheurísticas||Lista Tabu||GRASP"),("Dynamic scripting||Sistema adaptativo e inteligente para a educação||Estilos de aprendizagem"),("Twitter||Bots||Detecção de bots||Redes Sociais||Indução de regras"),("Ambientes virtuais de aprendizagem||Consciência situacional||Evasão||Tomada de decisão"),("Registro de ocorrência de incêndio||Incêndio florestal||APA do Alto do Mucuri||Random Forest||Unidade de conservação"),("Mente||Consciência||Propriedades||Emergentismo"))
arquivo[arquivo$Assuntos.em.português=="Não informado pela instituição" & arquivo$Sigla.da.instituição.de.defesa=="UFVJM", "Assuntos.em.português"] <- vetor_correcao_UFVJM


### Correção palavras-chave da instituição UNISINOS [1]
sequencia_links_UNISINOS<-arquivo_teste$Link.de.acesso[arquivo_teste$Sigla.da.instituição.de.defesa=="UNISINOS"]
#sequencia_links_UNISINOS

#Gero o vetor de correção e corrijo
arquivo[arquivo$Assuntos.em.português=="Não informado pela instituição" & arquivo$Sigla.da.instituição.de.defesa=="UNISINOS", "Assuntos.em.português"] = "Computação aplicada||Industria||Ontologia||Aprendizado de máquina||Manutenção preditiva"

### Correção palavras-chave da instituição UFSCAR
sequencia_links_UFSCAR<-arquivo_teste$Link.de.acesso[arquivo_teste$Sigla.da.instituição.de.defesa=="UFSCAR"]
#sequencia_links_UFSCAR

#Gero o vetor de correção e corrijo
vetor_correcao_UFSCAR<-c(("Never ending learning||Conversing learning||Social Web"),("Semantic drift||Bootstrap learning||Crowd-powered systems"),("Brain Computer Interface||EEG classification||Drowsiness detection||K-Nearest Neighbors||EEG signal processing||Kd-trees||Random forest||Feature selection"))
arquivo[arquivo$Assuntos.em.português=="Não informado pela instituição" & arquivo$Sigla.da.instituição.de.defesa=="UFSCAR", "Assuntos.em.português"] <- vetor_correcao_UFSCAR


### Correção palavras-chave da instituição UFRN [1]
sequencia_links_UFRN<-arquivo_teste$Título[arquivo_teste$Sigla.da.instituição.de.defesa=="UFRN"]
#sequencia_links_UFRN

#sequencia_links_UFRN
arquivo[arquivo$Assuntos.em.português=="Não informado pela instituição" & arquivo$Sigla.da.instituição.de.defesa=="UFRN", "Assuntos.em.português"] = "Fuzzy logics||Monoidal logic||Interval mathematics||T-norm||Residuated lattices||ML-algebras"


### Correção palavras-chave da instituição INATEL
sequencia_links_INATEL<-arquivo_teste$Título[arquivo_teste$Sigla.da.instituição.de.defesa=="INATEL"]
sequencia_links_INATEL

#sequencia_links_INATEL
arquivo[arquivo$Assuntos.em.português=="Não informado pela instituição" & arquivo$Sigla.da.instituição.de.defesa=="INATEL", "Assuntos.em.português"] =  "Internet of things||IoT Architecture||Middleware||Reference model||Platform||Constrained application protocol||Message Queuing Telemetry Transport||Hypertext Transfer Protocol||Industrial Internet of Things||Performance Evaluation||Bridge||Gateway||Application"

### Exporto o arquivo limpo e as analises serão realizadas em outro código
write.csv(arquivo,"/content/drive/MyDrive/Arquivo_leila/Tratamento_Leila/arquivo_limpo.csv",row.names=FALSE)


### Abre o arquivo .csv que já foi tratado, adicionar o diretório específico
arquivo<-read.csv("/content/drive/MyDrive/Arquivo_leila/arquivo_limpo.csv", encoding="UTF-8")
#head(arquivo$Keyword)

### Definição das palavras-chave em cada tópico
## Tópico 1: Aprendizado de máquina
sub_topico_1_1_Algoritmos_e_software<- c("Aprendizado de M?quina","Machine learning","Machine learning Automático","objetos de Machine learning","Machine learning online","Machine learning ATIVA","ML-algebras","Machine learning DE M?QUINA","Machine learning comportamental","Machine learning de m?quina","Machine learning Semi-Supervisionada","Machine learning on-line","Máquina de Vetor de Suporte","Rede definida por software","Machine learning semissupevisionado","Conversing learning","Learning real-time a*","Machine learning significativa","Machine learning computacional","Teoria do Machine learning","Software - Desenvolvimento","desempenho e Algoritmos& de algoritmos","Predição da qualidade de sementes de soja armazenadas em diferentes ambientes e embalagens usando Machine learning","Machine learning pela descoberta","Software - reutilização","Machine learning de m??quina","Aprendizado semissupervisionado sem fim","Machine learning aplicado à saúde","objeto de Machine learning","Machine learning - Machine learning","Automated Machine learning (AutoML)","Comitês de Machine learning","boosting","Avaliação não supervisionada","árvores de decisão","Auto machine laerning","Aprendizado semissupervisionado","Aprendizado Semi-Supervisionado","Aprendizado por máquina","Aprendizado de programação","aprendizado de maquinas","Aprendizado de maquina","Aprendizado de computador","Aprendizado automático","Ambientes Online de Machine learning","Ambientes de Machine learning por Computador","Algoritmos de Machine learning","Ambiente Interativo de Machine learning","Machine learning (Inteligência artificial)","Algoritmo propagação de afinidade","Algoritmo Twins","Transfer learning","Algorithms","Ambientes virtuais de Machine learning","Meta-learning","Machine learning supervisionado","Estilos de Machine learning","Aprendizado não supervisionado","Automated Machine learning"," Machine learning","Algoritmo de Machine learning","Algoritmos& por enxame de partículas","Algorítmos de computador","Machine learning profunda","Algoritmos de computador","Modelos de Machine learning","ALGORITMOS (PROGRAMA??O)","Machine learning", "Protótipo de software","Modelagem matemática","Machine learning colaborativa","Meta-aprendizado","Máquina de vetores de suporte","Reconhecimento de padrão","Aprendizado de m?quina","Machine learnings","Algoritmos&","Aprendizado : máquina","Lógica","Software engineering", "Algoritmos","Software","Ambiente virtual de Machine learning","Lógica simbólica e matemática","Árvore de decisão","Ambiente : Machine learning", "Simulação","Framework", "Algoritmos& combinatória", "Redes bayesianas","Árvores de decisão","Random Forest", "Machine learning - Técnica", "Técnicas de Machine learning", "Machine learning techniques", "Meta-Machine learning", "Machine learning Techniques", "Framework Machine learning", "Machine learning (ML)", "MACHINE-LEARNING", "Machine learning sem-fim", "Weak supervision", "Supervised algorithms", "Unsupervised algorithms", "Sistemas classificadores evolutivos", "Classificação ativa semi-supervisionada", "Algoritmos de regressão", "Regressão não-linear", "Algoritmo supervisionado", "Algoritmo não supervisionado", "Classificação ativa semi-supervisionada", "Algoritmos de classificação", "Combinação de classificadores", "Classificadores", "Aprendizado por similaridade", "Aprendizado de máquina", "Aprendizado semi-supervisionado", "Aprendizado por transferência", "Aprendizado supervisionado", "Aprendizado não-supervisionado", "Transferência de aprendizado", "Aprendizado adversarial", "Aprendizado supervisionado de máquina", "Aprendizado incremental", "Aprendizado ativo", "Meta-Aprendizado", "Aprendizado baseado em instância", "Aprendizado auto-supervisionado", "Aprendizado on-line", "Aprendizado neural", "Aprendizado híbrido", "Aprendizado de rankings", "Aprendizado de métrica", "Aprendizado desbalanceado", "Raciocínio baseado em casos", "Algorítmos", "Programação heurística", "Algoritmos& multiobjetivo", "Desenvolvimento de software", "SVM", "Algoritmo")
sub_topico_1_2_Aprendizado_por_Reforco<- c("Aprendizado por Reforço","Algoritmos de Machine learning por Reforço","Aprendizado por refor?o","Machine learning por reforço","Reinforcement Learning","Support vector machine","Reconhecimento de padrOes","ALGORITMO DE APRENDIZADO POR REFORÇO", "Deep reinforcement learning", "Aprendizado por reforço profundo", "Aprendizado por Reforço Profundo", "Aprendizado por reforço")
sub_topico_1_3_Redes_Neurais_e_Deep_Learning<- c("Classificadores neurais","ANN","redes neuronais","Rede neurais (Ciência da computação)","Neural networks","Rede Neurais Convolucionais (CNNs)","Rede neural convolucional","Modelos neurais x estudo comparativo","Machine learning profunda multivisão","Máquinas de vetores suporte","Redes neurais sem peso","Redes neurais Siameses","Redes neurais Sem Peso","Redes neurais (ANN)","Redes neurais Profundas","Raciocinio qualitativo : Tecnicas : Analise de regimes : Grafos : Diagnostico de falhas em sistemas fisicos","Redes neurais por pulsos","Grafo de interações NK","Grafo (Sistema de computador)","Redes neurais - RNA","Máquina de suporte vetorial","Máquinas de vetor de suporte","Algoritmo kNN","Peso k-Vizinhos Próximos (W-kNN)","Redes neurais de Hopfield","redes cognitivas","Neural Networks","rede neuro fuzzy","artificial cognitive system","compressão de Redes neurais","Computadores neurais","Ciência da computação neural","Ciência da computação Redes neurais","Busca de Arquiteturas Neurais","Aprendizado em grafos bipartidos","Algoritmo Mask RCNN","Algoritmo U-Net","Rede Neural Convolucional","Algoritmo Non-maximum Suppression","Redes neurais recorrentes","Long Short-Term Memory","Redes neurais (RNAs)","RNA", "Redes Neurais e Deep Learning","Rede neural", "Deep Learning","Previsão de preços","Complex networks","árvore de decisão","Rede neural artificial","Rede Neural Artificial", "Redes neurais", "Deep learning", "Deep learning (Machine learning)","Ciências cognitivas","Análise de componentes principais", "deep learning", "Aprendizado por reforço profundo", "Aprendizado por Reforço Profundo", "Aprendizaje profundo", "Multilayer perceptron", "multilayer perceptron", "Multilayer perceptrons", "Multi layer perceptron", "MultiLayer perceptron", "perceptron", "Perceptron", "Perceptron esparso", "Multi-layer perceptron", "Perceptron multocamadas", "Feedforward neural network", "Back-propagation", "Backpropagation", "Modelo Convolucional", "neural networks", "Teoria dos grafos", "fedes neuronais", "rede neural", "rede neural convulacional", "Rede neuronal", "Neural", "rede neuronal", "Deep neural networks", "Convolutional neural networks", "Artificial neural network", "Convolutional neural networks", "Neural networks (Computing)", "Generative adversarial networks", "Recurrent neural networks", "Multidimensional graph neural network", "Deep neural network", "Neural nets", "Network-based learning", "Convolutional Neural Networks", "Neural network (Computer science)", "Convolutional neural network", "Convolution neural network", "Interpretability of convolutional neural networks", "Neural network multilayer perceptron", "Recurrent Neural Networks", "Neural networks (Computer)", "Convolutional Neural Network", "Artificial neural networks", "Neural network", "Siamese neural network", "Neural networks (Computer science)", "Neural networks (Computer sicence)", "Deep convolutional neural networks", "Graph convolutional networks", "Deep reinforcement learning", "Recurrent neural network", "Rede neural autorregressiva não linear com entradas exógenas", "Neural network visualization", "Ensemble of neural networks", "Neural Network (Computer Science)", "Deep-wavelet neural networks", "Deep Learning Network", "Graph convolutional neural networks", "Redes neurais Modulares", "Rede Neural Auto-Organizável", "Rede Neural Profunda", "Rede Neural de Multitarefas", "Rede de Machine learning Profunda", "Redes neurai", "Redes neurais de Deep learning", "Aprendizado neural", "Deep reinforcement learning", "Deep learning de Métrica", "Rede neural profunda", "Rede neural recorrente", "Rede neural de Deep learning", "Rede neural MLP", "Redes neurais Recorrentes", "Interpretabilidade de Redes neurais", "Redes neurais paraconsistentes", "Committees of neural nets", "Convolutional neural networks")
sub_topico_1_4_Clustering<- c("Biclustering","agrupamento","Algoritmo k-means","Análise por agrupamento","Cluster","Algoritmo K-Means","k-Nearest Neighbors (KNN)","K-Nearest Neighbor","Índice de validação de agrupamentos","Clustering dinâmico","Problema de Agrupamento Autom?tico","Clusterização difusa","Agrupamento de opinião","Analise de Agrupamento","AGRUPAMENTO TEXTUAL","ASSOCIATION RULES","agrupamento de dados","Agrupamento Fuzzy de Dados","Agrupamento Fuzzy","Agrupamento de séries temporais","Agrupamento de imagens", "Hierarchical clustering","Affinity propagation algorithm", "clustering","Dynamic color clustering","K-means", "Subspace Clustering","clusters","Conjuntos aproximados","Análise de Agrupamento", "Clustering algorithms","Agrupamento de dados", "Clustering","Agrupamento", "Incremental clustering", "partition clustering", "Interactive document clustering", "Text clustering", "Partitional clustering", "Constrained clustering", "Bi-agrupamento de dados", "Clusterização")
sub_topico_1_5_Regressao_e_Classificacao<- c("Regressão","Classificadores baseados em Machine learning","REGRESSÃO SIMBÓLICA","Regressão Logística", "Mutual information regression","Componentes problema de classificação","Classificadores Multirrótulo","Classificador de padroes","Classificação supervisionada abordagem Orientada a região","ALGORITMOS DE CLASSIFICA??O E BUSCA","Classificação multirrótulo","CLASSIFICAÇÃO ONE-CLASS","classificação Machine learning prevenção à fraude detecção carão de crádito","CLASSIFICAÇÃO DE MOVIMENTOS","Classificação de padrões","Classificação de Vídeo","Classificação Automática de Modulação","Classificação de atividades pedagógicas","Regressão logística","Classificação de documentos","Classificação de Imagens","“Random Forest”","QSAR","vetores de suporte","Seleção dinâmica de classificadores","Identificação biométrica","Modelos de Regressão","Kriging", "Regression and autoregression","Ensemble de classificadores","Detecção de Discurso de Ódio","Classificação de vídeos","Classificação de texto", "Regression", "regressão logística","Best-Worst Method", "SVR regressor","Seleção de atributos", "Regressão linear", "Analise de regressão", "Análise de regressão", "Regressão stepwise", "Algoritmos de regressão", "regressão logística", "Cox regressão", "regressão", "Regressão não-linear", "KNeighbors regressor", "Regressão beta", "Text regression", "Regressão de parâmetros", "Regressão direta", "Stepwise regression", "Regresso", "Análise de regressão e correlação", "Regressão por vetores suporte", "Modelos de regressão", "Regressão e autorregressão", "Multiple regressions", "Classificação", "Classificação litológica", "classificadores", "Classificação de Risco", "Sistema de Classificação", "Event Classificação", "Image Classificação", "Estatistica - Classificação", "Statistics - Classificação", "Sistemas de Classificação", "Entrada acústica - classificação", "Classificação multiclasse", "Multiclass Classificação", "Classificação", "Protein Classificação", "Peixe - classificação", "Classificação de imagens (construção civil)", "Text Classificação", "Classificação de imagens", "Data Classificação", "Ordinal Classificação", "Classificação supervisionada", "classificação de imagens", "Classificação Models", "Classificação committees", "Classificação", "Classificação linear", "Sediments Classificação", "EMG Classificação", "Métodos de classificação", "Classificação methods", "Sistemas de multi-classificação", "Classificação Binária", "Algoritmos de classificação", "Classificação de substâncias", "Classificação hierárquica", "Sistemas de multiclassificação", "Modelos de classificação", "Classificação não supervisionada", "classificação", "Classificação of short circuit", "Classificação automática", "Serviço de classificação", "Classificação de descargas parciais", "Classificação of partial discharges", "Multilabel Classificação", "Classificação multi-classe", "Multi-class Classificação", "Classificadores", "Plantas - classificação", "Classificação de Conferências", "Images Classificação", "Classificação de algoritmos", "Classificação : Dados", "Classificações", "Classificação de e-mails", "Classificação Associativa", "Classificação textual", "Classificação de sinais", "text Classificação", "Classificação de vinhos", "Combinação de classificadores", "Classificação de anuros", "Anuran Classificação", "Clasificación", "Classificação", "combinação de classificadores", "Classificação Multiclasse", "Classificação problems", "EEG Classificação", "Hierarchical Classificação", "Problema de classificação", "algoritmos de classificação", "Classificação ativa semi-supervisionada", "Classificação não paramétrica", "Classificação baseada em instância", "Classificação justa", "Florestas - Classificação", "classificação de padrões", "Classificação de atividades econômicas", "Classificação de abelhas", "Rochas - Classificação", "Classificação - computadores", "Classificação de emoções faciais", "Classificação dos riscos", "Classificadores (Linguistica)", "Problemas de classificação", "Classificação automática de issues", "Automatic issues Classificação", "Classificador", "Classificação de uma classe", "Sistemas de classificação múltipla", "Classificação de vértices", "classificação de dados", "Classificação da informação", "classificação oneclass", "Traffic Classificação", "Classificação binária", "Binary Classificação", "Biologia - Classificação", "classificador", "Plant Species Classificação")

## Tópico 2: Processamento de linguagem natural
sub_topico_2_1_Processamento_de_linguagem_natural <- c("Processamento de linguagem natural","Argumentação","Redes Semânticas","Processamento automático das línguas naturais","Sumarização automática","Semântica - Cognição","Semantic drift","Processamento automático de línguas naturais","Jornalismo Automatizado","PROCESSAMENTO DA LINGUAGEM NATURAL","linguagens declarativas","Intertextualidade","Linguística aplicada","Textos enganosos","Conversing learning","Short text topic modeling","Teoria do Léxico Gerativo","Text Classificação pipelines","Linguistica - Processamento de dados","Named entity recognition","reconhecimento facial","Análise de assuntos","Reconhecimento automático da voz","Gramatica gerativa","chatbot Tay","Modelos de linguagem","Teorias da Estética da Recepção","Compressão sentencial","Argument structure","Classificação de documentos","CNPQ::LINGUISTICA","Aquisição da linguagem","Anotação de papéis semânticos","Atribuição de autoria","Análise multidocumento","Análise de textos ruidosos","Análise de fóruns online","Análise do Discurso","AGRUPAMENTO TEXTUAL","Semiótica computacional", "Natural language processing","semiose","Reconhecimento de fala","PROCESSAMENTO DE LINGUAGEM NATURAL","Emoções","Mineração de textos","Discurso de\nÓdio Punitivista","Linguagem","Leitura","Detecção de Discurso de Ódio","Context awareness","Classificação de texto", "Processamento de língua natural","Semiótica", "Processamento da linguagem natural (Ciência da computação)","OCR","Agentes Afetivos", "Processamento de Linguagem Natural", "Linguagem natural", "Processamento de linguagem natural (Ciência da computação)", "Natural Language Processing", "Processamento da linguagem natural", "Linguagem e línguas", "Language and languages", "Natural Language Generation", "Formal languages", "Cross-lingual learning", "processamento de linguagem natural", "Processamento : Linguagem natural", "Geração de linguagem natural", "Interpretação de linguagem natural", "Processamento\\nde Linguagem Natural", "Linguagem Natural", "Natural language inference", "Geração de língua natural", "Neural language models", "Processamento de lingua natural", "Natural Language", "Lenguaje Natural", "Machine learning de línguas estrangeiras", "Machine learning de línguas pós-humana", "Linguagem natural - processamento", "Geração de Linguagem Natural", "Linguagem natural (computadores)", "Semantica", "Análise de sentimentos", "Dialogue sentiment analysis", "Análise de sentimento baseado em contexto", "Context-based sentiment analysis", "Sentiment Analysis", "Segmentação semântica", "Análise de sentimento", "Sentiment analysis", "Análise de Sentimento", "análise de sentimentos", "An?lise de Sentimento", "Análise de Sentimentos", "Sentimento textual", "Textual sentiment", "análise de sentimento", "Reconhecimento de gestos", "Índice de sentimento", "Sentimento bancário", "Sentiment index", "Banking sentiment", "Enriquecimento de texto", "Aspect sentiment triplet extraction", "Semantic segmentation", "Text representation", "Sentimentos", "Gesture Recognition", "Reconhecimento de Gestos", "Computational Linguistics", "Linguística computacional", "Recursos léxicos computacionais", "Tradução", "Tradução automática", "tradução automática", "Assistente virtual inteligente", "Chatterbots", "Chatbot", "Chatbots", "Chatterbot", "chatbot", "Chatbot Tay", "Speech processing systems", "Text processing (Computer science)", "Processamento de texto", "pré-processamento de textos", "Pré-processamento textual", "Sistema de processamento de voz", "Representação semântica", "Processamento de textos", "Sumarização Automática Multidocumento", "Classificação automática de documentos", "pontuação automática de ensaios", "Análise do discurso", "Analise do discurso", "Discourse", "Partes do Discurso", "Fóruns de discussão", "Discurso político")
sub_topico_2_2_Reconhecimento_de_Gestos <- c("Sign language", "Brazilian Sign Language", "Brazilian Sign Language - Equipment and supplies", "Brazilian sign language","Língua brasileira de sinais", "linguagem visual", "Sign language Fatigue", "Gesture Recognition", "Reconhecimento de gestos", "Reconhecimento de Gestos", "Reconhecimento de Gestos", "Gesture recognition on surfaces", "Gesture recognition", "Processamento de imagens faciais")

##Tópico 3: Visão computacional
sub_topico_3_1_Visao_Computacional <- c("Visão Computacional","Processamento de imagens","Análise computacional de imagens","PROCESSAMENTO DE IMAGENS","Imagem por ressonância magnética","Imaging spectroscopy","Descri??es de Imagens","Machine learning profunda multivisão","Qualidade de imagem - avaliação","Satellite image time series","Reconhecimento de Imagens","Processamento de imagens - Técnica","Reconhecimento facial","Servovisão","Neuroimagem","Visão artificial","reconhecimento facial;","Imagens digitais - Análise","Processamento de imagens-Técnicas digitais","Processamento de imagens - técnicas digitais","Vídeo digital","Image Recognition","Análise de viés de cor","Colorização de imagens","Imagens como recursos de informação","Classificação de Vídeo","Classificação de Imagens","Agrupamento de imagens","Algoritmo Non-maximum Suppression","Dynamic color clustering","Processamento e análise de imagens","Processamento de imagem","Diagnóstico por imagem","Imagem humana","Classificação de vídeos", "Image Classificação","Sentinel-2", "Classificação de imagens (construção civil)","Cálculo de pré-imagem", "Classificação de imagens","OCR","Ciência da computação gráfica", "Graph convolutional networks", "Graph convolutional neural networks", "Visão computacional", "Imagens de Visão computacional", "Computer-assisted Visão computacional", "Visão computacional hiperespectral", "Agricultura - Visão computacional", "Visão computaciona", "Visão computacional com RPAS", "Digital Visão computacional", "visão computacional", "Florestas - Visão computacional", "Forests - Visão computacional", "Facial motion capture", "Visão computacional - Digital techniques", "Bovinos de corte - Visão computacional", "Sistemas de visão computacional", "Ciência da computação Visual", "Visão computacional - Digital technique", "Visão computacionales", "Imagens por Visão computacional", "vis?o computacional", "Visual computational sociology", "Sociologia computacional visual", "Vis?o Computacional", "Visão computacional images", "Agriculture - Visão computacional", "Vis?o computacional", "Satelites artificiais em Visão computacional", "Hyperspectral Visão computacional", "Incêndios florestais - Visão computacional", "DIGITAL Visão computacional", "Visão computacional - Mata Atlântica", "Pastagens - Manejo - Visão computacional", "Satélites artificiais em Visão computacional", "Agricultura de precisão - Visão computacional", "Ciência da computação visual", "Vis??o Computacional", "Image segmentation", "Diagnosis by image", "Imaging processing", "Image diagnostics", "Image analysis", "Imagens médicas", "Processamento de imagens digitais", "Image segmentation", "Processamento Digitais de Imagens", "Sinais e Imagens", "Signs and Images", "Classificação em imagens", "Imagens", "Imagens médicas", "Processamento digital de imagens", "Análise de imagens", "segmentação interativa de imagens", "Imageamento", "Segmentação de imagem", "segmentação de imagem", "Detecção de faces", "Detecção de objeto", "Detecção de objetos", "Técnicas de detecção de objetos", "detecção de objetos", "Facial expressions recognition", "Interpretação de imagens", "Segmentação de imagens", "Image Segmentation", "No-reference image quality assessment", "Image restoration", "Segmentação de imagens coloridas", "Reconstrução de imagens", "Information visualization", "Computational visualization", "Visualização computacional", "Seismic imaging", "Remote-sensing images", "Imagens manipuladas", "Imagens histológicas", "Imagens Digitais", "Processamento de imagens digitais", "Paisagem", "Imagens digitais", "imagens sísmicas", "Imagens multiespectrais", "Imagens de ressonância magnética", "Magnetic resonance imaging", "Forensic image analysis", "imagens térmicas", "Imagem tridimensional", "Signs and Images", "Processamento de imagens faciais", "Imagens bidimensionais", "Segmentação de imagens coloridas", "Segmentação", "Imageamento por ressonância magnética", "Navegação visual", "Segmentação de imagens coloridas", "Levantamentos Fotométricos", "Imageamento", "Deforestation mapping", "Image restoration", "Reconstrução de imagens", "Stereo vision", "Representações não Foto-Realísticas", "Non-Photo-Realistic Representations", "Sensoriamento espectral", "3D", "Face verification","Imagens fotográficas digitais intrabucais oclusais")

##Tópico 4: Robótico, Controle e Automação
sub_topico_4_1_Internet_das_Coisas <- c("Internet das coisas","Aplicativos móveis","5G","Smart home","Internet of things","Internet das Coisas","COMUNICAÇÃO IOT","Internet das coisas - indústria 4.0")
sub_topico_4_2_Robotica <- c("Robótica","Agricultural robot","Robôs Dinâmica","Robôs-Programação","Robôs móveis autônomos","Robótica de serviço","Humanização de robôs","Robótica (simulação computacional)","Robo : Robotica", "Robotica","Advogado-robô","Domótica","chatbot Tay","Androide", "Robôtica","Robots - Control systems","Robôs móveis", "robótica","robôs móveis", "Robotic", "robótica inteligente","Circuitos integrados", "Robôs", "Robos", "Robô", "Robótica social", "Navegação autônoma", "Navegação", "Navegação aérea", "Sistemas autônomos", "Controle de robôs", "Comunicação humano-robô", "Interação humano-máquina","Interação homem-máquina", "Agentes Autônomos Computacionais", "Indústria 4")
sub_topico_4_3_Controle_e_Sensores <- c("Sensores","Arduino", "Sensores de gases","Redes de sensores sem fios e consumo de energia","Rede de sensores","sensor de estrelas","Sensoriamento","Microcontroladores","Controle hidráulico","Redes de sensores","Controle : Trajetória","Agente Racional SAID","Circuitos digitais","Circuitos integrados analógicos","Circuitos VLSI","Circuitos Digitais","Circuitos FPGA","Calibração de sensores","Autoajuste de sensores","Algoritmo Twins","Agente Racional","FPGA","Sistemas embarcados","Semáforo - sistemas inteligentes de controle","Sistemas difusos", "Gas sensors","Detectores", "Sensores magnéticos","Fibras ópticas", "Magnetic sensors", "Sensores capacitivos", "Capacitive sensors", "Inertial sensors", "Sensores inerciais", "Load sensors", "Temperature sensors", "Sensor de estrelas", "Soil moisture sensor", "Soft-sensor", "Soft sensors", "Softsensors", "Software sensors", "Wrist-worn sensors", "Nós sensores", "Biosensores", "Biossensores", "Smart sensor", "Sensores vestíveis", "Sensor multidimensional", "Sensoriamento proximal", "Sensoriamento espectral", "Monitoramento", "Monitoramento de grãos", "Monitoramento não-intrusivo de cargas", "Sensores sem fio", "Wireless sensor", "Wireless sensor networks", "Redes de sensores sem fio", "fusão de sensores", "Fusão de sensores", "Processamento de Sinais", "Processamento do sinais", "Processamento de alarmes", "compressed sensing", "Aquisição multisinais", "Controle", "Control", "Controle Inteligente", "Intelligent Control", "Controle Preditivo", "Controle preditivo", "Controle automático", "Controle eletronico", "Controle Linear", "Controle não linear", "Controle robusto", "Controle robusto ótimo", "Controle proporcional-derivativo", "Controlador PI", "Controladores PID", "PID Controller", "Controladores nebulosos", "Controle de acoplamento mútuo", "Controle de processo", "Controle de produção", "Control of production", "Controle de movimento", "Controle concomitante", "Cybercontrole", "Neural Controllers", "Controle de semáforos", "Controle de trânsito", "Controle do tráfego aéreo", "Controle com realimentação", "Controle de helicópteros", "Controle ótimo", "Controladores eletrônicos", "Controle por modos deslizantes", "Controle centralizado Volt-VAr", "Centralized control Volt-VAr", "Comando e controle", "Sistemas de controle", "Controle de infestação", "Detecção de estresse", "Sistemas eletronicos de alarme", "Sistemas de monitoramento", "Sincronização de semáforos", "Controle externo", "Inteligência sensório-motora", "Transtorno do processamento sensorial", "Sensores fisiológicos", "Sinais fisiológicos", "Remote sensoring", "Sensoriamenro remoto", "Sensores remotos", "Controle Inteligente", "Sistemas de controle inteligente", "Intelligent control systems", "Intelligent Control System", "Intelligent Control Strategies", "Sistema de Controle Inteligente (SCI)", "Intelligent Task Scheduling", "Escalonamento Inteligente de Tarefas", "Intelligent Control", "Processamento de sinais")
sub_topico_4_4_Automacao <- c("Cibernética", "Mecatrônica","Agricultural automation","Fluxo de trabalho - Automação","Veículos remotamente pilotados","Veículos autônomos","Automação predial","avaliação automática","Bots","comportamento autônomo","Automatic State Generation","Automatic Program Repair","Automated production","Automated Planning","Automação Industrial","Automação de projeto eletrônico","Automação agrícola", "Automação", "Sistemas autônomos","Automatização","Automação industrial","detecção de anomalias","administração de impressoras","Aparelho de mudança de via")

##Tópico 5: Sistemas Inteligentes
sub_topico_5_1_Sistemas_Inteligentes <- c("Sistemas inteligentes","Ambientes inteligentes","sistemas tutores inteligentes","Sistemas inteligentes de controle","Logística","Predi??o de falha","SISTEMAS AUTOM?TICOS DE IDENTIFICA??O","Detector de Defeitos","Sistemas de multiagentes","Detecção de Falhas","PREVISÃO DE GERAÇÃO DE BIOGÁS","Multi-agent system","Serviços ao cliente","Sistemas inteligentes de veículos rodoviários","sistema multiagente","Consumo de Energia Elétrica","Raciocínio automático","Defesa aeroespacial","Sistema Tutor Inteligente (STI)","Preditores (Desenvolvimento)","Modelagem de Agentes","Testes adaptativos informatizados","Sistemas Autônomos Explicáveis","Gerência de redes","Tutores inteligentes","Reconhecimento automático da voz","Sistemas complexos","Processos de fabricação","Sistemas Multi-Agente","Sistema híbrido inteligente","Sistemas Dinâmicos","Seleção dinâmica de classificador","Sistemas especialistas : Microeletronica","Sistemas de Telecomunicações","Sistemas de Energia Elétrica","Previsão de oportunidades","Medidores inteligentes","Sistema Dedutivo Rotulado","Monitoramento do desgaste de ferramenta","Recrutamento e seleção digital","Seleção de característica","Turbogeradores : previsão de falhas","coordenação hidrotérmica;","roteirização dinâmica de veículos","ambientes virtuais inteligentes","Ciência da computação - Autômatos finitos","Avaliação de Desempenho","Avaliação de decisões estratégicas","avaliação automática","Ambiente Inteligente","Alinhamento estratégico","Alocação dinâmica de tarefas", "Intelligent transportation systems","agentes virtuais inteligentes","Agentes Semióticos","Administração da produção e sistemas de informação","Sistemas multiagente","sistemas especialistas","Sistemas de energia eletrica","Semáforo - sistemas inteligentes de controle","Autômato celular","detecção de anomalias","Aparelho de mudança de via","Análise de desempenho", "Sistemas de transporte inteligente (ITS)","Advanced driver-assistance systems","Adaptive systems","UAV","Agentes Inteligentes e Inteligência artificial","agentes inteligentes","Agentes : Software","agentes","Inteligência artificial distribuida","Inteligência artificial distribuída","Raciocínio","Eficiência energética","Revisão sistemática", "Sistemas inteligentes de transporte", "Sistema de transporte inteligente", "Sistema inteligente de manutenção", "Sistema especialista", "Sistemas especialistas", "Sistemas especialistas - Medicina", "Sistemas especialistas reativos", "Sistemas Especialistas Simbolistas", "Sistemas especialistas (SMA)", "Núcleo de Sistemas Especialistas (NSE)", "Sistemas especialistas - Shell", "Sistemas especialistas de diagnóstico", "Sistemas de apoio a decisões clínicas", "Inteligência artificial – Sistemas especialistas", "Representação do conhecimento (Sistemas especialistas)", "Sistemas especialistas : Microeletrônica", "Sistemas supervisórios e sistemas especialistas - interligação", "Sistema especialista (Ciência da computação)", "Sistemas especialistas - Teses", "Sistemas especialistas : Geologia", "Sistemas Especialistas", "Sistemas especialistas – Teses", "Aquisição de conhecimento (Sistemas especialistas)", "Conhecimento especialista", "Intelligent systems", "Sistemas Inteligentes", "Inteligência estratégica antecipativa", "Intelligent networks", "Redes inteligentes de energia", "Intelligent buildings", "Cidades inteligentes", "Campos Inteligentes", "Serviços Inteligentes", "Intelligent Services", "Intelligent Service Layer", "Intelligent agents", "Agentes inteligentes", "Intelligent agents (Software)", "Agente Inteligente", "Agente inteligente", "Agentes Inteligentes", "Agentes", "Agentes Tutores Inteligentes", "Intelligent Tutoring Agents", "Intelligent observers", "Intelligent tutoring systems", "Sistemas Tutores Inteligentes", "Sistemas tutores inteligentes", "Intelligent Tutoring Systems", "Sistema tutor inteligente", "Sistema tutor inteligente conexionista", "Sistemas tutoriais inteligentes", "Society of Intelligent Tutoring Agents", "Intelligent tutorial systems", "Sistema de Controle Inteligente", "Intelligent Tutoring Systems", "Inteligent agents", "Sistemas especialistas", "Specialist medical decision support system", "Sistemas Especialistas", "Sistemas especialistas - Medicina", "Sistemas especialistas baseados em personalidades", "Sistema especialista (Ciência da computação)", "Sistemas especialistas reativos", "Sistemas Especialistas Simbolistas", "Sistemas especialistas (SMA)", "Núcleo de Sistemas Especialistas (NSE)", "Sistemas especialistas - Shell", "Sistemas especialistas de diagnóstico", "Sistemas de apoio a decisões clínicas", "Inteligência artificial – Sistemas especialistas", "Representação do conhecimento (Sistemas especialistas)", "Sistemas especialistas : Microeletrônica", "Sistemas supervisórios e sistemas especialistas - interligação", "Sistema especialista", "Sistemas especialistas - Teses", "Sistemas especialistas : Geologia", "Sistemas Especialistas", "Sistemas especialistas – Teses", "Aquisição de conhecimento (Sistemas especialistas)", "Conhecimento especialista", "Sistema de Recomendação", "Sistemas de recomendação", "Sistema de recomendação", "Sistemas de Decisões Automatizadas", "Sistemas de recomendação", "Recomendação", "Intenção de recomendar", "Avaliação do comportamento", "Decisão", "Processos decisórios por critério múltiplo", "Inferência", "Inferência bayesiana", "Bayesian inference", "Sistema de inferência", "Statistical inference", "Inferência reversa", "Inferência estatística", "Causal Inference", "Modelos de decisão", "Processo decisório - Algoritmos", "Processo decisório", "Decisão", "Decisões", "Processo decisório (Ética)", "Multicriteria decision-making/aiding/analysis", "Processos decisórios por critério múltiplo", "Sistemas flexíveis de manufatura", "Optimization")
sub_topico_5_2_Planejamento <- c("Planejamento de caminho", "Planejamento de caminhos","Sistemas de planejamento","Planejamento da produção","Planejamento estratégico de defesa","Planejamento probabilístico","Políticas públicas para as mulheres","Investimentos públicos","Probabilidades - Planejamento","planejamento sob incerteza","PLANEJAMENTO DA OPERAÇÃO","Políticas públicas - avaliação","Projeto de produto","Planejamento de curto prazo","Construção civil : Planejamento","Planejamento Probabilístico","Governo eletrônico","Gastos públicos","Manutenção preditiva","Automated Planning","Programação da produção","planejamento baseado em casos","Planejamento Automático","Docagem","Escalonamento","Cenários futuros","Complexidade do espaço aéreo","Ciência da computação (Transportes","Ciência da computação : Transportes","Planejamento instrucional","planejamento hierárquico","Capacidade do espaço aéreo", "Planejamento automático","Políticas públicas","planejamento em Inteligência artificial","Produtividade", "Planejamento Automatizado","Energia elétrica - Distribuição", "Replanejamento automático", "Planejamento automatizado", "Planejamento sob incerteza", "Planejamento baseado em casos", "Planejamento de experimentos", "Planejamento experimental", "Planejamento multiagente", "Planejamento em IA", "Planejamento não-determinístico", "Replanejamento", "Planejamento estratégico", "Planejamento de rotas", "Lavra: Planejamento", "Planejamento empresarial", "Planejamento de operação eletroenergética", "Planejamento florestal", "Planejamento regional", "Planejamento e Gestão Urbana", "Planejamento de aeroportos", "Planejamento da operação", "Planejamento de propaganda", "Planejamento racional", "Planejamento fatorial", "Planejamento Urbano", "Planejamento de experimento computacional", "Planejamento de fármacos", "Estratégias de Controle Inteligente", "Intelligent Control Strategies", "Planejamento urbano", "Planejamento", "Planejamento temporal", "Gestão de projetos", "Manutenção", "Previsão")
sub_topico_5_3_Sistemas_de_Recomendacao <- c("Análise de comportamento","Administração de risco","Diagnóstico de acidentes","Sistema de apoio à decisão","sistemas de suporte à decisão","Riscos operacionais","modelo preditivo","Métodos de decisão multicritérios","gerenciamento de Projetos","Processo de ?Decis?o de Markov","Fundamentação das decisões","Sistemas de suporte de decisao","Decisão estatística","Apoio à decisão","SAVAD","Sistemas de apoio à decisão clínica","Analytic hierarchy process","Detecção de danos","Servitização","Sistemas de Recomendação","Teoria bayesiana de decisão estatística","Risco de crédito","Delphi method","Análise de risco","Confiabilidade", "Comportamento do consumidor","Avaliação","Gestão de riscos","Sistemas de apoio à decisão","AHP", "Comportamento animal","Predição","Árvore de decisão","Pesquisa operacional","Business processes", "Tomada de decisão","Comportamento coletivo", "Comportamento motor", "Comportamento espectral", "Comportamento oculomotor", "Comportamento de movimento", "Comportamento social", "Comportamento humano", "Comportamento político", "Comportamento dinâmico", "Comportamento caótico", "Comportamento estereotipado", "Comportamento autônomo", "Comportamento informacional", "Padrões de comportamento", "Perfis comportamentais", "Monitoramento de comportamentos", "Análise inteligente de dados", "Comportamento do consumidor", "Observação de comportamento", "Abelhas - Comportamento", "Formiga - comportamento – Algoritmos", "software de auxílio-diagnóstico")

##Tópico 6: Algoritmos inspirados na Natureza
sub_topico_6_1_Algoritmos_geneticos <- c("Algorítmos genéticos","algoritmos evolutivos","Genetic Algorithms - GA","Genetic Algorithm","Ciência da computação evolucionária","Aprendizado genético","Algorítmos Genéticos","Algoritmo genético e lgoritmo firefly","Algoritmos Evolucionários com Inspiração Quântica","Algoritmos Evolucionários de Inspiração Quântica", "Algoritimos genéricos","Algorítmo genético","Algoritmo evolucionário","algoritimos evolutivos","Algoritmo Genético", "Algoritmos geneticos", "algoritmos genteticos","Programação genética", "Algorítimos genéticos", "algoritmo genético", "Neuroevolução", "Algoritmos bio-inspirados", "Algoritmos bioinspirado", "Algoritmo bioinspirado", "Evolução", "Algoritmo genético", "Coevolução", "Ciência da computação evolutiva")
sub_topico_6_2_Bioinformatica <- c("Bioinformatics", "Bio-informatics", "Biologia computacional","Ciência da computação Bio-Inspirada","Cancer bioinformatics", "Mapeamento cromossômico","bioinformática","Alphafold","Bio-Inspired Computing","Aminoácidos", "Ontologia genética","QSAR", "Bioestatística", "Biologia computacional", "Bioinformática")

##Tópico7: Lógica fuzzy
topico_7_Logica_Fuzzy <- c("Fuzzy", "lógica fuzzy", "Lógica difusa","Agrupamento Fuzzy","Logica difusa","Lógica Difusa","Sistema Fuzzy","Fuzzy logics","Fuzzy AHP","Sistema Neuro-Fuzzy","Conjunto fuzzy","Modelos Nebulosos Evolutivos","L?gica Fuzzy","Teoria de conjuntos fuzzy","Conjuntos Fuzzy","rede neuro fuzzy","Ciência da computação nebulosa","Clusterização difusa","Agrupamento Fuzzy de Dados", "lógica Fuzzy", "Lógica fuzzy", "lógica nebulosa", "teoria nebulosa","Incerteza","Lógica","Sistemas difusos","Agrupamento fuzzy", "Algoritmos fuzzy", "Classificador fuzzy auto-organizável", "Controle fuzzi", "Incertezas", "Lógica Fuzzy", "Fuzzy (Inteligência artificial)")

##Tópico 8: Dados e informação
sub_topico_8_1_Informacao <- c("Recuperação da informação", "Informação privilegiada","Recuperação de Informação","Ciência da Informação","Mineração de opiniões","Suportes para informação","Processamento digital de sinais","Infodemia","Gestão da informação e do conhecimento","Representação do conhecimento (Teoria da informação)","Logica simbolica e matematica",": Sociedade Informacional","Metaheurísticas","Informações relacionais","Arquitetura da informação","Imagens como recursos de informação","Análise funcional","CADEIAS DE MARKOV - COMPUTA??O","Bayesian Network","Cadeia de Markov","Administração da produção e sistemas de informação","Recuperação de informação", "extração de informação","Modelagem computacional","Teoria bayesiana de decisão estatística","Heurística","Autômato celular","PROGRAMA DE PÓS-GRADUAÇÃO EM ENGENHARIA DA INFORMAÇÃO - UFABC","Dimensionality","Capacidade do espaço aéreo","Bayesian networks","Ciência da informação","Algoritmos de computador","Avaliação","Reconhecimento de padrão","Monitoração","Redes Bayesianas","Seleção de atributos","Google Earth Engine","séries temporais","Redes : Computadores", "Extração de informação","Processos de Markov","Bibliometria","recuperação de informação", "Filtragem de informação","Pesquisa operacional","Revisão sistemática", "Information retrieval","Lógica simbólica e matemática", "Information Retrieval","Redes complexas", "Music information retrieval", "Information Extrac-tion", "Multimodal information retrieval", "ontology-based information extraction", "Extraction d'information", "Information relaxation", "Information visualization", "Information visualization", "Information Visualization", "Information ranking", "Teoria da informação", "Information theory", "Information Theory", "Geometria da informação", "Information geometry", "entropia de informação", "entropia de informação", "Competência em informação", "Competência em informação", "Organização e Representação da Informação", "Representação da Informação", "Information science", "information science", "Gerenciamento da informação", "Information management", "Information technology", "Building information modeling", "Information literacy", "Mutual information estimation", "Mutual information regression", "Redes de computadores", "Sistemas de recuperação da informação", "Redes de Petri", "Tecnologia da informação", "Recuperacao : Informacao")
sub_topico_8_2_Dados <- c("Análise inteligente de dados","Analise de dados simbolicos","visualização de dados","mineração de dados","projeto de big data","DADOS","Rotulagem de Dados","Minera??o de Dados","Privacidade de dados","Análise Multiescala","Imputação de dados","Data dependency graphs","Data access delay","FLUXO DE DADOS","modelagem de dados","mining data streams","Integraçãode Dados","Economia - ciência de dados","Metadados","Soil data quality","Data-based techniques","Descrição de dados usando vetores de suporte","Fluxos contínuo de dados","Satellite image time series","Descoberta de conhecimento em banco de dados","Metodologias Baseadas em Mineração de dados","Linguistica - Processamento de dados","Inferencia (Logica)","Mineração de opinião","Processamento eletronico de dados na pesquisa","Procedural content generation","Proveniência de Dados","Processamento de sinais adaptativos","Representação temporal","Inferencia","Processamento eletrônico de dados","Programação funcional (Ciência da computação)","Modelo estatístico","Inteligência artificial - mineração de dados - detecção de anomalias","Tratamento de dados","Armazenamento de dados","Bases de dados","Comutação por pacotes (Transmissão de dados)","Compressão de dados (Ciência da computação)","Compressão de dados (Telecomunicações)","COLONIALISMO DE DADOS","Bugs’ Dataset Dissection","Cancer mutation data","Bancos de dados","Benchmark de bases de dados para detecção de outliers","Banco de dados orientado a objetos","BCI","Banco : Dados geograficos","bagging","ANALYSIS OF BOOLEAN FUNCTIONS","Angtrom-Prescott","Angstrom-Prescott","Análise Numérica","Analise multivariada","Análise Estatística","Análise estática de código","Ánalise de dados simbolicos","Análise espacial (Estatística)","agrupamento de dados","An?lise multivariada","análise de componentes principais (ACP)","Agrupamento Fuzzy de Dados","Algoritmos de bagging","Algoritmo Twins", "Database","Accounting - Data processing","Banco de Dados","Dataset shift decomposition","[Lei geral de proteção de dados pessoais (2018)]","Processamento de dados","Time series","Visual analytics","Mineração de textos","Data streams","Dataset shift","Kriging","data brokers","Criptografia de dados (Ciência da computação)","Mineração de dados educacionais","Conjuntos aproximados","Correlation rules","Análise multifractal","Algoritmos& por enxame de partículas","Análise de dados","Análise de Agrupamento","Alteridade digital","Teoria dos jogos","Agrupamento de dados","Algorítmos de computador","ALGORITMOS (PROGRAMA??O)", "Data base","Base de dados", "Data Stream", "Data Streams","Integração de dados","Proteção de dados","Proteção de dados pessoais","Inteligência artificial - Processamento de dados","Sentinel-2", "Dataset", "Data science","Fluxo de dados","Análise multivariada","Mineração de Dados","Análise de componentes principais", "Data Science", "Data Quality", "Data mart", "Data spatialization", "Data augmentation", "Data analytics", "Market Data", "Big data", "Big Data", "big data", "Dados", "Dado", "Dados desbalanceados", "Dados esparsos", "Dados ligados", "Dados abertos", "Dados pessoais", "dados rotulados", "Dados Pessoais", "Dados legados", "Mineração de dados", "data minning", "mining", "web mining", "Mining", "Opinion mining", "Séries temporais", "Banco : Dados", "Banco de dados", "Estatística", "Mineracao : Dados", "Previsão de séries temporais", "Análise de séries temporais", "Matemática", "Modelagem")
sub_topico_8_3_Cloud_Edge_Computing_IA <- c("Ciência da computação em nuvem","Ambiente virtual de Machine learning")
sub_topico_8_4_Inovacao_e_Design <- c("Criatividade","eficiência","Jogos computacionais","Tecnologia disruptiva no direito","Revolução 4","revolução 4","revolução 4.0","Design - Estudo e ensino","criatividade","Realidade Virtual","Design space exploration","Quarta Revolução Industrial","Internet das coisas - indústria 4.0","Arquitetura - inovação tecnologica","agricultura 4","Bitcoin","Blockchain", "Inovação", "Design","Design Science Research","Protótipo de software","5G","Desenho industrial","desenho","Desempenho", "Design estratégico","Ciência da computação de alto desempenho","Transformadores de potência", "Inovações tecnológicas","Tecnologia","Digital transformation","Acessibilidade","Tecnologias digitais","Ciência da computação quântica","indústria 4","Realidade virtual")

##Tópico 9: IA Aplicada
sub_topico_9_1_IA_Saude <- c("Imagens histológicas","Biomarcadores tumorais","Oftalmologia","Pneumonia","Monitoração de saúde","Dispositivos médicos","Stress (Fisiologia)","Sindromes Oculares","Tuberculose","Imagem por ressonância magnética","DIAGN?STICO POR IMAGEM","Marcadores genéticos","Leishmaniose","Escoliose","Sistema nervoso autônomo","Transplante de fígado","Doen?a de Chagas","prematuridade","Doencas infecciosas dos animais : Aves","transtorno do espectro autista","Leishmania amazonensis","Psicologia feminista","Transtornos mentais","Glucose-insulin regulatory system","Proteomics","Informática em saúde","Neuroimagem","Depressão","Joelho","genética","Sistemas biológicos","Teste Cardiopulmonar ao exercício","Patologia","Variabilidade de Frequência Cardíaca, Sistema Nervoso Autônomo, Inteligência artificial, Hipertensão","Índice de Placa Dentária","Epidemiologia - Brasil","Mapas cognitivos","Ressonância magnética","Teste Cardiopulmonar ao exercício.","Machine learning aplicado à saúde","Mediação e conciliação","Infecção hospitalar","Reprodução humana","Saude mental","Saúde mental","HIV","Engenharia biomédica - mamas - câncer","Cicatrização de feridas","monitoramento de pacientes","Educação física","Computadores - Doenças - Diagnóstico","Cirurgia refrativa","Coluna lombar - Músculos","Citogenética","Cognitive modeling","clorofilas","CNPQ::CIENCIAS DA SAUDE::ODONTOLOGIA","Citometria de fluxo","Atividade cerebral","Ciências da Saúde","Cérebro","Cellular automata","Ceratocone","Carga cognitiva","Calor efeito fisiológico","Câncer de lábio","Cancer genomics","Brain injury","BRAIN COMPUTER INTERFACE","Biotelemetria","Biorremediação","Biodeterioração","Biometrics","Bio-sinais","biologia molecular","Bacterias diazotroficas","APH","Ativação celular","Assisted reproduction","Assistência farmacêutica","Anotação de genes","Artrite reumatóide","Anatomia Patológica e Patologia Clínica","Amputação membro superior","Alphafold","Alzheimer pré-clínico","Alzheimer disease","Agentes cognitivos","Adesão à medicação","ND-O-LID","LID-1","q-PCR","Acidente Vascular Cerebral - AVC","Acetilcolinesterase","E-Health","Esclerose múltipla","Sistemas de energia elétrica","Toxicidade","Sistema imunológico","Segurança alimentar","Saúde pública","Malária","Identificação biométrica","Hepatite C","Genética","Eletrocardiógrafo interpretativo","Espermatozoide","Engenharia Biomédica","Epilepsia","Densidade da mama","Ciência cognitiva","Cognition","Imagens de ressonância magnética","CNPQ::CIENCIAS DA SAUDE::EDUCACAO FISICA","Ciencia cognitiva","Atividade Física","Câncer de mama","Biomarcador","Artrite Reumatóide","ALS","Inteligência artificial - Aplicações médicas","Aminoácidos","Transtorno bipolar","Acessibilidade","Dengue","Idoso","Diagnóstico por imagem","Glaucoma","Nematodes", "Magnetic resonance imaging","RNA","Contagion","CNPQ::CIENCIAS BIOLOGICAS::FARMACOLOGIA","Pedometria","Prognóstico","Ciências cognitivas","Cognição", "Imageamento por ressonância magnética","Biomarcadores","Biometria", "Forensic image analysis", "Sistemas especialistas - Medicina", "Sistemas especialistas de diagnóstico", "Sistemas de apoio a decisões clínicas", "Doencas", "Sistemas de saúde", "Telessaúde", "Desigualdades em saúde", "COVID19", "COVID-19", "COVID-19", "COVID19", "COVID 19", "COVID- 19", "Sinais biomédicos", "Sistema de apoio ao diagnóstico", "Diagnóstico automático", "Tecnologia Biomédica", "Biotecnologia", "Image diagnostics", "diagnosis", "Anestesia odontológica", "Odontologia", "Sistemas biologicos", "Cardiopatia congênita", "Prognostico", "Diagnóstico radioscópico", "Cistos Odontogênicos", "Citocinas", "Nutrition of monogastric", "Metapsicologia", "Fadiga cognitiva", "Sinais fisiológicos", "Medicina", "Ressonancia magnética", "Medicina translacional", "Ressonância Magnética Funcional", "Dinâmica pupilar", "Sarcoma osteogenico", "Diagnóstico de Candidemia", "Incapacidade intelectual", "Dermatite ocupacional", "defici?ncia intelectual", "Trauma cranioencefálico", "Suscetibilidade genética", "Pacientes", "Lesão periapical", "Politica farmaceutica", "Epidemiologia veterinaria", "Clinical medicine - Technological innovations", "Tecnologia Biomédica", "Biotecnologia", "Biotecnologia - Indústria", "Biotechnology industries", "Tecnologia biomédica", "Imageamento Médico", "Imagens fotográficas digitais intrabucais oclusais", "Tomografia computadorizada", "Immunobiological demand", "Informática na medicina", "Medicina computacional", "Clinical prediction explicability", "Ontologia biomédica", "Biologia computacional", "Bioacoustics", "Mapeamento cromossômico", "Microbiota intestinal", "Biomecânica", "Engenharia biomedica", "Biomedical ontology", "Fenômenos biomecânicos", "Biomecânica corneal", "Corneal biomechanics", "Cornea-Biomechanics", "Neurosciência computacional", "Dinâmica pupilar", "Conhecimento para diagnóstico", "Estimulação cognitiva", "software de auxílio-diagnóstico", "Monitoramento epidemiológico", "Psicologia cognitiva", "Psicologia educacional", "Psicopatologia", "Psicologia social", "Psicologia positiva", "Psicoterapia psicodinâmica", "Terapia psicanalitica", "Terapia cognitivo-comportamental", "Psicoterapia psicodinâmica", "Terapia cognitivo-comportamental", "Diagnóstico", "Diagnóstico clínico", "diagnóstico", "Diagnostico", "Diagnóstico de isolamentos", "Insulation diagnostics", "Diagnóstico Médico", "Medical Diagnostic", "Diagnósticos", "Diagnóstico cognitivo", "Diagnóstico ortodôntico", "diagnostic", "Foliar diagnosis", "diagnóstico salivar", "Esquizofrenia - diagnóstico", "Breast cancer", "breast cancer", "Breast Cancer", "Cancer", "Câncer", "Oral cancer", "Diagnóstico de Candidemia", "Candidemia", "Clinical prediction explicability", "Clinical medicine - Technological innovations", "Candida lipolytica", "Diagnóstico diferencial", "Reposicionamento de fármacos", "Septicemia", "Diabetic retinopathy", "Retinopatia diabética", "Suscetibilidade genética", "Variabilidade genética", "Suscetibilidade genética", "Informática médica", "Eletroencefalografia", "Eletroencefalografia","Enfermagem")
sub_topico_9_2_IA_economia_financas <- c("Financial forecasting","atividade econômica","Regulação financeira","Previs?o de Vendas","Economia política","Mercado ações brasileiro","Setor Financeiro","Desembolso financeiro","Ruptura no ponto de venda","Fatores de risco em ações","Mercado de valores","Avaliação de Empresas","Detecção de fraudes","Riscos operacionais","Economia - ciência de dados","Jogos de empresas (coautoria gerencial)","Previsão de demanda","Custo operacional","Projeto de produto","Predição de risco","Desenvolvimento econômico","Configuração empresarial","CIENCIAS CONTABEIS","preço arroba de boi, predição de valor, Inteligência artificial","Mercado brasileiro","relações de consumo","Modelos de crescimento e produção","Ciência da computação : Economia","Competitividade - Bancos","capital comercial","classificação Machine learning prevenção à fraude detecção carão de crádito","capital portador de juros","Capitalismo e comunicação de massa","agente econômico","Aspectos tributários","Análise de Séries Temporais","Análise de investimentos","Análise de crédito","Advogado-robô","Agrupamento de séries temporais","Administração financeira","Administração de empresas","Administração pública eletrônica","Advocacia", "Financial frauds","educação financeira","Risco de crédito","Mercado financeiro","Trading systems","Time series","Previsão de preços","Neoliberalismo","Crise econômica","Bancos - finanças - Brasil","Análise de risco","Business Intelligence","Economic system","Administração de risco","AHP", "Inadimplência (Finanças)","ADMINISTRAÇAO", "Inadimplência (Finanças) - Previsão - Inteligência artificial","séries temporais","Produtividade","Gestão de riscos","Processos de Markov", "Financial market","Business processes", "Macroeconomia", "Econometria", "Fatores socioeconômicos", "Socioeconomic factors", "Economia", "Risco (Economia)", "Incerteza macroeconômica", "Previsão macroeconômica", "Macroeconomic uncertainty", "Macroeconomic forecasting", "Crescimento e Desenvolvimento Econômico", "Economia do mercado", "Indicadores socioeconômicos", "Capital econômico", "Microeconomia", "Modelos macroeconômicos", "Contabilidade", "Contabilidade – Brasil", "Instituições Financeiras – Brasil", "Insolvência bancária", "Falência", "Notícias macroeconômicas", "Dados socioeconômicos", "Administração", "CNPQ::CIENCIAS SOCIAIS APLICADAS::ADMINISTRACAO")
sub_topico_9_3_IA_Ciencias_da_natureza_e_da_terra <- c("Agricultura","Impacto ambiental","Mapeamento geológico","descargas elétricas atmosféricas","Ferrugem da soja (Doença)","Floresta amazônica","Ecossistema aquático","Efluentes industriais","Soja","Reconhecimento do solo","Pureza da semente","Variáveis ambientais","Doencas infecciosas dos animais : Aves","Usina Termoelétrica","Mapeamento digital do solo","Soil data quality","Energia elétrica - Falhas","Consumo de Energia","PREVISÃO DE GERAÇÃO DE BIOGÁS","Solos - Conservação","Energia elétrica - Consumo","Café verde","Tropical biodiversity","Compensação ambiental","Precipitação pluviométrica","sistemas elétricos de potência","Informática na agricultura","Acephala DC","Horticultura","Sistemas hidrotérmicos","Quimiometria","Lentilhas - Sementes - Fisiologia - Qualidade","Dendezeiro - Cultivo - Amazônia","Produção animal","Transmissão de Energia Elétrica","Filmes ferromagnéticos","Wastewater treatment","Supercondutividade","rota química","Leite","Consumo de Energia Elétrica","Camuflagem (Biologia)","Pecuária de precisão","Florestas - Manejo","DINÂMICA MOLECULAR","Predição da qualidade de sementes de soja armazenadas em diferentes ambientes e embalagens usando Machine learning","Gases de efeito estufa","Previsão hidrológica","Vegetação e clima","Gás Natural","Geometalurgia","Leite analise","Inteligência artificial : Petrografia","Energia – consumo","mapeamento digital de solos","Flores","Seguro rural","Melhoramento genético - Simulação por computador","Sistemas de Energia Elétrica","Oceanografia","Petróleo","Seguro agrícola","Meteorologia florestal","Análise foliar","coordenação hidrotérmica","Umidade do solo","Melhoramento de plantas","Composição de serviços geográficos","Biologia - Estudo e ensino","Geofísica","Paleoparasitologia","Plantas espontâneas","Ecologia das paisagens","Arquitetura e clima","Hybrid energy system","CNPQ::CIENCIAS AGRARIAS","CNPQ::CIENCIAS AGRARIAS::ENGENHARIA AGRICOLA","Colônia de formigas","CIENCIAS AGRARIAS::ENGENHARIA AGRICOLA","clow humor","Ciência do Solo","CERRADO","Cerrado Brasileiro","Ciclicidade","Cevada","chlorophylls","Carbon dynamics","Cana-de-açúcar","Cereais","Cana-de-açúcar - Irrigação","Cana-de-açúcar - Melhoramento genético","Café - Doenças e pragas","Brazilian Semiarid region","Bugs’ Dataset Dissection","Bug Localization","Brassica oleracea - Doenças e pragas - Controle integrado","Bacias hidrográficas - Paraná","Boi gordo","Beneficiamento de sementes","Biologia aquática","Batata - Doenças e pragas - Diagnóstico","bacias hidrográficas brasileiras","Bacia Sedimentar Brasileira","Bacia sedimentar","Atributos químicos do solo","Automação agrícola","Assessment Tool for Environmental Sustainability","Arquitetura - aspectos ambientais","aquecimento global","Aroma floral","Áreas protegidas do Brasil","Archaea","Arbovirose","Araucaria angustifolia","Arapaima Gigas","anomalias de precipita??o","Alterações Climáticas","Analise do solo","Águas residuais - Purificação - Remoção de cor","Algodão - Cultivo","Água - consumo","Água - uso","Águas residuais","Água - Poluição","AGRONOMIA::AGROMETEOROLOGIA","Agricultural soils","Agroindústria","Agromatem?tica","Agroclimatology","Agroclimatologia","Agricultura sustentável","Agricultural robot","Agricultural machinery","Agricultural automation","agricultura de precis?o","agricultura 4", "Produtividade agrícola","sustentabilidade","Peixes","Biomassa","WaTEM/SEDEM","Eucalipto","Biomassa florestal","Química do solo","Energia - armazenamento","Bovino","Annonaceae","Mudanças climáticas","Anaxagorea dolichocarpa","Bovinos de corte","Energia eólica","Agronomia","Usinas hidrelétricas","Península","Energia solar","Electric motors","Energia elétrica","Geociências","Geomorfologia","Ambiente : Machine learning","Amazônia", "Agriculture","Abastecimento de água", "Evapotranspiração - Estimativas","Sustentabilidade", "Irrigação", "Hidrologia", "Recursos hídricos", "Qualidade da água", "Clorofila", "Natureza", "Florestas tropicais", "Florestas – Mortalidade", "Florestas – Crescimento", "Mata Atlântica", "Precision agriculture","Agricultura"," Produtividade agrícola"," Agriculture"," Evapotranspiração - Estimativas", "Irrigação","Hidrologia","Recursos hídricos","Qualidade da água","Clorofila","Natureza","Florestas tropicais","Florestas – Mortalidade","Florestas – Crescimento")
sub_topico_9_4_Entretenimento <- c("Redes sociais","música","Games","Internet","Redes sociais online","Redes sociais digitais","engajamento","Social media","Jogos sérios","Jogos educacionais","Desenvolvimento de jogos","Eventos hidrológicos críticos","Jogos","Ficção científica americana","Jogos eletronicos","Jogos : Estrategia","Phabrika; vendas; jogos de empresas; jogos de simulação; vendas financeira; serious games","Arte sonora","Jogos por computador - Aspectos sociais","Gorgulho do algodão - Simulação","Banda larga","Mídias sociais","Música eletroacústica","Jogos interativos","Digital games","Cinema - Aspectos sociais","cinema de ficção científica","Ciência da computação e Música","Arte e tecnologia","Arte computacional","Arte - criação","Análise de Redes Sociais","Aplicação multimídia","Análise de mídias sociais","jogos digitais","Jogos eletrônicos","Plataformas digitais","Música","arte e tecnologia","Mídia e interação","Jogos digitais","Instagram","Facebook","Jogos computacionais ","Digital media","arte digital","Artes","Ciência da computação musical","Ficção científica","Jogos Digitais","Mídia","Multímidia interativa","Realidade virtual","Teoria dos jogos")
sub_topico_9_5_Ia_Ciencias_sociais_filosofia <- c("Ontologia","CIENCIAS SOCIAIS APLICADAS","Teoria dos Fundamentos Morais","Seguro Social","Transportes - Aspectos sociais","Sistema de Justi?a Criminal","gestão jurídica","sistema de justiça","Educação em Direitos Humanos","Jornalismo Automatizado","mitos","Sociologia","Consciência","efetivação de direitos sociais","Direito Digital","Sociedade : Sistema social","Identidades","segurança pública","Ensino superior","Justi?a Federal","Pena (Direito)","Justiça","Educação matemática","educação","Recursos repetitivos","Direito do consumidor","Publicidade governamental","Prevenção de danos aos jurisdicionados","Brasil. Superior Tribunal de Justiça (STJ)","Psicologia aplicada","Educação - Tecnologia","Epistemologia genética","Sistemas : Revisao : Crencas","Hermeneutica","Tipologia (Psicologia)","Governo eletrônico","Inteligência artificial - aplicações educacionais","Patente","Gestão educacional","defesa do consumidor","Responsabilidade penal","Direito cibernético","Gastos públicos","Judiciário","Jurisprudência","Tecnologia disruptiva no direito","sistema de justiça.","Decisão judicial","Responsabilidade (Direito)","Ontologias (Inteligência artificial)","Fundamentação jurídica","confiabilidade","Computer Science in Education","Computers and Education","COLONIALISMO DE DADOS","Ciência Política","CIENCIAS SOCIAIS APLICADAS::DIREITO","Ciência social computacional","Ativismo social","Aspectos morais e eticos","Boa-fé","AUTORITARISMO","Capitalismo e comunicação de massa","AUTHORITARIANISM","Antropoformismo","Antropologia forense","Agentes Racionais","Alinhamento de Ontologias","Ameaça de estereótipo","ALGORITHMIC SOCIAL MODULATION","Acesso à Justiça","Agentes Morais Artificiais","acesso à Justiça","acesso à justiça","Informática jurídica","Senso comum","Tecnologia - Aspectos sociais","semiose","Pol?ticas P?blicas","Mineração de dados educacionais","Geopolítica","Heurística","educação financeira","Ensino a distância","Diversidade","Discurso de\nÓdio Punitivista","DIREITO","Direitos humanos","Direitos Fundamentais","DIREITO PROCESSUAL CIVIL","Direito à explicação","Antropoceno","Detecção de Discurso de Ódio","Construtivismo","Ontologias","CNPQ::CIENCIAS SOCIAIS APLICADAS::DIREITO","Ética em Inteligência artificial","Celeridade (Direito)","Políticas públicas","Responsabilidade civil","Lógica - Estudo e ensino","Filosofia","Educação a distância","CIENCIAS SOCIAIS APLICADAS::COMUNICACAO","Ciência politica","Semiótica","Direito e tecnologia","Informática na educação","Sociologia do desenvolvimento técnico","cinema político","Comportamento social","Neoconstitucionalismo","Ética em Inteligência artificial"," Inteligência artificial - Aplicações no direito", "Criminalidade"," Tecnologia - Filosofia","CNPQ::CIENCIAS HUMANAS::FILOSOFIA","Ética","Pensamento crítico","Educação","legalidade","atos judiciais"," Organização do conhecimento","Profissão jurídica","Patentes","Responsabilidade jurídica","Personalidade","Contratos","Cultura","Identitarismo","Política","Teoria Crítica","Pensamento","Tecnologias Educacionais", "Direito Civil","Responsabilidade Civil","Ética digital","Educação básica","Currículo","Tecnologia educacional","Inteligência artificial - Aplicações educacionais","Ativismo judicial","Educação especial","Inteligência artificial - Estudo e ensino","Tecnologia educacional","Professores - Formação","Direito","Sistema jurídico"," Contratos.")

##Tópico 10: Aspectos Éticos e Sociais em IA
sub_topico_10_1_seguranca_IA <- c("Intrusion detection systems","computer security","Computer security","Redes de Ciência da computação - Medidas de segurança","Privacidade de dados","Detecção de ataques","Hardware security","Planejamento estratégico de defesa","Medidas de segurança","Redes - Segurança","Detecção de fraudes","Seguranca : Redes : Computadores","Sistemas de detecção de intrusão","Conduta cognitiva","Defesa aeroespacial","Redes de computadores - Medidas de segurança","Cibernética - Medidas de segurança","[Lei geral de proteção de dados pessoais (2018)]", "Network intrusion detection system","Discurso de\nÓdio Punitivista","Detecção de Discurso de Ódio","Criptografia","Criptografia de dados (Ciência da computação)","Computadores - Medidas de segurança","Proteção de dados","Proteção de dados pessoais", "Intrusion Detection Systems", "Intrusion detection system", "Informação privilegiada", "Prevenção ao vazamento de informação", "Desinformação", "Informação e tecnologias de comunicação", "Information security", "Violação de conhecimento", "Segurança cibernética", "Sistemas de detecção de intrusos", "Invasão", "Segurança", "Ataques")
sub_topico_10_2_analise_sentimentos_e_comportamentos <- c("Análise de sentimentos","Ética não humana","Humanização de robôs","Viese algorítmico","Machine learning comportamental","Textos enganosos","Intencionalidade","Consumidores - Atitudes", "Dialogue sentiment analysis","Emoções","Aspectos morais e eticos","comportamento do consumidor","comportamento do Consumidor","Análise do comportamento aplicada","Behavior simulation","Agentes Morais Artificiais","Agentes emocionais","Agentes Afetivos", "Análise de sentimento baseado em contexto", "Context-based sentiment analysis", "Sentiment Analysis", "Análise de sentimento", "Sentiment analysis", "Análise de Sentimento", "análise de sentimentos", "An?lise de Sentimento", "Análise de Sentimentos", "Sentimento textual", "Textual sentiment", "análise de sentimento", "Índice de sentimento", "Sentimento bancário", "Sentiment index", "Banking sentiment", "Aspect sentiment triplet extraction", "Sentimentos", "Facial expressions recognition", "Reconhecimento de gestos", "Processamento de imagens faciais", "Face verification", "Análise de comportamento", "Comportamento do consumidor", "Comportamento animal", "Comportamento coletivo", "Comportamento motor", "Comportamento espectral", "Comportamento oculomotor", "Comportamento de movimento", "Comportamento social", "Comportamento humano", "Comportamento político", "Comportamento dinâmico", "Comportamento caótico", "Comportamento estereotipado", "Comportamento autônomo", "Comportamento informacional", "Padrões de comportamento", "Perfis comportamentais", "Monitoramento de comportamentos", "Comportamento do consumidor", "Observação de comportamento", "Abelhas - Comportamento", "Formiga - comportamento - Algoritmos", "Comportamento informacional", "Terapia cognitivo-comportamental", "Terapia cognitivo-comportamental")
sub_topico_10_3_IA_explicavel <- c("Interpretability of convolutional neural networks", "Interpretabilidade de Redes neurais","Inteligência artificial Explicável (XAI)", "Clinical prediction explicability", "Informática : Educação", "Representacao : Conhecimento","Inteligência artificial explicável","Inteligência artificial Explicável","Transparência")





Topico1_Aprendizado_de_Máquina<- c(sub_topico_1_1_Algoritmos_e_software, sub_topico_1_2_Aprendizado_por_Reforco, sub_topico_1_3_Redes_Neurais_e_Deep_Learning, sub_topico_1_4_Clustering, sub_topico_1_5_Regressao_e_Classificacao)
Topico2_Processamento_linguagem_natural <-c(sub_topico_2_1_Processamento_de_linguagem_natural,sub_topico_2_2_Reconhecimento_de_Gestos)
Topico3_Visao_computacional<-c(sub_topico_3_1_Visao_Computacional)
Topico4_Robotica_Controle_Automacao<-c(sub_topico_4_1_Internet_das_Coisas,sub_topico_4_2_Robotica,sub_topico_4_3_Controle_e_Sensores,sub_topico_4_4_Automacao)
Topico5_Sistemas_Inteligente<-c(sub_topico_5_1_Sistemas_Inteligentes,sub_topico_5_2_Planejamento,sub_topico_5_3_Sistemas_de_Recomendacao)
Topico6_Algoritmos_Inspirados_na_Natureza <-c(sub_topico_6_1_Algoritmos_geneticos,sub_topico_6_2_Bioinformatica)
Topico7_Logica_Fuzzy<-topico_7_Logica_Fuzzy
Topico8_Dados_e_informacao<-c(sub_topico_8_1_Informacao,sub_topico_8_2_Dados,sub_topico_8_3_Cloud_Edge_Computing_IA,sub_topico_8_4_Inovacao_e_Design)
Topico9_IA_Aplicada<-c(sub_topico_9_1_IA_Saude,sub_topico_9_2_IA_economia_financas,sub_topico_9_3_IA_Ciencias_da_natureza_e_da_terra,sub_topico_9_4_Entretenimento,sub_topico_9_5_Ia_Ciencias_sociais_filosofia)
Topico10_Aspectos_Sociais_e_Éticos_da_IA<-c(sub_topico_10_1_seguranca_IA,sub_topico_10_2_analise_sentimentos_e_comportamentos,sub_topico_10_3_IA_explicavel)

topicos <- list(Topico1_Aprendizado_de_Máquina, Topico2_Processamento_linguagem_natural,Topico3_Visao_computacional, Topico4_Robotica_Controle_Automacao, Topico5_Sistemas_Inteligente,Topico6_Algoritmos_Inspirados_na_Natureza,Topico7_Logica_Fuzzy,Topico8_Dados_e_informacao,Topico9_IA_Aplicada,Topico10_Aspectos_Sociais_e_Éticos_da_IA)
topico_names <- c("Aprendizado_de_maquina","Processamento_de_Linguagem_Natural","Visao_Computacional","Robotica_Controle_e_Automacao","Sistemas_Inteligente", "Algoritmos_Inspirados_na_Natureza","Logica_Fuzzy","Dados_e_Informacao","IA_Aplicada","Aspectos_Sociais_e_Éticos_da_IA")

expand_topics <- function(df, col_name, topicos, topico_names) {
  
  # Separar as strings da coluna original em uma lista de listas
  split_info <- str_split(df[[col_name]], "\\s*\\|\\|\\s*|\\s*;\\s*|\\s*\\.\\s*|\\s*,\\s*")
  
  # Inicializar um dataframe vazio para os tópicos
  topic_df <- data.frame(matrix(0, nrow = nrow(df), ncol = length(topico_names)))
  colnames(topic_df) <- topico_names
  
  # Preencher o dataframe com 1 onde o tópico está presente
  for (i in seq_along(split_info)) {
    present_topics <- split_info[[i]]
    for (j in seq_along(topicos)) {
      if (any(present_topics %in% topicos[[j]])) {
        topic_df[i, topico_names[j]] <- 1
      }
    }
  }
  
  # Combinar o dataframe original com o dataframe de tópicos
  df <- cbind(df, topic_df)
  return(df)
}

# Usar a função para expandir os tópicos
df_expanded_2 <- expand_topics(arquivo, "Keywords", topicos, topico_names)

# Exibir o dataframe resultante
dim(df_expanded_2)
colnames(df_expanded_2)
soma<-c()
for (name in topico_names){
  soma_coluna<-sum(df_expanded_2[,name])
  soma<-c(soma,soma_coluna)
}
soma

### Linhas que não estão inseridas em nenhum tópico
linhas_sem_topicos_2<-df_expanded_2[df_expanded_2$Aprendizado_de_maquina==0 & df_expanded_2$Processamento_de_Linguagem_Natural==0 & df_expanded_2$Visao_Computacional==0 & df_expanded_2$Robotica_Controle_e_Automacao==0 & df_expanded_2$Sistemas_Inteligente==0 & df_expanded_2$Algoritmos_Inspirados_na_Natureza==0 & df_expanded_2$Logica_Fuzzy==0 & df_expanded_2$Dados_e_Informacao==0 & df_expanded_2$IA_Aplicada==0 & df_expanded_2$Aspectos_Sociais_e_Éticos_da_IA==0,]
linhas_sem_topicos_2$Keywords
dim(linhas_sem_topicos_2)

########################################## Analises#############################

###################################### Análise Número de trabalhos por genero##############################
## Plot homens vs Mulheres
plot_genero<-ggplot(df_expanded_2, aes(x = genero)) +
  geom_bar(fill = c("deeppink1","deepskyblue")) +
  labs(title = "Número de Homens vs. Mulheres",
       x = "Gênero",
       y = "Contagem")
print(plot_genero)
## Código para exportar o gráfico em formato .png
ggsave("grafico_homens_vs_mulheres.png", plot = plot_genero, width = 10, height = 6, path="/content/drive/MyDrive/Arquivo_leila")


###################################### Análise Trabalhos Femininos por Tópicos ############################

# Definição dos nomes dos tópicos que vão aparecer no gráfico
topico_names_abreviados <- c("Aprendizado de máquina","Processamento de linguagem natural", "Visão computacional","Robótica, Controle e Automação","Sistemas Inteligentes","Algoritmos inspirados na natureza", "Lógica Fuzzy","Dados e informação","IA aplicada","Aspectos éticos e sociais em IA")

### Agrupo os dados cuja autoras são mulheres
dados_femininos<-df_expanded_2 %>% group_by(genero)%>%filter(genero=="Female") %>% ungroup()

###Faço a soma das colunas de cada tópico para definir o número de trabalhos relacionados a cada tópico
soma_F<-c()

for (name in topico_names){
  soma_coluna_F<-sum(dados_femininos[,name])
  soma_F<-c(soma_F,soma_coluna_F)
}
soma_F
dados_topicos_femininos<-data.frame(topicos=topico_names_abreviados, soma_values_F = soma_F)

df_plot_F <- dados_topicos_femininos %>% arrange(soma_values_F)

df_plot_F$topicos <- factor(df_plot_F$topicos, levels = df_plot_F$topicos)

###Faço o plot do gráfico
plot_topicos_trabalhosf<-ggplot(df_plot_F, aes(x = topicos, y = soma_values_F, fill = topicos)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Inverte os eixos para barras horizontais
  labs(title = "Número de Trabalhos Femininos por Tópico",
       x = "Tópicos",
       y = "Número de Trabalhos") +
  theme_minimal() +
  theme(legend.position = "none",    # Remove a legenda desnecessária
        axis.text.x = element_text(size = 10),  # Ajusta o tamanho do texto dos rótulos do eixo x
        axis.text.y = element_text(size = 10))
print(plot_topicos_trabalhosf)
## Código para exportar o gráfico em formato .png
ggsave("grafico_trabalhos_femininos_vs_topicos.png", plot = plot_topicos_trabalhosf, width = 10, height = 6, path="/content/drive/MyDrive/Arquivo_leila")


###################################### Análise Trabalhos Masculinos por Tópicos  ############################
## Faço a separação dos dados masculinos também para depois fazer a comparação de gênero

### Agrupo os dados cujos autores são homens
dados_masculinos<-df_expanded_2 %>% group_by(genero)%>%filter(genero=="Male") %>% ungroup()

###Faço a soma das colunas de cada tópico para definir o número de trabalhos relacionados a cada tópico
soma_M<-c()

for (name in topico_names){
  soma_coluna_M<-sum(dados_masculinos[,name])
  soma_M<-c(soma_M,soma_coluna_M)
}
dados_topicos_masculinos<-data.frame(topicos=topico_names_abreviados, soma_values_M = soma_M)

df_plot_M <- dados_topicos_masculinos %>% arrange(soma_values_M)

df_plot_M$topicos <- factor(df_plot_M$topicos, levels = df_plot_M$topicos)


plot_topicos_trabalhosm<-ggplot(df_plot_M, aes(x = topicos, y = soma_values_M, fill = topicos)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Inverte os eixos para barras horizontais
  labs(title = "Número de Trabalhos Masculinos por Tópico",
       x = "Tópicos",
       y = "Número de Trabalhos") +
  theme_minimal() +
  theme(legend.position = "none",    # Remove a legenda desnecessária
        axis.text.x = element_text(size = 10),  # Ajusta o tamanho do texto dos rótulos do eixo x
        axis.text.y = element_text(size = 10))

print(plot_topicos_trabalhosf)
## Código para exportar o gráfico em formato .png
#ggsave("grafico_trabalhos_masculinos_vs_topicos.png", plot = plot_topicos_trabalhosm, width = 10, height = 6, path="/content/drive/MyDrive/Arquivo_leila")


###################################### Análise número de trabalhos por gênero por Tópicos  ############################

##Pego as informações já retiradas nos passos anteriores
colnames(df_plot_M) <- c("topicos", "soma_values")
colnames(df_plot_F) <- c("topicos", "soma_values")

# Adicionando uma coluna para identificar o gênero
df_plot_M$genero <- "Masculino"
df_plot_F$genero <- "Feminino"

df_combined <- rbind(df_plot_M, df_plot_F)

###Faço o plot do gráfico
plot_trabalhos_topicos_genero<-ggplot(df_combined, aes(x = topicos, y = soma_values, fill = genero)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Comparação do número de Trabalhos por Tópico",
       x = "Tópicos",
       y = "Número de Trabalhos") +
  theme_minimal() +
  theme(legend.position = "top",    # Coloca a legenda na parte superior
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)) +
  scale_fill_manual(values = c("Masculino" = "deepskyblue", "Feminino" = "deeppink1"))

print(plot_trabalhos_topicos_genero)
## Código para exportar o gráfico em formato .png
ggsave("grafico_trabalhos_genero_vs_topicos.png", plot = plot_trabalhos_topicos_genero, width = 10, height = 6, path="/content/drive/MyDrive/Arquivo_leila")



###################################### Análise evolução temporal de trabalhos femininos por tópicos  ############################

# Subset dos dados de autoras
dados_femininos <- subset(df_expanded_2, genero == "Female")

# Calcular a soma dos tópicos agrupados pelo ano de defesa
dados_femininos_por_ano <- dados_femininos %>%
  group_by(Ano.de.defesa) %>%
  summarise(
    total_ML = sum(Aprendizado_de_maquina),
    total_IA_aplicada = sum(IA_Aplicada),
    total_DI = sum(Dados_e_Informacao),
    total_SI = sum(Sistemas_Inteligente),
    total_RCA = sum(Robotica_Controle_e_Automacao),
    total_VC = sum(Visao_Computacional),
    total_PLN = sum(Processamento_de_Linguagem_Natural),
    total_ASeEIA = sum(Aspectos_Sociais_e_Éticos_da_IA),
    total_AGeB = sum(Algoritmos_Inspirados_na_Natureza),
    total_LF = sum(Logica_Fuzzy),
    .groups = "drop"
  )

dados_femininos_por_ano <- dados_femininos_por_ano %>%
  pivot_longer(
    cols = starts_with("total_"),
    names_to = "topico",
    values_to = "total"
  ) %>%
  filter(total > 0)

# Defino o nome dos tópicos, da forma e na ordem que quero que apareça no gráfico
topico_names <- c(
  total_LF = "Lógica Fuzzy",
  total_AGeB = "Algoritmos Inspirados na Natureza",
  total_ASeEIA = "Aspectos Sociais e Éticos da IA",
  total_PLN = "Processamento de Linguagem Natural",
  total_VC = "Visão Computacional",
  total_RCA = "Robótica, Controle e Automação",
  total_SI = "Sistemas Inteligentes",
  total_DI = "Dados e Informação",
  total_IA_aplicada = "IA Aplicada",
  total_ML = "Aprendizado de Máquina"
)

dados_femininos_por_ano <- dados_femininos_por_ano %>%
  mutate(
    topico = recode(topico, !!!topico_names),
    topico = factor(topico, levels = topico_names)
  )

dados_femininos_por_ano <- dados_femininos_por_ano %>%
  arrange(topico)

## Criar um gráfico de área com ggplot2
plot_evolucao_trabalhos_femininos<-ggplot(dados_femininos_por_ano, aes(x = Ano.de.defesa, y = total, fill = topico)) +
  geom_area() +
  labs(x = "Ano de Defesa", y = "Evolução dos Trabalhos Femininos em IA", fill = "Tópicos") +
  theme_minimal()

print(plot_evolucao_trabalhos_femininos)
## Código para exportar o gráfico em formato .png
ggsave("evolucao_trabalhos_femininos_vs_tempo.png", plot = plot_evolucao_trabalhos_femininos, width = 10, height = 6, path="/content/drive/MyDrive/Arquivo_leila")

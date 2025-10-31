
### Pré-ambulo ###
import pandas as pd
import numpy as np
import pyperclip
import matplotlib.pyplot as plt
import re
import glob
import fitz
import os
import nltk
from genderbr import get_gender
from gensim.models import Word2Vec, KeyedVectors
from gensim.utils import simple_preprocess
from gensim.parsing.preprocessing import remove_stopwords
from sklearn.manifold import TSNE
from sklearn.cluster import KMeans
from sklearn.decomposition import PCA
from unidecode import unidecode
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize
from scipy.spatial.distance import cdist





### Base de Dados ###
data = pd.read_csv(r'C:\Users\lucas\Documents\Trabalhos\Data_Science\Portfolio\Data_Analysis_R\search_result-815643304_ORIGINAL(1).csv')
#data.info()
#data.describe
#data.head()

### Ajuste de Base de Dados ###
data=data.drop(columns=['ID Lattes do(a) autor(a)','ID Lattes dos orientadores',
    'Membros da banca','ID Lattes dos membros da banca','Instituição de defesa',
    'Departamento da instituição de defesa','Programa de Pós-Graduação da instituição de defesa',
    'Área do conhecimento CNPq', 'Tipos de acesso','Assuntos em inglês',
    'Idioma', 'Resumo', 'Resumo em Português', 'Resumo em Inglês','Link de acesso',
    'Referência Bibliográfica'])
#data.isnull().sum()
data_autores = data['Autor(a)'].str.split(',', expand=True)
data_autores.drop(columns=[2,3], inplace=True)
data_autores[data_autores[0].str.isalpha()==True].index
data_autores[data_autores[1].str.isalpha()==True].index
#data_autores[data_autores[2].str.isalpha()==True].index
simb_col0 = data_autores[data_autores[0].str.contains(r"[^A-Za-zÀ-ÖØ-öø-ÿ\s\-]", na=False)]
simb_col1 = data_autores[data_autores[1].str.contains(r"[^A-Za-zÀ-ÖØ-öø-ÿ\s\-]", na=False)]
#simb_col2 = data_autores[data_autores[2].str.contains(r"[^A-Za-zÀ-ÖØ-öø-ÿ\s\-]", na=False)]
data_autores.iloc[simb_col0.index,0]=['DORNELLAS','DORNELLAS','Santanna','Dias','Celiberto Jr','Pedro Henrique DAlmeida Giberti Rissato','DIlton Moreira Silveira','Roberto DAmore','Thiago Carvalho DAvila','Goncalves','Jr','Patrick Cadier DAquino e Baroni Santos','Melo Jr','Bulhoes','Goda','Tupinamba DOliveira Junior','Souza Junior','Assuncao','Barbosa Junior','Dores','Avila','Avila','Correa','SantAnna']
data_autores.iloc[simb_col1.index,1]=['Bruno Burini Robles','Lucas De Oliveira Batista',' Renan Fonseca','Alysson Gomes de','Filipe Alves de','Kamila Cunha De','Fábio Henrique Antunes','Ana Paula Alves','José Aldo Silva da','Joelson Nogueira de','Lauro Beltrão','Caio Saraiva','Eulanda Miranda dos','Luis Fernando Altenfelder de Arruda','Filipe José Medon','Brunna de Sousa Pereira','Wagner Gomes','José Arthur Pinto','Ferdinando Fernandes','Daniela Cabral de','Osvaldo Cesar Pinheiro de','Eduardo de Aguiar','Arielle dos Santos Bassanelli','Paul','Ana Carolina Melik','Mário Luiz de','Gleica DalOngaro','Gleica DalOngaro','Rômulo Nunes de','Evellyn de Assis','Thais Berrettini ','José Antônio Cândido Borges da','Ricardo José Leite de','Vitor Ayres','Bruna Salles','Fabio Piola','Bruno Albuquerque',' Bernardo Teixeira',' Lucas Garcia',' Matheus de Araújo','Eduardo Roque Nóbrega de','Victor Hugo Borba','Nathália Cristina Alves','Ivo Mário','Ariangelo Hauer','Silvia Maria Fonseca','Sergio Aparecido Braga da','Cleonilson Protásio de',' David Nadler',' Sérgio Faustino','Tarig Ali Abdurrahman El',' Maria José Herculano','Roni Valter de Souza',' Richerland Pinto', 'João José Peixoto',' Eduardo Bicalho','Juliane Elizabete de Souza','Cleuves Cajé de',' Israel Aires Costa',' Aline Barroca','Hamilton Santos', 'Pedro Henrique Moraes' ,' Lucas','Fernando Cesar' ,' Giuseppe' ,' Germana Menezes da', 'Francisco de Assis Pereira Vasconcelos de', ' José Fernando', 'Volnei da Silva','Alan Pedro da', ' David Nadler', ' Ernesto Trajano de', 'Roberta Vilhena', ' Ricardo Rubens Gomes', ' Fabrício Augusto', 'JOSÉ EDUARDO DE MELLO', ' Juliana Hoffmann Quinonez', 'Edvaldo Gomes' ,' Dheny', 'Ludimila Carvalho', 'Onilton de Oliveira', 'Maria Elisa', 'Sheila da Nóbrega', 'Luiz Henrique dos Santos', 'Anderson Antonio Carvalho', 'Diego de Azevedo', 'Mafran Martins', 'Alessandro Araújo', 'GABRIEL BASTOS', ' Sonia Leal', 'Olga Christina de Oliveira', 'William de Medeiros', 'Matheus Pereira Macedo de' ,'SANDRO AUGUSTO', 'José Reinaldo Da Silva Cabral De', ' Ivo Reis', 'Isadora Fernandes', 'Mauro Vitor de', 'Mauro Vitor' ,' Nicia Cristina Rocha', 'Matheus Gomes' ,' Thiago Goncalves Dos Santos', 'Welington de Souza', 'Alexandre Fructuoso da' ,' Luís Henrique Féres', 'Alex Kantorowicz', ' Ednaldo Dilorenzo de', 'Diane Otília Lima', ' Jayne de Morais', ' Alana Marques de', 'Ângela Batista' ,' Álvaro Vinícius de Souza', 'Aleksandra do Socorro da', ' Elisângela Silva da', 'Cláudio Reginaldo' ,' José Homero Feitosa', 'Marcos Antônio dos Santos', ' Lucas de Oliveira', 'Francisco de Assis Coutinho', ' Francisco Fabian de Macedo', 'Ig Ibert Bittencourt Santana', ' Lidiana de França', 'Cristiany de Nazaré Moscoso do Amaral', 'Felipe de Oliveira' ,' Renato de Macedo', 'Bruno Rodrigues de', ' Karla Rodrigues', 'Diego Henrique Emygdio' ,' Máximo Ítalo DAlmeida Athayde', 'Alex Sandro da Cunha', 'Filipe Dwan', 'LUKERCIO DE ABREU', ' Mauro Vitor', 'José Carlos Soares de', 'ADELINE CECILIA', 'José Rodrigo de Castro', 'Ricardo Ricci', 'RODRIGO ANDRADE', 'Thiago do Nascimento Santana de', 'Jorge da Costa','Adriano de Souza', ' Cícero Manoel dos', 'Esther Camilo dos', ' Carlos Alberto', 'Maurício Bruno Prado da' ,' Bartolomeu Félix', 'Anderson Almeida', ' Erito Marques de', 'João Pedro da Costa' ,' Brenno de Mello', 'Danilo Ricardo', ' Patricia Morais da Matta', 'Antônio Carlos de Abreu', 'Luiz Antonio', 'DANILO HERNANI', ' Thiago Feliph Silva' ,' João Maria de', 'Ricardo Bennesby da' ,'NINA IRIS', 'Giovany Oliveira Homem da' ,' Alex Fernandes de ', 'Maíra Baptista de', 'Fabio', 'YURI MARINHO', 'Marcello Montillo', ' Thiago Matheus', 'Suzane Peres', ' Jorge Cesar Abrantes de', 'Fernanda Beatriz' ,'ISAAC JESUS DA' ,'DOUGLAS DE RIZZO', ' RAFFAELLO','Claudia DellAgnolo',' André Franco','Maria Conceição Peres Young', 'Luís Fernando Ascenção' ,' Marcelo Rubia da', 'Euler Cássio Tavares de', 'Ricardo dos Santos', 'Andréa Corrêa Flôres', 'Francisco Araújo de', 'Pedro Sergio', ' Haroldo Cesar Frota' ,' Mauro Cavalcante', 'Patrícia Santos', ' Mário Ernesto de Souza e', 'José Ricardo da Silva', ' Algeir Prazeres', ' Alexandre Bezerra', 'Deam James Azevedo da', ' Esdras Ferreira' ,' Luciano Reis', 'Josenildo Costa da' ,' Jorge de Oliveira', 'Francisco Petrônio Alencar de', ' Fernando Maia', ' Thiago Pereira da', 'Thalita Cristine Ribeiro Lucas', 'Mariela Mizota', 'João Victor Campos de' ,'AMANDA GOMES', 'Mailson Freire de' ,' João Pedro Barros', 'Carlos de Oliveira' ,' Luis Fernando de', 'MOISÉS SANTANA', 'Rafael Nakamura', ' Mayara Simões', 'Filipe Santana Moreira do', 'Maíra Suzuka', ' José Guilherme Magalini Santos', 'Vitor de Andrade', 'Mayane Batista', 'Adriano', 'Felipe Azevedo', 'Klessius Renato', 'Adriano Honorato de', 'Guilherme Monteiro da', 'Leonardo Tadeu', 'Flavia Monteiro', 'Marcos Yuzuru de Oliveira' ,' Luiz Henrique', 'Thais Gomes de', 'Paulo César da Rocha', 'Sergio L', ' Cícero Marcelo de', ' Luís Henrique', 'Milton Fernando Campos', ' Frederico LG de', ' Eleandro A', 'Tânia Cristina DAgostini' ,' Paulo Henrique Soares ', 'Luciano', 'João Roberto Gomes', 'Janaína Mayara Pinto do', 'Alexandre da Costa e Silva', ' Anderson Luiz dos Santos ', 'Lia Borges de Mattos ', 'Diego Falcão de', 'Lucia Helena de Matos', ' Laura SantAnna Gualda', ' Fabio Rodrigues', 'Lucas Eduardo de Oliveira', 'Javier Zambreno', 'Eric Vieira das', 'Roberto Cidade', 'Adeilson Souza da', 'João Carlos', ' Luciênio de Macêdo','Vera Lucia Prudência dos' ,' Antonio Carlos de' ,' José Pedrosa', 'Flavius da Luz', ' Cícero Fellipe Diniz de', ' Rafael Mendonça Rocha', 'Raphael Silva de', 'Laion Lima', ' André Gustavo de', 'Carlos José de Almeida', ' Mateus Dias', 'Luciano Eustáquio', 'Anderson Sousa de', 'Carlos Raimundo Pereira dos', 'Diego Henrique', ' Gregori Stefanello', ' Talita Lôbo de', 'Elizabeth Haruna', ' Luiz Henrique de', 'Caio Moura', 'Márcia', 'Haline Pereira de Oliveira','George Leandro dos Santos','Aline Meira',' Carlos Alberto Campos da', 'Letícia Andrade', 'Márcio Aurélio dos Santos', 'Sônia Ribeiro ', ' Luiz' ,' Marco Aurélio de', 'Thaciana Guimarães de Oliveira' ,' Bruno Andrade', 'Allan Lincoln', ' Jean Felipe Fonseca de', ' Órion Darshan Winter de' ,' Estevao Smania', ' Angelo Eduardo', ' Juan Gabriel', ' Josiane Rodrigues da', ' Filipe Dwan', ' Francisco Fagner do Rego', ' Diego Fernandes de', ' Melquisedec Albert Einstein de Andrade',' Felipe Seabra', ' Sérgio de Brito', ' Francisco Magno Soares da', ' Lucas de Oliveira', ' Antonio Ricardo', ' Vinicius', ' Andre Vinicius', ' Gustavo de Aquino e', ' Thales Menezes de', ' Ígor Barbosa da' ,'MURILLO FREITAS', ' Guilherme Targino',' Jonas' ,'DAVI GUARACHO' ,' Luis Felipe de' ,' Gladson Euler', ' Carlos Roberto Dutra', ' Alexandre Nietupski', ' Adalberto Gomes', ' Bárbara Ribeiro da', 'CRISTIANO LOPES', ' Myke Douglas de Medeiros', ' Ángela Patricia', ' Felipe Delestro', ' Sergio Luiz', ' Rinaldo José de', ' Mateus Benchimol Ferreira de', 'FÁBIO HENRIQUE GONÇALVES','Juliana Fonseca','Ana Carolina Ferreira','Luciana Garcia da Silva','Selma Leticia Capinzaiki','Valquíria Ribeiro de Carvalho','Thiago Barbosa','Maurizio','Rogério Mendonça','Eduardo Batista de Moraes','Lilian Victorino Félix de','Wesllen Sousa','Linara Souza da Costa','João Renato Aguiar','Endrews Sznyder Souza da','Raul Emídio de','Luis Marcello Moraes','Yadini Pérez','Antônio Arquelau de Oliveira','ANDREY ARAUJO','FELIPE MARTINO ESPOSITO','Pedro Vianna ','Ricardo Taoni ','Eliéder Prates','Stanley Soares','Ketlen Karine Teles','PEDRO AUGUSTO SANTOS ORONA','Leandro','Marcio Luis','Janderson Borges do','Tonny Franck Osaki da','Igor Felipe Sodré Ribeiro','David Alan de Oliveira','Luísa dos Reis e','Luyla Advincula Candido de','RODRIGO ALVES DE LIMA','BRENNO TONDATO DE','DAVI ARAUJO DAL','JULIANO ALVES DE','Melis Mendes','Aline Márcia Marques','Fernando Wallase Carvalho','Guilherme Marino','Thiago de Souza','Albert França Josuá','Regis Antonio Saraiva','Maxwell Guimarães de','Paulo Antonio Caliendo Velloso da','Daniel Castro','Martha Roxana','Fernanda Teixeira','Thiago Villela Bastos','Pedro Henrique Venske da','Shirlene Kelly Santos','João Fernandes Britto','Kíssia','Aníbal','Stanley Robson de Medeiros','Haroldo Castro','José Demisio Simões da','Eduardo Lopes de','Agnaldo Volpe','Rafael Corrêa Gama de','Caio Santos Bezerra','Fernando Felix do','Jean','Aline R','Gilberto','Carolini C','Marcelo Alberto','Erik Navarro','Maria Regina Detoni Cavalcanti Rigolon','Luana Helena Oliveira Monteiro','Ranilson Oscar Araújo','Gypson Dutra Junqueira','Bruno','Pedro Marcio Raposo','Tânia Cristina DAgostini','Clayton Reginaldo','Ruben','Elizangela Santos da','FELIPE ALBERTO','ADELINE CECILIA','SILVIO ROMERO DE','Vinicius Sanchez dos','Dimas Cassimiro do','Danilo Coura','Guilherme Brandão','José David','Letícia Borges','Márcio Palheta','Antonio Jose Sobrinho','Luis Otavio de Colla','Max Willian Soares','Luís Augusto Martins','Vinícius Vassoler','Maria Azevedo','Emanuela Cristina Ramos','THIAGO TURCATO DO','DOUGLAS DE RIZZO','LEONARDO CONTADOR','VICTOR','RODRIGO DE CARVALHO','ERIC DE BAÉRE','DIEGO EDUARDO','Wellington Pacheco','Suzana Aparecida','Diego de Azevedo','Rodrigo Azevedo da','Vinicius da Silva','Leonardo Fontes do','Michel Marialva','Bashir','Mário Hozano Lucas de','Natalia Rodrigues','LAURA MARTINS VALENTE RAMOS','Romário Pimenta','Franciele Marques','Luciana Jeferson de','Juan Gabriel','Luis Miguel Rojas','Hendrio Luis de Souza','Wesllen Sousa','Bruno Ábia','Evandro de Barros','Rupila Rami da Silva','Agenor de Sousa','Antônio Luiz Mattos de Souza','Isabel','Marília','Franklin René Castro','Leonardo Falcão','Milena Marinho','Brenner Biasi Souza','Everton Luis','Fabio Brandolt','Antonio Rodrigo Delepiane De','Antonio','Everton de Oliveira','Julio Cesar da Costa','Jose Airton Chaves','Amanda de Oliveira Sabino das','Joelson Nogueira de','Bruno Rafael Araújo',' Mauricio Boff de','Mauricio Boff de','Michael Taynnan Albuquerque de Oliveira','Fernando Henrique Bezerra','Guilherme Monteiro','Elua Ramos','José Gildo de','Jose Leonardo dos Santos',' Tiago Lucas Pereira',' Hério Ênio de Sousa',' Danielle',' Elua Ramos',' Henriqueta Talita Guimaraes',' Thaisa de Oliveira','Camilla Zamfolini','Bárbara Lessa']
#pyperclip.copy(str(simb_col2.iloc[:, 2].values))
#simb_col2.iloc[:,2]=np.nan
#simb_col2.iloc[[101,166],2]=['Luis Allan','Jaci Corrêa']
#data_autores.iloc[simb_col2.index,2]=simb_col2.iloc[:,2].values.tolist()
data["Autor(a)"] = np.where(data_autores.iloc[:,1].notna(),
    data_autores.iloc[:,1] + " " +data_autores.iloc[:,0],
    data_autores.iloc[:,0])
data['Genero'] = data['Autor(a)'].apply(lambda x: x.split()[0]).apply(get_gender)

data['Assuntos em português'] = data['Assuntos em português'].str.split(r'\s*(?:\|\||;)\s*')
keywords=data['Assuntos em português'].explode()
print(len(keywords))
keywords=keywords.drop_duplicates().dropna()
keywords_symbol=keywords[keywords.str.contains(r"[^A-Za-zÀ-ÖØ-öø-ÿ\s\-]", na=False)].tolist()
#keywords_parentese=keywords[keywords.str.contains(r"[\(\)]", na=False)].tolist()
keywords = keywords.str.replace(r"[\(\)]", "", regex=True)
#keywords_arroba=keywords[keywords.str.contains(r"[@]", na=False)].tolist()
keywords = keywords.str.replace('CSCL@Work','pedagogia', regex=True)
#keywords_cnpq=keywords[keywords.str.contains(r'CNPQ::', na=False)].tolist()
keywords = keywords.str.replace('CNPQ::',"", regex=True)
#keywords_dp=keywords[keywords.str.contains(r'::', na=False)].tolist()
keywords = keywords.str.replace(r'::.*', '', regex=True)
#keywords_p=keywords[keywords.str.contains(r':', na=False)].tolist()
keywords = keywords.str.replace(r':.*', '', regex=True)
keywords_pi=keywords[keywords.str.contains(r'\?', na=False)].tolist()
list_pi=['Paciente', 'Aprendizado de Maquina', 'Aprendizado Nao-Supervisionado', 'Aprendizagem de maquina', 'Inteligencia Artificial', 'Inteligencia Computacional', 'tecnicas espectrais', 'fluorescencia', 'Inteligencia artificial', 'seguranca', 'Consumo', 'Aprendizagem de maquina', 'Caracterizacao', 'Reconhecimento', 'Inteligencia Artificial', 'Logica Fuzzy', 'Reabilitacao', 'visao computacional', 'aprendizado de maquina', 'Execucao Fiscal', 'Eficiencia', 'Justica Federal', 'Aprendizado de Maquinas', 'Fiscalizacao Tributaria', 'Predicao de Falhas de Software', 'Aprendiza do de maquina', 'Farmacogenomica', 'Cancer de Esofago', 'Aprendizado de maquina', 'Area de referencia', 'Processo Judicial Eletronico', 'Analise de Sentimento', 'Poloticas Publicas', 'Direito Tributario', 'Aprendizado por reforco', 'Indexacao automatica', 'Direito Probatorio', 'Inteligencia Artificial', 'Estatistica Multivariada', 'Aprendizagem de Maquina', 'Extracao de Informacao', 'Identificacao de plantas', 'Evacuacao de Multidoes', 'Simulacao de Humanos Virtuais', 'Seguranca', 'Multidoes', 'Identificacao de Perfis', 'Aplicativos Moveis', 'score', 'proteina', 'Bioinformatica', 'Ressonancia Magnetica', 'Cerebral', 'Visao Computacional', 'Reconhecimento de Acoes', 'Classificacao', 'Regressao', 'Planejamento Classico', 'Decomposicao', 'Processo de Decisao', 'algebra', 'Tensores', 'Ruido Conduzido', 'Interferdncia Eletromagnetica', 'Reflectancia foliar', 'Nutricao florestal', 'Analise multivariada', 'Maquina de aprendizagem', 'Astroinformatica', 'Linhas em emissao', 'Segmentacao Automatica de Tecidos Cerebrais', 'Imagens de Ressonancia Magnetica', 'faces', 'Visao computacional', 'Redes geradoras', 'Escalonamento', 'Trabalho', 'Computacao em Nuvem', 'Simulacao', 'Mineracao de Dados', 'Atributos', 'Classificadores', 'Mineracao de dados', 'Predicao', 'Processo Eletronico', 'Processo Fisico', 'Seguranca Juridica', 'Participacao', 'Juiz', 'dados ausentes', 'Evasao', 'etica', 'Hardware', 'Projeto', 'Trabalho', 'Logica', 'Direito', 'Justica', 'anomalias', 'climatica', 'Dados', 'Series Temporais', 'Previsao', 'Moda', 'desvanecimento', 'Paciente', 'Aprendizado de Maquina Supervisionado', 'INFORMATICA', 'ANALISE', 'Industria', 'IMAGEM', 'IMAGEM', 'TEXTOS', 'CATEGORIZACAO LINGUISTICA', 'ALGORITMOS', 'DADOS', 'CLASSIFICACAO', 'INFORMACAO', 'Metrica', 'Imagens', 'PROTEINAS', 'Portugues Brasileiro', 'Modelagem', 'Reconhecimento', 'Predicao de falha', 'Video', 'Praticas', 'Imagens', 'Pessoas', 'Aprendizado de Maquina', 'APRENDIZAGEM DE MAQUINA', 'Agrupamento', 'Heuristicas', 'Otimizacao', 'LINGUISTICA COMPUTACIONAL', 'telecomunicacoes', 'Direito Dados', 'Evolucao Disruptiva', 'Polinizacao', 'Agrotoxicos', 'Avaliacao de Risco', 'Visao Computacional', 'Controle', 'Identificacao', 'Agricultura', 'Cibernetica', 'Policia', 'Comunicacao', 'Jogos', 'Emocoes', 'Adaptacao', 'COMPUTADORES', 'REDES', 'PROCESSOS', 'COMPUTACAO', 'score', 'SISTEMAS', 'Classificacao', 'Falhas', 'Sistemas Distribuidos', 'Resolucao', 'perfuracao', 'circulacao', 'Repositorio', 'Cientifica', 'Tecnologia', 'Juridicos', 'Agro', 'Tecnologia', 'Recursos hidricos', 'Otimizacao agricola', 'Solo', 'Aprendizado', 'Otimizacao', 'Processo', 'Cancer de mama', 'Poluicao solo', 'Geneticos', 'Questionario', 'Adaptacao', 'Validacao', 'Rede', 'cientifico', 'validacao', 'proveniencia', 'meso', 'Doenca', 'triazois', 'esterol', 'empirico', 'Organizacional', 'Organizacional', 'Metodo', 'Geneticos', 'Reintroducao', 'Nitrogenio', 'agricultura', 'Solo', 'fungos colheita', 'perfuracao', 'Geoestatistica', 'Efeito eletrolitico', 'verde', 'Polifeneis', 'agrupamento', 'diversidade', 'dinamica', 'Polifeneis']
for old, new in zip(keywords_pi, list_pi):
    keywords = keywords.str.replace(re.escape(old), new, regex=True)
#keywords_trac=keywords[keywords.str.contains(r'-', na=False)].tolist()
keywords = keywords.str.replace(r'-', '', regex=True)
#keywords_und=keywords[keywords.str.contains(r'_', na=False)].tolist()
keywords = keywords.str.replace(r'_', '', regex=True)
#keywords_pt=keywords[keywords.str.contains(r'\.', na=False)].tolist()
keywords = keywords.str.replace(r'\.', '', regex=True)
#keywords_vir=keywords[keywords.str.contains(r',', na=False)].tolist()
keywords = keywords.str.replace(r',', '', regex=True)
keywords_e=keywords[keywords.str.contains(r'\&', na=False)].tolist()
list_e=['ATAQUE', 'Musica', 'Computacao', 'IBOVESPA', 'imersao', 'emocoes', 'Marco legal', 'Modelos']
for old, new in zip(keywords_e, list_e):
    keywords = keywords.str.replace(re.escape(old), new, regex=True)

#keywords_num=keywords[keywords.str.contains(r'\d+', na=False)].tolist()
#keywords = keywords.str.replace(r'\.', '', regex=True)

#keywords_fdc=keywords[keywords.str.contains(r"[^A-Za-zÀ-ÖØ-öø-ÿ\s\-]", na=False)].tolist()
keywords=keywords.str.replace(r"[^A-Za-zÀ-ÖØ-öø-ÿ\s\-]", '', regex=True)
print(keywords.dtype)
keywords.info()

tam_palavras = keywords.str.split().str.len()
tam_palavras.value_counts().sort_index()
tam_pal_10mais = keywords[tam_palavras.nlargest(27).index].tolist()
list_pal_10mais=['Algoritmos', 'Algoritmos', 'Classificação', 'Extração de Informação', 'Redes complexas classificação', 'Predição', 'Classificação de Textos', 'Classificação', 'Critérios de Seleção', 'Geração de Características', 'Geração de Termos', 'Classificação', 'Classificação', 'Rede neural', 'Deep learning ', 'Análise de Componentes Principal', 'Programação genética', 'programação', ' Controladores Genética', 'Classificação de Textos', 'regressão logística', 'Algoritmos', 'Algoritmos', 'DESIGN', 'Metadesign', 'Game design', 'Redes Bayesianas previsão', 'Machine learning', ' Contabilidade  Estudo e ensino', 'CIÊNCIAS CONTÁBEIS', 'Ministério da Educação', 'Educacionais dados', 'Seleção', 'Governo', 'Redes neurais', 'Câncer', 'estrada', 'Classificação', 'processamento de linguagem natural', 'Cardíaco', 'Algoritmo', 'Decisões', 'Direito', 'Direito', 'Intervenção humana', 'Privacidade', 'Direito', 'Classificação']
for old, new in zip(tam_pal_10mais, list_pal_10mais):
    keywords = keywords.str.replace(re.escape(old), new, regex=True)
tam_pal_9mais = keywords[tam_palavras.nlargest(27).index].tolist()
list_pal_9mais=['Classificação', 'Classificação', 'Seleção', 'Geração', 'Geração', 'Classificação de textos', 'INTELIGÊNCIA ARTIFICIAL', 'APRENDIZADO COMPUTACIONAL', 'MARKETING', 'POLÍTICAS', 'MONITORAMENTO', 'ARTIFICIAL INTELLIGENCE', 'MACHINE LEARNING', 'POLITICA', 'MONITORAMENTO', 'SOCIAIS', 'Agroquímicos', 'predição', 'DINÂMICA', 'SURFACTANTES', 'PETROLEO', 'MOLECULAR', 'SURFACTANTES', 'MATERIAIS', 'Planejamento', 'planejamento', 'decisão', 'Riscos', 'Avaliação de riscos', 'Gestão de riscos', 'NIDS', 'Redes de computadores', 'Otimização', 'Classificadores', 'Rede de computadores', 'Transmissão de dados', 'Desempenho', 'Computador', 'Dados', 'performance', 'PUBLICIDADE', 'MÍDIA', 'SOCIEDADE', 'DADOS', 'ALGORITMO', 'PROPAGANDA', 'MIDIA', 'SOCIEDADE', 'DADOS', 'ALGORITMO', 'SOCIAIS', 'agrícola', 'Crime', 'segurança', 'crimes', 'Computador', 'classificação', 'combustíveis', 'Precificação', 'Classificação', 'Risco', 'Random Forest', 'doença', 'Classificação de Imagens Fiscais', 'tecnologia', 'tecnologia', 'tecnologia', 'tecnologia', 'Empresas', 'Informação', 'REDES NEURAIS', 'IMAGENS', 'INTERPRETABILIDADE', 'EXPLICABILIDADE', 'REDES NEURAIS', 'IMAGEM', 'INTERPRETABILITY', 'EXPLICABILITY', 'INFORMAÇÃO', 'Representação', 'mineração de dados', 'Aprendizado de maquina', 'Internet das coisas  indústria ', 'RECOMENDAÇÕES',  'MACHINE LEARNING', 'APRENDIZADO DE MÁQUINA', 'RECOMENDAÇÕES', 'RECOMENDAÇÃO', 'COMPUTAÇÃO', 'ELETROENCEFALOGRAFIA', 'DOENÇA', 'REDUÇÃO DE ATRIBUTOS', 'Medicina', 'ALZHEIMER', 'Seleção', 'NEUROCIÊNCIA', 'dados', 'Lei de Dados', 'processo', 'poluição', 'Baterias', 'Imagem', 'Classificação', 'Imagem', 'Principal Componente', 'Mineração de texto Educação', 'Sociedade', 'Risco', 'Administração Pública e Judiciário', 'Decisões']
for old, new in zip(tam_pal_9mais, list_pal_9mais):
    keywords = keywords.str.replace(re.escape(old), new, regex=True)
tam_pal_7mais = keywords[tam_palavras.nlargest(79).index].tolist()


#print(tam_pal_10mais)
pyperclip.copy(tam_pal_7mais)
tam_palavras.hist(bins=range(1, tam_palavras.max()+2))
plt.xlabel("Número de palavras")
plt.ylabel("Frequência")
plt.title("Distribuição do tamanho das expressões")
plt.show()
#pyperclip.copy(keywords_num)
#pyperclip.copy(keywords_symbol)


arquivos = [os.path.join("Livros-IA" , arq) for arq in os.listdir("Livros-IA" ) if arq.endswith('.pdf')]

corpus = []
for arq in arquivos:
    print(f"\nLendo {arq} ...")
    texto = ""
    with fitz.open(arq) as doc:
        for pagina in doc:
            texto += pagina.get_text("text")
    print(f"→ Texto extraído: {len(texto)} caracteres")
    texto = texto.lower()
    texto = unidecode(texto)
    texto = re.sub(r'\d+', '', texto) 
    texto = re.sub(r'\s+', ' ', texto)  
    tokens = word_tokenize(texto)
    #stop_words = set(stopwords.words('portuguese'))
    #tokens = [t for t in tokens if t.isalpha() and t not in stop_words]
    print(f"→ {len(tokens)} tokens extraídos")
    corpus.append(tokens)

modelo = Word2Vec(
    sentences=corpus,
    vector_size=200,
    window=5,
    min_count=3,
    sg=1,
    workers=4,
    epochs=30)
modelo.save("embedding_IA_pdf.model")
print(modelo.wv)
vetores = []
expressoes_validas = []
for expr in keywords:
    palavras = expr.lower().split()
    palavras_validas = [p for p in palavras if p in modelo.wv]
    if not palavras_validas:
        continue
    v = sum(modelo.wv[p] for p in palavras_validas) / len(palavras_validas)
    vetores.append(v)
    expressoes_validas.append(expr)

len(vetores)
len(expressoes_validas)  
### Faço um PCA para diminuir a dimensionalidade dos vetores para 3D ###
coords_pca = PCA(n_components=3).fit_transform(vetores)
print(min(coords_pca[:,2]))
### Plot dos pontos no espaço 3D - PCA ###
plt.figure(figsize=(8,6)).add_subplot(projection='3d')
plt.scatter(coords_pca[:,0], coords_pca[:,1], coords_pca[:,2],marker='o')
plt.title("Espaço semântico (PCA)")
plt.xlim(-1, 2)
plt.ylim(-1, 1)
plt.set_zlim(-1.5, 1.5)
plt.xlabel('X')
plt.ylabel('Y')
plt.zlabel('Z')
plt.show()


### TESTE - fazer k-means antes do pca
kmeans_teste=KMeans(n_clusters=8, random_state=13)
labels_teste = kmeans_teste.fit_predict(vetores)
cluster_cent_teste=kmeans_teste.cluster_centers_

print(cluster_cent_teste.shape)
distancias_teste = cdist(cluster_cent_teste, vetores, metric='cosine')
palavras_representativas_teste = []
for i, d in enumerate(distancias_teste):
    idx_min = np.argmin(d)
    palavra_mais_proxima = expressoes_validas[idx_min]
    palavras_representativas_teste.append(palavra_mais_proxima)
    print(f"Cluster {i}: {palavra_mais_proxima}")



### Faço uma clusterização dos vetores 3D ###
kmeans = KMeans(n_clusters=8, random_state=13)
labels = kmeans.fit_predict(coords_pca)
cluster_cent=kmeans.cluster_centers_

distancias = cdist(cluster_cent, vetores, metric='cosine')
palavras_representativas = []
for i, d in enumerate(distancias):
    idx_min = np.argmin(d)
    palavra_mais_proxima = expressoes_validas[idx_min]
    palavras_representativas.append(palavra_mais_proxima)
    print(f"Cluster {i}: {palavra_mais_proxima}")


plt.figure(figsize=(10,8)).add_subplot(projection='3d')
plt.scatter(coords_pca[:,0], coords_pca[:,1], coords_pca[:,2], c=labels, cmap='tab10')
plt.xlim(-5, 5)
plt.ylim(-10, 10)
#plt.zlim(-0.05, 0.01)
plt.xlabel('X')
plt.ylabel('Y')
plt.title("Agrupamento semântico (PCA + KMeans)")
plt.show()


palavras_validas = [p for p in keywords if p in modelo.wv]
vetores = np.array([modelo.wv[p] for p in palavras_validas]) 
palavras_invalidas=[p for p in keywords if p not in palavras_validas]
print(f"\nTotal de palavras encontradas no modelo: {len(palavras_invalidas)}")



coords_tsne = TSNE(n_components=3, perplexity=30, random_state=42, max_iter=1500).fit_transform(vetores)
print(coords_tsne)
plt.figure(figsize=(10,8)).add_subplot(projection='3d')
plt.scatter(coords_tsne[:,0], coords_tsne[:,1], coords_tsne[:,2], alpha=0.6, s=25)
plt.title("Espaço semântico (t-SNE) — Embedding de IA")
plt.show()





#palavras_validas = [p for p in keywords if p in modelo.wv]
#vetores = [modelo.wv[p] for p in palavras_validas]


#vetores = np.array(vetores)
#tsne = TSNE(n_components=2, perplexity=30, random_state=42, max_iter=1500)
#coords_tsne = tsne.fit_transform(vetores)



#kmeans = KMeans(n_clusters=8, random_state=42)
#labels = kmeans.fit_predict(coords_tsne)

plt.figure(figsize=(10,8))
plt.scatter(coords_tsne[:,0], coords_tsne[:,1],
            c=labels, cmap='tab10', s=40, alpha=0.7, edgecolors='k')


#for i, palavra in enumerate(palavras_validas):
#    if i % 10 == 0:  
#        plt.annotate(palavra, (coords_tsne[i,0], coords_tsne[i,1]),
#                     fontsize=9, alpha=0.8)

plt.title("Mapa semântico — t-SNE + KMeans (Embedding IA)")
plt.xlabel("t-SNE Dimensão 1")
plt.ylabel("t-SNE Dimensão 2")
plt.tight_layout()
plt.show()

plt.figure(figsize=(10,8))
plt.scatter(coords_tsne[:,0], coords_tsne[:,1],
            c=labels, cmap='tab10', s=40, alpha=0.6, edgecolors='k')

# Calcula as posições médias dos clusters no espaço t-SNE
centroides_tsne = np.array([coords_tsne[labels == i].mean(axis=0) for i in range(kmeans.n_clusters)])

plt.scatter(centroides_tsne[:,0], centroides_tsne[:,1],
            c='black', s=120, marker='X', label='Centros')

# Anota as palavras representativas
for i, palavra in enumerate(palavras_representativas):
    plt.annotate(palavra, (centroides_tsne[i,0], centroides_tsne[i,1]),
                 fontsize=10, color='black', fontweight='bold')

plt.title("Mapa semântico — t-SNE + KMeans (Palavras representativas por cluster)")
plt.legend()
plt.tight_layout()
plt.show()

# Calcula distâncias entre centros e todos os vetores
distancias = cdist(kmeans.cluster_centers_, vetores, metric='cosine')

# Para cada centro, acha a palavra mais próxima
palavras_representativas = []
for i, d in enumerate(distancias):
    idx_min = np.argmin(d)
    palavra_mais_proxima = palavras_validas[idx_min]
    palavras_representativas.append(palavra_mais_proxima)
    print(f"Cluster {i}: {palavra_mais_proxima}")


kmeans = KMeans(n_clusters=8, random_state=42)
labels = kmeans.fit_predict(vetores)

distancias = cdist(kmeans.cluster_centers_, vetores, metric='cosine')

palavras_representativas = []
for i, d in enumerate(distancias):
    idx_min = np.argmin(d)
    palavra_mais_proxima = palavras_validas[idx_min]
    palavras_representativas.append(palavra_mais_proxima)
    print(f"Cluster {i}: {palavra_mais_proxima}")
#####  Curso #####
# Exemplo: DASS21, instrumento para avaliacao de afetos negativos/sintomas transdiagnosticos
# Disponivel em http://www2.psy.unsw.edu.au/dass/Portuguese/DASS%2021%20Brazilian%20Portuguese%20Tucci.pdf
# Amostra: como descrita em http://www.scielo.br/scielo.php?script=sci_arttext&pid=S1413-82712015000200259 

# Pacotes:

# Visualizacao de dados psicometricos
#install.packages("qgraph")
# Estimar o modelo Ising em dados dicotomicos 
#install.packages("IsingFit")
# Visualizacao avancada de redes
#install.packages("igraph")
# Analises psicometricas EFA/IRT/Reliability e outras
#install.packages("psych")
library(qgraph)
library(IsingFit)
library(igraph)
library(psych)

### Exemplo parametrico (Gaussian network), no caso presumidamente parametricos (correlacoes policoricas)
dasspoly<-read.csv("https://raw.githubusercontent.com/wagnerLM/netusf/master/dasspoly",sep = ";")
View(dasspoly)
# Nome resumido dos itens
dasslabels<-scan("https://raw.githubusercontent.com/wagnerLM/netusf/master/dasslabels",what = "character", sep = "\n")
dasslabels
# Itens completos 
dassnames<-scan("https://raw.githubusercontent.com/wagnerLM/netusf/master/dassnames",what = "character", sep = "\n")
dassnames

# Grupos de itens por dimensao (estresse, ansiedade, depressao)
# estresse c(1,6,8,11,12,14,18)
# ansiedade c(2,4,7,9,15,19,20)
# depressao c(3,5,10,13,16,17,21)

# Exemplo inicial com a subescala de depressao 
dasspoly_sub<-dasspoly[,c(3,5,10,13,16,17,21)]

# EFA
dass_fa<-fa(dasspoly_sub,1,cor = "poly")
# colar os nomes resumidos dos itens
rownames(dass_fa$loadings)<-paste(dasslabels[c(3,5,10,13,16,17,21)])
# visualizar os resultados da EFA, refletir sobre as informacoes do modelo
dass_fa
# EFA tem algumas limitacoes (http://eiko-fried.com/wp-content/uploads/Fried_2016-04481-001.pdf)

# Visualizacoes de correlacoes, informacao utilizada nos modelos fatoriais (var/covar)
cor.plot(cor_auto(dasspoly_sub),numbers = T)

# Correlacoes em redes (http://psychosystems.org/files/Literature/EpskampEtAl2012.pdf)
# Rede correlacoes bivariadas
dasspoly_sub_g<-qgraph(cor_auto(dasspoly_sub),nodeNames=dasslabels[c(3,5,10,13,16,17,21)])

# Algoritmo de posicionamento (https://onlinelibrary.wiley.com/doi/abs/10.1002/spe.4380211102)
dasspoly_sub_g<-qgraph(cor_auto(dasspoly_sub),nodeNames=dasslabels[c(3,5,10,13,16,17,21)],layout="spring")

# paradoxo da informacao: muita informacao é pouca informacao, poucas distincoes
# correlacao carrega relacoes espurias, vieses
# uma solucao possivel e a correlacao parcial
# correlacoes pariciais sao utilizadas em outros modelos, como por exemplo, nas regressoes multiplas

# Rede de correlacoes parciais (http://psycnet.apa.org/record/2018-13501-001?doi=1)
# Utiliza os argumentos graph, sampleSize e threshold para estimar as correlacoes parciais regularizadas
dasspoly_sub_g<-qgraph(cor_auto(dasspoly_sub),layout="spring",nodeNames=dasslabels[c(3,5,10,13,16,17,21)],graph="glasso",sampleSize=nrow(dasspoly_sub),threshold=T,minimum=0.1,lambda.min.ratio=.002)
# usa um metodo regularizado, fixando em zero valores pequenos e resolvendo assim o problema de esparcialidade

# Como identificar nodos mais relevantes: centralidade
# Medidas de centralidade (http://psychosystems.org/files/Literature/Bootnet.pdf)
#(https://www.sciencedirect.com/science/article/abs/pii/S0092656614000701)
centralityPlot(dasspoly_sub_g,include = "all",labels = dasslabels[c(3,5,10,13,16,17,21)])
centralityPlot(dasspoly_sub_g,include = "ExpectedInfluence",labels = dasslabels[c(3,5,10,13,16,17,21)])

# Visualizacao de vertices especificos
flow(dasspoly_sub_g, "DASS10")

# Matriz esparsa de correlacao parcial regularizada ou matriz de predicao: os numeros!!!
# Sao interpretados como betas de regressao: 0.1 = pequeno, 0.3 = medio e 0.5 = grande
# Matriz adjacente, função getWmat
View(getWmat(dasspoly_sub_g))
cor.plot(getWmat(dasspoly_sub_g),numbers = T)

# Modelo com todos os itens
# Definindo grupos de itens (teoria)
dassgroups<- list("Estresse"=c(1,6,8,11,12,14,18),"Ansiedade"=c(2,4,7,9,15,19,20),"Depressao"=c(3,5,10,13,16,17,21))
dassgroups

# Estimando a rede de correlacoes parciais
dasspoly_g<-qgraph(cor_auto(dasspoly),groups=dassgroups,nodeNames=dasslabels,layout="spring",legend.cex=.3,graph="glasso",sampleSize=nrow(dasspoly),threshold=T)

# Medidas de centralidade 
centralityPlot(dasspoly_g,include = "ExpectedInfluence",labels=dasslabels,orderBy = "ExpectedInfluence")

# Grafico de preditores de 'falta de sentido' (item 21)
flow(dasspoly_g, "DASS21")

# Matriz de predicao 
View(getWmat(dasspoly_g))

# Matriz de mediacao, permite identificar relacoes diretas ou indiretas por meio da medida de shortest path (caminho minimo)
dasspoly_cent<-centrality(dasspoly_g,all.shortest.paths = T)
View(as.data.frame(dasspoly_cent$ShortestPaths))

# Caminhos de influencia, permite destacar no grafo a rota de influencia entre vertices especificos
pathways(dasspoly_g, from = "DASS12", to = "DASS21")
pathways(dasspoly_g, from = "DASS15", to = "DASS21")

# Analise de comunidades, auxilia na identificacao de subgrupos na rede
# https://www.nature.com/articles/srep30750  recomenda metodo multinivel
# Analise utilizando o metodo Louvain
dass_lc<-cluster_louvain(as.igraph(qgraph(abs(getWmat(dasspoly_g)))))
# Visualizacao do numero de comunidades 
dass_lc
# Implementando a informacao na rede, com o argumento 'groups'
qgraph(cor_auto(dasspoly),groups=as.factor(dass_lc$membership),nodeNames=dasslabels,layout="spring",legend.cex=.3,graph="glasso",sampleSize=nrow(dasspoly),threshold=T)
# ha outras opcoes como walktrap ou spinglass
# ha tentativas de igualar o conceito de comunidade e 'variavel latente', contudo e necessario cautela
# (https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0174035)

### Dados dicotomicos 
# Artigos de referencia 
# http://psychosystems.org/files/Literature/vanBorkulo_ising.pdf 
# https://www.nature.com/articles/srep34175.pdf
# vanBorkulo et al desenvolveram um metodo baseado em regressoes logisticas, equivalente ao modelo Ising (fisica)
# O modelo Ising e matematicamente equivalente a TRI (MIRT), pois os itens (vertices) possuem um parametro proprio e 
# um parametro de influencia dos pares, analogos aos parametros de dificuldade e discriminacao na TRI (MIRT)
dassbin<-read.csv("https://raw.githubusercontent.com/wagnerLM/netusf/master/dassbin",sep = ";")
View(dassbin)
# Estimando o modelo Ising 
dass_ising<-IsingFit(dassbin,family = "binomial",AND = TRUE,gamma = 0.25,plot = TRUE)
# Observando o parametro do item (tau), semelhante ao parametro de dificuldade na TRI
dass_ising$thresholds
# Plotando a matriz adjacente usando o qgraph
dass_ising_g<-qgraph(dass_ising$weiadj,layout="spring",nodeNames=dasslabels,groups=dassgroups,legend.cex=0.3)
# Medidas de centralidade, notar que os resultados sao ligeiramente diferentes em funcao do aumento da sensibilidade
centralityPlot(dass_ising_g,include = "ExpectedInfluence",labels=dasslabels,orderBy = "ExpectedInfluence")
# Visualizacao de relacoes diretas e indiretas com um vertice especifico
flow(dass_ising_g, "DASS3")
# uma possibilidade de integrar a informacao do threshold na rede (tamanho dos nodos), com o argumento 'vsize'
dass_ising$thresholds
dass_ising_g<-qgraph(dass_ising$weiadj,layout="spring",nodeNames=dasslabels,groups=dassgroups,legend.cex=0.3,vsize=c((dass_ising$thresholds)+10))

#####  Curso #####
# Exemplo: DASS21, instrumento para avaliacao de afetos negativos/sintomas transdiagnosticos
# Disponivel em http://www2.psy.unsw.edu.au/dass/Portuguese/DASS%2021%20Brazilian%20Portuguese%20Tucci.pdf
# Amostra, como descrita em http://www.scielo.br/scielo.php?script=sci_arttext&pid=S1413-82712015000200259 

# Pacotes
#install.packages("qgraph")
#install.packages("IsingFit")
#install.packages("igraph")
#install.packages("psych")
library(qgraph)
library(IsingFit)
library(igraph)
library(psych)

### Exemplo parametrico (Gaussian network)
dasspoly<-read.csv("https://raw.githubusercontent.com/wagnerLM/netusf/master/dasspoly",sep = ";")
View(dasspoly)
dasslabels<-scan("https://raw.githubusercontent.com/wagnerLM/netusf/master/dasslabels",what = "character", sep = "\n")
dasslabels
dassnames<-scan("https://raw.githubusercontent.com/wagnerLM/netusf/master/dassnames",what = "character", sep = "\n")
dassnames

# grupos por dimensao (estresse, ansiedade, depressao)
# estresse c(1,6,8,11,12,14,18)
# ansiedade c(2,4,7,9,15,19,20)
# depressao c(3,5,10,13,16,17,21)

dasspoly_sub<-dasspoly[,c(3,5,10,13,16,17,21)]

# EFA
dass_fa<-fa(dasspoly_sub,1,cor = "poly")
rownames(dass_fa$loadings)<-paste(dasslabels[c(3,5,10,13,16,17,21)])
dass_fa
# EFA tem algumas limitacoes (http://eiko-fried.com/wp-content/uploads/Fried_2016-04481-001.pdf)

# Correlacoes
cor.plot(cor_auto(dasspoly_sub),numbers = T)

# Correlacoes em redes (http://psychosystems.org/files/Literature/EpskampEtAl2012.pdf)
# Rede correlacoes bivariadas
dasspoly_sub_g<-qgraph(cor_auto(dasspoly_sub),nodeNames=dasslabels[c(3,5,10,13,16,17,21)])
# Algoritmo de posicionamento (https://onlinelibrary.wiley.com/doi/abs/10.1002/spe.4380211102)
dasspoly_sub_g<-qgraph(cor_auto(dasspoly_sub),nodeNames=dasslabels[c(3,5,10,13,16,17,21)],layout="spring")


# Rede de correlacoes parciais (http://psycnet.apa.org/record/2018-13501-001?doi=1)
dasspoly_sub_g<-qgraph(cor_auto(dasspoly_sub),layout="spring",nodeNames=dasslabels[c(3,5,10,13,16,17,21)],graph="glasso",sampleSize=nrow(dasspoly_sub),threshold=T,minimum=0.1,lambda.min.ratio=.002)

# Medidas de centralidade (http://psychosystems.org/files/Literature/Bootnet.pdf)
#(https://www.sciencedirect.com/science/article/abs/pii/S0092656614000701)
centralityPlot(dasspoly_sub_g,include = "all",labels = dasslabels[c(3,5,10,13,16,17,21)])
centralityPlot(dasspoly_sub_g,include = "ExpectedInfluence",labels = dasslabels[c(3,5,10,13,16,17,21)])

# Visualizacao de vertices especificos
flow(dasspoly_sub_g, "DASS10")

# Matriz esparsa de correlacao parcial regularizada ou matriz de predicao
View(getWmat(dasspoly_sub_g))
cor.plot(getWmat(dasspoly_sub_g),numbers = T)

# Modelo com todos os itens
# definindo grupos
dassgroups<- list("Estresse"=c(1,6,8,11,12,14,18),"Ansiedade"=c(2,4,7,9,15,19,20),"Depressao"=c(3,5,10,13,16,17,21))
dassgroups

# estimando a rede de correlacoes parciais
dasspoly_g<-qgraph(cor_auto(dasspoly),groups=dassgroups,nodeNames=dasslabels,layout="spring",legend.cex=.3,graph="glasso",sampleSize=nrow(dasspoly),threshold=T)

# medidas de centralidade 
centralityPlot(dasspoly_g,include = "ExpectedInfluence",labels=dasslabels)

# grafico de preditores
flow(dasspoly_g, "DASS21")

# matriz de predicao 
View(getWmat(dasspoly_g))

# matriz de mediacao 
dasspoly_cent<-centrality(dasspoly_g,all.shortest.paths = T)
View(as.data.frame(dasspoly_cent$ShortestPaths))

# trajetorias de influencia
pathways(dasspoly_g, from = "DASS12", to = "DASS21")
pathways(dasspoly_g, from = "DASS15", to = "DASS21")

# analise de comunidades
# https://www.nature.com/articles/srep30750 
dass_lc<-cluster_louvain(as.igraph(qgraph(abs(getWmat(dasspoly_g)))))
dass_lc
qgraph(cor_auto(dasspoly),groups=as.factor(dass_lc$membership),nodeNames=dasslabels,layout="spring",legend.cex=.3,graph="glasso",sampleSize=nrow(dasspoly),threshold=T)
# ha outras opcoes como walktrap ou spinglass
# ha tentativas de igualar o conceito de comunidade e 'variavel latente', contudo ha criticas a esta abordagem
# (https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0174035)

### Dados dicotomicos 
# Artigo de referencia 
# http://psychosystems.org/files/Literature/vanBorkulo_ising.pdf 
# https://www.nature.com/articles/srep34175 
dassbin<-read.csv("https://raw.githubusercontent.com/wagnerLM/netusf/master/dassbin",sep = ";")
View(dassbin)
dass_ising<-IsingFit(dassbin,family = "binomial",AND = TRUE,gamma = 0.25,plot = TRUE)
dass_ising$thresholds
dass_ising_g<-qgraph(dass_ising$weiadj,layout="spring",nodeNames=dasslabels,groups=dassgroups,legend.cex=0.3)
centralityPlot(dass_ising_g,include = "ExpectedInfluence",labels=dasslabels)
flow(dass_ising_g, "DASS3")
dass_ising$thresholds
dass_ising_g<-qgraph(dass_ising$weiadj,layout="spring",nodeNames=dasslabels,groups=dassgroups,legend.cex=0.3,vsize=c((dass_ising$thresholds)+10))
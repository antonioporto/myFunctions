##################################################################################
### Antonio Porto - FuturaGene/ Suzano S.A.                                      #
### Objetivo: Gerar sorteio para delineamento em faixas                          #
### Data: Outubro 2021                                                           #
################################################################################ #

library(tidyverse)
library(readxl)

#' criando a funcao para sorteio e expotartação da tabela

sorteio.faixas.dbc<-function(fator1, fator2,bloco){

F1 <- as.factor(fator1)
F2 <- as.factor(fator2)
b <- bloco 

#### fazendo o sorteio ###

datalist = list()
options(maxprint=10000)

for(i in 1:b){ # criando estrutura para cada um dos n blocos

F1 <- sample(F1) 
factor1 <- rep(F1, nlevels(F2))
F2 <- sample(F2)
factor2 <- rep(F2, each = nlevels(F1))
str_b <- data.frame(factor1, factor2)
srt_t <- cbind(Block = rep(i, nrow(str_b)), str_b) # inserindo informação do bloco

datalist[[i]] <- srt_t # inserindo objetos dentro da lista

}

options(maxprint=99999)
result = do.call(rbind, datalist)
result <- result[ , order(names(result))]
result <- cbind(Plot = c(1:nrow(result)), result)
print(result)


write.csv2(result, "desing_trial.csv") # exportar sorteio

}


######## Exemplo ##################

# Fator 1, Faixa 1
f1 <- 1:3
# Fator2, Faixa 2
f2 <- c("a", "b", "c")
# número de blocos
blocos = 4


sorteio.faixas.dbc(f1,f2,blocos)

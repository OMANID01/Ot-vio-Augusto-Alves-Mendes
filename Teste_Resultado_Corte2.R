#LEITURA DE DADOS 
dados<-read.table("dados1.txt",header = T,dec=",")

str(dados)
dados$cv<-as.factor(dados$cv)

dados$rep<-as.factor(dados$rep)

dados$corte<-as.factor(dados$corte)

#ANEXAR OS DADOS 
attach(dados)
names(dados)

#RESUMO ESTATISTICO
summary(dados)

#ATIVAR PACOTE ExpDes.pt
library(ExpDes.pt)

#RODAR OS DADOS
dic(dados$cv,dados$ALT,mcomp="tukey")
dic(dados$cv,dados$MSA,mcomp="tukey")
dic(dados$cv,dados$PSA,mcomp="tukey")
dic(dados$cv,dados$PMST,mcomp="tukey")

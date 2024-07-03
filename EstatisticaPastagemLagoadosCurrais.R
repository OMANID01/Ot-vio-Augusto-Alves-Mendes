#LEITURA DE DADOS 
dados<-read.table("dados3.txt",header = T,dec=",")

str(dados)
dados$cv<-as.factor(dados$cv)

dados$Rep<-as.factor(dados$Rep)

#anexar os dados 
attach(dados)
names(dados)

#resumo estatistico
summary(dados)

#P acima de 0,05 

#normaalidade dos residuos - teste de shapiro-wilk
modelo1<-lm(PB~cv+Rep)
r0<-residuals(modelo1)
shapiro.test(r0)

hist(dados$PB)

modelo2<-lm(Lignina~cv+Rep)
r1<-residuals(modelo2)
shapiro.test(r1)

modelo3<-lm(MS~cv+Rep)
r2<-residuals(modelo3)
shapiro.test(r2)

modelo4<-lm(Digestibilidadeinvitro~cv+Rep)
r3<-residuals(modelo4)
shapiro.test(r3)

hist(dados$Digestibilidadeinvitro)

modelo5<-lm(ConteudoCelular~cv+Rep)
r4<-residuals(modelo5)
shapiro.test(r4)

modelo6<-lm(PMSkg.ha~cv+Rep)
r5<-residuals(modelo6)
shapiro.test(r5)

modelo7<-lm(FDA~cv+Rep)
r6<-residuals(modelo7)
shapiro.test(r6)


modelo8<-lm(MateriaMineral~cv+Rep)
r7<-residuals(modelo8)
shapiro.test(r7)


modelo9<-lm(Altmed~cv+Rep)
r8<-residuals(modelo9)
shapiro.test(r8)

modelo10<-lm(FDN~cv+Rep)
r9<-residuals(modelo10)
shapiro.test(r9)

#ANÁLISE DE VARIÂNCIA - ANOVA
anova<-aov(modelo1)
summary(anova)

anova1<-aov(modelo2)
summary(anova1)

anova2<-aov(modelo3)
summary(anova2)

anova3<-aov(modelo4)
summary(anova3)

anova4<-aov(modelo5)
summary(anova4)

anova5<-aov(modelo6)
summary(anova5)

anova6<-aov(modelo7)
summary(anova6)

anova7<-aov(modelo8)
summary(anova7)

anova8<-aov(modelo9)
summary(anova8)

anova9<-aov(modelo10)
summary(anova9)

#Fazer todos os dados rodarem juntos.

formula <- as.formula(paste0("cbind(", paste(names(dados)[4:13], collapse = ","), ") ~ (cv+Rep)^2"))
fit <- aov(formula, data=dados)
summary(fit)
capture.output(summary(fit),file="all_anovas.txt")


#testes de médias - TUKEY
Tk<-TukeyHSD(anova)
Tk
modelox<-lm(PB~cv,data=dados)
plot(modelox)
plot(PB~cv)
dados$cv<-as.numeric(dados$cv)
plot(PB~cv)

Tk1<-TukeyHSD(anova1)
Tk1
modelox1<-lm(Lignina~cv,data=dados)
plot(modelox1)
plot(Lignina~cv)
dados$cv<-as.numeric(dados$cv)
plot(Lignina~cv)

Tk2<-TukeyHSD(anova2)
Tk2
modelox2<-lm(MS~cv,data=dados)
plot(modelox2)
plot(MS~cv)
dados$cv<-as.numeric(dados$cv)
plot(MS~cv)

Tk3<-TukeyHSD(anova3)
Tk3
modelox3<-lm(Digestibilidadeinvitro~cv,data=dados)
plot(modelox3)
plot(Digestibilidadeinvitro~cv)
dados$cv<-as.numeric(dados$cv)
plot(Digestibilidadeinvitro~cv)

Tk4<-TukeyHSD(anova4)
Tk4
modelox4<-lm(ConteudoCelular~cv,data=dados)
plot(modelox4)
plot(ConteudoCelular~cv)
dados$cv<-as.numeric(dados$cv)
plot(ConteudoCelular~cv)

Tk5<-TukeyHSD(anova5)
Tk5
modelox5<-lm(PMSkg.ha~cv,data=dados)
plot(modelox5)
plot(PMSkg.ha~cv)
dados$cv<-as.numeric(dados$cv)
plot(PMSkg.ha~cv)

Tk6<-TukeyHSD(anova6)
Tk6
modelox6<-lm(FDA~cv,data=dados)
plot(modelox6)
plot(FDA~cv)
dados$cv<-as.numeric(dados$cv)
plot(FDA~cv)

Tk7<-TukeyHSD(anova7)
Tk7
modelox7<-lm(MateriaMineral~cv,data=dados)
plot(modelox7)
plot(MateriaMineral~cv)
dados$cv<-as.numeric(dados$cv)
plot(MateriaMineral~cv)

Tk8<-TukeyHSD(anova8)
Tk8
modelox8<-lm(Altmed~cv,data=dados)
plot(modelox8)
plot(Altmed~cv)
dados$cv<-as.numeric(dados$cv)
plot(Altmed~cv)

Tk9<-TukeyHSD(anova9)
Tk9
modelox9<-lm(FDN~cv,data=dados)
plot(modelox9)
plot(FDN~cv)
dados$cv<-as.numeric(dados$cv)
plot(FDN~cv)

#FAZER TODOS DADOS RODAREM




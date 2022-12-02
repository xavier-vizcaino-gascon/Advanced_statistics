#This is a R markdown file embedded in R file
---
title: "A2 - Anàlitica descriptiva i inferencial"
author: "Xavier Vizcaino Gascon"
date: '`r format(Sys.Date(),"%e de %B, %Y")`'
output:
  pdf_document: 
    toc: yes
    toc_depth: 2
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r load_libraries, include=FALSE}
library(knitr)
library(dplyr)
library(ggplot2)
library(gridExtra)
```

```{r general, include=FALSE}
mypalette<-c("darkorange","darkorange1","darkorange2","darkorange3","darkorange4" )
```


## 1. Lectura del fitxer i preparació de les dades

*Llegiu el fitxer CensusIncome_clean.csv i guardeu les dades en un objecte amb identificador denominat cens. A continuació, verifiqueu que les dades s'han carregat correctament.*

Carreguem el fitxer de dades amb la següent comanda i generem un *dataset* que anomenarem **cens**:

```{r ex1_1_carrega}
cens <- read.csv("CensusIncome_clean.csv", stringsAsFactors=TRUE)
```

S'utilitza *read.csv* ja que el separador és la coma (,). També s'utilitza la opció *stringsAsFactors=TRUE*, doncs permet tenir una primera visió dels *strings* repetits en diferents registres.

Examinem el tipus de dades amb que R ha interpretat cada variable, per fer-ho apliquem a través de *sapply()* la funció *class()* en tot el *dataset*.

```{r ex1_2_examen1}
sapply(cens,class)
```
\newpage
A continuació examinem els valors resum, de cada tipus de variable amb la funció *summary()* aplicada a tot el *dataset*:

```{r ex1_3_examen2}
summary(cens)
```
Observem que només la variable *CS_ID* té un nombre elevat de *levels*, indicatiu que aquest tipus no és el més adequat i hauriem de canviar el tipus de variable a *char*.

```{r ex1_4_modificacions}
#Modificació
cens$CS_ID<-as.character(cens$CS_ID)

#Comprovació
class(cens$CS_ID)
summary(cens$CS_ID)
```
\newpage
## 2. Edat

*Calculeu l'interval de confiança de la mitjana d'edat. Seguiu els passos que s'especifiquen a continuació.*

### 2.1. Distribució d’edats

*Visualitzeu gràficament la distribució de l'edat. Escolliu el gràfic que sigui més apropiat, considerant que es vol conèixer la distribució de la variable i si aquesta segueix una distribució normal.*

Escollim un histograma i un gràfic QQ-plot per validar si les dades segueixen una distribució normal

```{r ex2_1_grafics}
par(mfrow=c(1,2))
hist(cens$age, main="Histograma Edat", col = mypalette[1])
qqnorm(cens$age, main="QQ-plot Edat", col = mypalette[1])
```

### 2.2. Normalitat

*Podem assumir normalitat per calcular l'interval de confiança de la mitjana d'edat? Justifiqueu la vostra resposta.*

Com es pot observar en el gràfic QQ-plot, els registres segueixen una línia completament recta en l'interval entre 25 i 75 anys; per tant podem assumir que les dades segueixen una distribució normal en aquest interval.
Tanmateix, com es pot observar en l'histograma, la freqüència més alta la trobem en el rang entre 30 i 35 anys, per tant podem deduir que la mitjana d'edat es trobarà dins de l'interval en que les dades tenen una distribució normal. Així doncs, podem assumir normalitat per a calcular l'interval de confiança de la mitjana d'edat.

### 2.3. Interval de confiança

*Calculeu manualment l'interval de confiança de la mitjana de la variable age. Per fer-ho, definiu una funció IC que rebi la variable, la confiança, i que retorni un vector amb els valors de l'interval de confiança.*

Funció IC per a calcular l'interval de confiança:

```{r ex2_3_registres}

IC<-function(x, NC){
  alfa<-1-NC
  sd<-sd(x)
  n<-length(x)
  SE<-sd/sqrt(n)
  # Distribució t-student doncs no coneixem la variança poblacional
  z<-qt(alfa/2, df=n-1, lower.tail = FALSE)
  L<-mean(x)-z*SE
  U<-mean(x)+z*SE
  round(c(L,U),3)
}
```

### 2.4. Càlculs

*Calculeu l'interval de confiança al 90% i 95 %. Compareu els resultats.*

Apliquem la funció definida anteriorment a la variable *cens$age* amb els nivells de confiança sol·licitats:

```{r ex2_4_modificacions2}
R2a<-IC(cens$age, 0.90)
R2b<-IC(cens$age, 0.95)
R2a; R2b
```

L'interval de confiança al 90% és [`r R2a`] i l'interval de confiança al 95% és [`r R2b`]. A partir d'aquests resultats es pot observar que un increment en el nivell de confiança, comporta un increment en l'amplitud de l'interval de confiança calculat.

### 2.5. Interpretació

*Expliqueu com s'interpreta l'interval de confiança a partir dels resultats obtinguts.*

La interpretació dels resultats indica que el NC% de les mostres aleatòries obtingudes de la població donen lloc a un interval que conté el valor real de la mitjana poblacional. Anant al cas concret del primer càlcul, podem afirmar que en el cas que obtinguéssim infinites mostres de la població, el **90%** de les mostres, contindrien el valor real de la **mitjana poblacional** en l'interval [`r R2a`].

## 3. Salari

*Ara investigarem el salari de la població. En particular, ens preguntem si en promig, el salari de les persones Self-Employed és inferior al de la resta de modalitats. Seguiu els passos que s'especifiquen a continuació.*

### 3.1. Pregunta de recerca

*Formuleu la pregunta de recerca.*

La mitja de salari de les persones de la categoria *Self-Employed* és inferior a la resta de modalitats?

\newpage

Considerarem dues opcions (o definicions més precises) de la pregunta de recerca:

* **Opció 1**: La mitja de salari de les persones de la categoria *Self-Employed* és inferior a la mitja de salaris del complementari, és a dir, de les persones *no Self-Employed*?

* **Opció 2**: La mitja de salari de les persones de la categoria *Self-Employed* és inferior a la mitja de salaris de les persones de cada una de les altres categories (*Government*, *Other/Unknown*, *Private*) ?

### 3.2. Hipòtesi

*Escriviu les hipòtesis (hipòtesi nul·la i hipòtesi alternativa).*

$$H_{0}: \mu_{self}=\mu_{i}$$
$$H_{1}: \mu_{self}<\mu_{i}$$
On $\mu_{i}$ fa referència a:

* La mitja de la mostra *no Self-Empoyed* en la **opció 1**.

* La mitja de la mostra per cada categoria (*Government*, *Other/Unknown*, *Private*) en la **opció 2**.

### 3.3. Test a aplicar

*Expliqueu quin tipus de test podem aplicar atesa la pregunta de recerca plantejada i les característiques de la mostra. Justifiqueu la vostra elecció.*

El test a aplicar és per a **dues mostres independents, sobre la mitjana amb variàncies desconegudes**.

En aquest moment, però, no sabem si les variàncies son desconegudes però iguals o bé, desconegudes i diferents. Per aquest motiu, aplicarem inicialment un test d'**igualtat de variàncies**

**Test d'igualtat de variàncies**

$$H_{0}: \sigma_{self}^{2}=\sigma_{i}^{2}$$
$$H_{1}: \sigma_{self}^{2}\neq\sigma_{i}^{2}$$

Funció var_test per a calcular el test de variança:

```{r test de variances}
var_test<-function(m1, m2, NC){
  alfa<-1-NC
  meanX<-mean(m1); meanY<-mean(m2)
  nX<-length(m1); nY<-length(m2)
  sX<-sd(m1); sY<-sd(m2)
  
  fobs<-sX^2/sY^2
  fcritL<-qf(alfa/2, df1 = nX-1, df2 = nY-2)
  fcritU<-qf(1-alfa/2, df1 = nX-1, df2 = nY-2)
  pvalue<-2*min(pf(fobs, df1 = nX-1, df2 = nY-2, lower.tail = FALSE),
              pf(fobs, df1 = nX-1, df2 = nY-2))
  return(data.frame(fobs, fcritL, fcritU, pvalue, nX, nY))
}
```

\newpage

**Opció 1**

$$H_{0}: \sigma_{self}^{2}=\sigma_{no\_self}^{2}$$
$$H_{1}: \sigma_{self}^{2}\neq\sigma_{no\_self}^{2}$$

Aplicació de la funció definida anteriorment:

```{r}
self<-cens$income[cens$workclass=="Self-Employed"]
no_self<-cens$income[!cens$workclass=="Self-Employed"]

r3a<-var_test(self,no_self, 0.95)
r3a
```

Graficant els valors resultat obtenim:

```{r echo=FALSE, fig.height=3, fig.width=4, fig.align='center'}
custom<-function(x) {df(x, df1 = r3a$nX-1, df2 = r3a$nY-2)}

ggplot(data=data.frame(x=c(0.5, 1.5)), aes(x))+
  stat_function(fun = custom, n=10000)+
  geom_vline(data=r3a, aes(xintercept=fcritL), linetype = "dashed") +
  geom_vline(data=r3a, aes(xintercept=fcritU), linetype = "dashed") +
  geom_vline(data=r3a, aes(xintercept=fobs), linetype = "solid", color="red") +
  geom_rect(data=r3a, aes(NULL, NULL, xmin=fcritL, xmax=fcritU), ymin=-0.2, ymax=0, color="black", fill="orange") +
  labs(title = "Test de variança", subtitle = "Var1 = Self\nVar2 = no_Self")
```

**NOTA GENERAL** 

En el transcurs de l'informe, sempre que es representi gràficament el resultat d'un test trobarem:

* La distribució utilitzada en el càlcul, en negre sòlid. 

* El valor o valors crítics, depenent si es un test unilateral o bilateral, representats amb una o dues línies verticals discontinues ($f_{critL}$, $f_{critU}$).

* L'interval d'acceptació d'$H_0$ marcat amb un petit rectangle taronja.

* El valor observat ($Z_{obs}$) representat amb una línia en vermell.

Així doncs, en el cas concret del test de variança obtenim tant numèricament com gràficament, que $f_{obs}$ està fora de l'interval d'acceptació d'$H_0$, anàlogament, el valor p es inferior al nivell de significança; per tant rebutjem la hipòtesi nul·la en favor de l'alternativa i conseqüentment podem dir que les variàncies son diferents.

\newpage

**Opció 2**

$$H_{0}: \sigma_{self}^{2}=\sigma_{worklass_i}^{2}$$
$$H_{1}: \sigma_{self}^{2}\neq\sigma_{worklass_i}^{2}$$

Aplicació de la funció definida anteriorment:

```{r}
gov<-cens$income[cens$workclass=="Government"]
priv<-cens$income[cens$workclass=="Private"]
unk<-cens$income[cens$workclass=="Other/Unknown"]

r3b1<-var_test(self,gov,0.95)
r3b2<-var_test(self,priv,0.95)
r3b3<-var_test(self,unk, 0.95)
r3b1; r3b2; r3b3
```

Gràficament:

```{r echo=FALSE, fig.height=3}
custom<-function(x) {df(x, df1 = r3b1$nX-1, df2 = r3b1$nY-2)}

g1<-ggplot(data=data.frame(x=c(0.5, 1.5)), aes(x))+
  stat_function(fun = custom, n=10000)+
  geom_vline(data=r3b1, aes(xintercept=fcritL), linetype = "dashed") +
  geom_vline(data=r3b1, aes(xintercept=fcritU), linetype = "dashed") +
  geom_vline(data=r3b1, aes(xintercept=fobs), linetype = "solid", color="red") +
  geom_rect(data=r3b1, aes(NULL, NULL, xmin=fcritL, xmax=fcritU), ymin=-0.2, ymax=0, color="black", fill="orange") +
  labs(title = "Test de variança", subtitle = "Var1 = Self\nVar2 = Gov")

custom<-function(x) {df(x, df1 = r3b2$nX-1, df2 = r3b2$nY-2)}

g2<-ggplot(data=data.frame(x=c(0.5, 1.5)), aes(x))+
  stat_function(fun = custom, n=10000)+
  geom_vline(data=r3b2, aes(xintercept=fcritL), linetype = "dashed") +
  geom_vline(data=r3b2, aes(xintercept=fcritU), linetype = "dashed") +
  geom_vline(data=r3b2, aes(xintercept=fobs), linetype = "solid", color="red") +
  geom_rect(data=r3b2, aes(NULL, NULL, xmin=fcritL, xmax=fcritU), ymin=-0.2, ymax=0, color="black", fill="orange") +
  labs(title = "", subtitle = "Var1 = Self\nVar2 = Priv")

custom<-function(x) {df(x, df1 = r3b3$nX-1, df2 = r3b3$nY-2)}

g3<-ggplot(data=data.frame(x=c(0.5, 1.5)), aes(x))+
  stat_function(fun = custom, n=10000)+
  geom_vline(data=r3b3, aes(xintercept=fcritL), linetype = "dashed") +
  geom_vline(data=r3b3, aes(xintercept=fcritU), linetype = "dashed") +
  geom_vline(data=r3b3, aes(xintercept=fobs), linetype = "solid", color="red") +
  geom_rect(data=r3b3, aes(NULL, NULL, xmin=fcritL, xmax=fcritU), ymin=-0.2, ymax=0, color="black", fill="orange") +
  labs(title = "", subtitle = "Var1 = Self\nVar2 = Unk")

grid.arrange(g1,g2,g3,ncol=3)
```
En aquest cas, també observem que la $f_{obs}$ està fora de l'interval d'acceptació d'$H_0$, i que el valor p és inferior al nivell de significança en tots els casos d'estudi; per tant rebutjem la hipòtesi nul·la en favor de l'alternativa. I per tant, confirmem que en tots els casos les variàncies son diferents.

Finalment podem concloure que el test a aplicar es per a **dues mostres independents, sobre la mitjana amb variàncies desconegudes i diferents**.

\newpage

### 3.4. Càlcul

*Calculeu el test usant una funció pròpia. Implementeu una funció que realitzi el càlcul del test i que pugueu utilitzar amb diferents valors de nivell de confiança.*
*Calculeu el test per a un nivell de confiança del 95% i del 90 %. Mostreu els resultats (valor observat, crític i valor p) en una taula.*

Funció my_test_1 per a calcular el test sobre la mitjana de dues mostres independents amb variances desconegudes i independents:

```{r funcio}
my_test_1<-function(m1, m2, NC){
  
  alfa<-1-NC
  meanX<-mean(m1); meanY<-mean(m2)
  nX<-length(m1); nY<-length(m2)
  sX<-sd(m1); sY<-sd(m2)
  
  v<-((((sX^2)/nX)+((sY^2)/nY))^2)/(((((sX^2)/nX)^2)/(nX-1))+((((sY^2)/nY)^2)/(nY-1)))
  
  tobs<-(meanX-meanY)/sqrt((sX^2/nX)+(sY^2/nY))
  
  tcritL<-qt(alfa, v)
  tcritU<-"INF"
  pvalue<-pt(tobs, df = v, lower.tail = TRUE)
  
  tobs<-round(tobs,4)
  tcritL<-round(tcritL,4)
  pvalue<-round(pvalue,4)
  
  return(data.frame(tobs,tcritL,tcritU,pvalue,v))
}
```

**Opció 1**

Aplicació de la funció definida anteriorment:

```{r}
R3a1<-my_test_1(self, no_self, 0.95)
R3a2<-my_test_1(self, no_self, 0.90)
R3a<-rbind(R3a1,R3a2)
rownames(R3a)<-c("NC=95%", "NC=90%")
kable(R3a)
```

```{r echo=FALSE, fig.height=3}
custom<-function(x) {dt(x, df = R3a1$v)}

g1<-ggplot(data=data.frame(x=c(-10, 10)), aes(x))+
  stat_function(fun = custom, n=10000)+
  geom_vline(data=R3a1, aes(xintercept=tcritL), linetype = "solid") +
  geom_vline(data=R3a1, aes(xintercept=tobs), linetype = "solid", color="red") +
  geom_rect(data=R3a1, aes(NULL, NULL, xmin=tcritL, xmax=10), ymin=-0.0125, ymax=0, color="black", fill="orange") +
  labs(title = "Test sobre la mitjana", subtitle = "Var1 = Self\nVar2 = no Self\nNC = 95%")

custom<-function(x) {dt(x, df = R3a2$v)}

g2<-ggplot(data=data.frame(x=c(-10, 10)), aes(x))+
  stat_function(fun = custom, n=10000)+
  geom_vline(data=R3a2, aes(xintercept=tcritL), linetype = "solid") +
  geom_vline(data=R3a2, aes(xintercept=tobs), linetype = "solid", color="red") +
  geom_rect(data=R3a2, aes(NULL, NULL, xmin=tcritL, xmax=10), ymin=-0.0125, ymax=0, color="black", fill="orange") +
  labs(title = "", subtitle = "Var1 = Self\nVar2 = no Self\nNC = 90%")

grid.arrange(g1,g2,ncol=2)
```


**Opció 2**

Aplicació de la funció definida anteriorment:

```{r calcul}
R3b1<-my_test_1(self,gov,0.95)
R3b2<-my_test_1(self,gov,0.90)

R3b3<-my_test_1(self,priv,0.95)
R3b4<-my_test_1(self,priv, 0.90)

R3b5<-my_test_1(self,unk,0.95)
R3b6<-my_test_1(self,unk, 0.90)

R3b<-rbind(R3b1,R3b2,R3b3,R3b4,R3b5,R3b6)
rownames(R3b)<-c("Gov NC=95%", "Gov NC=90%", "Priv NC=95%", "Priv NC=90%", 
                 "Unk NC=95%", "Unk NC=90%")
kable(R3b)

```

```{r echo=FALSE, fig.height=8}
custom<-function(x) {dt(x, df = R3b1$v)}

g1<-ggplot(data=data.frame(x=c(-40, 40)), aes(x))+
  stat_function(fun = custom, n=10000)+
  geom_vline(data=R3b1, aes(xintercept=tcritL), linetype = "solid") +
  geom_vline(data=R3b1, aes(xintercept=tobs), linetype = "solid", color="red") +
  geom_rect(data=R3b1, aes(NULL, NULL, xmin=tcritL, xmax=40), ymin=-0.0125, ymax=0, color="black", fill="orange") +
  coord_cartesian(xlim = c(-30,30)) +
  labs(title = "Test sobre la mitjana", subtitle = "Var1 = Self\nVar2 = Gov\nNC = 95%")

custom<-function(x) {dt(x, df = R3b2$v)}

g2<-ggplot(data=data.frame(x=c(-40, 40)), aes(x))+
  stat_function(fun = custom, n=10000)+
  geom_vline(data=R3b2, aes(xintercept=tcritL), linetype = "solid") +
  geom_vline(data=R3b2, aes(xintercept=tobs), linetype = "solid", color="red") +
  geom_rect(data=R3b2, aes(NULL, NULL, xmin=tcritL, xmax=40), ymin=-0.0125, ymax=0, color="black", fill="orange") +
  coord_cartesian(xlim = c(-30,30)) +
  labs(title = "", subtitle = "Var1 = Self\nVar2 = Gov\nNC = 90%")

custom<-function(x) {dt(x, df = R3b3$v)}

g3<-ggplot(data=data.frame(x=c(-20, 20)), aes(x))+
  stat_function(fun = custom, n=10000)+
  geom_vline(data=R3b3, aes(xintercept=tcritL), linetype = "solid") +
  geom_vline(data=R3b3, aes(xintercept=tobs), linetype = "solid", color="red") +
  geom_rect(data=R3b3, aes(NULL, NULL, xmin=tcritL, xmax=20), ymin=-0.0125, ymax=0, color="black", fill="orange") +
  coord_cartesian(xlim = c(-10,10)) +
  labs(title = "", subtitle = "Var1 = Self\nVar2 = Priv\nNC = 95%")

custom<-function(x) {dt(x, df = R3b4$v)}

g4<-ggplot(data=data.frame(x=c(-20, 20)), aes(x))+
  stat_function(fun = custom, n=10000)+
  geom_vline(data=R3b4, aes(xintercept=tcritL), linetype = "solid") +
  geom_vline(data=R3b4, aes(xintercept=tobs), linetype = "solid", color="red") +
  geom_rect(data=R3b4, aes(NULL, NULL, xmin=tcritL, xmax=20), ymin=-0.0125, ymax=0, color="black", fill="orange") +
  coord_cartesian(xlim = c(-10,10)) +
  labs(title = "", subtitle = "Var1 = Self\nVar2 = Priv\nNC = 90%")

custom<-function(x) {dt(x, df = R3b5$v)}

g5<-ggplot(data=data.frame(x=c(-60, 60)), aes(x))+
  stat_function(fun = custom, n=10000)+
  geom_vline(data=R3b5, aes(xintercept=tcritL), linetype = "solid") +
  geom_vline(data=R3b5, aes(xintercept=tobs), linetype = "solid", color="red") +
  geom_rect(data=R3b5, aes(NULL, NULL, xmin=tcritL, xmax=60), ymin=-0.0125, ymax=0, color="black", fill="orange") +
  coord_cartesian(xlim = c(-50,50)) +
  labs(title = "", subtitle = "Var1 = Self\nVar2 = Unk\nNC = 95%")

custom<-function(x) {dt(x, df = R3b6$v)}

g6<-ggplot(data=data.frame(x=c(-60, 60)), aes(x))+
  stat_function(fun = custom, n=10000)+
  geom_vline(data=R3b6, aes(xintercept=tcritL), linetype = "solid") +
  geom_vline(data=R3b6, aes(xintercept=tobs), linetype = "solid", color="red") +
  geom_rect(data=R3b6, aes(NULL, NULL, xmin=tcritL, xmax=60), ymin=-0.0125, ymax=0, color="black", fill="orange") +
  coord_cartesian(xlim = c(-50,50)) +
  labs(title = "", subtitle = "Var1 = Self\nVar2 = Unk\nNC = 90%")

grid.arrange(g1,g2,g3,g4,g5,g6,ncol=2)
```

\newpage

### 3.5. Conclusió

*A partir dels resultats obtinguts, doneu resposta a la pregunta de recerca.*

**Opció 1**

Amb els resultats obtinguts, podem concloure que a la població, la mitja de salari de les persones de la categoria *Self-Employed* **no** és inferior a la mitja de salari de les persones *no Self-Employed* amb un nivell de confiança del 90 o 95%, donat que la $t_{obs}$ està dins de l'interval d'acceptació d'$H_0$. Exemple al 95% $t_{obs}$=`r R3a1$tobs`, interval d'acceptació d'$H_0$=[`r c(R3a1$tcritL, R3a1$tcritU)`].

**Opció 2**

1. En la població, la mitja de salari de les persones de la categoria *Self-Employed* **si** és inferior a la mitja de salari de les persones *Goverment* amb un nivell de confiança del 90 i del 95%, donat que la $t_{obs}$ està fora de l'interval d'acceptació d'$H_0$ i per tant podem rebutjar la hipòtesi nul·la en favor de l'alternativa. Exemple al 95% $t_{obs}$=`r R3b1$tobs`, interval d'acceptació d'$H_0$=[`r c(R3b1$tcritL, R3b1$tcritU)`].

2. En la població, la mitja de salari de les persones de la categoria *Self-Employed* **no** és inferior a la mitja de salari de les persones *Private* amb un nivell de confiança del 90 i del 95%, donat que la $t_{obs}$ està dins de l'interval d'acceptació d'$H_0$. Exemple al 95% $t_{obs}$=`r R3b3$tobs`, interval d'acceptació d'$H_0$=[`r c(R3b3$tcritL, R3b3$tcritU)`].

3. En la població, la mitja de salari de les persones de la categoria *Self-Employed* **no** és inferior a la mitja de salari de les persones *Other/Unknown* amb un nivell de confiança del 90 i del 95%, donat que la $t_{obs}$ està dins de l'interval d'acceptació d'$H_0$. Exemple al 95% $t_{obs}$=`r R3b5$tobs`, interval d'acceptació d'$H_0$=[`r c(R3b5$tcritL, R3b5$tcritU)`].

## 4. Proporció de Self-Employed

*Ens preguntem si el percentatge de Self-Employed a la població és superior al 10 %. Apliqueu el test necessari per donar resposta a aquesta pregunta. Seguiu els passos que s'indiquen a continuació.*

### 4.1. Pregunta

*Formuleu la pregunta de recerca que es planteja en aquesta secció.*

El percentatge de persones *Self-Employed* a la població és superior al 10%?

### 4.2. Hipòtesi

*Escriviu les hipòtesis (hipòtesi nul·la i hipòtesi alternativa).*

$$H_{0}: p_{self}=0.1$$
$$H_{1}: p_{self}>0.1$$

### 4.3. Anàlisi visual

*Representeu de forma gràfica la proporció de Self-Employed a la mostra.*

Representem les proporcions a la mostra en un diagrama de sectors.

```{r fig.height=4}
cens$Self_Employed<-as.factor(ifelse(cens$workclass=="Self-Employed",
                                     "Self-Employed","no_Self-Employed"))
par(mfrow=c(1,2))
pie(summary(cens$Self_Employed),main = "workclass", col = mypalette, cex=0.7)
pie(summary(cens$workclass),main = "workclass detallat", col = mypalette, cex=0.7)
```

### 4.4. Contrast

*Expliqueu quin tipus de contrast podem aplicar atesa la pregunta de recerca plantejada i les característiques de la mostra. Justifiqueu la vostra elecció.*

Donada la hipòtesi alternativa, es tracta d'un test unilateral per la dreta d'una mostra sobre la proporció, l'estadístic de contrast sota la hipòtesi nul·la és:

$$
z=\frac{\hat{p}-p_0}{\sqrt{\frac{p_0(1-p_0)}{n}}}\sim N(0,1)
$$

### 4.5. Càlcul

*Calculeu el test usant una funció pròpia. Podeu crear una funció que rebi els paràmetres necessaris i el nivell de confiança. Després, calculeu el test, cridant aquesta funció amb nivell de confiança del 95 %. Mostreu els resultats (valor observat, crític i valor p) en una taula.*

Funció my_test_2 per a calcular el test d'una mostra sobre la proporció:

```{r}
my_test_2<-function(m, cat, p, NC){
  
  alfa<-1-NC
  n<-length(m)
  pm<-length(m[m==cat])/n

  zobs<-(pm-p)/sqrt((p*(1-p))/n)
  
  zcritL<-"-INF"
  zcritU<-qnorm(1-alfa)
  pvalue<-pnorm(zobs, lower.tail = FALSE)
  
  zobs<-round(zobs,4)
  zcritU<-round(zcritU,4)
  pvalue<-round(pvalue,4)
  
  return(data.frame(zobs, zcritL, zcritU, pvalue))
}
```

Aplicació de la funció definida anteriorment:

```{r}
R4<-my_test_2(cens$workclass, "Self-Employed", 0.1, 0.95)
kable(R4)
```

```{r echo=FALSE, fig.height=3, fig.width=4, fig.align='center'}
custom<-function(x) {dnorm(x)}

g1<-ggplot(data=data.frame(x=c(-20, 20)), aes(x))+
  stat_function(fun = custom, n=10000)+
  geom_vline(data=R4, aes(xintercept=zcritU), linetype = "solid") +
  geom_vline(data=R4, aes(xintercept=zobs), linetype = "solid", color="red") +
  geom_rect(data=R4, aes(NULL, NULL, xmin=-20, xmax=zcritU), ymin=-0.0125, ymax=0, color="black", fill="orange") +
  coord_cartesian(xlim = c(-10,10)) +
  labs(title = "Test sobre la proporció", subtitle = "Var1 = Self\np = 0.1\nNC = 95%")
g1
```


### 4.6. Conclusió

*A partir dels resultats obtinguts, doneu resposta a la pregunta de recerca.*

Obtenim una $z_{obs}$=`r R4$zobs` fora de l'interval d'acceptació d'$H_0$=[`r c(R4$zcritL,R4$zcritU)`], anàlogament obtenim un valor p=`r R4$pvalue` molt inferior al nivell de significança ($\alpha$=0.05); per tant podem rebutjar la hipòtesi nul·la en favor de l'alternativa i afirmar que el percentatge de persones *Self-Employed* a la població es superior al 10%. 

## 5. Proporció de Self-Employed en dones i homes

*Ens preguntem si la proporció de Self-Employed és menor entre les dones que entre els homes en la població. Per donar resposta a aquesta pregunta, seguiu els passos que s’indiquen a continuació.*

### 5.1. Pregunta de recerca

*Formuleu la pregunta de recerca que es planteja en aquesta secció.*

La proporció de persones *Self-Employed* és inferior entre dones que entre homes?

### 5.2. Anàlisi visual

*Representeu de forma gràfica la proporció de Self-Employed a la mostra d'homes i dones respectivament.*

```{r fig.height=4}
censM<-cens[cens$gender=="m",]
censF<-cens[cens$gender=="f",]

censM$Self_Employed<-as.factor(ifelse(censM$workclass=="Self-Employed",
                                      "Self-Employed","no_Self-Employed"))
censF$Self_Employed<-as.factor(ifelse(censF$workclass=="Self-Employed",
                                      "Self-Employed","no_Self-Employed"))
par(mfrow=c(1,2))
pie(summary(censM$Self_Employed),main = "w_class_Homes", col = mypalette, cex=0.7)
pie(summary(censF$Self_Employed),main = "w_class_Dones", col = mypalette, cex=0.7)
```

### 5.3. Hipòtesi

*Escriviu la hipòtesi nul·la i la hipòtesi alternativa.*

$$
H_{0}: p^{dones}_{self}=p^{homes}_{self}
$$
$$
H_{1}: p^{dones}_{self}<p^{homes}_{self}
$$

### 5.4. Test

*Expliqueu quin tipus de test podem aplicar atesa la pregunta de recerca plantejada i les característiques de la mostra. Justifiqueu la vostra elecció.*

Es tracta d'un test unilateral per l'esquerra de dues mostres independents (homes i dones) sobre la proporció.

### 5.5. Càlcul

*Calculeu el test usant una funció pròpia. Igual que en apartats anteriors, es recomana definir una funció que faci el càlcul i que rebi els paràmetres necessaris.*
*Calculeu el contrast per a un nivell de confiança del 97 %. Mostreu els resultats (valor observat, crític i valor p) en una taula.*

Funció my_test_3 per a calcular el test de dues mostres independents sobre la proporció:

```{r}
my_test_3<-function(m1, m2, cat, NC){
  
  alfa<-1-NC
  n1<-length(m1); n2<-length(m2)
  pm1<-length(m1[m1==cat])/n1; pm2<-length(m2[m2==cat])/n2;
  p<-((n1*pm1)+(n2*pm2))/(n1+n2)

  zobs<-(pm1-pm2)/sqrt(p*(1-p)*((1/n1)+(1/n2)))
  
  zcritL<-qnorm(alfa)
  zcritU<-"INF"
  pvalue<-pnorm(zobs, lower.tail = TRUE)
  
  zobs<-round(zobs,4)
  zcritL<-round(zcritL,4)
  pvalue<-round(pvalue,4)
  
  return(data.frame(zobs, zcritL, zcritU, pvalue))
}
```

Aplicació de la funció definida anteriorment:

```{r}
R5<-my_test_3(censF$workclass,censM$workclass,"Self-Employed", 0.97)
kable(R5)
```

```{r echo=FALSE, fig.height=3, fig.width=4, fig.align='center'}
custom<-function(x) {dnorm(x)}

g1<-ggplot(data=data.frame(x=c(-40, 40)), aes(x))+
  stat_function(fun = custom, n=10000)+
  geom_vline(data=R5, aes(xintercept=zcritL), linetype = "solid") +
  geom_vline(data=R5, aes(xintercept=zobs), linetype = "solid", color="red") +
  geom_rect(data=R5, aes(NULL, NULL, xmin=zcritL, xmax=40), ymin=-0.0125, ymax=0, color="black", fill="orange") +
  coord_cartesian(xlim = c(-30,30)) +
  labs(title = "Test sobre la proporció de dues mostres", subtitle = "Var1 = Self\nm1 = homes\nm2 = dones\nNC = 97%")
g1
```


### 5.6. Conclusió

*A partir dels resultats obtinguts, proporcioneu una resposta a la pregunta de recerca.*

A partir dels càlculs anteriors obtenim una $z_{obs}$=`r R5$zobs` fora de l'interval d'acceptació d'$H_0$=[`r c(R5$zcritL,R5$zcritU)`] i un valor p=`r R5$pvalue` molt inferior al nivell de significança ($\alpha$=0.03), per tant podem rebutjar la hipòtesi nul·la en favor de l'alternativa i afirmar que el percentatge de persones *Self-Employed* és menor entre les dones que entre els homes en la població.

## 6. Dependència Gènere - Self-Employed

*En aquesta secció es demana aplicar el test d´independència Chi quadrat per avaluar si les variables gènere i Self-Employed són independents. Seguiu els passos que s'indiquen a continuació..*

### 6.1. Pregunta de recerca

Les variables *gender* i *workclass==Self-Employed* estan relacionades o son independents?

### 6.2. Hipòtesi

*Escriviu la hipòtesi nul·la i alternativa.*

* $H_0$: Les variables *gender* i *workclass==Self-Employed* són independents

* $H_1$: Hi ha una relació entre les variables *gender* i *workclass==Self-Employed*

### 6.3. Test

*Descriviu breument en què consisteix el test Chi quadrat. Calculeu la matriu de contingència i mostreu-ne els valors.*

El test Chi quadrat tracta de comparar les freqüències esperades si les variables no estiguessin relacionades, amb les obtingudes de la mostra. Quan les freqüències observades són molt diferent a les esperades, podem concloure que hi ha una relació entre les variables.

Càlcul de la matriu de contingència:

```{r}
tc<-table(cens$gender,cens$Self_Employed)
tc<-cbind(tc,rowSums(tc))
colnames(tc)[3]<-"sum_row"
tc<-rbind(tc,colSums(tc))
rownames(tc)[3]<-"sum_col"
kable(tc)
```

\newpage

### 6.4. Càlcul

*Realitzeu els càlculs del test Chi quadrat, implementant una funció pròpia. Calculeu el contrast per a un nivell de confiança de 97 %.*

Funció my_test_4 per a calcular el test d'independència de dues variables:

```{r}
my_test_4<-function(x, y, NC){
  alfa<-1-NC
  #taula de contingència
  tc<-table(x,y)
  tc<-cbind(tc,rowSums(tc))
  colnames(tc)[ncol(tc)]<-"sum_row"
  tc<-rbind(tc,colSums(tc))
  rownames(tc)[nrow(tc)]<-"sum_col"
  #valors esperats
  te<-matrix(1,2,2)
  for (i in 1:nrow(te)){
    for (j in 1:ncol(te)){
      te[i,j]<-round((tc[i,3]*tc[3,j])/tc[3,3],2)
    }
  }
  df<-(nrow(te)-1)*(ncol(te)-1)
  
  chisqobs<-(sum(((tc[1:2,1:2]-te)^2)/te))
  
  chicritL<-"-INF"
  chicritU<-qchisq(1-alfa, df, lower.tail = FALSE)
  pvalue<-pchisq(chisqobs,df, lower.tail = FALSE)
  
  return(data.frame(chisqobs, chicritL, chicritU, pvalue, df))
}
```

Aplicació de la funció definida anteriorment:

```{r}
R6<-my_test_4(cens$gender,cens$Self_Employed, 0.97)
kable(R6)
```

```{r echo=FALSE, fig.height=3, fig.width=4, fig.align='center'}
custom<-function(x) {dchisq(x, df=R6$df)}

g1<-ggplot(data=data.frame(x=c(-800, 800)), aes(x))+
  stat_function(fun = custom, n=100)+
  geom_vline(data=R6, aes(xintercept=chicritU), linetype = "solid") +
  geom_vline(data=R6, aes(xintercept=chisqobs), linetype = "solid", color="red") +
  geom_rect(data=R6, aes(NULL, NULL, xmin=-800, xmax=chicritU), ymin=-0.0125, ymax=0, color="black", fill="orange") +
  coord_cartesian(xlim = c(-250,700)) +
  labs(title = "Test d'independència", subtitle = "Var1 = Genere\nVar2 = Self\nNC = 97%")
g1
```


### 6.5. Conclusió

*Responeu la pregunta de recerca plantejada en aquest apartat. Relacioneu el resultat amb el contrast de la secció anterior, on es realitza un test sobre les proporcions.*

A partir dels càlculs anteriors obtenim una $\chi^2_{obs}$=`r R6$chisqobs` fora de l'interval d'acceptació d'$H_0$=[`r c(R6$chicritL,round(R6$chicritU,5))`] i un valor p=`r R6$pvalue` molt inferior al nivell de significança ($\alpha$=0.03), per tant podem rebutjar la hipòtesi nul·la en favor de l'alternativa i afirmar hi ha una relació entre les variables *gender* i *workclass==Self-Employed*. 

Aquest mateix fet l'hem trobat en l'exercici anterior quan hem corroborat que $p^{dones}_{self}<p^{homes}_{self}$, fet que indica que existeix una relació entre les variables *gender* i *workclass==Self-Employed*. Si no existís relació entre aquestes variables esperaríem que la proporció de persones *Self-Employed* fos la mateixa
tan per *gender=="f"* com per *gender=="m"*.

## 7. Resum i conclusions

*Presenteu una taula amb els resultats principals de cada secció: la pregunta de recerca plantejada, els valors obtinguts del contrast i la conclusió obtinguda a cada apartat.*

```{r echo=FALSE}
N="2a"
Pregunta="Interval de confiança de la mitjana d'edat al 95%"
Resultat=paste("[",round(R2b[1],2),",",round(R2b[2],2),"]", sep = "")
Conclusió=paste("L'interval de confiança de la mitjana d'edat al 95% és ",
                "[",round(R2b[1],2),",",round(R2b[2],2),"]", sep = "")
R7<-data.frame(N, Pregunta, Resultat, Conclusió)

R7[nrow(R7)+1,]<-c("2b", "Interval de confiança de la mitjana d'edat al 90%", 
                   paste("[",round(R2a[1],2),",",round(R2a[2],2),"]", sep = ""), 
                   paste("L'interval de confiança de la mitjana d'edat al 90% és ",
                "[",round(R2a[1],2),",",round(R2a[2],2),"]", sep = ""))
R7[nrow(R7)+1,]<-c("3a.1", "En promitg, el salari de Self-Employed és inferior a la resta (en conjunt) al 95%?", 
                   paste(R3a1$tobs, R3a1$tcritL, R3a1$tcritU, R3a1$pvalue, sep = ", "), 
                   "En promitg, el salari de Self-Employed NO és inferior a la resta (en conjunt) al 95%")
R7[nrow(R7)+1,]<-c("3a.2", "En promitg, el salari de Self-Employed és inferior a la resta (en conjunt) al 90%?", 
                   paste(R3a2$tobs, R3a2$tcritL, R3a2$tcritU, R3a2$pvalue, sep = ", "), 
                   "En promitg, el salari de Self-Employed NO és inferior a la resta (en conjunt) al 90%")
R7[nrow(R7)+1,]<-c("3b.1", "En promitg, el salari de Self-Employed és inferior a les Government al 95%?", 
                   paste(R3b1$tobs, R3b1$tcritL, R3b1$tcritU, R3b1$pvalue, sep = ", "), 
                   "En promitg, el salari de Self-Employed SI és inferior a les Government al 95%")
R7[nrow(R7)+1,]<-c("3b.2", "En promitg, el salari de Self-Employed és inferior a les Government al 90%?", 
                   paste(R3b2$tobs, R3b2$tcritL, R3b2$tcritU, R3b2$pvalue, sep = ", "), 
                   "En promitg, el salari de Self-Employed SI és inferior a les Government al 90%")
R7[nrow(R7)+1,]<-c("3b.3", "En promitg, el salari de Self-Employed és inferior a les Private al 95%?", 
                   paste(R3b3$tobs, R3b3$tcritL, R3b3$tcritU, R3b3$pvalue, sep = ", "), 
                   "En promitg, el salari de Self-Employed NO és inferior a les Private al 95%")
R7[nrow(R7)+1,]<-c("3b.4", "En promitg, el salari de Self-Employed és inferior a les Private al 90%?", 
                   paste(R3b4$tobs, R3b4$tcritL, R3b4$tcritU, R3b4$pvalue, sep = ", "), 
                   "En promitg, el salari de Self-Employed NO és inferior a les Private al 90%")
R7[nrow(R7)+1,]<-c("3b.5", "En promitg, el salari de Self-Employed és inferior a les Other/Unknown al 95%?", 
                   paste(R3b5$tobs, R3b5$tcritL, R3b5$tcritU, R3b5$pvalue, sep = ", "), 
                   "En promitg, el salari de Self-Employed NO és inferior a les Other/Unknown al 95%")
R7[nrow(R7)+1,]<-c("3b.6", "En promitg, el salari de Self-Employed és inferior a les Other/Unknown al 90%?", 
                   paste(R3b6$tobs, R3b6$tcritL, R3b6$tcritU, R3b6$pvalue, sep = ", "), 
                   "En promitg, el salari de Self-Employed NO és inferior a les Other/Unknown al 90%")
R7[nrow(R7)+1,]<-c("4", "El percentatge de persones Self-Employed a la població es superior al 10%?", 
                   paste(R4$zobs, R4$zcritL, R4$zcritU, R4$pvalue, sep = ", "), 
                   "SI, els percentatge de persones Self-Employed és superior al 10% en la població")
R7[nrow(R7)+1,]<-c("5", "La proporció de persones Self-EMployed és menor entre les dones que entre els homes a la població?",
                   paste(R5$zobs, R5$zcritL, R5$zcritU, R5$pvalue, sep = ", "), 
                   "SI, el percentatge de persones Self-Employed és menor entre les dones que entre els homes a la població")
R7[nrow(R7)+1,]<-c("6", "Hi ha una relació entre les variables Gender i Workclass==Self-Employed?", 
                   paste(round(R6$chisqobs,4), R6$chicritL, round(R6$chicritU,4), round(R6$pvalue,4), sep = ", "), 
                   "SI, existeix relació entre les variables Gender i Workclass==Self-Employed")

kable(R7)
```
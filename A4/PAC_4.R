#This is a R markdown file embedded in R file
---
title: "A4 - Anàlisi de la variància i repàs del curs"
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
library(tidyverse)
library(ggpubr)
library(cowplot)
library(stats)
library(agricolae)



library(knitr)
```

```{r general, include=FALSE}
mypalette<-c("#F8766D","#00BFC4","#7CAE00","#C77CFF","#D89000" )
```

## 1. Lectura del fitxer i preparació de les dades

*Llegiu el fitxer gpa.csv i guardeu les dades en un objecte denominat gpa. A continuació, verifiqueu el tipus de cada variable. Quines variables són de tipus numèric? Quines variables són de tipus qualitatiu?*

Llegim el fitxer amb l'opció *stringsAsFactors = TRUE* i posteriorment corregim el tipus de dades de la variable *tothors* a *char*.

```{r}
gpa <- read.csv("gpa.csv", stringsAsFactors=TRUE)
gpa$tothrs<-as.character(gpa$tothrs)
str(gpa)
```

### 1.1. Preparació de les dades

*La variable tothrs està classificada com a character. Per a poder treballar amb ella cal convertir-la en numèrica, eliminant el text “h” de les dades.*

Substituïm la "h" per "" i posteriorment convertim les dades a *integer*.

```{r}
gpa_orignal<-gpa
gpa$tothrs<-as.integer(sub("h","",gpa$tothrs))
str(gpa$tothrs)
```

### 1.2. Valors absents

*Comproveu quantes observacions tenen valors absents i traieu conclusions sobre com de preocupant és el problema de valors absents en aquestes dades.*

Obtenim les dimensions del *DataFrame* i estudiem quants NA tenim i en quines variables.

```{r}
dim (gpa)
sapply(gpa,function(x) sum(is.na(x)))
```

Observem que només tenim `r sum(is.na(gpa$colgpa))` NA i tots són en la variable *colgpa*. Si tenim present que el dataset té `r dim(gpa)[1]` registres; els NA representen tan sols el `r round((sum(is.na(gpa$colgpa))/dim(gpa)[1])*100,2)`% de les dades i conseqüentment arribem a la conclusió que el nombre de NAs no suposa un problema.

*Elimineu els valors absents del conjunt de dades. Denomineu al nou conjunt de dades ‘gpaclean‘.*

```{r}
gpa<-gpa[!is.na(gpa$colgpa),]
dim (gpa)
gpaclean<-gpa
```

### 1.3. Equivalència de la nota en lletres

*La variable colgpa conté la nota numèrica de l'estudiant. Creeu una variable categòrica anomenada gpaletter, que indiqui la nota en lletra de cada estudiant de la següent forma: A, de 3.50 a 4.00; B, de 2.50 a 3.49; C, de 1.50 a 2.49; D, de 0 a 1.49.*

```{r}
gpaclean$gpaletter<-as.factor(ifelse(gpaclean$colgpa<=1.49,"D",
                              ifelse(gpaclean$colgpa<=2.49,"C",
                              ifelse(gpaclean$colgpa<=3.49,"B",
                                                          "A"))))
summary(gpaclean$gpaletter)
```

## 2. Estadística descriptiva i visualització

### 2.1. Anàlisi descriptiva
*Realitzeu una anàlisi descriptiva numèrica de les dades (resumiu els valors de les variables numèriques i categòriques). Mostreu el nombre d'observacions i el nombre de variables.*

```{r}
summary(gpaclean)
```

### 2.2. Visualització

*Mostreu amb diversos diagrames de caixa (boxplot) la distribució de la variable ‘sat‘ segons la variable ‘female‘, segons ‘athlete‘, i segons ‘gpaletter‘.*

```{r echo=FALSE, fig.height=5.6}
g1<-ggplot(gpaclean, aes(x=female, y=sat, color=female)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 0))
g2<-ggplot(gpaclean, aes(x=athlete, y=sat, color=athlete)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 0))
g3<-ggplot(gpaclean, aes(x=gpaletter, y=sat, color=gpaletter)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 0))
ggarrange(g1,g2,g3,ncol=2, nrow = 2)
```

*Creeu una variable denominada ‘excelente‘ que indiqui si l'estudiant ha obtingut una A de nota mitjana al final del semestre. Aquesta nova variable s'ha de codificar com una variable dicotòmica que pren el valor 1 quan l'estudiant ha obtingut una A i el valor 0 en cas contrari.* 

```{r}
gpaclean$excelente<-as.factor(ifelse(gpaclean$gpaletter == "A", 1, 0))
summary(gpaclean$excelente)
```

*Dibuixeu un gràfic que mostri el percentatge d'estudiants excel·lents.*

```{r fig.height=4}
n<-summary(gpaclean$excelente)
p<-paste(round(prop.table(summary(gpaclean$excelente))*100,2), "%", sep = "")
t<-c("FALSE", "TRUE")

df<-data.frame(n,p,t)

ggplot(df, aes(x = "", y = n, fill = t)) +
  geom_col() +
  geom_label(aes(label = p),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  labs(title = "Excel·lents") +
  coord_polar(theta = "y") + 
  theme_void()
```

*Interpreteu els gràfics breument.*

* *sat* vs *female*: S'observen diferències en les medianes de *sat* segons la variable *female*. La mediana per al grup *female = False* (homes) és superior tot i que la dispersió, especialment per valors baixos de *sat* és superior.

* *sat* vs *athlete*: També s'observen diferències en les distribucions de *sat* segons la variable *athlete*. En general, la distribució per al grup *athlete = False* (no esportistes) té tots els valors (min, max, quartils i mediana) més elevats.

* *sat* vs *gpaletter*: S'observa un cert nivell d'ordre en les distribucions de *sat* segons la variable *gpaletter*. En general, tots els valors (min, max, quartils i mediana) estan endreçats seguint A > B > C > D. Més enllà d'aquesta afirmació general caldria destacar que el grup *gpaletter = C* podria presentar una dispersió superior a la resta de grups especialment per valors baixos de *sat*

* excel·lents: Només el `r p[2]` dels alumnes pertanyen al grup *excelente*.

## 3. Estadística inferencial

### 3.1. Interval de confiança de la mitjana poblacional de la variable sat

*Calculeu manualment l'interval de confiança al 95% de la mitjana poblacional de la variable sat dels estudiants. Per fer-ho, definiu una funció IC que rebi la variable, la confiança, i que retorni un vector amb els valors de l'interval de confiança.*

```{r}
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

IC_sat<-IC(gpaclean$sat, 0.95)
IC_sat
```

*A partir del resultat obtingut, expliqueu com s'interpreta l'interval de confiança.*

La interpretació dels resultats indica que el NC% (en el nostres cas el 95%) de les mostres aleatòries obtingudes de la població donen lloc a un interval que conté el valor real de la mitjana poblacional. En el cas que estem estudiant, podem afirmar que si obtinguéssim infinites mostres de la població, el **95%** de les mostres, contindrien el valor real de la **mitjana poblacional** en l'interval [`r IC_sat`].

*Calculeu els intervals de confiança al 95% de la mitjana poblacional de la variable sat, en funció de si els estudiants son homes o dones.* 

```{r}
IC_sat_dones<-IC(gpaclean$sat[gpaclean$female == TRUE], 0.95)
IC_sat_homes<-IC(gpaclean$sat[gpaclean$female == FALSE], 0.95)

IC_sat_dones
IC_sat_homes
```

A continuació es representen gràficament les distribucions de la variable *sat* per als grups *homes* i *dones* utilitzant boxplot i histogrames. Adicionalment en els histogrames s'han afegit els valors de la mitjana i l'IC al 95% de la mitjana poblacional per cada grup.

```{r echo=FALSE, message=FALSE, warning=FALSE}
m_sat_dones<-mean(gpaclean$sat[gpaclean$female == TRUE])
m_sat_homes<-mean(gpaclean$sat[gpaclean$female == FALSE])

g1<-ggplot(gpaclean, aes(x=female, y=sat, color=female)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 0)) +
  coord_cartesian(ylim = c(400,1600) ) + 
  coord_flip()

g2<-ggplot(gpaclean, aes(x=sat, color=female)) + 
  geom_histogram(fill="white", bins = 50) + 
  geom_vline(aes(xintercept=m_sat_homes), color="#F8766D") +
  geom_vline(aes(xintercept=IC_sat_homes[1]), color="#F8766D", linetype = "dashed") +
  geom_vline(aes(xintercept=IC_sat_homes[2]), color="#F8766D", linetype = "dashed") +
  geom_vline(aes(xintercept=m_sat_dones), color="#00BFC4") +
  geom_vline(aes(xintercept=IC_sat_dones[1]), color="#00BFC4", linetype = "dashed") +
  geom_vline(aes(xintercept=IC_sat_dones[2]), color="#00BFC4", linetype = "dashed") +
  scale_x_continuous(breaks = seq(500,1500, by=250))

plot_grid(g1, g2, align = "hv", ncol = 1)

```

Cal destacar que l'interval de confiança s'ha calculat per la mitjana mentre que el gràfic boxplot assenyala la mediana.
En el cas dels homes, mitjana i mediana son força semblants (mitjana = `r round(mean(gpaclean$sat[gpaclean$female == FALSE]),2)` i mediana = `r round(median(gpaclean$sat[gpaclean$female == FALSE]),2)`), per contra en el cas de les dones aquests dos paràmetres son lleugerament diferents (mitjana = `r round(mean(gpaclean$sat[gpaclean$female == TRUE]),2)` i mediana = `r round(median(gpaclean$sat[gpaclean$female == TRUE]),2)`).

*Quina conclusió es pot extreure de la comparació dels dos intervals, en relació a si existeix solapament o no en els intervals de confiança? Justifiqueu la resposta.*

Observant valors i representacions gràfiques es pot concloure que amb un nivell de confiança del 95% les mitjanes poblacionals per a homes i dones **NO** es solapen.

### 3.2. Contrast d'hipòtesi per a la diferència de mitjanes de colgpa

*Volem analitzar si la nota mitjana del primer semestre és diferent per a les dones i els homes utilitzant un nivell de confiança 95%.*

**3.2.1. Pregunta de recerca**

*Formuleu la pregunta de recerca.*

La mitja de notes mitjanes de les *dones* és diferent a la mitja de notes mitjanes dels *homes*?

**3.2.2. Escriviu la hipòtesi nul·la i l'alternativa**.

$$H_{0}: \mu_{dones}=\mu_{homes}$$
$$H_{1}: \mu_{dones}\neq\mu_{homes}$$
\newpage
**3.2.3. Justificació del test a aplicar**

El test a aplicar és per a **dues mostres independents, sobre la mitjana amb variàncies desconegudes**.

En aquest moment, però, no sabem si les variàncies son desconegudes però iguals o bé, desconegudes i diferents. Per aquest motiu, aplicarem inicialment un test d'igualtat de variàncies.

**Test d'igualtat de variàncies**

$$H_{0}: \sigma_{dones}^{2}=\sigma_{homes}^{2}$$
$$H_{1}: \sigma_{dones}^{2}\neq\sigma_{homes}^{2}$$

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

Aplicació de la funció definida anteriorment:

```{r}
var_sat_HD<-var_test(gpaclean$sat[gpaclean$female == TRUE],
                     gpaclean$sat[gpaclean$female == FALSE],
                     0.95)
kable(var_sat_HD)
```

Graficant els valors resultat obtenim:

```{r echo=FALSE, fig.height=2.6, fig.width=3.2, fig.align='center'}
custom<-function(x) {df(x, df1 = var_sat_HD$nX-1, df2 = var_sat_HD$nY-2)}

ggplot(data=data.frame(x=c(0.5, 1.5)), aes(x))+
  stat_function(fun = custom, n=10000)+
  geom_vline(data=var_sat_HD, aes(xintercept=fcritL), linetype = "dashed") +
  geom_vline(data=var_sat_HD, aes(xintercept=fcritU), linetype = "dashed") +
  geom_vline(data=var_sat_HD, aes(xintercept=fobs), linetype = "solid", color="#F8766D") +
  geom_rect(data=var_sat_HD, aes(NULL, NULL, xmin=fcritL, xmax=fcritU), ymin=-0.2, ymax=0, color="black", fill="#00BFC4") +
  labs(title = "Test de variança", subtitle = "Var1 = dones\nVar2 = homes")
```

S'obté que la $f_{obs}$ està fora de l'interval d'acceptació d'$H_0$, i que el valor p és inferior al nivell de significança; per tant rebutjem la hipòtesi nul·la en favor de l'alternativa i confirmem que les variàncies son diferents. 

Així doncs, el test a aplicar és un **test bilateral** per a **dues mostres independents, sobre la mitjana amb variàncies desconegudes i diferents**.

*Realitzeu els càlculs de l'estadístic de contrast, valor crític i p valor a un nivell de confiança del 95%.*

Funció e_contrast per a calcular el test bilateral sobre la mitjana de dues mostres independents amb variances desconegudes i independents:

```{r funcio}
e_contrast<-function(m1, m2, NC){
  
  alfa<-1-NC
  meanX<-mean(m1); meanY<-mean(m2)
  nX<-length(m1); nY<-length(m2)
  sX<-sd(m1); sY<-sd(m2)
  
  v<-((((sX^2)/nX)+((sY^2)/nY))^2)/(((((sX^2)/nX)^2)/(nX-1))+((((sY^2)/nY)^2)/(nY-1)))
  
  tobs<-(meanX-meanY)/sqrt((sX^2/nX)+(sY^2/nY))
  
  tcritL<-qt(alfa/2, v)
  tcritU<-qt(1-alfa/2, v)
  pvalue<-pt(abs(tobs), df = v, lower.tail = FALSE)*2
  
  tobs<-round(tobs,4)
  tcritL<-round(tcritL,4)
  pvalue<-round(pvalue,4)
  
  return(data.frame(tobs,tcritL,tcritU,pvalue,v))
}

sat_HD<-e_contrast(gpaclean$sat[gpaclean$female == TRUE],
                gpaclean$sat[gpaclean$female == FALSE],
                0.95)
kable(sat_HD)
```

```{r echo=FALSE, fig.height=2.6, fig.width=3.2, fig.align='center'}
custom<-function(x) {dt(x, df = sat_HD$v)}

ggplot(data=data.frame(x=c(-15, 5)), aes(x))+
  stat_function(fun = custom, n=10000)+
  geom_vline(data=sat_HD, aes(xintercept=tcritL), linetype = "dashed") +
  geom_vline(data=sat_HD, aes(xintercept=tcritU), linetype = "dashed") +
  geom_vline(data=sat_HD, aes(xintercept=tobs), linetype = "solid", color="#F8766D") +
  geom_rect(data=sat_HD, aes(NULL, NULL, xmin=tcritL, xmax=tcritU), ymin=-0.2, ymax=0, color="black", fill="#00BFC4") +
  labs(title = "Test sobre la mitjana", subtitle = "Var1 = dones\nVar2 = homes")
```

**3.2.5. Interpretació del test**

En la població, la mitja de notes mitjanes de les *dones* **SI** és diferent a la mitja de notes mitjanes dels *homes* amb un nivell de confiança del 95%, donat que la $t_{obs}$ està fora de l'interval d'acceptació d'$H_0$ i el valor p és inferior al nivell de significança $\alpha$ per tant podem rebutjar la hipòtesi nul·la en favor de l'alternativa.
$t_{obs}$=`r sat_HD$tobs`, interval d'acceptació d'$H_0$=[`r round(c(sat_HD$tcritL, sat_HD$tcritU),4)`].

## 4. Model de regressió lineal

*Estimeu un model de regressió lineal múltiple que tingui com a variables explicatives: sat, female, tothrs, athlete, i hsperc, i com a variable dependent colgpa.*

```{r}
lm_colgpa<-lm(formula=colgpa~sat+female+tothrs+athlete+hsperc, data=gpaclean)
summary(lm_colgpa)
```

### 4.1. Interpretació del model

*Interpreteu el model lineal ajustat:*

Com es pot observar en el sumari anterior, totes les variables explicatives son significatives ja que el valor p associat a l'estadístic és molt inferior al nivell se significança $\alpha$.

*Quina és la qualitat de l'ajust?*

```{r}
summary(lm_colgpa)$adj.r.squared
```

La qualitat de l'ajust és pobre doncs tan sols el `r round(summary(lm_colgpa)$adj.r.squared*100,2)`% de la variabilitat de *colgpa* és explicada pel model.

*Expliqueu la contribució de les variables explicatives*

```{r}
coefficients(lm_colgpa)
```

Com es pot observar en la taula de coeficients, les variables *sat*, *female=TRUE*, *tothrs* i *athlete=TRUE* tenen una correlació positiva amb la variable *colgpa*, és a dir un increment en aquestes variable suposa un increment en la resposta del model i per tant un valor de *colgpa* predit més elevat. Per altra banda, la variable *hsperc* té una correlació negativa, és a dir un increment en el valor d'aquesta variables suposa un decrement en el valor predit de *colgpa*. 

Així doncs, si tenim en compte que la variable *sat* és la nota d'accés, la variable *tothors* és el total d'hores cursades i la variable *hsperc* és el rànquing relatiu de l'alumne en percentatge; els signes dels coeficients del model semblen lògics.

### 4.2. Predicció

*Independentment del R2 obtingut a l'apartat previ, apliqueu el model de regressió per a predir la nota mitjana d'un estudiant home, atleta, amb una nota d'entrada de 800, un total d'hores al semestre de 60 i una posició relativa al rànquing del 60%.*

```{r}
predict(lm_colgpa, newdata = data.frame(sat = 800,
                                           female = FALSE,
                                           tothrs = 60,
                                           athlete = TRUE,
                                           hsperc = 60))
```

D'acord al model l'estudiant obtindrà una nota mitjana de `r round(predict(lm_colgpa, newdata = data.frame(sat = 800,female = FALSE,tothrs = 60,athlete = TRUE,hsperc = 60)),2)`

## 5. Regressió logística

### 5.1. Estimació del model

*Estimeu un model logístic per a predir la probabilitat de ser un estudiant excel·lent al final del primer semestre a la universitat en funció de les variables: female, athlete, sat, tothrs, black, white i hsperc.*

Inicialment es comproven els nivells assignats a les variables categòriques:

```{r}
contrasts(gpaclean$female)
contrasts(gpaclean$athlete)
contrasts(gpaclean$black)
contrasts(gpaclean$white)
```

I posteriorment s'estima el model de regressió logística:

```{r}
logi_excelent<-glm(formula = excelente~female+athlete+sat+tothrs+black+white+hsperc,
                   data = gpaclean, 
                         family = binomial(link = logit))
summary(logi_excelent)
```

### 5.2. Interpretació del model estimat

*Interpreteu els resultats obtinguts. Concretament, analitzeu la significativitat de les variables explicatives i expliqueu la seva contribució per predir la probabilitat de ser un estudiant excel·lent.*

Observant els valor p de la taula sumari obtenim que les variables *athlete=TRUE* i *white=TRUE* no son significatives doncs el seu p valor és clarament superior a un nivell de significança $\alpha$=0.05. Adicionalment podem dir que la variable *black=TRUE* tampoc és significativa doncs el seu p valor és lleugerament superior al nivell de significança citat anteriorment.

En relació als signes dels coeficients estimats i dels valors de l'estadístic (z value) podem afirmar que les condicions *female=TRUE* i/o l'increment de *sat* contribueixen positivament (incrementen) la probabilitat de ser un estudiant excel·lent, mentre que la resta de variables en redueixen la probabilitat doncs tenen signes negatius.

Les variables amb un pes més elevat son *sat* i *hsperc* ja que tenen els valors de l'estadístic (z value) més alts. La primera incrementa la probabilitat de ser excel·lent (coeficient positiu) mentre que la segona en redueix la probabilitat (coeficient negatiu).

### 5.3. Importància de ser dona

*Al model anterior, interpreteu els nivells de la variable female a partir del odds ratio. En quin percentatge es veu augmentada la probabilitat de ser un estudiant excel·lent si ets dona? Proporcioneu intervals de confiança del 95% dels odds ratio.*

```{r}
round(exp(coefficients(logi_excelent)),4)
```
El fet de ser dona increment la probabilitat de ser un estudiant excel·lent en un `r (round(exp(coefficients(logi_excelent)),4)[2]-1)*100`% respecte a ser home.

Els intervals de confiança del 95% per als *odds ratio* són:

```{r}
round(exp(confint(logi_excelent, level = 0.95)),4)
```

Continuant amb la importància de ser dona, podem afirmar amb un nivell de confiança del 95% que la probabilitat de ser un estudiant excel·lent si l'estudiant és dona incrementa entre un `r (round(exp(confint(logi_excelent, level = 0.95)),4)[2,1]-1)*100`% i un `r (round(exp(confint(logi_excelent, level = 0.95)),4)[2,2]-1)*100`% respecte a si l'estudiant és home.

### 5.4. Predicció

*Amb quina probabilitat una estudiant dona, no atleta, amb un sat de 1200 punts, 50 hores cursades, de raça negra i amb un rànquing relatiu (hsperc) del 10% serà excel·lent?*

```{r}
predict(logi_excelent, newdata = data.frame(female = TRUE,
                                            athlete = FALSE,
                                            sat = 1200,
                                            tothrs = 50,
                                            black = TRUE,
                                            white = FALSE,
                                            hsperc = 10),
        type="response")
```

La probabilitat que l'alumne amb les condicions citades sigui excel·lent és del `r round(predict(logi_excelent, newdata = data.frame(female = TRUE,athlete = FALSE,sat = 1200,tothrs = 50,black = TRUE,white = FALSE,hsperc = 10),type="response")*100,2)`%. 

## 6. Anàlisi de la variància (ANOVA) d’un factor

*Realitzarem un ANOVA per a contrastar si existeixen diferències a la variable colgpa en funció de la raça dels estudiants.*

*En primer lloc, a partir de les variables black i white creeu una variable categòrica denominada race, que indiqui la raça de l'estudiant en una d'aquestes tres categories: black, white i other (per a estudiants que no són de raça negra ni blanca).*

```{r}
gpaclean$race<-as.factor(ifelse(gpaclean$white,"white",
                                ifelse(gpaclean$black,"black","other")))
summary(gpaclean$race)
```

### 6.1. Visualització gràfica

*Mostreu gràficament la distribució de colgpa segons els valors de race.*

```{r fig.height=3.1}
ggplot(gpaclean, aes(x=race, y=colgpa, color=race)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 0))
```

```{r}
mean(gpaclean$colgpa)
tapply(gpaclean$colgpa,gpaclean$race,mean)
```


### 6.2. Hipòtesi nul·la i alternativa

*Escriviu la hipòtesi nul·la i l’alternativa.*

$$H_{0}: \alpha_{black}=\alpha_{other}=\alpha_{white}$$
$$H_{1}: \alpha_{i}\neq\alpha_{j} \text{ per algun } i\neq j \text{ amb } i,j\in[black, white, other] $$

### 6.3. Model

*Calculeu l'anàlisi de variància, fent servir la funció aov o lm. Interpreteu el resultat de l'anàlisi, tenint en compte els valors: Sum Sq, Mean SQ, F i Pr (> F).*

```{r}
mod<-aov(colgpa~race, data=gpaclean)
taov<-anova(mod)
taov
```
En la taula sumari obtenim els següents valors tan per als tractament com per a l'error: els graus de llibertat (a-1 i N-a), les sumes de quadrats (SSA i SSE), els quadrats mitjans (MSA i MSE) el valor estadístic F i el valor p associat.

Observem que el valor de l'estadístic F és força més gran que la unitat fet que indica que MSA > MSE i per tant existeix algun $\alpha_i\neq0$. De manera adicional,  el valor p és clarament inferior a un nivell de significança $\alpha$ del 5% i conseqüentment acceptem la hipòtesi alternativa per concloure que el factor *race* és significatiu.

### 6.4. Efectes dels nivells del factor

*Proporcioneu l'estimació de l'efecte dels nivells del factor race. Calculeu també la part de la variabilitat de colgpa explicada per l'efecte dels nivells.*

```{r}
m<-model.tables(mod, type="means")
m
e<-model.tables(mod, type="effects")
e
```

En la taula de mitjanes obtenim els valors mitjans de la variable *colgpa* per a cada un dels tractaments.

En la taula d'efectes obtenim la diferencia en la mitjana de cada un dels tractaments $\alpha_i$. Així doncs podem afirmar que l'efecte del tractament *black* en la mitjana de *colgpa* és `r round(e$tables$race[1],4)`. Anàlogament l'efecte pel tractament *white* és `r round(e$tables$race[3],4)`. 

Per tal de conèixer la variabilitat de *colgpa* explicada per l'efecte dels nivells dividirem el valor de la suma de quadrats de la variable *race* per la suma de quadrats totals (variable *race* + residus).

```{r}
(taov$"Sum Sq"[1]/(taov$"Sum Sq"[1]+taov$"Sum Sq"[2]))*100
```

Així doncs, el `r round((taov$"Sum Sq"[1]/(taov$"Sum Sq"[1]+taov$"Sum Sq"[2]))*100,2)`% de la variabilitat és explicada per l'efecte dels nivells.

### 6.5. Conclusió dels resultats del model ANOVA

*Extraieu conclusions de l’ANOVA realitzat.*


```{r}
LSD.test(mod,"race",group=T,p.adj="bonferroni",console=T)
```

Amb l'anàlisi realitzat es pot concloure que:

* El factor *race* és significatiu.

* Els efectes dels tractaments son:
  * Black = `r round(e$tables$race[1],4)`
  * White = `r round(e$tables$race[3],4)`
  * Other = `r round(e$tables$race[2],4)`

* El test LSD amb ajust de Bonferroni indica que els tractaments **white** i **other** no son significativament diferents entre ells (grup a), mentre que si ho son amb el tractament **black** (grup b). 

### 6.6. Normalitat dels residus

*Feu servir el gràfic Normal Q-Q i el test Shapiro-Wilk per avaluar la normalitat dels residus. Podeu fer servir les funcions de R corresponents per fer el gràfic i el test.*

```{r fig.height=4}
qqnorm(residuals(mod))
qqline(residuals(mod))
```

Observem que la majoria dels residus s'ajusten a la recta, especialment en la zona central del gràfic. De totes formes no sembla prou evidència per afirmar o rebutjar el supòsit de normalitat. Així doncs contrastarem la normalitat mitjançant el test de Shapiro-Wilk.  

```{r}
shapiro.test(residuals(mod))
```

En el test Shapiro-Wilk la hipòtesi nul·la  afirma que la distribució és normal. En el càlcul s'obté un valor p molt petit (inferior a 0.05) per tant podem rebutjar la hipòtesi nul·la en favor de la alternativa i dir que la distribució no és normal.

Sota aquest supòsit hauriem d'escollir el test no-paramètric de Kruskal-Wallis doncs no necessita la assumpció de normalitat.

```{r}
kruskal.test(colgpa~race, data=gpaclean)
```

En aquest cas, obtenim un valor p molt petit, inferior al nivell de significança i per tant acceptem que hi ha diferències significatives en la variable *colgpa* segons el nivell del factor *race*.

**6.6.1. Homoscedasticitat dels residus**

*El gràfic “Residuals vs Fitted” proporciona informació sobre la homoscedasticitat dels residus. Mostreu i interpreteu aquest gràfic.*

```{r fig.height=4}
plot(mod,which=1)
```

Observem que els residus estan alineats sobre 3 linies corresponents a les mitjanes de cada un dels tractaments. Pel que fa a la homoscedasticitat, la visualització del gràfic no resulta concloent, així doncs apliquem el test de Bartlett.
\newpage
```{r}
bartlett.test(colgpa~race, data=gpaclean)
```

Obtenim un p-valor superior al nivell de significança, per tant no rebutjem la hipòtesi nul·la i acceptem que les variances son iguals.

## 7. ANOVA multifactorial

*A continuació, es desitja avaluar l'efecte sobre colgpa de la raça del estudiant combinada amb el factor gènere de l'estudiant (female). Seguiu els passos que s'indiquen a continuació.*

### 7.1. Anàlisi visual dels efectes principals i possibles interaccions

*Representeu la interacció dels dos factors race i female i comenteu els gràfics resultants.*

```{r fig.height=4}
gpaclean$female<-as.factor(gpaclean$female)
interaction.plot(gpaclean$female,gpaclean$race,gpaclean$colgpa)
```

En el gràfic s'observa que existeix interacció en els efectes dels factors *female* i *race* sobre la variable *colgpa* ja que la el nivell *TRUE* de la variable *female* incrementa la resposta de *colgpa* per als nivells *white* i *black*, en canvi disminueix la resposta de *colgpa* per al nivell *others* de la variable *race*.  

\newpage
### 7.2. Càlcul del model

*Calculeu el model ANOVA multifactorial. Podeu fer servir la funció aov.*

```{r}
mod_m <- aov(colgpa~race*female, data = gpaclean)
anova(mod_m)
```

### 7.3. Interpretació dels resultats

*Interpreteu els resultats obtinguts.*

Observem en els resultat de l'ANOVA que els estadístics F dels factors principals (*race* i *female*) son significatius (p valor molt petit i inferior al nivell de significança), per tant acceptem que hi ha efecte de la raça i del sexe. També observem que la interacció *race:female* és significativa, fet que està alineat amb el que s'havia observat en els gràfics d'interacció.

Les mitjanes per nivells dels factors i interaccions són les següents:

```{r}
model.tables(mod_m, type = "means")
```
\newpage
Els efectes dels factors principals i de les interaccions són:

```{r}
model.tables(mod_m, type = "effects")
```

Com que els factors principals són significatius realitzarem comparacions per parelles. Començarem per el factor *race*.

```{r}
HSD.test(mod_m, "race", console = TRUE)
```

Observem que els nivells *white* i *other* son significativament poc diferents entre si i per tant formen un grup (a), mentre que si son significativament diferents amb el nivell *black* (group b). Aquestes són les mateixes conclusions que s'han obtingut en el model d'un factor.

No es necessari realitzar l'estudi per a la variable *female* ja que només té 2 nivells i com anteriorment hem conclòs que és significativa, cada un dels seus nivells serà significativament diferent de l'altre.

Finalment realitzem l'estudi de les interaccions doncs hem obtingut anteriorment que aquestes són significatives.

```{r}
inter <- with(gpaclean, interaction(race, female))
mod_m_i<-aov(colgpa~inter, data = gpaclean)
HSD.test(mod_m_i, "inter", console = TRUE)
```

Observem que l'HSD test detecta 2 grups homogenis: el grup a format pels tractaments *other.male*, *white.female*, *white.male* i *other.female*; i el grup b format pels tractaments *other.female*, *black.female* i *black.male*. Observem també que el tractament *other.female* no és significativament diferent dels tractaments del grup a ni dels tractaments del grup b; tot i que els tractaments del grup a si que son significativament diferents als tractaments del grup b.
\newpage
### 7.4. Adequació del model

*Interpreteu l'adequació del model ANOVA obtingut utilitzant els gràfics dels residus.*

```{r fig.height=4}
plot(mod_m, which = c(1,2))
```

Analitzem la normalitat dels residus amb el test de Shapiro-Wilk

```{r}
shapiro.test(residuals(mod_m))
```
Novament, obtenim un valor p molt petit (inferior a 0.05), rebutjem la hipòtesi nul·la en favor de la alternativa i podem dir que la distribució no és normal, per tant el model anova no és adequat.

## 8. Conclusions

*Resumiu les conclusions principals de l'anàlisi (apartats 3 a 7). Per a això, podeu resumir les conclusions de cadascun dels apartats.*

**Estadística descriptiva i visualització**

De la informació gràfica podem extreure:

* *sat* vs *female*: S'observen diferències en les medianes de *sat* segons la variable *female*. La mediana per al grup *female = False* (homes) és superior tot i que la dispersió, especialment en el rang baix de *sat* és superior.

* *sat* vs *athlete*: També s'observen diferències en les distribucions de *sat* segons la variable *athlete*. En general, la distribució per al grup *athlete = False* (no esportistes) té tots els valors (min, max, quartils i mediana) superiors.

* *sat* vs *gpaletter*: S'observa un cert nivell d'ordre en les distribucions de *sat* segons la variable *gpaletter*. En general, tots els valors (min, max, quartils i mediana) estan endreçats seguint A > B > C > D. Més enllà caldria destacar que el grup *gpaletter = C* podria presentar una dispersió superior a la resta de grups especialment en el rang inferior de *sat*

**Estadística inferencial**

Podem concloure (amb un nivell de confiança del 95%) que:

* La mitjana poblacional de la variable *sat* estarà dins l'interval [`r IC_sat`].

* La mitjana poblacional per a les dones de la variable *sat* estarà dins l'interval [`r IC_sat_dones`].

* La mitjana poblacional per als homes de la variable *sat* estarà dins l'interval [`r IC_sat_homes`].

* En la població, la mitja de notes mitjanes de les dones **SI** és diferent a la mitja de les notes mitjanes dels homes donat que la $t_{obs}$ està fora de l'interval d'acceptació d'$H_0$ i el valor p és inferior al nivell de significança $\alpha$. Conseqüentment podem rebutjar la hipòtesi nul·la en favor de l'alternativa.

**Model de regressió lineal**

* Les variables explicatives *sat*, *female*, *tothrs*, *athlete* i *hsperc* són significatives per explicar la variable dependent *colgpa*

* La qualitat del model és pobre ja que només el `r round(summary(lm_colgpa)$adj.r.squared*100,2)`% de la variabilitat de *colgpa* és explicada pel model.

* D'acord al model l'estudiant amb les característiques proposades obtindrà una nota mitjana de `r round(predict(lm_colgpa, newdata = data.frame(sat = 800,female = FALSE,tothrs = 60,athlete = TRUE,hsperc = 60)),2)`

**Regressió logística**

* Les variables *athlete=TRUE* i *white=TRUE* no són significatives doncs el seu p valor és clarament superior a un nivell de significança $\alpha$=0.05. Adicionalment, la variable *black=TRUE* tampoc és significativa doncs el seu p valor és lleugerament superior al nivell de significança citat anteriorment.

* Les condicions *female=TRUE* i/o l'increment de *sat* contribueixen positivament (incrementen) la probabilitat de ser un estudiant excel·lent, mentre que la resta de variables redueixen la probabilitat doncs tenen signes negatius.

* Les variables amb un pes més elevat son *sat* i *hsperc* doncs tenen els valors de l'estadístic (z value) més alts.

* El fet de ser dona incrementa la probabilitat de ser un estudiant excel·lent en un `r (round(exp(coefficients(logi_excelent)),4)[2]-1)*100`% respecte a ser home.

* Podem afirmar amb un nivell de confiança del 95% que la probabilitat de ser un estudiant excel·lent si l'estudiant és dona incrementa entre un `r (round(exp(confint(logi_excelent, level = 0.95)),4)[2,1]-1)*100`% i un `r (round(exp(confint(logi_excelent, level = 0.95)),4)[2,2]-1)*100`% respecte a si l'estudiant és home.

* La probabilitat que l'alumne amb les condicions citades sigui excel·lent és del `r round(predict(logi_excelent, newdata = data.frame(female = TRUE,athlete = FALSE,sat = 1200,tothrs = 50,black = TRUE,white = FALSE,hsperc = 10),type="response")*100,2)`%. 

**Anàlisi de la variància (ANOVA) d’un factor**

* Podem concloure que el factor *race* és significatiu ja que l'estadístic F és força més gran que la unitat, indicant que MSA > MSE i per tant existeix algun $\alpha_i\neq0$. De manera adicional,  el valor P és clarament inferior a un nivell de significança $\alpha$ del 5%.

* Els efectes dels tractaments son:
  * Black = `r round(e$tables$race[1],4)`
  * White = `r round(e$tables$race[3],4)`
  * Other = `r round(e$tables$race[2],4)`

* El test LSD amb ajust de Bonferroni indica que els tractaments **white** i **other** no son significativament diferents entre ells (grup a), mentre que si ho son amb el tractament **black** (grup b).

* En l'anàlisi de normalitat de residus (Shapiro-Wilk) obtenim un valor p molt petit fet que ens indica que la distribució no és normal, per tant no es compleixen els supòsits per al test anova i procedim a l'anàlisi a través del test de Kruskal-Wallis. Aquest darrer ens dona un p-valor molt petit confirmant que existeixen diferencies significatives en la variable *colgpa* segons el nivell del factor *race*.

* Realitzem l'anàlisi d'homoscedasticitat a través del test de Bartlett i concloem que les variances son iguals.

**ANOVA multifactorial**

* Gràficament s'observa interacció en els efectes dels factors *female* i *race*.

* En el model ANOVA s'observa que tant els factors principals com la interacció són significatius.

* Analitzant per parelles obtenim:
  * Els nivells del factor *race*: *white* i *other* son significativament poc diferents entre si (formen el grup a), alhora són significativament diferents amb el nivell *black* (grup b).
  * Els 2 nivells del factor *female* són significativament diferents.
  * En l'anàlisi d'interacció entre els dos factors obtenim 2 grups homogenis: 
    * El grup a format pels tractaments *other.male*, *white.female*, *white.male* i *other.female*
    * El grup b format pels tractaments *other.female*, *black.female* i *black.male*.

* Aplicant el test de normalitat de Shapiro-Wilk als residus obtenim un valor p molt petit (inferior a 0.05). Llavors rebutjem la hipòtesi nul·la en favor de la alternativa i afirmem que la distribució dels residus no és normal, per tant el model anova no és adequat.
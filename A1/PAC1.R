#This is a R markdown file embedded in R file
---
title: "A1 - Preprocés de dades"
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
library(VIM)
library(ggplot2)
library(gridExtra)
```

```{r general, include=FALSE}
mypalette<-c("darkorange","darkorange1","darkorange2","darkorange3","darkorange4" )
```


## 1. Càrrega de l'arxiu

*Carregueu el fitxer de dades i examineu el tipus de dades amb què R ha interpretat cada variable. Examinar també els valors resum de cada tipus de variable.*

Carreguem el fitxer de dades amb la següent comanda i generem un *dataset* que anomenarem **CIds**:

```{r ex1_1_carrega}
CIds <- read.csv2("CensusIncomedataset.csv", stringsAsFactors=TRUE)
```

S'ha utilitzat *read.csv2* ja que el separador és el punt i coma (;). També s'ha utilitzat la opció *stringsAsFactors=TRUE*, doncs permet tenir una primera visió dels *strings* repetits en diferents registres.

Examinem el tipus de dades amb que R ha interpretat cada variable, per fer-ho apliquem a través de *sapply()* la funció *class()* en tot el *dataset*.

```{r ex1_2_examen1}
sapply(CIds,class)
```

\newpage

A continuació examinem els valors resum, de cada tipus de variable amb la funció *summary()* aplicada a tot el *dataset*:

```{r ex1_3_examen2}
summary(CIds)
```

En aquest punt, comprovem que ,durant la importació de dades amb la utilització de l'opció *stringsAsFactors=TRUE*, totes les variables que contenen cadenes de text s'han carregat com a *factor*.

Veiem que tenim algunes variables de tipus *factor* amb un nombre elevat de *levels* fet que indica que aquest tipus no és el més adequat i hauriem de canviar el tipus de variable a *char*.

Així doncs, realitzem les següents modificacions addicionals. Primer, generant un vector amb els noms de les variables a modificar i posteriorment recorrent amb un *for* cada una de les variables realitzant la modificació amb la funció *as.character()*:

```{r ex1_4_modificacions}
#Vector de variables a modificar
t_vector<-c("CS_ID","hours_per_week","income")

#Loop
for (i in t_vector){
  #Canvi de tipus a char
  CIds[,i]<-as.character(CIds[,i])
}

#Analitzem novament la classe de cada variable
sapply(CIds,class)
```

## 2. Obtenció del *dataset* per fer l'estudi

*En aquest cas, es vol eliminar les variables fnlwgt, capital_gain i capital_loss a més de, els registres amb més de 5 valors NAs.*
*Per altra part, es poden crear noves variables en funció de les disponibles. En aquest cas, es crearà la variable education_cat que categoritza la formació acadèmica en formació primaria si education_num és menor de 7 anys, secundaria si education_num esta entre 7 i 9 anys, universitària si education_num esta entre 10 i 13 anys i postuniversitaria si education_num és major de 13 anys.*
*Per últim, es vol canviar el nom de la variable sex per gender.*

**Eliminar variables**

Inicialment el *dataset* té `r ncol(CIds)` variables (columnes).

```{r ex2_1_variables}
ncol(CIds)
```

Eliminem les variables sol·licitades (*fnlwgt, capital_gain* i *capital_loss*), re-assignant al *dataset* totes les variables excepte les mencionades.

Per fer-ho considerem novament un vector amb el nom de les variables a eliminar. En aquest cas, però, no utilitzem un *for* per fer les modificacions si no que ho fem a traves de l'assignació del complementari (operació més eficient). Finalment es comprova novament el nombre de columnes del *dataset* per validar els canvis.

```{r ex2_2_modificacions1}
#Vector de variables a modificar
t_vector<-c("fnlwgt","capital_gain","capital_loss")

#Reasignació del complementari
CIds<-CIds[ ,!(names(CIds) %in% t_vector)]

#Reportem numero de variables
ncol(CIds)
```

Després de la modificació, el *dataset* té `r ncol(CIds)` variables.

**Eliminar registres**

Per a tenir un punt de partida, amb la següent comanda s'informa que inicialment el *dataset* té `r nrow(CIds)` registres (files).

```{r ex2_3_registres}
nrow(CIds)
```

\newpage

L'eliminació dels registres amb més de 5 valors = NA, es realitza novament a través de l'operació d'assignació. Així doncs re-assignem només amb aquells registres que no tenen més de 5 NA al *dataset* i mostrem el nombre de registres després de la modificació.

```{r ex2_4_modificacions2}
CIds<-CIds[!rowSums(is.na(CIds))>5,]
nrow(CIds)
```

Ara el *dataset* té `r nrow(CIds)` registres.

**Variable categòrica**

Per tal de categoritzar la formació acadèmica en funció de la variable *education_num*, creem una nova variable i assignem els valors d'acord als *levels* proposats en l'anunciat a través de la combinació de múltiples funcions *ifelse()*:

```{r ex2_5_modificacions3}
CIds$education_cat<-as.factor(ifelse(CIds$education_num<7,"primaria",
                              ifelse(CIds$education_num<=9,"secundaria",
                              ifelse(CIds$education_num<=13,"universitaria",
                                                          "postuniversitaria"))))
ncol(CIds)
```

Ara el *dataset* té `r ncol(CIds)` variables.

```{r ex2_6_info}
summary(CIds$education_cat)
```

**Canvi nom variable**

Finalment, canviem el nom de la variable *sex* a *gender* a través de la funció *names()* identificant la variable on es vol canviar el nom i assignant el nou nom.

```{r ex2_7_canvinom}
names(CIds)[names(CIds) == "sex"] <- "gender"
colnames(CIds)
```

## 3. Duplicació de codis

*Verifiqueu la consistència en la variable CS_ID. Si hi ha registres duplicats, assigneu un nou codi per evitarcodis duplicats. El nou codi ha de ser un valor no usat (valors superiors al màxim valor numèric contingut en CS_ID). Conserveu el mateix format que la resta de codis, amb “CS” davant de la seqüència numèrica. Podeu utilitzar la funció duplicated d’R per detectar els duplicats.*

El *dataset* té `r nrow(CIds[duplicated(CIds$CS_ID),])` registres on la variable *CS_ID* està duplicada.

Per a resoldre la duplicació de codis d'acord als requeriments de l'anunciat, comencem per analitzar el valor màxim de la part numèrica de la variable *CS_ID* utilitzant les funcions *substring()*, *as.integer()* i *max()*.

Continuem amb la creació de dos sub-*datasets* temporals, un amb els registres on la variable *CS_ID* no està repetida i l'altre amb el complementari, és a dir amb els registres on la variable *CS_ID* si està repetida, a través de la funció *duplicated()*.

Calculem els valors de *CS_ID* inicial i final que s'han d'assignar als registres del *dataset* de repetits partint del valor màxim de *CS_ID* inicial i el número de registres repetits trobats. Amb aquests valors generem un vector que posteriorment, després de donar-li el format de la variable *CS_ID* fent ús de les funcions *as.character()* i *paste()* s'imputa en la variable *CS_ID* del sub-*dataset* de repetits.

Finalment ajuntem per files els dos sub-*datasets* amb la funció *rbind()* i assignem el resultat al identificador de *dataset* que s'ha utilitzat fins ara **CIds**. Comprovem que no tenim duplicats en la variable *CS_ID* i mostrem alguns registres del final del *dataset* per validar el procés.

```{r ex3_1_principal}
#Obtenim el valor màxim del codi
t_maxcodis<-max(as.integer(substring(CIds$CS_ID,3,)))

#Separem el dataset en 2 sub-datasets
t_repetits<-CIds[duplicated(CIds$CS_ID),]
t_norepetits<-CIds[!duplicated(CIds$CS_ID),]

#Extraiem el numero de registres repetits i els valor numèrics dels nous codis inicial i final 
t_numrep<-nrow(t_repetits)
t_inici<-t_maxcodis+1
t_final<-t_maxcodis+t_numrep

#Generem un vector amb els nous codis per als registres repetits
t_ID_vector<-t_inici:t_final

#Formatem el vector per a complir amb el format de ID del dataset
t_ID_vector<-as.character(t_ID_vector)
t_ID_vector<-paste("CS",t_ID_vector, sep="")

#Asignem els valors del ID al sub-dataset de repetits i combinem els 2 sub-datasets
t_repetits$CS_ID<-t_ID_vector
CIds<-rbind(t_norepetits,t_repetits)

#Comprovem duplicats
sum(duplicated(CIds$CS_ID))

#Comprovem registres finals del dataset
tail(CIds[,c(1,2,3)],15)
```

## 4. Normalització de les dades qualitatives

### 4.1. Eliminació d'espais en blanc

*S’ha observat que hi espais en blanc l’inici dels valors a les variables qualitatives. Per tant, cal eliminar aquest espais en blancs.*

De manera anàloga als canvis addicionals del apartat 1, generem un vector amb les variables on aplicar les modificacions i les recorrem amb un *loop for*. Per tal d'eliminar els espais en blanc fem ús de la funció *trimws()*. Cal destacar que s'ha de garantir que les dades d'entrada a la funció són de tipus *char*, així doncs es converteixen amb la funció *as.character()*.

El resultat de la eliminació d'espais en blanc es torna a convertir a *factor* amb la funció *as.factor()* i s'assigna a la variable corresponent del *dataset*.

```{r ex4_1_blancs}
#Vector de variables a modificar
t_vector<-c("workclass","marital_status","relationship","occupation","race","gender")

#Loop
for (i in t_vector){
  #Conversio a char, eliminació de blancs, reconversió a factor i assignació
  CIds[,i]<-as.factor(trimws(as.character(CIds[,i])))
}
```

### 4.2. Marital-status

*Canviar les categories de la variable marital status actuals per altres que ocupin una caràcter. Els valors que s’assignaren a la variable marital_status són: M per Married, S per Single, X per Separated, D per Divorced, W per Widowed. Representeu gràficament la distribució dels valors de la variable.*

Inicialment es comprova l'ordre de les categories de la variable *marital_status* amb la funció *summary()*. Posteriorment s'assignen les noves categories utilitzant la funció *levels()*. Finalment es comprova novament que les modificacions s'han realitzat correctament amb la funció *summary()* comparant els resultats finals amb els inicials.

```{r ex4_2_marital}
summary(CIds$marital_status)
levels(CIds$marital_status)<-c("D","M","X","S","W")
summary(CIds$marital_status)
```

Els gràfics de la següent pàgina mostren, a l'esquerra, en gràfic de barres, el nombre de registres per cada categoria de la variable *marital_status* i a la dreta en diagrama de sectors la freqüència relativa de cada categoria respecte la mostra. Els gràfics es generen amb les funcions *plot()* i *pie()* respectivament.

```{r ex4_3_marital_graph, echo=TRUE}
par(mfrow=c(1,2))
plot(CIds$marital_status, main="Marital-status", col = mypalette,ylab="unitats")
pie(summary(CIds$marital_status),main = "Marital-status", col = mypalette)
```

### 4.3. Gènere

*Reviseu la consistència dels valors de la variable gender i feu les modificacions oportunes per indicar les categories finals com f i m que correspon a femeni i masculi, respectivament. Representeu gràficament la distribució dels valors de la variable.*

Inicialment es comprova la quantitat de categories de la variable *gender* amb la funció *summary()*. S'observa que existeixen errors sintàctics que introdueixen un nombre més elevat de categories. Es procedeix a reparar-los amb l're-assignació de les categories a valors de base "f" i "m" utilitzant la funció *levels()*. Finalment es comprova novament que les modificacions s'han realitzat correctament amb la funció *summary()* comparant els resultats finals amb els inicials.

```{r ex4_4_gender}
summary(CIds$gender)
levels(CIds$gender)<-c("f","f","f","f","m","m","m","m")
summary(CIds$gender)
```

Els gràfics de la pàgina següent mostren, a l'esquerra, en gràfic de barres, el nombre de registres per cada categoria de la variable *gender* i a la dreta, en diagrama de sectors la freqüència relativa de cada categoria respecte la mostra.

```{r ex4_5_gender_graph, echo=TRUE}
par(mfrow=c(1,2))
plot(CIds$gender, main="Gènere", col = mypalette, ylab="unitats")
pie(summary(CIds$gender),main = "Gènere", col = mypalette)
```

## 5. Normalització de les dades quantitatives

### 5.1. Edat

*Reviseu el format de la variable age i feu les transformacions oportunes segons els criteris especificats anteriorment: han de ser de tipus sencer, sense decimals.*

Es comprova que la variable *age* sigui de tipus *integer* amb la funció *is.integer()*. I es valida que no es necessari realitzar modificacions.

```{r ex5_1_check}
is.integer(CIds$age)
```

### 5.2. Educació

*Reviseu el format de la variable education_num i feu les transformacions oportunes segons els criteris especificats anteriorment: han de ser de tipus sencer, sense decimals.*

Es comprova que la variable *education_num* sigui de tipus *integer* amb la comanda *is.integer()*. I es valida que no es necessari realitzar modificacions.

```{r ex5_2_check}
is.integer(CIds$education_num)
```

### 5.3. Hores per setmana

*Reviseu el format de la variable hours_per_week i feu les transformacions oportunes segons els criteris especificats anteriorment: En les dades numèriques, el símbol de separador decimal és el punt i no la coma. A més, si es presenta la unitat de la variable, per exemple hores en el cas de hours_per_week, cal eliminar per convertir la variable a tipus numèric.*

Per tal de complir amb els requisits de l'anunciat s'ha de treure la unitat "h" de cada un dels registres. També canviar la coma (,) pel punt (.) i convertir el tipus de variable a numèrica. Així doncs es canvia el *substring* " h" per "" (conjunt buit) en els registres de la variable *hours_per_week* amb la funció *sub()*. A continuació, també amb la funció *sub()* es canvia la coma "," pel punt ".", també en tots els registres de la mateixa variable. Per acabar, es converteix el tipus de variable a numèric amb decimals amb la funció *as.double()*. Es mostren els 100 primers valors de la variable amb la funció *head()* i se'n fa un resum amb la funció *summary()*. Cal destacar que totes les conversions es re-assignen a la mateixa variable del *dataset*, *hours_per_week*.

```{r ex5_3_hores_setmana}
#Modificacions
CIds$hours_per_week<-sub(" h","",CIds$hours_per_week)
CIds$hours_per_week<-sub(",",".",CIds$hours_per_week)
CIds$hours_per_week<-as.double(CIds$hours_per_week)

#Comprovacions
head(CIds$hours_per_week,100)
summary(CIds$hours_per_week)
```

### 5.3. Income

*Reviseu el format de la variable income segons el criteri indicat més amunt: La variable ‘income‘ s’ha d’expressar en milers d’euros (k€)*

D'acord amb l'anunciat, la variable *income* ha de ser de tipus numèric i s'ha d'expressar en milers d'euros (k€). Procedim a visualitzar alguns registres de la variable *income* amb la funció *head()* per a conèixer les possibles modificacions a realitzar.

```{r ex5_4_mostra}
head(CIds$income,30)
```

Observem que alguns registres ja estan expressats en "Milers d'euros" mentre que d'altres estan expressats en "euros". Per tant s'ha de diferenciar entre dos grups de dades per a realitzar les modificacions.

Primer grup, enfocat en els registres expressats en "Milers d'euros" en els que es substitueix els *substring* " Milers d'euros" per "" a través de la funció *sub()*.

Segon grup, enfocat en els registres expressats en "euros" . La modificació dels registres es realitza a través d'una *REGEX* que defineix un primer grup [Grup 1] de 0 o més dígits (\\\\d\*), concatenat a un segon grup [Grup 2] que defineix exactament 3 dígits (\\\\d{3}) i concatenat al *substring* " euros". Aquesta estructura es substitueix pel [Grup 1] i el [Grup 2] separats per una coma "," amb la funció *sub()*. Amb aquesta modificació s'obté el resultat de passar "euros" a "k€" sense deixar de treballar amb cadenes de caràcters (*chr*).

Finalment, es canvia la coma "," pel punt "." de tots els registres novament amb la funció *sub()*, es canvia el tipus de variable a numèrica amb la funció *as.double()* i es validen les modificacions mostrant els 100 primers valors de la variable amb la funció *head()* i fent un resum amb la funció *summary()* 

```{r ex5_5_modificacions}
#Modificacions primer group en la variable income
CIds$income<-sub(" Milers d'euros","",CIds$income)

#Modificacions segon grup en la variable income
CIds$income<-sub("(\\d*)(\\d{3}) euros","\\1,\\2",CIds$income, fixed=FALSE)

#Modificacions tots els registres de la variable income
CIds$income<-sub(",",".",CIds$income)
CIds$income<-as.double(CIds$income)

#Comprovacions
head(CIds$income,100)
summary(CIds$income)
```

## 6. Valors atípics

*Reviseu si hi ha valors atípics en les variables age, education_num, hours_per_week i income. Si es tracta d’un valor anòmal, és a dir anormalment alt o baix, substituir el seu valor per NA, que posteriorment s’ha d’imputar.*

**Age**

Per analitzar els valors atípics es mostren gràficament els registres de la variable *age* en un diagrama de caixa (esquerra), les estadístiques principals i el numero de registres que queden fora dels "bigotis". Aquestes operacions es realitzen a través de les funcions *boxplot*, el mètode *stats* de *boxplot* i la funció *length*.

Amb aquest anàlisi i aplicant sentit comú, es defineixen com a valors atípics aquells amb un valor en la variable *age* superior a 120 i conseqüentment se'ls assigna NA.

Finalment es genera un nou diagrama de caixa (dreta) i s'extreuen estadístiques de la variable *age* ja modificada amb les assignacions de NA per a valors atípics.  

```{r ex6_1_age_atipics}
par(mfrow=c(1,2))

#Boxplot amb dades originals
boxplot(CIds$age, main="Original", col = mypalette)
boxplot.stats(CIds$age)$stats
length(boxplot.stats(CIds$age)$out)

#Considerem valors atípics valors d'edats > 120
CIds$age[CIds$age>120]<-NA

#Boxplot amb dades modificades
boxplot(CIds$age, main="Modificat", col = mypalette)
summary(CIds$age)
```

\newpage

**Education num**

Amb la variable *education_num* es procedeix de la mateixa manera que en la variable anterior. En aquest cas, observant el diagrama de caixa inicial s'interpreta que no hi ha valors atípics i per tant no es realitzen modificacions.

```{r ex6_2_education_atipics}
par(mfrow=c(1,2))

#Boxplot amb dades originals
boxplot(CIds$education_num, main="Original", col = mypalette)
boxplot.stats(CIds$education_num)$stats
length(boxplot.stats(CIds$education_num)$out)

#Considerarem que en aquesta variable no hi ha valors atípics

boxplot(CIds$education_num, main="Modificat", col = mypalette)
summary(CIds$education_num)
```

\newpage

**Hours per week**

Per la variable *hours_per_week* es procedeix de la mateixa manera que amb les dues anteriors variables. En aquest cas, segons l'anunciat de l'activitat es defineixen com a valors atípics aquells amb un nombre *hours_per_week* superior a 80.

```{r ex6_3_hours_atipics}
par(mfrow=c(1,2))

#Boxplot amb dades originals
boxplot(CIds$hours_per_week, main="Original", col = mypalette)
boxplot.stats(CIds$hours_per_week)$stats
length(boxplot.stats(CIds$hours_per_week)$out)

#Es considera atípic valors d'hours_per_week > 80
CIds$hours_per_week[CIds$hours_per_week>80]<-NA

#Boxplot amb dades modificades
boxplot(CIds$hours_per_week, main="Modificat", col = mypalette)
summary(CIds$hours_per_week)
```

\newpage

**Income**

Finalment, per la variable *income* es procedeix de la mateixa manera que les variables anteriors. En aquest cas, a partir de l'observació del diagrama de caixa inicial es defineixen com a valors atípics aquells valors de la variable *income* que son superiors a 200 o inferiors a 5.

```{r ex6_4_income_atipics}
par(mfrow=c(1,2))

#Boxplot amb dades originals
boxplot(CIds$income, main="Original", col = mypalette)
boxplot.stats(CIds$income)$stats
length(boxplot.stats(CIds$income)$out)

#Es considera atípic valors d'income > 200 k€ o < 5k€
CIds$income[CIds$income>200 | CIds$income<5]<-NA

#Boxplot amb dades modificades
boxplot(CIds$income, main="Modificat", col = mypalette)
summary(CIds$income)
```

\newpage

## 7. Imputació de valors

*Busqueu si hi ha valors perduts en les variables quantitatives age, education_num, hours_per_week i income. En cas de valors perduts, apliqueu el procés:*

**Age**

*En la variable ‘age‘, apliqueu imputació per la mitjana aritmètica.*

Inicialment hi ha `r sum(is.na(CIds$age))` NA en la variable *age*.

Per al càlcul de la mitjana s'utilitza la funció *mean()* amb l'opció *na.rm=TRUE* per a no considerar els valors NA en el càlcul. El resultat s'assigna als registres NA de la variable *age* utilitzant la funció *is.na()*

```{r ex7_1_mitjana}
#Imputació
CIds$age[is.na(CIds$age)]<-mean(CIds$age,na.rm=TRUE)

#Comprovació NA
sum(is.na(CIds$age))
```

Després de la imputació, tenim `r sum(is.na(CIds$age))` NA en la variable *age*.

**Income**

*En la variable ‘income‘, apliqueu imputació per la mitjana aritmètica dels registres del mateix gènere, és a dir, separat per gènere*.

Inicialment hi ha `r sum(is.na(CIds$income))` NA en la variable *income*.

L'operativa per a imputar valors es equivalent a l'explicada anteriorment amb la diferencia que s'ha de considerar la mitjana aritmètica dels registres del mateix gènere. Llavors l'operació es realitza en dos passos:

En el primer pas, es realitza la imputació en els registres de la variable *income* amb gènere masculí; fet que s'aconsegueix utilitzant dues expressions condicionals unides amb l'operador "&", que obliga que siguin certes les dues.

```{r ex7_2_mitjana_genere_1}
#Registres NA
CIds[is.na(CIds$income),c("CS_ID","income")]

#Imputació
CIds$income[is.na(CIds$income) & CIds$gender=="m"]<-
  mean(CIds[CIds$gender=="m","income"],na.rm = TRUE)
```

Després de la imputació de NAs en el pas anterior, hi ha `r sum(is.na(CIds$income))` NA en la variable *income*.
El segon pas es per a imputar en els registres de la variable *income* amb gènere femení. Aquesta imputació es anàloga a l'anterior canviant en *level* objectiu de la variable *gender* de "m" a "f".

```{r ex7_3_mitjana_genere_2}
#Registres NA
CIds[is.na(CIds$income),c("CS_ID","income")]

#Imputació
CIds$income[is.na(CIds$income) & CIds$gender=="f"]<-
  mean(CIds[CIds$gender=="f","income"],na.rm = TRUE)

#Comprovació NA
sum(is.na(CIds$income))
```

Després de totes les imputacions, hi ha `r sum(is.na(CIds$income))` NA en la variable *income*.

**Hours per week**

*A la resta de variables, apliqueu imputació per veïns més propers, utilitzant la distància de Gower, considerant en el còmput dels veïns més propers la resta de variables quantitatives esmentades en aquest apartat. A més, considereu que la imputació s’ha de fer amb registres del mateix gènere. Per exemple, si un registre a imputar és de gènere "M", s’ha de realitzar la imputació usant les variables quantitatives dels registres de gènere "M". Per realitzar aquesta imputació, podeu fer servir la funció "kNN" de la llibreria VIM amb un nombre de veïns igual a 11.*

Inicialment hi ha `r sum(is.na(CIds$hours_per_week))` NA en la variable *hours_per_week*.
De la mateixa manera que en l'apartat anterior, es realitza la imputació de valors en dos passos. Primer en els registres de gènere "m" i posteriorment en els registres de gènere "f".

A continuació es mostren els 10 primers registres NA en la variable *hours_per_week* de genere "m" i s'assignen els seus codis o valors de la variable *CS_ID* en un vector temporal que es recuperarà posteriorment per a comprovar que les imputacions s'han realitzat correctament.

```{r ex7_4_knn_1}
#Per a gènere == "m"
#10 primers valors a imputar
head(CIds[is.na(CIds$hours_per_week) & CIds$gender=="m",c("CS_ID","hours_per_week")],10)

#Vector codis CS_ID dels 10 primers valors a imputar de gènere "m"
mID<-head(CIds[is.na(CIds$hours_per_week) & CIds$gender=="m","CS_ID"],10)

#Per a gènere == "f"
#10 primers valors a imputar
head(CIds[is.na(CIds$hours_per_week) & CIds$gender=="f",c("CS_ID","hours_per_week")],10)

#Vector codis CS_ID dels 10 primers valors a imputar de gènere "f"
fID<-head(CIds[is.na(CIds$hours_per_week) & CIds$gender=="f","CS_ID"],10)
```

S'imputem NA per kNN en dos passos utilitzant la funció *kNN()* de la llibreria *VIM*; separant per gènere "m" i gènere "f". s'escull l'opció *imp_var = FALSE* per evitar que es generi una variable adicional i que es tingui un *Warning* informant que s'està realitzant una imputació de N+1 variables en un *dataset* de N variables. 

```{r ex7_5_knn_2}
#Imputació
CIds[CIds$gender=="m",]<-kNN(CIds[CIds$gender=="m",],
                             variable="hours_per_week",k=11, imp_var = F)
CIds[CIds$gender=="f",]<-kNN(CIds[CIds$gender=="f",],
                             variable="hours_per_week",k=11, imp_var = F)
```

Comprovem les imputacions recuperant el vector de codis per a gènere "m"

```{r ex7_6_knn_3}
CIds[CIds$CS_ID %in% mID,c("CS_ID","hours_per_week")]
```

Comprovem les imputacions recuperant el vector de codis per a gènere "f"

```{r ex7_5_knn_4}
CIds[CIds$CS_ID %in% fID,c("CS_ID","hours_per_week")]
```

Després de la imputació, tenim `r sum(is.na(CIds$hours_per_week))` NA en la variable *hours_per_week*.

\newpage

## 8. Estudi descriptiu

### 8.1. Funcions de mitjana robustes

*Implementeu una funció en R que, donat un vector amb dades numèriques, calculi la mitjana retallada i la mitjana Winsor.*

La implementació de la mitjana retallada es pot observar a continuació.

```{r ex8_1 mitjana_retallada}
mitjana.retallada<-function(x, perc=0.05){
  lowval<-quantile(x,perc)
  highval<-quantile(x,1-perc)
  x<-x[x>lowval & x<highval]
  output<-mean(x)
  output
}
```

Es busquen els valors extrems (*lowval* i *highval*) que deixen el percentatge indicat de la mostra fora amb la funció *quantile()*. Posteriorment es re-assignen els valors que compleixen els dos condicionals units per un operador "&", ser més gran que *lowval* i més petit que *highval*. En el vector resultant es realitza la mitjana aritmètica amb la funció *mean()* i se'n retorna els resultat.

La implementació de la mitjana winsor es pot observar a continuació.

```{r ex_8_2_mitjana_winsor}
mitjana.winsor<-function(x, perc=0.05){
  lowval<-quantile(x,perc)
  highval<-quantile(x,1-perc)
  x<-replace(x,x<lowval,lowval)
  x<-replace(x,x>highval,highval)
  output<-mean(x)
  output
}
```

De manera similar a la mitjana retallada, per la mitjana winsor també es defineixen els valors extrems amb la funció *quantile()*. En aquest cas, però enlloc de re-assignar els valors deixant fora del vector els que no estan en l'interval (*lowval*,*highval*), fet que redueix el nombre d'elements; el que es fa es substituir els valors de fora l'interval, pel valor extrem. Aquesta operació es realitza en dos passos, primer pels valors més petits que *lowval* als que se'ls assigna *lowval* i després pels valors més grans que *highval* als que se'ls assigna *highval*. Aquestes operacions de substitució es realitzen amb la funció *replace()*.Finalment, es calcula la mitja aritmètica i es retorna el resultat. 

### 8.2. Estudi descriptiu de les variables quantitatives

*Feu un estudi descriptiu de les variables quantitatives age, education_num, hours_per_week i income. Per a això, prepareu una taula amb diverses mesures de tendència central i dispersió, robustes i no robustes. Feu servir, entre d’altres, les funcions de l’apartat anterior. Presenteu, així mateix gràfics on es visualitzi la distribució dels valors d’aquestes variables quantitatives.*

La següent taula mostra les principals mesures de tendència central i dispersió, robustes i no robustes per a les variables *age*, *education_num*, *hours_per_week* i *income*.

```{r ex8_3_taula}

t_row<-c("age","education_num","hours_per_week","income")
t_col<-c("mean","median","trim_mean","winsor_mean","sd","IQR","mad")
M<-matrix(1,length(t_row),length(t_col))
rownames(M)<-t_row
colnames(M)<-t_col

for (i in t_row){
  M[i,"mean"]<-round(mean(CIds[,i]),2)
  M[i,"median"]<-round(median(CIds[,i]),2)
  M[i,"trim_mean"]<-round(mitjana.retallada(CIds[,i]),2)
  M[i,"winsor_mean"]<-round(mitjana.winsor(CIds[,i]),2)
  M[i,"sd"]<-round(sd(CIds[,i]),2)
  M[i,"IQR"]<-round(IQR(CIds[,i]),2)
  M[i,"mad"]<-round(mad(CIds[,i]),2)
}

kable(M)
```

En les properes pàgines es mostren diversos gràfics de la distribució de valors (histograma, diagrama de densitat i diagrama de caixa ) per cada una de les variables considerades.

\newpage

**Age**

```{r ex8_4_grafics_age, fig.height=7.8}
par(mfrow=c(2,2))
hist(CIds$age, col = mypalette[1], main = "Age distribution")
plot(density(CIds$age), col = mypalette[3], main = "Age density")
boxplot(CIds$age, col = mypalette[2], ylab="Age", main = "Age boxplot")

```

\newpage
**Education_num**

```{r ex8_5_grafics_education ,fig.height=7.8}
par(mfrow=c(2,2))
hist(CIds$education_num, col = mypalette[1], main = "Education_num distribution")
plot(density(CIds$education_num), col = mypalette[3], main = "Education_num density")
boxplot(CIds$education_num, col = mypalette[2], ylab="Education_num", 
        main = "Education_num boxplot")

```

\newpage
**Hours_per_week**

```{r ex8_6_grafics_hours ,fig.height=7.8}
par(mfrow=c(2,2))
hist(CIds$hours_per_week, col = mypalette[1], main = "Hours_per_week distribution")
plot(density(CIds$hours_per_week), col = mypalette[3], main = "Hours_per_week density")
boxplot(CIds$hours_per_week, col = mypalette[2], ylab="Hours_per_week", 
        main = "Hours_per_week boxplot")

```

\newpage
**Income**

```{r ex8_7_grafics_income ,fig.height=7.8}
par(mfrow=c(2,2))
hist(CIds$income, col = mypalette[1], main = "Income distribution")
plot(density(CIds$income), col = mypalette[3], main = "Income density")
boxplot(CIds$income, col = mypalette[2], ylab="Income", main = "Income boxplot")

```

\newpage
**Gràfics addicionals**

Finalment es mostren alguns diagrames addicionals generats amb la llibreria *ggplot*, per analitzar a simple vista si existeix biaix de gènere (*gender*) o raça (*race*) en la variable *income*. També es mostra la distribució de la variable *income* per cada nivell d'ocupació (*occupation*) i per cada categoria d'educació (*educacion_cat*).  

```{r ex8_8_grafics_addicionals ,fig.height=4.9 }
#Creació de gràfics
fmean<-mean(CIds[CIds$gender=="f",]$income)
mmean<-mean(CIds[CIds$gender=="m",]$income)
g1<-ggplot(CIds, aes(x=income, color=gender)) + 
  geom_histogram(fill="white", bins = 50) + 
  geom_vline(aes(xintercept=fmean), color="hotpink") + 
  geom_vline(aes(xintercept=mmean), color="turquoise")
g2<-ggplot(CIds, aes(x=race, y=income, color=race, order)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90), legend.position = 'none')
g3<-ggplot(CIds, aes(x=education_cat, y=income, color=education_cat, order)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90), legend.position = 'none')
g4<-ggplot(CIds, aes(x=occupation, y=income, color=occupation)) + 
  geom_violin()+
  theme(axis.text.x = element_text(angle = 90), legend.position = 'none')

#Alineament de gràfics
grid.arrange(g1,g4,g2,g3,ncol=2)
```

## 9. Arxiu final

*Un cop realitzat el preprocessament sobre l’arxiu, copieu el resultat de les dades a un fitxer anomenat CensusIncome_clean.csv.*

Per a la creació del l'arxiu final es proposa el codi que es mostra a continuació, considerant el format espanyol i per tant utilitzant la funció *write.csv2*:

```{r ex9_final}
write.csv2(CIds, file="CensusIncome_clean.csv", row.names = FALSE)
```


#data = read_csv("https://www.datos.gov.co/resource/rgxm-mmea.csv")

# Se puede realizar directamente desde el API

#data <- read_csv("BM_Prueba/Tienda_Virtual_del_Estado_Colombiano_-_Consolidado.csv")

# Sin embargo, este archivo solo muestra 1000 registros, por lo cual se realizar la carga manual de los datos

## Carga de datos
data <- read_csv("BM_Prueba/Tienda_Virtual_del_Estado_Colombiano_-_Consolidado.csv")
View(data)

## Librerias 
install.packages("datos")
library(tidyverse)
library(datos)
library(scales)
library(RColorBrewer)
library(ggplot2)
library(lubridate)
library(zoo)
#options(scipen=0) 

summary(data)
colSums(is.na(data))
table(data$Agregacion)

# Debemos solo quedarnos con los datos correspondientes al PAE

agregacion = unique(data$Agregacion)
refpae=str_view(agregacion,"PAE", match = TRUE)

# Con lo anterior observamos que las agregaciones que se relacionan con el PAE son: 
# Suministro de alimentos PAE, suministro de alimentos PAE II, Almacenamiento PAE II, PAE
# Suministro de alimentos PAE II, Almacenamiento PAE y Subasta Alimentos PAE

# Ahora solo nos quedaremos con los datos relacionados con el PAE

data_pae = data[which(data$Agregacion=="Suministro de alimentos PAE" | data$Agregacion == "Suministro de alimentos PAE II" | data$Agregacion == "Almacenamiento PAE II" | data$Agregacion == "PAE" | data$Agregacion == "Suministro de Alimentos PAE II" | data$Agregacion == "Almacenamiento PAE"| data$Agregacion == "Subasta Alimentos PAE"),]

table(data_pae$Agregacion)

barplot(table(data_pae$Agregacion), cex.names = 0.5)


dim(data_pae)

# De los 54473 compras reportadas en la base de datos, 533 se relacionan con el PAE, esto representa un 
# 0.97% del total de las compras. 

# ¿Hay NAs?

colSums(is.na(data_pae))


# Deflactación 

#Año base 2018



data_pae$TotalD=ifelse(data_pae$Año==2015,data_pae$Total/0.8805,data_pae$Total)
data_pae$TotalD=ifelse(data_pae$Año==2016,data_pae$Total/0.9311,data_pae$TotalD)
data_pae$TotalD=ifelse(data_pae$Año==2017,data_pae$Total/0.9692,data_pae$TotalD)
data_pae$TotalD=ifelse(data_pae$Año==2019,data_pae$Total/1.038,data_pae$TotalD)
data_pae$TotalD=ifelse(data_pae$Año==2020,data_pae$Total/1.0529,data_pae$TotalD)

dcast(data_pae,Año~1, fun.aggregate=sum, value.var="Total")
dcast(data_pae,Año~1, fun.aggregate=sum, value.var="TotalD")

# Coeficientes de deflactaci?n calculados a partir de informaci?n del Banco de la Rep?blica. Fuente: https://www.banrep.gov.co/es/estadisticas/indice-precios-consumidor-ipc

### Estadisticas descriptivas 

## Por año
table(data_pae$Año)
ggplot(data_pae) + aes(x=as.factor(Año)) + geom_bar(stat="count", fill="steelblue") + labs(x="A?o", y="Cantidad de compras") + scale_y_continuous(labels=comma)
ggplot(data_pae) + aes(x=as.factor(Año), y=Total/1000000) + geom_col(fill="steelblue") + labs(x="A?o", y="Valor total de las compras (en millones de pesos)") + scale_y_continuous(labels=comma)
ggplot(data_pae) + aes(x=as.factor(Año), y=TotalD/1000000) + geom_col(fill="steelblue") + labs(x="A?o", y="Pesos (en millones)") + scale_y_continuous(labels=comma)


anotot=dcast(data_pae,Año~1, fun.aggregate=sum, value.var="TotalD")
names(anotot)[2]="Total"

anoc=as.data.frame(table(data_pae$Año))
names(anoc)=c("Año","Count")

data_a=merge(anotot,anoc,by="Año")

ylim1min = 0
ylim1max = max(data_a$Total/1000000)
ylim2min = min(data_a$Count)
ylim2max = max(data_a$Count)

scale_to_value1 <- function(values) rescale(values, to = c(ylim1min, ylim1max))
scale_to_value2 <- function(values) rescale(values, to = c(ylim2min, ylim2max))
ggplot(data_a, aes(x=as.factor(A?o))) + 
    geom_col(aes(y=Total/1000000, fill=as.character(Total[1]))) + 
    labs(y="Valor de las compras (en millones de pesos)",x="A?o", color="Legend") +
    geom_line (aes(y=scale_to_value1(Count), group=as.character(Count[1]), color=as.character(Count[1]))) +
    scale_y_continuous(labels = comma, limits=c(ylim1min,ylim1max),sec.axis = sec_axis(~ scale_to_value2(.), name="N?mero de compras")) +
    scale_color_manual(name = "Linea", label="Número de compras", values="red") + 
    scale_fill_manual(name="Barras",label = "Valor de las compras",values = "steelblue") +
    theme(legend.title=element_blank())

## Por rama
table(data_pae$`Rama de la Entidad`)
# Las compras solo pertenecen a la rama ejecutiva 

## Por sector de la entidad  
table(data_pae$`Sector de la Entidad`)
# Las compras corresponden a dos sectores, Defensa Nacional y Educación Nacional

# Por entidad
table(data_pae$Entidad)
# Las compras del sector de Defensa Nacional son hechas por el Comando Armada Nacional 

# Por solicitante
table(data_pae$Solicitante)
soluni=unique(data_pae$Solicitante)
length(soluni)
# Las 533 compras han sido realizadas por 5 personas 

# Fecha
# head(data_pae$Fecha)
# data_pae$month=month(mdy_hms(data_pae$Fecha))
# table(data_pae$month)
# barplot(table(data_pae$month), labels=c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"))
# ggplot(data_pae) + aes(x=month) + geom_bar(stat="count") + labs(x="Months", y="Cantidad de compras") 
# #scale_x_discrete(labels(c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")))
# 
# plot(data_pae$Fecha)
# data_pae$month=as.factor(data_pae$month)
# levels(data_pae$month)=c(c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"))
# ggplot(data_pae) + aes(x=month) + geom_bar(stat="count",fill="steelblue") + labs(x="Meses", y="Cantidad de compras") + scale_y_continuous(labels=comma)
# ggplot(data_pae) + aes(x=month, y=TotalD/1000000) + geom_col() + labs(x="Meses", y="Pesos (en millones)") + scale_y_continuous(labels=comma, breaks = c(50000,100000,150000,200000,250000))


data_pae$Fecha2=mdy_hms(data_pae$Fecha)
data_pae$Fecha2=as.yearmon(data_pae$Fecha2)
fecha=as.data.frame(table(data_pae$Fecha2))
fecha[,1]=as.yearmon(fecha[,1])

ggplot(fecha) + geom_line(aes(x=fecha$Var1, y=fecha$Freq), color="blue", size=0.8)
ggplot(fecha) + geom_bar(aes(y=fecha$Freq), color="blue")

# Por proveedor
table(data_pae$Proveedor)
prouni=unique(data_pae$Proveedor)
length(prouni)
# Las 533 compras han sido realizadas por 81 proveedores

# Por estado 
table(data_pae$Estado)
barplot(table(data_pae$Estado))

# Por valor 
summary(data_pae$Total)
hist(data_pae$Total, freq=F)
hist(data_pae$Total)
plot(density(data_pae$TotalD))
ggplot(data_pae) + aes(x=TotalD/1000000, y = ..density..) + geom_histogram(fill="steelblue",bins=40,fill = "grey", color = "black") + scale_x_continuous(labels=comma) + xlab("Total (en millones de pesos)") + ylab("Density")
options(scientific(8.268e+08))
summary(data_pae$TotalD)
options(scientific(0))
# Por ciudad
table(data_pae$Ciudad)
unique(data_pae$Ciudad)


# Entidad obligada 
table(data_pae$`Entidad Obigada`)

# Post Conflicto 
table(data_pae$EsPostconflicto)

# Por proveedor
prov=table(data_pae$`NIT Proveedor`)
provuni = unique(data_pae$`NIT Proveedor`)
length(provuni)
barplot(sort(prov,decreasing = T))
plot(density(table(data_pae$`NIT Proveedor`)))  

# Por actividad econ?mica 
table(data_pae$`Actividad Economica Proveedor`)
plot(density(table(data_pae$`Actividad Economica Proveedor`)))
data_pae$`Actividad Economica Proveedor` = ifelse(data_pae$`Actividad Economica Proveedor`=="Comercio al por mayor no especializado.", 4690, data_pae$`Actividad Economica Proveedor`)

prov_tot=dcast(data_pae,data_pae$`NIT Proveedor` ~ 1, sum, value.var = "Total")
class(prov_tot)
prov=as.data.frame(prov)


### Otros analisis 

names(prov)[1]="NIT"
names(prov_tot)[1]="NIT"
data_j = merge(prov_tot,prov, by="NIT")
names(data_j)[2:3]=c("Total", "Count")
data_j$mean = (data_j$Total/data_j$Count) 
View(data_j)

ggplot(data_j) + aes(x=NIT, y=sort(mean/1000000, decreasing=T)) + geom_col(fill="steelblue") + labs(y="Valor promedio del contrato (en millones de pesos)", x="Empresa") +scale_y_continuous(labels = comma)

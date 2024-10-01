#INSTALAR Y CARGAR LOS PAQUETES NECESARIOS
install.packages("pracma")
install.packages("readxl")
install.packages("ggplot2")
install.packages("remotes")
remotes::install_github("andrewheiss/reconPlots")

library(readxl)
library(pracma)
library(ggplot2)
library(reconPlots)

#CARGA DE DOCUMENTOS Y CREACIÓN DE DATA.FRAME CON TODOS LOS PLANES

list.files("./plan")
archivos <- list.files("./plan")

planes <- data.frame()
for (leer in archivos) {
  nombre_archivo <- paste0("./plan/", leer)
  plan <- read_excel(nombre_archivo)
planes <- rbind(planes,plan)
}

columna <- rep(c("1","2","3"),c(61,61,61))

planes$Plan <- columna
planes <- as.data.frame(planes)

################################################################################
#FUNCIÓN PLAN 1
  
polyfit(seq(0,15,by=0.25), planes[planes$Plan == "1", 2 ], n=4)

funcionplan1 <- function(x) {
    0.0003062817*x^4 -0.0033059202*x^3  -0.0062609479*x^2 -0.0049915930*x + 
    2.9990706823
}


#Gráfico Plan 1
ggplot(planes[planes$Plan == "1", ], aes(trimestre, Retorno))+ 
  labs(title = "Plan 1",x= "Años", y="Retorno (%)") +  geom_point()+ 
  geom_function(fun=~ 0.0003062817*.x^4 -0.0033059202*.x^3 -0.0062609479*.x^2 
                -0.0049915930*.x + 2.9990706823, color="blue",lwd=1.2)+
  scale_x_continuous(breaks=seq(1,15,1))+scale_y_continuous(breaks = seq(1,6,1))

################################################################################
#FUNCIÓN PLAN 2

polyfit(seq(0,15,by=0.25), planes[planes$Plan == "2", 2 ], n=4)

funcionplan2 <- function(x) {
  -0.0003035228*x^4 +0.0020421733*x^3 +0.0204922165*x^2 -0.0078784722*x+ 
    4.0277001383
}

#Gráfico Plan 2

ggplot(planes[planes$Plan == "2",], aes(trimestre, Retorno))+ 
  labs(title = "Plan 2", x= "Años", y="Retorno (%)" ) +  geom_point()+
  geom_function(fun=~ -0.0003035228*.x^4 +0.0020421733*.x^3 +0.0204922165*.x^2 
                -0.0078784722*.x+ 4.0277001383, color="blue",lwd=1.2)+
  scale_x_continuous(breaks=seq(1,15,1))+scale_y_continuous(breaks = seq(1,6,1))

################################################################################ 
#FUNCIÓN PLAN 3

polyfit(seq(0,15,by=0.25), planes[planes$Plan == "3", 2 ], n=2)


funcionplan3 <- function(x) {
  -0.020027487*x^2  +0.001043184*x  + 4.987738410
}


#Gráfico Plan 3
ggplot(planes[planes$Plan == "3",  ], aes(trimestre, Retorno))+
  labs(title = "Plan 3",x= "Años", y="Retorno (%)") +  geom_point()+ 
  geom_function(fun=~-0.020027487*.x^2 +0.001043184*.x  +4.987738410 , 
                color="blue",lwd=1.2)+ scale_x_continuous(breaks=seq(1,15,1))+
  scale_y_continuous(breaks = seq(1,6,1))

################################################################################

#GRÁFICO DE LOS 3 PLANES

ggplot(planes, mapping= aes(x=trimestre, y=Retorno, color=Plan, size=Retorno))+
  geom_point(size=3)+ 
  labs(title = "Tasa de Retorno por Trimestre", x = "Años", y = "Retorno (%)")+
  theme_bw()+ scale_color_brewer(palette = "Pastel1")+ geom_function(
  fun=~0.0003062817*.x^4 -0.0033059202*.x^3-0.0062609479*.x^2 -0.0049915930*.x + 
  2.9990706823, color="red",lwd=1.2 )+geom_function(fun=~ -0.0003035228*.x^4 +
  0.0020421733*.x^3 +0.0204922165*.x^2-0.0078784722*.x+ 4.0277001383, color="blue", 
  lwd=1.2)+ geom_function(fun=~-0.020027487*.x^2  +0.001043184*.x  +4.987738410 ,
  color="green",lwd=1.2)+ scale_x_continuous(breaks=seq(1,15,1))+
  scale_y_continuous(breaks = seq(1,6,1))

################################################################################

#ANALISIS DE GRÁFICO     

#En el intervalo del 1er al 5to año, se observa que, el plan 3 es el que entrega
#un mayor retorno. Para saber en que trimestre se debe cambiar de plan tenemos 
# que encontrar en que punto se intersecta la curva del plan 3 con la curva del 
#plan 2.
# La curva del plan 2 representa el siguiente intervalo con mayor retorno.

curve_intersect(funcionplan3, funcionplan2, empirical=FALSE, domain = c(0, 5))

#hay que cambiarse, en el tercer trimestre del quinto del año, al plan numero 2, 
#dado que, el punto de intersección es en el año x = 4.81 que es mas cercano al
#año 4.75 (tercer semestre del quinto año) que a 5 que corresponde al 4to
#trimestre del 5to año.



#El siguiente cambio se observa entre el año 12 y 13, para saber exactamente en
#que trimestre se debe cambiar el plan, tenemos que obtener la intersección 
#entre el #plan 2 y el plan 1, dado que, posterior al año 13, el plan 1 ofrece 
#mayor retorno.

curve_intersect(funcionplan2, funcionplan1, empirical=FALSE, domain = c(5, 13))

#hay que cambiarse en el primer trimestre del año 14, ya que x = 12.91 es mas
#cercano a 13 (4to trimestre del año 13) que a 12.75 (3er trimestre del año 13) 











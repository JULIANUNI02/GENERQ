#Definir directorio de trabajo
setwd("D:/MAESTRIA/2019 I/hidroinformatica/trabajo final/info")

#Leer datos de precipitacion total
ppt<-read.csv("PPTT.csv",sep=";",dec=".")

#Leer datos de Matriz de variabilidad
matvar<-read.csv("MATRIZVAR.csv",sep=";",dec=".")

#Leer datos de Matriz de variabilidad

#Leer datos de temperatuta media
tm<-read.csv("TMED.CSV", sep=";",dec=".")

#Leer datos de Matriz de variabilidad
mattemp<-read.csv("MATRIZTEMP.csv",sep=";",dec=".")


#Extraer columnas con lo datos mensuales de ppt
ppt2<-ppt[1:50,5:16]

#Extraer columnas con los datos mensuales de Tm
tm2<-tm[,5:16]

#Extraer columnas de la matriz de variabilidad de ppt
matppt2<-matvar[1:50,5:16]

#Extraer columnas de la matriz de variabilidad de tm
mattm2<-mattemp[1:11,5:16]

#calcular promedios mensuales multianuales
prom_ppt<-colMeans(ppt2)
prom_tm<-colMeans(tm2)

#Graficar distribución mensual promedio
plot(prom_ppt,type="h",col="blue",xlab="meses", ylab="mm",
     main="Distribucion precipitacion mensual promedio  - ESTACION LLANGANUCO")
plot(prom_tm,type="o",col="red",xlab="meses", ylab="C",
     main="Distribucion temperatura mensual promedio  - ESTACION LLANGANUCO")

ppt_paron<-1.7241*4100^0.7501

#calcular precipitaciones mensuales de cuenca paron en base a matriz de vaiabilidad de Est. LLANGANUCO
ppt_mat_paron<-ppt_paron*matppt2

tpm_paron<--0.0077*4100 + 38.124

#calcular temperaturas medias mensuales de cuenca paron en base a matriz de vaiabilidad de Est. LLANGANUCO
tm_mat_paron<-tpm_paron*mattm2

A_paron<-48.04
prom_ppt_paron<-colMeans(ppt_mat_paron)
prom_tm_paron<-colMeans(tm_mat_paron)
pxt_paron<-colMeans(ppt_mat_paron)*colMeans(tm_mat_paron)
suma_pxt<-sum(pxt_paron)

#caudales generados por Metodo de Coutagne
promtm_anu_paron<-sum(prom_tm_paron)/12
lamda<-1/(0.8+0.14*promtm_anu_paron)

#verificacion del rango de P entre 1/8 ?? y 1/2??
valor_inf<-1/(8*lamda)
valor_sup<-1/(2*lamda)

#Como el lamda está dentro del rango, el déficit de Escurrimiento es
Defesc_paron<-0.2*0.035*promtm_anu_paron

Q_Lam_esc<-ppt_paron/1000 - Defesc_paron
Qmm_paron<-A_paron*1000*Q_Lam_esc*1000

#Caudal generado por metodo de Contagne
Q_gen_paron<-Qmm_paron/(365*24*3600)

#caudales generados por Metodo de Turc
L<-300+25*promtm_anu_paron+0.05*promtm_anu_paron^3
Dparon<-ppt_paron/(0.9+(ppt_paron/L)^2)^0.5
Qmm_paroturc<-ppt_paron-Dparon
V<-Qmm_paroturc*A_paron*1000
Q_gen_paronturc<-V/(365*24*3600)

#caudales generados por Metodo Curva Número
cn<-92
S<-254*(100/cn -1)
Esc_cn<-((prom_ppt_paron-0.2*S)^2)/(prom_ppt_paron+0.8*S)
Qanul_cn<-sum(Esc_cn)
Q_gen_paroncn<-Qanul_cn*A_paron*1000/(365*24*3600)

#Elección de Metodo de calaculos de caudales
#Los Metodos de Coutagne y Curva Numero dan resultados confiables y se calibrará con el caudal promedio registrado en Paron
prom_contagne_Cn<-(Q_gen_paroncn+Q_gen_paron)/2

#Caudal calibrado
Q_paron<-prom_contagne_Cn*1.37


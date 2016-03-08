temb <- function (planilla, med = NA, edadmin = 0, edadmax = 100, procedimiento = NA,
                   inicio = NA, fin = NA, M2 = FALSE, m = NA, export = "NO")
  
### CALCULA TASA DE EMBARAZO Y ABORTO EN FUNCION DE LOS MESES Y LAS GRAFICA ###
{
  require(gdata)


if (!(file.exists(paste(c("Planillas Fiv"), c(planilla), sep="/"))))
  stop("El archivo no existe, chequear mayusculas y extension o ubicacion de la planilla")
data<-read.xls(paste(c("Planillas Fiv"), c(planilla), sep="/"),stringsAsFactors = FALSE) #Levanta todos los datos de la planillas
data$Fecha<-strptime(data$Fecha, "%m/%d/%Y")


##CHEQUEA QUE LA PLANILLA SEA COHERENTE Y TIRA ALGUN WARNING## #Por ej, si hay mas apellidos que nro de ciclos

if (length(data$Nro) != length(data$Apellido[data$Apellido != ""])) warning("El Nro de ciclos no coincide con el numero de pacientes ")

##FILTROs##
data<-subset(data,Edad.Fem >= edadmin & Edad.Fem <= edadmax) ##Filtra por edad
if (!is.na(med)) 
  data<-subset(data, Medico == med) ##Filtra por medico
if (!is.na(procedimiento)[1])  ## Asi escrito para que el if no tire warning por pasarle un vector
  data<-subset(data, Proc %in% procedimiento )  ##Filtra por procedimiento
if(!is.na(inicio | fin)) 
  data<-subset(data, Fecha$mon >= inicio & Fecha$mon <= fin)
if (M2 == TRUE ) 
  data<-subset(data, MII >= 4)

##embclin <- length(data$C.sacos[!is.na(data$C.sacos) & data$C.sacos!=0])
##meses<-subset(data, Fecha$mon == )
c.transfer<-vector(mode = "numeric", length = 12)
embclin<-vector(mode = "numeric", length = 12)
abortos<-vector(mode = "numeric", length = 12)

for  (m in 0:11) {
datos<-subset(data, Fecha$mon >= m & Fecha$mon <= m)
c.transfer[m+1]<-length(datos$emb.transf[!is.na(datos$emb.transf) & datos$emb.transf!=0])
embclin[m+1]<-length(datos$C.sacos[!is.na(datos$C.sacos) & datos$C.sacos!=0])
abortos[m+1]<-length(datos$Aborto[!is.na(datos$Aborto & datos$Aborto!=0)])
}

t.emb <- embclin/c.transfer
t.aborto <- abortos/embclin

plot(1:12,t.emb, type = "b", col = "blue")

par(new = TRUE)
plot(1:12, t.aborto, axes = FALSE, ylab = "", type = "b", col = "red")
}


estgens <- function (planilla, med = NA, edadmin = 0, edadmax = 100, procedimiento = NA,
                     inicio = NA, fin = NA, M2 = FALSE , m = NA, export = "NO")


  ###TOMA LAS TABLAS EXCEL Y DEVUELVE UNA LISTA DE SUMAS Y TASAS DE INTERES#################  
  #################################   PARAMETROS ########################################### 
  ###INICIO Y FIN: NUMERO DEL 1 AL 12 REPRESENTANDO LOS MESES                           ####
  ###M2 ACTIVA UNA OPCION QUE DESCARTA LOS CICLOS CON MENOS DE 4 OOCITOS M2 RECUPERADOS ####
  ##########################################################################################
{
  
  require(gdata)
  
  #assign("data","data", envir= .GlobalEnv)            ## genera la variable data en el global enviroment
  
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
  

  
 # assign("sumas","sumas", envir= .GlobalEnv)            ## genera la variable data en el global enviroment
  sumas<-vector(mode="numeric",length=23)    ##Inicializo un vector donde se guardan los totales y sumas
  names(sumas)<-c("C.Iniciados","C.int.Rec.de.Oocitos","C.con.Rec.de.Oocitos",    ##Nombres de las sumas
                  "C.ovulos.ferti","C.con.Transfer","N.FoliculosAsp","N.Ovo.Rec","MII","MI",
                  "VG","ZF","Ovos.Ins","Ovos.Fert","Ovos.NoFert","Ovos.Multinucl","Emb.Cliv","Emb.Trf",
                  "Emba.Clinic","Nac.Vivos","Abortos","Emba.Ectop","Emba.Simpl","Emba.Multipl")
 sumas["C.Iniciados"]<-length(data$Apellido[data$Apellido != ""])   
  sumas["C.int.Rec.de.Oocitos"]<-sum(!is.na(data$Foliculos.Asp))
  sumas["C.con.Rec.de.Oocitos"]<-sum(!is.na(data$Foliculos.Asp))
  sumas["C.ovulos.ferti"]<-length(data$f..norm[!is.na(data$f..norm) & data$f..norm!=0])
  sumas["C.con.Transfer"]<-length(data$emb.transf[!is.na(data$emb.transf) & data$emb.transf!=0])
  sumas["N.FoliculosAsp"]<-sum(data$Foliculos.Asp, na.rm = TRUE)
  sumas["N.Ovo.Rec"]<-sum(data$Ovocitos.Recuperados, na.rm = TRUE)
  sumas["MII"]<-sum(data$MII, na.rm = TRUE)
  sumas["MI"]<- sum(data$MI, na.rm = TRUE)
  sumas["VG"]<-sum(data$VG, na.rm = TRUE)
  sumas["ZF"]<-sum(data$ZF, na.rm = TRUE)
  sumas["Ovos.Ins"]<-sum(data$o.ins.iny, na.rm = TRUE)
  sumas["Ovos.Fert"]<-sum(data$f..norm, na.rm = TRUE)
  sumas["Ovos.NoFert"]<-sum(data$no..Fert, na.rm = TRUE)
  sumas["Ovos.Multinucl"]<-sum(data$F.anor, na.rm = TRUE)
  sumas["Emb.Cliv"]<-sum(as.numeric(data$X48.hs), na.rm = TRUE)
  sumas["Emb.Trf"]<-sum(data$emb.transf, na.rm = TRUE)
  sumas["Emba.Clinic"]<-length(data$C.sacos[!is.na(data$C.sacos) & data$C.sacos!=0])
  sumas["Nac.Vivos"]<-length(data$Peso.1.BB[!is.na(data$Peso.1.BB)])
  sumas["Abortos"]<-length(data$Aborto[!is.na(data$Aborto)])
  sumas["Emba.Ectop"]<-c(0)
  sumas["Emba.Simple"]<-nrow(data[!is.na(data$Peso.1.BB) & is.na(data$Peso.2.BB),])
  sumas["Emba.Multiple"]<-nrow(data[!is.na(data$Peso.1.BB) & !is.na(data$Peso.2.BB),])    ### Calculo todos los valores que iran en 'sumas' 
  
  
 print(sumas)
  
 tasas<-vector(mode = "numeric", length=14) ##Inicializo un vector donde se guardan las tasas calculadas a partir de las sumas
 names(tasas)<-c("t.MII","t.Ovo.x.Foliculo.Asp","t.Fertilizacion","t.Clivaje","t.Emb.Clin.x.Ciclo.Asp",
                "t.Nac.Viv.x.Ciclo.Asp","t.Emb.Clin.x.Ciclo.Transf","t.Nac.Viv.x.Ciclo.Asp","t.Aborto",
                "t.Emb.Ectop","t.Emb.Simple","t.Multigest.","t.Implantacion","Edad Promedio") 
 
 tasas["t.MII"]<-sumas["MII"]/sumas["N.Ovo.Rec"] 
 tasas["t.Ovo.x.Foliculo.Asp"]<-sumas["N.Ovo.Rec"]/sumas["N.FoliculosAsp"]
 tasas["t.Fertilizacion"]<-sumas["Ovos.Fert"]/sumas["Ovos.Ins"]
 tasas["t.Clivaje"]<-sumas["Emb.Cliv"]/sumas["Ovos.Fert"]
 tasas["t.Emb.Clin.x.Ciclo.Asp"]<-sumas["Emba.Clinic"]/sumas["C.int.Rec.de.Oocitos"] ##Ojo,esta tasa no sirve para OD
 tasas["t.Nac.Viv.x.Ciclo.Asp"]<-sumas["Nac.Vivos"]/sumas["C.int.Rec.de.Oocitos"]   ##Idem ademas, las crio?
 tasas["t.Emb.Clin.x.Ciclo.Transf"]<-sumas["Emba.Clinic"]/sumas["C.con.Transfer"]
 tasas["t.Nac.Viv.x.Ciclo.Transf"]<-sumas["Nac.Vivos"]/sumas["C.con.Transfer"]
 tasas["t.Aborto"]<-(sumas["Abortos"]/sumas["Emba.Clinic"])
 tasas["t.Emb.Ectop"]<-c(0)
 tasas["t.Emb.Simple"]<-sumas["Emba.Simple"]/(sumas["Emba.Simple"]+sumas["Emba.Multiple"])
 tasas["t.Multigest."]<-sumas["Emba.Multiple"]/(sumas["Emba.Simple"]+sumas["Emba.Multiple"])
 tasas["t.Implantacion"]<-sumas["Emba.Clinic"]/sumas["Emb.Trf"]
 tasas["Edad Promedio"]<-mean(data$Edad.Fem)
 
 print(tasas)
 
 ##Exportar##
 if (export == "SI")
      {
      
       
       df.sumas<-data.frame(as.list(sumas)) ## convertir vector en data frame
       n<-c("Resumen Sumas")
       df.sumas<-as.data.frame(t(df.sumas[,-1]))  ##Transpone dataframe
       names(df.sumas) <- n
       if (!is.na(m)) names(df.sumas) <- m #Si se ejecuta infestgens m no es NA y se sobreescribe names
       write.table(df.sumas, "Reporte Sumas", sep = " = ", append = TRUE)
       
        df.tasas<-data.frame(as.list(tasas)) ## convertir vector en data frame
        n<-c("Resumen Tasas")
        df.tasas<-as.data.frame(t(df.tasas[,-1])) ## Transpone dataframe
        names(df.tasas) <- n
        if (!is.na(m)) names(df.tasas) <- m #Si se ejecuta infestgens m no es NA y se sobreescribe names
        write.table(df.tasas, "Reporte Tasas", sep = " = ", append = TRUE)
        
     }
        
        
 

 
 
 
  
}

infestgens <- function (planilla)
###Corre estgens.R pero con las distintas combinaciones de parametros####
{   

print("Edad < 35, FRESCO, 1er Semestre")
estgens(planilla, med = NA, edadmin = 0, edadmax = 34, procedimiento = c("ICSI","FIV"),
        inicio = 1, fin = 6, m = "Edad < 35, FRESCO, 1er Semestre", export = "SI")

print("34<Edad<40, FRESCO, 1er Semestre")
estgens(planilla, med = NA, edadmin = 35, edadmax = 39, procedimiento = c("ICSI","FIV"),
          inicio = 1, fin = 6, m = "34<Edad<40, FRESCO, 1er Semestre", export = "SI")

print("Edad>39, FRESCO, 1er Semestre")
estgens(planilla, med = NA, edadmin = 40, edadmax = 100, procedimiento = c("ICSI","FIV"),
        inicio = 1, fin = 6, m = "Edad>39, FRESCO, 1er Semestre", export = "SI")

print("Edad<35, OD, 1er Semestre")
estgens(planilla, med = NA, edadmin = 0, edadmax = 34, procedimiento = c("ICSI/OD","FIV/OD"),
        inicio = 1, fin = 6, m = "Edad<35, OD, 1er Semestre", export = "SI")

print("34<Edad<40, OD, 1er Semestre")
estgens(planilla, med = NA, edadmin = 35, edadmax = 39, procedimiento = c("ICSI/OD","FIV/OD"),
        inicio = 1, fin = 6, m = "34<Edad<40, OD, 1er Semestre", export = "SI")

print("Edad>39, OD, 1er Semestre")
estgens(planilla, med = NA, edadmin = 40, edadmax = 100, procedimiento = c("ICSI/OD","FIV/OD"),
        inicio = 1, fin = 6, m = "Edad>39, OD, 1er Semestre", export = "SI")

print ("Edad < 35, FRESCO, 2do Semestre")
estgens(planilla,med = NA, edadmin = 0, edadmax = 34, procedimiento = c("ICSI","FIV"),
        inicio = 7, fin = 12, m = "Edad < 35, FRESCO, 2do Semestre", export = "SI")

print ("34<Edad<40, FRESCO, 2do Semestre")
estgens(planilla, med = NA, edadmin = 35, edadmax = 39, procedimiento = c("ICSI","FIV"),
        inicio = 7, fin = 12, m = "34<Edad<40, FRESCO, 2do Semestre", export = "SI")

print ("Edad>39, FRESCO, 2do Semestre")
estgens(planilla, med = NA, edadmin = 40, edadmax = 100, procedimiento = c("ICSI","FIV"),
        inicio = 7, fin = 12, m = "Edad>39, FRESCO, 2do Semestre", export = "SI")

print ("Edad<35, OD, 2do Semestre")
estgens(planilla, med = NA, edadmin = 0, edadmax = 34, procedimiento = c("ICSI/OD","FIV/OD"),
        inicio = 7, fin = 12, m = "Edad<35, OD, 2do Semestre", export = "SI")

print ("34<Edad<40, OD, 2do Semestre")
estgens(planilla, med = NA, edadmin = 35, edadmax = 39, procedimiento = c("ICSI/OD","FIV/OD"),
        inicio = 7, fin = 12, m = "34<Edad<40, OD, 2do Semestre" , export = "SI")

print ("Edad>39, OD, 2do Semestre")
estgens(planilla, med = NA, edadmin = 40, edadmax = 100, procedimiento = c("ICSI/OD","FIV/OD"),
        inicio = 7, fin = 12, m = "Edad>39, OD, 2do Semestre", export = "SI")
  
  

}

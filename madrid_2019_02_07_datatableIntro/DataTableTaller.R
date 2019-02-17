#----------------------------------------------------------
# Data table Help
# Sara Rodríguez López
# colledit : http://collabedit.com/xcp5y
# Check: Cheat Sheet data table https://s3.amazonaws.com/assets.datacamp.com/blog_assets/datatable_Cheat_Sheet_R.pdf
#----------------------------------------------------------
# Comparaciones de tiempos de ejecución
# https://github.com/Rdatatable/data.table/wiki
# https://h2oai.github.io/db-benchmark/ # aqui compara con spark
# https://github.com/Rdatatable/data.table/wiki/Benchmarks-%3A-Grouping # dplyr, datatable pandas

# Intro
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html
# More
# https://cran.r-project.org/web/packages/data.table/vignettes/

# install.packages("data.table")
library(data.table)

# Reducimos el tiempo de crear el código ( repetimos menos el nombre de las variables)
# Reducimos el tiempo de computación (más rápido que dplyr!)
# Muy rápido a la hora de leer y escribir (paraleliza)
# Operacioens como filtros y joinings se paralelizan (con secondary keys)
# data.table es un data.frame --> puedes aplicarle las mismas funciones listas para los data.frame

# When the number of rows to print exceeds the global option datatable.print.nrows (default = 100), 
# it automatically prints only the top 5 and bottom 5 rows. 
# If you’ve had a lot of experience with data.frames, you may have found yourself waiting around while 
# larger tables print-and-page, sometimes seemingly endlessly. 

################################################
# TIEMPOS DE CARGA
# "https://github.com/arunsrinivasan/flights/wiki/NYCflights14/flights14.csv"
# DT 

system.time(flights <- fread("D:/TallerRladies/flights14.csv"))
class(flights)
# DF
system.time(flightsdf <- read.table("D:/TallerRladies/flights14.csv",sep = ",",header = TRUE))
class(flightsdf)

########################################################
# VISUALIZAR
flights # SI SON MENOS DE 100 TE LO MUESTRA ENTERO, SI NO 5 PRIEMRAS FILAS Y 5 ÚLTIMAS FILAS
flightsdf

########################################################
# ENTENDER LOS ROWNAMES
# Row numbers are printed with a : in order to visually separate the row number from the first column.

set.seed(4L) # En R para pasar un numeric (así se llaman float en R) a integer ponemos la L
dfPrueba <- data.frame(ShopID=c(rep("A",5),rep("B",2),rep("C",5)),
                       Price=sample(10:60, 12) ,
                       Units=c(1:8,seq(5,11,2))*10,
                       Class = c(rep("Food",2),rep("Drinks",5),rep("Clothes",2),rep("Food",3)),
                       row.names = sample(LETTERS[1:12]))
class(dfPrueba)
rownames(dfPrueba)

# Puedo indexar los dataframes por el nombre 
dfPrueba["C",]

# Y si lo convierto a datatable...
dtprueba<-as.data.table(dfPrueba)
dtprueba # los índices han desaparecido!
rownames(dtprueba)
# si lo fuerzo...
dtprueba<-as.data.table(dfPrueba,keep.rownames = TRUE)
dtprueba # me añadade una nueva columna!!!


# dt[i, j, by] 
# pensando en dataframe..
# i --> filas ( WHERE EN sql)
# j --> columnas ( SELECT en sql)
# by --> expresiones sobre las columnas ( GROUP BY en sql)

# Take dt, subset rows using i, then calculate j grouped by by
# i : subset the rows
# j : calculate this function
# by: grouped by this variables

# General form of data.table syntax
# DT[i, j, by]
# |  |  |
# |  |  --> grouped by what?
# |  -----> what to do?
# --------> on which rows?

# faq: https://cloud.r-project.org/web/packages/data.table/vignettes/datatable-faq.html

#----------------------------------------------------------
# Creation of a data table
#----------------------------------------------------------
set.seed(4L) # En R para pasar un numeric (así se llaman float en R) a integer ponemos la L
DT <- data.table(ShopID=c(rep("A",5),rep("B",2),rep("C",5)),
                 Price=sample(10:60, 12) ,
                 Units=c(1:8,seq(5,11,2))*10,
                 Class = c(rep("Food",2),rep("Drinks",5),rep("Clothes",2),rep("Food",3)))

class(DT)
# se acepta como dataframe en paquetes que no saben que es un datatable y funcionará ok con ellos!!

# We can change it to dataframe
df <- as.data.frame(DT)
DT2<-as.data.table(df)
class(DT2)
remove(DT2)
remove(df)
#----------------------------------------------------------
# Overview of the data table
#----------------------------------------------------------
dim(DT)
nrow(DT)# igual que en df
ncol(DT) # 
names(DT) # colnames(DT)
head(DT)
tail(DT)
str(DT)

# - attr(*, ".internal.selfref")=<externalptr> 
# attribute is used internally by data.table

tables() # Returns all datatables in memory
#----------------------------------------------------------
# 1. Selection of rows using i
#----------------------------------------------------------
# Select the first row
#df[1,] # umm y df[1].. devuelve la primera columna!
DT[1,]
DT[1]

# Select only the first 5 rows
#df[1:5,]
DT[1:5,]
DT[1:5]

# Select all rows but not 5
DT[-5]

DT[-c(2,5,9)]
DT[-(1:5)]

# Select all rows that have a specific value in  a column --> Clear syntaxi!
# df[df$Class == "Food",] o hago un attach(df) y entonces df[Class=="Food",]
DT[Class == "Food"] # ¿Qué es lo que estás metiendo ahí ? DT$Class=="Food"
DT[Class != "Food"]

# Select all rows that have value x or y in column a column
# df[df$Class %in% c("Food","Drinks"),]
# df[df$Price <30,]
DT[Class %in% c("Drinks","Food")]
DT[Class == "Drinks" | Class =="Food"]
DT[Price < 30]

# Order (internamente opitimizado)
DT[order(Price)]
DT[order(-Price)]
DT[order(Class,-Price)]

#----------------------------------------------------------
# 2. Manipulating on Columns in j
#----------------------------------------------------------

# 2.1. Selection of columns using j

# 2.1.1. Using columns names
# Using dataframe
#df$Price
#df[,c("Price"),drop=FALSE]

# Return a column as a vector (hay muchas formas de hacerlo..)
DT[,Price] ####### nos gusta esta
DT[,c(Price)]

# Return a column as a data table (hay muchas formas de hacerlo..)
# .() is an alias for list() which only applies within the data.table format
DT[,.(Price)] ########## nos gusta esta!
DT[,list(Price)]
DT[,c("Price")]

# Return two columns as a vector
DT[,c(ShopID,Price)]

# Return two columns as a data table...
DT[,.(ShopID,Price)]
DT[,list(ShopID,Price)]
DT[,c("ShopID","Price")]

# we can name columns as we would while creating a list.
DT[,.(tienda = ShopID, Precio = Price)]

# 2.1.2. Using columns names in string variables

# Return a column using a string variable
nameSel<-"Price"
DT[,nameSel] #  ERRRORR
DT[,.(nameSel)] # no es lo que quieres
DT[,get(nameSel)] # Vector
DT[,..nameSel]
DT[,nameSel,with=FALSE] # Data table ####
DT[,mget(nameSel)]# Data table
DT[,.(get(nameSel))] # Data table

# Return two columns using a string vector
nameSel<-c("Price","ShopID")
DT[,..nameSel] # DT
DT[,nameSel,with=FALSE] # DT ####
DT[,mget(nameSel)] # DT

# Pero RECUERDA nosotros nos centraremos en:
DT[,.(Price)]
DT[,.(Price,Class)]

# 2.1.1. Using columns index
DT[,1]


# 2.2. Manipulating on Columns in j

# sum price
DT[,sum(Price)] 
sum(DT[,.(Price)]) # resultado es un número
sum(DT[,Price])
DT[,.(sum(Price))]
DT[, .(sum_price = sum(Price))] # assign a name to the sum variable

# std Units
DT[,sd(Units)]

#Both
DT[,.(sum(Price),sd(Units))]
# With names
DT[,.(sumprice = sum(Price),sdUnits = sd(Units))]

# Select a column and create a variable ( the single value is recycled)
DT[,.(Class,sumPrice= sum(Price))]

# Combine i and j
DT[Class=="Food",.(sum(Price))] # DT[Class=="Food"]
#----------------------------------------------------------
# 3. Group by
#----------------------------------------------------------

# 3.1. Group by a variable

# Average the price per class
DT[,mean(Price),by=.(Class)]
DT[,.(mean(Price)),by=.(Class)]
DT[,.(meanPriceClass = mean(Price)),by=Class]


# Max price per Class and ShopID
DT[, .(max_price = max(Price)), by=.(Class, ShopID)]

# 3.2. Group by a variable and order 
DT[, .(max_price = max(Price)), keyby=.(Class, ShopID)]
# 3.3. Group by result of a function. # grouping variables in increasing order # no funciona el - para ordenar al revés


# Mean units available depending on price
DT[, .(avg_units = mean(Units)), by=.(Price > 25)]
DT[, .(avg_units = mean(Units)), by=.(Price > 25,Units>5)]
# Assign a name to the grouping
DT[, .(avg_units = mean(Units)), by=.(Price_larger_25 = Price >25)]

# 3.4. Filtering and grouping. 

DT[1:4,.(meanPriceClass = mean(Price)),by=.(Class)]
DT[!(Class %in% "Food"),.(meanPriceClass = mean(Price)),by=.(Class)]

# 3.5. Counting
DT[,length(Price),by=.(Class)]
DT[,.N,by=.(Class)]
DT[,.(count = .N),by=.(Class)]
DT[Class == "Food",.N]

### Special .N --> Think of .N as a variable for the number of instances.

DT[,.N] # Returns number of rows
DT[.N]  # Returns the last row
DT[.N-2] # Returns the last row minus two
DT[1:(.N-3)] 

# DT[, .(val = list(c(Class))), by = .(ShopID)]

#----------------------------------------------------------
# 5. Using .SD --> en j
# "Subset of Data" ~ (SD)
# piensa que SD tiene todas las columnas del datatable
#----------------------------------------------------------
DT[, print(.SD), by=.(ShopID)] # devuelve un grupo para cada elemento del by
# presentando todas las columnas salvo aquella por la que se ha hecho el by.
# The same as the above, using SDcols to select the columns
DT[, print(.SD),by=.(ShopID),.SDcols = c("Class","Price")]

# .SD contains all the columns except the grouping columns by default.

# Subset for each group
# First row per ShopID
DT[, .SD[1], by=ShopID]
# Most expensive item per shopID
DT[order(ShopID,-Price)][, .SD[1], by=.(ShopID)]
# Last three rows per ShopID
DT[, tail(.SD,3), by=.(ShopID)]
# Select the first and last row of each group
DT[, .SD[c(1,.N)], by=.(ShopID)]

# Unique values
DT[, lapply(.SD, uniqueN)]

# Same operation but grouping
DT[, lapply(.SD, uniqueN),by=ShopID]

# Same operation but selection columns
DT[, lapply(.SD, uniqueN),.SDcols=c("Price","Class")]
DT[, lapply(.SD, uniqueN),by=.(ShopID),.SDcols=c("Price","Class")]

# Mean value
DT[, lapply(.SD,mean), by=ShopID]
DT[, lapply(.SD,mean), by=ShopID,.SDcols = c("Price","Units")] # DT[,.(mean(Price),mean(Units)),by=ShopID]
DT[, lapply(.SD,mean), by=ShopID,.SDcols =sapply(DT, is.numeric) ] # sapply(DT, is.numeric)

#----------------------------------------------------------
# 4. Updating or creating new columns by reference
#     If you want to print the result in the screen add [] ###
#----------------------------------------------------------

# 4.1. New column with a specific value
# Create one column with value 1
DT[, value_1 := 1] # DT[, value_1 := 1][] 
# Remove new column by reference
DT[,value_1 := NULL]

# 4.2. New column which depends on other column
DT[, Price_discount := Price*0.75][]
DT[, Price := Price*0.75][] # updating price!
DT[, Price_discount := NULL]

DT[, avg_price_per_Class := mean(Price), by=.(Class)]

# 4.3. Columns using .N
DT[, index := 1:.N]
DT[, new_index := 1:5] # A warning message appears! Recycling!
DT[, new_index2 := .(1:nrow(DT))][]

# Create two columns one with ascending index and an other with descending index
DT[, c("asc_index", "des_index") := .(1:.N, .N:1)]

# Creating a column named as string value
colVar = "Unidad"
DT[, (colVar) := 1][]


# 4.4. Adding/updating a few columns
# OPTION 1
# DT[,c("V1","V2"):=list(round(exp(Units),2),LETTERS[1:12])]
# # OPTION 2
# DT[,c("V1","V2"):=.(round(exp(Units),2),LETTERS[1:12])]
# # OPTION 3
# DT[,':=' (V1 = round(exp(Units),2),V2 = LETTERS[1:12])]

# 4.5. Removing columns
DT

DT[, c("index", "new_index","new_index2", "asc_index", "des_index") := NULL]


#----------------------------------------------------------
# 6. Extras útiles
#----------------------------------------------------------

# Devuelve índice de las filas donde se cumple la condición
DT[,.I[ShopID%in% c("A","C")]]
# Primero hace el subset y sobre este devuelve los índices
DT[ShopID =="A",.I]

# Hacer operaciones con filas de antes y después
DT2 <- data.table(mes = c("E","F","M","A","MY"),Agua=1:5, Luz=1:5*10, Calefac=1:5*100)
DT2[ , LuzSig := shift(Luz, 1L, type="lead")][]
DT2[ , c("Agua-1","Agua-2") := .(shift(Agua, 1L, type="lag"),shift(Agua, 2L, type="lag"))][]

DT2[ , E := Agua > shift(Agua, 1L, type="lag")][]

DT2[ , Calefacsum := Reduce(`+`, shift(Calefac, 1:2,type = "lag",fill = 0))][]
DT2[ , Calefacsum := Reduce(`+`, shift(Calefac, 1:2,type = "lag"))][]


######Animate y lee sobre las keys

# Copy data tables
# Cuando es por valor, la información de la variable se almacenan en una dirección de memoria diferente 
# al recibirla en la funcion, por lo tanto si el valor de esa variable cambia no afecta la variable original,
# solo se modifica dentro del contexto de la función.

# Cuando es por referencia, la variable que se recibe como parámetro en la función apunta exactamente a la misma
# dirección de memoria que la variable original por lo que si dentro de la función se modifica su valor también 
# se modifica la variable original.

# https://www.rdocumentation.org/packages/data.table/versions/1.11.8/topics/copy

dt = data.table(a=c("A","B","C","B"),b=c(1,2,3,4))
dt2 = dt
dt2[,b := b+1]
dt2
dt

dt = data.table(a=c("A","B","C","B"),b=c(1,2,3,4))
dt2 = copy(dt)
dt2[,b := b+1]
dt2
dt

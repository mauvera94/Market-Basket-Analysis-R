

library("DBI")
library("RMySQL")
mydb = dbConnect(MySQL(), user='root', password='', dbname='proyectodafne', host='localhost')
dbListTables(mydb)
dbListFields(mydb, 'detalleventa')
res = dbSendQuery(mydb, "SELECT detalleventa.cDETVENT_idVenta, productos.cPROD_nombreProducto FROM detalleventa inner join productos on detalleventa.cDETVENT_idProducto = productos.cPROD_idProducto order by detalleventa.cDETVENT_idVenta")
data <- fetch(res, n=-1)


library(Matrix)
library(arules)
library(grid)
library(arulesViz)
library(datasets)

data<-split(data$cPROD_nombreProducto,data$cDETVENT_idVenta)
Txns<-as(data,"transactions")
summary(Txns)
Sys.sleep(5)


rules <- apriori(Txns, parameter = list(supp = 0.01, conf = 0.8,target="rules",minlen=2))
inspect(rules)
Sys.sleep(5)


plot(rules,method="graph",interactive=TRUE,shading=NA)
Sys.sleep(5)

itemFrequencyPlot(Txns, topN = 5)
Sys.sleep(5)

library(arulesViz)
plot(rules)
Sys.sleep(5)


########
rules<-apriori(data=Txns, parameter=list(supp=0.001,conf = 0.08), appearance = list(default="lhs",
rhs="AGUA CIEL"),control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

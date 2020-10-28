#purpose: read AIRCOA data from FEF and SPL http://www.eol.ucar.edu/homes/stephens/RACCOON/index.html
#author: Dave Moore
#Date: 03/31/2015

#data stored locally on my machine
FEFACO = read.table("AIRCOA//rdNCAR_FEF_most_recent.lin03312015" ,header=TRUE, stringsAsFactors = FALSE, na.strings=c('-999.990'))

FEFACO$CO21[FEFACO$CO21==-999.990]=NA

SPLACO = read.table("AIRCOA//rdNCAR_SPL_most_recent.lin03312015" ,header=TRUE, stringsAsFactors = FALSE,  na.strings=c('-999.990'))

SPLACO$CO21[SPLACO$CO21==-999.990]=NA


#store data in .Rdata file
save (FEFACO,SPLACO, file="Aircoa.rdata")


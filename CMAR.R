cmar = read.csv("/Users/sarahblaskey/Documents/Stats/Sharks/SharkTradeCMAR.csv")
head(cmar)
library(tidyverse)

sort(tapply(as.numeric(cmarex$Netweight), cmarex$Year,  FUN = sum, na.rm = TRUE))
sort(tapply(as.numeric(cmarex$Netweight), cmarex$Reporter,  FUN = sum, na.rm = TRUE))
cmarex = cmar[cmar$TradeFlow == "Export",]
cmarim = cmar[cmar$TradeFlow == "Import",]

costarica = cmar[cmar$Reporter == "Costa Rica",]
crex = costarica[costarica$TradeFlow == "Export",]
crim = costarica[costarica$TradeFlow == "Import",]


sum(as.numeric(cmarex$Netweight)) - sum(as.numeric(cmarim$Netweight))

(tapply(as.numeric(cmarex$Netweight), cmarex$Reporter,  FUN = sum, na.rm = TRUE))-(tapply(as.numeric(cmarim$Netweight), cmarim$Reporter,  FUN = sum, na.rm = TRUE))

cmarex =  unite(cmarex, "key", c("Reporter","Year"), remove = FALSE)
cmarim =  unite(cmarim, "key", c("Reporter","Year"), remove = FALSE)

ex = data.frame(tapply(as.numeric(cmarex$Netweight), cmarex$key,  FUN = sum, na.rm = TRUE))
ex <- add_rownames(ex, "key")
im = data.frame(tapply(as.numeric(cmarim$Netweight), cmarim$key,  FUN = sum, na.rm = TRUE))
im <- add_rownames(im, "key")

imex = merge(ex, im, BY = key) 
imex$exports = imex$tapply.as.numeric.cmarex.Netweight...cmarex.key..FUN...sum..na.rm...TRUE. - imex$tapply.as.numeric.cmarim.Netweight...cmarim.key..FUN...sum..na.rm...TRUE.
(sum(as.numeric(cmarex$Netweight)))-(sum(as.numeric((cmarim$Netweight))))
(tapply(as.numeric(cmarex$Netweight), cmarex$Reporter,  FUN = sum, na.rm = TRUE))-(tapply(as.numeric(cmarim$Netweight), cmarim$Reporter,  FUN = sum, na.rm = TRUE))



c = c("Costa Rica", "Honduras", "El Salvador", "Nicaragua", "Panama", "Guatemala", "Ecuador", "Colombia")
regionaltrade = cmar[(cmar$Reporter %in% c) & (cmar$Partner %in% c),]
nonregionaltrade = setdiff(cmar, regionaltrade)

cmar$priceperkilo = cmar$TradeValue/cmar$Netweight

tapply(as.numeric(cmar$priceperkilo), cmar$CommodityCode, FUN = mean, na.rm = TRUE)

exports = nonregionaltrade[nonregionaltrade$TradeFlow == "Export",]

tapply(as.numeric(exports$Netweight), exports$Year, FUN = sum)
sort(tapply(as.numeric(exports$Netweight), exports$Reporter, FUN = sum))
sort(tapply(as.numeric(imports$Netweight), imports$Reporter, FUN = sum))

tapply(as.numeric(exports$TradeValue), exports$Year, FUN = sum, na.rm = TRUE)/(tapply(as.numeric(exports$Netweight), exports$Year, FUN = sum, na.rm = TRUE))
tapply(as.numeric(imports$TradeValue), imports$Reporter, FUN = sum)/(tapply(as.numeric(imports$Netweight), imports$Reporter, FUN = sum))


tapply(as.numeric(exports$TradeValue), exports$Year, FUN = sum, na.rm = TRUE)

imports = nonregionaltrade[nonregionaltrade$TradeFlow == "Import",]
exports =  unite(exports, "key", c("ReporterCode","PartnerCode"), remove = FALSE)
imports =  unite(imports, "key", c("PartnerCode","ReporterCode"), remove = FALSE)
sort(table(exports$key))
exkeys = unique(exports$key)
imkeys = unique(imports$key)
x = setdiff(exkeys, imkeys)

exportsclean = exports[exports$key %in% x,]
exportsdiff = setdiff(exports, exportsclean)

tapply(as.numeric(exports$Netweight), exports$Year,  FUN = sum, na.rm = TRUE)

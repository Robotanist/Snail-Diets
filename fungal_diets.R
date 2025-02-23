library(readxl)
X120920WMits_zotus_fa_fungi_species <- read_excel("2023 Kaala Snail Diets Research/ITS data for 2018-2020 Mt Kaala/120920WMits-zotus.fa.fungi.species.xlsx", 
                                                  sheet = "species_Percentages")

fungi <- X120920WMits_zotus_fa_fungi_species

values<- as.matrix(fungi[-1])

barplot.default(values)

barplot.default(values, xlab = "SAMPLE", ylab = "PERCENT",
                legend.text =fungi$species)

barplot.default(values, xlab = "SAMPLE", ylab = "PERCENT")


# terrain colors
barplot.default(values, xlab = "", ylab = "PERCENT", names.arg=colnames(fungi[-1]), las=2,  cex.names = 0.45, col= terrain.colors(1184))

# fucking legend, skip it
legend("right", 
       legend = fungi$species, 
       pch = 20, 
       col = terrain.colors(1184),cex=0.05)


#############################################################
# Subsetted Data No Cibotium spp. or Metrosiderous polymorpha
preferred <- as.matrix(fungi_preferred_hosts[-1])

# terrain colors
barplot.default(preferred, xlab = "", ylab = "PERCENT", names.arg=colnames(fungi_preferred_hosts[-1]), las=2,  cex.names = 0.45, col= terrain.colors(1184))
# legend, skip it this time.
legend("right", 
       legend = fungi_preferred_hosts$species, 
       pch = 15, 
       col = terrain.colors(1184),cex=1)
################################################################
# Further subset and filter negative controls out, most samples will not add up to 100 percent now

subset<- as.matrix(fungi_preferred_subset[-1])


# terrain colors
barplot.default(subset, xlab = "", ylab = "PERCENT", names.arg=colnames(fungi_preferred_subset$species[-1]), las=2,  cex.names = .5, col= terrain.colors(1184))
# legend, skip it
legend("right", 
       legend = fungi_preferred_subset$species, 
       pch = 15, 
       col = terrain.colors(1184),cex=1)


# Further subset by plant species

# Clermontia host pick correct sheet "Oha wai"

fungi_preferred_subset <- read_excel("2023 Kaala Snail Diets Research/fungi_preferred_subset.xls", 
                                     +     sheet = "Oha wai")

clermontia <- as.matrix(fungi_preferred_subset[-1])

# terrain colors
barplot.default(clermontia, xlab = "", ylab = "PERCENT", names.arg=colnames(fungi_preferred_subset[-1]), las=2,  cex.names = .5, col= terrain.colors(1184))
# legend, skip it
legend("right", 
       legend = fungi_preferred_subset$species, 
       pch = 15, 
       col = terrain.colors(1184),cex=1)

# go to top 10 sheet

clermontia_top10 <- as.matrix(fungi_preferred_subset[-1])

# terrain colors
barplot.default(clermontia_top10, xlab = "", ylab = "PERCENT", names.arg=colnames(fungi_preferred_subset[-1]), las=2,  cex.names = .5, col= terrain.colors(40))

legend("topleft", 
       legend = fungi_preferred_subset$species, 
       pch = 15, 
       col = terrain.colors(40),cex=.2)

# Ilex host pick correct sheet "Kawau"

ilex <- as.matrix(fungi_preferred_subset[-1])

# terrain colors
barplot.default(ilex, xlab = "", ylab = "PERCENT", names.arg=colnames(fungi_preferred_subset[-1]), las=2,  cex.names = .5, col= terrain.colors(1184))

# go to top 10 sheet

ilex_top10 <- as.matrix(fungi_preferred_subset[-1])

# terrain colors
barplot.default(ilex_top10, xlab = "", ylab = "PERCENT", names.arg=colnames(fungi_preferred_subset[-1]), las=2,  cex.names = .5, col= terrain.colors(60))

legend("topleft", 
       legend = fungi_preferred_subset$species, 
       pch = 15, 
       col = terrain.colors(60),cex=.1)

# Hydrangea host pick correct sheet "Kanawao"

hydrangea <- as.matrix(fungi_preferred_subset[-1])

# terrain colors
barplot.default(hydrangea, xlab = "", ylab = "PERCENT", names.arg=colnames(fungi_preferred_subset[-1]), las=2,  cex.names = .5, col= terrain.colors(1184))

# go to top 10 sheet

hydrangea_top10 <- as.matrix(fungi_preferred_subset[-1])

# terrain colors
barplot.default(hydrangea_top10, xlab = "", ylab = "PERCENT", names.arg=colnames(fungi_preferred_subset[-1]), las=2,  cex.names = .5, col= terrain.colors(60))

legend("topleft", 
       legend = fungi_preferred_subset$species, 
       pch = 15, 
       col = terrain.colors(60),cex=.12)

# go to all preferred top 10 sheet

preferred_top10 <- as.matrix(fungi_preferred_subset[-1])

# terrain colors
barplot.default(preferred_top10, xlab = "", ylab = "PERCENT", names.arg=colnames(fungi_preferred_subset[-1]), las=2,  cex.names = .5, col= terrain.colors(102))

legend("topright", 
       legend = fungi_preferred_subset$species, 
       pch = 15, 
       col = terrain.colors(102),cex=.08)





       

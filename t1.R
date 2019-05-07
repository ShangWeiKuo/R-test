# ---------
#  25-2
# ---------
# ---------------------------------------------------------- #

indicators <- c("BX.KLT.DINV.WD.GD.ZS", "NY.GDP.DEFL.KD.ZG",
                "NY.GDP.MKTP.CD", "NY.GDP.MKTP.KD.ZG",
                "NY.GDP.PCAP.CD", "NY.GDP.PCAP.KD.ZG",
                "TG.VAL.TOTL.GD.ZS")

library(WDI)

# ?N?Ҧ????a???????ܼƪ????T???X???J?C??
# ???O?Ҧ????a?????C?ӫ????ܼƪ????T
# ?@?ǰ??a?????L????????
wbInfo <- WDI(country="all", indicator=indicators, start=2011,
              end=2011, extra=TRUE)

# ?NAggregates???T??????
wbInfo <- wbInfo[wbInfo$region != "Aggregates", ]

# ?N?Ҧ??????ܼƬ?NA?????a??????
wbInfo <- wbInfo[which(rowSums(!is.na(wbInfo[, indicators])) > 0), ]

# ?Niso???򥢭Ȫ????Ʋ?????
wbInfo <- wbInfo[!is.na(wbInfo$iso2c), ]

# ---------------------------------------------------------- #

# ?ѩ??ڭ̤??̾ڰ??a???��s
# ?]?w???ƦW?٥i?H???ڭ̪??D???Ʃ??ݰ??a
rownames(wbInfo) <- wbInfo$iso2c

# ???s?]?Ƥưϰ?(region),???J(income)?M?ɶU(lending)
# ?o?˥??̪?level???????ܤƳ????Q?Ҷq?b??
wbInfo$region <- factor(wbInfo$region)
wbInfo$income <- factor(wbInfo$income)
wbInfo$lending <- factor(wbInfo$lending)

# ---------------------------------------------------------- #

# ???X?n?O?d??????
keep.cols <- which(!names(wbInfo) %in% c("iso2c", "country", "year",
                                         "capital", "iso3c"))
# ?��s
wbPam <- pam(x=wbInfo[, keep.cols], k=12, keep.diss=TRUE,
             keep.data=TRUE)

# ????medoid?[????
wbPam$medoids

# ø?s???v??
plot(wbPam, which.plots=2, main="")

# ---------------------------------------------------------- #

download.file(url="http://jaredlander.com/data/worldmap.zip",
              destfile="data/worldmap.zip", method="curl")

# ---------------------------------------------------------- #

unzip(zipfile = "data/worldmap.zip", exdir = "data")

# ---------------------------------------------------------- #

library(maptools)
world <- readShapeSpatial(
         "data/world_country_admin_boundary_shapefile_with_fips_codes.shp"
         )
head(world@data)

# ---------------------------------------------------------- #

library(plyr)
world@data$FipsCntry <- as.character(
  recode(world@data$FipsCntry,
           AU="AT", AS="AU", VM="VN", BM="MM", SP="ES",
           PO="PT", IC="IL", SF="ZA", TU="TR", IZ="IQ",
           UK="GB", EI="IE", SU="SD", MA="MG", MO="MA",
           JA="JP", SW="SE", SN="SG")
)


# ---------------------------------------------------------- #

# ?ξ??ƦW?٫إߤ@??id????
world@data$id <- rownames(world@data)

#?⥦?ഫ??data.frame
library(broom)
world.df <- fortify(world, region = "id")
head(world.df)

# ---------------------------------------------------------- #

world.df <- join(world.df,
                 world@data[, c("id", "CntryName", "FipsCntry")],
                 by="id")
head(world.df)

clusterMembership <- data.frame(FipsCntry=names(wbPam$clustering),
                                Cluster=wbPam$clustering,
                                stringsAsFactors=FALSE)
head(clusterMembership)

world.df <- join(world.df, clusterMembership, by="FipsCntry")
world.df$Cluster <- as.character(world.df$Cluster)
world.df$Cluster <- factor(world.df$Cluster, levels=1:12)

# ---------------------------------------------------------- #

ggplot() +
   geom_polygon(data=world.df, aes(x=long, y=lat, group=group,
                                   fill=Cluster, color=Cluster)) +
   labs(x=NULL, y=NULL) + coord_equal() +
   theme(panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(),
         axis.text.x=element_blank(), axis.text.y=element_blank(),
         axis.ticks=element_blank(), panel.background=element_blank())

# ---------------------------------------------------------- #

wbPam$clusinfo

# ---------------------------------------------------------- #
# ---------
#  25-2
# ---------
# ---------------------------------------------------------- #

indicators <- c("BX.KLT.DINV.WD.GD.ZS", "NY.GDP.DEFL.KD.ZG",
                "NY.GDP.MKTP.CD", "NY.GDP.MKTP.KD.ZG",
                "NY.GDP.PCAP.CD", "NY.GDP.PCAP.KD.ZG",
                "TG.VAL.TOTL.GD.ZS")

library(WDI)

# 將所有國家的指標變數的資訊抽出放入列表
# 不是所有國家都有每個指標變數的資訊
# 一些國家完全無任何資料
wbInfo <- WDI(country="all", indicator=indicators, start=2011,
              end=2011, extra=TRUE)

# 將Aggregates資訊移除掉
wbInfo <- wbInfo[wbInfo$region != "Aggregates", ]

# 將所有指標變數為NA的國家移除掉
wbInfo <- wbInfo[which(rowSums(!is.na(wbInfo[, indicators])) > 0), ]

# 將iso為遺失值的橫排移除掉
wbInfo <- wbInfo[!is.na(wbInfo$iso2c), ]

# ---------------------------------------------------------- #

# 由於我們不依據國家做分群
# 設定橫排名稱可以讓我們知道橫排所屬國家
rownames(wbInfo) <- wbInfo$iso2c

# 重新因數化區域(region),收入(income)和借貸(lending)
# 這樣它們的level有任何變化都能被考量在內
wbInfo$region <- factor(wbInfo$region)
wbInfo$income <- factor(wbInfo$income)
wbInfo$lending <- factor(wbInfo$lending)

# ---------------------------------------------------------- #

# 找出要保留的直排
keep.cols <- which(!names(wbInfo) %in% c("iso2c", "country", "year",
                                         "capital", "iso3c"))
# 分群
wbPam <- pam(x=wbInfo[, keep.cols], k=12, keep.diss=TRUE,
             keep.data=TRUE)

# 顯示medoid觀測值
wbPam$medoids

# 繪製側影圖
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

# 用橫排名稱建立一個id直排
world@data$id <- rownames(world@data)

#把它轉換成data.frame
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
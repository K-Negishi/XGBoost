# ATTED-IIのマイクロアレイデータから、任意の実験データを抽出 ---------------------------------------
# 20181118
# Kohei
#



# 使用するパッケージ ---------------------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(data.table)
library(stringr)



# 使用するデータの読み込みと簡単な加工 ------------------------------------------------------

# 遺伝子発現データ
# ATTED-II(http://atted.jp/download.shtml)
GeneExp.v3 <- read_delim("Input/ATTED2/GeneExp_v3",
                         delim = "\t", col_names = T)
GeneExp.v3 <- as.data.frame(GeneExp.v3)
rownames(GeneExp.v3) <- toupper(GeneExp.v3[,1])
GeneExp.v3 <- GeneExp.v3[,-1]
dim(GeneExp.v3)
head(GeneExp.v3[,1:5])


# 実験条件のデータ
Des.GeneExp.v3 <- read_delim("Input/ATTED2/Description_GeneExp_v3_formatted.txt",
                             delim = "\t", col_names = T)
Des.GeneExp.v3 <- as.data.frame(Des.GeneExp.v3[,-1])
head(Des.GeneExp.v3)


# 抽出したい実験条件データ(先生からもらった)
int.conditions <- read_delim("Input/ATTED2/Microarray_info_formatted.txt",
                             delim = "\t", col_names = T) %>%
  rename(rep.id = 'reprecate ID')







# 遺伝子発現データと実験条件データを合わせる ---------------------------------------------------
GeneExp.v3 <- GeneExp.v3 %>%
  t() %>%
  as.data.frame()
GeneExp.v3 <- data.frame(repID=rownames(GeneExp.v3),
                         GeneExp.v3) %>%
  as_tibble()
GeneExp.v3$repID <- Des.GeneExp.v3$reprecateID[match(Des.GeneExp.v3$headerofGeneExpfile, GeneExp.v3$repID)]
dim(GeneExp.v3)
head(GeneExp.v3[,1:5])




# 任意の実験条件のデータのみを抽出する ------------------------------------------------------
t.GeneExp.v3 <- GeneExp.v3 %>%
  filter(repID %in% int.conditions$rep.id)
tmp <- t.GeneExp.v3 %>% 
  select(-repID) %>%
  transpose() %>% 
  as_tibble() %>% 
  mutate(AGI = colnames(t.GeneExp.v3)[-1]) %>% 
  select(AGI, 1:ncol(.))
colnames(tmp) <- c("AGI", t.GeneExp.v3$repID)

t.GeneExp.v3 <- tmp





# データの保存 ------------------------------------------------------------------
write.table(t.GeneExp.v3,
            file = "Output/Microarray_Bioinfo2.txt",
            sep = "\t",
            quote = F, col.names = T, row.names = F, append = F)






# Motifデータ(DAP)の前処理 -------------------------------------------------------
# 20181114
# Kohei
# Motifデータをまとめる
#   TF(Motif)でフラグをたてる & TF familyでフラグをたてる




# 使用するパッケージ ---------------------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(stringr)



# 使用するデータの読み込み ----------------------------------------------------------------
# Cistrome and Epicistrome Features Shape the Regulatory DNA Landscape.
# Plant Cistrome Database(http://neomorph.salk.edu/dap_web/pages/browse_table_aj.php)
t.dir <- "Input/dap_data_v4/genes/"
t.dir.full <- list.files(paste0(t.dir, list.files(path = t.dir)), full.names = T)

t.tf <- t.dir.full %>% 
  str_remove("Input/dap_data_v4/genes/") %>% 
  str_split("/") %>% 
  unlist() %>% 
  matrix(byrow = T, ncol = 2) %>% 
  as_tibble() %>% 
  select(V1) %>% 
  unlist() %>% 
  str_split("_") %>% 
  unlist() %>% 
  matrix(byrow = T, ncol = 2) %>% 
  as_tibble() %>% 
  select(V1) %>% 
  unlist()
total <- length(t.dir.full)
t.file.all <- c()
for(i in 1:total){
  t.file <- read_delim(file = paste0(t.dir.full[i], "/chr1-5/chr1-5_GEM_events.nS_targets.txt"),
                       delim = "\t", col_names = TRUE) %>% 
    mutate(tf.family = rep(t.tf[i], times = nrow(.)))
  t.file.all <- bind_rows(t.file.all, t.file)
  
  print(total - i)
}

tmp <- t.file.all %>% 
  mutate(dupli = paste0(tf.at_id, target.at_id, tf.family))
length(tmp$dupli) == length(unique(tmp$dupli))
length(tmp$dupli)
length(unique(tmp$dupli))

t.file.all <- tmp %>% 
  distinct(dupli, .keep_all = TRUE) %>%   # http://a-habakiri.hateblo.jp/entry/2016/11/29/215013
  select(-dupli)





# データの整形 ------------------------------------------------------------------
# TF Family, TF, Targetの3列
motif.df <- t.file.all %>% 
  rename(tf = tf.at_id, target = target.at_id) %>% 
  select(tf.family, tf, target) %>% 
  mutate(tf = toupper(tf), target = toupper(target)) %>% 
  as_tibble()


# 
motif.df$tf.family <- as.factor(motif.df$tf.family)
motif.df$tf <- as.factor(motif.df$tf)

tf.family.num <- motif.df %>%
  group_by(target, tf.family) %>%
  summarise(count = n()) %>%
  spread(tf.family, count, fill=0)

tf.family <- tf.family.num
tf.family[tf.family > 0] <- 1



tf <- motif.df %>%
  group_by(target, tf) %>%
  summarise(count = n()) %>%
  spread(tf, count, fill=0)





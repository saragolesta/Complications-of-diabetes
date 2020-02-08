library(tidyverse)
library(factoextra)


d = read.csv('diabetic_data.csv')
d2 = read.csv('combined.csv')
levels(d$diag_1)[levels(d$diag_1) == "?"] = "unknown"
levels(d$diag_2)[levels(d$diag_2) == "?"] = "unknown"
levels(d$diag_3)[levels(d$diag_3) == "?"] = "unknown"

d %>% group_by(diag_1) %>% summarize(count = n()) %>% arrange(-count)
d %>% group_by(readmitted) %>% summarize(count = n())

diag_1_df = data.frame(matrix(ncol = 717, nrow = 11357))
colnames(diag_1_df) = paste("ID", levels(d$diag_1), sep = "")
diag_1_df = diag_1_df %>% select(-ID428)

for (i in 1:716){
  diag_1_df[,i] = ifelse(d[d$readmitted == "<30",]$diag_1 == 
                           str_remove(colnames(diag_1_df)[i], "ID"), 1, 0)
}

sum(diag_1_df$ID110)
d %>% group_by(diag_1, readmitted) %>% summarize(count = n()) %>% 
  filter(diag_1 == "110")

dummy_variables = function(ref, numcol, numrow, data){
  df = data.frame(matrix(ncol = numcol, nrow = numrow))
  colnames(df) = paste("ID", levels(data), sep = "")
  df = df %>% select(-as.factor(paste("ID", as.character(ref), sep = "")))
  return(df)
}

as.factor(paste("ID", as.character(428), sep = ""))

dummy_variables(ref = 428, numcol = 717, 
                numrow = 11357, data = d$diag_1)


pca = prcomp(diag_1_df, scale = F)
fviz_eig(pca)
fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("gray20", "black"),
             repel = TRUE)
contributions = get_pca_var(pca)$contrib[, 1:2]
contributions["ID786", ]
distance = sqrt(contributions[,1]^2 + contributions[,2]^2)
contributions = cbind(contributions, distance)

head(contributions[order(contributions[,3], decreasing = T),], 10)

d %>% filter(readmitted == "<30") %>% group_by(diag_1) %>% 
  summarize(count = n()) %>% 
  arrange(-count)

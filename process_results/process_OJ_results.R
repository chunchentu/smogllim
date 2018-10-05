library(dplyr)
library(ggplot2)
data_name = "OJ_results.Rdata"
load(data_name)
K = 5
Lw = 8
temp = smogllim_df %>% filter(K==K & Lw==Lw & data_type=="test") %>%
    group_by(index) %>% summarize(mse = mean(SE))

temp = smogllim_df %>% filter(Lw==Lw & data_type=="test") %>%
    group_by(K, dropTh) %>% summarize(mse = mean(SE))
temp$K = factor(temp$K)
ggplot(temp) + geom_line(aes(dropTh, mse, color=K))


temp = smogllim_df %>% filter(Lw==Lw & data_type=="test" & cvID<=10 & K==5) %>%
    group_by(K, cvID, dropTh) %>% summarize(mse=mean(SE))
temp$cvID = factor(temp$cvID)
ggplot(temp) + geom_line(aes(dropTh, mse, color=cvID))

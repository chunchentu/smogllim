library(dplyr)
library(ggplot2)
data_name = "OJ_results.Rdata"
load(data_name)
K = 5
Lw = 8
temp = smogllim_df %>% filter(K==K & Lw==Lw & data_type=="test") %>%
    group_by(index) %>% summarize(mse = mean(SE))

temp = smogllim_df %>% filter(Lw==Lw & data_type=="test" & K %in% c(5, 10, 15)) %>%
    group_by(K, dropTh) %>% summarize(mse = mean(SE))
temp$K = as.factor(temp$K)
ggplot(temp) %>% geom_line(aes(x=dropTh, y=mse))


target_id = 1
temp = smogllim_df %>% filter(Lw==Lw & data_type=="test" & index==target_id)
temp$Lw = factor(temp$Lw)
ggplot(temp) + geom_line(aes(dropTh, SE, color=Lw))


temp = smogllim_df %>% filter(Lw==Lw & data_type=="test" & K %in% c(5, 10, 15)) %>%
    group_by(K, dropTh) %>% summarize(cnt=n())


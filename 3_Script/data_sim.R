
library(tidyverse)
library(faux)
set.seed(123)
Distractor_sim <- sim_design(
  n = 50, # 50 subjects
  #contain two factor
  within = list(Type = c("Self", "Other", "Neutral"), Position = c("High", "Low")), # 生成被试内因子的列表
  
  mu = c(self_high = 680, self_low = 900, other_high = 650, other_low=780, neutral_high=600, neutral_low=690), # 设置变量的均值
  
  sd = c(20, 30,25, 35, 28, 40), # 设置变量的标准差
  
  long = TRUE,  # 输出长数据形式
  
  dv = "score", # 因变量命名为score
  plot = FALSE   # 显示图
)
Distractor_sim %>% 
  group_by(Type, Position) %>% 
  summarise(m = mean(score), 
            sd = sd(score)) %>% 
  arrange()



Non_distractor <- sim_design(
  n = 50, # 生成50名被试
  within = list(Type = c("None")), # 生成被试内因子的列表
  
  mu = c(a=500), # 设置变量的均值
  
  sd = c(a=20), # 设置变量的标准差
  
  long = TRUE,  # 输出长数据形式
  
  dv = "score", # 因变量命名为score
  plot = FALSE   # 显示图
) %>% 
  mutate(Position = "None") %>% 
  select(id, Type, Position, score)


all <- rbind(Distractor_sim, Non_distractor)
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}


errbar_lims <- all %>% 
  group_by(Type, Position) %>% 
  summarize(mean  = mean(score), 
            se    = sd(score) / sqrt(n()), 
            upper = mean + se, 
            lower = mean - se) %>% mutate(x_axis = case_when(
              Type == "Self" & Position == "High" ~ 0.5, 
              Type == "Other" & Position == "High" ~ 1, 
              Type == "Neutral" & Position == "High" ~ 1.5, 
              Type == "Self" & Position == "Low" ~ 3.5, 
              Type == "Other" & Position == "Low" ~ 4, 
              Type == "Neutral" & Position == "Low" ~ 4.5,
              Type == "None" ~ 6.5
            ))


plot_dat <- all %>% mutate(x_axis = case_when(
  Type == "Self" & Position == "High" ~ 0.5, 
  Type == "Other" & Position == "High" ~ 1, 
  Type == "Neutral" & Position == "High" ~ 1.5, 
  Type == "Self" & Position == "Low" ~ 3.5, 
  Type == "Other" & Position == "Low" ~ 4, 
  Type == "Neutral" & Position == "Low" ~ 4.5,
  Type == "None" ~ 6.5
))

ggplot(plot_dat) + 
  geom_violin(data = plot_dat %>% filter(x_axis == 0.5), aes(x=x_axis, y=score, fill = "Self"), width=0.5) + 
  geom_errorbar(data=errbar_lims%>% filter(x_axis == 0.5), 
                aes(x = x_axis, ymax = upper, ymin = lower, group = Type), 
                stat = 'identity', position = position_dodge(width = 0.1), width = 0.1) + 
  geom_violin(data = plot_dat %>% filter(x_axis == 1), aes(x=x_axis, y=score, fill = "Other"), width=0.5) + 
  geom_errorbar(data=errbar_lims%>% filter(x_axis == 1), 
                aes(x = x_axis, ymax = upper, ymin = lower, group = Type), 
                stat = 'identity', position = position_dodge(width = 0.1), width = 0.1) + 
  geom_violin(data = plot_dat %>% filter(x_axis == 1.5), aes(x=x_axis, y=score, fill = "Neutral"), width=0.5) + 
  geom_errorbar(data=errbar_lims%>% filter(x_axis == 1.5), 
                aes(x = x_axis, ymax = upper, ymin = lower, group = Type), 
                stat = 'identity', position = position_dodge(width = 0.1), width = 0.1) + 
  geom_violin(data = plot_dat %>% filter(x_axis == 3.5), aes(x=x_axis, y=score, fill = "Self"),width=0.5) + 
  geom_errorbar(data=errbar_lims%>% filter(x_axis == 3.5), 
                aes(x = x_axis, ymax = upper, ymin = lower, group = Type), 
                stat = 'identity', position = position_dodge(width = 0.1), width = 0.1) + 
  geom_violin(data = plot_dat %>% filter(x_axis == 4), aes(x=x_axis, y=score, fill = "Other") ,width=0.5) + 
  geom_errorbar(data=errbar_lims%>% filter(x_axis == 4), 
                aes(x = x_axis, ymax = upper, ymin = lower, group = Type), 
                stat = 'identity', position = position_dodge(width = 0.1), width = 0.1) +
  geom_violin(data = plot_dat %>% filter(x_axis == 4.5), aes(x=x_axis, y=score, fill = "Neutral"),width=0.5) + 
  geom_errorbar(data=errbar_lims%>% filter(x_axis == 4.5), 
                aes(x = x_axis, ymax = upper, ymin = lower, group = Type), 
                stat = 'identity', position = position_dodge(width = 0.1), width = 0.1) + 
  geom_violin(data = plot_dat %>% filter(x_axis == 6.5), aes(x=x_axis, y=score, fill = "None"),width=0.5) + 
  geom_errorbar(data=errbar_lims%>% filter(x_axis == 6.5), 
                aes(x = x_axis, ymax = upper, ymin = lower, group = Type), 
                stat = 'identity', position = position_dodge(width = 0.1), width = 0.1) + 
  geom_line(data=errbar_lims %>% filter(Type == "Self"), aes(x = x_axis, y=mean), color="gray") + 
  geom_line(data=errbar_lims %>% filter(Type == "Other"), aes(x = x_axis, y=mean), color="gray") + 
  geom_line(data=errbar_lims %>% filter(Type == "Neutral"), aes(x = x_axis, y=mean), color="gray") + 
  scale_fill_manual(name="Distractor type", 
                    guide = "legend", 
                    values = c("Self" = "#E64B35B2", 
                               "Other" = "#4DBBD5B2", 
                               "Neutral" = "#00A087B2", 
                               "None" = "#7E6148B2"), 
                    breaks = c("Self", "Other", "Neutral", "None")) + 
  scale_x_continuous(breaks=c(1,4,6.5), labels=c("High-probability", "Low-probability", "No distractor"), limits=c(0, 7)) + 
  xlab("Distractor position") +
  ylab("Reaction time(ms)") + 
  papaja::theme_apa()

ggsave("exp2.png", width = 8, height = 7, dpi = 300)








all %>% 
  group_by(Type) %>% 
  summarize(mean  = mean(score), 
            se    = sd(score) / sqrt(n()), 
            upper = mean + se, 
            lower = mean - se) %>% 
  ggplot(aes(x = Type,
             y = mean, 
             ymin = lower,
             ymax = upper, fill=Type)) +
  geom_col(width = .5, position = position_dodge(.6)) +
  geom_errorbar(width = .1, position = position_dodge(.6)) +
  scale_fill_manual(values = c("#737373")) +  xlab("Distractor type") +
  ylab("Reaction time(ms)") + 
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank(),
    plot.title = element_text(size = 22, face = "bold",
                              hjust = 0.5,
                              margin = margin(b = 15)),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 22, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.text.y = element_text(size = 17),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks.x = element_blank(),
    legend.position = c(0.20, 0.8),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 15),
    legend.margin = margin(t = 5, l = 5, r = 5, b = 5),
    legend.key = element_rect(color = NA, fill = NA)
  ) + 
  scale_y_continuous(expand = c(0,0)) +
  papaja::theme_apa() + 
  ggsci::scale_fill_npg()
ggsave("exp1.png", width = 8, height = 7, dpi = 300)

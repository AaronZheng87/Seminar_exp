within <- list(
  Valence = c(self = "self-related", 
              other = "Other-related",
              None = "None"),
  Posit = c(High = "high", 
            Low  = "low")
)



design <- check_design(within, between, mu = mu, plot = FALSE)
str(design$mu)
mu <- data.frame(
  row.names = c("con_easy", "con_med", "con_hard",
                "inc_easy", "inc_med",  "inc_hard"),
  novice_day = 10:15,
  novice_night = 11:16,
  expert_day = 9:14,
  expert_night = 10:15
)


data.frame(
  row.names = c("con_easy", "con_med", "con_hard",
                "inc_easy", "inc_med",  "inc_hard"),
  novice_day = 10:15,
  novice_night = 11:16,
  expert_day = 9:14,
  expert_night = 10:15
)

r <- list(
  novice_day = 0.3,
  novice_night = 0.2,
  expert_day = 0.5,
  expert_night = 0.4
)





# upper right triangle correlation specification
# inc and con have r = 0.5 within each difficultly level, 0.2 otherwise
#          ce,  ie,  cm,  im,  ch,  ih
triangle <-  c(0.5, 0.2, 0.2, 0.2, 0.2, #con_easy
               0.2, 0.2, 0.2, 0.2, #inc_easy
               0.5, 0.2, 0.2, #con_med
               0.2, 0.2, #inc_med
               0.5) #con_hard
#inc_hard

r <- list(
  novice_day = triangle,
  novice_night = triangle,
  expert_day = triangle,
  expert_night = triangle
)
df <- sim_design(within, between, n = 100, 
                 mu = mu, sd = 2, r = r, 
                 dv = c(rt = "Reaction Time"), 
                 plot = FALSE, long = TRUE)

head(df)

hhh <- sim_design(
  n = 50, # 生成50名被试
  within = list(groupA = c("A1", "A2", "A3"), groupB = c("B1", "B2")), # 生成被试内因子的列表
  
  mu = c(A1B1 = 0, A1B2 = 100, A2B1 = 200, A2B2=300, A3B1=400, A3B2=500), # 设置变量的均值
  
  sd = c(A1B1 =20, A1B2 =40, A2B1 =60, A2B2=80, A3B1=100, A3B2=120), # 设置变量的标准差
  
  long = TRUE,  # 输出长数据形式
  
  dv = "score", # 因变量命名为score
  plot = FALSE   # 显示图
)

aaa <- sim_design(
  n = 50, # 生成50名被试
  within = list(groupA = c("A4")), # 生成被试内因子的列表
  
  mu = c(a=600), # 设置变量的均值
  
  sd = c(a=100), # 设置变量的标准差
  
  long = TRUE,  # 输出长数据形式
  
  dv = "score", # 因变量命名为score
  plot = FALSE   # 显示图
) %>% 
  mutate(groupB = "None") %>% 
  select(id, groupA, groupB, score)


d = rbind(hhh, aaa)

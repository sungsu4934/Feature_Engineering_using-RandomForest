install.packages('arules')
library(arules)

# 데이터 불러오기
dim(test2_over)
table(test2_over$M_SUI_CON)
df_ard <- test2_over[,c('dv1','dv2','dv4','dv3','dv15','CITY','dv17','M_WK_TIME','dv12',
                         'dv8','dv5','dv11','dv7','HW_S_R','dv14','dv20','M_WK_TIME_K',
                         'dv6','PA_SWD_S','dv19','M_SUI_CON')]
dim(df_ard)
str(df_ard)

# 전처리
df_ard$M_WK_TIME <- trunc(df_ard$M_WK_TIME) # 4~9
df_ard$M_WK_TIME_K <- trunc(df_ard$M_WK_TIME_K) # 0~24
df_ard$PA_SWD_S <-ifelse(0 <= df_ard$PA_SWD_S & df_ard$PA_SWD_S < 60 , 0,
                  ifelse(60 <= df_ard$PA_SWD_S & df_ard$PA_SWD_S < 120, 1,
                  ifelse(120 <= df_ard$PA_SWD_S & df_ard$PA_SWD_S < 180, 2,
                  ifelse(180 <= df_ard$PA_SWD_S & df_ard$PA_SWD_S < 240, 3,
                  ifelse(240 <= df_ard$PA_SWD_S & df_ard$PA_SWD_S < 300, 4,
                  ifelse(300 <= df_ard$PA_SWD_S & df_ard$PA_SWD_S < 360, 5,
                  ifelse(360 <= df_ard$PA_SWD_S & df_ard$PA_SWD_S < 420, 6,
                  ifelse(420 <= df_ard$PA_SWD_S & df_ard$PA_SWD_S < 480, 7,
                  ifelse(480 <= df_ard$PA_SWD_S & df_ard$PA_SWD_S < 540, 8,
                  ifelse(540 <= df_ard$PA_SWD_S & df_ard$PA_SWD_S < 600, 9,
                  ifelse(600 <= df_ard$PA_SWD_S & df_ard$PA_SWD_S < 660, 10,
                  ifelse(660 <= df_ard$PA_SWD_S & df_ard$PA_SWD_S < 720, 11,
                  ifelse(720 <= df_ard$PA_SWD_S & df_ard$PA_SWD_S < 780, 12,
                  ifelse(780 <= df_ard$PA_SWD_S & df_ard$PA_SWD_S < 840, 13,
                  ifelse(840 <= df_ard$PA_SWD_S & df_ard$PA_SWD_S < 900, 14,
                  ifelse(900 <= df_ard$PA_SWD_S & df_ard$PA_SWD_S < 960, 15,
                  ifelse(960 <= df_ard$PA_SWD_S & df_ard$PA_SWD_S < 1020, 16,
                  ifelse(1020 <= df_ard$PA_SWD_S & df_ard$PA_SWD_S < 1080, 17,
                  ifelse(1080 <= df_ard$PA_SWD_S & df_ard$PA_SWD_S < 1140, 18,
                  ifelse(1140 <= df_ard$PA_SWD_S & df_ard$PA_SWD_S < 1200, 19,
                  ifelse(1200 <= df_ard$PA_SWD_S & df_ard$PA_SWD_S < 1260, 20,
                  ifelse(1260 <= df_ard$PA_SWD_S & df_ard$PA_SWD_S < 1320, 21,
                  ifelse(1320 <= df_ard$PA_SWD_S & df_ard$PA_SWD_S < 1380, 22,
                  ifelse(1380 <= df_ard$PA_SWD_S & df_ard$PA_SWD_S < 1440, 23, 24))))))))))))))))))))))))


# 연관성 분석
for (i in 1:21){
  df_ard[,i] <- as.factor(df_ard[,i])
}
str(df_ard)
table(df_ard$M_SUI_CON)
df_ard_tr <- as(df_ard, "transactions")
str(df_ard_tr)

dim(df_ard)
table(df_ard[df_ard$dv15==1 & df_ard$dv17==1 & df_ard$dv8==1 & df_ard$dv19==1,'M_SUI_CON'])

dim(test2_over)
table(test2_over[test2_over$TC_LT==1 & test2_over$E_SES<=3 & test2_over$M_SAD == 2 & test2_over$M_STR<=2, 'M_SUI_CON'])



# 규칙도출
rule_1 <- apriori(df_ard_tr, parameter=list(support = 0.000000000000000001, minlen=3, maxlen=3, maxtime=20000), appearance = list(rhs="M_SUI_CON=1"))
rule_1_order <- sort(rule_1, by=c('support'))
df1 <- head(inspect(rule_1_order),2)
df1


rule_2 <- apriori(df_ard_tr, parameter=list(smax=5756/45922, support = 0.005, confidence=0.5, minlen=5, maxlen=5, maxtime=500), appearance = list(rhs="M_SUI_CON=1"))
rule_2_order <- sort(rule_2, by=c('confidence'))
inspect(rule_2_order)
df2 <- inspect(rule_2_order)
df2$multiple <- df2$confidence * df2$lift
df2


rule_3 <- apriori(df_ard_tr, parameter=list(support = 0.000000001, confidence=0.000000005, minlen=21, maxlen=21, maxtime=500), appearance = list(rhs="M_SUI_CON=1"))
rule_3_order <- sort(rule_3, by=c('support'))
df3 <- inspect(rule_3_order)
df3$multiple <- df3$confidence * df3$lift
df3

rule_4 <- apriori(df_ard_tr, parameter=list(smax=5756/45922, support = 0.01, confidence=1, minlen=3, maxlen=5, maxtime=1000), appearance = list(rhs="M_SUI_CON=1"))
rule_4_order <- sort(rule_4, by=c('confidence'))
df4 <- inspect(rule_4_order)
df4$multiple <- df4$confidence * df4$lift
df4 <- sort(df4, by=c('multiple'))
df4
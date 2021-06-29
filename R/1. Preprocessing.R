install.packages('caret')
library(caret)
install.packages('dplyr')
library(dplyr)
install.packages('doBy')
library(doBy)


df <- read.csv(file='df.csv') # df는 기존 데이터에서 10% 규칙으로 전처리 제거 및 YEAR, M_SUI_ATT, M_SUI_PLN 및 무의미한 OBJECT자료를 삭제한 df //  M_SUI_CON은 0과 1로 맵핑 되어있는 상태.
dim(df)
str(df)

# factor화 (39개)
df$CITY <- as.factor(df$CITY)
df$CTYPE <- as.factor(df$CTYPE)
df$CTYPE_SD <- as.factor(df$CTYPE_SD)
df$MH <- as.factor(df$MH)
df$SCHOOL <- as.factor(df$SCHOOL)
df$STYPE <- as.factor(df$STYPE)
df$F_EDU <- as.factor(df$F_EDU)
df$WC_MN <- as.factor(df$WC_MN)
df$M_SAD <- as.factor(df$M_SAD)
df$M_SUI_CON <- as.factor(df$M_SUI_CON)
df$O_BR_SLP <- as.factor(df$O_BR_SLP)
df$O_SLNT <- as.factor(df$O_SLNT)
df$O_SCL <- as.factor(df$O_SCL)
df$O_SYMP1 <- as.factor(df$O_SYMP1)
df$O_SYMP2 <- as.factor(df$O_SYMP2)
df$O_SYMP3 <- as.factor(df$O_SYMP3)
df$O_SYMP4 <- as.factor(df$O_SYMP4)
df$O_SYMP5 <- as.factor(df$O_SYMP5)
df$O_SYMP6 <- as.factor(df$O_SYMP6)
df$O_EDU <- as.factor(df$O_EDU)
df$HW_S_R <- as.factor(df$HW_S_R)
df$HW_EDU <- as.factor(df$HW_EDU)
df$I_SCH_TRT <- as.factor(df$I_SCH_TRT)
df$AC_LT <- as.factor(df$AC_LT)
df$TC_LT <- as.factor(df$TC_LT)
df$TC_EC_LT <- as.factor(df$TC_EC_LT)
df$TC_HTP_LT <- as.factor(df$TC_HTP_LT)
df$TC_GHW <- as.factor(df$TC_GHW)
df$SEX <- as.factor(df$SEX)
df$S_SI <- as.factor(df$S_SI)
df$S_EDU <- as.factor(df$S_EDU)
df$DR_EXP <- as.factor(df$DR_EXP)
df$AS_DG_LT <- as.factor(df$AS_DG_LT)
df$RH_DG_LT <- as.factor(df$RH_DG_LT)
df$ECZ_DG_LT <- as.factor(df$ECZ_DG_LT)
df$INT_WD <- as.factor(df$INT_WD)
df$INT_WK <- as.factor(df$INT_WK)
df$E_RES <- as.factor(df$E_RES)
df$A_FM <- as.factor(df$A_FM)
str(df)

# 비해당 존재 feature 체크
table(df$A_FM) #비해당이 존재하는 항목이나 없음을 확인
table(df$S_SI) #비해당이 존재하는 항목이나 없음을 확인

# 주말 잠자리에 드는 시각 [M_SLP_HR_K]
table(df$M_SLP_HR_K)
df$M_SLP_HR_K <-ifelse(df$M_SLP_HR_K == 20, 0,
                ifelse(df$M_SLP_HR_K == 21, 1,
                ifelse(df$M_SLP_HR_K == 22, 2,
                ifelse(df$M_SLP_HR_K == 23, 3,
                ifelse(df$M_SLP_HR_K == 24, 4,
                ifelse(df$M_SLP_HR_K == 1, 5,
                ifelse(df$M_SLP_HR_K == 2, 6,
                ifelse(df$M_SLP_HR_K == 3, 7,
                ifelse(df$M_SLP_HR_K == 4, 8,
                ifelse(df$M_SLP_HR_K == 5, 9,
                ifelse(df$M_SLP_HR_K == 6, 10,
                ifelse(df$M_SLP_HR_K == 7, 11,
                ifelse(df$M_SLP_HR_K == 8, 12,
                ifelse(df$M_SLP_HR_K == 9, 13,
                ifelse(df$M_SLP_HR_K == 10, 14,
                ifelse(df$M_SLP_HR_K == 11, 15,
                ifelse(df$M_SLP_HR_K == 12, 16,
                ifelse(df$M_SLP_HR_K == 13, 17,
                ifelse(df$M_SLP_HR_K == 14, 18,
                ifelse(df$M_SLP_HR_K == 15, 19,
                ifelse(df$M_SLP_HR_K == 16, 20,
                ifelse(df$M_SLP_HR_K == 17, 21,
                ifelse(df$M_SLP_HR_K == 18, 22,
                ifelse(df$M_SLP_HR_K == 19, 23, 24))))))))))))))))))))))))
table(df$M_SLP_HR_K)

# 평일 잠자리에 드는 시각[M_SLP_HR]
table(df$M_SLP_HR)
df$M_SLP_HR <- ifelse(df$M_SLP_HR == 20, 0,
               ifelse(df$M_SLP_HR == 21, 1,
               ifelse(df$M_SLP_HR == 22, 2,
               ifelse(df$M_SLP_HR == 23, 3,
               ifelse(df$M_SLP_HR == 24, 4,
               ifelse(df$M_SLP_HR == 1, 5,
               ifelse(df$M_SLP_HR == 2, 6, 7)))))))
table(df$M_SLP_HR)

# 주말 기상 시각[M_WK_HR_K]
table(df$M_WK_HR_K)
df$M_WK_HR_K <-ifelse(df$M_WK_HR_K == 4, 0,
                ifelse(df$M_WK_HR_K == 5, 1,
                ifelse(df$M_WK_HR_K == 6, 2,
                ifelse(df$M_WK_HR_K == 7, 3,
                ifelse(df$M_WK_HR_K == 8, 4,
                ifelse(df$M_WK_HR_K == 9, 5,
                ifelse(df$M_WK_HR_K == 10, 6,
                ifelse(df$M_WK_HR_K == 11, 7,
                ifelse(df$M_WK_HR_K == 12, 8,
                ifelse(df$M_WK_HR_K == 13, 9,
                ifelse(df$M_WK_HR_K == 14, 10,
                ifelse(df$M_WK_HR_K == 15, 11,
                ifelse(df$M_WK_HR_K == 16, 12,
                ifelse(df$M_WK_HR_K == 17, 13,
                ifelse(df$M_WK_HR_K == 18, 14,
                ifelse(df$M_WK_HR_K == 19, 15,
                ifelse(df$M_WK_HR_K == 20, 16,
                ifelse(df$M_WK_HR_K == 21, 17,
                ifelse(df$M_WK_HR_K == 22, 18,
                ifelse(df$M_WK_HR_K == 23, 19,
                ifelse(df$M_WK_HR_K == 24, 20,
                ifelse(df$M_WK_HR_K == 1, 21,
                ifelse(df$M_WK_HR_K == 2, 22, 23)))))))))))))))))))))))
table(df$M_WK_HR_K)

# 수면시간 시간과 분 합치기 및 변수 구간화
df$M_SLP_TIME <- df$M_SLP_HR + round(df$M_SLP_MM/60,2) #주중 잠에든 시간
table(df$M_SLP_TIME)

df$M_SLP_TIME_K <- df$M_SLP_HR_K + round(df$M_SLP_MM_K/60,2) #주말 잠에든 시간
table(df$M_SLP_TIME_K)

df$M_WK_TIME <- df$M_WK_HR + round(df$M_WK_MM/60,2) #주중 일어난 시간
table(df$M_WK_TIME)

df$M_WK_TIME_K <- df$M_WK_HR_K + round(df$M_WK_MM_K/60,2) #주말 일어난 시간
table(df$M_WK_TIME_K)

df <- subset(df, select=-c(M_SLP_HR,M_SLP_HR_K, M_WK_HR, M_WK_HR_K, M_SLP_MM, M_SLP_MM_K, M_WK_MM, M_WK_MM_K))

dim(df)

# train test 나누기
set.seed(42)
index <- createDataPartition(df$M_SUI_CON, p = 0.7, list = F)
train <- df[index,]
test <- df[-index,]
dim(train)
dim(test)

train_x <- subset(train, select=-M_SUI_CON)
train_y <- subset(train, select=M_SUI_CON)
test_x <- subset(test, select=-M_SUI_CON)
test_y <- subset(test, select=M_SUI_CON)
dim(train_x)
dim(train_y)
dim(test_x)
dim(test_y)

train <- cbind(train_x, train_y)
test <- cbind(test_x, test_y)

# 업샘플링
table(train$M_SUI_CON)

set.seed(42)
train_over <- upSample(subset(train, select=-M_SUI_CON),train$M_SUI_CON)
names(train_over)[names(train_over) == "Class"] <- c("M_SUI_CON") 

table(train$M_SUI_CON)
table(train_over$M_SUI_CON)

train_over_x <- subset(train_over, select=-M_SUI_CON)
train_over_y <- subset(train_over, select=M_SUI_CON)
dim(train_over_x)
dim(train_over_y)

colnames(train_over[91])

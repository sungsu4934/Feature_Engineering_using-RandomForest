##### M_SUI_CON을 포함하지 않고 파생변수 생성 코드
# 오버샘플링 데이터
train2_over_x <- train_over_x
train2_over <- cbind(train2_over_x, train_over_y)
train2_over_x$dv1 <- ifelse(train2_over[,73] %in% c('2') & train2_over[,75] %in% c('1') , 1, 0)
train2_over_x$dv2 <- ifelse(train2_over[,32]<=167.95 & train2_over[,66] %in% c('1') , 1, 0)
train2_over_x$dv3 <- ifelse(train2_over[,48] %in% c('0') & train2_over[,73] %in% c('2') , 1, 0)
train2_over_x$dv4 <- ifelse(train2_over[,75] %in% c('1') & train2_over[,91]>5.085 , 1, 0)
train2_over_x$dv5 <- ifelse(train2_over[,36]>3.5 & train2_over[,75] %in% c('1') , 1, 0)
train2_over_x$dv6 <- ifelse(train2_over[,6] %in% c('남녀공학','여학교') & train2_over[,36]>3.5 , 1, 0)
train2_over_x$dv7 <- ifelse(train2_over[,74] %in% c('2') & train2_over[,86]<=3.5 , 1, 0)
train2_over_x$dv8 <- ifelse(train2_over[,35]<=2.5 & train2_over[,86]<=3.5 , 1, 0)
train2_over_x$dv9 <- ifelse(train2_over[,36]>3.5 & train2_over[,86]<=3.5 , 1, 0)
train2_over_x$dv10 <- ifelse(train2_over[,65]<=1.5 & train2_over[,74] %in% c('2') & train2_over[,86]<=3.5 , 1, 0)
train2_over_x$dv11 <- ifelse(train2_over[,35]<=2.5 & train2_over[,71]<=1.5 , 1, 0)
train2_over_x$dv12 <- ifelse(train2_over[,70]<=2.5 & train2_over[,91]>5.085 , 1, 0)
train2_over_x$dv13 <- ifelse(train2_over[,36]>3.5 & train2_over[,50] %in% c('0') , 1, 0)
train2_over_x$dv14 <- ifelse(train2_over[,35]<=2.5 & train2_over[,62] %in% c('1') , 1, 0)
train2_over_x$dv15 <- ifelse(train2_over[,37] %in% c('2') & train2_over[,66] %in% c('1') , 1, 0)
train2_over_x$dv16 <- ifelse(train2_over[,35]<=2.5 & train2_over[,36]>3.5 , 1, 0)
train2_over_x$dv17 <- ifelse(train2_over[,37] %in% c('2') & train2_over[,86]<=3.5 , 1, 0)
train2_over_x$dv18 <- ifelse(train2_over[,36]>3.5 & train2_over[,47] %in% c('0') , 1, 0)
train2_over_x$dv19 <- ifelse(train2_over[,35]<=2.5 & train2_over[,37] %in% c('2') , 1, 0)
train2_over_x$dv20 <- ifelse(train2_over[,35]<=2.5 & train2_over[,72]<=3.5 , 1, 0)

dim(train2_over_x)
train2_over <- cbind(train2_over_x, train_over_y)
dim(train2_over)

# 오버샘플링 테스트 데이터
test2_over_x <- test_x
test2_over <- cbind(test2_over_x, test_y)
test2_over_x$dv1 <- ifelse(test2_over[,73] %in% c('2') & test2_over[,75] %in% c('1') , 1, 0)
test2_over_x$dv2 <- ifelse(test2_over[,32]<=167.95 & test2_over[,66] %in% c('1') , 1, 0)
test2_over_x$dv3 <- ifelse(test2_over[,48] %in% c('0') & test2_over[,73] %in% c('2') , 1, 0)
test2_over_x$dv4 <- ifelse(test2_over[,75] %in% c('1') & test2_over[,91]>5.085 , 1, 0)
test2_over_x$dv5 <- ifelse(test2_over[,36]>3.5 & test2_over[,75] %in% c('1') , 1, 0)
test2_over_x$dv6 <- ifelse(test2_over[,6] %in% c('남녀공학','여학교') & test2_over[,36]>3.5 , 1, 0)
test2_over_x$dv7 <- ifelse(test2_over[,74] %in% c('2') & test2_over[,86]<=3.5 , 1, 0)
test2_over_x$dv8 <- ifelse(test2_over[,35]<=2.5 & test2_over[,86]<=3.5 , 1, 0)
test2_over_x$dv9 <- ifelse(test2_over[,36]>3.5 & test2_over[,86]<=3.5 , 1, 0)
test2_over_x$dv10 <- ifelse(test2_over[,65]<=1.5 & test2_over[,74] %in% c('2') & test2_over[,86]<=3.5 , 1, 0)
test2_over_x$dv11 <- ifelse(test2_over[,35]<=2.5 & test2_over[,71]<=1.5 , 1, 0)
test2_over_x$dv12 <- ifelse(test2_over[,70]<=2.5 & test2_over[,91]>5.085 , 1, 0)
test2_over_x$dv13 <- ifelse(test2_over[,36]>3.5 & test2_over[,50] %in% c('0') , 1, 0)
test2_over_x$dv14 <- ifelse(test2_over[,35]<=2.5 & test2_over[,62] %in% c('1') , 1, 0)
test2_over_x$dv15 <- ifelse(test2_over[,37] %in% c('2') & test2_over[,66] %in% c('1') , 1, 0)
test2_over_x$dv16 <- ifelse(test2_over[,35]<=2.5 & test2_over[,36]>3.5 , 1, 0)
test2_over_x$dv17 <- ifelse(test2_over[,37] %in% c('2') & test2_over[,86]<=3.5 , 1, 0)
test2_over_x$dv18 <- ifelse(test2_over[,36]>3.5 & test2_over[,47] %in% c('0') , 1, 0)
test2_over_x$dv19 <- ifelse(test2_over[,35]<=2.5 & test2_over[,37] %in% c('2') , 1, 0)
test2_over_x$dv20 <- ifelse(test2_over[,35]<=2.5 & test2_over[,72]<=3.5 , 1, 0)
test2_over <- cbind(test2_over_x, test_y)


dim(test2_over)
dim(train2_over)
dim(test2_over)
table(test2_over$M_SUI_CON)


# 방법론 3의 오버샘플링 MDA
set.seed(42)
over_rf_MDA <- randomForest(formula = as.factor(M_SUI_CON)~., data = train2_over, importance=TRUE)
MDA_df <- importance(over_rf_MDA)
orderBy(~-MeanDecreaseAccuracy,MDA_df)[,c(3,4)]
varImpPlot(over_rf_MDA, n.var=20)


write.csv(train2_over, file='train4_over.csv')
write.csv(test2_over, file='test_over_A.csv')

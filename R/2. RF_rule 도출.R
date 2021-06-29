install.packages('RRF')
library(RRF)
library(inTrees)
library(randomForest)


# 규칙도출 (오버샘플링 데이터)
set.seed(42)
over_rf <- randomForest(as.factor(M_SUI_CON)~., train_over)

# K=2
treeList0_over <- RF2List(over_rf)
ruleExec0_over <- extractRules(treeList0_over,train_over,maxdepth=2) # transform to R-executable rules
ruleExec0_over
ruleExec0_over <- unique(ruleExec0_over)
ruleMetric0_over <- getRuleMetric(ruleExec0_over,train_over,train_over$M_SUI_CON) # measure rules
ruleMetric0_over
ruleMetric0_over <- ruleMetric0_over[,c(1,2,4,5)]
ruleMetric0_sui_over <- ruleMetric0_over[ruleMetric0_over[,4] == 0,]
ruleMetric0_sui_over
ruleMetric0_sui_df_over <- as.data.frame(ruleMetric0_sui_over)
ruleMetric0_sui_df_order_over <- ruleMetric0_sui_df_over[order(ruleMetric0_sui_df_over$freq, decreasing = T),]
ruleMetric0_sui_df_order_over

#dim(train_over[(train_over$TC_GHW == 2) & (train_over$S_SI == 1),])[1] / dim(train_over)[1]
colnames(train_over)[37]
#table(train_over$O_SYMP1)
# K=3
treeList1_over <- RF2List(over_rf)
ruleExec1_over <- extractRules(treeList1_over,train_over,maxdepth=3) # transform to R-executable rules
ruleExec1_over <- unique(ruleExec1_over)
ruleMetric1_over <- getRuleMetric(ruleExec1_over,train_over,train_over$M_SUI_CON) # measure rules
ruleMetric1_over
presentRules(ruleMetric1_over, colnames(train_over))
ruleMetric1_over <- ruleMetric1_over[,c(1,2,4,5)]
ruleMetric1_sui_over <- ruleMetric1_over[ruleMetric1_over[,4] == 0,]
ruleMetric1_sui_over
ruleMetric1_sui_over <- as.data.frame(ruleMetric1_sui_over)
ruleMetric1_sui_df_order_over <- ruleMetric1_sui_over[order(ruleMetric1_sui_over$freq, decreasing = T),]
ruleMetric1_sui_df_order_over

# K=4
treeList2_over <- RF2List(over_rf)
ruleExec2_over <- extractRules(treeList2_over,train_over,maxdepth=4) # transform to R-executable rules
ruleExec2_over <- unique(ruleExec2_over)
ruleMetric2_over <- getRuleMetric(ruleExec2_over,train_over,train_over$M_SUI_CON) # measure rules
ruleMetric2_over
presentRules(ruleMetric2_over, colnames(train_over))
ruleMetric2_over <- ruleMetric2_over[,c(1,2,4,5)]
ruleMetric2_sui_over <- ruleMetric2_over[ruleMetric2_over[,4] == 0,]
ruleMetric2_sui_over
ruleMetric2_sui_over <- as.data.frame(ruleMetric2_sui_over)
ruleMetric2_sui_df_order_over <- ruleMetric2_sui_over[order(ruleMetric2_sui_over$freq, decreasing = T),]
ruleMetric2_sui_df_order_over

# K=5
treeList3_over <- RF2List(over_rf)
ruleExec3_over <- extractRules(treeList3_over,train_over,maxdepth=5) # transform to R-executable rules
ruleExec3_over <- unique(ruleExec3_over)
ruleMetric3_over <- getRuleMetric(ruleExec3_over,train_over,train_over$M_SUI_CON) # measure rules
ruleMetric3_over
presentRules(ruleMetric3_over, colnames(train_over))
ruleMetric3_over <- ruleMetric3_over[,c(1,2,4,5)]
ruleMetric3_sui_over <- ruleMetric3_over[ruleMetric3_over[,4] == 1,]
ruleMetric3_sui_over
ruleMetric3_sui_over <- as.data.frame(ruleMetric3_sui_over)
ruleMetric3_sui_df_order_over <- ruleMetric3_sui_over[order(ruleMetric3_sui_over$freq, decreasing = T),]
ruleMetric3_sui_df_order_over

rf_rule_over <- rbind(ruleMetric0_sui_df_order_over, ruleMetric1_sui_df_order_over, ruleMetric2_sui_df_order_over, ruleMetric3_sui_df_order_over)
rf_rule_over_order <- rf_rule_over[order(rf_rule_over$freq, decreasing = T),]
head(unique(rf_rule_over_order),20)
rf_rule_over_order <- head(unique(rf_rule_over_order),20)
rownames(rf_rule_over_order) <- 1:nrow(rf_rule_over_order) # 인덱스 초기화
rf_rule_over_order

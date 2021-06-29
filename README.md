# Research
### RandomForest Rule 기반의 파생변수 생성 및 연관성분석을 통한 파생변수 기반 매커니즘 해석

## 분석과정
### 1. 데이터 전처리
 - 활용데이터: 청소년건강행태조사(2019)
   > http://www.kdca.go.kr/yhs/
 - 타겟변수: 자살생각 여부
 - 전처리
   > 1) YEAR, 자살시도, 자살계획 등 의미없는 변수를 제거하고 결측치가 10%이상인 변수는 제거한 데이터 (자살시도 및 계획은 자살생각과 직접적 연관이 있다고 판단하여 제거)
   > 2) 잠자는 시각을 20시 기준으로 0~23을 매핑, 기상시각은 04시를 기준으로 0~23 매핑
   > 3) 수면시간 및 기상시각 시간과 분을 결합
   > 4) Nomial Type의 데이터는 Factor화
 - 오버샘플링
   > 1) 데이터의 불균형이 존재(자살생각을 한 학생은 전체 12.5%에 불과). 자살생각을 한 학생에 대해서도 올바른 학습을 위해 오버샘플링을 실시
 
### 2. RandomForest Rule 추출 및 파생변수 생성
 - RRF 패키지를 활용하여 RandomForest Rule들을 추출 (max_depth가 2~5까지 기준으로 추출)
 - 각각 상위 20개씩 규칙을 도출한 후 이를 freq 기준으로 내림차순 정렬 -> 상위 20개 규칙 도출
 - 규칙을 통해 도출된 변수들을 파생변수로 결합(and조건). 각각 dv1에서 dx20으로 네이밍
   > 이 때 변수를 결합하는 과정에서 해당 규칙을 통해 "자살생각O"로 예측되었다면, "자살생각을 했다"를 함께 파생변수를 만드는 데 활용하였다. (연구의 큰 오류. 종속변수의 특성을 이미 독립변수에서 알려주면서 모델링을 시도.)

  ![image](https://user-images.githubusercontent.com/28617435/123726930-c6c9b280-d8cb-11eb-9710-39d232b46d45.png)
  
 - 파생변수 해석 예시
 
 ![image](https://user-images.githubusercontent.com/28617435/123726855-aa2d7a80-d8cb-11eb-9a7f-2fbc0a3894fa.png)

### 3. RandomForest MDA를 통해 파생변수가 유의미하게 도출되었는지 파악
 - 파생변수가 MDA 변수중요도에는 약 10여가지 파생변수가 도출되었다.

 ![image](https://user-images.githubusercontent.com/28617435/123726756-74889180-d8cb-11eb-8e86-222db57a5c81.png)


### 4. MDA 상위 20개 변수들을 연관성분석을 통해 해석
 - 자살생각여부만 rhs에 고정시키고, lhs에 1개에서 20개까지 늘려가며 인자개수별 연관성 메커니즘을 파악. 이 때 상위 2개의 규칙만 집중하였다.
 - lhs에 등장한 변수들을 직접 본인이 해석해가며 lhs의 변수가 늘어날수록 어떤 변수가 추가되는지 보면서 메커니즘으로써 해석 가능성 파악
 - 상위 규칙 선정 기준은 support
 
  ![image](https://user-images.githubusercontent.com/28617435/123726613-41460280-d8cb-11eb-84f8-8774f7e452d1.png)


### 5. MDA 변수중요도 상위 20개 변수들에 대해 Logistic regression 적합.
 - VIF로 다중공선성이 존재하는 변수는 우선 제거
 - Accuracy, Recall에서 매우 말도 안되는 성능. 이 과정에서 연구의 오점을 파악하였다. <2. RandomForest Rule 추출 및 파생변수 생성>에서 파생변수 생성 시 독립변수에 종속변수의 특성을 대입한 것이 큰 문제가 되었다. 

![image](https://user-images.githubusercontent.com/28617435/123726980-db0daf80-d8cb-11eb-9a3b-ba2cfdf74b99.png)

### 6. RandomForest Rule 추출 및 파생변수 생성(2)
 - 이번엔 종속변수의 특성을 빼고 규칙생성 및 파생변수 생성

### 7. RandomForest MDA를 통해 파생변수가 유의미하게 도출되었는지 파악
 - 생성한 파생변수가 단 한 개도 도출되지 못하였다. (연구종료)
 
 ![image](https://user-images.githubusercontent.com/28617435/123727047-f7a9e780-d8cb-11eb-8323-39af862e181c.png)

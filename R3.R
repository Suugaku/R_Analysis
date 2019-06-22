 library(ggplot2)
 
 #データを読み込む
 data=read.csv("ad_sales.csv")
 #散布図の作成
 plot(data$ad,data$sales)
 #回帰モデル
 model = lm ( sales ~ ad,  data)
 summary(model)
 abline(model)

 #予測する1
 new_data=data.frame(ad=15000)
 predict(model, new_data)
 #予測する2
 new_data=read.csv ("ad_sales_test.csv")
 predict(model, new_data)
 
 #Step Wiseをつかった回帰分析
 data=read.csv("house_rent.csv",header=TRUE)
 null_model <- lm(rent~1,  data      )
 full_model <- lm(rent~., data   )
 step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")
 test_data=read.csv("house_rent_test.csv",header=TRUE)
 predict(step_model, test_data)

 #diamondsデータ
 str(diamonds)
 dim(diamonds)
 class(diamonds)
 names(diamonds)
 View(diamonds)
 head(diamonds,3)
 tail(diamonds,3)
 
 #回帰モデル
 model=lm(price~.,diamonds)
 p=predict(model,diamonds)
 error=p-diamonds$price
 sqrt(mean(error^2))
 
 
 # 80%-20% Split
 # Shuffle row indices: rows
 rows <- sample(nrow(diamonds))
 # Randomly order data
 shuffled_diamonds <- diamonds[rows, ]
 # Determine row to split on: split
 split=round(nrow(diamonds)*0.8)
 # Create train
 train=diamonds[1:split,]
 # Create test
 test=diamonds[(split+1):nrow(diamonds),]
 
 # Fit lm model on train: model
 model=lm(price~.,train)
 # Predict on test: p
 p=predict(model,test)
 # Compute errors: error
 error=p-test$price
 # Calculate RMSE
 sqrt(mean(error^2))
 
 
 library(Mass)
 Boston
 # Get the number of observations
 n_obs=nrow(Boston)
 # Shuffle row indices: permuted_rows
 permuted_rows=sample(nrow(Boston))
 # Randomly order data: Boston
 Boston_shuffled= Boston[permuted_rows,]
 # Identify row to split on: split
 split <- round(n_obs *0.6)
 # Create Train data
 train= Boston_shuffled [1:split,]
 # Create Test data
 test= Boston_shuffled [(split+1):nrow(Boston),]
 
 model=lm(medv~.,train)
 yp=predict(model,test)
 error=train$medv-yp
 mean(sum(error^2))
 
 #cross validation
 model <- train(medv~., Boston , method = "lm",
                trControl = trainControl(
                  method = "cv", 
                  number =5 ,
                  verboseIter = TRUE
                )
 )
                  
 
 
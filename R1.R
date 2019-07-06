# パッケージ"ggplot2"の準備
# install.packages("ggplot2")
library(ggplot2)
library(dplyr)

# データの読み込み
gapminder_2007 = read.csv("gapminder_2007.csv")
View(gapminder_2007)

#読み込んだデータの確認
gapminder_2007
str(gapminder_2007) # データ型
dim(gapminder_2007) # データサイズ(次元)
class(gapminder_2007) # データのクラス
names(gapminder_2007) # 変数名
View(gapminder_2007) # データの表示
head(gapminder_2007) # データの先頭
tail(gapminder_2007) #データの末尾

# 2007年にlifeExpが最大値だった国は？
# 2007年のlifeExpの最大値は？
max(gapminder_2007$lifeExp)
which(gapminder_2007$lifeExp==max(gapminder_2007$lifeExp))
gapminder_2007$country[6]

# 基礎統計量
max(gapminder_2007$lifeExp)
min(gapminder_2007$lifeExp)
mean(gapminder_2007$lifeExp)
median(gapminder_2007$lifeExp)
sd(gapminder_2007$lifeExp)
quantile(gapminder_2007$lifeExp)

# 問題
# 2007年のlifeExpの最小値は？
min(gapminder_2007$lifeExp)
# 2007年のlifeExpの平均値は？
mean(gapminder_2007$lifeExp)
# 2007年のpopの四分位は?
quantile(gapminder_2007$pop)
# 2007年にpopの平均値は?
mean(gapminder_2007$pop)

#  ggplotを使ったデータの可視化
# scatterplot:散布図 +geom_point()
# linegraphs:線グラフ +geom_line()
# boxplots:箱ひげ図 +geom_boxplot()
# histograms:ヒストグラム +geom_histogram()
# barplots:棒グラフ+geom_col()

# 散布図による可視化
ggplot(gapminder_2007,aes(x=gdpPercap,y=lifeExp))+
  geom_point()
# 大陸ごとに識別
ggplot(gapminder_2007,aes(x=gdpPercap,y=lifeExp, color=continent))+
  geom_point()
#popの大きさを識別
ggplot(gapminder_2007,aes(x=gdpPercap,y=lifeExp,color=continent,size=pop))+
  geom_point()
# 国名を表示
ggplot(gapminder_2007,aes(x=gdpPercap,y=lifeExp, color=continent,size=pop))+
  geom_point()+
  geom_text(aes(x=gdpPercap, y=lifeExp,label=country),vjust=2,size=3)

# 線グラフによる可視化
gapminder_jp = read.csv("gapminder_jp.csv")
ggplot(gapminder_jp,aes(x=year,y=gdpPercap))+geom_line()
ggplot(gapminder_jp,aes(x=year,y=pop))+geom_line()

# ヒストグラムによる可視化
ggplot(gapminder_2007,aes(x=gdpPercap))+geom_histogram()
# 色の付与
ggplot(gapminder_2007,aes(x=gdpPercap)) +geom_histogram(bins=9,color="red", fill = "white")
# gapminder_2007のpopのヒストグラム
ggplot(gapminder_2007,aes(x=pop)) +geom_histogram(bins=9, color="red", fill = "white")

# boxplotによる可視化
ggplot(gapminder_jp,aes(x=country,y=gdpPercap))+geom_boxplot()
# mappingを合わせて表示
ggplot(gapminder_jp,aes(x=country,y=gdpPercap))+geom_boxplot() +geom_jitter(width = 0.1, height = 0.1, alpha = 1,color="red")
# 国別の箱ひげ図を作成
ggplot(gapminder_2007,aes(x=continent,y=pop))+geom_boxplot() +geom_jitter(width = 0.1, height = 0.1, alpha = 1,color="red")
# 対数スケールで表示
ggplot(gapminder_2007,aes(x=continent,y=pop))+geom_boxplot() +geom_jitter(width = 0.1, height = 0.1, alpha = 1,color="red")+ scale_y_log10()

# gapminder_2007のpopデータの棒グラフ作成
ggplot(gapminder_2007,aes(x=continent,y=pop))+geom_col()

# 回帰分析
ggplot(gapminder_jp,aes(x=gdpPercap,y=pop))+geom_point()
ggplot(gapminder_jp,aes(x=gdpPercap,y=pop))+geom_point() +geom_smooth(method="lm")

# 練習問題
data=read.csv("cardata.csv")
# mpgが最大値の車の名前は？
data$name[which(data$mpg==max(data$mpg))]
#mogのヒストグラムを作成する
ggplot(data,aes(x=mpg)) + geom_histogram(binwidth=5)
# ラベルの付与
ggplot(data,aes(x=mpg)) + geom_histogram(binwidth=5)+
  xlab('Miles per Gallon')+ylab('Number of Cars')
# 散布図を作成する
ggplot(data,aes(x=wt,y=mpg)) + geom_point()
# ラベルの付与
ggplot(data,aes(x=wt,y=mpg)) + geom_point()+
  xlab('Weight (x 1000lbs)') + ylab('Miles per Gallon')
# mpg vs. wtの散布図を作成し、回帰直線を作成する
ggplot(data,aes(x=wt,y=mpg)) + geom_point()+geom_smooth(method="lm")
# ラベルの付与
ggplot(data,aes(x=wt,y=mpg)) + geom_point()+geom_smooth(method="lm")+
  xlab('Weight (x 1000lbs)') + ylab('Miles per Gallon')
# 散布図を作成する
ggplot(mtcars,aes(x=wt,y=mpg,col=cyl)) + geom_point()
#cylを質的変数に型変換(factorize)する
ggplot(mtcars,aes(x=wt,y=mpg,col=factor(cyl))) + geom_point()
# 箱ひげ図を作成する
ggplot(data,aes(x=factor(cyl),y=mpg)) + geom_boxplot()
# 散布図を作成する
ggplot(data,aes(x=wt,y=mpg,size=hp,col=factor(cyl))) + geom_point()+geom_text(aes(x=wt,y=mpg,label=name),vjust=2,size=3)
# 必要なパッケージの読み込み
install.packages("dplyr")
library(dplyr)
install.packages("gapminder")
library(gapminder)

library(readr)
gapminder <- read.csv("gapminder.csv")

# 何行何列のデータなのか？
str(gapminder)

# 2007年のデータを抽出する
gapminder %>%
  filter(year==2007)

# アメリカのデータを抽出する
gapminder %>%
  filter(country=="United States")

# 2007年のアメリカのデータのみ抽出する
gapminder %>%
  filter(year==2007, country=="United States")

# 1957年のみを抽出する
gapminder %>%
  filter(year == 1957)

# 2002年の中国のみを抽出する
gapminder %>%
  filter(country == "China", year == 2002)

# 新しい列を追加する
gapminder %>%
  mutate(gdp = gdpPercap * pop)

# 単位の変換
gapminder %>%
  mutate(pop=pop/1000000)

# 列lifeExpを「年」から「月」に変換する
gapminder %>%
  mutate(lifeExpMonths = lifeExp * 12)

# 列lifeExpの平均値を求める
gapminder %>%
  summarize(meanLifeExp = mean(lifeExp))

# 2007年の列lifeExpの平均値を求める
gapminder %>%
  filter(year == 2007) %>%
  summarize(meanLifeExp2007 = mean(lifeExp))

# 2007年のLifeExpの平均値の人口の中央値は？
gapminder %>%
  filter(year == 2007) %>%
  summarize(meanLifeExp = mean(lifeExp), Totalpop = median(lifeExp))

# 1957年のLifeExpの中央値を求める
gapminder %>%
  filter(year == 1957) %>%
  summarize(medianlifeExl = median(lifeExp))

# 1957年のlifeExpの中央値と、GDP per capital の最大値を求めよ
gapminder %>%
  filter(year == 1957) %>%
  summarize(mean(lifeExp),max(gdpPercap))

# 各年度ごとに、LifeExpの平均値を求める
gapminder %>%
  group_by(year) %>%
  summarize(meanlifeExp = mean(lifeExp))

# 2007年のlifeExpの平均値を大陸ごとに求める
gapminder  %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarize(meanlifeExp = mean(lifeExp))

# 2007年のlifeExpと人口の平均値を大陸ごとに求める
gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarize(mean(lifeExp), mean(pop))

# 各年度ごとのLifeExpの中央値と、GDP per  capitaの最大値を求めよ
gapminder %>%
  group_by(year) %>%
  summarize(medianlifeExp=median(lifeExp),maxGDPercap=max(gdpPercap))

# 大陸ごとに、1957ねんのLifeExpの中央値と、GDP per capitaの最大値を求めよ
gapminder %>% filter(year==1957) %>%
  group_by(continent) %>%
  summarize(medianlifeExp=median(lifeExp), maxGDPercap=max(gdpPercap))

# 出力した結果を変数に格納する
a = gapminder %>% filter(year==1957) %>%
  group_by(continent) %>%
  summarize(medianlifeExp=median(lifeExp), maxGDPercap=max(gdpPercap))
a

# 2007年のデータを抽出する
gapminder_2007<-gapminder %>%
  filter(year==2007)

# 2007年度のgdpPercapとlifeExpを大陸ごとにマッピングする
library(ggplot2)
ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  scale_x_log10() +
  facet_wrap(~continent)

# lifeExpとgdpPercapを年度ごとにマッピングして、大陸を色で、人口をサイズで識別する
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp,color = continent, size = pop)) +
  geom_point() +
  scale_x_log10() +
  facet_wrap(~year)

# 検定
# HRデータの読み込み
library(readr)
HR = read.csv("HR.csv")

# 読み込んだデータの表示
View(HR)
str(HR)

# データの読み込み
A = c(70, 67, 81, 92, 78, 62, 85, 73)
B = c(66, 75, 48, 58, 80, 57, 50)

# 検定の実行
t.test(A, B, var.equal = FALSE)

# HRで、退職者と在職者で満足度に差があるか検証せよ。
t.test(HR$satisfaction_level[HR$left==1], HR$satisfaction_level[HR$left==0], var.equal = FALSE)

HR_0 = HR %>%
filter(left == 0)
A = HR_0$satisfaction_level
HR_1 = HR %>%
filter(left == 1)
B = HR_1$satisfaction_level
t.test(A, B, var.equal = FALSE)

# HRでhrとaccountingの部署で満足度に差があるか検証せよ。
HR_ac = HR %>%
filter(department =="accounting")
A = HR_ac$satisfaction_level
HR_hr = HR %>%
filter(department =="hr")
B = HR_hr$satisfaction_level
t.test(A, B, var.equal = FALSE)

# カイ二乗検定の実行
t = matrix(c(70, 180, 30, 120), nrow = 2, byrow = T)
chisq.test(t) # Yate'sの補正あり(correct変数 = 1(default))
chisq.test(t, correct=0) #補正なし

# accouuntingとhrで退職率に差があるか検証する
HR2 = HR %>%
filter(department == "accounting" | department == "hr")
t = table(droplevels(HR2$department), HR2$left) #droplevels()で不要なカラムを削除
chisq.test(t)

# power analysis in R
install.packages("pwr")
library(pwr)

# t検定
pwr.t.test(power = 0.8, sig.level = 0.05, d = 0.2)
pwr.t.test(power = 0.8, sig.level = 0.05, d = 0.8)

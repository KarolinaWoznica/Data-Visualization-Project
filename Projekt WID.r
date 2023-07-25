# Databricks notebook source
# MAGIC %md
# MAGIC # Analiza liczby samobójstw

# COMMAND ----------

# MAGIC %md
# MAGIC ### Źródło danych oraz wyjaśnienie poszczególnych zmiennych
# MAGIC Dane zostały pobrane ze strony kaggle : https://www.kaggle.com/datasets/omkargowda/suicide-rates-overview-1985-to-2021?select=master.csv
# MAGIC
# MAGIC Dane użyte do raportu to:
# MAGIC - rok
# MAGIC - wiek
# MAGIC - płeć
# MAGIC - liczba samobójstw
# MAGIC - populacja
# MAGIC - wskaźnik samobójstw (samobójstwo/100 tyś ludzi)
# MAGIC - PKB na mieszkańca 
# MAGIC - generacja
# MAGIC
# MAGIC Dodatkowo:
# MAGIC
# MAGIC Wiek jest podzielony na 6 podziałów :
# MAGIC - 5-14 lat, 15-24 lat, 25-34 lat, 35-54 lat, 55-74 lat, 75+ lat 
# MAGIC
# MAGIC Generacja jest podzielona na 6 podziałów (od najmłodszej do najstarszej):
# MAGIC - Generation Z, Millenials, Generation X, Boomers , Silent, , G.I. Generation
# MAGIC
# MAGIC

# COMMAND ----------

# MAGIC %md
# MAGIC #Opis projektu
# MAGIC Pomimo że w wielu krajach działają kampanie zapobiegające samobójstwom i zachęcające do sięgania po pomoc, corocznie jednak wielu ludzi decyduje się popełnić samobójstwo. Są to osoby w różnym wieku, które borykają się z różnymi problemami. W Polsce można skorzystać z wielu form pomocy, które działają 24h na dobę takich jak Centrum Wsparcia dla Osób Dorosłych w Kryzysie Psychicznym czy Dziecięcy Telefon Zaufania Rzecznika Praw Dziecka.
# MAGIC
# MAGIC Liczba samobójstw w danym kraju może zależeć od wielu czynników tak jak położenie geograficzne Państwa, ilość godzin słońca w ciągu dnia, poczucia bezpieczeństwa w kraju, zaufania do rządu, PKB, ale również od wieku, płci czy własnych osobistych problemów.
# MAGIC
# MAGIC W tym raporcie zostanie poruszony problem liczby samobójstw w 10 krajach europejskich w latach 2000 - 2010. Zostanie przeanalizowane czy liczba samobójstw zależy od wieku, płci, generacji oraz PKB na mieszkańca. Również zostanie rozpatrzona liczba samobójstw w Polsce na tle pozostałych krajów. Badany okres dotyczy lat 2000 do 2010, ponieważ dane z późniejszych lat są niepełne, co zaburzało odczyt i interpretacje. Selekcja 10 krajów nastąpiła przez wybranie krajów, których PKB jest najbliższe PKB Polski. Wybrane kraje to: Belgia, Francja, Niemcy, Grecja, Włochy, Finlandia, Polska, Portugalia, Rumunia i Hiszpania.

# COMMAND ----------

library(dplyr)
library(ggplot2)
library(magrittr)
library(SparkR)
library(gridExtra)
library(tidyr)


# COMMAND ----------

d <- read.df('/FileStore/tables/master-2.csv', source ='csv', header='true', inferSchema='true')
d <- as.data.frame(d)
summary(d) 
display(d)

# COMMAND ----------

years <- c(2000:2010)

d1 <- d[d$year %in% years, ]

display(d1)

# COMMAND ----------

#Europe countries
countries <- c("Poland", "France", "Italy", "Spain", "Romania", "Netherlands", "Greece", "Portugal", "Belgium", "Germany")
                 
d2 <- d1[d1$country %in% countries,]

display(d2)

# COMMAND ----------

d6 <- d2 %>% 
  dplyr::group_by(country, year) %>% 
  summarise(Total = sum(population, na.rm = TRUE))

plot1 <- ggplot(data=d6, aes(x=year, y=Total, color=country)) +
geom_line() + scale_x_continuous("Rok", labels = as.character(d6$year), breaks = d6$year)  +
theme(legend.position='bottom') + theme(text=element_text(size=7)) +
ylab("Populacja") +
labs(color="Kraj") +
ggtitle("Wyk.1 Populacja w latach 2000 - 2010 ") 

d7 <- d2 %>% 
  dplyr::rename(gdp_per_capita = `gdp_per_capita ($)`) %>%
  dplyr::group_by(country, year) %>% 
  summarise(Total = mean(gdp_per_capita, na.rm = TRUE))

plot2 <- ggplot(data=d7, aes(x=year, y=Total, color=country)) +
geom_line() + scale_x_continuous("Rok", labels = as.character(d7$year), breaks = d7$year)  +
theme(legend.position='bottom') + theme(text=element_text(size=7)) +
ylab("PKB na mieszkańca") +
labs(color="Kraj") +
ggtitle("Wyk.2 PKB na mieszkańca w latach 2000 - 2010") 

grid.arrange(plot1, plot2, ncol = 2)


# COMMAND ----------

# MAGIC %md
# MAGIC Jak widać na wykresie pierwszym, populacja w wybranych krajach w ciągu 10 lat utrzymywała się we wszystkich krajach na podobnym poziomie. Największy przyrost odnotowano we Francji, we Włoszech i w Hiszpanii. Jak widać na drugim wykresie, PKB na mieszkańca wykazywało tendencję rosnącą dla każdego kraju, aż do roku 2008, kiedy to pojawił się kryzys. PKB na mieszkańca tych krajów jest na podobnym poziomie.  

# COMMAND ----------


plot1 <- ggplot(d2[order(d2$country),], aes(x=country, y=suicides_no)) +
geom_bar(stat="identity", aes(fill=country)) +
xlab("Kraj") + ylab("Liczba samobójstw") +
scale_fill_discrete(name="Country") + theme(legend.position='bottom') + theme(text=element_text(size=7)) +
scale_y_continuous(limits = c(0, 120000), breaks = seq(0, 120000, by = 2500)) +
labs(color="Kraj") +
ggtitle("Wyk.3 Całkowita liczba samobójstwa w latach 2000 - 2010") 

plot2 <- ggplot(d2[order(d2$country),], aes(x=d2$country, y=d2[ ,'suicides/100k pop'])) +
geom_bar(stat="identity", aes(fill=country)) +
xlab("Kraj") + ylab("liczba samobójstw/100 tysięcy ludzi") +
scale_fill_discrete(name="Country") + theme(legend.position='bottom') + theme(text=element_text(size=7)) +
scale_y_continuous(limits = c(0, 2600), breaks = seq(0, 2600, by = 200)) +
labs(color="Kraj") +
ggtitle("Wyk.4 Całkowity wskaźnik liczby samobójstw w latach 2000 - 2010") 

grid.arrange(plot1, plot2, ncol = 2)



# COMMAND ----------

# MAGIC %md
# MAGIC W celu ogólnego zarysowania sytuacji powyżjej przedstawione są dwa wykresy, które sumują dane z 10 lat. Jak widać na wykresie 3, kraje z największą liczbą samobójstw w ciągu 10 lat to Francja, Niemcy i Polska a z najmniejszą liczbą to Grecja, Portugalia i Belgia. Jednak gdy weźmie się pod uwagę wskaźnik samobójstw, który jest przedstawiony na wykresie czwartym, można zobaczyć, że tak naprawdę najwyższy odsetek samobójstw jest w Belgi, we Francji i w Polsce, również w dalszym ciągu jest on wysoki w Niemczech, które zajęły czwarte miejsce na wykresie oraz dla Rumunii. Najniższy w dalszym ciągu wskaźnik jest dla Grecji.
# MAGIC
# MAGIC Podsumowując pierwsze cztery wykresy, można by było się spodziewać, że największa liczba samobójstw pojawi się w krajach, w których populacja jest największa i rzeczywiście, jeśli chodzi o Niemcy i Francję to i liczba samobójstw i wskaźnik samobójstw jest wysoki. Jednak najwyższy wskaźnik jest w Belgii, której populacja w tym zbiorze jest prawie najmniejsza na równi z Portugalią, która zajmuje trzecie miejsce pod względem najmniejszego wskaźnika samobójstw. Również populacja Grecji jest mała, jak i liczba samobójstw w tym kraju. Co ciekawe Włochy są trzecim krajem co do wielkości populacji, a ich wskaźnik samobójstw jest też bardzo niski. Biorąc jednak pod uwagę, powyższe dane można uznać, że istnieje zależność pomiędzy wielkością populacji a wskaźnikiem samobójstw i że jest on większy dla krajów z większą populacją.

# COMMAND ----------

ggplot(data=d2, aes(d2[,'suicides/100k pop'], y = ..density..)) +
     geom_histogram(binwidth = 2) +
geom_density(aes(x=d2[,'suicides/100k pop']), color='red', lwd=1)  + 
    ggtitle(paste("Wyk.5 Histogram przedstawiający wskaźnik samobójstw we wszystkich krajach")) +
    xlab("Samobójstwa/100tyś ludzi") +
    ylab("Gęstość")


# COMMAND ----------

# MAGIC %md
# MAGIC Wykres 5 przedstawia histogram asymetrii lewostronnej to oznacza, że obserwacje statystyczne skupiają się przy wartościach cechy większych od średniej arytmetycznej. W tym wypadku można odczytać, że niższe wskaźniki samobójstw pojawiały się dużo częściej niż wysokie wskaźniki samobójstw.

# COMMAND ----------

d3 <- d2 %>%
  dplyr::group_by(country, year) %>% 
  summarise(Total = sum(suicides_no, na.rm = TRUE))

d4 <- d3 %>% dplyr::filter(country == 'Poland')

ggplot(data=d4, aes(x=year, y=Total, color=country)) + geom_point() + geom_smooth(method='lm') + scale_x_continuous("Rok", labels = as.character(d4$year), breaks = d4$year) + ggtitle(paste("Wyk.6 Regresja dla danych dotyczących samobójstw w Polsce w latach 2000 - 2010 ")) + ylab("Suma samobójstw")

# COMMAND ----------

# MAGIC %md
# MAGIC Z wykresu 6 można odczytać, że do 2006 roku liczba samobójstw w Polsce utrzymywała się na podobnym poziomie i rosły one wzdłuż prostej regresji. Najbardziej skrajne wyniki pojawiły się w 2007 roku gdzie liczba samobójstw spadła prawie do 5250, a najwyższy wynik pojawił się w roku 2009 i wyniósł on prawie 6500 samobójstw. Pomimo tych dwóch skrajnych wyników, można stwierdzić liniową zależność pomiędzy zmiennymi.

# COMMAND ----------

d3m <- d2 %>% 
  dplyr::filter(sex == 'male') %>%
  dplyr::group_by(country, year) %>% 
  summarise(Total = sum(suicides_no, na.rm = TRUE))

plot1 <- ggplot(data=d3m, aes(x=year, y=Total, color=country)) +
geom_line() + scale_x_continuous("Rok", labels = as.character(d3m$year), breaks = d3m$year) +
theme(legend.position='bottom') + theme(text=element_text(size=7)) +
scale_y_continuous(limits = c(0, 8500), breaks = seq(0, 8500, by = 500))+
labs(color="Kraj") +
ylab("Suma wszystkich samobójstw") +
ggtitle("Wyk.7 Suma samobójstw mężczyzn na przestrzeni lat ")

d3f <- d2 %>% 
  dplyr::filter(sex == 'female') %>%
  dplyr::group_by(country, year) %>% 
  summarise(Total = sum(suicides_no, na.rm = TRUE))

plot2 <- ggplot(data=d3f, aes(x=year, y=Total, color=country)) +
geom_line() + scale_x_continuous("Rok", labels = as.character(d3f$year), breaks = d3f$year)  +
theme(legend.position='bottom') + theme(text=element_text(size=7)) +
scale_y_continuous(limits = c(0, 8500), breaks = seq(0, 8500, by = 500)) +
ylab("Suma wszystkich samobójstw") +
labs(color="Kraj") +
ggtitle("Wyk.8 Suma samobójstw kobiet na przestrzeni lat ") 

grid.arrange(plot1, plot2, ncol = 2)

# COMMAND ----------

# MAGIC %md
# MAGIC Na podstawie wykresów 7 i 8 można zauważyć, że w każdym kraju to mężczyźni częściej popełniają samobójstwo niż kobiety. Również liczba samobójstw u kobiet utrzymuje się na podobnym poziomie na przestrzeni lat, gdy u mężczyzn wynik ten (oprócz Belgii, Grecji oraz Norwegii) jest bardziej zróżnicowany. Od roku 2007 liczba samobójstw u mężczyzn wzrosła w każdym kraju, jednak najbardziej widoczne jest to we Francji, w Niemczech i w Polsce. Również z wykresów tych można odczytać, że liczba samobójstw pomiędzy kobietami a mężczyznami w krajach: Rumunia, Hiszpania, Włochy, Polska, Francja i Niemcy, jest bardzo duża. W Polsce w 2009 i w 2010 roku różnica jest ponad pięciokrotna. Natomiast w Grecji różnica pomiędzy kobietami i mężczyznami jest niewielka.

# COMMAND ----------

d5 <- aggregate(d2$suicides_no, by=list(Age=d2$age), FUN=sum)

levels <- c('5-14 years', '15-24 years', '25-34 years', '35-54 years', '55-74 years', '75+ years') 
display(d5)
ggplot(data=d5, aes(x=factor(Age, level = levels), y=x)) + geom_line(group=1) + geom_point() + 
ylab("Liczba samobójstw") +
xlab("Grupy wiekowe") +
ggtitle("Wyk.9 Liczba samobójstw w poszczególnych grupach wiekowych") 

# COMMAND ----------

# MAGIC %md
# MAGIC Wykres 9 zawiera sumę wszystkich samobójstw ze wszystkich krajów podzielonych na wiek. Jak widać, na tym wykresie najwięcej samobójstw popełniają osoby w wieku 35-54, można to powiązać ze stresem związanym z pracą, niezadowolenia z komfortu życia, braku spełnienia siebie. Również dość wysoka liczba samobójstw pojawia się w grupie wiekowej 55-74, która może być spowodowana tymi samymi czynnikami. Najmniejsza liczba samobójstw jest dla dzieci pomiędzy 5 a 14 rokiem życia, może być to związane z tym, że dzieci widzą wszystko w lepszych barwach i są bardziej szczęśliwe od dorosłych.

# COMMAND ----------

d10 <- aggregate(d2$suicides_no, by=list(Generation=d2$generation), FUN=sum)
display(d10)
pie(d10$x, labels = d10$x, main = "Wyk.10 Samobójstwa w danej generacji", col = rainbow(length(d10$x)))
legend("topright", d10$Generation, cex = 0.8,
   fill = rainbow(length(d10$x)))

# COMMAND ----------

# MAGIC %md
# MAGIC Z wykresu kołowego można wnioskować, że najwięcej samobójstw popełniają osoby z generacji Silent (ur. 1928-1945) i Boomers (ur. 1946-1964), są to dwie z najstarszych grup w tym zestawieniu, dlatego też liczba samobójstw w tych grupach może być największa, ponieważ generacje te żyją najdłużej. Najpóźniej urodzonymi osobami są osoby z Generacji Z urodzone po roku 1995 i liczba samobójstw dla nich jest najmniejsza. Kolejnymi dwoma generacjami są Millenails (ur.1981-1994) i Generacja X (1965 – 1980), dla których liczba samobójstw jest w miarę wysoka.
# MAGIC
# MAGIC Podsumowując, z wykresu nie da się jednoznacznie określić czy dana generacja ma większe skłonności do samobójstw niż młodsze. Zgodnie z wykresem 9, osoby w wieku od 35 do 74, czyli w badanym okresie generacje to głównie Silent i Boomers. Dlatego można wywnioskować, że to wiek wpływa na liczbę samobójstw oraz płeć, a nie generacja. W celu określenia czy liczba samobójstw zależy od generacji, należałoby przebadać większy zakres lat, żeby móc porównać każdą generację ze sobą, w odpowiednim wieku. Z tymi danymi jest to niemożliwe, a dla późniejszych lat dane były niepełne.

# COMMAND ----------

d6 <- d2 %>% 
  dplyr::filter(year == '2010') %>%
  dplyr::group_by(country, year) %>% 
  summarise(`TotalPopulation/2000` = sum(population, na.rm = TRUE), TotalSuicides = sum(suicides_no, na.rm = TRUE)) %>%
  dplyr::mutate(`TotalPopulation/2000` = `TotalPopulation/2000` / 2000) %>%
  pivot_longer(d6, cols=c(`TotalPopulation/2000`, 'TotalSuicides'), names_to='variable', values_to="value")

ggplot(data=d6, aes(x=country, y=value, fill=variable)) +
geom_bar(stat="identity", position='dodge') +
xlab("Kraj") + ylab(" ") +
scale_fill_discrete(name="Country") + theme(legend.position='bottom') + theme(text=element_text(size=7)) +
scale_y_continuous(limits = c(0, 40000), breaks = seq(0, 40000, by = 2500)) +
labs(color="Kraj") +
ggtitle("Wyk.11 Wykres przedstawiający liczbę samobójstw względem populacji w danych krajach w 2010 roku")

# COMMAND ----------

d7 <- d2 %>% 
  dplyr::filter(year == '2010') %>%
  dplyr::rename(gdp_per_capita = `gdp_per_capita ($)`) %>%
  dplyr::group_by(country, year) %>% 
  summarise(TotalGDP_per_capita = mean(gdp_per_capita, na.rm = TRUE), TotalSuicides = sum(suicides_no, na.rm = TRUE)) %>%
  pivot_longer(d6, cols=c('TotalGDP_per_capita', 'TotalSuicides'), names_to='variable', values_to="value")

ggplot(data=d7, aes(x=country, y=value, fill=variable)) +
geom_bar(stat="identity", position='dodge') +
xlab("Kraj") + ylab(" ") +
scale_fill_discrete(name="Country") + theme(legend.position='bottom') + theme(text=element_text(size=7)) +
scale_y_continuous(limits = c(0, 55000), breaks = seq(0, 55000, by = 2500)) +
labs(color="Kraj") +
ggtitle("Wyk.12 Wykres przedstawiający liczbę samobójstw względem PKB na mieszkańca w danych krajach w 2010 roku")

# COMMAND ----------

# MAGIC %md
# MAGIC Na koniec zostały stworzone dwa wykresy dotyczące tylko roku 2010 w celu zobrazowania zależności pomiędzy liczbą samobójstw a populacją oraz PKB na mieszkańca. Jak widać na wykresie 11, liczba samobójstw jest zazwyczaj wyższa w krajach, w których populacja jest większa, czego można było się spodziewać. Można więc stwierdzić, że liczba samobójstw zależy od liczby mieszkańców danego kraju. Większą zależność jednak widać na wykresie 12, że im wyższe jest PKB na mieszkańca, tym mniejsza jest liczba samobójstw, nie jest to również w 100% prawdą dla każdego kraju jak np. Francja i Niemcy, gdzie liczba samobójstw jest wysoka.

# COMMAND ----------

# MAGIC %md
# MAGIC #Podsumowanie
# MAGIC Biorąc pod uwagę, wszystkie dane z raportu można stwierdzić, że liczba samobójstw jest na pewno zależna od wieku i płci. Częściej samobójstwa popełniają mężczyźni oraz osoby w wieku 35 do 74 lat. Może być to wywołane między innymi presją społeczeństwa. Kolejnymi czynnikami, które mogą pośrednio wpływać na tę liczbę w danym kraju, jest PKB na mieszkańca oraz ilość mieszkańców danego państwa. Pomimo że można by było się spodziewać, że bogactwo kraju wpływa na poziom życia mieszkańców, a co za tym idzie to szczęście i poczucie bezpieczeństwa, to jednak na podstawie tego raportu nie widać aż takiego związku pomiędzy tymi zmiennymi i dużej różnicy pomiędzy biedniejszymi krajami a bogatszymi. Analizując generacje, można wywnioskować, że najmłodsza i najstarsza generacja w badanym okresie popełniła najmniej samobójstw co może być spowodowane ich wiekiem. Co oznacza, że grupa najmłodsza w tym okresie miała maksymalnie 15 lat, co potwierdza, że dzieci popełniają najmniej samobójstw oraz najstarsza grupa miała ponad 75 lat co może oznaczać, że osoby te już nie mają tyle zmartwień i spokojnie żyją na emeryturze. Trzeba jednak pamiętać, że samobójstwa są bardzo poważną sprawą, na którą wpływa bardzo wiele czynników w sposób bezpośredni. Odnosząc się do porównania Polski z innymi krajami, można stwierdzić, że wskaźnik samobójstw w Polsce w badanym okresie jest wysoki na tle innych krajów. Dodatkowo wykres przedstawiający regresje pokazuje, że w Polsce z roku na rok liczba samobójstw się zwiększa, co być między innymi związane z przyrostem populacji. Jednak nie jest to dobra wiadomość dla Polski i jej mieszkańców, oznacza to, że należałoby bardziej skupić się na pomocy osobą, które jej potrzebują oraz spróbowania zminimalizowania czynników, które mają największy wpływ na liczbę samobójstw.

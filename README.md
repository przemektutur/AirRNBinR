# AirRNBinR

#### Zadania do wykonania:
1. Poprawne załadowanie danych ze źródła internetowego do ramki danych, z uwzględnieniem
nagłówków, kodowania zbioru, separatorów itd.;
2. Poznanie rozmiaru zbioru danych (liczby obserwacji i liczby zmiennych, które je opisują)
i oszacowanie czasochłonności procesu analizy;
3. Wyświetlenie próbki surowych danych w celu wyrobienia sobie wyobrażenia o nich – poznania
struktury danych i wstępnej oceny przydatności poszczególnych zmiennych;
4. Weryfikacja typów poszczególnych zmiennych (całkowite, zmiennoprzecinkowe, kategoryczne
porządkowe, kategoryczne nominalne, zmienne typu logicznego, daty) i ich ewentualna korekta
(zamiana typu string na float, interpretacja zmiennych numerycznych jako kategorii itp.);
5. Zbudowanie podsumowania zmiennych numerycznych opisujących zbiór, w postaci jednej tabelki,
zawierającej podstawowe informacje, takie jak:
a. wartości minimalne,
b. wartości maksymalne,
c. średnia,
d. mediana,
e. drugi (dolny) kwartyl,
f. trzeci (górny) kwartyl,
g. odchylenie standardowe,
h. liczba danych brakujących lub nienumerycznych.
W tym kroku należy również dokonać analogicznej analizy zmiennych kategorycznych, dającej dla
każdej z nich informacje m.in. takie jak:
a. liczby poszczególnych kategorii i ich liczności,
b. wartości najczęściej występującej i częstości jej występowania,
c. liczba wartości unikalnych,
d. liczba braków danych.
6. Sprawdzenie, czy w zbiorze występują braki danych. Należy sporządzić odrębne podsumowanie,
skupiając się na poszukiwaniu brakujących wartości w zbiorze – Pozwoli to Państwu odpowiedzieć
na pytanie, jakie zmienne zawierają braki i jaka jest ich liczba, z czego mogą one wynikać itd.
Etap ten (wraz z poprzednim) pozwoli Państwu odnaleźć błędy w danych – brakujące wartości,
błędne interpretacje rodzaju zmiennych itp. Da również wskazówki, które atrybuty wybrać do
analizy (pod kątem ich istotności dla przewidywań modelu), czy i jak uzupełnić brakujące dane
(ewentualnie usunąć wiersze/kolumny, zawierające zbyt wiele braków danych), dokonać ich
transformacji itd.
7. Wizualizacja rozkładu (wybranych) zmiennych (zarówno numerycznych, jak i kategorycznych)
poprzez histogramy i próba ich scharakteryzowania (np. poprzez ich skośność i kurtozę) – będzie to
pomocne np. w procesie imputacji (uzupełniania) zmiennych numerycznych;
8. Przeprowadzenie czyszczenia danych, obejmujące m.in.:
a. uzupełnienie brakujących danych (np. wartością stałą, średnią/medianą/modą dla całego
zbioru lub dla podzbiorów według kategorii, poprzez interpolację itp.), usunięcie
wierszy/kolumn, zawierających zbyt wiele braków danych,
b. przycięcie odstających wartości (ang. outliers) – pomocne będą m.in. takie techniki, jak
wykres punktowy (gdzie nanosimy na obu osiach ten sam atrybut) lub wykres pudełkowy
i ewentualna normalizacja danych numerycznych (metodą min-max lub Z-score) – niektóre
algorytmy modelowania danych są wrażliwe na punkty odstające (np. metody regresji
liniowej, korelacja Pearsona) czy różnice w zakresie zmienności poszczególnych atrybutów
(niektóre algorytmy klasyfikacji bądź grupowania);
9. Zbadanie zależności pomiędzy zmiennymi – krok ten pozwoli odkryć związki pomiędzy
poszczególnymi zmiennymi; informacje te mogą także zostać użyte, np. na etapie transformacji
zmiennych lub do podjęcia decyzji, które zmienne wybrać do budowy modelu:
a. obliczenie macierzy korelacji (można użyć współczynnika korelacji rang Spearmana lub
współczynnika Pearsona) pomiędzy zmiennymi numerycznymi i zwizualizowanie ich za
pomocą wykresów punktowych (ang. scatter plots) lub tzw. wykresów par zmiennych
(ang. pairplots),
b. ewentualne zbadanie zależności pomiędzy zmiennymi kategorycznymi (współczynnik
V Cramméra) i zależności pomiędzy zmiennymi kategorycznymi i numerycznymi
(współczynnik R modelu liniowego z jedną zmienną kategoryczną, która objaśnia zmienną
numeryczną) oraz (podobnie jak powyżej) zwizualizowanie tych zależności w formie
wykresów

### INSTALACJA SRODOWISKA URUCHOMIENIOWEGO
```
# !apt-get install r-base

# !R -e 'install.packages("IRkernel")'
# !R -e 'IRkernel::installspec(user = FALSE)'

install.packages("caret")

install.packages("corrplot")

install.packages("combinat")

install.packages("DescTools")
```
## ANALIZA

### DODANIE POTRZEBNYCH BIBLIOTEK R'OWYCH
```
# Załadowanie potrzebnych bibliotek
library(readr)      # Wczytywanie danych
library(dplyr)      # Przetwarzanie danych
library(ggplot2)    # Wizualizacja danych
library(lubridate)  # Praca z datami
library(jsonlite)   # Praca z plikami JSON
library(caret)      # Preprocessing i modelowanie
library(scales)     # Skalowanie danych
library(corrplot)   # Wykresy korelacji
library(e1071)      # Miary skośności i kurtozy
library(stats)      # Statystyki
library(tidyr)      # Manipulacja danymi
library(combinat)   # Kombinacje
library(DescTools)  # Asocjacje i inne narzędzia statystyczne

# Ustawienia dla ggplot2
theme_set(theme_minimal())
```
## DEFINICJA FUNKCJI UPLOADUJĄCEJ DANE

### ZAŁADOWANIE DANYCH PRZY POMOCY FUNKCJI ŁADUJĄCEJ
```
load_housing_data <- function(
  url = 'http://data.insideairbnb.com/canada/on/toronto/2023-12-12/data/listings.csv.gz'
) {
  tarball_path <- 'datasets/listings.csv.gz'

  if (!file.exists(tarball_path)) {
    if (!dir.exists('datasets')) {
      dir.create('datasets', recursive = TRUE)
    }
    download.file(url, tarball_path)
  }

  df <- read.csv(gzfile(tarball_path))
  return(df)
}
# Przykład użycia funkcji
df <- load_housing_data()
print(head(df))
```
### WYŚWIETLENIE PRÓBKI SUROWYCH DANYCH W CELU WYROBIENIA SOBIE WYOBRAŻENIA O NICH - POZNANIA STRUKTURY DANYCH I WSTĘPNEJ OCENY PRZYDATNOŚCI POSZCZEGÓLNYCH ZMIENNYCH
```
data_frame <- load_housing_data()
# Wyświetlenie pierwszych kilku wierszy ramki danych
head(data_frame)
# Wyświetlenie całej ramki danych
data_frame
```
### POZNANIE ROZMIARU ZBIORU DANYCH (LICZBY OBSERWACJI I LICZBY ZMIENNYCH, KTÓRE JE OPISUJĄ)
```
# Wyświetlenie liczby elementów (komórek) w ramce danych

num_elements <- nrow(data_frame) * ncol(data_frame)
print(num_elements)
```
### POZNANIE WYSTĘPUJĄCYCH TYPÓW DANYCH W ZBIORZE
```
# Wyświetlenie unikalnych typów danych kolumn w ramce danych
unique_types <- unique(sapply(data_frame, class))
print(unique_types)
```
### POZNANIE CECH DANYCH
```
# Wyświetlenie nazw kolumn w ramce danych
column_names <- colnames(data_frame)
print(column_names)
```
### WERYFIKACJA TYPÓW POSZCZEGÓLNYCH ZMIENNYCH (CAŁKOWITE, ZMIENNOPRZECINKOWE, KATEGORYCZNE PORZĄDKOWE, KATEGORYCZNE NOMINALNE, ZMIENNE TYPU LOGICZNEGO, DATY) I ICH EWENTUALNA KOREKTA (ZAMIANA TYPU STRING NA FLOAT, INTERPRETACJA ZMIENNYCH NUMERYCZNYCH JAKO KATEGORII ITP.)
```
# Informacje o ramce danych w R

# Wyświetlenie struktury ramki danych
str(data_frame)

# Podsumowanie danych w każdej kolumnie
summary(data_frame)

# Liczba brakujących wartości w każdej kolumnie

missing_values <- sapply(data_frame, function(x) sum(is.na(x)))
print(missing_values)

# Typy danych w każdej kolumnie

data_types <- sapply(data_frame, class)
print(data_types)
```
### WARTOŚCI MINIMALNE, WARTOŚCI MAKSYMALNE, ŚREDNIA, MEDIANA, DRUGI (DOLNY) KWARTYL, TRZECI (GÓRNY) KWARTYL, ODCHYLENIE STANDARDOWE
```
# Podsumowanie statystyczne kolumn numerycznych w ramce danych
numeric_summary <- summary(data_frame)
print(numeric_summary)

# Podsumowanie statystyczne tylko dla kolumn numerycznych
numeric_cols <- sapply(data_frame, is.numeric)
numeric_summary <- summary(data_frame[, numeric_cols])
print(numeric_summary)
```
### SPRAWDZENIE WARTOŚCI UNIKALNYCH, KATEGORII ORAZ SPOSOBU ZAPISU DLA DANYCH W KAŻDEJ Z KOLUMN
```
# Iteracja przez kolumny ramki danych i wyświetlanie nazw kolumn oraz unikalnych wartości
for (column in colnames(data_frame)) {
  cat("---", column, "---\n")
  print(unique(data_frame[[column]]))
}

options(width = 100)  # Ustawienie szerokości wyświetlania w konsoli
```
### SPRAWDZENIE WARTOSCI NAN
```
# Liczba brakujących wartości w każdej kolumnie
missing_values <- sapply(data_frame, function(x) sum(is.na(x)))

# Sortowanie brakujących wartości rosnąco
sorted_missing_values <- sort(missing_values)
print(sorted_missing_values)
```
### UTWORZENIE FILTRU DLA WARTOSCI NAN
```
# Sprawdzenie, które kolumny mają brakujące wartości
missing_values <- sapply(data_frame, function(x) sum(is.na(x)) > 0)

# Wyświetlenie wyników
print(missing_values)

WYLISTOWANIE WIERSZY Z WARTOŚCIAMI NAN

# Wybór wszystkich kolumn, które mają brakujące wartości
data_frame_with_na <- data_frame[, missing_values]

# Wyświetlenie wyników
print(data_frame_with_na)
```
### WYSORTOWANIE SUMY WARTOSCI NAN
```
# Wybór wszystkich kolumn, które mają brakujące wartości
data_frame_with_na <- data_frame[, missing_values]

# Liczba brakujących wartości w każdej z tych kolumn
na_counts <- sapply(data_frame_with_na, function(x) sum(is.na(x)))

# Sortowanie brakujących wartości rosnąco
sorted_na_counts <- sort(na_counts)

# Wyświetlenie wyników
print(sorted_na_counts)
```
### PROCENTOWY UDZIAŁ BRAKÓW W KOLUMNACH DANYCH
```
# Wybór wszystkich kolumn, które mają brakujące wartości
data_frame_with_na <- data_frame[, missing_values]

# Liczba brakujących wartości w każdej z tych kolumn
na_counts <- sapply(data_frame_with_na, function(x) sum(is.na(x)))

# Obliczenie procentu brakujących wartości dla każdej kolumny
percent_na <- (na_counts / nrow(data_frame)) * 100

# Sortowanie procentu brakujących wartości rosnąco
sorted_percent_na <- sort(percent_na)

# Wyświetlenie wyników
print(sorted_percent_na)
```
OSZACOWANIE CZASOCHŁONNOŚCI

RELATYWNIE DUŻO DANYCH ORAZ BRAKUJĄCYCH WARTOŚCI (NAN) WYMAGAĆ BEDZIE WIĘCEJ CZASOCHŁONNOŚCI - SZACOWANY CZAS OBRÓBKI DANYCH - 8 DO 12 GODZIN (OKOŁO 1,5 DZNIA ROBOCZEGO).

WARTOŚCI BRAKUJĄCE W DUŻYM PROCENCIE ZOSTANĄ USUNIĘTE JEŚLI NIE MOŻNA ICH WYWNIOSKOWAĆ Z INNYCH ZMIENNYCH.

## PRZYGOTOWANIE DANYCH

### WSTĘPNE USUNIĘCIE KOLUMN Z BRAKUJĄCYMI DANYMI, KTÓRYCH NIE MOŻNA UZYSKAĆ LUB ZASTĄPIĆ
```
# Usuwanie kolumn 'neighbourhood_group_cleansed' i 'calendar_updated'
data_frame <- data_frame[, !colnames(data_frame) %in% c('neighbourhood_group_cleansed', 'calendar_updated')]

# Wyświetlenie wyników, aby upewnić się, że kolumny zostały usunięte
print(colnames(data_frame))
```
### ZAMIANA TEKSTU CENY WYNAJMU NA WARTOŚCI ZMIENNOPRZECINKOWE
```
# Usuwanie symboli dolara i przecinków oraz konwersja na typ numeric
data_frame$price <- gsub("\\$", "", data_frame$price)  # Usuwanie symboli dolara
data_frame$price <- gsub(",", "", data_frame$price)    # Usuwanie przecinków
data_frame$price <- as.numeric(data_frame$price)       # Konwersja na typ numeric

# Wyświetlenie pierwszych kilku wierszy, aby upewnić się, że operacja się powiodła
print(head(data_frame$price))
```
### ILOŚĆ BRAKUJĄCYCH DANYCH W KOLUMNIE 'price'
```
# Liczba brakujących wartości w kolumnie 'price'
na_count_price <- sum(is.na(data_frame$price))

# Wyświetlenie wyniku
print(na_count_price)
```
### MEDIANA DLA WARTOSCI NOT NAN W KOLUMNIE 'price'
```
# Mediana kolumny 'price', ignorując brakujące wartości
median_price <- median(data_frame$price, na.rm = TRUE)

# Wyświetlenie wyniku
print(median_price)
```
### UZUPEŁNIENIE BRAKÓW
```
# Zastąpienie brakujących wartości w kolumnie 'price' wartością 120.0
data_frame$price[is.na(data_frame$price)] <- 120.0

# Wyświetlenie pierwszych kilku wierszy, aby upewnić się, że operacja się powiodła
print(head(data_frame$price))
```
### SPRAWDZENIE DANYCH
```
# Sprawdzenie, czy wszystkie wartości w kolumnie 'price' są prawdziwe (różne od zera i niepuste)
all_prices_nonzero <- all(data_frame$price)

# Wyświetlenie wyniku
print(all_prices_nonzero)
```
### WIZUALIZACJA CEN WYNAJMU DLA TORONTO W ZALEZNOSCI OD POLOZENIA
```
# Załadowanie potrzebnych bibliotek
library(ggplot2)

# Funkcja generująca kolory mapy kolorów 'jet'
jet.colors <- function(n) {
  colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))(n)
}

# Załadowanie potrzebnych bibliotek
library(ggplot2)

# Tworzenie wykresu rozrzutu z kolorowaniem punktów na podstawie ceny
ggplot(data_frame, aes(x = longitude, y = latitude, color = price)) +
  geom_point(alpha = 0.8) +
  scale_color_gradientn(colors = jet.colors(7), limits = c(0, 200), oob = scales::squish) +
  theme_minimal() +
  labs(color = 'Price') +
  theme(legend.position = "right")

```

### SPRAWDZENIE PROCENTOWEJ ZAWARTOSCI REKORDOW BEZ WARTOŚCI
```
# Ustawienie kolumny 'id' jako wiersze indeksu
rownames(data_frame) <- data_frame$id

# Usunięcie kolumny 'id' jeśli jest już ustawiona jako wiersze indeksu
data_frame$id <- NULL

# Wyświetlenie pierwszych kilku wierszy, aby upewnić się, że operacja się powiodła
print(head(data_frame))

# Sprawdzenie, które kolumny mają brakujące wartości
missing_values <- sapply(data_frame, function(x) sum(is.na(x)) > 0)

# Wybór wszystkich kolumn, które mają brakujące wartości
data_frame_with_na <- data_frame[, missing_values]

# Liczba brakujących wartości w każdej z tych kolumn
na_counts <- sapply(data_frame_with_na, function(x) sum(is.na(x)))

# Obliczenie procentu brakujących wartości dla każdej kolumny
percent_na <- (na_counts / nrow(data_frame)) * 100

# Sortowanie procentu brakujących wartości malejąco
sorted_percent_na <- sort(percent_na, decreasing = TRUE)

# Wyświetlenie wyników
print(sorted_percent_na)
```
### USUNIĘCIE DODATKOWYCH KOLUMN DANYCH Z BRAKUJĄCYMI DANYMI
```
library(dplyr)

# Lista kolumn do usunięcia
columns_to_remove <- c('host_about', 'neighbourhood', 'neighborhood_overview', 'host_neighbourhood')

# Usuwanie kolumn, które istnieją w data_frame
existing_columns_to_remove <- columns_to_remove[columns_to_remove %in% colnames(data_frame)]
data_frame <- data_frame %>% select(-one_of(existing_columns_to_remove))

# Wyświetlenie nazw kolumn, aby upewnić się, że operacja się powiodła
print(colnames(data_frame))
```
### USUNIĘCIE scrape_id JAK KOLUMNY NIE WNOSZĄCEJ UŻYTECZNEJ INFORMACJI DO ANALIZY
```
# Wyświetlenie unikalnych wartości w kolumnie 'scrape_id'
unique_scrape_id <- unique(data_frame$scrape_id)

# Wyświetlenie wyników
print(unique_scrape_id)

library(dplyr)

# Sprawdzenie, czy kolumna 'scrape_id' istnieje, zanim ją usuniemy
if ("scrape_id" %in% colnames(data_frame)) {
  data_frame <- data_frame %>%
    select(-scrape_id)
}

# Wyświetlenie nazw kolumn, aby upewnić się, że operacja się powiodła
print(colnames(data_frame))
```
### OBRÓBKA DANYCH TYPU DATETIME
```
# Konwersja kolumn na typ daty i wyświetlenie unikalnych wartości
date_columns <- c('last_scraped', 'last_review', 'first_review')

for (column in date_columns) {
  data_frame[[column]] <- as.Date(data_frame[[column]])
  unique_dates <- unique(data_frame[[column]])
  cat("---", column, "---\n")
  print(unique_dates)
}
```
### UZUPEŁNIENIE WARTOŚCI 'NAT'
```
# Ustawienie domyślnych dat
default_date_last_review <- as.Date('2023-12-12')
default_date_first_review <- as.Date('2018-10-19')

# Wypełnianie brakujących wartości w kolumnie 'last_review' domyślną datą
data_frame$last_review[is.na(data_frame$last_review)] <- default_date_last_review

# Wypełnianie brakujących wartości w kolumnie 'first_review' domyślną datą
data_frame$first_review[is.na(data_frame$first_review)] <- default_date_first_review

# Wyświetlenie pierwszych kilku wierszy, aby upewnić się, że operacja się powiodła
print(head(data_frame[c('last_review', 'first_review')]))
```
### SPRAWDZENIE UZUPEŁNIENIA
```
# Konwersja kolumn na typ daty i wyświetlenie unikalnych wartości
date_columns <- c('last_scraped', 'last_review', 'first_review')

for (column in date_columns) {
  data_frame[[column]] <- as.Date(data_frame[[column]])
  unique_dates <- unique(data_frame[[column]])
  cat("---", column, "---\n")
  print(unique_dates)
}
```
### OBRÓBKA DANYCH TYPU BOOLEEN
```
# Wybieranie kolumn, które zawierają wartości 't' lub 'f'
data_true_false <- colnames(data_frame)[sapply(data_frame, function(col) any(col %in% c('t', 'f')))]

# Wyświetlenie wyników
print(data_true_false)
```
### MAPOWANIE DANYCH NA WARTOŚCI BOOLEAN
```
# Iteracja przez kolumny z wartościami 't' i 'f' oraz ich konwersja
for (column in data_true_false) {
  data_frame[[column]] <- ifelse(data_frame[[column]] == 't', TRUE,
                                 ifelse(data_frame[[column]] == 'f', FALSE, FALSE))
}

# Wyświetlenie pierwszych kilku wierszy, aby upewnić się, że operacja się powiodła
print(head(data_frame[, data_true_false]))
```
### SPRAWDZENIE ZMAPOWANYCH DANYCH
```
# Iteracja przez kolumny z wartościami 't' i 'f' oraz drukowanie unikalnych wartości
for (column in data_true_false) {
  unique_values <- unique(data_frame[[column]])
  cat(column, ":", unique_values, "\n")
}
```
## OBRÓBKA KOLUMN 'host_response_rate' i 'host_acceptance_rate'


### ZAMIANA CENY NA WARTOŚĆ ZMIENNOPRZECINKOWA ZAMIANA host_* NA WARTOŚCI ZMIENNOPRZECINKOWE DOZWIERCIEDLAJĄCE PROCENTY
```
# Usuwanie znaków procenta, konwersja na typ numeric i dzielenie przez 100
data_frame$host_response_rate <- as.numeric(gsub('%', '', data_frame$host_response_rate)) / 100.0
data_frame$host_acceptance_rate <- as.numeric(gsub('%', '', data_frame$host_acceptance_rate)) / 100.0

# Wyświetlenie pierwszych kilku wierszy, aby upewnić się, że operacja się powiodła
print(head(data_frame[c('host_response_rate', 'host_acceptance_rate')]))

# Sprawdzenie, czy jakiekolwiek wartości w kolumnie 'host_response_rate' są prawdziwe (różne od zera i niepuste)
any_host_response_rate <- any(data_frame$host_response_rate)

# Wyświetlenie wyniku
print(any_host_response_rate)

# Sprawdzenie, czy jakiekolwiek wartości w kolumnie 'host_acceptance_rate' są prawdziwe (różne od zera i niepuste)
any_host_acceptance_rate <- any(data_frame$host_acceptance_rate)

# Wyświetlenie wyniku
print(any_host_acceptance_rate)
```
### ROZSZYCIE WARTOŚCI LISTOWYCH W KOLUMNIE 'host_verifications' - JSON
```
# Wyświetlenie unikalnych wartości w kolumnie 'host_verifications'
unique_host_verifications <- unique(data_frame$host_verifications)

# Wyświetlenie wyników
print(unique_host_verifications)
```
### SPLIT PRZY UŻYCIU REGEXP
```
# Usuwanie określonych znaków i dzielenie ciągów na listy słów w kolumnie 'host_verifications'
data_frame$host_verifications_1 <- lapply(data_frame$host_verifications, function(x) {
  x <- as.character(x)  # Konwersja x do ciągu znaków
  if (x == "[]" || x == "None") {
    return(character(0))  # Zwraca pustą listę dla "[]" lub "None"
  }
  # Usunięcie nawiasów i podział ciągu na elementy listy
  x_clean <- gsub("\\[|\\]|'", "", x)
  strsplit(x_clean, ",\\s*")[[1]]
})

# Wyświetlenie wyników
print(head(data_frame$host_verifications_1))

# Wyświetlenie unikalnych wartości w kolumnie 'host_verifications'
unique_host_verifications_1 <- unique(data_frame$host_verifications_1)
print(unique_host_verifications_1)
```
### PROCENTOWY UDZIAŁ WARTOŚCI W KOLUMNIE
```
# Obliczenie rozkładu częstości wartości w kolumnie 'host_verifications'
# Używamy funkcji table() do stworzenia tabeli częstości, a następnie prop.table() do znormalizowania wartości

# Ponieważ `data_frame$host_verifications` jest listą, musimy najpierw unlistować i skonwertować na faktor
host_verifications_flat <- unlist(data_frame$host_verifications_1)

# Utworzenie tabeli częstości i znormalizowanie wartości
verification_counts <- table(host_verifications_flat)
normalized_counts <- prop.table(verification_counts)

# Wyświetlenie wyników
print(normalized_counts)

# Wyświetlenie struktury ramki danych
str(data_frame)

# Podsumowanie danych w każdej kolumnie
summary(data_frame)

# Liczba brakujących wartości w każdej kolumnie
missing_values <- sapply(data_frame, function(x) sum(is.na(x)))
print(missing_values)

# Typy danych w każdej kolumnie
data_types <- sapply(data_frame, class)
print(data_types)
```
### USUNIĘCIE KOLUMNY 'amenities' - BRAK JAKICHKOLWIEK DANYCH I MOŻLIWOŚCI ICH UZUPEŁNIENIA
```
# Wyświetlenie unikalnych wartości w kolumnie 'amenities'
unique_amenities <- unique(data_frame$amenities)

# Wyświetlenie wyników
print(unique_amenities)

library(dplyr)

# Sprawdzenie, czy kolumna 'amenities' istnieje, zanim ją usuniemy
if ("amenities" %in% colnames(data_frame)) {
  data_frame <- data_frame %>%
    select(-amenities)
}

# Wyświetlenie nazw kolumn, aby upewnić się, że operacja się powiodła
print(colnames(data_frame))
```
### OBRÓBKA KOLUMNY 'room_type' I 'host_response_time' - KATEGORYCZNE DANE
```
# Wyświetlenie unikalnych wartości w kolumnie 'room_type'
unique_room_type <- unique(data_frame$room_type)

# Wyświetlenie wyników
print(unique_room_type)

# Konwersja kolumny 'room_type' na typ faktor (kategoria)
data_frame$room_type <- as.factor(data_frame$room_type)

# Wyświetlenie pierwszych kilku wierszy, aby upewnić się, że operacja się powiodła
print(head(data_frame$room_type))

# Wyświetlenie zawartości kolumny 'room_type'
room_type_content <- data_frame$room_type

# Wyświetlenie wyników
print(room_type_content)
```
### SPRAWDZENIE DANYCH UNIKALNYCH - WIDAC WARTOSCI NAN
```
# Wyświetlenie unikalnych wartości w kolumnie 'host_response_time'
unique_host_response_time <- unique(data_frame$host_response_time)

# Wyświetlenie wyników
print(unique_host_response_time)

# Wyświetlenie podsumowania statystycznego kolumny 'host_response_time'
host_response_time_summary <- summary(data_frame$host_response_time)

# Wyświetlenie wyników
print(host_response_time_summary)

# Zastąpienie brakujących wartości w kolumnie 'host_response_time' wartością 'within an hour'
data_frame$host_response_time[is.na(data_frame$host_response_time)] <- 'within an hour'

# Wyświetlenie pierwszych kilku wierszy, aby upewnić się, że operacja się powiodła
print(head(data_frame$host_response_time))
```
### UDZIAŁ PROCENTOWY DANYCH KATEGORYCZNYCH W KOLUMNIE
```
# Obliczenie rozkładu częstości wartości w kolumnie 'room_type' i znormalizowanie do sumy 1
room_type_counts <- prop.table(table(data_frame$room_type))

# Wyświetlenie wyników
print(room_type_counts)
```
### ZAMIANA NA ZMIENNĄ KATEGORYCZNĄ
```
# Konwersja kolumny 'host_response_time' na typ faktor (kategoria)
data_frame$host_response_time <- as.factor(data_frame$host_response_time)

# Wyświetlenie pierwszych kilku wierszy, aby upewnić się, że operacja się powiodła
print(head(data_frame$host_response_time))
```
### SPRAWDZENIE ZMIENNYCH DLA KOLUMNY 'host_response_time'
```
# Obliczenie rozkładu częstości wartości w kolumnie 'host_response_time' i znormalizowanie do sumy 1
host_response_time_counts <- prop.table(table(data_frame$host_response_time))

# Wyświetlenie wyników
print(host_response_time_counts)
```
### WYPEŁNIENIE KOLUMNY 'bathrooms' PRZY UŻYCIU DANYCH Z KOLUMNY 'bathrooms_text'
```
# Wyświetlenie unikalnych wartości w kolumnie 'bathrooms_text'
unique_bathrooms_text <- unique(data_frame$bathrooms_text)

# Wyświetlenie wyników
print(unique_bathrooms_text)

# Usunięcie wartości NA z kolumny 'bathrooms_text'
bathrooms_text_non_na <- data_frame$bathrooms_text[!is.na(data_frame$bathrooms_text)]

# Sprawdzenie, czy ciągi w kolumnie 'bathrooms_text' zawierają słowo 'half' (ignorując wielkość liter)
contains_half <- grepl('half', bathrooms_text_non_na, ignore.case = TRUE)

# Wyświetlenie unikalnych wartości wyników
unique_contains_half <- unique(contains_half)

# Wyświetlenie wyników
print(unique_contains_half)
```
### PODSTAWIENIE DANYCH PRZY POMOCY np.where - IF half -> 0.5 ELSE WARTOŚĆ WYCIĘTA Z POCZĄTKU TEKSTU
```
# Funkcja pomocnicza do sprawdzenia, czy wartość jest liczbą
is_number <- function(x) {
  !is.na(suppressWarnings(as.numeric(x)))
}

# Zamiana wartości w kolumnie 'bathrooms_text' na 0.5, jeśli zawierają słowo 'half'
# W przeciwnym razie użycie pierwszego słowa z ciągu jako liczby
bathrooms_numeric <- ifelse(
  is.na(data_frame$bathrooms_text), NA,  # Obsługa wartości NA
  ifelse(
    grepl('half', data_frame$bathrooms_text, ignore.case = TRUE),
    0.5,
    as.numeric(sapply(strsplit(data_frame$bathrooms_text, " "), function(x) ifelse(is_number(x[1]), x[1], NA)))
  )
)

# Wyświetlenie pierwszych kilku wierszy, aby upewnić się, że operacja się powiodła
print(head(bathrooms_numeric))
```
### UŻYCIE METODY UNIQUE DO SPRAWDZENIA ZAMIANY
```
# Funkcja pomocnicza do sprawdzenia, czy wartość jest liczbą
is_number <- function(x) {
  !is.na(suppressWarnings(as.numeric(x)))
}

# Zamiana wartości w kolumnie 'bathrooms_text' na 0.5, jeśli zawierają słowo 'half'
# W przeciwnym razie użycie pierwszego słowa z ciągu jako liczby
bathrooms_numeric <- ifelse(
  is.na(data_frame$bathrooms_text), NA,  # Obsługa wartości NA
  ifelse(
    grepl('half', data_frame$bathrooms_text, ignore.case = TRUE),
    0.5,
    sapply(strsplit(data_frame$bathrooms_text, " "), function(x) {
      first_word <- x[1]
      if (is_number(first_word)) {
        as.numeric(first_word)
      } else {
        NA
      }
    })
  )
)

# Wyświetlenie unikalnych wartości po przekształceniach
unique_bathrooms_numeric <- unique(bathrooms_numeric)

# Wyświetlenie wyników
print(unique_bathrooms_numeric)
```
### PRZYPISANIE DANYCH
```
# Funkcja pomocnicza do sprawdzenia, czy wartość jest liczbą
is_number <- function(x) {
  !is.na(suppressWarnings(as.numeric(x)))
}

# Zamiana wartości w kolumnie 'bathrooms_text' na 0.5, jeśli zawierają słowo 'half'
# W przeciwnym razie użycie pierwszego słowa z ciągu jako liczby
data_frame$bathrooms <- ifelse(
  is.na(data_frame$bathrooms_text), NA,  # Obsługa wartości NA
  ifelse(
    grepl('half', data_frame$bathrooms_text, ignore.case = TRUE),
    0.5,
    sapply(strsplit(data_frame$bathrooms_text, " "), function(x) {
      first_word <- x[1]
      if (is_number(first_word)) {
        as.numeric(first_word)
      } else {
        NA
      }
    })
  )
)

# Wyświetlenie pierwszych kilku wierszy nowej kolumny, aby upewnić się, że operacja się powiodła
print(head(data_frame$bathrooms))

# Wyświetlenie unikalnych wartości w kolumnie 'bathrooms'
unique_bathrooms <- unique(data_frame$bathrooms)

# Wyświetlenie wyników
print(unique_bathrooms)


# Liczba wystąpień brakujących wartości w kolumnie 'bathrooms'
na_counts_bathrooms <- table(is.na(data_frame$bathrooms))

# Wyświetlenie wyników
print(na_counts_bathrooms)
```
### WYLICZANIE MEDIANY (BEZ BRANIA POD UWAGE NAN)
```
# Mediana kolumny 'bathrooms', ignorując brakujące wartości
median_bathrooms <- median(data_frame$bathrooms, na.rm = TRUE)

# Wyświetlenie wyniku
print(median_bathrooms)

# Obliczenie mediany kolumny 'bathrooms', ignorując brakujące wartości
median_bathrooms <- median(data_frame$bathrooms, na.rm = TRUE)

# Wypełnienie brakujących wartości w kolumnie 'bathrooms' medianą
data_frame$bathrooms <- ifelse(is.na(data_frame$bathrooms), median_bathrooms, data_frame$bathrooms)

# Alternatywnie, można użyć `replace` z pakietu `dplyr`
# library(dplyr)
# data_frame <- data_frame %>%
#   mutate(bathrooms = replace(bathrooms, is.na(bathrooms), median_bathrooms))

# Wyświetlenie pierwszych kilku wierszy, aby upewnić się, że operacja się powiodła
print(head(data_frame$bathrooms))

# Obliczenie rozkładu częstości wartości w kolumnie 'bathrooms'
bathrooms_counts <- table(data_frame$bathrooms)

# Wyświetlenie wyników
print(bathrooms_counts)

# Obliczenie rozkładu częstości wartości w kolumnie 'bathrooms' i znormalizowanie do sumy 1
bathrooms_counts_normalized <- prop.table(table(data_frame$bathrooms))

# Wyświetlenie wyników
print(bathrooms_counts_normalized)

# Liczba brakujących wartości w kolumnie 'bedrooms'
na_count_bedrooms <- sum(is.na(data_frame$bedrooms))

# Wyświetlenie wyniku
print(na_count_bedrooms)


# Wyświetlenie unikalnych wartości w kolumnie 'bedrooms'
unique_bedrooms <- unique(data_frame$bedrooms)

# Wyświetlenie wyników
print(unique_bedrooms)
```
### OBRÓBKA DANYCH Z KOLUMNY 'bedrooms'
```
# Wybranie wierszy, w których kolumna 'bedrooms' ma wartość NA, oraz zliczenie wartości w kolumnie 'room_type'
room_type_counts_for_na_bedrooms <- table(data_frame$room_type[is.na(data_frame$bedrooms)])

# Wyświetlenie wyników
print(room_type_counts_for_na_bedrooms)

# Wyświetlanie unikalnych wartości w kolumnie 'bedrooms'
unique_bedrooms <- unique(data_frame$bedrooms)
print(unique_bedrooms)

# Wyświetlanie liczby wystąpień wartości w kolumnie 'room_type', gdzie 'bedrooms' jest NA
room_type_counts <- table(data_frame$room_type[is.na(data_frame$bedrooms)])
print(room_type_counts)

# Usuwanie kolumny 'bedrooms'
data_frame <- data_frame[, !colnames(data_frame) %in% 'bedrooms']

# Wyświetlenie nazwy kolumn, aby upewnić się, że operacja się powiodła
print(colnames(data_frame))
```
### USUNIĘCIE PUSTEJ KOLUMNY 'description'
```
# Wyświetlenie unikalnych wartości w kolumnie 'description'
unique_description <- unique(data_frame$description)

# Wyświetlenie wyników
print(unique_description)

library(dplyr)
data_frame <- data_frame %>%
  select(-description)

# Wyświetlenie nazw kolumn, aby upewnić się, że operacja się powiodła
print(colnames(data_frame))
```
### SPRAWDZENIE DANYCH TYPU OBIEKT
```
# Generowanie podsumowania statystycznego dla kolumn znakowych i transponowanie wyniku
# Najpierw wyselekcjonujemy tylko kolumny znakowe, a następnie wygenerujemy podsumowanie

# Filtracja kolumn znakowych
char_cols <- sapply(data_frame, is.character)
data_frame_char <- data_frame[, char_cols]

# Generowanie podsumowania statystycznego
char_summary <- summary(data_frame_char)

# Transponowanie wyniku
char_summary_t <- t(char_summary)

# Wyświetlenie wyników
print(char_summary_t)
```
### SPRAWDZENIE DANYCH DATETIME
```
# Generowanie podsumowania statystycznego dla kolumn nie będących liczbami i transponowanie wyniku
# Najpierw wyselekcjonujemy kolumny niebędące liczbami, a następnie wygenerujemy podsumowanie

# Filtracja kolumn nie będących liczbami (np. znakowe, daty)
non_numeric_cols <- sapply(data_frame, function(x) !is.numeric(x))
data_frame_non_numeric <- data_frame[, non_numeric_cols]

# Generowanie podsumowania statystycznego
non_numeric_summary <- summary(data_frame_non_numeric)

# Transponowanie wyniku
non_numeric_summary_t <- t(non_numeric_summary)

# Wyświetlenie wyników
print(non_numeric_summary_t)
```
### KODOWANIE ZMIENNYCH KATEGORYCZNYCH
```
# Kodowanie kategorycznych kolumn na wartości liczbowe za pomocą `as.integer(factor(x))`
data_frame$room_type_encoded <- as.integer(factor(data_frame$room_type))
data_frame$host_response_time_encoded <- as.integer(factor(data_frame$host_response_time))
data_frame$neighbourhood_cleansed_encoded <- as.integer(factor(data_frame$neighbourhood_cleansed))
data_frame$property_type_encoded <- as.integer(factor(data_frame$property_type))

# Wyświetlenie pierwszych kilku wierszy, aby upewnić się, że operacja się powiodła
print(head(data_frame[c('room_type_encoded', 'host_response_time_encoded', 'neighbourhood_cleansed_encoded', 'property_type_encoded')]))
```
### SKOŚNOŚĆ I KURTOZA DLA DANYCH NUMERYCZNYCH
```
# Załadowanie potrzebnych bibliotek
library(e1071)

# Iteracja przez wszystkie kolumny i obliczanie kurtozy oraz skośności dla kolumn numerycznych
for (column in colnames(data_frame)) {
  tryCatch({
    if (!is.factor(data_frame[[column]]) && !is.character(data_frame[[column]])) {
      # Wyliczenie kurtozy i skośności
      kurtoza <- kurtosis(data_frame[[column]], na.rm = TRUE)
      skosnosc <- skewness(data_frame[[column]], na.rm = TRUE)
      cat("\n\n")
      cat('NAZWA KOLUMNY:', column, "\n")
      cat('KURTOZA:', kurtoza, "\n")
      cat('SKOŚNOŚĆ:', skosnosc, "\n")
      cat('TYP DANYCH:', class(data_frame[[column]]), "\n")
    }
  }, error = function(e) {
    # Obsługa błędów
  })
}

```
### ZALEŻNOŚĆ 'review_scores_communication' ORAZ 'review_scores_checkin'
```
# Usunięcie brakujących wartości w kolumnie 'review_scores_location'
review_scores_location_non_na <- data_frame$review_scores_location[!is.na(data_frame$review_scores_location)]

# Obliczenie logarytmu naturalnego dla kolumny 'review_scores_location'
log_host_r_t_e <- log(review_scores_location_non_na)

# Tworzenie histogramu
hist(log_host_r_t_e)

library(ggplot2)

# Tworzenie wykresu punktowego (scatter plot)
ggplot(data = data_frame, aes(x = review_scores_communication, y = review_scores_checkin)) +
  geom_point() +
  labs(x = "Review Scores Communication", y = "Review Scores Checkin") +
  ggtitle("Scatter Plot of Review Scores Communication vs Checkin")
```
### REVIEW SCORES RATING
```
library(dplyr)
library(ggplot2)

# Grupowanie po 'host_response_time' i obliczenie średniej 'review_scores_communication'
avg_review_scores <- data_frame %>%
  group_by(host_response_time) %>%
  summarise(mean_communication = mean(review_scores_communication, na.rm = TRUE))

# Tworzenie wykresu słupkowego
ggplot(data = avg_review_scores, aes(x = host_response_time, y = mean_communication)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Host Response Time", y = "Mean Review Scores Communication") +
  ggtitle("Mean Review Scores Communication by Host Response Time")
```
### OCZYSZCZENIE DANYCH W KOLUMNIE 'review_scores_rating'
```
# Wybieranie kolumn liczbowych i obliczanie macierzy korelacji
numeric_cols <- data_frame[sapply(data_frame, is.numeric)]
corr <- cor(numeric_cols, use = "complete.obs")

# Sprawdzenie, czy 'review_scores_rating' jest w macierzy korelacji
if ('review_scores_rating' %in% rownames(corr)) {
  # Tworzenie histogramu dla korelacji z 'review_scores_rating'
  hist(corr['review_scores_rating', ], breaks = 20,
       main = "Histogram of Correlation with review_scores_rating",
       xlab = "Correlation")
} else {
  print("Kolumna 'review_scores_rating' nie istnieje lub nie jest liczbową kolumną.")
}

# Usunięcie górnej i prawej ramki wykresu
par(lend = 1)
```
### PODSTAWIENIE MODĄ
```
# Wyświetlenie unikalnych wartości w kolumnie 'review_scores_rating', w tym NaN
unique_values <- unique(data_frame$review_scores_rating)

print(unique_values)

library(dplyr)

# Znalezienie modalnej wartości kolumny 'review_scores_rating'
mode_rsr <- data_frame %>%
  summarise(mode_rsr = names(which.max(table(review_scores_rating))))

print(mode_rsr$mode_rsr)

# Znajdź modalną wartość kolumny 'review_scores_rating'
mode_rsr <- names(which.max(table(data_frame$review_scores_rating)))

# Uzupełnij brakujące wartości w kolumnie 'review_scores_rating' wartością modalną
data_frame$review_scores_rating[is.na(data_frame$review_scores_rating)] <- mode_rsr

```
### SPRAWDZENIE KOLUMNY 'reviews_per_month' - WARTOŚCI NAN
```
review_scores_rating_column <- data_frame$review_scores_rating
print(review_scores_rating_column)

```
### SPRAWDZENIE KOLUMNY 'reviews_per_month' - PODSTAWIENIE
```
# Filtracja ramki danych, usuwając wiersze, gdzie 'reviews_per_month' jest NA
filtered <- data_frame[!is.na(data_frame$reviews_per_month), ]$reviews_per_month

# Obliczenie percentyla 95
perc <- quantile(filtered, probs = 0.95, na.rm = TRUE)

# Znalezienie modalnej wartości 'reviews_per_month'
data_mode <- names(which.max(table(data_frame$reviews_per_month)))

# Uzupełnienie brakujących wartości w kolumnie 'reviews_per_month' wartością modalną
data_frame$reviews_per_month[is.na(data_frame$reviews_per_month)] <- data_mode

# Obliczenie liczby brakujących wartości w kolumnie 'reviews_per_month'
num_na <- sum(is.na(data_frame$reviews_per_month))

print(num_na)

```
### UZUPEŁNIENIE BRAKUJĄCYCH RATE'ÓW DLA HOSTA WARTOŚCIAMI ŚREDNIMI
```
# Obliczenie średniej wartości kolumny 'host_response_rate'
mean_rates <- mean(data_frame$host_response_rate, na.rm = TRUE)

print(mean_rates)

# Obliczenie średniej wartości kolumny 'host_acceptance_rate'
mean_rates2 <- mean(data_frame$host_acceptance_rate, na.rm = TRUE)

print(mean_rates2)


# Obliczenie średnich wartości dla kolumn 'host_response_rate' i 'host_acceptance_rate'
mean_response_rate <- mean(data_frame$host_response_rate, na.rm = TRUE)
mean_acceptance_rate <- mean(data_frame$host_acceptance_rate, na.rm = TRUE)

# Wypełnienie brakujących wartości w kolumnie 'host_response_rate'
data_frame$host_response_rate[is.na(data_frame$host_response_rate)] <- round(mean_response_rate, 2)

# Wypełnienie brakujących wartości w kolumnie 'host_acceptance_rate'
data_frame$host_acceptance_rate[is.na(data_frame$host_acceptance_rate)] <- round(mean_acceptance_rate, 2)

host_acceptance_rate_column <- data_frame$host_acceptance_rate
print(host_acceptance_rate_column)
```
### SPRAWDZENIE PODSTAWIENIA
```
unique_values <- unique(data_frame$host_response_rate)
print(unique_values)

unique_values <- unique(data_frame$host_acceptance_rate)
print(unique_values)
```
### OBRÓBKA KOLUMN ZWIĄZANYCH Z REVIEW SCORES
```
library(dplyr)

# Wybrane kolumny do obliczenia statystyk
reviews <- c('review_scores_value',
             'review_scores_location',
             'review_scores_checkin',
             'review_scores_communication',
             'review_scores_accuracy',
             'review_scores_cleanliness')

# Obliczenie mediany i średniej dla wybranych kolumn
stats <- data_frame %>%
  summarise(across(all_of(reviews), list(median = median, mean = mean), na.rm = TRUE))

# Transpozycja wyników dla lepszej czytelności
stats <- t(stats)

print(stats)
```
### SPRAWDZENIE CZY ZOSTAŁY NIEOBSŁUŻONE WARTOŚCI NAN
```
# Lista kolumn do uzupełnienia
reviews <- c('review_scores_value',
             'review_scores_location',
             'review_scores_checkin',
             'review_scores_communication',
             'review_scores_accuracy',
             'review_scores_cleanliness')

# Pętla for do uzupełnienia brakujących wartości w każdej kolumnie
for (column in reviews) {
  data_frame[[column]] <- ifelse(is.na(data_frame[[column]]),
                                 mean(data_frame[[column]], na.rm = TRUE),
                                 data_frame[[column]])
}

# Wyświetlenie zaktualizowanej ramki danych
print(data_frame)

```
### FILTR NA KOLUMNY Z WARTOŚCIAMI NUMERYCZNYMI - BEZ OBJEKTÓW, DATETIME
```
# Wybrane kolumny do sprawdzenia
reviews <- c('review_scores_value',
             'review_scores_location',
             'review_scores_checkin',
             'review_scores_communication',
             'review_scores_accuracy',
             'review_scores_cleanliness')

# Obliczenie liczby brakujących wartości w wybranych kolumnach
num_na <- colSums(is.na(data_frame[reviews]))

print(num_na)


library(dplyr)

# Lista wszystkich nazw kolumn
all_columns <- names(data_frame)

# Filtrowanie kolumn na podstawie warunków
data_frame_check <- all_columns[!(all_columns %in% c("id", "price")) &
                                !grepl("id", all_columns) &
                                !grepl("price", all_columns) &
                                !(sapply(data_frame[, all_columns], class) %in% c("character", "POSIXct"))]

print(data_frame_check)

```
### TABELA Z WARTOŚCIAMI OPISUJĄCYMI DANE - ILOŚĆ, ŚREDNIA, ODCHYLENIE STANDARDOWE, WARTOŚCI MINIMALNA, PERCENTYLE, WARTOŚCI MAXYMALNE
```
library(dplyr)

# Uzyskanie statystyk opisowych dla wybranych kolumn
summary_stats <- data_frame %>%
  select(all_of(data_frame_check)) %>%
  summary()

# Transpozycja wyników dla lepszej czytelności
summary_stats <- t(summary_stats)

print(summary_stats)
```
## WYKRESY

### SPRAWDZENIE OUTLAIER'ÓW
```
# Wybierz tylko kolumny numeryczne
numeric_columns <- Filter(is.numeric, data_frame)

# Pętla for do tworzenia wykresów punktowych
for (column in names(numeric_columns)) {
  plot(x = numeric_columns[[column]], y = numeric_columns[[column]],
       main = column, xlab = column, ylab = column)
}

# Pętla for do tworzenia wykresów
for (column in names(data_frame)) {
  tryCatch({
    # Tworzenie wykresu histogramu
    hist(data_frame[[column]], main = column, xlab = column, col = "lightblue")
    print("\n\n")
    Sys.sleep(1)  # Pauza dla lepszego rozróżnienia wykresów
  }, error = function(e) {
    # Obsługa błędów (np. gdy kolumna nie jest numeryczna)
    cat(paste("Skipping histogram for column:", column, "\n"))
  })

  tryCatch({
    # Tworzenie wykresu pudełkowego
    boxplot(data_frame[[column]], main = column, col = "lightgreen")
    Sys.sleep(1)  # Pauza dla lepszego rozróżnienia wykresów
  }, error = function(e) {
    # Obsługa błędów (np. gdy kolumna nie jest numeryczna)
    cat(paste("Skipping boxplot for column:", column, "\n"))
  })
}
```
### FUNKCJA DO USUWANIA OUTLIER'ÓW
```
identify_outliers <- function(df, column) {
  # Obliczenie kwartyli
  Q1 <- quantile(df[[column]], 0.10, na.rm = TRUE)
  Q3 <- quantile(df[[column]], 0.90, na.rm = TRUE)

  # Obliczenie szerokości przedziału międzykwartylowego (IQR)
  IQR <- Q3 - Q1

  # Dolna granica wartości odstających
  lower_bound <- Q1 - 1.5 * IQR

  # Górna granica wartości odstających
  upper_bound <- Q3 + 1.5 * IQR

  # Filtracja wartości odstających
  outliers <- df[df[[column]] < lower_bound | df[[column]] > upper_bound, ]

  return(outliers)
}
```
### UŻYCIE FUNKCJI NA ZMIENNYCH NUMERYCZNYCH
```
# Lista do przechowywania indeksów wierszy do usunięcia
indexes <- vector("list")

# Pętla for do przetwarzania kolumn
for (column in names(data_frame)) {
  # Sprawdzenie typu danych kolumny
  if (is.numeric(data_frame[[column]])) {
    # Identyfikacja wartości odstających
    outliers_df <- identify_outliers(data_frame, column)
    # Dodanie indeksów wierszy do listy
    indexes <- c(indexes, rownames(outliers_df))
  }
}

# Usunięcie wierszy zawierających wartości odstające
df_cleaned <- data_frame[setdiff(rownames(data_frame), unlist(indexes)), ]

# Wyświetlenie oczyszczonej ramki danych
print(df_cleaned)

# Pętla for do tworzenia wykresów
for (column in names(df_cleaned)) {
  tryCatch({
    # Tworzenie wykresu histogramu
    hist(df_cleaned[[column]], main = column, xlab = column, col = "lightblue")
    print("\n\n")
    Sys.sleep(1)  # Pauza dla lepszego rozróżnienia wykresów
  }, error = function(e) {
    # Obsługa błędów (np. gdy kolumna nie jest numeryczna)
    cat(paste("Skipping histogram for column:", column, "\n"))
  })

  tryCatch({
    # Tworzenie wykresu pudełkowego
    boxplot(df_cleaned[[column]], main = column, col = "lightgreen")
    Sys.sleep(1)  # Pauza dla lepszego rozróżnienia wykresów
  }, error = function(e) {
    # Obsługa błędów (np. gdy kolumna nie jest numeryczna)
    cat(paste("Skipping boxplot for column:", column, "\n"))
  })
}
```
### CECHA - 'calculated_host_listings_count_shared_rooms' - JEST STAŁA
```
unique_values <- unique(data_frame$calculated_host_listings_count_shared_rooms)
print(unique_values)
```
### KOWARIANCJA MIĘDZY TĄ ZMIENNĄ A INNYMI ZMIENNYCMI POZOSTAJE STAŁA
```
library(dplyr)

# Sprawdzenie, czy kolumna 'calculated_host_listings_count_shared_rooms' istnieje
if ("calculated_host_listings_count_shared_rooms" %in% colnames(data_frame)) {
  # Usunięcie kolumny 'calculated_host_listings_count_shared_rooms' za pomocą dplyr
  data_frame <- data_frame %>%
    select(-calculated_host_listings_count_shared_rooms)
} else {
  print("Kolumna 'calculated_host_listings_count_shared_rooms' nie istnieje.")
}

# Wyświetlenie nazw kolumn, aby upewnić się, że operacja się powiodła
print(colnames(data_frame))
```
### MACIEŻ KORELACJI

Macież korelacji
```
library(dplyr)

# Wybór kolumn numerycznych
numeric_columns <- Filter(is.numeric, data_frame)

# Obliczenie macierzy korelacji
corr <- cor(select_if(data_frame, is.numeric))

# Wyświetlenie macierzy korelacji
print(corr)
```
## ZBADANIE ZALEŻNOŚCI POMIĘDZY ZMIENNYMI – KROK TEN POZWOLI ODKRYĆ ZWIĄZKI POMIĘDZY POSZCZEGÓLNYMI ZMIENNYMI

### KORELACJA PRZY UZYCIU METODY PEARSON
```
install.packages("reshape2")
install.packages("plotly")

library(ggplot2)
library(reshape2)
library(plotly)

# Wybór kolumn numerycznych
numeric_columns <- Filter(is.numeric, data_frame)

# Obliczenie macierzy korelacji
corr_matrix <- cor(numeric_columns, use = "complete.obs")

# Przekształcenie macierzy do formatu odpowiedniego dla ggplot2
corr_matrix_melted <- melt(corr_matrix)

# Utworzenie mapy cieplnej za pomocą ggplot2
heatmap_plot <- ggplot(data = corr_matrix_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       limits = c(-1, 1), breaks = seq(-1, 1, by = 0.2),
                       name = "Korelacja", labels = scales::number_format(accuracy = 0.1)) +
  labs(title = "Korelacja cech - metoda Pearsona") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Konwersja do interaktywnego wykresu przy użyciu plotly (opcjonalne)
heatmap_plotly <- ggplotly(heatmap_plot)

# Wyświetlenie wykresu
heatmap_plotly

print(head(corr_matrix_melted))
```
### KORELACJA PRZY UZYCIU METODY SPEARMANA
```
install.packages("ggplot2")
install.packages("Hmisc")

library(ggplot2)
library(Hmisc)

# Wybór kolumn numerycznych
numeric_columns <- Filter(is.numeric, data_frame)

# Obliczenie macierzy korelacji metodą Spearmana
corr_matrix <- rcorr(as.matrix(numeric_columns), type = "spearman")

# Macierz korelacji Spearmana
corr_spearman <- corr_matrix$r

# Ustawienie nazw wierszy i kolumn
rownames(corr_spearman) <- colnames(numeric_columns)
colnames(corr_spearman) <- colnames(numeric_columns)

# Przekształcenie macierzy do formatu odpowiedniego dla ggplot2
corr_spearman_melted <- melt(corr_spearman)

# Utworzenie mapy cieplnej za pomocą ggplot2
heatmap_plot <- ggplot(data = corr_spearman_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       limits = c(-1, 1), breaks = seq(-1, 1, by = 0.2),
                       name = "Korelacja", labels = scales::number_format(accuracy = 0.1)) +
  labs(title = "Korelacja cech dla Toronto - metoda Spearmana") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Wyświetlenie wykresu
print(heatmap_plot)
```
### ZBADANIE ZALEŻNOŚCI POMIĘDZY ZMIENNYMI KATEGORYCZNYMI
```
data <- data_frame
data2 <- data_frame

# Przypisanie istniejącej ramki danych do nowej zmiennej 'data'
data_save <- data_frame
data <- data_save

# Załadowanie wymaganych bibliotek
install.packages("vcd")
install.packages("dplyr")
install.packages("gtools")

library(vcd)
library(dplyr)
library(gtools)


# Zamiana kolumn na faktory (kategorie)
data <- data %>%
  mutate(across(c(name, source, room_type, property_type, host_id), ~ as.factor(.)))

# Tworzenie par kombinacji zmiennych
variable_combinations <- combinations(n = 5, r = 2, v = c('name', 'source', 'room_type', 'property_type', 'host_id'), set = TRUE, repeats.allowed = FALSE)

print(variable_combinations)

# Lista do przechowywania wyników V Cramera
vcramer_results <- list()

# Funkcja do obliczenia V Cramera
calculate_vcramer <- function(var1, var2, data) {
  contingency_table <- table(data[[var1]], data[[var2]])
  cramer_v <- assocstats(contingency_table)$cramer
  return(cramer_v)
}

# Iteracja przez kombinacje zmiennych
for (i in 1:nrow(variable_combinations)) {
  var1 <- variable_combinations[i, 1]
  var2 <- variable_combinations[i, 2]
  print(paste('KOMBINACJA ZMIENNYCH:', var1, 'and', var2))
  v_cramer_value <- calculate_vcramer(var1, var2, data)
  vcramer_results[[paste(var1, var2, sep = "_")]] <- v_cramer_value
  print(paste('V Cramer:', v_cramer_value))
}

# Dodatkowe obliczenia V Cramera dla każdej zmiennej z samą sobą
for (x in c('name', 'source', 'room_type', 'property_type', 'host_id')) {
  v_cramer_value <- calculate_vcramer(x, x, data)
  vcramer_results[[paste(x, x, sep = "_")]] <- v_cramer_value
}

# Wyświetlenie wyników
# print(vcramer_results)
```
### V CRAMER GENERACJA WYKRESU
```
# Załadowanie wymaganych bibliotek
install.packages("vcd")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("combinat")
install.packages("tidyr")
library(vcd)
library(dplyr)
library(ggplot2)
library(reshape2)
library(combinat)
library(tidyr)

# Generowanie przykładowych danych
data <- data.frame(
  name = sample(letters[1:5], 100, replace = TRUE),
  src = sample(LETTERS[1:5], 100, replace = TRUE),
  room_type = sample(c("Private room", "Entire home/apt", "Shared room"), 100, replace = TRUE),
  property_type = sample(c("Apartment", "House", "Condominium"), 100, replace = TRUE),
  host_id = sample(1:20, 100, replace = TRUE),
  num_feature1 = rnorm(100),
  num_feature2 = runif(100)
)

# Usunięcie pustych wartości
data <- data %>%
  drop_na()

# Zamiana na kategorie
data$name <- as.factor(data$name)
data$src <- as.factor(data$src)
data$room_type <- as.factor(data$room_type)
data$property_type <- as.factor(data$property_type)
data$host_id <- as.factor(data$host_id)

# Tworzenie par kombinacji zmiennych
variable_combinations <- combn(c("name", "src", "room_type", "property_type", "host_id"), 2, simplify = FALSE)

# Funkcja do obliczenia V Craméra
cramers_v_matrix <- function(cecha1, cecha2) {
  contingency_table <- table(cecha1, cecha2)
  chi2_result <- chisq.test(contingency_table, correct = FALSE)
  chi2 <- chi2_result$statistic
  n <- sum(contingency_table)
  min_dim <- min(dim(contingency_table)) - 1
  cramers_v <- sqrt(chi2 / (n * min_dim))
  return(cramers_v)
}

# Wyliczenie V Craméra dla wszystkich kombinacji
df_vcramer <- data.frame(matrix(NA, ncol = length(variable_combinations), nrow = 1))
colnames(df_vcramer) <- sapply(variable_combinations, function(x) paste(x, collapse = " and "))

for (i in 1:length(variable_combinations)) {
  var1 <- variable_combinations[[i]][1]
  var2 <- variable_combinations[[i]][2]
  cramers <- cramers_v_matrix(data[[var1]], data[[var2]])
  df_vcramer[1, i] <- round(cramers, 2)
}

# Obliczenie V Craméra dla par danych kategorycznych
df_cat <- data %>%
  select(where(is.factor))
rows <- list()

for (var1 in colnames(df_cat)) {
  col <- numeric(ncol(df_cat))
  for (var2 in colnames(df_cat)) {
    cramers <- cramers_v_matrix(df_cat[[var1]], df_cat[[var2]])
    col[which(colnames(df_cat) == var2)] <- round(cramers, 2)
  }
  rows[[var1]] <- col
}

# Ramka odpowiedzi
cramers_results <- do.call(rbind, rows)
colnames(cramers_results) <- colnames(df_cat)
rownames(cramers_results) <- colnames(df_cat)

# Reshaping danych
melted_cramers_results <- melt(cramers_results, varnames = c("Var1", "Var2"), value.name = "CramersV")

# Utworzenie heatmapy - ggplot2
ggplot(data = melted_cramers_results, aes(x = Var1, y = Var2, fill = CramersV)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0, 1)) +
  geom_text(aes(label = round(CramersV, 2)), color = "black") +
  theme_minimal() +
  labs(title = "Heatmapa współczynników V Craméra", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Wyliczenie R-squared
df_num <- data %>%
  select(where(is.numeric))
r_sq <- matrix(NA, nrow = ncol(df_num), ncol = ncol(df_cat))
rownames(r_sq) <- colnames(df_num)
colnames(r_sq) <- colnames(df_cat)

for (num in colnames(df_num)) {
  for (cat in colnames(df_cat)) {
    formula <- as.formula(paste(num, "~", cat))
    model <- lm(formula, data = data)
    r_sq[num, cat] <- summary(model)$r.squared
  }
}

r_sq <- round(as.data.frame(r_sq), 2)

# Reshaping ramki danych R-squared
melted_r_sq <- melt(r_sq, varnames = c("NumericVar", "CategoricalVar"), value.name = "R_Squared")

# Utworzenie heatmapy - ggplot2
ggplot(data = melted_r_sq, aes(x = CategoricalVar, y = NumericVar, fill = R_Squared)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0, 1)) +
  geom_text(aes(label = round(R_Squared, 2)), color = "black") +
  theme_minimal() +
  labs(title = "Heatmapa wartości R-Squared zmienne kategoryczne i numeryczne", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

```
### WYKRESY

![7](https://github.com/przemektutur/AirRNBinR/assets/120668914/c213b183-8a11-488b-bac1-63e53d789a8a)
![6](https://github.com/przemektutur/AirRNBinR/assets/120668914/b979d7f1-65ff-4ca8-87fb-ef928b7bf6c7)
![5](https://github.com/przemektutur/AirRNBinR/assets/120668914/c78b14ac-bdc4-4433-8014-a5114f1b33bb)
![4](https://github.com/przemektutur/AirRNBinR/assets/120668914/ee324b26-f448-4bc5-84b5-f47286fdc109)
![3](https://github.com/przemektutur/AirRNBinR/assets/120668914/c808113d-2201-4e38-8216-21429550fbed)
![2](https://github.com/przemektutur/AirRNBinR/assets/120668914/4256f67b-22bb-4633-8dcf-a3c5fc6674d9)
![1](https://github.com/przemektutur/AirRNBinR/assets/120668914/353d2deb-0893-480c-8bb7-ec18afc82aa4)
![10](https://github.com/przemektutur/AirRNBinR/assets/120668914/494c9738-dbb5-4dcd-8d9e-b10289c554c6)
![9](https://github.com/przemektutur/AirRNBinR/assets/120668914/7bfa42da-1b68-4043-bf62-756fc7574144)
![8](https://github.com/przemektutur/AirRNBinR/assets/120668914/a4fdbb9b-e271-4b64-a21a-0d2cc11a0f5e)

**PODSUMOWANIE**

WYKONANO PRZYGOTOWANIE DANYCH DO DALSZEGO WYKORZYSTANIA. ANALIZA ZAJĘŁA OKOŁO 14 GODZIN. UZUPEŁNIONO BRAKUJĄCE DANE, A GDZIE NIE BYŁO TO MOŻLIWE ZE ZWZGLĘDU NA DUŻE BRAKI USUNIĘTO KOLUNMY JAKO NIEISTOTNE. STOPNIE ZALEŻNOŚCI POMIĘDZY POSZCZEGÓLNYMI DANYMI ZOSTAŁY ZAPREZENTOWANE NA HEATMAPACH (METODA PEARSONA ORAZ METODA SPEARMANA). DOKONANO TAKŻE ANALIZY DLA DANYCH KATEGORYCZNYCH - WYLICZONO WSPÓŁCZYNNIK V CRAMERA.

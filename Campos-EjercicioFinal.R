# Limpieza del entorno
rm(list = ls())

# Cargar las librerías: 
library(dplyr)
library(ggplot2)
library(tidyverse)
library(wordcloud)
library(tm)
library(tidytext)
library(readtext)
library(stopwords)

# Leemos el archivo a utilizar (corpus2.txt)
corpus <- readtext("corpus2/*.txt")

# Tokenización
palabras <- corpus %>%
  unnest_tokens(word, text)

frecuencia_palabras <- palabras %>%
  count(word, sort = TRUE)

# Observamos cuales son las 20 palabras que aparecen con más frecuencia, sin filtrar las stopwords 
print(frecuencia_palabras, n= 20)

# Cantidad de palabras sin limpiar en general
numeroPalabras <- nrow(frecuencia_palabras) 
numeroPalabras

# Filtro palabras vacías y stop words
stop <- stopwords("spanish")  
palabras_limpias <- palabras %>%
  filter(word != "") %>%    # palabras vacias
  filter(!word %in% stop) %>%    # stop words
  filter(!str_detect(word, "\\d")) %>%  # las que no sean caracter 
  filter(nchar(word) > 1)   # las de longitud < 1 

# Cantidad de palabras en general, una vez hecha la limpieza
frecuencia_palabras_limpias <- palabras_limpias %>%
  count(word, sort = TRUE)

numero_palabras_unicas <- nrow(frecuencia_palabras_limpias)
numero_palabras_unicas

# Contar frecuencia por texto
frecuencia_por_texto <- palabras_limpias %>%
  count(doc_id, word, sort = TRUE)

top_palabras_por_texto <- frecuencia_por_texto %>%
  group_by(doc_id) %>%
  slice_max(n, n = 10) %>%   
  summarise(top_words = paste(word, collapse = ", ")) %>%
  ungroup()

top_palabras_por_texto

# Vemos las 20 palabras más frecuentes generalmente en los textos
frecuencia <- palabras_limpias %>%
  count(word, sort = TRUE)
frecuencia_20 <- head(frecuencia, 20)
print(frecuencia_20,20)

# Realizamos la Nube de Palabras
wordcloud(words = frecuencia$word, freq = frecuencia$n, max.words = 400, random.order = FALSE, colors = c("red", "blue", "green", "orange"))

# Realizamos el gráfico de frecuencias
ggplot(frecuencia_20, aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "lightblue") +
  coord_flip() +
  labs(title = "20 palabras más frecuentes",
       x = "Palabras",
       y = "Frecuencia") +
  theme_light()




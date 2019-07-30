library(tidytext)
library(tidyverse)
library(spacyr)
library(widyr)
library(tm)
library(tokenizers)


stopwords <- tibble(word = stopwords::stopwords(language = "de", 
                                                source = "stopwords-iso"))

# Lemma lookup table
# https://github.com/michmech/lemmatization-lists
lemma_lookup <- read_delim("lemmatization-de_noun.txt", delim = "\t") %>%
  set_names(c("lemma", "word"))


# Function to process texts
process_text <- function(text, treshold) {

  # Tokenize into sentences
  sentences <- tibble(
    text = tokenize_sentences(text)[[1]]) %>%
    rownames_to_column(var = "line")
  
  # Tidy text
  tidy_text <- sentences %>%
    unnest_tokens(word, text, to_lower = FALSE) %>%
    left_join(lemma_lookup, by = "word") %>%
    mutate(
      lemma = coalesce(lemma, word),
      uppercase = grepl("^[A-Z]", lemma)
    ) %>%
    dplyr::filter(uppercase == TRUE) %>%
    mutate(
      word = tolower(lemma)
    ) %>%
    anti_join(stopwords, by = "word")

  # Build pairs
  pairs <- pairwise_count(tidy_text,
                          item = word,
                          feature = line,
                          sort = TRUE) %>%
    dplyr::filter(n > treshold)
  
  nodes <- tidy_text %>% 
    select(-line, lemma, -word, -uppercase) %>% 
    transmute(label = str_to_lower(lemma)) %>% 
    rownames_to_column(var = "id")
  
  nodes <- pairs %>% select(-n) %>% 
    gather(variable, value) %>% 
    select(-variable) %>% 
    unique() %>% 
    rownames_to_column(var = "id") %>% 
    rename(label = value) %>% 
    mutate(title = label,
           font.color = "white")
  
  edges <- pairs %>%
    rename(label = item1) %>% 
    left_join(nodes, by = "label") %>% 
    rename(from = id) %>% 
    select(-label) %>% 
    rename(label = item2) %>% 
    left_join(nodes, by = "label") %>% 
    rename(to = id) %>% 
    select(-label) %>% 
    mutate(
      concat_one = paste0(from, to) %>% 
        map_chr(~ paste(sort(unlist(strsplit(., ""))), collapse = ""))
    ) %>%
    group_by(concat_one) %>% 
    slice(1:1) %>% ungroup %>% 
    mutate(width = n)
  
  visNetwork(nodes, edges, width = "100%", height = "1000px") %>% 
    visNodes(shape = "ellipse",
             color = list(
               background = "#3E5AA7",
               border = "#3E5AA7"
             )) %>% 
    visEdges(color = list(color = "#cccccc")) %>% 
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
}


process_text("Die Formulierung Negative Dialektik verstoBt gegen die Uber-
lieferung. Dialektik will bereits bei Platon, da6 durchs Denkmittel
             der Negation ein Positives sich herstelle; die Figur einer Negation
             der Negation benannte das spater pragnant. Das Buch mochte
             Dialektik von derlei affirmativem Wesen befreien, ohne an Be-
             stimmtheit etwas nachzulassen. Die Entfaltung seines paradoxen
             Titels ist eine seiner Absichten.Eine kurze Skizze des Fortschritts
             der Ansichten von der Entstehung der Arten beabsichtige ich an dieser
             Stelle zu geben. Bis vor kurzem hielt die große Mehrzahl der
             Naturforscher die Arten für unveränderliche Naturerzeugnisse,
             von denen jede für sich geschaffen worden sei. Diese Ansicht
             ist von vielen Verfassern geschickt verfochten worden, während
             einige wenige Naturforscher annahmen, daß die Arten der
             Umformung unterworfen seien, und die jetzt bestehenden Lebensformen
             vermittelst wirklicher Zeugung von früher bestehenden herstammen.
             Wenn wir die Stellen übergehen, in denen die Schriftsteller des
             klassischen Altertums den Gegenstand streifen, [Fußnote] so war
             Buffon der erste, der ihn mit wissenschaftlichem Geiste behandelt
             hat. Aber da seine Ansichten zu verschiedenen Zeiten sehr wechselten,
             und da er sich nicht mit den Ursachen oder den Mitteln der
             Umformung der Arten befaßt, so brauche ich nicht auf Einzelheiten
             einzugehen.", 1)




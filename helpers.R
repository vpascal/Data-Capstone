
# Cleaning and formating input --------------------------------------------


input <- function(x) {
  
  x <- tolower(x)
  x <- gsub("[[:punct:]]|\\d","",x)
  x <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
  
  
  input_len <- sapply(gregexpr("\\S+", x), length)
  
  if (input_len == 1) {
    monogram <- unlist(strsplit(x, " "))[(input_len - 1 + 1):input_len]
    monogram <- paste(monogram, collapse = " ")
    list(monogram = monogram)
    
  } else {
    monogram <- unlist(strsplit(x, " "))[(input_len - 1 + 1):input_len]
    monogram <- paste(monogram, collapse = " ")
    bigram <- unlist(strsplit(x, " "))[(input_len - 2 + 1):input_len]
    bigram <- paste(bigram, collapse = " ")
    
    list(monogram = monogram, bigram = bigram)
    
   }
}

# Predict function --------------------------------------------------------


predictor <- function(phrase, bigram_table, trigram_table) {
  
  if (is.null(phrase$bigram))
    
  {
    temp1 <- str_detect(bigram_table$ngrams, paste0("^", paste0(phrase$monogram, " ")))
    temp1 <- bigram_table %>% filter(temp1)
    
    temp1$ngrams <-
      gsub(
        x = temp1$ngrams,
        pattern = paste0("^", paste0(phrase$monogram, " ")),
        replacement = ""
      )
    
    temp1$ngrams <- str_trim(temp1$ngrams, side = "both")
    
    if (nrow(temp1) == 0) {
      list(temp1 = "the",candidates=c("and","for","that", "with"))
    } else{
      temp1 <- temp1 %>% arrange(freq) %>% top_n(5)
      candidates <- temp1 %>% arrange(freq) %>% top_n(-4) %>% select(ngrams)
      candidates <- paste(unlist(candidates),sep = ",")
      temp1 <- temp1 %>% filter (freq==max(freq)) %>% slice(1L)
     list(temp1 = temp1,candidates=candidates)
    }
    
 
  } else {
    
    temp2 <- str_detect(trigram_table$ngrams, paste0("^", paste0(phrase$bigram, " ")))
    temp2 <- trigram_table %>% filter(temp2)
    temp2$ngrams <-
      gsub(
        x = temp2$ngrams,
        pattern = paste0("^", paste0(phrase$bigram, " ")),
        replacement = ""
      )
    
    temp2$ngrams <- str_trim(temp2$ngrams, side = "both")
    
    temp3 <- str_detect(bigram_table$ngrams, paste0("^", paste0(phrase$monogram, " ")))
    temp3 <- bigram_table %>% filter(temp3)
    temp3$ngrams <-
      gsub(
        x = temp3$ngrams,
        pattern = paste0(phrase$monogram, " "),
        replacement = ""
      )
    
    
    temp3$ngrams <- str_trim(temp3$ngrams, side = "both")
    
    if (nrow(temp2) > 0) {
      
      temp2 <- temp2 %>% arrange(freq) %>% top_n(5)
      candidates <- temp2 %>% arrange(freq) %>% top_n(-4) %>% select(ngrams)
      candidates <- paste(unlist(candidates),sep = ",")
      
      temp2 <- temp2 %>% filter(freq == max(freq)) %>% slice(1L)
      list(temp1 = temp2,candidates=candidates)
      
    } else if (nrow(temp3) == 0) {
      list(temp1 = "the",candidates=c("and","for","that", "with"))
    } else{
      
      temp3 <- temp3 %>% arrange(freq) %>% top_n(5)
      candidates <- temp3 %>% arrange(freq) %>% top_n(-4) %>% select(ngrams)
      candidates <- paste(unlist(candidates),sep = ",")
      
      temp3 <- temp3 %>% filter(freq == max(freq)) %>% slice(1L)
      list(temp1 = temp3,candidates=candidates)
    
      }
  }
  
}



# box ---------------------------------------------------------------------


box <- function(header, text, myicon) {
  HTML(paste0(
"<div class='w3-quarter'> <div class='w3-card-2'><header class='w3-container w3-blue'> <h4>",header,"</h4></header><br>", myicon,"<p>",text,"</p></div></div>"))
  
}









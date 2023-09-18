
library(shiny)
library(arrow)
library(quanteda)
library(data.table)
library(stringr)

summary_uni <- setDT(read_feather("summary_uni"))
summary_bi <- setDT(read_feather("summary_bi"))
summary_tri <- setDT(read_feather("summary_tri"))
summary_quad <- setDT(read_feather("summary_quad"))

ui <- fluidPage(
        tags$head(
                tags$script(src = "awesomplete.min.js"),
                tags$link(rel = "stylesheet", href = "awesomplete.min.css"),
                tags$script(HTML("
      var awesomplete;
      $(document).ready(function() {
        var input = document.getElementById('text_input');
        awesomplete = new Awesomplete(input, {
          minChars: 1,
          maxItems: 5,
          autoFirst: true,
          sort: false,
          filter: function(text, input) {
            var words = input.split(' ');
            var lastWord = words[words.length - 1];
            return Awesomplete.FILTER_CONTAINS(text, lastWord);
          },
          replace: function(suggestion) {
            var words = input.value.split(' ');
            words[words.length - 1] = suggestion;
            input.value = words.join(' ');
          }
        });
      });
      Shiny.addCustomMessageHandler('updateList', function(data) {
        awesomplete.list = data;
        document.getElementById('first_word').textContent = data[0];
        
        document.getElementById('other_words').innerHTML = '';
        for (var i = 1; i < data.length; i++) {
          var element = document.createElement('p');
          element.textContent = data[i];
          document.getElementById('other_words').appendChild(element);
        }
      });
    "))
        ),
        tags$style(HTML("
    .navbar {
      min-height: 15px;
      padding-top: 0px;
      padding-bottom: 0px;
    }
    .navbar-brand {
      padding-top: 0px;
      padding-bottom: 0px;
    }
    .nav-link {
      padding-top: 3px;
      padding-bottom: 3px;
    }
  ")),
        tags$div(
                tags$h1("Text completer 1.0", style = "text-align: left; margin-bottom: 10px; margin-top: 10px;font-size: 20px;")
        ),
        #theme = bs_theme(bootswatch = "zephyr"),
        navbarPage(
                title = "",
                tabPanel("Index",
                         fluidRow(
                                 style = "display: flex; justify-content: left; align-items: top; height: 100vh;",
                                 tags$textarea(
                                         id = "text_input",
                                         type = "text",
                                         placeholder = "Type and end with space to get suggestions!",
                                         style = "width: 400px; resize: none; font-size: 16px; padding: 10px; border: 1px solid #ccc; border-radius: 5px;"
                                 ),
                                 tags$div(
                                         id = "label_div",
                                         style = "margin-left: 8px;",
                                         tags$p("Predicted word:       "),
                                         tags$p(
                                                 id = "first_word",
                                                 style = "font-weight: bold; margin-bottom: 0px;"
                                         ),),
                                 tags$div(
                                         id = "label_div2",
                                         style = "margin-left: 8px;",
                                         tags$p("Other suggestions:         "),
                                         tags$p(
                                                 id = "other_words",
                                                 style = "font-weight: bold; margin-bottom: 0px;"
                                         ),)
                                 
                         )),
                tabPanel("Help",h4("How to use the app"),br(),
                         p("Start type a sentence in English and end with a blank space to trigger the algorithm."),

p("Up to three suggestions may appear."),p("You may select a suggestion by using the up and down arrow followed by Enter (alternatively you can click with your mouse or trackpad).") 
                )
        )
)

server <- function(input, output, session) {

        
        observeEvent(input$text_input, {
                
                New_sent<-0
               
                text <- input$text_input
                if (nchar(text) >= 1 && trimws(text) != "") {
                        
                        sentences <- str_split(text, pattern = "(?<=\\.\\s)", simplify = TRUE)
                        
                        
                        last_sentence <- tail(sentences, n = 1)
                        
                        cleaned_text <- paste(last_sentence, collapse = " ")
                        
                        if (substr(text, nchar(text), nchar(text)) == " ") {
                                cleaned_text<-paste0(cleaned_text," ")
                                
                        } 
                        
                        if (grepl("[\\.\\?!]+\\s*$", text)) {
                                cleaned_text <- ""
                                
                        }
                        
                        #Function that generates suggestions
                        backoff3 <- function(input_string) {
                                found_sugg <- FALSE
                                relevant <- NULL
                                
                                words <- tokens(tolower(input_string), what = "word")
                                words_orig <- tokens(input_string, what = "word")
          
                                words_lengths <- ntoken(input_string)
                                
                                new_words <- ""
                                
                                for (w in 1:words_lengths){
                                        token<-paste(tokens_select(words, startpos = w, endpos = w), collapse = " ")
                                        
                                        if(words_lengths>1){
                                        token <- summary_uni$last_word[match(token, summary_uni$last_word, nomatch="<UNK>")]
                                        }
                                        new_words <- paste0(new_words, token, sep=" ")
                                }
                                
                                last_words_input<-tokens(new_words)
                                
                                if (words_lengths > 3) {
                                        
                                        last_words <- trimws(paste(tokens_select(last_words_input, startpos = words_lengths - 2, endpos = words_lengths), collapse = " "))
                                        
                                        } else {
                                                last_words <- trimws(paste(tokens_select(tokens(last_words_input), startpos = 1 , endpos = words_lengths), collapse = " "))
                                        
                                }
                                
                                last_two_words <- paste(tokens_select(last_words_input, startpos = words_lengths-1, endpos = words_lengths), collapse = " ")
                                last_word <- paste(tokens_select(words, startpos = words_lengths, endpos = words_lengths), collapse = " ")
                                last_word_orig <- paste(tokens_select(words_orig, startpos = words_lengths, endpos = words_lengths), collapse = " ")
                                
                                length_last_words <- ntoken(last_words)
                        
                                if (length_last_words == 3) {
                                        ngram_lengths <- 4:2
                                }
                                if (length_last_words == 2) {
                                        ngram_lengths <- 3:2
                                }
                                
                                if (length_last_words == 1) {
                                        ngram_lengths <- ifelse(substr(input_string, nchar(input_string), nchar(input_string)) == " ", 2:2, 1:1)
                                }
                               
                                low_last_words<-tolower(last_words)
                                low_last_two_words<-tolower(last_two_words)
                                low_last_word<-tolower(last_word)
                                
                                if(trimws(text) != ""){
                                        if (substr(text, nchar(text), nchar(text)) == " " && trimws(cleaned_text) != "") {
                                                
                                                for (ngram_length in ngram_lengths) {
                                                        
                                                        if (found_sugg == FALSE) {
                                                                ngram_integer <-ngram_length
                                                                
                                                                if (ngram_integer == 4) {
                                                                        
                                                                        relevant <- summary_quad[rest_of_string %in% low_last_words][1:3]
                                                                        relevant <- subset(relevant, select = c(last_word, Frequency))
                                                                }
                                                                if (ngram_integer == 3) {
                                                                        
                                                                        relevant <- summary_tri[rest_of_string %in% low_last_two_words][1:3]
                                                                        relevant <- subset(relevant, select = c(last_word, Frequency))
                                                                }
                                                                if (ngram_integer == 2) {
                                                                        
                                                                        relevant <- summary_bi[rest_of_string %in% low_last_word][1:3]
                                                                        relevant <- subset(relevant, select = c(last_word, Frequency))
                                                                }
                                                                
                                                                
                                                                if (!is.na(relevant$last_word[1])) {
                                                                        found_sugg <- TRUE
                                                                        break
                                                                }
                                                        }
                                                }
                                                
                                                if (is.na(relevant$last_word[1])) {
                                                        relevant$last_word[1]<-"a"
                                                }
                                                
                                        }
                                        
                                        if (substr(text, nchar(text), nchar(text)) != " " && trimws(cleaned_text) != "") {
                                                
                                                for (ngram_length in ngram_lengths) {
                                                        if (found_sugg == FALSE) {
                                                                
                                                                ngram_integer <- as.integer(ngram_length)
                                                                if (ngram_integer == 4) {
                                                                        
                                                                        relevant <- summary_quad[startsWith(summary_quad$rest_of_string, low_last_words), ][1:3]
                                                                        relevant <- subset(relevant, select = c(last_word, Frequency))
                                                                }
                                                                if (ngram_integer == 3) {
                                                                        
                                                                        relevant <- summary_tri[startsWith(summary_tri$rest_of_string, low_last_two_words), ][1:3]
                                                                        relevant <- subset(relevant, select = c(last_word, Frequency))
                                                                }
                                                                if (ngram_integer == 2) {
                                                                        
                                                                        relevant <- summary_bi[startsWith(summary_bi$rest_of_string, low_last_word), ][1:3]
                                                                        relevant <- subset(relevant, select = c(last_word, Frequency))
                                                                        
                                                                }
                                                                
                                                                
                                                                if (found_sugg==FALSE) {
                                                                        
                                                                        relevant <- summary_uni[startsWith(summary_uni$last_word, low_last_word), ][1:3]
                                                                        relevant <- subset(relevant, select = c(last_word, Frequency))
                                                                }
                                                                
                                                                if (!is.na(relevant$last_word[1])) {
                                                                        found_sugg <- TRUE
                                                                        
                                                                        if (grepl("^[A-Z]", last_word_orig)) {
                                                                                relevant$last_word <- paste(toupper(substr(relevant$last_word, 1, 1)), substr(relevant$last_word, 2, nchar(relevant$last_word)), sep = "")
                                                                                New_sent<-1
                                                                        }
                                                                        
                                                                        break
                                                                }
                                                                
                                                                
                                                                
                                                                
                                                        }
                                                }
                                                
                                                
                                                
                                               
                                          
                                        }
                                }
                                
                                
                                return(relevant$last_word)
                                
                                
                        }
                        
                        itm_list<-list(NA)
                        
                        if(cleaned_text!=""){
                                
                                itm_list <- backoff3(cleaned_text)
                                
                                #List of words that where the first letter should be a capital letter
                                cap_words <- c("states of america","i","i'm","i'd", "mrs", "mr", "ms", "america", "france",
                                               "january", "february", "march", "april", "may", "june",
                                               "july", "august", "september", "october", "november", "december",
                                               "usa", "uea", "nato", "iea", "new york","chicago","los angeles",
                                               "barack", "obama", "trump", "reagan", "mcdonald", "garbo",
                                               "tara", "president", "romney", "elizabeth", "ghandi", "united",
                                               "jefferson","johnson", "zimmerman", "vidal", "stalin", "hitler", "eichman",
                                               "jordan", "jackson", "haag", "gates", "merill", "eu", "europe","u.s","u.s.","kennedy","walters",
                                               "africa", "asia","depp","hanks", "cruise", "kidman","brown","springer","cruz","brady","winfrey",
                                               "khan","earl", "dylan", "marley", "tyler", "spielberg","andrew","andersen", "merkel", "smith", "claus")
                                
                                for (i in 1:length(itm_list)) {
                                        if (itm_list[i] %in% cap_words) {
                                                itm_list[i] <- str_to_title(itm_list[i])
                                        }
                                }
                                
                                
                                
                                
                                
                        }
                        
                        if (grepl("^[A-Z]", cleaned_text)) {
                                if (New_sent==1) {
                                for (i in 1:length(itm_list)) {
                                                itm_list[i] <- str_to_title(itm_list[i])
                                }
                                }
                                
                        }
                        
                        itm_list <- itm_list[!is.na(itm_list)]
                        itm_list <- as.list(itm_list)
                        
                        session$sendCustomMessage(type = "updateList", itm_list)
                        
                }
        })
}

shinyApp(ui, server)

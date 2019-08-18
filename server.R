# Load the necessary library packages
library(shiny)
library(tm)
library(stringr)

# Load the necessary Rds files
load("UnigramList.RData")
load("BigramList.RData")
load("TrigramList.RData")
load("QuadgramList.RData")

# Model Functions
CleanInputString <- function(InputString)
{
  InputString <- iconv(InputString, "latin1", "ASCII", sub=" ");
  InputStringCrps <- VCorpus(VectorSource(InputString))
  InputStringCrps <- tm_map(InputStringCrps, content_transformer(tolower))
  InputStringCrps <- tm_map(InputStringCrps, removePunctuation)
  InputStringCrps <- tm_map(InputStringCrps, removeNumbers)
  InputStringCrps <- tm_map(InputStringCrps, stripWhitespace)
  CleanInputString <- as.character(InputStringCrps[[1]])
  CleanInputString <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", CleanInputString)
  
  if (nchar(CleanInputString) > 0) {
    return(CleanInputString); 
  } else {
    return("");
  }
}


PredictNextWord <- function(InputString) {
  
  InputStr <- unlist(strsplit(InputString, split=" "));
  InputStrLen <- length(InputStr);
  IsMatch <- FALSE
  
  # Function for UniGram
  if (InputStrLen > 0 & !IsMatch)  {
    NextWordMatch <- UniFreqList$word[1];
    MatchGram <- 1;
    IsMatch <- TRUE;
  }
  
  # Function for BiGram
  if (InputStrLen >= 1 & !IsMatch)  {
    StrToSearch <- InputStr[InputStrLen]; 
    StrToSearch <- paste("^",StrToSearch, sep = "");
    MatchString <- BiFreqList[grep(StrToSearch, BiFreqList$word), ];
    
    if (length(MatchString[,1]) > 0) {
      NextWordMatch <- MatchString[1,1];
      MatchGram <- 2;
      IsMatch <- TRUE;
    }
  }
    
  # Function for TriGram
  if (InputStrLen >= 2 & !IsMatch)  {
    StrToSearch <- paste(InputStr[(InputStrLen-1):InputStrLen], collapse=" "); 
    StrToSearch <- paste("^",StrToSearch, sep = "");
    MatchString <- TriFreqList[grep(StrToSearch, TriFreqList$word), ];
    
    if (length(MatchString[,1]) > 0) {
      NextWordMatch <- MatchString[1,1];
      MatchGram <- 3;
      IsMatch <- TRUE;
    }
  }
  
  # Function for QuadGram
  if (InputStrLen >=3 & !IsMatch)  {
    StrToSearch <- paste(InputStr[(InputStrLen-2):InputStrLen], collapse=" "); 
    StrToSearch <- paste("^",StrToSearch, sep = "");
    MatchString <- QuadFreqList[grep(StrToSearch, QuadFreqList$word), ];
    
    if (length(MatchString[,1]) > 0) {
      NextWordMatch <- MatchString[1,1];
      MatchGram <- 4;
      IsMatch <- TRUE;
    }
  }
  
  if (InputStrLen > 0 & IsMatch) {
    FinalResult <- data.frame(InputStr = InputString, PredictedWord = word(NextWordMatch, -1), MatchStr = NextWordMatch, GramType = MatchGram);
    return(FinalResult);
  } else {
    FinalResult <- data.frame(InputStr = "", PredictedWord = "", MatchStr = "", GramType = "");
    return(FinalResult);
  } 
  
}

# Shiny Server
shinyServer(function(input, output) {
  observeEvent(input$do, {
    CleanInputStr <- CleanInputString(input$Input);
    PredictDF <- PredictNextWord(CleanInputStr);
    output$PredictedWord <- renderText({as.character(PredictDF[1,2])});
  })
}
)

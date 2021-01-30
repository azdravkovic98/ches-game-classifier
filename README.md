# Problem

Construct a classifier that predicts the result of the chess game (as seen in the column _winner_) using the data from the accompanying file data.csv.

# Data info

data.csv is a database containing data on 18 000 games played on the site _Lichess.org_. The database contains the following columns:

```{r echo=FALSE, warning = FALSE}
library(kableExtra)
library(glue)

table <- rbind(c(1, "id", "game identification label"), c(2, "rated", "whether or not the game is rated (T/F)"), c(3, "created_at", "time of the beginning of the game"), c(4, "last_move_at", "end time of the game"), c(5, "turns", "number of total moves"), c(6, "victory_status", "the way the game ended (mate/resign/outoftime/draw)"), c(7, "winner", "who has won (black/white)"), c(8, "increment_code", "time control of the game"), c(9, "white_id", "white players's username"), c(10, " white_rating", "ranking of the white player"), c(11, "black_id", "black player's username"), c(12, "black_ranking", "ranking of the black player"), c(13, "moves", "list of all the moves played (in the standard chess notation)"), c(14, "opening_eco", "classification of the opening"), c(15, "opening_name", "the name of the opening"), c(16, "opening_ply", "number of moves in the opening move"))
knitr::kable(table, format = "latex", escape = TRUE)
table %>%
  kbl() %>%
  kable_styling()
```

## Solution

Firstly, our dataframe should be imported.
By looking at the summary of our data, we are able to see that length of the column id (17 226) is smaller than number of rows in total (18 000). That means there is some duplicated data which should be cleaned.

```{r}
data <- read.csv("data.csv")
# calling the function summary() on our data
summary(data) 
# duplicated() returns a logical vector where TRUE specifies which 
# elements of a vector or data frame are duplicates. !duplicated() means 
# that we don’t want duplicate rows
data <- data[!duplicated(data$id), ]
```

To avoid introducing a bias in test using train data, the train-test split should be performed before data preparation steps. To simulate a train and test set we are going to split randomly this data set into 80% train and 20% test.

```{r}
# Random sample indexes
train_index <- sample(1:nrow(data), 0.8 * nrow(data))
test_index <- setdiff(1:nrow(data), train_index)

# Build data_train and data_test dataframes
data_train <- data[train_index, ]
data_test <- data[test_index, ]
```

Firstly, by looking at our dataset we can conclude that when *victory_status = "mate"* or *victory_status = "outoftime"* or *victory_status = "draw"* - the prediction can be easily made just by looking at the parity of turns, as seen in the function *predict_notResign* (since the white player always plays first). That is the reason why we've divided *data_train* into two subsets - *data_obvious*, whose predictions are going to be made in function *predict_notResign*, and *data_toAnalyze*, which we are going to analyze in the further text.
The analyses presented in the function *predict_notResign* are as following:

* *victory_status = "mate"* or *victory_status = "outoftime"* 
  + If the number of total turns is even it means that the black player made the last move: winner is *black*.
  + If the number of total turns is odd it means that the white player made the last move : winner is *white*.
* *victory_status = "draw"*
  + The winner is said to be *draw*.


```{r}
data_obvious <- data_train[data_train$victory_status != "resign", ]
data_toAnalyze <- data_train[data_train$victory_status == "resign", ]

predict_notResign <- function(data_obvious){
  prediction <- c()
  # we make a prediction for each observation, i.e. for each row in the data frame
  for(i in 1:nrow(data_obvious)){
    if((data_obvious[i, ]$victory_status) == "mate" || 
       (data_obvious[i, ]$victory_status) == "outoftime"){
      # if the number of turns is even - the winner is "black"
      if(data_obvious[i, ]$turns %% 2 == 0)
        prediction <- c(prediction, "black")
      else
        prediction <- c(prediction, "white")
    }
    else{ 
      # victory_status = "draw"
      # winner is "draw"
      prediction <- c(prediction, "draw")
    }
  }
  return(prediction)
}
```

The accuracy of the function *predict_obvious* can be calculated as seen bellow. As expected, it is very high. The reason why the accuracy isn't 100% is because there are observations with *victory_status = "outoftime"*, but *winner = "draw"*. We are going to ignore this for now, since the result is satisfying.

```{r}
prediction_notResign <- predict_notResign(data_obvious)
accuracy_notResign <- sum(prediction_notResign == 
                            data_obvious$winner)/nrow(data_obvious)
accuracy_notResign

```


Before using logistic regression to tackle the problem in hand, our idea is to form a number of arrays, containing some valuable information on who the winner might be. Later, we will use these arrays as predictors in logistic regression.

The following function *info* takes data frame *data* and returns arrays:

```{r echo=FALSE, warning = FALSE}
library(kableExtra)

table <- rbind(c("white_score", "score for the white player, calculated by summing the values of all the white pieces at the end of the game"), c("black_score", "score for the black player, calculated by summing the values of all the black pieces at the end of the game"), c("check_black", "total number of checks by the black player (in the last 10 moves)"), c("check_white", "total number of checks by the white player (in the last 10 moves)"), c("king_moves_black", "total number of king moves by the black player (in the last 10 moves)"), c("king_moves_white", "total number of king moves by the white player (in the last 10 moves)"))
knitr::kable(table, format = "latex", escape = TRUE)
table %>%
  kbl() %>%
  kable_styling()
```

The score is calculated by summing all the remaining pieces values. Each piece has its own value: 

```{r echo=FALSE, warning = FALSE}
library(kableExtra)

table <- rbind(c("pawn", "1"), c("knight", "3"), c("bishop", "3.5"), c("rook", "5"), c("queen", "9"))

knitr::kable(table, format = "latex", escape = TRUE)
table %>%
  kbl() %>%
  kable_styling()
```



The look of *data[5, ]$moves*:

```{r echo=FALSE, warning = FALSE}

table <- (data[5, ]$moves)

knitr::kable(table, format = "latex", escape = TRUE)
table %>%
  kbl() %>%
  kable_styling()
```



The function *info* uses information on standard algebraic chess notation (Wikipedia explanation can be seen [here](https://en.wikipedia.org/wiki/Algebraic_notation_(chess)#:~:text=Algebraic%20notation%20(or%20AN)%20is,books%2C%20magazines%2C%20and%20newspapers.)) to parse the moves and calculate the wanted arrays.
The scores should be a fair predictor, since the bigger score indicates the player's advantage in the number of pieces left on the board. Also, number of checks made in the last 10 moves by a player should be a signal of his advantage.
On the other hand, when a player moves the king in the last 10 moves, it suggests that he is "running away".  

```{r}
info <- function(data){
  n <- nrow(data)
  
  pawn <- 1
  knight <- 3
  bishoop <- 3.5
  rook <- 5
  queen <- 9
  
  # allocating arrays
  white_score <- integer(n)
  black_score <- integer(n)
  check_black <- integer(n)
  check_white <- integer(n)
  king_moves_black <- integer(n)
  king_moves_white <- integer(n)
  
  # forming arrays for each observation, i.e. for each row in the data frame
  for(m in 1:nrow(data)){
    row <- data[m,]
    
    # we are going to analyze the moves of each game. For that we are going to 
    # make an array of strings, where each string is one move.
    # Bear in mind that odd moves are played by the white player,
    # and even moves by the black player
    tokens <- strsplit(row$moves, " ")
    tokens <- unlist(tokens)
    
    # for the games with more than 10 moves, we are going to calculate
    # numbers of chess by white/black player in the last 10 moves, as well as
    # the number of king moves by the white/black player
    if(length(tokens) > 10){
      # we want to keep the same parity format, i.e. odd moves are played
      # by the white player, and even by the black;
      # variable l will enable us just that. If the number of moves is even - we
      # will observe the last 11; if the number of moves is odd - we will observe
      # the last 10 (so the first move is always played by the white player)
      l <- ifelse(length(tokens) %% 2 == 0, 11, 10)
      tmp <- tokens[(length(tokens) - l):length(tokens)]
      
      for(i in 1:length(tmp)){
        # grepl(needle, haystack)
        # in the standard chess notation check is labeled as +
        # + is a special character, so we need to search it as "\\+", rather than "+"
        if(grepl("\\+", tmp[i])){
          # the move is odd -> the white player played it -> the check is his
          if(i %% 2 == 1)
            check_white[m] = check_white[m] + 1
          else
            check_black[m] = check_black[m] + 1
        }
        # in the standard key notation king is labeled as "K"
        if(grepl("K", tmp[i])){
          if(i %% 2 == 1)
            king_moves_white[m] = king_moves_white[m] + 1
          else
            king_moves_black[m] = king_moves_black[m] + 1
        }
      }
    }
    
    for (i in 1:length(tokens)) {
      # pawn promotion: 
      # e.g. e8=Q means that the pawn is promoted to a queen
      # when a move contains "=" we can easily parse to which figure had it been promoted,
      # since the format of these moves is always "##=#"
      # when a player promoted one of his pawns, his score should be increased for the
      # corresponding value
      if (grepl("=", tokens[i])) {
        # the last character is the character to which the pawn is promoted
        piece <- substr(tokens[i], 4, 4)
        # even move -> the black player made it -> the promotion is hiss -> 
        # -> increase his score -> continue to the next move
        if (i %% 2 == 0) {
          if (piece == "R") { # it's rook
            black_score[m] = black_score[m] + rook
            break
          }
          else if(piece == "N"){ # it's knight
            black_score[m] = black_score[m] + knight
            break
          }
          else if(piece == "B"){ # it's bishop
            black_score[m] = black_score[m] + bishoop
            break
          }
          else if(piece == "Q"){ # it's queen
            black_score[m] = black_score[m] + queen
            break
          }
        }
        else{
          # odd move -> the white player made it -> the promotion is hiss -> 
          # -> increase his score -> continue to the next move
          if (piece == "R") { # it's rook
            white_score[m] = white_score[m] + rook
            break
          }
          else if (piece == "N") { # it's knight
            white_score[m] = white_score[m] + knight
            break
          }
          else if (piece == "B") { # it's bishop
            white_score[m] = white_score[m] + bishoop
            break
          }
          else if (piece == "Q") { # it's queen
            white_score[m] = white_score[m] + queen
            break
          }
        }
      }
      # captures:
      # e.g. Bxe5 = the bishop captures the piece standing on the field e5
      # when we encounter a move with the capture label (Bxe5), we should go through all
      # of the previous moves (backwards!) until we find the first occurrence of the
      # field where the capture occurred (e5). That occurrence would look like Re5 or Rxe5 or e5. 
      # Either way, the first character signals the piece being captured (if there is no capital
      # letter as a first character, the pawn had been captured)
      if (grepl("x", tokens[i])) {
        # field where the capture occurred is always the last two characters of the move
        field <- substr(tokens[i], 3, 4)
        # searching backwards through the previous moves in order to determine the piece
        # being captured
        for (j in (i - 1):1) {
          if (grepl(field, tokens[j])) {
            # when the first occurrence (backwards) of the field had been found, the piece
            # captured will be extracted from the first character
            piece <- substr(tokens[j], 1, 1)
            # even move -> it's made by the black player -> black captured white ->
            # -> subtract the piece value from the white
            if (i %% 2 == 0) {
              if (piece == "R") { # it's rook
                white_score[m] = white_score[m] - rook
                break
              }
              else if (piece == "N") { # it's knight
                white_score[m] = white_score[m] - knight
                break
              }
              else if (piece == "B") { # it's bishop
                white_score[m] = white_score[m] - bishoop
                break
              }
              else if (piece == "Q") { # it's queen
                white_score[m] = white_score[m] - queen
                break
              }
              else{ # it's pawn
                white_score[m] = white_score[m] - pawn
                break
              }
            }
            # odd move -> it's made by the white player -> white captured black ->
            # -> subtract the piece value from the black
            else{
              if (piece == "R") { # it's rook
                black_score[m] = black_score[m] - rook
                break
              }
              else if(piece == "N"){ # it's knight
                black_score[m] = black_score[m] - knight
                break
              }
              else if(piece == "B"){ # it's bishop
                black_score[m] = black_score[m] - bishoop
                break
              }
              else if(piece == "Q"){ # it's queen
                black_score[m] = black_score[m] - queen
                break
              }
              else{ # it's pawn
                black_score[m] = black_score[m] - pawn
                break
              }
            }
          }
        }
      }
    }
  }
  return(data.frame(white_score, 
                    black_score, 
                    check_white, 
                    check_black, 
                    king_moves_white, 
                    king_moves_black))
}
```

Let's calculate the info for our dataframes *data_toAnalyze* and *data_test*.

```{r}
# calculating info for data_toAnalyze
info_res <- info(data_toAnalyze)
white_score <- info_res$white_score
black_score <- info_res$black_score
check_white <- info_res$check_white
check_black <- info_res$check_black
king_moves_white <- info_res$king_moves_white
king_moves_black <- info_res$king_moves_black

# calculating info for data_test
info_res_test <- info(data_test)
white_score_test <- info_res_test$white_score
black_score_test <- info_res_test$black_score
check_white_test <- info_res_test$check_white
check_black_test <- info_res_test$check_black
king_moves_white_test <- info_res_test$king_moves_white
king_moves_black_test <- info_res_test$king_moves_black
```

Now we are able to make our logistic regression model.

Our predictors are going to be difference of rankings, difference of scores (calculated in the function *info*), parity of the turns played and difference of the checks played by the white and the black player, respectively. 
Also, the last predictor will be difference of the king moves made by black and king moves made by white. Notice that here we are subtracting white from black, while in the previous predictors it was done vice verse. The reason is because if the difference of scores (white - black) is positive it suggests that the white player is the winner. On the other hand, if the difference of the king moves (white - black) is positive it suggest that the white player "ran away" more than the black did, so the black player would be the winner.

Since we have all of the aforementioned predictors, we can model our problem on *data_toAnalyze*, and then test it on *data_test*. 

```{r}
# column winner has to be transformed for logistic regression
winner <- as.factor(data_toAnalyze$winner)

# predictors are being calculated on the data_toAnalyze
a <- data_toAnalyze$white_rating - data_toAnalyze$black_rating
b <- white_score - black_score
c <- data_toAnalyze$turns %% 2
d <- check_white - check_black
e <- king_moves_black - king_moves_white
model <- glm(winner ~ a + b + c + d + e, family = binomial)
# we can see that all of our predictors are significant
summary(model)

# predictions are being tested on data_test
newdata <- data.frame(a = data_test$white_rating - data_test$black_rating,
                      b = white_score_test - black_score_test, 
                      c = data_test$turns%%2, 
                      d = check_white_test - check_black_test,
                      e = king_moves_black_test - king_moves_white_test)

glm.probs <- predict(model, newdata = newdata, type = "response")
glm.predict <- ifelse(glm.probs > 0.5, "white", "black")

# comparing our predicted values with the actual ones
accuracy.glm <- mean(glm.predict == data_test$winner)

# total accuracy can be calculated by the formula
# accuracy <- (accuracy_on_set1 * size(set1) + 
#              accuracy_on_set2 * size(set2))/size(disjointUnion(set1, set2))
# have in mind that data_train was disjointly partitioned on data_obvious and 
# data_toAnalyze
accuracy <- (accuracy_notResign * nrow(data_obvious) + 
               accuracy.glm * nrow(data_toAnalyze))/nrow(data_train)

glue("total accuracy in 1 iteration: {accuracy}")

```

The last accuracy can vary depending on the partitioning done in the beginning on train-test data. Having that in mind, the idea is to repeat the same process of predicting values for 10 times and in the end define accuracy as the mean of all accuracy values.
As the function *info* is time consuming, it takes a couple of minutes for the following code to execute!

```{r}
# accuracy_total is the vector containing accuracy values for all 10 iterations
accuracy_total <- c()

for(i in 1:10){
 # Random sample indexes
 train_index <- sample(1:nrow(data), 0.8 * nrow(data))
 test_index <- setdiff(1:nrow(data), train_index)
 
 # Build data_train and data_test data frames
 data_train <- data[train_index, ]
 data_test <- data[test_index, ]
 
 # partition to data that can be obviously predicted, and data that needs to be predicted
 # via logistic regression
 data_obvious <- data_train[data_train$victory_status != "resign", ]
 data_toAnalyze <- data_train[data_train$victory_status == "resign", ]
 
 # make obvious predictions and calculate its accuracy
 prediction_notResign <- predict_notResign(data_obvious)
 accuracy_notResign <- sum(prediction_notResign ==
                           data_obvious$winner)/nrow(data_obvious)
 
 # calculating info for data_toAnalyze
 info_res <- info(data_toAnalyze)
 white_score <- info_res$white_score
 black_score <- info_res$black_score
 check_white <- info_res$check_white
 check_black <- info_res$check_black
 king_moves_white <- info_res$king_moves_white
 king_moves_black <- info_res$king_moves_black
 # calculating info for data_test
 info_res_test <- info(data_test)
 white_score_test <- info_res_test$white_score
 black_score_test <- info_res_test$black_score
 check_white_test <- info_res_test$check_white
 check_black_test <- info_res_test$check_black
 king_moves_white_test <- info_res_test$king_moves_white
 king_moves_black_test <- info_res_test$king_moves_black
 
 # column winner has to be transformed for logistic regression
 winner <- as.factor(data_toAnalyze$winner)
 
 # predictors are beeing calculated on the data_toAnalyze
 a <- data_toAnalyze$white_rating - data_toAnalyze$black_rating
 b <- white_score - black_score
 c <- data_toAnalyze$turns %% 2
 d <- check_white - check_black
 e <- king_moves_black - king_moves_white

 # make model
 model <- glm(winner ~ a + b + c + d + e, family = binomial)
 
 # we can see that all of our predictors are significant
 # predictions are being tested on data_test
 newdata <- data.frame(a = data_test$white_rating - data_test$black_rating,
                       b = white_score_test - black_score_test,
                       c = data_test$turns%%2,
                       d = check_white_test - check_black_test,
                       e = king_moves_black_test - king_moves_white_test)
 
 # predicting the values in newdata
 glm.probs <- predict(model, newdata = newdata, type = "response")
 glm.predict <- ifelse(glm.probs > 0.5, "white", "black")
 
 # comparing our predicted values with the actual ones
 accuracy.glm <- mean(glm.predict == data_test$winner)
 
 # accuracy of each iteration can be calculated by the formula
 # accuracy <- (accuracy_on_set1 * size(set1) +
 #              accuracy_on_set2 * size(set2))/size(disjointUnion(set1, set2))
 # have in mind that data_train was disjointedly partitioned on data_obvious and
 # data_toAnalyze
 # accuracy_total is vector containing accuracy values from each iteration
 accuracy_total <- c(accuracy_total, (accuracy_notResign * nrow(data_obvious) +
                                     accuracy.glm * nrow(data_toAnalyze))/
                                     nrow(data_train))
}

# print the final accuracy
glue("total accuracy in 10 iterations: {mean(accuracy_total)}")

```




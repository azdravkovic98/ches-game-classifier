---
title: "Chess Game Classifier"
author: "by Aleksandra Zdravkovic"
date: "november 2020"
output:
  prettydoc::html_pretty:
    theme: cayman
    highlight: github
    keep_md: true
---
<style>
body {
text-align: justify}
</style>


# Problem {#id1}

Construct a classifier that predicts the result of the chess game (as seen in the column _winner_) using the data from the accompanying file data.csv.

# Data info {#id2}

data.csv is a database containing data on 18 000 games played on the site _Lichess.org_. The database contains the following columns:


\begin{tabular}{l|l|l}
\hline
1 & id & game identification label\\
\hline
2 & rated & whether or not the game is rated (T/F)\\
\hline
3 & created\_at & time of the beginning of the game\\
\hline
4 & last\_move\_at & end time of the game\\
\hline
5 & turns & number of total moves\\
\hline
6 & victory\_status & the way the game ended (mate/resign/outoftime/draw)\\
\hline
7 & winner & who has won (black/white)\\
\hline
8 & increment\_code & time control of the game\\
\hline
9 & white\_id & white players's username\\
\hline
10 & white\_rating & ranking of the white player\\
\hline
11 & black\_id & black player's username\\
\hline
12 & black\_ranking & ranking of the black player\\
\hline
13 & moves & list of all the moves played (in the standard chess notation)\\
\hline
14 & opening\_eco & classification of the opening\\
\hline
15 & opening\_name & the name of the opening\\
\hline
16 & opening\_ply & number of moves in the opening move\\
\hline
\end{tabular}

<table class="table" style="margin-left: auto; margin-right: auto;">
<tbody>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> id </td>
   <td style="text-align:left;"> game identification label </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> rated </td>
   <td style="text-align:left;"> whether or not the game is rated (T/F) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> created_at </td>
   <td style="text-align:left;"> time of the beginning of the game </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> last_move_at </td>
   <td style="text-align:left;"> end time of the game </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> turns </td>
   <td style="text-align:left;"> number of total moves </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> victory_status </td>
   <td style="text-align:left;"> the way the game ended (mate/resign/outoftime/draw) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> winner </td>
   <td style="text-align:left;"> who has won (black/white) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> increment_code </td>
   <td style="text-align:left;"> time control of the game </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> white_id </td>
   <td style="text-align:left;"> white players's username </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> white_rating </td>
   <td style="text-align:left;"> ranking of the white player </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> black_id </td>
   <td style="text-align:left;"> black player's username </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> black_ranking </td>
   <td style="text-align:left;"> ranking of the black player </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> moves </td>
   <td style="text-align:left;"> list of all the moves played (in the standard chess notation) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> opening_eco </td>
   <td style="text-align:left;"> classification of the opening </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> opening_name </td>
   <td style="text-align:left;"> the name of the opening </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> opening_ply </td>
   <td style="text-align:left;"> number of moves in the opening move </td>
  </tr>
</tbody>
</table>

## Solution

Firstly, our dataframe should be imported.
By looking at the summary of our data, we are able to see that length of the column id (17 226) is smaller than number of rows in total (18 000). That means there is some duplicated data which should be cleaned.


```r
data <- read.csv("data.csv")
# calling the function summary() on our data
summary(data) 
```

```
##        X              id              rated           created_at       
##  Min.   :    1   Length:18000       Mode :logical   Min.   :1.377e+12  
##  1st Qu.: 4501   Class :character   FALSE:3465      1st Qu.:1.478e+12  
##  Median : 9000   Mode  :character   TRUE :14535     Median :1.496e+12  
##  Mean   : 9000                                      Mean   :1.484e+12  
##  3rd Qu.:13500                                      3rd Qu.:1.503e+12  
##  Max.   :18000                                      Max.   :1.504e+12  
##   last_move_at           turns        victory_status        winner         
##  Min.   :1.377e+12   Min.   :  1.00   Length:18000       Length:18000      
##  1st Qu.:1.478e+12   1st Qu.: 37.00   Class :character   Class :character  
##  Median :1.496e+12   Median : 55.00   Mode  :character   Mode  :character  
##  Mean   :1.484e+12   Mean   : 60.54                                        
##  3rd Qu.:1.503e+12   3rd Qu.: 79.00                                        
##  Max.   :1.504e+12   Max.   :349.00                                        
##  increment_code       white_id          white_rating    black_id        
##  Length:18000       Length:18000       Min.   : 784   Length:18000      
##  Class :character   Class :character   1st Qu.:1398   Class :character  
##  Mode  :character   Mode  :character   Median :1567   Mode  :character  
##                                        Mean   :1597                     
##                                        3rd Qu.:1793                     
##                                        Max.   :2700                     
##   black_rating     moves           opening_eco        opening_name      
##  Min.   : 789   Length:18000       Length:18000       Length:18000      
##  1st Qu.:1391   Class :character   Class :character   Class :character  
##  Median :1562   Mode  :character   Mode  :character   Mode  :character  
##  Mean   :1589                                                           
##  3rd Qu.:1785                                                           
##  Max.   :2723                                                           
##   opening_ply    
##  Min.   : 1.000  
##  1st Qu.: 3.000  
##  Median : 4.000  
##  Mean   : 4.822  
##  3rd Qu.: 6.000  
##  Max.   :28.000
```

```r
# duplicated() returns a logical vector where TRUE specifies which 
# elements of a vector or data frame are duplicates. !duplicated() means 
# that we don’t want duplicate rows
data <- data[!duplicated(data$id), ]
```

To avoid introducing a bias in test using train data, the train-test split should be performed before data preparation steps. To simulate a train and test set we are going to split randomly this data set into 80% train and 20% test.


```r
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



```r
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


```r
prediction_notResign <- predict_notResign(data_obvious)
accuracy_notResign <- sum(prediction_notResign == 
                            data_obvious$winner)/nrow(data_obvious)
accuracy_notResign
```

```
## [1] 0.9940427
```


Before using logistic regression to tackle the problem in hand, our idea is to form a number of arrays, containing some valuable information on who the winner might be. Later, we will use these arrays as predictors in logistic regression.

The following function *info* takes data frame *data* and returns arrays:


\begin{tabular}{l|l}
\hline
white\_score & score for the white player, calculated by summing the values of all the white pieces at the end of the game\\
\hline
black\_score & score for the black player, calculated by summing the values of all the black pieces at the end of the game\\
\hline
check\_black & total number of checks by the black player (in the last 10 moves)\\
\hline
check\_white & total number of checks by the white player (in the last 10 moves)\\
\hline
king\_moves\_black & total number of king moves by the black player (in the last 10 moves)\\
\hline
king\_moves\_white & total number of king moves by the white player (in the last 10 moves)\\
\hline
\end{tabular}

<table class="table" style="margin-left: auto; margin-right: auto;">
<tbody>
  <tr>
   <td style="text-align:left;"> white_score </td>
   <td style="text-align:left;"> score for the white player, calculated by summing the values of all the white pieces at the end of the game </td>
  </tr>
  <tr>
   <td style="text-align:left;"> black_score </td>
   <td style="text-align:left;"> score for the black player, calculated by summing the values of all the black pieces at the end of the game </td>
  </tr>
  <tr>
   <td style="text-align:left;"> check_black </td>
   <td style="text-align:left;"> total number of checks by the black player (in the last 10 moves) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> check_white </td>
   <td style="text-align:left;"> total number of checks by the white player (in the last 10 moves) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> king_moves_black </td>
   <td style="text-align:left;"> total number of king moves by the black player (in the last 10 moves) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> king_moves_white </td>
   <td style="text-align:left;"> total number of king moves by the white player (in the last 10 moves) </td>
  </tr>
</tbody>
</table>

The score is calculated by summing all the remaining pieces values. Each piece has its own value: 


\begin{tabular}{l|l}
\hline
pawn & 1\\
\hline
knight & 3\\
\hline
bishop & 3.5\\
\hline
rook & 5\\
\hline
queen & 9\\
\hline
\end{tabular}

<table class="table" style="margin-left: auto; margin-right: auto;">
<tbody>
  <tr>
   <td style="text-align:left;"> pawn </td>
   <td style="text-align:left;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> knight </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> bishop </td>
   <td style="text-align:left;"> 3.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rook </td>
   <td style="text-align:left;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> queen </td>
   <td style="text-align:left;"> 9 </td>
  </tr>
</tbody>
</table>



The look of *data[5, ]$moves*:


\begin{tabular}{l}
\hline
x\\
\hline
e4 e5 Nf3 d6 d4 Nc6 d5 Nb4 a3 Na6 Nc3 Be7 b4 Nf6 Bg5 O-O b5 Nc5 Bxf6 Bxf6 Bd3 Qd7 O-O Nxd3 Qxd3 c6 a4 cxd5 Nxd5 Qe6 Nc7 Qg4 Nxa8 Bd7 Nc7 Rc8 Nd5 Qg6 Nxf6+ Qxf6 Rfd1 Re8 Qxd6 Bg4 Qxf6 gxf6 Rd3 Bxf3 Rxf3 Rd8 Rxf6 Kg7 Rf3 Rd2 Rg3+ Kf8 c3 Re2 f3 Rc2 Rg5 f6 Rh5 Kg7 Rd1 Kg6 Rh3 Rxc3 Rd7 Rc1+ Kf2 Rc2+ Kg3 h5 Rxb7 Kg5 Rxa7 h4+ Rxh4 Rxg2+ Kxg2 Kxh4 b6 Kg5 b7 f5 exf5 Kxf5 b8=Q e4 Rf7+ Kg5 Qg8+ Kh6 Rh7\#\\
\hline
\end{tabular}

<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> x </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> e4 e5 Nf3 d6 d4 Nc6 d5 Nb4 a3 Na6 Nc3 Be7 b4 Nf6 Bg5 O-O b5 Nc5 Bxf6 Bxf6 Bd3 Qd7 O-O Nxd3 Qxd3 c6 a4 cxd5 Nxd5 Qe6 Nc7 Qg4 Nxa8 Bd7 Nc7 Rc8 Nd5 Qg6 Nxf6+ Qxf6 Rfd1 Re8 Qxd6 Bg4 Qxf6 gxf6 Rd3 Bxf3 Rxf3 Rd8 Rxf6 Kg7 Rf3 Rd2 Rg3+ Kf8 c3 Re2 f3 Rc2 Rg5 f6 Rh5 Kg7 Rd1 Kg6 Rh3 Rxc3 Rd7 Rc1+ Kf2 Rc2+ Kg3 h5 Rxb7 Kg5 Rxa7 h4+ Rxh4 Rxg2+ Kxg2 Kxh4 b6 Kg5 b7 f5 exf5 Kxf5 b8=Q e4 Rf7+ Kg5 Qg8+ Kh6 Rh7# </td>
  </tr>
</tbody>
</table>



The function *info* uses information on standard algebraic chess notation (Wikipedia explanation can be seen [here](https://en.wikipedia.org/wiki/Algebraic_notation_(chess)#:~:text=Algebraic%20notation%20(or%20AN)%20is,books%2C%20magazines%2C%20and%20newspapers.)) to parse the moves and calculate the wanted arrays.
The scores should be a fair predictor, since the bigger score indicates the player's advantage in the number of pieces left on the board. Also, number of checks made in the last 10 moves by a player should be a signal of his advantage.
On the other hand, when a player moves the king in the last 10 moves, it suggests that he is "running away".  


```r
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


```r
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


```r
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
```

```
## 
## Call:
## glm(formula = winner ~ a + b + c + d + e, family = binomial)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.7814  -0.3443   0.0397   0.3550   4.1617  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.1681950  0.0549274 -21.268  < 2e-16 ***
## a            0.0033698  0.0001867  18.050  < 2e-16 ***
## b            0.2868759  0.0096793  29.638  < 2e-16 ***
## c            2.6513745  0.0811470  32.674  < 2e-16 ***
## d            0.3428824  0.0407644   8.411  < 2e-16 ***
## e            0.3012911  0.0389774   7.730 1.08e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 10710  on 7736  degrees of freedom
## Residual deviance:  4234  on 7731  degrees of freedom
## AIC: 4246
## 
## Number of Fisher Scoring iterations: 6
```

```r
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

```
## total accuracy in 1 iteration: 0.920646242630441
```

The last accuracy can vary depending on the partitioning done in the beginning on train-test data. Having that in mind, the idea is to repeat the same process of predicting values for 10 times and in the end define accuracy as the mean of all accuracy values.
As the function *info* is time consuming, it takes a couple of minutes for the following code to execute!


```r
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

```
## total accuracy in 10 iterations: 0.920781613397498
```




#Ian Gallmeister
#Statistical Computing + Machine Learning
#18 February 2016

#The Deck -------------------------------

#2:14 --> 2, 3, ..., J, Q, K, A --- Multiplied by 10
#1:4 --> Club, Diamond, Heart, Spade --- Added
poker_deck <- c(outer((10 * 2:14), 1:4, "+"))

#Generating Hands -----------------------

hand <- sample(poker_deck, 5)

royal_flush <- c(outer((10 * 10:14), sample.int(4, size = 1), "+"))

#Four of a Kind
generate_four_of_a_kind <- function(){
  the_four <- c(outer(10*sample(2:14), 1:4, "+"))
  the_one <- sample(setdiff(the_four, c(outer((10 * 2:14), 1:4, "+"))), size = 1)
  the_hand <- c(the_four, the_one)
  the_hand
}

#Straight Flush
generate_straight_flush <- function(){
  start <- sample(2:14, 1)
  straight_flush <- c(outer((10*start:(start+4)), sample.int(4, size = 1), "+"))
  straight_flush
}  

#Flush
generate_flush <- function(){
  suit <- sample.int(4, size = 1)
  values <- sample(2:14, 5)
  flush <- c(outer(10*values, suit, "+"))
}

#Full House
generate_full_house <- function(){
  values <- sample(2:14, 2)
  threesuits <- sample.int(4, size = 3) #choose the three suits
  pairsuits <- sample.int(4, size = 2) #choose the two suits
  three_of_a_kind <- c(outer((10*values[1]), threesuits, "+"))
  pair <- c(outer((10*values[2]), pairsuits, "+"))
  hand <- c(three_of_a_kind, pair)
  hand
}

#Straight
generate_straight <- function(){
  suits <- sample.int(4, size = 5, replace = TRUE)
  if (max(table(suits)) == 5) {suits <- sample.int(4, size = 5, replace = TRUE)}
  start <- sample.int(9, size=1) + 1
  hand <- c(outer((10*start:(start+4)), suits, "+"))
  hand
}

#Three of a Kind
generate_three_of_a_kind <- function(){
  value <- sample.int(13, size = 1) + 1
  suits <- sample.int(4, size = 3)
  thethree <- c(outher((10*value), suits, "+"))
  theothers <- sample(setdiff(c(outer((10 * 2:14), 1:4, "+")), thethree), size = 2)
  hand <- c(thethree, theothers)
}

#Calculating Suits and Ranks ----------

#Return suits of a hand
suits <- function(hand){
  hand %% 10
}

ranks <- function(hand){
  hand %/% 10
}

#What Kind of Hand? -------------------

#Royal Flush
is_royal_flush <- function(hand){
  shapes <- suits(hand)
  numbers <- ranks(hand)
  min(shapes) == max(shapes) && min(numbers) == 10 && max(numbers) == 14
}

#Straight Flush
is_straight_flush <- function(hand){
  shapes <- suits(hand)
  numbers <- ranks(hand)
  min(shapes) == max(shapes) && min(numbers) + 4 == max(numbers)
}


##Four of a Kind
is_four_of_a_kind <- function(hand){
  shapes <- suits(hand)
  numbers <- ranks(hand)
  min(shapes) == 1 && max(shapes) == 4 && max(table(numbers)) == 4
}

#Flush
is_flush <- function(hand){
  shapes <- suits(hand)
  numbers <- ranks(hand)
  table(shapes) == 5 && min(numbers) + 4 != max(numbers)
}

#Full House
is_full_house <- function(hand){
  numberstable <- table(ranks(hand))
  min(numberstable) == 2 && max(numberstable) == 3
}

#Straight
is_straight <- function(hand){
  numbers <- ranks(hand)
  shapes <- suits(hand)
  max(table(shapes)) != 5 && min(numbers) + 4 == max(numbers) && min(numbers) != 10
} #Write function to generate straight hands


#Three of a Kind
#is_three_of_a_kind needs to preclude four of a kind
is_three_of_a_kind <- function(hand){
  numbers <- table(ranks(hand))
  max(numbers) == 3 && min(numbers) != 2
}

#Two Pair
is_two_pair <- function(hand){
  numbers <- ranks(hand)
  length(table(numbers)) == 3 && max(numbers) < 3
}

#Pair
#is_pair needs to preclude three of a kind and four of a kind
is_pair <- function(hand){
  numbers <- ranks(hand)
  max(table(numbers)) == 2 && (table(table(numbers)) == c(3,1) || table(table(numbers)) == c(1,3))
}

#High Card
high_card <- function(hand){
  counts <- table(ranks(hand))
  length(counts) == 5
}

#Estimating Probabilities ---------------
#Before draw draws five random cards and calculates best hand from those cards

before_draw <- function(...){
  hand <- sample(poker_deck, 5)
  if (is_royal_flush(hand)) {"Royal Flush"} 
  else if (is_straight_flush(hand)) {"Straight Flush"} 
  else if (is_four_of_a_kind(hand)) {"Four of a Kind"} 
  else if (is_full_house(hand)) {"Full House"} 
  else if (is_flush(hand)) {"Flush"} 
  else if (is_straight(hand)) {"Straght"} 
  else if (is_three_of_a_kind(hand)) {"Three of a Kind"} 
  else if (is_two_pair(hand)) {"Two Pair"} 
  else if (is_pair(hand)) {"Pair"} 
  else {"High Card"}
}

#Tabulating Probabilities ---------------

results <- table(sapply(1:10000, FUN = before_draw))

#Texas Hold'em and Seven Card Stud ------
#Use what you have to write a function that chooses the best 5-card hand from 7 cards dealt

# best_hold_em <- function(...){
#   hand <- sample(poker_deck, 7)
#   if (is_royal_flush(hand)) {"Royal Flush"} else {
#     if (is_straight_flush(hand)) {"Straight Flush"} else {
#       if (is_four_of_a_kind(hand)) {"Four of a Kind"} else {
#         if (is_full_house(hand)) {"Full House"} else {
#           if (is_flush(hand)) {"Flush"} else {
#             if (is_straight(hand)) {"Straght"} else {
#               if (is_three_of_a_kind(hand)) {"Three of a Kind"} else {
#                 if (is_two_pair(hand)) {"Two Pair"} else {
#                   if (is_pair(hand)) {"Pair"} else {"Well, that's not great"}
#                 }}}}}}}}
# }

#Scoring --------------------------------

scoreActivity::score253(day = 9)
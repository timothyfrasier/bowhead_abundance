#####################################
# Estimate Proportions With         #
# Different Sighting Patterns       #
# (For table for estimating size    #
# of 5th location)                  #
#                                   #
# REQUIRES: posterior probabilities #
# for all four population sizes     #
# and all four migrations rates.    #
#####################################

totals <- function(pop1post, pop2post, pop3post, pop5post, m12post, m13post, m23post, m5post) {

    #---------------------------------#
    # Just in location 1              #
    #---------------------------------#
    cell1 <- pop1post - (pop1post * m12post) - (pop1post * m13post) - (pop1post * m5post) # Individuals from location 1 that don't move anywhere else

    #---------------------------------#
    # Just in location 2              #
    #---------------------------------#
    cell2 <- pop2post - (pop2post * m12post) - (pop2post * m23post) - (pop2post * m5post) # Individuals from location 2 that don't move anywhere else

    #---------------------------------#
    # Just in location 3              #
    #---------------------------------#
    cell3 <- pop3post - (pop3post * m13post) - (pop3post * m23post) - (pop3post * m5post) # Individuals from location 3 that don't move anywhere else

    #---------------------------------#
    # Just in location 5              #
    #---------------------------------#
    cell5 <- pop5post - (pop5post * m5post) - (pop5post * m5post) -  (pop5post * m5post) # Individuals from location 5 that don't move anywhere else

    #---------------------------------#
    # Population 1 & 2                #
    #---------------------------------#
    cell12y <- pop1post * m12post  # Pop1 individuals that go to 2
    cell12a <- cell12y - (cell12y * m23post) - (cell12y * m5post)  # Pop1 individuals that go to 2 but not 3 or 5
    cell12z <- pop2post * m12post  # Pop2 individuals that go to 1
    cell12b <- cell12z - (cell12z * m13post) - (cell12z * m5post)  # Pop2 individuals that go to 1 but not 3 or 5
    cell12 <- cell12a + cell12b

    #---------------------------------#
    # Population 1 & 3                #
    #---------------------------------#
    cell13y <- pop1post * m13post  # Pop1 individuals that go to 3
    cell13a <- cell13y - (cell13y * m23post) - (cell13y * m5post)  # Pop1 individuals that go to 3 but not 2 or 5
    cell13z <- pop3post * m13post  # Pop3 individuals that go to 1
    cell13b <- cell13z - (cell13z * m12post) - (cell13z * m5post)  # Pop3 individuals that go to 1 but not 2 or 5
    cell13 <- cell13a + cell13b

    #---------------------------------#
    # Population 1 & 5                #
    #---------------------------------#
    cell15y <- pop1post * m5post  # Pop1 individuals that go to 5
    cell15a <- cell15y - (cell15y * m5post) - (cell15y * m5post)  # Pop1 individuals that go to 5 but not 2 or 3
    cell15z <- pop5post * m5post  # Pop5 individuals that go to 1
    cell15b <- cell15z - (cell15z * m12post) - (cell15z * m13post)  # Pop4 individuals that go to 1 but not 2 or 3
    cell15 <- cell15a + cell15b
    
    
    #---------------------------------#
    # Population 2 & 3                #
    #---------------------------------#
    cell23y <- pop2post * m23post  # Pop2 individuals that go to 3
    cell23a <- cell23y - (cell23y * m13post) - (cell23y * m5post)  # Pop2 individuals that go to 3 but not 1 or 5
    cell23z <- pop3post * m23post  # Pop3 individuals that go to 2
    cell23b <- cell23z - (cell23z * m12post) - (cell23z * m5post)  # Pop3 individuals that go to 2 but not 1 or 5
    cell23 <- cell23a + cell23b
    
    #---------------------------------#
    # Population 2 & 5                #
    #---------------------------------#
    cell25y <- pop2post * m5post  # Pop2 individuals that go to 5
    cell25a <- cell25y - (cell25y * m5post) - (cell25y * m5post) # Pop2 individuals that go to 5 but not 1 or 3
    cell25z <- pop5post * m5post  # Pop5 individuals that go to 2
    cell25b <- cell25z - (cell25z * m12post) - (cell25z * m23post)  # Pop5 individuals that go to 2 but not 1 or 3
    cell25 <- cell25a + cell25b

    #---------------------------------#
    # Population 3 & 5                #
    #---------------------------------#
    cell35y <- pop3post * m5post  # Pop3 individuals that go to 5
    cell35a <- cell35y - (cell35y * m5post) - (cell35y * m5post) # Pop3 individuals that go to 5 but not 1 or 2
    cell35z <- pop5post * m5post  # Pop5 individuals that go to 3
    cell35b <- cell35z - (cell35z * m13post) - (cell35z * m23post)  # Pop5 individuals that go to 3 but not 1 or 2
    cell35 <- cell35a + cell35b
    
    #---------------------------------#
    # Populations 1, 2, 3             #
    #---------------------------------#
    
    #---Scenario1 1->2->3 ---#
    cell123a <- pop1post * m12post * m23post # Individuals that move from 1 to 2 to 3
    cell123b <- cell123a - (cell123a * m5post) # Individuals that move from 1 to 2 to 3 but not to 5
    
    #---Scenario1 1->3->2 ---#
    cell123c <- pop1post * m13post * m23post # Individuals that move from 1 to 3 to 2
    cell123d <- cell123c - (cell123c * m5post) # Individuals that move from 1 to 3 to 2 but not to 5

    #---Scenario1 2->1->3 ---#
    cell123e <- pop2post * m12post * m13post # Individuals that move from 2 to 1 to 3
    cell123f <- cell123e - (cell123e * m5post) # Individuals that move from 2 to 1 to 3 but not to 5
    
    #---Scenario1 2->3->1 ---#
    cell123g <- pop2post * m23post * m13post # Individuals that move from 2 to 3 to 1
    cell123h <- cell123g - (cell123g * m5post) # Individuals that move from 2 to 3 to 1 but not to 5

    #---Scenario1 3->1->2 ---#
    cell123i <- pop3post * m13post * m12post # Individuals that move from 3 to 1 to 2
    cell123j <- cell123i - (cell123i * m5post) # Individuals that move from 3 to 1 to 2 but not to 5
    
    #---Scenario1 3->2->1 ---#
    cell123k <- pop3post * m23post * m12post # Individuals that move from 3 to 2 to 1
    cell123l <- cell123k - (cell123k * m5post) # Individuals that move from 3 to 2 to 1 but not to 5
    
    cell123 <- cell123b + cell123d + cell123f + cell123h + cell123j + cell123l

    
    
    #---------------------------------#
    # Populations 1, 2, 5             #
    #---------------------------------#
    
    #---Scenario1 1->2->5 ---#
    cell125a <- pop1post * m12post * m5post # Individuals that move from 1 to 2 to 5
    cell125b <- cell125a - (cell125a * m5post) # Individuals that move from 1 to 2 to 5 but not to 3
    
    #---Scenario1 1->5->2 ---#
    cell125c <- pop1post * m5post * m5post # Individuals that move from 1 to 5 to 2
    cell125d <- cell125c - (cell125c * m23post) # Individuals that move from 1 to 5 to 2 but not to 3
    
    #---Scenario1 2->1->5 ---#
    cell125e <- pop2post * m12post * m5post # Individuals that move from 2 to 1 to 5
    cell125f <- cell125e - (cell125e * m5post) # Individuals that move from 2 to 1 to 5 but not to 3
    
    #---Scenario1 2->5->1 ---#
    cell125g <- pop2post * m5post * m5post # Individuals that move from 2 to 5 to 1
    cell125h <- cell125g - (cell125g * m13post) # Individuals that move from 2 to 5 to 1 but not to 3
    
    #---Scenario1 5->1->2 ---#
    cell125i <- pop5post * m5post * m12post # Individuals that move from 5 to 1 to 2
    cell125j <- cell125i - (cell125i * m23post) # Individuals that move from 5 to 1 to 2 but not to 3
    
    #---Scenario1 5->2->1 ---#
    cell125k <- pop5post * m5post * m12post # Individuals that move from 5 to 2 to 1
    cell125l <- cell125k - (cell125k * m13post) # Individuals that move from 5 to 2 to 1 but not to 3
    
    cell125 <- cell125b + cell125d + cell125f + cell125h + cell125j + cell125l


    
    #---------------------------------#
    # Populations 1, 3, 5             #
    #---------------------------------#
    
    #---Scenario1 1->3->5 ---#
    cell135a <- pop1post * m13post * m5post # Individuals that move from 1 to 3 to 5
    cell135b <- cell135a - (cell135a * m5post) # Individuals that move from 1 to 3 to 5 but not to 2
    
    #---Scenario1 1->5->3 ---#
    cell135c <- pop1post * m5post * m5post # Individuals that move from 1 to 5 to 3
    cell135d <- cell135c - (cell135c * m23post) # Individuals that move from 1 to 5 to 3 but not to 2
    
    #---Scenario1 3->1->5 ---#
    cell135e <- pop3post * m13post * m5post # Individuals that move from 3 to 1 to 5
    cell135f <- cell135e - (cell135e * m5post) # Individuals that move from 3 to 1 to 5 but not to 2
    
    #---Scenario1 3->5->1 ---#
    cell135g <- pop3post * m5post * m5post # Individuals that move from 3 to 5 to 1
    cell135h <- cell135g - (cell135g * m12post) # Individuals that move from 3 to 5 to 1 but not to 2
    
    #---Scenario1 5->1->3 ---#
    cell135i <- pop5post * m5post * m13post # Individuals that move from 5 to 1 to 3
    cell135j <- cell135i - (cell135i * m23post) # Individuals that move from 5 to 1 to 3 but not to 2
    
    #---Scenario1 5->3->1 ---#
    cell135k <- pop5post * m5post * m13post # Individuals that move from 5 to 3 to 1
    cell135l <- cell135k - (cell135k * m12post) # Individuals that move from 5 to 3 to 1 but not to 2
    
    cell135 <- cell135b + cell135d + cell135f + cell135h + cell135j + cell135l
    
    
    #---------------------------------#
    # Populations 2, 3, 5             #
    #---------------------------------#
    
    #---Scenario1 2->3->5 ---#
    cell235a <- pop2post * m23post * m5post # Individuals that move from 2 to 3 to 5
    cell235b <- cell235a - (cell235a * m5post) # Individuals that move from 2 to 3 to 5 but not to 1
    
    #---Scenario1 2->5->3 ---#
    cell235c <- pop2post * m5post * m5post # Individuals that move from 2 to 5 to 3
    cell235d <- cell235c - (cell235c * m13post) # Individuals that move from 2 to 5 to 3 but not to 1
    
    #---Scenario1 3->2->5 ---#
    cell235e <- pop3post * m23post * m5post # Individuals that move from 3 to 2 to 5
    cell235f <- cell235e - (cell235e * m5post) # Individuals that move from 3 to 2 to 5 but not to 1
    
    #---Scenario1 3->5->2 ---#
    cell235g <- pop3post * m5post * m5post # Individuals that move from 3 to 5 to 2
    cell235h <- cell235g - (cell235g * m12post) # Individuals that move from 3 to 5 to 2 but not to 1
    
    #---Scenario1 5->2->3 ---#
    cell235i <- pop5post * m5post * m23post # Individuals that move from 5 to 2 to 3
    cell235j <- cell235i - (cell235i * m13post) # Individuals that move from 5 to 2 to 3 but not to 1
    
    #---Scenario1 5->3->2 ---#
    cell235k <- pop5post * m5post * m23post # Individuals that move from 5 to 3 to 2
    cell235l <- cell235k - (cell235k * m12post) # Individuals that move from 5 to 3 to 2 but not to 1
    
    cell235 <- cell235b + cell235d + cell235f + cell235h + cell235j + cell235l
    
    
    #---------------------------------#
    # Populations 1, 2, 3, & 5        #
    #---------------------------------#
    
    #---Senario1 1->2->3->5 ---#
    cell1235a <- pop1post * m12post * m23post * m5post # Individuals that move from 1 to 2 to 3 to 5
    
    #---Senario1 1->2->5->3 ---#
    cell1235c <- pop1post * m12post * m5post * m5post # Individuals that move from 1 to 2 to 5 to 3
    
    #---Senario1 1->3->2->5 ---#
    cell1235e <- pop1post * m13post * m23post * m5post # Individuals that move from 1 to 3 to 2 to 5
    
    #---Senario1 1->3->5->2 ---#
    cell1235g <- pop1post * m13post * m5post * m5post # Individuals that move from 1 to 3 to 5 to 2
    
    #---Senario1 1->5->2->3 ---#
    cell1235i <- pop1post * m5post * m5post * m23post # Individuals that move from 1 to 5 to 2 to 3
    
    #---Senario1 1->5->3->2 ---#
    cell1235k <- pop1post * m5post * m5post * m23post # Individuals that move from 1 to 5 to 3 to 2
    
    #---Senario1 2->1->3->5 ---#
    cell1235m <- pop2post * m12post * m13post * m5post # Individuals that move from 2 to 1 to 3 to 5
    
    #---Senario1 2->1->5->3 ---#
    cell1235o <- pop2post * m12post * m5post * m5post # Individuals that move from 2 to 1 to 5 to 3
    
    #---Senario1 2->3->1->5 ---#
    cell1235q <- pop2post * m23post * m13post * m5post # Individuals that move from 2 to 3 to 1 to 5
    
    #---Senario1 2->3->5->1 ---#
    cell1235s <- pop2post * m23post * m5post * m5post # Individuals that move from 2 to 3 to 5 to 1
    
    #---Senario1 2->5->1->3 ---#
    cell1235u <- pop2post * m5post * m5post * m13post # Individuals that move from 2 to 5 to 1 to 3
    
    #---Senario1 2->5->3->1 ---#
    cell1235w <- pop2post * m5post * m5post * m13post # Individuals that move from 2 to 5 to 3 to 1
    
    #---Senario1 3->1->2->5 ---#
    cell1235y <- pop3post * m13post * m12post * m5post # Individuals that move from 3 to 1 to 2 to 5
    
    #---Senario1 3->1->5->2 ---#
    cell1235aa <- pop3post * m13post * m5post * m5post # Individuals that move from 3 to 1 to 5 to 2
    
    #---Senario1 3->2->1->5 ---#
    cell1235ac <- pop3post * m23post * m12post * m5post # Individuals that move from 3 to 2 to 1 to 5
    
    #---Senario1 3->2->5->1 ---#
    cell1235ae <- pop3post * m23post * m5post * m5post # Individuals that move from 3 to 2 to 5 to 1
    
    #---Senario1 3->5->1->2 ---#
    cell1235ag <- pop3post * m5post * m5post * m12post # Individuals that move from 3 to 5 to 1 to 2
    
    #---Senario1 3->5->2->1 ---#
    cell1235ai <- pop3post * m5post * m5post * m12post # Individuals that move from 3 to 5 to 2 to 1
    
    #---Senario1 5->1->2->3 ---#
    cell1235ak <- pop5post * m5post * m12post * m23post # Individuals that move from 5 to 1 to 2 to 3
    
    #---Senario1 5->1->3->2 ---#
    cell1235am <- pop5post * m5post * m13post * m23post # Individuals that move from 5 to 1 to 3 to 2
    
    #---Senario1 5->2->1->3 ---#
    cell1235ao <- pop5post * m5post * m12post * m13post # Individuals that move from 5 to 2 to 1 to 3
    
    #---Senario1 5->2->3->1 ---#
    cell1235aq <- pop5post * m5post * m23post * m13post # Individuals that move from 5 to 2 to 3 to 1
    
    #---Senario1 5->3->1->2 ---#
    cell1235as <- pop5post * m5post * m13post * m12post # Individuals that move from 5 to 3 to 1 to 2
    
    #---Senario1 5->3->2->1 ---#
    cell1235au <- pop5post * m5post * m23post * m12post # Individuals that move from 5 to 3 to 2 to 1
    
    cell1235 <- cell1235a + cell1235c + cell1235e + cell1235g + cell1235i + cell1235k + cell1235m + cell1235o + cell1235q + cell1235s + cell1235u + cell1235w + cell1235y + cell1235aa + cell1235ac + cell1235ae + cell1235ag + cell1235ai + cell1235ak + cell1235am + cell1235ao + cell1235aq + cell1235as + cell1235au
    

    totalpop <- (cell1 + cell2 + cell3 + cell5 + cell12 + cell13 + cell15 + cell23 + cell25 + cell35 + cell123 + cell125 + cell135 + cell235 + cell1235)

    #---------------------------------#
    # Plot results                    #
    #---------------------------------#
    par(mfrow = c(1, 1))
    histinfo = plotPost(totalpop, xlab = "Total Abundance", showMode = TRUE, col = "gray")

    return(totalpop)
}

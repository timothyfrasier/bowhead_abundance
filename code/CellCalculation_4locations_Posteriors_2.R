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

totals <- function(pop1post, pop2post, pop3post, pop4post, pop5post, m12post, m13post, m14post, m23post, m24post, m34post, m5post) {

    #---------------------------------#
    # Just in location 1              #
    #---------------------------------#
    cell1 <- pop1post - (pop1post * m12post) - (pop1post * m13post) - (pop1post * m14post) - (pop1post * m5post) # Individuals from location 1 that don't move anywhere else

    #----------------------------------#
    # Just in location 2              #
    #---------------------------------#
    cell2 <- pop2post - (pop2post * m12post) - (pop2post * m23post) - (pop2post * m24post) - (pop2post * m5post) # Individuals from location 2 that don't move anywhere else

    #---------------------------------#
    # Just in location 3              #
    #---------------------------------#
    cell3 <- pop3post - (pop3post * m13post) - (pop3post * m23post) - (pop3post * m34post) - (pop3post * m5post) # Individuals from location 3 that don't move anywhere else

    #---------------------------------#
    # Just in location 4              #
    #---------------------------------#
    cell4 <- pop4post - (pop4post * m14post) - (pop4post * m24post) - (pop4post * m34post) - (pop4post * m5post) # Individuals from location 4 that don't move anywhere else

    #---------------------------------#
    # Just in location 5              #
    #---------------------------------#
    cell5 <- pop5post - (pop5post * m5post) - (pop5post * m5post) - (pop5post * m5post) - (pop5post * m5post) # Individuals from location 4 that don't move anywhere else

    #---------------------------------#
    # Population 1 & 2                #
    #---------------------------------#
    cell12y <- pop1post * m12post  # Pop1 individuals that go to 2
    cell12a <- cell12y - (cell12y * m23post) - (cell12y * m24post) - (cell12y * m5post)  # Pop1 individuals that go to 2 but not 3, 4, or 5
    cell12z <- pop2post * m12post  # Pop2 individuals that go to 1
    cell12b <- cell12z - (cell12z * m13post) - (cell12z * m14post) - (cell12z * m5post)  # Pop2 individuals that go to 1 but not 3, 4, or 5
    cell12 <- cell12a + cell12b

    #---------------------------------#
    # Population 1 & 3                #
    #---------------------------------#
    cell13y <- pop1post * m13post  # Pop1 individuals that go to 3
    cell13a <- cell13y - (cell13y * m23post) - (cell13y * m34post) - (cell13y * m5post)  # Pop1 individuals that go to 3 but not 2, 4, or 5
    cell13z <- pop3post * m13post  # Pop3 individuals that go to 1
    cell13b <- cell13z - (cell13z * m12post) - (cell13z * m14post) - (cell13z * m5post)  # Pop3 individuals that go to 1 but not 2, 4, or 5
    cell13 <- cell13a + cell13b

    #---------------------------------#
    # Population 1 & 4                #
    #---------------------------------#
    cell14y <- pop1post * m14post  # Pop1 individuals that go to 4
    cell14a <- cell14y - (cell14y * m24post) - (cell14y * m34post) - (cell14y * m5post)  # Pop1 individuals that go to 4 but not 2, 3, or 5
    cell14z <- pop4post * m14post  # Pop4 individuals that go to 1
    cell14b <- cell14z - (cell14z * m12post) - (cell14z * m13post) - (cell14z * m5post)  # Pop4 individuals that go to 1 but not 2, 3, or 5
    cell14 <- cell14a + cell14b
    
    #---------------------------------#
    # Population 1 & 5                #
    #---------------------------------#
    cell15y <- pop1post * m5post  # Pop1 individuals that go to 5
    cell15a <- cell15y - (cell15y * m5post) - (cell15y * m5post) - (cell15y * m5post)  # Pop1 individuals that go to 5 but not 2, 3, or 4
    cell15z <- pop5post * m5post  # Pop5 individuals that go to 1
    cell15b <- cell15z - (cell15z * m12post) - (cell15z * m13post) - (cell15z * m14post)  # Pop4 individuals that go to 1 but not 2, 3, or 4
    cell15 <- cell15a + cell15b
    
    
    #---------------------------------#
    # Population 2 & 3                #
    #---------------------------------#
    cell23y <- pop2post * m23post  # Pop2 individuals that go to 3
    cell23a <- cell23y - (cell23y * m13post) - (cell23y * m34post) - (cell23y * m5post)  # Pop2 individuals that go to 3 but not 1, 4, or 5
    cell23z <- pop3post * m23post  # Pop3 individuals that go to 2
    cell23b <- cell23z - (cell23z * m12post) - (cell23z * m24post) - (cell23z * m5post)  # Pop3 individuals that go to 2 but not 1, 4, or 5
    cell23 <- cell23a + cell23b

    #---------------------------------#
    # Population 2 & 4                #
    #---------------------------------#
    cell24y <- pop2post * m24post  # Pop2 individuals that go to 4
    cell24a <- cell24y - (cell24y * m14post) - (cell24y * m34post) - (cell24y * m5post)  # Pop2 individuals that go to 4 but not 1, 3, or 5
    cell24z <- pop4post * m24post  # Pop4 individuals that go to 2
    cell24b <- cell24z - (cell24z * m12post) - (cell24z * m23post) - (cell24z * m5post)  # Pop4 individuals that go to 2 but not 1, 3, or 5
    cell24 <- cell24a + cell24b
    
    #---------------------------------#
    # Population 2 & 5                #
    #---------------------------------#
    cell25y <- pop2post * m5post  # Pop2 individuals that go to 5
    cell25a <- cell25y - (cell25y * m5post) - (cell25y * m5post) - (cell25y * m5post)  # Pop2 individuals that go to 5 but not 1, 3, or 4
    cell25z <- pop5post * m5post  # Pop5 individuals that go to 2
    cell25b <- cell25z - (cell25z * m12post) - (cell25z * m23post) - (cell25z * m24post)  # Pop5 individuals that go to 2 but not 1, 3, or 4
    cell25 <- cell25a + cell25b

    #---------------------------------#
    # Population 3 & 4                #
    #---------------------------------#
    cell34y <- pop3post * m34post  # Pop3 individuals that go to 4
    cell34a <- cell34y - (cell34y * m14post) - (cell34y * m24post) - (cell34y * m5post)  # Pop3 individuals that go to 4 but not 1, 2, or 5
    cell34z <- pop4post * m34post  # Pop4 individuals that go to 3
    cell34b <- cell34z - (cell34z * m13post) - (cell34z * m23post) - (cell34z * m5post)  # Pop4 individuals that go to 3 but not 1, 2, or 5
    cell34 <- cell34a + cell34b
    
    #---------------------------------#
    # Population 3 & 5                #
    #---------------------------------#
    cell35y <- pop3post * m5post  # Pop3 individuals that go to 5
    cell35a <- cell35y - (cell35y * m5post) - (cell35y * m5post) - (cell35y * m5post)  # Pop3 individuals that go to 5 but not 1, 2, or 4
    cell35z <- pop5post * m5post  # Pop5 individuals that go to 3
    cell35b <- cell35z - (cell35z * m13post) - (cell35z * m23post) - (cell35z * m34post)  # Pop5 individuals that go to 3 but not 1, 2, or 4
    cell35 <- cell35a + cell35b
    
    #---------------------------------#
    # Population 4 & 5                #
    #---------------------------------#
    cell45y <- pop4post * m5post  # Pop4 individuals that go to 5
    cell45a <- cell45y - (cell45y * m5post) - (cell45y * m5post) - (cell45y * m5post)  # Pop4 individuals that go to 5 but not 1, 2, or 3
    cell45z <- pop5post * m5post  # Pop5 individuals that go to 4
    cell45b <- cell45z - (cell45z * m14post) - (cell45z * m24post) - (cell45z * m34post)  # Pop5 individuals that go to 4 but not 1, 2, or 3
    cell45 <- cell45a + cell45b
    
    #---------------------------------#
    # Populations 1, 2, 3             #
    #---------------------------------#
    
    #---Scenario1 1->2->3 ---#
    cell123a <- pop1post * m12post * m23post # Individuals that move from 1 to 2 to 3
    cell123b <- cell123a - (cell123a * m34post) - (cell123a * m5post) # Individuals that move from 1 to 2 to 3 but not to 4 or 5
    
    #---Scenario1 1->3->2 ---#
    cell123c <- pop1post * m13post * m23post # Individuals that move from 1 to 3 to 2
    cell123d <- cell123c - (cell123c * m24post) - (cell123c * m5post) # Individuals that move from 1 to 3 to 2 but not to 4 or 5

    #---Scenario1 2->1->3 ---#
    cell123e <- pop2post * m12post * m13post # Individuals that move from 2 to 1 to 3
    cell123f <- cell123e - (cell123e * m34post) - (cell123e * m5post) # Individuals that move from 2 to 1 to 3 but not to 4 or 5
    
    #---Scenario1 2->3->1 ---#
    cell123g <- pop2post * m23post * m13post # Individuals that move from 2 to 3 to 1
    cell123h <- cell123g - (cell123g * m14post) - (cell123g * m5post) # Individuals that move from 2 to 3 to 1 but not to 4 or 5

    #---Scenario1 3->1->2 ---#
    cell123i <- pop3post * m13post * m12post # Individuals that move from 3 to 1 to 2
    cell123j <- cell123i - (cell123i * m24post) - (cell123i * m5post) # Individuals that move from 3 to 1 to 2 but not to 4 or 5
    
    #---Scenario1 3->2->1 ---#
    cell123k <- pop3post * m23post * m12post # Individuals that move from 3 to 2 to 1
    cell123l <- cell123k - (cell123k * m14post) - (cell123k * m5post) # Individuals that move from 3 to 2 to 1 but not to 4 or 5
    
    cell123 <- cell123b + cell123d + cell123f + cell123h + cell123j + cell123l
    
    
    #---------------------------------#
    # Populations 1, 2, 4             #
    #---------------------------------#
    
    #---Scenario1 1->2->4 ---#
    cell124a <- pop1post * m12post * m24post # Individuals that move from 1 to 2 to 4
    cell124b <- cell124a - (cell124a * m34post) - (cell124a * m5post) # Individuals that move from 1 to 2 to 4 but not to 3 or 5
    
    #---Scenario1 1->4->2 ---#
    cell124c <- pop1post * m14post * m24post # Individuals that move from 1 to 4 to 2
    cell124d <- cell124c - (cell124c * m23post) - (cell124c * m5post) # Individuals that move from 1 to 4 to 2 but not to 3 or 5
    
    #---Scenario1 2->1->4 ---#
    cell124e <- pop2post * m12post * m14post # Individuals that move from 2 to 1 to 4
    cell124f <- cell124e - (cell124e * m34post) - (cell124e * m5post) # Individuals that move from 2 to 1 to 4 but not to 3 or 5
    
    #---Scenario1 2->4->1 ---#
    cell124g <- pop2post * m24post * m14post # Individuals that move from 2 to 4 to 1
    cell124h <- cell124g - (cell124g * m13post) - (cell124g * m5post) # Individuals that move from 2 to 4 to 1 but not to 3 or 5
    
    #---Scenario1 4->1->2 ---#
    cell124i <- pop4post * m14post * m12post # Individuals that move from 4 to 1 to 2
    cell124j <- cell124i - (cell124i * m23post) - (cell124i * m5post) # Individuals that move from 4 to 1 to 2 but not to 3 or 5
    
    #---Scenario1 4->2->1 ---#
    cell124k <- pop4post * m24post * m12post # Individuals that move from 4 to 2 to 1
    cell124l <- cell124k - (cell124k * m13post) - (cell124k * m5post) # Individuals that move from 4 to 2 to 1 but not to 3 or 5
    
    cell124 <- cell124b + cell124d + cell124f + cell124h + cell124j + cell124l
    
    
    #---------------------------------#
    # Populations 1, 2, 5             #
    #---------------------------------#
    
    #---Scenario1 1->2->5 ---#
    cell125a <- pop1post * m12post * m5post # Individuals that move from 1 to 2 to 5
    cell125b <- cell125a - (cell125a * m5post) - (cell125a * m5post) # Individuals that move from 1 to 2 to 5 but not to 3 or 4
    
    #---Scenario1 1->5->2 ---#
    cell125c <- pop1post * m5post * m5post # Individuals that move from 1 to 5 to 2
    cell125d <- cell125c - (cell125c * m23post) - (cell125c * m24post) # Individuals that move from 1 to 5 to 2 but not to 3 or 4
    
    #---Scenario1 2->1->5 ---#
    cell125e <- pop2post * m12post * m5post # Individuals that move from 2 to 1 to 5
    cell125f <- cell125e - (cell125e * m5post) - (cell125e * m5post) # Individuals that move from 2 to 1 to 5 but not to 3 or 4
    
    #---Scenario1 2->5->1 ---#
    cell125g <- pop2post * m5post * m5post # Individuals that move from 2 to 5 to 1
    cell125h <- cell125g - (cell125g * m13post) - (cell125g * m14post) # Individuals that move from 2 to 5 to 1 but not to 3 or 4
    
    #---Scenario1 5->1->2 ---#
    cell125i <- pop5post * m5post * m12post # Individuals that move from 5 to 1 to 2
    cell125j <- cell125i - (cell125i * m23post) - (cell125i * m24post) # Individuals that move from 5 to 1 to 2 but not to 3 or 4
    
    #---Scenario1 5->2->1 ---#
    cell125k <- pop5post * m5post * m12post # Individuals that move from 5 to 2 to 1
    cell125l <- cell125k - (cell125k * m13post) - (cell125k * m14post) # Individuals that move from 5 to 2 to 1 but not to 3 or 4
    
    cell125 <- cell125b + cell125d + cell125f + cell125h + cell125j + cell125l


    #---------------------------------#
    # Populations 1, 3, 4             #
    #---------------------------------#
    
    #---Scenario1 1->3->4 ---#
    cell134a <- pop1post * m13post * m34post # Individuals that move from 1 to 3 to 4
    cell134b <- cell134a - (cell134a * m24post) - (cell134a * m5post) # Individuals that move from 1 to 3 to 4 but not to 2 or 5
    
    #---Scenario1 1->4->3 ---#
    cell134c <- pop1post * m14post * m34post # Individuals that move from 1 to 4 to 3
    cell134d <- cell134c - (cell134c * m23post) - (cell134c * m5post) # Individuals that move from 1 to 4 to 3 but not to 2 or 5
    
    #---Scenario1 3->1->4 ---#
    cell134e <- pop3post * m13post * m14post # Individuals that move from 3 to 1 to 4
    cell134f <- cell134e - (cell134e * m24post) - (cell134e * m5post) # Individuals that move from 3 to 1 to 4 but not to 2 or 5
    
    #---Scenario1 3->4->1 ---#
    cell134g <- pop3post * m34post * m14post # Individuals that move from 3 to 4 to 1
    cell134h <- cell134g - (cell134g * m12post) - (cell134g * m5post) # Individuals that move from 3 to 4 to 1 but not to 2 or 5
    
    #---Scenario1 4->1->3 ---#
    cell134i <- pop4post * m14post * m13post # Individuals that move from 4 to 1 to 3
    cell134j <- cell134i - (cell134i * m23post) - (cell134i * m5post) # Individuals that move from 4 to 1 to 3 but not to 2 or 5
    
    #---Scenario1 4->3->1 ---#
    cell134k <- pop4post * m34post * m13post # Individuals that move from 4 to 3 to 1
    cell134l <- cell134k - (cell134k * m12post) - (cell134k * m5post) # Individuals that move from 4 to 3 to 1 but not to 2 or 5
    
    cell134 <- cell134b + cell134d + cell134f + cell134h + cell134j + cell134l
    
    
    #---------------------------------#
    # Populations 1, 3, 5             #
    #---------------------------------#
    
    #---Scenario1 1->3->5 ---#
    cell135a <- pop1post * m13post * m5post # Individuals that move from 1 to 3 to 5
    cell135b <- cell135a - (cell135a * m5post) - (cell135a * m5post) # Individuals that move from 1 to 3 to 5 but not to 2 or 4
    
    #---Scenario1 1->5->3 ---#
    cell135c <- pop1post * m5post * m5post # Individuals that move from 1 to 5 to 3
    cell135d <- cell135c - (cell135c * m23post) - (cell135c * m34post) # Individuals that move from 1 to 5 to 3 but not to 2 or 4
    
    #---Scenario1 3->1->5 ---#
    cell135e <- pop3post * m13post * m5post # Individuals that move from 3 to 1 to 5
    cell135f <- cell135e - (cell135e * m5post) - (cell135e * m5post) # Individuals that move from 3 to 1 to 5 but not to 2 or 4
    
    #---Scenario1 3->5->1 ---#
    cell135g <- pop3post * m5post * m5post # Individuals that move from 3 to 5 to 1
    cell135h <- cell135g - (cell135g * m12post) - (cell135g * m14post) # Individuals that move from 3 to 5 to 1 but not to 2 or 4
    
    #---Scenario1 5->1->3 ---#
    cell135i <- pop5post * m5post * m13post # Individuals that move from 5 to 1 to 3
    cell135j <- cell135i - (cell135i * m23post) - (cell135i * m34post) # Individuals that move from 5 to 1 to 3 but not to 2 or 4
    
    #---Scenario1 5->3->1 ---#
    cell135k <- pop5post * m5post * m13post # Individuals that move from 5 to 3 to 1
    cell135l <- cell135k - (cell135k * m12post) - (cell135k * m14post) # Individuals that move from 5 to 3 to 1 but not to 2 or 4
    
    cell135 <- cell135b + cell135d + cell135f + cell135h + cell135j + cell135l
    
    
    #---------------------------------#
    # Populations 1, 4, 5             #
    #---------------------------------#
    
    #---Scenario1 1->4->5 ---#
    cell145a <- pop1post * m14post * m5post # Individuals that move from 1 to 4 to 5
    cell145b <- cell145a - (cell145a * m5post) - (cell145a * m5post) # Individuals that move from 1 to 4 to 5 but not to 2 or 3
    
    #---Scenario1 1->5->4 ---#
    cell145c <- pop1post * m5post * m5post # Individuals that move from 1 to 5 to 4
    cell145d <- cell145c - (cell145c * m24post) - (cell145c * m34post) # Individuals that move from 1 to 5 to 4 but not to 2 or 3
    
    #---Scenario1 4->1->5 ---#
    cell145e <- pop4post * m14post * m5post # Individuals that move from 4 to 1 to 5
    cell145f <- cell145e - (cell145e * m5post) - (cell145e * m5post) # Individuals that move from 4 to 1 to 5 but not to 2 or 3
    
    #---Scenario1 4->5->1 ---#
    cell145g <- pop4post * m5post * m5post # Individuals that move from 4 to 5 to 1
    cell145h <- cell145g - (cell145g * m12post) - (cell145g * m13post) # Individuals that move from 4 to 5 to 1 but not to 2 or 3
    
    #---Scenario1 5->1->4 ---#
    cell145i <- pop5post * m5post * m14post # Individuals that move from 5 to 1 to 4
    cell145j <- cell145i - (cell145i * m24post) - (cell145i * m34post) # Individuals that move from 5 to 1 to 4 but not to 2 or 3
    
    #---Scenario1 5->4->1 ---#
    cell145k <- pop5post * m5post * m14post # Individuals that move from 5 to 4 to 1
    cell145l <- cell145k - (cell145k * m12post) - (cell145k * m13post) # Individuals that move from 5 to 4 to 1 but not to 2 or 3
    
    cell145 <- cell145b + cell145d + cell145f + cell145h + cell145j + cell145l

    
    #---------------------------------#
    # Populations 2, 3, 4             #
    #---------------------------------#
    
    #---Scenario1 2->3->4 ---#
    cell234a <- pop2post * m23post * m34post # Individuals that move from 2 to 3 to 4
    cell234b <- cell234a - (cell234a * m14post) - (cell234a * m5post) # Individuals that move from 2 to 3 to 4 but not to 1 or 5
    
    #---Scenario1 2->4->3 ---#
    cell234c <- pop2post * m24post * m34post # Individuals that move from 2 to 4 to 3
    cell234d <- cell234c - (cell234c * m13post) - (cell234c * m5post) # Individuals that move from 2 to 4 to 3 but not to 1 or 5
    
    #---Scenario1 3->2->4 ---#
    cell234e <- pop3post * m23post * m24post # Individuals that move from 3 to 2 to 4
    cell234f <- cell234e - (cell234e * m14post) - (cell234e * m5post) # Individuals that move from 3 to 2 to 4 but not to 1 or 5
    
    #---Scenario1 3->4->2 ---#
    cell234g <- pop3post * m34post * m24post # Individuals that move from 3 to 4 to 2
    cell234h <- cell234g - (cell234g * m12post) - (cell234g * m5post) # Individuals that move from 3 to 4 to 2 but not to 1 or 5
    
    #---Scenario1 4->2->3 ---#
    cell234i <- pop4post * m24post * m23post # Individuals that move from 4 to 2 to 3
    cell234j <- cell234i - (cell234i * m13post) - (cell234i * m5post) # Individuals that move from 4 to 2 to 3 but not to 1 or 5
    
    #---Scenario1 4->3->2 ---#
    cell234k <- pop4post * m34post * m23post # Individuals that move from 4 to 3 to 2
    cell234l <- cell234k - (cell234k * m12post) - (cell234k * m5post) # Individuals that move from 4 to 3 to 2 but not to 1 or 5
    
    cell234 <- cell234b + cell234d + cell234f + cell234h + cell234j + cell234l
    
    
    #---------------------------------#
    # Populations 2, 3, 5             #
    #---------------------------------#
    
    #---Scenario1 2->3->5 ---#
    cell235a <- pop2post * m23post * m5post # Individuals that move from 2 to 3 to 5
    cell235b <- cell235a - (cell235a * m5post) - (cell235a * m5post) # Individuals that move from 2 to 3 to 5 but not to 1 or 4
    
    #---Scenario1 2->5->3 ---#
    cell235c <- pop2post * m5post * m5post # Individuals that move from 2 to 5 to 3
    cell235d <- cell235c - (cell235c * m13post) - (cell235c * m34post) # Individuals that move from 2 to 5 to 3 but not to 1 or 4
    
    #---Scenario1 3->2->5 ---#
    cell235e <- pop3post * m23post * m5post # Individuals that move from 3 to 2 to 5
    cell235f <- cell235e - (cell235e * m5post) - (cell235e * m5post) # Individuals that move from 3 to 2 to 5 but not to 1 or 4
    
    #---Scenario1 3->5->2 ---#
    cell235g <- pop3post * m5post * m5post # Individuals that move from 3 to 5 to 2
    cell235h <- cell235g - (cell235g * m12post) - (cell235g * m24post) # Individuals that move from 3 to 5 to 2 but not to 1 or 4
    
    #---Scenario1 5->2->3 ---#
    cell235i <- pop5post * m5post * m23post # Individuals that move from 5 to 2 to 3
    cell235j <- cell235i - (cell235i * m13post) - (cell235i * m34post) # Individuals that move from 5 to 2 to 3 but not to 1 or 4
    
    #---Scenario1 5->3->2 ---#
    cell235k <- pop5post * m5post * m23post # Individuals that move from 5 to 3 to 2
    cell235l <- cell235k - (cell235k * m12post) - (cell235k * m24post) # Individuals that move from 5 to 3 to 2 but not to 1 or 4
    
    cell235 <- cell235b + cell235d + cell235f + cell235h + cell235j + cell235l
    
    
    #---------------------------------#
    # Populations 2, 4, 5             #
    #---------------------------------#
    
    #---Scenario1 2->4->5 ---#
    cell245a <- pop2post * m24post * m5post # Individuals that move from 2 to 4 to 5
    cell245b <- cell245a - (cell245a * m5post) - (cell245a * m5post) # Individuals that move from 2 to 4 to 5 but not to 1 or 3
    
    #---Scenario1 2->5->4 ---#
    cell245c <- pop2post * m5post * m5post # Individuals that move from 2 to 5 to 4
    cell245d <- cell245c - (cell245c * m14post) - (cell245c * m34post) # Individuals that move from 2 to 5 to 4 but not to 1 or 3
    
    #---Scenario1 4->2->5 ---#
    cell245e <- pop4post * m24post * m5post # Individuals that move from 4 to 2 to 5
    cell245f <- cell245e - (cell245e * m5post) - (cell245e * m5post) # Individuals that move from 4 to 2 to 5 but not to 1 or 3
    
    #---Scenario1 4->5->2 ---#
    cell245g <- pop4post * m5post * m5post # Individuals that move from 4 to 5 to 2
    cell245h <- cell245g - (cell245g * m12post) - (cell245g * m23post) # Individuals that move from 4 to 5 to 2 but not to 1 or 3
    
    #---Scenario1 5->2->4 ---#
    cell245i <- pop5post * m5post * m24post # Individuals that move from 5 to 2 to 4
    cell245j <- cell245i - (cell245i * m14post) - (cell245i * m34post) # Individuals that move from 5 to 2 to 4 but not to 1 or 3
    
    #---Scenario1 5->4->2 ---#
    cell245k <- pop5post * m5post * m24post # Individuals that move from 5 to 4 to 2
    cell245l <- cell245k - (cell245k * m12post) - (cell245k * m23post) # Individuals that move from 5 to 4 to 2 but not to 1 or 3
    
    cell245 <- cell245b + cell245d + cell245f + cell245h + cell245j + cell245l
    
    
    #---------------------------------#
    # Populations 3, 4, 5             #
    #---------------------------------#
    
    #---Scenario1 3->4->5 ---#
    cell345a <- pop3post * m34post * m5post # Individuals that move from 3 to 4 to 5
    cell345b <- cell345a - (cell345a * m5post) - (cell345a * m5post) # Individuals that move from 3 to 4 to 5 but not to 1 or 2
    
    #---Scenario1 3->5->4 ---#
    cell345c <- pop3post * m5post * m5post # Individuals that move from 3 to 5 to 4
    cell345d <- cell345c - (cell345c * m14post) - (cell345c * m24post) # Individuals that move from 3 to 5 to 4 but not to 1 or 2
    
    #---Scenario1 4->3->5 ---#
    cell345e <- pop4post * m34post * m5post # Individuals that move from 4 to 3 to 5
    cell345f <- cell345e - (cell345e * m5post) - (cell345e * m5post) # Individuals that move from 4 to 3 to 5 but not to 1 or 2
    
    #---Scenario1 4->5->3 ---#
    cell345g <- pop4post * m5post * m5post # Individuals that move from 4 to 5 to 3
    cell345h <- cell345g - (cell345g * m13post) - (cell345g * m23post) # Individuals that move from 4 to 5 to 3 but not to 1 or 2
    
    #---Scenario1 5->3->4 ---#
    cell345i <- pop5post * m5post * m34post # Individuals that move from 5 to 3 to 4
    cell345j <- cell345i - (cell345i * m14post) - (cell345i * m24post) # Individuals that move from 5 to 3 to 4 but not to 1 or 2
    
    #---Scenario1 5->4->3 ---#
    cell345k <- pop5post * m5post * m34post # Individuals that move from 5 to 4 to 3
    cell345l <- cell345k - (cell345k * m13post) - (cell345k * m23post) # Individuals that move from 5 to 4 to 3 but not to 1 or 2
    
    cell345 <- cell345b + cell345d + cell345f + cell345h + cell345j + cell345l
    
    
    #---------------------------------#
    # Populations 1, 2, 3, & 4        #
    #---------------------------------#

    #---Senario1 1->2->3->4 ---#
    cell1234a <- pop1post * m12post * m23post * m34post # Individuals that move from 1 to 2 to 3 to 4
    cell1234b <- cell1234a - (cell1234a * m5post) # Individuals that move from 1 to 2 to 3 to 4 but not to 5
    
    #---Senario1 1->2->4->3 ---#
    cell1234c <- pop1post * m12post * m24post * m34post # Individuals that move from 1 to 2 to 4 to 3
    cell1234d <- cell1234c - (cell1234c * m5post) # Individuals that move from 1 to 2 to 4 to 3 but not to 5
    
    #---Senario1 1->3->2->4 ---#
    cell1234e <- pop1post * m13post * m23post * m24post # Individuals that move from 1 to 3 to 2 to 4
    cell1234f <- cell1234e - (cell1234e * m5post) # Individuals that move from 1 to 3 to 2 to 4 but not to 5
    
    #---Senario1 1->3->4->2 ---#
    cell1234g <- pop1post * m13post * m34post * m24post # Individuals that move from 1 to 3 to 4 to 2
    cell1234h <- cell1234g - (cell1234g * m5post) # Individuals that move from 1 to 3 to 4 to 2 but not to 5

    #---Senario1 1->4->2->3 ---#
    cell1234i <- pop1post * m14post * m24post * m23post # Individuals that move from 1 to 4 to 2 to 3
    cell1234j <- cell1234i - (cell1234i * m5post) # Individuals that move from 1 to 4 to 2 to 3 but not to 5
    
    #---Senario1 1->4->3->2 ---#
    cell1234k <- pop1post * m14post * m34post * m23post # Individuals that move from 1 to 4 to 3 to 2
    cell1234l <- cell1234k - (cell1234k * m5post) # Individuals that move from 1 to 4 to 3 to 2 but not to 5

    #---Senario1 2->1->3->4 ---#
    cell1234m <- pop2post * m12post * m13post * m34post # Individuals that move from 2 to 1 to 3 to 4
    cell1234n <- cell1234m - (cell1234m * m5post) # Individuals that move from 2 to 1 to 3 to 4 but not to 5
    
    #---Senario1 2->1->4->3 ---#
    cell1234o <- pop2post * m12post * m14post * m34post # Individuals that move from 2 to 1 to 4 to 3
    cell1234p <- cell1234o - (cell1234o * m5post) # Individuals that move from 2 to 1 to 4 to 3 but not to 5
    
    #---Senario1 2->3->1->4 ---#
    cell1234q <- pop2post * m23post * m13post * m14post # Individuals that move from 2 to 3 to 1 to 4
    cell1234r <- cell1234q - (cell1234q * m5post) # Individuals that move from 2 to 3 to 1 to 4 but not to 5
    
    #---Senario1 2->3->4->1 ---#
    cell1234s <- pop2post * m23post * m34post * m14post # Individuals that move from 2 to 3 to 4 to 1
    cell1234t <- cell1234s - (cell1234s * m5post) # Individuals that move from 2 to 3 to 4 to 1 but not to 5
    
    #---Senario1 2->4->1->3 ---#
    cell1234u <- pop2post * m24post * m14post * m13post # Individuals that move from 2 to 4 to 1 to 3
    cell1234v <- cell1234u - (cell1234u * m5post) # Individuals that move from 2 to 4 to 1 to 3 but not to 5
    
    #---Senario1 2->4->3->1 ---#
    cell1234w <- pop2post * m24post * m34post * m13post # Individuals that move from 2 to 4 to 3 to 1
    cell1234x <- cell1234w - (cell1234w * m5post) # Individuals that move from 2 to 4 to 3 to 1 but not to 5
    
    #---Senario1 3->1->2->4 ---#
    cell1234y <- pop3post * m13post * m12post * m24post # Individuals that move from 3 to 1 to 2 to 4
    cell1234z <- cell1234y - (cell1234y * m5post) # Individuals that move from 3 to 1 to 2 to 4 but not to 5
    
    #---Senario1 3->1->4->2 ---#
    cell1234aa <- pop3post * m13post * m14post * m24post # Individuals that move from 3 to 1 to 4 to 2
    cell1234ab <- cell1234aa - (cell1234aa * m5post) # Individuals that move from 3 to 1 to 4 to 2 but not to 5
    
    #---Senario1 3->2->1->4 ---#
    cell1234ac <- pop3post * m23post * m12post * m14post # Individuals that move from 3 to 2 to 1 to 4
    cell1234ad <- cell1234ac - (cell1234ac * m5post) # Individuals that move from 3 to 2 to 1 to 4 but not to 5
    
    #---Senario1 3->2->4->1 ---#
    cell1234ae <- pop3post * m23post * m24post * m14post # Individuals that move from 3 to 2 to 4 to 1
    cell1234af <- cell1234ae - (cell1234ae * m5post) # Individuals that move from 3 to 2 to 4 to 1 but not to 5
    
    #---Senario1 3->4->1->2 ---#
    cell1234ag <- pop3post * m34post * m14post * m12post # Individuals that move from 3 to 4 to 1 to 2
    cell1234ah <- cell1234ag - (cell1234ag * m5post) # Individuals that move from 3 to 4 to 1 to 2 but not to 5
    
    #---Senario1 3->4->2->1 ---#
    cell1234ai <- pop3post * m34post * m24post * m12post # Individuals that move from 3 to 4 to 2 to 1
    cell1234aj <- cell1234ai - (cell1234ai * m5post) # Individuals that move from 3 to 4 to 2 to 1 but not to 5
    
    #---Senario1 4->1->2->3 ---#
    cell1234ak <- pop4post * m14post * m12post * m23post # Individuals that move from 4 to 1 to 2 to 3
    cell1234al <- cell1234ak - (cell1234ak * m5post) # Individuals that move from 4 to 1 to 2 to 3 but not to 5
    
    #---Senario1 4->1->3->2 ---#
    cell1234am <- pop4post * m14post * m13post * m23post # Individuals that move from 4 to 1 to 3 to 2
    cell1234an <- cell1234am - (cell1234am * m5post) # Individuals that move from 4 to 1 to 3 to 2 but not to 5
   
    #---Senario1 4->2->1->3 ---#
    cell1234ao <- pop4post * m24post * m12post * m13post # Individuals that move from 4 to 2 to 1 to 3
    cell1234ap <- cell1234ao - (cell1234ao * m5post) # Individuals that move from 4 to 2 to 1 to 3 but not to 5
    
    #---Senario1 4->2->3->1 ---#
    cell1234aq <- pop4post * m24post * m23post * m13post # Individuals that move from 4 to 2 to 3 to 1
    cell1234ar <- cell1234aq - (cell1234aq * m5post) # Individuals that move from 4 to 2 to 3 to 1 but not to 5
    
    #---Senario1 4->3->1->2 ---#
    cell1234as <- pop4post * m34post * m13post * m12post # Individuals that move from 4 to 3 to 1 to 2
    cell1234at <- cell1234as - (cell1234as * m5post) # Individuals that move from 4 to 3 to 1 to 2 but not to 5
    
    #---Senario1 4->3->2->1 ---#
    cell1234au <- pop4post * m34post * m23post * m12post # Individuals that move from 4 to 3 to 2 to 1
    cell1234av <- cell1234au - (cell1234au * m5post) # Individuals that move from 4 to 3 to 2 to 1 but not to 5
    
    cell1234 <- cell1234b + cell1234d + cell1234f + cell1234h + cell1234j + cell1234l + cell1234n + cell1234p + cell1234r + cell1234t + cell1234v + cell1234x + cell1234z + cell1234ab + cell1234ad + cell1234af + cell1234ah + cell1234aj + cell1234al + cell1234an + cell1234ap + cell1234ar + cell1234at + cell1234av
    
    
    #---------------------------------#
    # Populations 1, 2, 3, & 5        #
    #---------------------------------#
    
    #---Senario1 1->2->3->5 ---#
    cell1235a <- pop1post * m12post * m23post * m5post # Individuals that move from 1 to 2 to 3 to 5
    cell1235b <- cell1235a - (cell1235a * m5post) # Individuals that move from 1 to 2 to 3 to 5 but not to 4
    
    #---Senario1 1->2->5->3 ---#
    cell1235c <- pop1post * m12post * m5post * m5post # Individuals that move from 1 to 2 to 5 to 3
    cell1235d <- cell1235c - (cell1235c * m34post) # Individuals that move from 1 to 2 to 5 to 3 but not to 4
    
    #---Senario1 1->3->2->5 ---#
    cell1235e <- pop1post * m13post * m23post * m5post # Individuals that move from 1 to 3 to 2 to 5
    cell1235f <- cell1235e - (cell1235e * m5post) # Individuals that move from 1 to 3 to 2 to 5 but not to 4
    
    #---Senario1 1->3->5->2 ---#
    cell1235g <- pop1post * m13post * m5post * m5post # Individuals that move from 1 to 3 to 5 to 2
    cell1235h <- cell1235g - (cell1235g * m24post) # Individuals that move from 1 to 3 to 5 to 2 but not to 4
    
    #---Senario1 1->5->2->3 ---#
    cell1235i <- pop1post * m5post * m5post * m23post # Individuals that move from 1 to 5 to 2 to 3
    cell1235j <- cell1235i - (cell1235i * m34post) # Individuals that move from 1 to 5 to 2 to 3 but not to 4
    
    #---Senario1 1->5->3->2 ---#
    cell1235k <- pop1post * m5post * m5post * m23post # Individuals that move from 1 to 5 to 3 to 2
    cell1235l <- cell1235k - (cell1235k * m24post) # Individuals that move from 1 to 5 to 3 to 2 but not to 4
    
    #---Senario1 2->1->3->5 ---#
    cell1235m <- pop2post * m12post * m13post * m5post # Individuals that move from 2 to 1 to 3 to 5
    cell1235n <- cell1235m - (cell1235m * m5post) # Individuals that move from 2 to 1 to 3 to 5 but not to 4
    
    #---Senario1 2->1->5->3 ---#
    cell1235o <- pop2post * m12post * m5post * m5post # Individuals that move from 2 to 1 to 5 to 3
    cell1235p <- cell1235o - (cell1235o * m34post) # Individuals that move from 2 to 1 to 5 to 3 but not to 4
    
    #---Senario1 2->3->1->5 ---#
    cell1235q <- pop2post * m23post * m13post * m5post # Individuals that move from 2 to 3 to 1 to 5
    cell1235r <- cell1235q - (cell1235q * m5post) # Individuals that move from 2 to 3 to 1 to 5 but not to 4
    
    #---Senario1 2->3->5->1 ---#
    cell1235s <- pop2post * m23post * m5post * m5post # Individuals that move from 2 to 3 to 5 to 1
    cell1235t <- cell1235s - (cell1235s * m14post) # Individuals that move from 2 to 3 to 5 to 1 but not to 4
    
    #---Senario1 2->5->1->3 ---#
    cell1235u <- pop2post * m5post * m5post * m13post # Individuals that move from 2 to 5 to 1 to 3
    cell1235v <- cell1235u - (cell1235u * m34post) # Individuals that move from 2 to 5 to 1 to 3 but not to 4
    
    #---Senario1 2->5->3->1 ---#
    cell1235w <- pop2post * m5post * m5post * m13post # Individuals that move from 2 to 5 to 3 to 1
    cell1235x <- cell1235w - (cell1235w * m14post) # Individuals that move from 2 to 5 to 3 to 1 but not to 4
    
    #---Senario1 3->1->2->5 ---#
    cell1235y <- pop3post * m13post * m12post * m5post # Individuals that move from 3 to 1 to 2 to 5
    cell1235z <- cell1235y - (cell1235y * m5post) # Individuals that move from 3 to 1 to 2 to 5 but not to 4
    
    #---Senario1 3->1->5->2 ---#
    cell1235aa <- pop3post * m13post * m5post * m5post # Individuals that move from 3 to 1 to 5 to 2
    cell1235ab <- cell1235aa - (cell1235aa * m24post) # Individuals that move from 3 to 1 to 5 to 2 but not to 4
    
    #---Senario1 3->2->1->5 ---#
    cell1235ac <- pop3post * m23post * m12post * m5post # Individuals that move from 3 to 2 to 1 to 5
    cell1235ad <- cell1235ac - (cell1235ac * m5post) # Individuals that move from 3 to 2 to 1 to 5 but not to 4
    
    #---Senario1 3->2->5->1 ---#
    cell1235ae <- pop3post * m23post * m5post * m5post # Individuals that move from 3 to 2 to 5 to 1
    cell1235af <- cell1235ae - (cell1235ae * m14post) # Individuals that move from 3 to 2 to 5 to 1 but not to 4
    
    #---Senario1 3->5->1->2 ---#
    cell1235ag <- pop3post * m5post * m5post * m12post # Individuals that move from 3 to 5 to 1 to 2
    cell1235ah <- cell1235ag - (cell1235ag * m24post) # Individuals that move from 3 to 5 to 1 to 2 but not to 4
    
    #---Senario1 3->5->2->1 ---#
    cell1235ai <- pop3post * m5post * m5post * m12post # Individuals that move from 3 to 5 to 2 to 1
    cell1235aj <- cell1235ai - (cell1235ai * m14post) # Individuals that move from 3 to 5 to 2 to 1 but not to 4
    
    #---Senario1 5->1->2->3 ---#
    cell1235ak <- pop5post * m5post * m12post * m23post # Individuals that move from 5 to 1 to 2 to 3
    cell1235al <- cell1235ak - (cell1235ak * m34post) # Individuals that move from 5 to 1 to 2 to 3 but not to 4
    
    #---Senario1 5->1->3->2 ---#
    cell1235am <- pop5post * m5post * m13post * m23post # Individuals that move from 5 to 1 to 3 to 2
    cell1235an <- cell1235am - (cell1235am * m24post) # Individuals that move from 5 to 1 to 3 to 2 but not to 4
    
    #---Senario1 5->2->1->3 ---#
    cell1235ao <- pop5post * m5post * m12post * m13post # Individuals that move from 5 to 2 to 1 to 3
    cell1235ap <- cell1235ao - (cell1235ao * m34post) # Individuals that move from 5 to 2 to 1 to 3 but not to 4
    
    #---Senario1 5->2->3->1 ---#
    cell1235aq <- pop5post * m5post * m23post * m13post # Individuals that move from 5 to 2 to 3 to 1
    cell1235ar <- cell1235aq - (cell1235aq * m14post) # Individuals that move from 5 to 2 to 3 to 1 but not to 4
    
    #---Senario1 5->3->1->2 ---#
    cell1235as <- pop5post * m5post * m13post * m12post # Individuals that move from 5 to 3 to 1 to 2
    cell1235at <- cell1235as - (cell1235as * m24post) # Individuals that move from 5 to 3 to 1 to 2 but not to 4
    
    #---Senario1 5->3->2->1 ---#
    cell1235au <- pop5post * m5post * m23post * m12post # Individuals that move from 5 to 3 to 2 to 1
    cell1235av <- cell1235au - (cell1235au * m14post) # Individuals that move from 5 to 3 to 2 to 1 but not to 4
    
    cell1235 <- cell1235b + cell1235d + cell1235f + cell1235h + cell1235j + cell1235l + cell1235n + cell1235p + cell1235r + cell1235t + cell1235v + cell1235x + cell1235z + cell1235ab + cell1235ad + cell1235af + cell1235ah + cell1235aj + cell1235al + cell1235an + cell1235ap + cell1235ar + cell1235at + cell1235av
    
    
    #---------------------------------#
    # Populations 1, 2, 4, & 5        #
    #---------------------------------#
    
    #---Senario1 1->2->4->5 ---#
    cell1245a <- pop1post * m12post * m24post * m5post # Individuals that move from 1 to 2 to 4 to 5
    cell1245b <- cell1245a - (cell1245a * m5post) # Individuals that move from 1 to 2 to 4 to 5 but not to 3
    
    #---Senario1 1->2->5->4 ---#
    cell1245c <- pop1post * m12post * m5post * m5post # Individuals that move from 1 to 2 to 5 to 4
    cell1245d <- cell1245c - (cell1245c * m34post) # Individuals that move from 1 to 2 to 5 to 4 but not to 3
    
    #---Senario1 1->4->2->5 ---#
    cell1245e <- pop1post * m14post * m24post * m5post # Individuals that move from 1 to 4 to 2 to 5
    cell1245f <- cell1245e - (cell1245e * m5post) # Individuals that move from 1 to 4 to 2 to 5 but not to 3
    
    #---Senario1 1->4->5->2 ---#
    cell1245g <- pop1post * m14post * m5post * m5post # Individuals that move from 1 to 4 to 5 to 2
    cell1245h <- cell1245g - (cell1245g * m23post) # Individuals that move from 1 to 4 to 5 to 2 but not to 3
    
    #---Senario1 1->5->2->4 ---#
    cell1245i <- pop1post * m5post * m5post * m24post # Individuals that move from 1 to 5 to 2 to 4
    cell1245j <- cell1245i - (cell1245i * m34post) # Individuals that move from 1 to 5 to 2 to 4 but not to 3
    
    #---Senario1 1->5->4->2 ---#
    cell1245k <- pop1post * m5post * m5post * m24post # Individuals that move from 1 to 5 to 4 to 2
    cell1245l <- cell1245k - (cell1245k * m23post) # Individuals that move from 1 to 5 to 4 to 2 but not to 3
    
    #---Senario1 2->1->4->5 ---#
    cell1245m <- pop2post * m12post * m14post * m5post # Individuals that move from 2 to 1 to 4 to 5
    cell1245n <- cell1245m - (cell1245m * m5post) # Individuals that move from 2 to 1 to 4 to 5 but not to 3
    
    #---Senario1 2->1->5->4 ---#
    cell1245o <- pop2post * m12post * m5post * m5post # Individuals that move from 2 to 1 to 5 to 4
    cell1245p <- cell1245o - (cell1245o * m34post) # Individuals that move from 2 to 1 to 5 to 4 but not to 3
    
    #---Senario1 2->4->1->5 ---#
    cell1245q <- pop2post * m24post * m14post * m5post # Individuals that move from 2 to 4 to 1 to 5
    cell1245r <- cell1245q - (cell1245q * m5post) # Individuals that move from 2 to 4 to 1 to 5 but not to 3
    
    #---Senario1 2->4->5->1 ---#
    cell1245s <- pop2post * m24post * m5post * m5post # Individuals that move from 2 to 4 to 5 to 1
    cell1245t <- cell1245s - (cell1245s * m13post) # Individuals that move from 2 to 4 to 5 to 1 but not to 3
    
    #---Senario1 2->5->1->4 ---#
    cell1245u <- pop2post * m5post * m5post * m14post # Individuals that move from 2 to 5 to 1 to 4
    cell1245v <- cell1245u - (cell1245u * m34post) # Individuals that move from 2 to 5 to 1 to 4 but not to 3
    
    #---Senario1 2->5->4->1 ---#
    cell1245w <- pop2post * m5post * m5post * m14post # Individuals that move from 2 to 5 to 4 to 1
    cell1245x <- cell1245w - (cell1245w * m13post) # Individuals that move from 2 to 5 to 4 to 1 but not to 3
    
    #---Senario1 4->1->2->5 ---#
    cell1245y <- pop4post * m14post * m12post * m5post # Individuals that move from 4 to 1 to 2 to 5
    cell1245z <- cell1245y - (cell1245y * m5post) # Individuals that move from 4 to 1 to 2 to 5 but not to 3
    
    #---Senario1 4->1->5->2 ---#
    cell1245aa <- pop4post * m14post * m5post * m5post # Individuals that move from 4 to 1 to 5 to 2
    cell1245ab <- cell1245aa - (cell1245aa * m23post) # Individuals that move from 4 to 1 to 5 to 2 but not to 3
    
    #---Senario1 4->2->1->5 ---#
    cell1245ac <- pop4post * m24post * m12post * m5post # Individuals that move from 4 to 2 to 1 to 5
    cell1245ad <- cell1245ac - (cell1245ac * m5post) # Individuals that move from 4 to 2 to 1 to 5 but not to 3
    
    #---Senario1 4->2->5->1 ---#
    cell1245ae <- pop4post * m24post * m5post * m5post # Individuals that move from 4 to 2 to 5 to 1
    cell1245af <- cell1245ae - (cell1245ae * m13post) # Individuals that move from 4 to 2 to 5 to 1 but not to 3
    
    #---Senario1 4->5->1->2 ---#
    cell1245ag <- pop4post * m5post * m5post * m12post # Individuals that move from 4 to 5 to 1 to 2
    cell1245ah <- cell1245ag - (cell1245ag * m23post) # Individuals that move from 4 to 5 to 1 to 2 but not to 3
    
    #---Senario1 4->5->2->1 ---#
    cell1245ai <- pop4post * m5post * m5post * m12post # Individuals that move from 4 to 5 to 2 to 1
    cell1245aj <- cell1245ai - (cell1245ai * m13post) # Individuals that move from 4 to 5 to 2 to 1 but not to 3
    
    #---Senario1 5->1->2->4 ---#
    cell1245ak <- pop5post * m5post * m12post * m24post # Individuals that move from 5 to 1 to 2 to 4
    cell1245al <- cell1245ak - (cell1245ak * m34post) # Individuals that move from 5 to 1 to 2 to 4 but not to 3
    
    #---Senario1 5->1->4->2 ---#
    cell1245am <- pop5post * m5post * m14post * m24post # Individuals that move from 5 to 1 to 4 to 2
    cell1245an <- cell1245am - (cell1245am * m23post) # Individuals that move from 5 to 1 to 4 to 2 but not to 3
    
    #---Senario1 5->2->1->4 ---#
    cell1245ao <- pop5post * m5post * m12post * m14post # Individuals that move from 5 to 2 to 1 to 4
    cell1245ap <- cell1245ao - (cell1245ao * m34post) # Individuals that move from 5 to 2 to 1 to 4 but not to 3
    
    #---Senario1 5->2->4->1 ---#
    cell1245aq <- pop5post * m5post * m24post * m14post # Individuals that move from 5 to 2 to 4 to 1
    cell1245ar <- cell1245aq - (cell1245aq * m13post) # Individuals that move from 5 to 2 to 4 to 1 but not to 3
    
    #---Senario1 5->4->1->2 ---#
    cell1245as <- pop5post * m5post * m14post * m12post # Individuals that move from 5 to 4 to 1 to 2
    cell1245at <- cell1245as - (cell1245as * m23post) # Individuals that move from 5 to 4 to 1 to 2 but not to 3
    
    #---Senario1 5->4->2->1 ---#
    cell1245au <- pop5post * m5post * m24post * m12post # Individuals that move from 5 to 4 to 2 to 1
    cell1245av <- cell1245au - (cell1245au * m13post) # Individuals that move from 5 to 4 to 2 to 1 but not to 3
    
    cell1245 <- cell1245b + cell1245d + cell1245f + cell1245h + cell1245j + cell1245l + cell1245n + cell1245p + cell1245r + cell1245t + cell1245v + cell1245x + cell1245z + cell1245ab + cell1245ad + cell1245af + cell1245ah + cell1245aj + cell1245al + cell1245an + cell1245ap + cell1245ar + cell1245at + cell1245av
    
    
    #---------------------------------#
    # Populations 1, 3, 4, & 5        #
    #---------------------------------#
    
    #---Senario1 1->3->4->5 ---#
    cell1345a <- pop1post * m13post * m34post * m5post # Individuals that move from 1 to 3 to 4 to 5
    cell1345b <- cell1345a - (cell1345a * m5post) # Individuals that move from 1 to 3 to 4 to 5 but not to 2
    
    #---Senario1 1->3->5->4 ---#
    cell1345c <- pop1post * m13post * m5post * m5post # Individuals that move from 1 to 3 to 5 to 4
    cell1345d <- cell1345c - (cell1345c * m24post) # Individuals that move from 1 to 3 to 5 to 4 but not to 2
    
    #---Senario1 1->4->3->5 ---#
    cell1345e <- pop1post * m14post * m34post * m5post # Individuals that move from 1 to 4 to 3 to 5
    cell1345f <- cell1345e - (cell1345e * m5post) # Individuals that move from 1 to 4 to 3 to 5 but not to 2
    
    #---Senario1 1->4->5->3 ---#
    cell1345g <- pop1post * m14post * m5post * m5post # Individuals that move from 1 to 4 to 5 to 3
    cell1345h <- cell1345g - (cell1345g * m23post) # Individuals that move from 1 to 4 to 5 to 3 but not to 2
    
    #---Senario1 1->5->3->4 ---#
    cell1345i <- pop1post * m5post * m5post * m34post # Individuals that move from 1 to 5 to 3 to 4
    cell1345j <- cell1345i - (cell1345i * m24post) # Individuals that move from 1 to 5 to 3 to 4 but not to 2
    
    #---Senario1 1->5->4->3 ---#
    cell1345k <- pop1post * m5post * m5post * m34post # Individuals that move from 1 to 5 to 4 to 3
    cell1345l <- cell1345k - (cell1345k * m23post) # Individuals that move from 1 to 5 to 4 to 3 but not to 2
    
    #---Senario1 3->1->4->5 ---#
    cell1345m <- pop3post * m13post * m14post * m5post # Individuals that move from 3 to 1 to 4 to 5
    cell1345n <- cell1345m - (cell1345m * m5post) # Individuals that move from 3 to 1 to 4 to 5 but not to 2
    
    #---Senario1 3->1->5->4 ---#
    cell1345o <- pop3post * m13post * m5post * m5post # Individuals that move from 3 to 1 to 5 to 4
    cell1345p <- cell1345o - (cell1345o * m24post) # Individuals that move from 3 to 1 to 5 to 4 but not to 2
    
    #---Senario1 3->4->1->5 ---#
    cell1345q <- pop3post * m34post * m14post * m5post # Individuals that move from 3 to 4 to 1 to 5
    cell1345r <- cell1345q - (cell1345q * m5post) # Individuals that move from 3 to 4 to 1 to 5 but not to 2
    
    #---Senario1 3->4->5->1 ---#
    cell1345s <- pop3post * m34post * m5post * m5post # Individuals that move from 3 to 4 to 5 to 1
    cell1345t <- cell1345s - (cell1345s * m12post) # Individuals that move from 3 to 4 to 5 to 1 but not to 2
    
    #---Senario1 3->5->1->4 ---#
    cell1345u <- pop3post * m5post * m5post * m14post # Individuals that move from 3 to 5 to 1 to 4
    cell1345v <- cell1345u - (cell1345u * m24post) # Individuals that move from 3 to 5 to 1 to 4 but not to 2
    
    #---Senario1 3->5->4->1 ---#
    cell1345w <- pop3post * m5post * m5post * m14post # Individuals that move from 3 to 5 to 4 to 1
    cell1345x <- cell1345w - (cell1345w * m12post) # Individuals that move from 3 to 5 to 4 to 1 but not to 2
    
    #---Senario1 4->1->3->5 ---#
    cell1345y <- pop4post * m14post * m13post * m5post # Individuals that move from 4 to 1 to 3 to 5
    cell1345z <- cell1345y - (cell1345y * m5post) # Individuals that move from 4 to 1 to 3 to 5 but not to 2
    
    #---Senario1 4->1->5->3 ---#
    cell1345aa <- pop4post * m14post * m5post * m5post # Individuals that move from 4 to 1 to 5 to 3
    cell1345ab <- cell1345aa - (cell1345aa * m23post) # Individuals that move from 4 to 1 to 5 to 3 but not to 2
    
    #---Senario1 4->3->1->5 ---#
    cell1345ac <- pop4post * m34post * m13post * m5post # Individuals that move from 4 to 3 to 1 to 5
    cell1345ad <- cell1345ac - (cell1345ac * m5post) # Individuals that move from 4 to 3 to 1 to 5 but not to 2
    
    #---Senario1 4->3->5->1 ---#
    cell1345ae <- pop4post * m34post * m5post * m5post # Individuals that move from 4 to 3 to 5 to 1
    cell1345af <- cell1345ae - (cell1345ae * m12post) # Individuals that move from 4 to 3 to 5 to 1 but not to 2
    
    #---Senario1 4->5->1->3 ---#
    cell1345ag <- pop4post * m5post * m5post * m13post # Individuals that move from 4 to 5 to 1 to 3
    cell1345ah <- cell1345ag - (cell1345ag * m23post) # Individuals that move from 4 to 5 to 1 to 3 but not to 2
    
    #---Senario1 4->5->3->1 ---#
    cell1345ai <- pop4post * m5post * m5post * m13post # Individuals that move from 4 to 5 to 3 to 1
    cell1345aj <- cell1345ai - (cell1345ai * m12post) # Individuals that move from 4 to 5 to 3 to 1 but not to 2
    
    #---Senario1 5->1->3->4 ---#
    cell1345ak <- pop5post * m5post * m13post * m34post # Individuals that move from 5 to 1 to 3 to 4
    cell1345al <- cell1345ak - (cell1345ak * m24post) # Individuals that move from 5 to 1 to 3 to 4 but not to 2
    
    #---Senario1 5->1->4->3 ---#
    cell1345am <- pop5post * m5post * m14post * m34post # Individuals that move from 5 to 1 to 4 to 3
    cell1345an <- cell1345am - (cell1345am * m23post) # Individuals that move from 5 to 1 to 4 to 3 but not to 2
    
    #---Senario1 5->3->1->4 ---#
    cell1345ao <- pop5post * m5post * m13post * m14post # Individuals that move from 5 to 3 to 1 to 4
    cell1345ap <- cell1345ao - (cell1345ao * m24post) # Individuals that move from 5 to 3 to 1 to 4 but not to 2
    
    #---Senario1 5->3->4->1 ---#
    cell1345aq <- pop5post * m5post * m34post * m14post # Individuals that move from 5 to 3 to 4 to 1
    cell1345ar <- cell1345aq - (cell1345aq * m12post) # Individuals that move from 5 to 3 to 4 to 1 but not to 2
    
    #---Senario1 5->4->1->3 ---#
    cell1345as <- pop5post * m5post * m14post * m13post # Individuals that move from 5 to 4 to 1 to 3
    cell1345at <- cell1345as - (cell1345as * m23post) # Individuals that move from 5 to 4 to 1 to 3 but not to 2
    
    #---Senario1 5->4->3->1 ---#
    cell1345au <- pop5post * m5post * m34post * m13post # Individuals that move from 5 to 4 to 3 to 1
    cell1345av <- cell1345au - (cell1345au * m12post) # Individuals that move from 5 to 4 to 3 to 1 but not to 2
    
    cell1345 <- cell1345b + cell1345d + cell1345f + cell1345h + cell1345j + cell1345l + cell1345n + cell1345p + cell1345r + cell1345t + cell1345v + cell1345x + cell1345z + cell1345ab + cell1345ad + cell1345af + cell1345ah + cell1345aj + cell1345al + cell1345an + cell1345ap + cell1345ar + cell1345at + cell1345av
    
    
    #---------------------------------#
    # Populations 2, 3, 4, & 5        #
    #---------------------------------#
    
    #---Senario1 2->3->4->5 ---#
    cell2345a <- pop2post * m23post * m34post * m5post # Individuals that move from 2 to 3 to 4 to 5
    cell2345b <- cell2345a - (cell2345a * m5post) # Individuals that move from 2 to 3 to 4 to 5 but not to 1
    
    #---Senario1 2->3->5->4 ---#
    cell2345c <- pop2post * m23post * m5post * m5post # Individuals that move from 2 to 3 to 5 to 4
    cell2345d <- cell2345c - (cell2345c * m14post) # Individuals that move from 2 to 3 to 5 to 4 but not to 1
    
    #---Senario1 2->4->3->5 ---#
    cell2345e <- pop2post * m24post * m34post * m5post # Individuals that move from 2 to 4 to 3 to 5
    cell2345f <- cell2345e - (cell2345e * m5post) # Individuals that move from 2 to 4 to 3 to 5 but not to 1
    
    #---Senario1 2->4->5->3 ---#
    cell2345g <- pop2post * m24post * m5post * m5post # Individuals that move from 2 to 4 to 5 to 3
    cell2345h <- cell2345g - (cell2345g * m13post) # Individuals that move from 2 to 4 to 5 to 3 but not to 1
    
    #---Senario1 2->5->3->4 ---#
    cell2345i <- pop2post * m5post * m5post * m34post # Individuals that move from 2 to 5 to 3 to 4
    cell2345j <- cell2345i - (cell2345i * m14post) # Individuals that move from 2 to 5 to 3 to 4 but not to 1
    
    #---Senario1 2->5->4->3 ---#
    cell2345k <- pop2post * m5post * m5post * m34post # Individuals that move from 2 to 5 to 4 to 3
    cell2345l <- cell2345k - (cell2345k * m13post) # Individuals that move from 2 to 5 to 4 to 3 but not to 1
    
    #---Senario1 3->2->4->5 ---#
    cell2345m <- pop3post * m23post * m24post * m5post # Individuals that move from 3 to 2 to 4 to 5
    cell2345n <- cell2345m - (cell2345m * m5post) # Individuals that move from 3 to 2 to 4 to 5 but not to 1
    
    #---Senario1 3->2->5->4 ---#
    cell2345o <- pop3post * m23post * m5post * m5post # Individuals that move from 3 to 2 to 5 to 4
    cell2345p <- cell2345o - (cell2345o * m14post) # Individuals that move from 3 to 2 to 5 to 4 but not to 1
    
    #---Senario1 3->4->2->5 ---#
    cell2345q <- pop3post * m34post * m24post * m5post # Individuals that move from 3 to 4 to 2 to 5
    cell2345r <- cell2345q - (cell2345q * m5post) # Individuals that move from 3 to 4 to 2 to 5 but not to 1
    
    #---Senario1 3->4->5->2 ---#
    cell2345s <- pop3post * m34post * m5post * m5post # Individuals that move from 3 to 4 to 5 to 2
    cell2345t <- cell2345s - (cell2345s * m12post) # Individuals that move from 3 to 4 to 5 to 2 but not to 1
    
    #---Senario1 3->5->2->4 ---#
    cell2345u <- pop3post * m5post * m5post * m24post # Individuals that move from 3 to 5 to 2 to 4
    cell2345v <- cell2345u - (cell2345u * m14post) # Individuals that move from 3 to 5 to 2 to 4 but not to 1
    
    #---Senario1 3->5->4->2 ---#
    cell2345w <- pop3post * m5post * m5post * m24post # Individuals that move from 3 to 5 to 4 to 2
    cell2345x <- cell2345w - (cell2345w * m12post) # Individuals that move from 3 to 5 to 4 to 2 but not to 1
    
    #---Senario1 4->2->3->5 ---#
    cell2345y <- pop4post * m24post * m23post * m5post # Individuals that move from 4 to 2 to 3 to 5
    cell2345z <- cell2345y - (cell2345y * m5post) # Individuals that move from 4 to 2 to 3 to 5 but not to 1
    
    #---Senario1 4->2->5->3 ---#
    cell2345aa <- pop4post * m24post * m5post * m5post # Individuals that move from 4 to 2 to 5 to 3
    cell2345ab <- cell2345aa - (cell2345aa * m13post) # Individuals that move from 4 to 2 to 5 to 3 but not to 1
    
    #---Senario1 4->3->2->5 ---#
    cell2345ac <- pop4post * m34post * m23post * m5post # Individuals that move from 4 to 3 to 2 to 5
    cell2345ad <- cell2345ac - (cell2345ac * m5post) # Individuals that move from 4 to 3 to 2 to 5 but not to 1
    
    #---Senario1 4->3->5->2 ---#
    cell2345ae <- pop4post * m34post * m5post * m5post # Individuals that move from 4 to 3 to 5 to 2
    cell2345af <- cell2345ae - (cell2345ae * m12post) # Individuals that move from 4 to 3 to 5 to 2 but not to 1
    
    #---Senario1 4->5->2->3 ---#
    cell2345ag <- pop4post * m5post * m5post * m23post # Individuals that move from 4 to 5 to 2 to 3
    cell2345ah <- cell2345ag - (cell2345ag * m13post) # Individuals that move from 4 to 5 to 2 to 3 but not to 1
    
    #---Senario1 4->5->3->2 ---#
    cell2345ai <- pop4post * m5post * m5post * m23post # Individuals that move from 4 to 5 to 3 to 2
    cell2345aj <- cell2345ai - (cell2345ai * m12post) # Individuals that move from 4 to 5 to 3 to 2 but not to 1
    
    #---Senario1 5->2->3->4 ---#
    cell2345ak <- pop5post * m5post * m23post * m34post # Individuals that move from 5 to 2 to 3 to 4
    cell2345al <- cell2345ak - (cell2345ak * m14post) # Individuals that move from 5 to 2 to 3 to 4 but not to 1
    
    #---Senario1 5->2->4->3 ---#
    cell2345am <- pop5post * m5post * m24post * m34post # Individuals that move from 5 to 2 to 4 to 3
    cell2345an <- cell2345am - (cell2345am * m13post) # Individuals that move from 5 to 2 to 4 to 3 but not to 1
    
    #---Senario1 5->3->2->4 ---#
    cell2345ao <- pop5post * m5post * m23post * m24post # Individuals that move from 5 to 3 to 2 to 4
    cell2345ap <- cell2345ao - (cell2345ao * m14post) # Individuals that move from 5 to 3 to 2 to 4 but not to 1
    
    #---Senario1 5->3->4->2 ---#
    cell2345aq <- pop5post * m5post * m34post * m24post # Individuals that move from 5 to 3 to 4 to 2
    cell2345ar <- cell2345aq - (cell2345aq * m12post) # Individuals that move from 5 to 3 to 4 to 2 but not to 1
    
    #---Senario1 5->4->2->3 ---#
    cell2345as <- pop5post * m5post * m24post * m23post # Individuals that move from 5 to 4 to 2 to 3
    cell2345at <- cell2345as - (cell2345as * m13post) # Individuals that move from 5 to 4 to 2 to 3 but not to 1
    
    #---Senario1 5->4->3->2 ---#
    cell2345au <- pop5post * m5post * m34post * m23post # Individuals that move from 5 to 4 to 3 to 2
    cell2345av <- cell2345au - (cell2345au * m12post) # Individuals that move from 5 to 4 to 3 to 2 but not to 1
    
    cell2345 <- cell2345b + cell2345d + cell2345f + cell2345h + cell2345j + cell2345l + cell2345n + cell2345p + cell2345r + cell2345t + cell2345v + cell2345x + cell2345z + cell2345ab + cell2345ad + cell2345af + cell2345ah + cell2345aj + cell2345al + cell2345an + cell2345ap + cell2345ar + cell2345at + cell2345av
    
    
    #---------------------------------------------------#
    # Population 1, 2, 3, 4, & 5                        #
    # Note that there are 120 possible combinations (!),#
    # 24 for each starting population
    #---------------------------------------------------#
    
    
    ### Starting with pop1 ###
    
    #--- Scenario 1: 1->2->3->4->5 ---#
    cell12345a1 <- pop1post * m12post * m23post * m34post * m5post
    
    #--- Scenario 2: 1->2->3->5->4 ---#
    cell12345a2 <- pop1post * m12post * m23post * m5post * m5post
    
    #--- Scenario 3: 1->2->5->3->4 ---#
    cell12345a3 <- pop1post * m12post * m5post * m5post * m34post
    
    #--- Scenario 4: 1->2->5->4->3 ---#
    cell12345a4 <- pop1post * m12post * m5post * m5post * m34post
    
    #--- Scenario 5: 1->5->2->3->4 ---#
    cell12345a5 <- pop1post * m5post * m5post * m23post * m34post
    
    #--- Scenario 6: 1->5->2->4->3 ---#
    cell12345a6 <- pop1post * m5post * m5post * m24post * m34post
    
    #--- Scenario 7: 1->5->3->2->4 ---#
    cell12345a7 <- pop1post * m5post * m5post * m23post * m24post
    
    #--- Scenario 8: 1->5->3->4->2 ---#
    cell12345a8 <- pop1post * m5post * m5post * m34post * m24post
    
    #--- Scenario 9: 1->5->4->2->3 ---#
    cell12345a9 <- pop1post * m5post * m5post * m24post * m23post
    
    #--- Scenario 10: 1->5->4->3->2 ---#
    cell12345a10 <- pop1post * m5post * m5post * m34post * m23post
    
    #--- Scenario 11: 1->2->4->3->5 ---#
    cell12345a11 <- pop1post * m12post * m24post * m34post * m5post
    
    #--- Scenario 12: 1->2->4->5->3 ---#
    cell12345a12 <- pop1post * m12post * m24post * m5post * m5post
    
    #--- Scenario 13: 1->4->2->3->5 ---#
    cell12345a13 <- pop1post * m14post * m24post * m23post * m5post
    
    #--- Scenario 14: 1->4->2->5->3 ---#
    cell12345a14 <- pop1post * m14post * m24post * m5post * m5post
    
    #--- Scenario 15: 1->4->3->5->2 ---#
    cell12345a15 <- pop1post * m14post * m34post * m5post * m5post

    #--- Scenario 16: 1->4->3->2->5 ---#
    cell12345a16 <- pop1post * m14post * m34post * m23post * m5post

    #--- Scenario 17: 1->4->5->2->3 ---#
    cell12345a17 <- pop1post * m14post * m5post * m5post * m23post

    #--- Scenario 18: 1->4->5->3->2 ---#
    cell12345a18 <- pop1post * m14post * m5post * m5post * m23post

    #--- Scenario 19: 1->3->2->4->5 ---#
    cell12345a19 <- pop1post * m13post * m23post * m24post * m5post
    
    #--- Scenario 20: 1->3->2->5->4 ---#
    cell12345a20 <- pop1post * m13post * m23post * m5post * m5post
    
    #--- Scenario 21: 1->3->4->2->5 ---#
    cell12345a21 <- pop1post * m13post * m34post * m24post * m5post
    
    #--- Scenario 22: 1->3->4->5->2 ---#
    cell12345a22 <- pop1post * m13post * m34post * m5post * m5post
    
    #--- Scenario 23: 1->3->5->2->4 ---#
    cell12345a23 <- pop1post * m13post * m5post * m5post * m24post
    
    #--- Scenario 24: 1->3->5->4->2 ---#
    cell12345a24 <- pop1post * m13post * m5post * m5post * m24post
    
    
    ### Starting with pop2 ###
    
    #--- Scenario 1: 2->1->3->4->5 ---#
    cell12345b1 <- pop2post * m12post * m13post * m34post * m5post
    
    #--- Scenario 2: 2->1->3->5->4 ---#
    cell12345b2 <- pop2post * m12post * m13post * m5post * m5post
    
    #--- Scenario 3: 2->1->5->3->4 ---#
    cell12345b3 <- pop2post * m12post * m5post * m5post * m34post
    
    #--- Scenario 4: 2->1->5->4->3 ---#
    cell12345b4 <- pop2post * m12post * m5post * m5post * m34post
    
    #--- Scenario 5: 2->5->1->3->4 ---#
    cell12345b5 <- pop2post * m5post * m5post * m13post * m34post
    
    #--- Scenario 6: 2->5->1->4->3 ---#
    cell12345b6 <- pop2post * m5post * m5post * m14post * m34post
    
    #--- Scenario 7: 2->5->3->1->4 ---#
    cell12345b7 <- pop2post * m5post * m5post * m13post * m14post
    
    #--- Scenario 8: 2->5->3->4->1 ---#
    cell12345b8 <- pop2post * m5post * m5post * m34post * m14post
    
    #--- Scenario 9: 2->5->4->1->3 ---#
    cell12345b9 <- pop2post * m5post * m5post * m14post * m13post
    
    #--- Scenario 10: 2->5->4->3->1 ---#
    cell12345b10 <- pop2post * m5post * m5post * m34post * m13post
    
    #--- Scenario 11: 2->1->4->3->5 ---#
    cell12345b11 <- pop2post * m12post * m14post * m34post * m5post
    
    #--- Scenario 12: 2->1->4->5->3 ---#
    cell12345b12 <- pop2post * m12post * m14post * m5post * m5post
    
    #--- Scenario 13: 2->4->1->3->5 ---#
    cell12345b13 <- pop2post * m24post * m14post * m13post * m5post
    
    #--- Scenario 14: 2->4->1->5->3 ---#
    cell12345b14 <- pop2post * m24post * m14post * m5post * m5post
    
    #--- Scenario 15: 2->4->3->5->1 ---#
    cell12345b15 <- pop2post * m24post * m34post * m5post * m5post
    
    #--- Scenario 16: 2->4->3->1->5 ---#
    cell12345b16 <- pop2post * m24post * m34post * m13post * m5post
    
    #--- Scenario 17: 2->4->5->1->3 ---#
    cell12345b17 <- pop2post * m24post * m5post * m5post * m13post
    
    #--- Scenario 18: 2->4->5->3->1 ---#
    cell12345b18 <- pop2post * m24post * m5post * m5post * m13post
    
    #--- Scenario 19: 2->3->1->4->5 ---#
    cell12345b19 <- pop2post * m23post * m13post * m14post * m5post
    
    #--- Scenario 20: 2->3->1->5->4 ---#
    cell12345b20 <- pop2post * m23post * m13post * m5post * m5post
    
    #--- Scenario 21: 2->3->4->1->5 ---#
    cell12345b21 <- pop2post * m23post * m34post * m14post * m5post
    
    #--- Scenario 22: 2->3->4->5->1 ---#
    cell12345b22 <- pop2post * m23post * m34post * m5post * m5post
    
    #--- Scenario 23: 2->3->5->1->4 ---#
    cell12345b23 <- pop2post * m23post * m5post * m5post * m14post
    
    #--- Scenario 24: 2->3->5->4->1 ---#
    cell12345b24 <- pop2post * m23post * m5post * m5post * m14post
    
    
    ### Starting with pop3 ###
    
    #--- Scenario 1: 3->1->2->4->5 ---#
    cell12345c1 <- pop3post * m13post * m12post * m24post * m5post
    
    #--- Scenario 2: 3->1->2->5->4 ---#
    cell12345c2 <- pop3post * m13post * m12post * m5post * m5post
    
    #--- Scenario 3: 3->1->5->2->4 ---#
    cell12345c3 <- pop3post * m13post * m5post * m5post * m24post
    
    #--- Scenario 4: 3->1->5->4->2 ---#
    cell12345c4 <- pop3post * m13post * m5post * m5post * m24post
    
    #--- Scenario 5: 3->5->1->2->4 ---#
    cell12345c5 <- pop3post * m5post * m5post * m12post * m24post
    
    #--- Scenario 6: 3->5->1->4->2 ---#
    cell12345c6 <- pop3post * m5post * m5post * m14post * m24post
    
    #--- Scenario 7: 3->5->2->1->4 ---#
    cell12345c7 <- pop3post * m5post * m5post * m12post * m14post
    
    #--- Scenario 8: 3->5->2->4->1 ---#
    cell12345c8 <- pop3post * m5post * m5post * m24post * m14post
    
    #--- Scenario 9: 3->5->4->1->2 ---#
    cell12345c9 <- pop3post * m5post * m5post * m14post * m12post
    
    #--- Scenario 10: 3->5->4->2->1 ---#
    cell12345c10 <- pop3post * m5post * m5post * m24post * m12post
    
    #--- Scenario 11: 3->1->4->2->5 ---#
    cell12345c11 <- pop3post * m13post * m14post * m24post * m5post
    
    #--- Scenario 12: 3->1->4->5->2 ---#
    cell12345c12 <- pop3post * m13post * m14post * m5post * m5post
    
    #--- Scenario 13: 3->4->1->2->5 ---#
    cell12345c13 <- pop3post * m34post * m14post * m12post * m5post
    
    #--- Scenario 14: 3->4->1->5->2 ---#
    cell12345c14 <- pop3post * m34post * m14post * m5post * m5post
    
    #--- Scenario 15: 3->4->2->5->1 ---#
    cell12345c15 <- pop3post * m34post * m24post * m5post * m5post
    
    #--- Scenario 16: 3->4->2->1->5 ---#
    cell12345c16 <- pop3post * m34post * m24post * m12post * m5post
    
    #--- Scenario 17: 3->4->5->1->2 ---#
    cell12345c17 <- pop3post * m34post * m5post * m5post * m12post
    
    #--- Scenario 18: 3->4->5->2->1 ---#
    cell12345c18 <- pop3post * m34post * m5post * m5post * m12post
    
    #--- Scenario 19: 3->2->1->4->5 ---#
    cell12345c19 <- pop3post * m23post * m12post * m14post * m5post
    
    #--- Scenario 20: 3->2->1->5->4 ---#
    cell12345c20 <- pop3post * m23post * m12post * m5post * m5post
    
    #--- Scenario 21: 3->2->4->1->5 ---#
    cell12345c21 <- pop3post * m23post * m24post * m14post * m5post
    
    #--- Scenario 22: 3->2->4->5->1 ---#
    cell12345c22 <- pop3post * m23post * m24post * m5post * m5post
    
    #--- Scenario 23: 3->2->5->1->4 ---#
    cell12345c23 <- pop3post * m23post * m5post * m5post * m14post
    
    #--- Scenario 24: 3->2->5->4->1 ---#
    cell12345c24 <- pop3post * m23post * m5post * m5post * m14post
    
    
    ### Starting with pop4 ###
    
    #--- Scenario 1: 4->1->2->3->5 ---#
    cell12345d1 <- pop4post * m14post * m12post * m23post * m5post
    
    #--- Scenario 2: 4->1->2->5->3 ---#
    cell12345d2 <- pop4post * m14post * m12post * m5post * m5post
    
    #--- Scenario 3: 4->1->5->2->3 ---#
    cell12345d3 <- pop4post * m14post * m5post * m5post * m23post
    
    #--- Scenario 4: 4->1->5->3->2 ---#
    cell12345d4 <- pop4post * m14post * m5post * m5post * m23post
    
    #--- Scenario 5: 4->5->1->2->3 ---#
    cell12345d5 <- pop4post * m5post * m5post * m12post * m23post
    
    #--- Scenario 6: 4->5->1->3->2 ---#
    cell12345d6 <- pop4post * m5post * m5post * m13post * m23post
    
    #--- Scenario 7: 4->5->2->1->3 ---#
    cell12345d7 <- pop4post * m5post * m5post * m12post * m13post
    
    #--- Scenario 8: 4->5->2->3->1 ---#
    cell12345d8 <- pop4post * m5post * m5post * m23post * m13post
    
    #--- Scenario 9: 4->5->3->1->2 ---#
    cell12345d9 <- pop4post * m5post * m5post * m13post * m12post
    
    #--- Scenario 10: 4->5->3->2->1 ---#
    cell12345d10 <- pop4post * m5post * m5post * m23post * m12post
    
    #--- Scenario 11: 4->1->3->2->5 ---#
    cell12345d11 <- pop4post * m14post * m13post * m23post * m5post
    
    #--- Scenario 12: 4->1->3->5->2 ---#
    cell12345d12 <- pop4post * m14post * m13post * m5post * m5post
    
    #--- Scenario 13: 4->3->1->2->5 ---#
    cell12345d13 <- pop4post * m34post * m13post * m12post * m5post
    
    #--- Scenario 14: 4->3->1->5->2 ---#
    cell12345d14 <- pop4post * m34post * m13post * m5post * m5post
    
    #--- Scenario 15: 4->3->2->5->1 ---#
    cell12345d15 <- pop4post * m34post * m23post * m5post * m5post
    
    #--- Scenario 16: 4->3->2->1->5 ---#
    cell12345d16 <- pop4post * m34post * m23post * m12post * m5post
    
    #--- Scenario 17: 4->3->5->1->2 ---#
    cell12345d17 <- pop4post * m34post * m5post * m5post * m12post
    
    #--- Scenario 18: 4->3->5->2->1 ---#
    cell12345d18 <- pop4post * m34post * m5post * m5post * m12post
    
    #--- Scenario 19: 4->2->1->3->5 ---#
    cell12345d19 <- pop4post * m24post * m12post * m13post * m5post
    
    #--- Scenario 20: 4->2->1->5->3 ---#
    cell12345d20 <- pop4post * m24post * m12post * m5post * m5post
    
    #--- Scenario 21: 4->2->3->1->5 ---#
    cell12345d21 <- pop4post * m24post * m23post * m13post * m5post
    
    #--- Scenario 22: 4->2->3->5->1 ---#
    cell12345d22 <- pop4post * m24post * m23post * m5post * m5post
    
    #--- Scenario 23: 4->2->5->1->3 ---#
    cell12345d23 <- pop4post * m24post * m5post * m5post * m13post
    
    #--- Scenario 24: 4->2->5->3->1 ---#
    cell12345d24 <- pop4post * m24post * m5post * m5post * m13post
    
    
    ### Starting with pop5 ###
    
    #--- Scenario 1: 5->1->2->3->4 ---#
    cell12345e1 <- pop5post * m5post * m12post * m23post * m34post
    
    #--- Scenario 2: 5->1->2->4->3 ---#
    cell12345e2 <- pop5post * m5post * m12post * m24post * m34post
    
    #--- Scenario 3: 5->1->4->2->3 ---#
    cell12345e3 <- pop5post * m5post * m14post * m24post * m23post
    
    #--- Scenario 4: 5->1->4->3->2 ---#
    cell12345e4 <- pop5post * m5post * m14post * m34post * m23post
    
    #--- Scenario 5: 5->4->1->2->3 ---#
    cell12345e5 <- pop5post * m5post * m14post * m12post * m23post
    
    #--- Scenario 6: 5->4->1->3->2 ---#
    cell12345e6 <- pop5post * m5post * m14post * m13post * m23post
    
    #--- Scenario 7: 5->4->2->1->3 ---#
    cell12345e7 <- pop5post * m5post * m24post * m12post * m13post
    
    #--- Scenario 8: 5->4->2->3->1 ---#
    cell12345e8 <- pop5post * m5post * m24post * m23post * m13post
    
    #--- Scenario 9: 5->4->3->1->2 ---#
    cell12345e9 <- pop5post * m5post * m34post * m13post * m12post
    
    #--- Scenario 10: 5->4->3->2->1 ---#
    cell12345e10 <- pop5post * m5post * m34post * m23post * m12post
    
    #--- Scenario 11: 5->1->3->2->4 ---#
    cell12345e11 <- pop5post * m5post * m13post * m23post * m24post
    
    #--- Scenario 12: 5->1->3->4->2 ---#
    cell12345e12 <- pop5post * m5post * m13post * m34post * m24post
    
    #--- Scenario 13: 5->3->1->2->4 ---#
    cell12345e13 <- pop5post * m5post * m13post * m12post * m24post
    
    #--- Scenario 14: 5->3->1->4->2 ---#
    cell12345e14 <- pop5post * m5post * m13post * m14post * m24post
    
    #--- Scenario 15: 5->3->2->4->1 ---#
    cell12345e15 <- pop5post * m5post * m23post * m24post * m14post
    
    #--- Scenario 16: 5->3->2->1->4 ---#
    cell12345e16 <- pop5post * m5post * m23post * m12post * m14post
    
    #--- Scenario 17: 5->3->4->1->2 ---#
    cell12345e17 <- pop5post * m5post * m34post * m14post * m12post
    
    #--- Scenario 18: 5->3->4->2->1 ---#
    cell12345e18 <- pop5post * m5post * m34post * m24post * m12post
    
    #--- Scenario 19: 5->2->1->3->4 ---#
    cell12345e19 <- pop5post * m5post * m12post * m13post * m34post
    
    #--- Scenario 20: 5->2->1->4->3 ---#
    cell12345e20 <- pop5post * m5post * m12post * m14post * m34post
    
    #--- Scenario 21: 5->2->3->1->4 ---#
    cell12345e21 <- pop5post * m5post * m23post * m13post * m14post
    
    #--- Scenario 22: 5->2->3->4->1 ---#
    cell12345e22 <- pop5post * m5post * m23post * m34post * m14post
    
    #--- Scenario 23: 5->2->4->1->3 ---#
    cell12345e23 <- pop5post * m5post * m24post * m14post * m13post
    
    #--- Scenario 24: 5->2->4->3->1 ---#
    cell12345e24 <- pop5post * m5post * m24post * m34post * m13post
    
    cell12345 <- (cell12345a1 + cell12345a2 + cell12345a3 + cell12345a4 + cell12345a5 + cell12345a6 + cell12345a7 + cell12345a8 + cell12345a9 + cell12345a10 + cell12345a11 + cell12345a12 + cell12345a13 + cell12345a14 + cell12345a15 + cell12345a16 + cell12345a17 + cell12345a18 + cell12345a19 + cell12345a20 + cell12345a21 + cell12345a22 + cell12345a23 + cell12345a24 + cell12345b1 + cell12345b2 + cell12345b3 + cell12345b4 + cell12345b5 + cell12345b6 + cell12345b7 + cell12345b8 + cell12345b9 + cell12345b10 + cell12345b11 + cell12345b12 + cell12345b13 + cell12345b14 + cell12345b15 + cell12345b16 + cell12345b17 + cell12345b18 + cell12345b19 + cell12345b20 + cell12345b21 + cell12345b22 + cell12345b23 + cell12345b24 + cell12345c1 + cell12345c2 + cell12345c3 + cell12345c4 + cell12345c5 + cell12345c6 + cell12345c7 + cell12345c8 + cell12345c9 + cell12345c10 + cell12345c11 + cell12345c12 + cell12345c13 + cell12345c14 + cell12345c15 + cell12345c16 + cell12345c17 + cell12345c18 + cell12345c19 + cell12345c20 + cell12345c21 + cell12345c22 + cell12345c23 + cell12345c24 + cell12345d1 + cell12345d2 + cell12345d3 + cell12345d4 + cell12345d5 + cell12345d6 + cell12345d7 + cell12345d8 + cell12345d9 + cell12345d10 + cell12345d11 + cell12345d12 + cell12345d13 + cell12345d14 + cell12345d15 + cell12345d16 + cell12345d17 + cell12345d18 + cell12345d19 + cell12345d20 + cell12345d21 + cell12345d22 + cell12345d23 + cell12345d24 + cell12345e1 + cell12345e2 + cell12345e3 + cell12345e4 + cell12345e5 + cell12345e6 + cell12345e7 + cell12345e8 + cell12345e9 + cell12345e10 + cell12345e11 + cell12345e12 + cell12345e13 + cell12345e14 + cell12345e15 + cell12345e16 + cell12345e17 + cell12345e18 + cell12345e19 + cell12345a20 + cell12345e21 + cell12345e22 + cell12345e23 + cell12345e24)


    totalpop <- (cell1 + cell2 + cell3 + cell4 + cell5 + cell12 + cell13 + cell14 + cell15 + cell23 + cell24 + cell25 + cell34 + cell35 + cell45 + cell123 + cell124 + cell125 + cell134 + cell135 + cell145 + cell234 + cell235 + cell245 + cell345 + cell1234 + cell1235 + cell1245 + cell1345 + cell2345 + cell12345)

    #---------------------------------#
    # Plot results                    #
    #---------------------------------#
    par(mfrow = c(1, 1))
    source("plotPost.R")
    histinfo = plotPost(totalpop, xlab = "Total Abundance", showMode = TRUE)

    return(totalpop)
}

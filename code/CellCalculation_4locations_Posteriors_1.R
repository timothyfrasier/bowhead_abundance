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

cellcounts <- function(pop1post, pop2post, pop3post, pop4post, m12post, m13post, m14post, m23post, m24post, m34post, m5post) {

    #---------------------------------#
    # Just in location 1              #
    #---------------------------------#
    cell1 <- pop1post - (pop1post * m12post) - (pop1post * m13post) - (pop1post * m14post) # Individuals from location 1 that don't move anywhere else

    #---------------------------------#
    # Just in location 2              #
    #---------------------------------#
    cell2 <- pop2post - (pop2post * m12post) - (pop2post * m23post) - (pop2post * m24post) # Individuals from location 2 that don't move anywhere else

    #---------------------------------#
    # Just in location 3              #
    #---------------------------------#
    cell3 <- pop3post - (pop3post * m13post) - (pop3post * m23post) - (pop3post * m34post) # Individuals from location 3 that don't move anywhere else

    #---------------------------------#
    # Just in location 4              #
    #---------------------------------#
    cell4 <- pop4post - (pop4post * m14post) - (pop4post * m24post) - (pop4post * m34post) # Individuals from location 4 that don't move anywhere else

    #---------------------------------#
    # Population 1 & 2                #
    #---------------------------------#
    cell12y <- pop1post * m12post
    cell12a <- cell12y - (cell12y * m23post) - (cell12y * m24post)  # Pop1 individuals that go to 2 but not 3 or 4
    cell12z <- pop2post * m12post
    cell12b <- cell12z - (cell12z * m13post) - (cell12z * m14post)  # Pop2 individuals that go to 1 but not 3 or 4
    cell12 <- cell12a + cell12b

    #---------------------------------#
    # Population 1 & 3                #
    #---------------------------------#
    cell13y <- pop1post * m13post
    cell13a <- cell13y - (cell13y * m23post) - (cell13y * m34post)  # Pop1 individuals that go to 3 but not 2 or 4
    cell13z <- pop3post * m13post
    cell13b <- cell13z - (cell13z * m12post) - (cell13z * m14post)  # Pop3 individuals that go to 1 but not 2 or 4
    cell13 <- cell13a + cell13b

    #---------------------------------#
    # Population 1 & 4                #
    #---------------------------------#
    cell14y <- pop1post * m14post
    cell14a <- cell14y - (cell14y * m24post) - (cell14y * m34post) # Pop1 individuals that go to 4 but not 2 or 3
    cell14z <- pop4post * m14post
    cell14b <- cell14z - (cell14z * m12post) - (cell14z * m13post) # Pop4 individuals that go to 1 but not 2 or 3
    cell14 <- cell14a + cell14b
    
    #---------------------------------#
    # Population 2 & 3                #
    #---------------------------------#
    cell23y <- pop2post * m23post
    cell23a <- cell23y - (cell23y * m13post) - (cell23y * m34post)  # Pop2 individuals that go to 3 but not 1 or 4
    cell23z <- pop3post * m23post
    cell23b <- cell23z - (cell23z * m12post) - (cell23z * m24post)  # Pop3 individuals that go to 2 but not 1 or 4
    cell23 <- cell23a + cell23b

    #---------------------------------#
    # Population 2 & 4                #
    #---------------------------------#
    cell24y <- pop2post * m24post
    cell24a <- cell24y - (cell24y * m14post) - (cell24y * m34post)  # Pop2 individuals that go to 4 but not 1 or 3
    cell24z <- pop4post * m24post
    cell24b <- cell24z - (cell24z * m12post) - (cell24z * m23post)  # Pop4 individuals that go to 2 but not 1 or 3
    cell24 <- cell24a + cell24b

    #---------------------------------#
    # Population 3 & 4                #
    #---------------------------------#
    cell34y <- pop3post * m34post
    cell34a <- cell34y - (cell34y * m14post) - (cell34y * m24post)  # Pop3 individuals that go to 4 but not 1 or 2
    cell34z <- pop4post * m34post
    cell34b <- cell34z - (cell34z * m13post) - (cell34z * m23post)  # Pop4 individuals that go to 3 but not 1 or 2
    cell34 <- cell34a + cell34b
    
    
    #---------------------------------#
    # Populations 1, 2, 3             #
    #---------------------------------#
    
    #---Scenario1 1->2->3 ---#
    cell123a <- pop1post * m12post * m23post # Individuals that move from 1 to 2 to 3
    cell123b <- cell123a - (cell123a * m34post) # Individuals that move from 1 to 2 to 3 but not to 4
    
    #---Scenario1 1->3->2 ---#
    cell123c <- pop1post * m13post * m23post # Individuals that move from 1 to 3 to 2
    cell123d <- cell123c - (cell123c * m24post) # Individuals that move from 1 to 3 to 2 but not to 4

    #---Scenario1 2->1->3 ---#
    cell123e <- pop2post * m12post * m13post # Individuals that move from 2 to 1 to 3
    cell123f <- cell123e - (cell123e * m34post) # Individuals that move from 2 to 1 to 3 but not to 4
    
    #---Scenario1 2->3->1 ---#
    cell123g <- pop2post * m23post * m13post # Individuals that move from 2 to 3 to 1
    cell123h <- cell123g - (cell123g * m14post) # Individuals that move from 2 to 3 to 1 but not to 4

    #---Scenario1 3->1->2 ---#
    cell123i <- pop3post * m13post * m12post # Individuals that move from 3 to 1 to 2
    cell123j <- cell123i - (cell123i * m24post) # Individuals that move from 3 to 1 to 2 but not to 4
    
    #---Scenario1 3->2->1 ---#
    cell123k <- pop3post * m23post * m12post # Individuals that move from 3 to 2 to 1
    cell123l <- cell123k - (cell123k * m14post) # Individuals that move from 3 to 2 to 1 but not to 4
    
    cell123 <- cell123b + cell123d + cell123f + cell123h + cell123j + cell123l
    
    
    #---------------------------------#
    # Populations 1, 2, 4             #
    #---------------------------------#
    
    #---Scenario1 1->2->4 ---#
    cell124a <- pop1post * m12post * m24post # Individuals that move from 1 to 2 to 4
    cell124b <- cell124a - (cell124a * m34post) # Individuals that move from 1 to 2 to 4 but not to 3
    
    #---Scenario1 1->4->2 ---#
    cell124c <- pop1post * m14post * m24post # Individuals that move from 1 to 4 to 2
    cell124d <- cell124c - (cell124c * m23post) # Individuals that move from 1 to 4 to 2 but not to 3
    
    #---Scenario1 2->1->4 ---#
    cell124e <- pop2post * m12post * m14post # Individuals that move from 2 to 1 to 4
    cell124f <- cell124e - (cell124e * m34post) # Individuals that move from 2 to 1 to 4 but not to 3
    
    #---Scenario1 2->4->1 ---#
    cell124g <- pop2post * m24post * m14post # Individuals that move from 2 to 4 to 1
    cell124h <- cell124g - (cell124g * m13post) # Individuals that move from 2 to 4 to 1 but not to 3
    
    #---Scenario1 4->1->2 ---#
    cell124i <- pop4post * m14post * m12post # Individuals that move from 4 to 1 to 2
    cell124j <- cell124i - (cell124i * m23post) # Individuals that move from 4 to 1 to 2 but not to 3
    
    #---Scenario1 4->2->1 ---#
    cell124k <- pop4post * m24post * m12post # Individuals that move from 4 to 2 to 1
    cell124l <- cell124k - (cell124k * m13post) # Individuals that move from 4 to 2 to 1 but not to 3
    
    cell124 <- cell124b + cell124d + cell124f + cell124h + cell124j + cell124l

    #---------------------------------#
    # Populations 1, 3, 4             #
    #---------------------------------#
    
    #---Scenario1 1->3->4 ---#
    cell134a <- pop1post * m13post * m34post # Individuals that move from 1 to 3 to 4
    cell134b <- cell134a - (cell134a * m24post) # Individuals that move from 1 to 3 to 4 but not to 2
    
    #---Scenario1 1->4->3 ---#
    cell134c <- pop1post * m14post * m34post # Individuals that move from 1 to 4 to 3
    cell134d <- cell134c - (cell134c * m23post) # Individuals that move from 1 to 4 to 3 but not to 2
    
    #---Scenario1 3->1->4 ---#
    cell134e <- pop3post * m13post * m14post # Individuals that move from 3 to 1 to 4
    cell134f <- cell134e - (cell134e * m24post) # Individuals that move from 3 to 1 to 4 but not to 2
    
    #---Scenario1 3->4->1 ---#
    cell134g <- pop3post * m34post * m14post # Individuals that move from 3 to 4 to 1
    cell134h <- cell134g - (cell134g * m12post) # Individuals that move from 3 to 4 to 1 but not to 2
    
    #---Scenario1 4->1->3 ---#
    cell134i <- pop4post * m14post * m13post # Individuals that move from 4 to 1 to 3
    cell134j <- cell134i - (cell134i * m23post) # Individuals that move from 4 to 1 to 3 but not to 2
    
    #---Scenario1 4->3->1 ---#
    cell134k <- pop4post * m34post * m13post # Individuals that move from 4 to 3 to 1
    cell134l <- cell134k - (cell134k * m12post) # Individuals that move from 4 to 3 to 1 but not to 2
    
    cell134 <- cell134b + cell134d + cell134f + cell134h + cell134j + cell134l

    
    #---------------------------------#
    # Populations 2, 3, 4             #
    #---------------------------------#
    
    #---Scenario1 2->3->4 ---#
    cell234a <- pop2post * m23post * m34post # Individuals that move from 2 to 3 to 4
    cell234b <- cell234a - (cell234a * m14post) # Individuals that move from 2 to 3 to 4 but not to 1
    
    #---Scenario1 2->4->3 ---#
    cell234c <- pop2post * m24post * m34post # Individuals that move from 2 to 4 to 3
    cell234d <- cell234c - (cell234c * m13post) # Individuals that move from 2 to 4 to 3 but not to 1
    
    #---Scenario1 3->2->4 ---#
    cell234e <- pop3post * m23post * m24post # Individuals that move from 3 to 2 to 4
    cell234f <- cell234e - (cell234e * m14post) # Individuals that move from 3 to 2 to 4 but not to 1
    
    #---Scenario1 3->4->2 ---#
    cell234g <- pop3post * m34post * m24post # Individuals that move from 3 to 4 to 2
    cell234h <- cell234g - (cell234g * m12post) # Individuals that move from 3 to 4 to 2 but not to 1
    
    #---Scenario1 4->2->3 ---#
    cell234i <- pop4post * m24post * m23post # Individuals that move from 4 to 2 to 3
    cell234j <- cell234i - (cell234i * m13post) # Individuals that move from 4 to 2 to 3 but not to 1
    
    #---Scenario1 4->3->2 ---#
    cell234k <- pop4post * m34post * m23post # Individuals that move from 4 to 3 to 2
    cell234l <- cell234k - (cell234k * m12post) # Individuals that move from 4 to 3 to 2 but not to 1
    
    cell234 <- cell234b + cell234d + cell234f + cell234h + cell234j + cell234l

    
    #---------------------------------#
    # Estimate number of individuals  #
    # that SHOULD have been seen      #
    # across all four locations.      #
    #---------------------------------#

    #---Senario1 1->2->3->4 ---#
    cell1234a <- pop1post * m12post * m23post * m34post # Individuals that move from 1 to 2 to 3 to 4
    
    #---Senario1 1->2->4->3 ---#
    cell1234c <- pop1post * m12post * m24post * m34post # Individuals that move from 1 to 2 to 4 to 3
    
    #---Senario1 1->3->2->4 ---#
    cell1234e <- pop1post * m13post * m23post * m24post # Individuals that move from 1 to 3 to 2 to 4
    
    #---Senario1 1->3->4->2 ---#
    cell1234g <- pop1post * m13post * m34post * m24post # Individuals that move from 1 to 3 to 4 to 2

    #---Senario1 1->4->2->3 ---#
    cell1234i <- pop1post * m14post * m24post * m23post # Individuals that move from 1 to 4 to 2 to 3
    
    #---Senario1 1->4->3->2 ---#
    cell1234k <- pop1post * m14post * m34post * m23post # Individuals that move from 1 to 4 to 3 to 2

    #---Senario1 2->1->3->4 ---#
    cell1234m <- pop2post * m12post * m13post * m34post # Individuals that move from 2 to 1 to 3 to 4
    
    #---Senario1 2->1->4->3 ---#
    cell1234o <- pop2post * m12post * m14post * m34post # Individuals that move from 2 to 1 to 4 to 3
    
    #---Senario1 2->3->1->4 ---#
    cell1234q <- pop2post * m23post * m13post * m14post # Individuals that move from 2 to 3 to 1 to 4
    
    #---Senario1 2->3->4->1 ---#
    cell1234s <- pop2post * m23post * m34post * m14post # Individuals that move from 2 to 3 to 4 to 1
    
    #---Senario1 2->4->1->3 ---#
    cell1234u <- pop2post * m24post * m14post * m13post # Individuals that move from 2 to 4 to 1 to 3
    
    #---Senario1 2->4->3->1 ---#
    cell1234w <- pop2post * m24post * m34post * m13post # Individuals that move from 2 to 4 to 3 to 1
    
    #---Senario1 3->1->2->4 ---#
    cell1234y <- pop3post * m13post * m12post * m24post # Individuals that move from 3 to 1 to 2 to 4
    
    #---Senario1 3->1->4->2 ---#
    cell1234aa <- pop3post * m13post * m14post * m24post # Individuals that move from 3 to 1 to 4 to 2
    
    #---Senario1 3->2->1->4 ---#
    cell1234ac <- pop3post * m23post * m12post * m14post # Individuals that move from 3 to 2 to 1 to 4
    
    #---Senario1 3->2->4->1 ---#
    cell1234ae <- pop3post * m23post * m24post * m14post # Individuals that move from 3 to 2 to 4 to 1
    
    #---Senario1 3->4->1->2 ---#
    cell1234ag <- pop3post * m34post * m14post * m12post # Individuals that move from 3 to 4 to 1 to 2
    
    #---Senario1 3->4->2->1 ---#
    cell1234ai <- pop3post * m34post * m24post * m12post # Individuals that move from 3 to 4 to 2 to 1
    
    #---Senario1 4->1->2->3 ---#
    cell1234ak <- pop4post * m14post * m12post * m23post # Individuals that move from 4 to 1 to 2 to 3
    
    #---Senario1 4->1->3->2 ---#
    cell1234am <- pop4post * m14post * m13post * m23post # Individuals that move from 4 to 1 to 3 to 2
   
    #---Senario1 4->2->1->3 ---#
    cell1234ao <- pop4post * m24post * m12post * m13post # Individuals that move from 4 to 2 to 1 to 3
    
    #---Senario1 4->2->3->1 ---#
    cell1234aq <- pop4post * m24post * m23post * m13post # Individuals that move from 4 to 2 to 3 to 1
    
    #---Senario1 4->3->1->2 ---#
    cell1234as <- pop4post * m34post * m13post * m12post # Individuals that move from 4 to 3 to 1 to 2
    
    #---Senario1 4->3->2->1 ---#
    cell1234au <- pop4post * m34post * m23post * m12post # Individuals that move from 4 to 3 to 2 to 1
    
    cell1234 <- cell1234a + cell1234c + cell1234e + cell1234g + cell1234i + cell1234k + cell1234m + cell1234o + cell1234q + cell1234s + cell1234u + cell1234w + cell1234y + cell1234aa + cell1234ac + cell1234ae + cell1234ag + cell1234ai + cell1234ak + cell1234am + cell1234ao + cell1234aq + cell1234as + cell1234au


    #---------------------------------#
    # Plot results                    #
    #---------------------------------#
    source("plotPost.R")
    par(mfrow = c(3, 3))
    histinfo = plotPost(cell1234, xlab = "Locations 1, 2, 3, & 4", showMode = TRUE)
    histinfo = plotPost(cell123, xlab = "Locations 1, 2, & 3", showMode = TRUE)
    histinfo = plotPost(cell124, xlab = "Locations 1, 2, & 4", showMode = TRUE)
    histinfo = plotPost(cell134, xlab = "Locations 1, 3, & 4", showMode = TRUE)
    histinfo = plotPost(cell234, xlab = "Locations 2, 3, & 4", showMode = TRUE)
    histinfo = plotPost(cell12, xlab = "Locations 1 & 2", showMode = TRUE)
    histinfo = plotPost(cell13, xlab = "Locations 1 & 3", showMode = TRUE)
    histinfo = plotPost(cell14, xlab = "Locations 1 & 4", showMode = TRUE)
    histinfo = plotPost(cell23, xlab = "Locations 2 & 3", showMode = TRUE)
    histinfo = plotPost(cell24, xlab = "Locations 2 & 4", showMode = TRUE)
    histinfo = plotPost(cell34, xlab = "Locations 3 & 4", showMode = TRUE)
    histinfo = plotPost(cell1, xlab = "Location 1", showMode = TRUE)
    histinfo = plotPost(cell2, xlab = "Location 2", showMode = TRUE)
    histinfo = plotPost(cell3, xlab = "Location 3", showMode = TRUE)
    histinfo = plotPost(cell4, xlab = "Location 4", showMode = TRUE)

    #---------------------------------#
    # Output results as list for      #
    # subsequent analyses.            #
    #---------------------------------#
    results1 <- cbind(cell1234, cell123, cell124, cell134, cell234, cell12, cell13, cell14, cell23, cell24, cell34, cell1, cell2, cell3, cell4)

    return(results1)
}

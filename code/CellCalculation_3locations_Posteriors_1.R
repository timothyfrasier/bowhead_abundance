#####################################
# Estimate Proportions With         #
# Different Sighting Patterns       #
#                                   #
# REQUIRES: posterior probabilities #
# for all three population sizes    #
# and all three migrations rates.   #
#####################################

cellcounts <- function(pop1post, pop2post, pop3post, m12post, m13post, m23post, m5post) {

    #---------------------------------#
    # Just in location 1              #
    #---------------------------------#
    cell1 <- pop1post - (pop1post * m12post) - (pop1post * m13post) # Individuals from location 1 that don't move anywhere else

    #---------------------------------#
    # Just in location 2              #
    #---------------------------------#
    cell2 <- pop2post - (pop2post * m12post) - (pop2post * m23post) # Individuals from location 2 that don't move anywhere else

    #---------------------------------#
    # Just in location 3              #
    #---------------------------------#
    cell3 <- pop3post - (pop3post * m13post) - (pop3post * m23post) # Individuals from location 3 that don't move anywhere else

    #---------------------------------#
    # Population 1 & 2                #
    #---------------------------------#
    cell12y <- pop1post * m12post
    cell12a <- cell12y - (cell12y * m23post)  # Pop1 individuals that go to 2 but not 3
    cell12z <- pop2post * m12post
    cell12b <- cell12z - (cell12z * m13post) # Pop2 individuals that go to 1 but not 3
    cell12 <- cell12a + cell12b

    #---------------------------------#
    # Population 1 & 3                #
    #---------------------------------#
    cell13y <- pop1post * m13post
    cell13a <- cell13y - (cell13y * m23post) # Pop1 individuals that go to 3 but not 2
    cell13z <- pop3post * m13post
    cell13b <- cell13z - (cell13z * m12post) # Pop3 individuals that go to 1 but not 2
    cell13 <- cell13a + cell13b

    
    #---------------------------------#
    # Population 2 & 3                #
    #---------------------------------#
    cell23y <- pop2post * m23post
    cell23a <- cell23y - (cell23y * m13post) # Pop2 individuals that go to 3 but not 1
    cell23z <- pop3post * m23post
    cell23b <- cell23z - (cell23z * m12post) # Pop3 individuals that go to 2 but not 1
    cell23 <- cell23a + cell23b
    
    
    #---------------------------------#
    # Populations 1, 2, 3             #
    #---------------------------------#
    
    #---Scenario1 1->2->3 ---#
    cell123a <- pop1post * m12post * m23post # Individuals that move from 1 to 2 to 3
    
    #---Scenario1 1->3->2 ---#
    cell123c <- pop1post * m13post * m23post # Individuals that move from 1 to 3 to 2

    #---Scenario1 2->1->3 ---#
    cell123e <- pop2post * m12post * m13post # Individuals that move from 2 to 1 to 3
    
    #---Scenario1 2->3->1 ---#
    cell123g <- pop2post * m23post * m13post # Individuals that move from 2 to 3 to 1

    #---Scenario1 3->1->2 ---#
    cell123i <- pop3post * m13post * m12post # Individuals that move from 3 to 1 to 2
    
    #---Scenario1 3->2->1 ---#
    cell123k <- pop3post * m23post * m12post # Individuals that move from 3 to 2 to 1
    
    cell123 <- cell123a + cell123c + cell123e + cell123g + cell123i + cell123k
    


    #---------------------------------#
    # Plot results                    #
    #---------------------------------#
    source("plotPost.R")
    par(mfrow = c(2, 2))
    histinfo = plotPost(cell123, xlab = "Locations 1, 2, & 3", showMode = TRUE, col = "gray")
    histinfo = plotPost(cell12, xlab = "Locations 1 & 2", showMode = TRUE, col = "gray")
    histinfo = plotPost(cell13, xlab = "Locations 1 & 3", showMode = TRUE, col = "gray")
    histinfo = plotPost(cell23, xlab = "Locations 2 & 3", showMode = TRUE, col = "gray")
    histinfo = plotPost(cell1, xlab = "Location 1", showMode = TRUE, col = "gray")
    histinfo = plotPost(cell2, xlab = "Location 2", showMode = TRUE, col = "gray")
    histinfo = plotPost(cell3, xlab = "Location 3", showMode = TRUE, col = "gray")

    #---------------------------------#
    # Output results as list for      #
    # subsequent analyses.            #
    #---------------------------------#
    results1 <- cbind(cell123, cell12, cell13, cell23, cell1, cell2, cell3)

    return(results1)
}

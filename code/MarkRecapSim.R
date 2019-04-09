###################################################
#                 markrecapsim                    #
#                                                 #
# A function for simulating mark-recapture data   #
# for individuals from 4 different, partially     #
# overlapping, "populations" where a fifth,       #
# unsampled "population" also exists.             #
#                                                 #
#                       by                        #
#                  Tim Frasier                    #
#                  05-Jan-2016                    #
###################################################

markrecapsim <- function (nPop1, nPop2, nPop3, nPop4, nPop5, pop1Samp1, pop2Samp1, pop3Samp1, pop4Samp1, m12, m13, m14, m15, m23, m24, m25, m34, m35, m45, pop1Samp2, pop2Samp2, pop3Samp2, pop4Samp2) {

	#--------------------------------#
	#  CAPTURE & MARK INDIVIDUALS    #
	#--------------------------------#

	#---Marked individuals have a "100", unmarked have a "000"---#
	# Population 1
	pop1cap <- rep(100, pop1Samp1)
	pop1nocap <- rep(000, (nPop1 - pop1Samp1))
	pop1a <- c(pop1cap, pop1nocap)

	# Population 2
	pop2cap <- rep(200, pop2Samp1)
	pop2nocap <- rep(000, (nPop2 - pop2Samp1))
	pop2a <- c(pop2cap, pop2nocap)

	# Population 3
	pop3cap <- rep(300, pop3Samp1)
	pop3nocap <- rep(000, (nPop3 - pop3Samp1))
	pop3a <- c(pop3cap, pop3nocap)
    
    # Population 4
    pop4cap <- rep(400, pop4Samp1)
    pop4nocap <- rep(000, (nPop4 - pop4Samp1))
    pop4a <- c(pop4cap, pop4nocap)


	#------------------------------#
	#    SHUFFLE POPULATIONS       #
	#------------------------------#
	pop1mix <- sample(pop1a, nPop1, replace = FALSE)
	pop2mix <- sample(pop2a, nPop2, replace = FALSE)
	pop3mix <- sample(pop3a, nPop3, replace = FALSE)
    pop4mix <- sample(pop4a, nPop4, replace = FALSE)
	pop5mix <- rep(0, nPop5)


	#---------------------------------#
	#  HAVE INDIVIDUALS "MOVE"        #
	#---------------------------------#

	#---From Pop1---#
	# Rounding ensures # of migrants is a whole number
	nMigrants12 <- round(min((nPop1 * m12), (nPop2 * m12)))
	nMigrants13 <- round(min((nPop1 * m13), (nPop3 * m13)))
	nMigrants14 <- round(min((nPop1 * m14), (nPop4 * m14)))
    nMigrants15 <- round(min((nPop1 * m15), (nPop5 * m15)))
	nMigrants1 <- sum(nMigrants12, nMigrants13, nMigrants14, nMigrants15)
	if (nMigrants12 > 0) { 
		pop21 <- pop1mix[1:nMigrants12]
	} else {
		pop21 <- 0
	}	
	if (nMigrants13 > 0) {	
		pop31 <- pop1mix[(nMigrants12 + 1):(nMigrants12 + nMigrants13)]
	} else {
		pop31 <- 0
	}	
	if (nMigrants14 > 0) { 
		pop41 <- pop1mix[(nMigrants12 + nMigrants13 + 1):(nMigrants12 + nMigrants13 + nMigrants14)]
	} else {
		pop41 <- 0
	}
    if (nMigrants15 > 0) {
        pop51 <- pop1mix[(nMigrants12 + nMigrants13 + nMigrants14 + 1):(nMigrants12 + nMigrants13 + nMigrants14 + nMigrants15)]
    } else {
        pop51 <- 0
    }
    
	pop11 <- pop1mix[(nMigrants1 + 1):nPop1]

	#---From Pop2---#
	nMigrants23 <- round(min((nPop2 * m23), (nPop3 * m23)))
	nMigrants24 <- round(min((nPop2 * m24), (nPop4 * m24)))
    nMigrants25 <- round(min((nPop2 * m25), (nPop5 * m25)))
	nMigrants2 <- sum(nMigrants12, nMigrants23, nMigrants24, nMigrants25)
	if (nMigrants12 > 0) { 
		pop12 <- pop2mix[1:nMigrants12]
	} else {
		pop12 <- 0
	}	
	if (nMigrants23 > 0) {	
		pop32 <- pop2mix[(nMigrants12 + 1):(nMigrants12 + nMigrants23)]
	} else {
		pop32 <- 0
	}	
	if (nMigrants24 > 0) { 
		pop42 <- pop2mix[(nMigrants12 + nMigrants23 + 1):(nMigrants12 + nMigrants23 + nMigrants24)]
	} else {
		pop42 <- 0
	}
    if (nMigrants25 > 0) {
        pop52 <- pop2mix[(nMigrants12 + nMigrants23 + nMigrants24 + 1):(nMigrants12 + nMigrants23 + nMigrants24 + nMigrants25)]
    } else {
        pop52 <- 0
    }
	pop22 <- pop2mix[(nMigrants2 + 1):nPop2]

	#---From Pop3---#
	nMigrants34 <- round(min((nPop3 * m34), (nPop4 * m34)))
    nMigrants35 <- round(min((nPop3 * m35), (nPop5 * m35)))
	nMigrants3 <- sum(nMigrants13, nMigrants23, nMigrants34, nMigrants35)
	if (nMigrants13 > 0) { 
		pop13 <- pop3mix[1:nMigrants13]
	} else {
		pop13 <- 0
	}	
	if (nMigrants23 > 0) {	
		pop23 <- pop3mix[(nMigrants13 + 1):(nMigrants13 + nMigrants23)]
	} else {
		pop23 <- 0
	}	
	if (nMigrants34 > 0) { 
		pop43 <- pop3mix[(nMigrants13 + nMigrants23 + 1):(nMigrants13 + nMigrants23 + nMigrants34)]
	} else {
		pop43 <- 0
	}
    if (nMigrants35 > 0) {
        pop53 <- pop3mix[(nMigrants13 + nMigrants23 + nMigrants35 + 1):(nMigrants13 + nMigrants23 + nMigrants34 + nMigrants35)]
    } else {
        pop53 <- 0
    }
	pop33 <- pop3mix[(nMigrants3 + 1):nPop3]
	
	#---From Pop4---#
    nMigrants45 <- round(min((nPop4 * m45), (nPop5 * m45)))
	nMigrants4 <- sum(nMigrants14, nMigrants24, nMigrants34, nMigrants45)
	if (nMigrants14 > 0) { 
		pop14 <- pop4mix[1:nMigrants14]
	} else {
		pop14 <- 0
	}	
	if (nMigrants24 > 0) {	
		pop24 <- pop4mix[(nMigrants14 + 1):(nMigrants14 + nMigrants24)]
	} else {
		pop24 <- 0
	}	
	if (nMigrants34 > 0) { 
		pop34 <- pop4mix[(nMigrants14 + nMigrants24 + 1):(nMigrants14 + nMigrants24 + nMigrants34)]
	} else {
		pop34 <- 0
	}
    if (nMigrants45 > 0) {
        pop54 <- pop4mix[(nMigrants14 + nMigrants24 + nMigrants34 + 1):(nMigrants14 + nMigrants24 + nMigrants34 + nMigrants45)]
    } else {
        pop54 <- 0
    }
    pop44 <- pop4mix[(nMigrants4 + 1):nPop4]
    
    #---From Pop5---#
    nMigrants5 <- sum(nMigrants15, nMigrants25, nMigrants35, nMigrants45)
    if (nMigrants15 > 0) {
        pop15 <- pop5mix[1:nMigrants15]
    } else {
        pop15 <- 0
    }
    if (nMigrants25 > 0) {
        pop25 <- pop5mix[(nMigrants15 + 1):(nMigrants15 + nMigrants25)]
    } else {
        pop25 <- 0
    }
    if (nMigrants35 > 0) {
        pop35 <- pop5mix[(nMigrants15 + nMigrants25 + 1):(nMigrants15 + nMigrants25 + nMigrants35)]
    } else {
        pop35 <- 0
    }
    if (nMigrants45 > 0) {
        pop45 <- pop5mix[(nMigrants15 + nMigrants25 + nMigrants35 + 1):(nMigrants15 + nMigrants25 + nMigrants35 + nMigrants45)]
    } else {
        pop45 <- 0
    }
    pop55 <- pop5mix[(nMigrants5 + 1):nPop5]

	#---Merge Data and Shuffle---#
	pop1new <- c(pop11, pop12, pop13, pop14, pop15)
	pop1new <- sample(pop1new, nPop1, replace = FALSE)

	pop2new <- c(pop21, pop22, pop23, pop24, pop25)
	pop2new <- sample(pop2new, nPop2, replace = FALSE)

	pop3new <- c(pop31, pop32, pop33, pop34, pop35)
	pop3new <- sample(pop3new, nPop3, replace = FALSE)
	
	pop4new <- c(pop41, pop42, pop43, pop44, pop45)
	pop4new <- sample(pop4new, nPop4, replace = FALSE)
	
    pop5new <- c(pop51, pop52, pop53, pop54, pop55)
    pop5new <- sample(pop5new, nPop5, replace = FALSE)

	#---------------------------------#
	#  RESAMPLE POPULATIONS           #
	#---------------------------------#
	pop1resamp <- pop1new[1:pop1Samp2]
	pop2resamp <- pop2new[1:pop2Samp2]
	pop3resamp <- pop3new[1:pop3Samp2]
    pop4resamp <- pop4new[1:pop4Samp2]
	
	
	#---------------------------------#
	# INDICATE WHICH INDIVIDUALS WERE #
	# CAPTURED WHERE, USING SECOND    #
	# DIGIT                           #
	#---------------------------------#
	
	#--In Pop1--#
	for (marked in 1:pop1Samp2) {
		if (pop1resamp[marked] >= 100) {
			pop1resamp[marked] <- pop1resamp[marked] + 10
		}
	}
	
	#--In Pop2--#
	for (marked in 1:pop2Samp2) {
		if (pop2resamp[marked] >= 100) {
			pop2resamp[marked] <- pop2resamp[marked] + 20
		}
	}
	
	#--In Pop3--#
	for (marked in 1:pop3Samp2) {
		if (pop3resamp[marked] >= 100) {
			pop3resamp[marked] <- pop3resamp[marked] + 30
		}
	}
	
    #--In Pop4--#
    for (marked in 1:pop4Samp2) {
        if (pop4resamp[marked] >= 100) {
            pop4resamp[marked] <- pop4resamp[marked] + 40
        }
    }
    
    
    #----------------------------------------------#
    # GENERATE DATA FOR ESTIMATING MIGRATION RATES #
    #----------------------------------------------#
    
    #--- Pop1 & 2 ---#
    #--- Recaptures will be all individuals with sighting histories > 10 ro 20 ---#
    pop1recap <- sum(pop1resamp > 10)
    pop2recap <- sum(pop2resamp > 20)
    captured12 <- sum(pop1resamp == 210)
    captured21 <- sum(pop2resamp == 120)
    
    #--- Pop1 & 3 ---#
    pop3recap <- sum(pop3resamp > 30)
    captured13 <- sum(pop1resamp == 310)
    captured31 <- sum(pop3resamp == 130)
   
    #--- Pop1 & 4 ---#
    pop4recap <- sum(pop4resamp > 40)
    captured14 <- sum(pop1resamp == 410)
    captured41 <- sum(pop4resamp == 140)
   
    #--- Pop2 & 3 ---#
    captured23 <- sum(pop2resamp == 320)
    captured32 <- sum(pop3resamp == 230)
   
    #--- Pop2 & 4 ---#
    captured24 <- sum(pop2resamp == 420)
    captured42 <- sum(pop4resamp == 240)
    
    #--- Pop3 & 4 ---#
    captured34 <- sum(pop3resamp == 430)
    captured43 <- sum(pop4resamp == 340)
    
    #--- Create data frame with data ---#
    mpop12 <- c("Pop 1 & 2", captured12, pop1recap, captured21, pop2recap)
    mpop13 <- c("Pop 1 & 3", captured13, pop1recap, captured31, pop3recap)
    mpop14 <- c("Pop 1 & 4", captured14, pop1recap, captured41, pop4recap)
    mpop23 <- c("Pop 2 & 3", captured23, pop2recap, captured32, pop3recap)
    mpop24 <- c("Pop 2 & 4", captured24, pop2recap, captured42, pop4recap)
    mpop34 <- c("Pop 3 & 4", captured34, pop3recap, captured43, pop4recap)
    migration_data <- as.data.frame(cbind(mpop12, mpop13, mpop14, mpop23, mpop24, mpop34))
    
    
    #----------------------------------------------#
    # GENERATE CAPTURE HISTORIES FOR EACH LOCATION #
    #----------------------------------------------#
    totalcap1 <- pop1Samp1 + pop1Samp2
    totalcap2 <- pop2Samp1 + pop2Samp2
    totalcap3 <- pop3Samp1 + pop3Samp2
    totalcap4 <- pop4Samp1 + pop4Samp2
    
    #---------------#
    #   Pop 1       #
    #---------------#
    
    #--- Initialize matrix to hold data & fill with zeros ---#
    pop1hist <- matrix(0, nrow = totalcap1, ncol = 2)
    
    #--- Fill appropriate # of rows in first column with 1's ---#
    for (i in 1:pop1Samp1) {
        pop1hist[i, 1] <- 1
    }
    
    #--- Recaptures will be all individuals with sighting histories > 10 ---#
    pop1recap <- sum(pop1resamp > 10)
    
    for (i in 1:pop1recap) {
        pop1hist[i, 2] <- 1
    }
    
    #--- Captures only in second time period will be those left ---#
    pop1remaining <- pop1Samp2 - pop1recap
    
    for (i in (pop1Samp1 + 1):(pop1Samp1 + 1 + pop1remaining)) {
        pop1hist[i, 2] <- 1
    }


    #--- Trim matrix for only rows with data ---#
    #--- Rows with data ---#
    datarows <- 0
    for (i in 1:totalcap1) {
        if (pop1hist[i, 1] == 1 | pop1hist[i, 2] == 1) {
            datarows <- datarows + 1
        }
    }
    
    pop1hist2 <- matrix(0, nrow = datarows, ncol = 2)
    
    counter <- 1
    for (i in 1:totalcap1) {
        if (pop1hist[i, 1] == 1 | pop1hist[i, 2] == 1) {
            pop1hist2[counter, ] <- pop1hist[i, ]
            counter <- counter + 1
        }
    }
    
    #---------------#
    #   Pop 2       #
    #---------------#
    
    #--- Initialize matrix to hold data & fill with zeros ---#
    pop2hist <- matrix(0, nrow = totalcap2, ncol = 2)
    
    #--- Fill appropriate # of rows in first column with 1's ---#
    for (i in 1:pop2Samp1) {
        pop2hist[i, 1] <- 1
    }
    
    #--- Recaptures will be all individuals with sighting histories > 20 ---#
    pop2recap <- sum(pop2resamp > 20)
    
    for (i in 1:pop2recap) {
        pop2hist[i, 2] <- 1
    }
    
    #--- Captures only in second time period will be those left ---#
    pop2remaining <- pop2Samp2 - pop2recap
    
    for (i in (pop2Samp1 + 1):(pop2Samp1 + 1 + pop2remaining)) {
        pop2hist[i, 2] <- 1
    }
    
    
    #--- Trim matrix for only rows with data ---#
    #--- Rows with data ---#
    datarows <- 0
    for (i in 1:totalcap2) {
        if (pop2hist[i, 1] == 1 | pop2hist[i, 2] == 1) {
            datarows <- datarows + 1
        }
    }
    
    pop2hist2 <- matrix(0, nrow = datarows, ncol = 2)
    
    counter <- 1
    for (i in 1:totalcap2) {
        if (pop2hist[i, 1] == 1 | pop2hist[i, 2] == 1) {
            pop2hist2[counter, ] <- pop2hist[i, ]
            counter <- counter + 1
        }
    }
    
    #---------------#
    #   Pop 3       #
    #---------------#
    
    #--- Initialize matrix to hold data & fill with zeros ---#
    pop3hist <- matrix(0, nrow = totalcap3, ncol = 2)
    
    #--- Fill appropriate # of rows in first column with 1's ---#
    for (i in 1:pop3Samp1) {
        pop3hist[i, 1] <- 1
    }
    
    #--- Recaptures will be all individuals with sighting histories > 30 ---#
    pop3recap <- sum(pop3resamp > 30)
    
    for (i in 1:pop3recap) {
        pop3hist[i, 2] <- 1
    }
    
    #--- Captures only in second time period will be those left ---#
    pop3remaining <- pop3Samp2 - pop3recap
    
    for (i in (pop3Samp1 + 1):(pop3Samp1 + 1 + pop3remaining)) {
        pop3hist[i, 2] <- 1
    }
    
    
    #--- Trim matrix for only rows with data ---#
    #--- Rows with data ---#
    datarows <- 0
    for (i in 1:totalcap3) {
        if (pop3hist[i, 1] == 1 | pop3hist[i, 2] == 1) {
            datarows <- datarows + 1
        }
    }
    
    pop3hist2 <- matrix(0, nrow = datarows, ncol = 2)
    
    counter <- 1
    for (i in 1:totalcap3) {
        if (pop3hist[i, 1] == 1 | pop3hist[i, 2] == 1) {
            pop3hist2[counter, ] <- pop3hist[i, ]
            counter <- counter + 1
        }
    }

    #---------------#
    #   Pop 4       #
    #---------------#

    #--- Initialize matrix to hold data & fill with zeros ---#
    pop4hist <- matrix(0, nrow = totalcap4, ncol = 2)

    #--- Fill appropriate # of rows in first column with 1's ---#
    for (i in 1:pop4Samp1) {
        pop4hist[i, 1] <- 1
    }

    #--- Recaptures will be all individuals with sighting histories > 40 ---#
    pop4recap <- sum(pop4resamp > 40)

    for (i in 1:pop4recap) {
        pop4hist[i, 2] <- 1
    }

    #--- Captures only in second time period will be those left ---#
    pop4remaining <- pop4Samp2 - pop4recap

    for (i in (pop4Samp1 + 1):(pop4Samp1 + 1 + pop4remaining)) {
        pop4hist[i, 2] <- 1
    }


    #--- Trim matrix for only rows with data ---#
    #--- Rows with data ---#
    datarows <- 0
    for (i in 1:totalcap4) {
        if (pop4hist[i, 1] == 1 | pop4hist[i, 2] == 1) {
            datarows <- datarows + 1
        }
    }

    pop4hist2 <- matrix(0, nrow = datarows, ncol = 2)

    counter <- 1
    for (i in 1:totalcap4) {
        if (pop4hist[i, 1] == 1 | pop4hist[i, 2] == 1) {
            pop4hist2[counter, ] <- pop4hist[i, ]
            counter <- counter + 1
        }
    }


    #-----------------------#
    #   RETURN RESULTS      #
    #-----------------------#
    results1 <- list(migration_data, pop1hist2, pop2hist2, pop3hist2, pop4hist2)

	return(results1)
}	

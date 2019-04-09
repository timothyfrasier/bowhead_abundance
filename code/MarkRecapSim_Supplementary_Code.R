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

#------------------------------#
#       EXAMPLE DATA           #
#------------------------------#

#----Enter Data for Population Sizes---#
nPop1 = 3000		  # Size of population 1
nPop2 = 8000	    # Size of population 2
nPop3 = 9000		  # Size of population 3
nPop4 = 5000      # Size of population 4
nPop5 = 500	      # Size of first "unknown" population
nPop6 = 900       # Size of second "unknown" population

#---Enter Initial Sample Sizes (for "marking")---#
pop1Samp1 = round(nPop1 * 0.15)
pop2Samp1 = round(nPop2 * 0.15)
pop3Samp1 = round(nPop3 * 0.15)
pop4Samp1 = round(nPop4 * 0.15)

#---Enter Data for "Mixing" Rates---#
m12 = 0.24		# Movement rate between 1 and 2 (symmetrical)
m13 = 0.28		# Movement rate between 1 and 3 (symmetrical)
m14 = 0.16    # Movement rate between 1 and 4 (symmetrical)
m15 = 0.06    # Movement rate between 1 and 5 (symmetrical)
m16 = 0.07    # Movement rate between 1 and 6 (symmetrical)
m23 = 0.18		# Movement rate between 2 and 3 (symmetrical)
m24 = 0.26		# Movement rate between 2 and 4 (symmetrical)
m25 = 0.08		# Movement rate between 2 and 5 (symmetrical)
m26 = 0.05		# Movement rate between 2 and 6 (symmetrical)
m34 = 0.23		# Movement rate between 3 and 4 (symmetrical)
m35 = 0.10		# Movement rate between 3 and 5 (symmetrical)
m36 = 0.07		# Movement rate between 3 and 6 (symmetrical)
m45 = 0.05		# Movement rate between 4 and 5 (symmetrical)
m46 = 0.08		# Movement rate between 4 and 6 (symmetrical)
m56 = 0.23		# Movement rate between 5 and 6 (symmetrical)

#---Enter First Resampling Sample Sizes---#
pop1Samp2 = round(nPop1 * 0.15)
pop2Samp2 = round(nPop2 * 0.15)
pop3Samp2 = round(nPop3 * 0.15)
pop4Samp2 = round(nPop4 * 0.15)

#------------------------------#
#      RUN THE FUNCTION        #
#------------------------------# 
sim1 <- markrecapsim(nPop1, nPop2, nPop3, nPop4, nPop5, nPop6, pop1Samp1, pop2Samp1, pop3Samp1, pop4Samp1, m12, m13, m14, m15, m16, m23, m24, m25, m26, m34, m35, m36, m45, m46, m56, pop1Samp2, pop2Samp2, pop3Samp2, pop4Samp2)

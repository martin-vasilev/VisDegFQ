## SCRIPT TO DEGRADE TARGET WORDS IN SENTENCES #####################
## IF YOU CREATED YOUR STIM'S WITH THE SCRIPT, PLS CITE:           #
## Marx, C., Hawelka, S., Schuster, S., & Hutzler, F. (2015).      #
## An incremental boundary study on parafoveal preprocessing in    #
## children reading aloud: Parafoveal masks overestimate the       #
## preview benefit. Journal Cognitive Psychology, 27, 2044-5911.   #
## doi:10.1080/20445911.2015.1008494                               #
##                                                                 #
## AS YET ONLY RUNS ON UNIX SYSTEMS, BC IT REQUIRES pngtopnm       #
####################################################################

require("pixmap")

#################################
## DEFINE STIM PROPERTIES HERE ##
#################################
crtv <- 0   # % pixles replaced
pxpi <- 60   # pixles per inch (ppi) - treat me thoughtfully!!!!!!!
wdth <- 1024-50  # width of the png
hght <- 40   # height of the png
pnts <- 22   # font size (pointsize)
wsch <- 11    # width of a single character

## maximal vertical and horizontal distance for replacing pixels
## only useful for PNGs with a reasonable high resolution, but
## do not set the limits too narrow (danger of not breaking out of loop)
## defaults are the width and height of the png (true randomness)
mxDst_v <- wdth 
mxDst_h <- hght

## THE SCRIPT PRODUCES ANTI-ALIASES PNGs
## YOU CAN PROBABLY IMPROVE THE "TARGET-ABILITY" OF THE DEGRADED STIMs
## BY TURNING THE ANTIALIASES PIXEL TO BLACK - CHECK IT OUT
## HERE IS THE OPTION - default should be FALSE
## define an alternative mono-chrome color, if you wish (but most probably, 
## you will want black (i.e., 0) 
## IF YOU CHOOSE 1, THEN THE SCRIPT PERFORMS PIXEL REMOVAL!!!!!!!!!
no_antiAlias <- FALSE
altern_value <- 0 

## STIMS HERE ...
wrds <-  c('The old man suffered a recent fall when he was shopping.',
'The couple wanted a white carpet to put in their living room.',
'The large carton with the brown side looked very battered and worn.',
paste('Calvin',"'",'s grandfather owned the first hotel in South Dorset.',sep=''),
'After the battle, the whole army wanted to go home.',
'Margaret was stressed and her quick typing resulted in many typos.',
'Dr. Leon was nervous about giving the short speech at the fund raising banquet.',
'The politician was troubled by the public affair and resigned.',
'The small town was far away from the modern city where the decisions were made.',
'The supporters cheered when the popular team finally won the match.',
paste('Tom was worried about his mother',"'",'s major illness but couldn',"'",'t visit her.',sep=''),
paste('The hikers were concerned that the mixed fruit wouldn',"'",'t last very long.',sep=''),
'According to the report, it should be clear today and chilly tomorrow.',
'The carpenter examined the middle door that had to be mended.',
'Karen knew that the basic fact would soon be discovered.',
'Ray noticed that the nervous coach had been ten minutes late to the game.',
'The nurse tended to the tired feet of the grumpy old woman.',
'Thomas finished eating his daily cereal and then had a cup of coffee.',
'The children thought the green bird at the zoo was beautiful.',
'In the field, the sweet dog was playing catch with a frisbee.',
'At the salon, the stylist cut the extra hair for the regular customer.',
'The experts concluded that the complete statue had been made in Rome.',
'The decorators worked on the royal home until it looked wonderful.',
'The guests immediately liked the gorgeous house and its big garden.',
'The midwife admired the famous baby after the difficult birth.',
'The artist criticised the black line that was the focus of the painting.',
'Nobody in the kingdom knew that the grand prince planned to kill his father.',
'Yesterday, Nichole made a brief comment about her manager that was scandalous.',
'Dave regretted his sorry past despite his recent successes.',
'Lucy said she enjoyed the fresh music but not everyone thought it was good.',
'Few tourists had heard about the western island where nobody lived.',
'Because the cook never cleaned his long sleeves at work, he was punished.',
'The aging champion was no match for the young boxer challenging him.',
paste('She didn',"'",'t like the white colour of the dress at all.',sep=''),
'The nurse visited the hungry patient during her afternoon shift.',
'The visitors were impressed by the excellent architecture of the city.',
'The young couple felt closer after their honest conversation about their future.',
'The mountaineer had a strong determination to make it to the top.',
'The priest knew that the simple actions of the church could not help the village.',
'Carla wondered if Tom thought she was a cold person after their date last night.',
'The golfer remained skeptical despite the perfect conditions for the game.',
'Without her glasses, Margaret had poor vision that made it difficult to read.',
'Without doubt, the natural beauty of the jungle attracts many tourists.',
'Jake was worried that his secret infidelity could break his marriage.',
'The woman next door gave John a sad smile when he passed by her house.',
'Theodore eventually found the right road late at night.',
'Susan thought the interesting painting had to be in the gallery.',
'The officer was investigating the terrible attack on a businessman.',
'His favourite place to read was the nice room overlooking the garden.',
paste('Carmen couldn',"'",'t find the yellow shirt when she unpacked from her trip.',sep=''),
'The residents of the little town voted for a new mayor.',
'His mother purchased the central cottage for a summer home.',
'She was captivated by the unique view across the cliff tops.',
'The teenagers used the private area behind the apartments to play football.',
'Near the University, there is a level field where students practice rugby.',
'When the issue was discovered, the former employee took all the blame.',
paste('Joe didn',"'",'t like the tight deadlines that he had to meet.',sep=''),
'Phil managed to read the foreign book within a few days.',
'The appraiser had noted the clean edge of the antique razor.',
'The trained mechanic used the unusual tool to repair the engine.',
'The solicitor carefully studied the special case although it was very tedious.',
'The lawyers thought that the classic report might not be enough.',
'Michael was enraged when the local officers pulled him over for no reason.',
'The poor miners were annoyed by the massive cuts to their salaries.',
'The statistician analysed the original data before he consulted his manager.',
'The forecast warned about the wet highway outside of town.',
paste('Bill bought a good outfit for his manager',"'",'s birthday party.',sep=''),
'Jennifer had to meet with her difficult clients later this afternoon.',
'The workers replaced the lower window on the second floor of the house.',
'The teacher waited in the bright hall before the school assembly.',
'The couple considered the large garden before placing an offer on the house.',
'They were debating whether the strange behaviour of the officer warranted a punishment.',
'Sheep grazed on the quiet farm across the road.',
'All students were invited to attend the open trial at the court.',
'The cook ordered the expensive food from the local market.',
'The child pestered the tiny fish that was hiding behind the pondweed.',
'The technician compared the broken half with the remaining substance.',
'The investigator examined the dark portion for any signs of tampering.',
'Outside the school, the happy girl skipped around the other children.',
'The witness clearly remembered the thick beard of the murder suspect.',
paste('Henry held his mother',"'",'s small hand as she walked down the steps.',sep=''),
'Mike found the tough work tiresome but rewarding.',
'It is not known whether the entire species may be susceptible to the disease.',
'Liz hung a picture on her empty wall to add some decoration to the room.',
'Ginger was careful with the ancient bowl after she learned how expensive it was.',
paste('The report was found to contain false claims that couldn',"'",'t be verified.',sep=''),
'The old man reminisced about his great life while his family listened.',
'Heather read a book about the military leader who ruled the small country.',
paste('The peasants didn',"'",'t enjoy their current way of life in the village.',sep=''),
'The book had a brilliant approach to the problem of child poverty.',
'The young author was offended by the personal review of his editor.',
'Tim never missed an episode of the legal drama until it was moved to a new time.',
'The detective noted the correct statement that Sam gave.',
'To the disappointment of the audience, the huge performance had to be cancelled.',
paste('Many of the listeners didn',"'",'t understand the final point of his argument.',sep=''),
'Andrew liked the blue paint colour of his new sports car.',
'The team devised a careful plan with a new schedule.',
'The waitress was a friendly woman and immediately took their orders.',
'For the position, only competent and responsible candidates will be considered.',
'She designed the fabulous kitchen in her house herself.',
'Even when they were facing total defeat in the game, they remained brave.',
'The mayor stepped down after his angry outbursts became a problem.',
'The staff had left a welcome note for the newcomers.',
'He adamantly refused to accept different opinions from anyone, even his advisors.',
'She instructed the teams to try similar strategies and report the results.',
'Any short straight cable can easily serve as an aerial.',
'The minister was outraged by the low security standards at the airport.',
paste('The fact that Jack was a top gamer didn',"'",'t seem to impress his friends.',sep=''),
'He managed to carry his heavy brother upstairs with great effort.',
'The meal had a weird flavour that Cameron found unpleasant.',
'The young man had a ready response for why he was late.',
'Despite his popularity, the liberal lawmaker had few friends outside of work.',
'The incredibly dirty kitchen did not inspire confidence in the restaurant.',
'The man had a young voice that made him easy to recognise.',
paste('Jane considered buying the same dress for her mother',"'",'s upcoming birthday.',sep=''),
paste('The landlady didn',"'",'t like the fancy curtains that the previous tenants had left behind.',sep=''),
paste('The CEO wasn',"'",'t happy about the price of the fine chairs that his designer had bought.',sep=''),
'The teacher was surprised by the wild accusations of the misbehaving student.',
'Mark was looking to buy a proper car for his new job as a construction worker.',
'Suzy noticed the dry humour that her manager occasionally used.',
paste('Linda didn',"'",'t initially realise what a clever man her father-in-law was.',sep=''),
'Everybody was afraid that the wrong message could harm the company.',
'In need of more space, Jenny took her other suitcase to carry her clothes.',
'The waitress told the customer that the third drink is for free.',
'Few people knew what a lovely lady Mrs. Jones was.',
'The tourists preferred the beautiful beaches to the snowy mountains.',
'Marisa wanted a magic solution to help her with her illness.',
'David never imagined what a serious effect his actions would have had.',
'The judges thought that the athlete stood a real chance of winning the race.',
'The contestants remained focused despite the funny atmosphere on the stage.',
'It is well known that children learn general knowledge from an early age.',
'The student lost points for using a single proof in her assignment.',
'The politician needed national support to win the elections.',
'After 10 years behind bars, the prisoner felt the exciting sense of his freedom.',
'The instructor thought that the available materials would be suitable for his course.',
'Gina would always wrap her warm blanket around her before going to bed.')

# NUMBER OF CHARACTERS (INCLUDING SPACES) PRIOR TO THE TARGET WORD:          
Nchars <- c( 30, 26, 32, 37, 28, 36, 44, 42, 44, 40, 41, 41, 44, 34, 26, 29, 30, 33, 31, 24, 40,
 40, 35, 42, 31, 32, 42, 32, 25, 32, 41, 40, 46, 26, 29, 45, 48, 29, 32, 45, 50, 39, 27, 33, 36,
 36, 30, 43, 41, 32, 28, 33, 33, 31, 38, 42, 26, 33, 34, 38, 44, 37, 35, 44, 39, 34, 19, 40, 31,
 33, 32, 39, 26, 45, 31, 28, 35, 35, 30, 41, 30, 21, 35, 32, 36, 38, 39, 39, 40, 25, 46, 41, 32,
 48, 50, 22, 27, 28, 49, 26, 33, 39, 29, 41, 40, 19, 37, 29, 30, 21, 26, 36, 21, 20, 32, 35, 49,
 38, 33, 21, 45, 36, 44, 46, 30, 37, 22, 36, 49, 51, 45, 43, 31, 59, 42, 32)

# HERE YOU MUST SPECIFY UNIQUE CODES FOR THE FILENAME OF THE PNG: 
flnm <- c('HF1', 'HF2', 'HF3', 'HF4', 'HF5', 'HF6', 'HF7', 'HF8', 'HF9', 'HF10', 'HF11', 'HF12',
 'HF13', 'HF14', 'HF15', 'HF16', 'HF17', 'HF18', 'HF19', 'HF20', 'HF21', 'HF22', 'HF23', 'HF24',
 'HF25', 'HF26', 'HF27', 'HF28', 'HF29', 'HF30', 'HF31', 'HF32', 'HF33', 'HF34', 'HF35', 'HF36',
 'HF37', 'HF38', 'HF39', 'HF40', 'HF41', 'HF42', 'HF43', 'HF44', 'HF45', 'HF46', 'HF47', 'HF48',
 'HF49', 'HF50', 'HF51', 'HF52', 'HF53', 'HF54', 'HF55', 'HF56', 'HF57', 'HF58', 'HF59', 'HF60',
 'HF61', 'HF62', 'HF63', 'HF64', 'HF65', 'HF66', 'HF67', 'HF68', 'HF69', 'HF70', 'HF71', 'HF72',
 'HF73', 'HF74', 'HF75', 'HF76', 'HF77', 'HF78', 'HF79', 'HF80', 'HF81', 'HF82', 'HF83', 'HF84',
 'HF85', 'HF86', 'HF87', 'HF88', 'HF89', 'HF90', 'HF91', 'HF92', 'HF93', 'HF94', 'HF95', 'HF96',
 'HF97', 'HF98', 'HF99', 'HF100', 'HF101', 'HF102', 'HF103', 'HF104', 'HF105', 'HF106', 'HF107',
 'HF108', 'HF109', 'HF110', 'HF111', 'HF112', 'HF113', 'HF114', 'HF115', 'HF116', 'HF117', 'HF118',
 'HF119', 'HF120', 'HF121', 'HF122', 'HF123', 'HF124', 'HF125', 'HF126', 'HF127', 'HF128', 'HF129',
 'HF130', 'HF131', 'HF132', 'HF133', 'HF134', 'HF135', 'HF136')

for (i in (1:length(wrds))) {

  countChngdM = 0
  countBlackM = 0
  
  # create initial full contrast png
  png(filename = "temp.png", width = wdth, height = hght, 
      units = "px", pointsize = pnts, bg = "white", res = pxpi)          
    par(mar=c(0,0,0,0))
    plot(c(0,1), axes=F, main="", xlab="" ,ylab="", col="white")
    text(0.95,0.48, wrds[i], pos=4,  family="mono", font=2)
  dev.off()

  # make and read pnm-file and extract the pxl-matrix
  system('pngtopnm ./temp.png >./temp.pnm') 
  my_matrix <- read.pnm("temp.pnm")

  # get the area - as a rectangle - where there are black pixels (i.e., the word boundaries)
  for (upperBorder in c(1:nrow(my_matrix@grey))) {
    if (sum(my_matrix@grey[upperBorder,]) < ncol(my_matrix@grey)) break  
  }
  for (lowerBorder in c(nrow(my_matrix@grey):1)) {
    if (sum(my_matrix@grey[lowerBorder,]) < ncol(my_matrix@grey)) break  
  }
  for (leftBorder in c(1:ncol(my_matrix@grey))) {
    if (sum(my_matrix@grey[,leftBorder]) < nrow(my_matrix@grey)) break  
  }
  for (rightBorder in c(ncol(my_matrix@grey):1)) {
    if (sum(my_matrix@grey[,rightBorder]) < nrow(my_matrix@grey)) break  
  }

  leftBorder <- leftBorder + Nchars[i] * wsch

  # count non-white pixels in the matrix
  for (spalte in c(leftBorder:ncol(my_matrix@grey))) {
    countBlackM <- countBlackM + length(my_matrix@grey[,spalte][my_matrix@grey[,spalte] < 1])
  }  
  
  while (1) {
    zeile <- sample(c(upperBorder:lowerBorder))[1]  
    spalte <- sample(c(leftBorder:rightBorder))[1]
    if (my_matrix@grey[zeile, spalte] < 1) {
      if (no_antiAlias) {
        remCol <- altern_value
      } else { remCol <- my_matrix@grey[zeile,spalte] }
      rand = sample.int(100, 100)
      if (rand[50] <= crtv) {
        countChngdM <- countChngdM + 1 
        # re-insert the non-white spalte elsewhere within the word boundaries
        while (1) {
          newPxlRow <- sample(c(upperBorder:lowerBorder))[1]
          newPxlCol <- sample(c(leftBorder:rightBorder))[1] 
            if (my_matrix@grey[newPxlRow,newPxlCol] == 1 &
                abs(newPxlRow-zeile) <= mxDst_v  & 
                abs(newPxlCol-spalte) <= mxDst_h) {
              my_matrix@grey[newPxlRow,newPxlCol] <- remCol
              break
            } 
         }
         my_matrix@grey[zeile,spalte] <- 1
       }
    }
    if (countChngdM/countBlackM >= crtv/100) break  
  }

  print(countChngdM/countBlackM)
  
  # PNG Files: change name here
  bmp(filename = paste(flnm[i],"_", crtv, ".bmp", sep=""), width = wdth, height = hght)
    par(mar=c(0,0,0,0))
    plot(my_matrix, axes=F, main="", xlab="" ,ylab="")
  dev.off()
  
}  
  
   

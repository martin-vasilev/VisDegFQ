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
wrds <- c('The old man suffered a lethal fall when he was shopping.',
'The couple wanted a plush carpet to put in their living room.',
'The large carton with the beige side looked very battered and worn.',
paste('Calvin',"'",'s grandfather owned the famed hotel in South Dorset.',sep=''),
'After the battle, the irate army wanted to go home.',
'Margaret was stressed and her hasty typing resulted in many typos.',
'Dr. Leon was nervous about giving the pithy speech at the fund raising banquet.',
'The politician was troubled by the morbid affair and resigned.',
'The small town was far away from the hectic city where the decisions were made.',
'The supporters cheered when the passive team finally won the match.',
paste('Tom was worried about his mother',"'",'s acute illness but couldn',"'",'t visit her.',sep=''),
paste('The hikers were concerned that the scant fruit wouldn',"'",'t last very long.',sep=''),
'According to the report, it should be humid today and chilly tomorrow.',
'The carpenter examined the shoddy door that had to be mended.',
'Karen knew that the taboo fact would soon be discovered.',
'Ray noticed that the bearded coach had been ten minutes late to the game.',
'The nurse tended to the pasty feet of the grumpy old woman.',
'Thomas finished eating his bland cereal and then had a cup of coffee.',
'The children thought the timid bird at the zoo was beautiful.',
'In the field, the macho dog was playing catch with a frisbee.',
'At the salon, the stylist cut the blond hair for the regular customer.',
'The experts concluded that the faceless statue had been made in Rome.',
'The decorators worked on the dingy home until it looked wonderful.',
'The guests immediately liked the spotless house and its big garden.',
'The midwife admired the serene baby after the difficult birth.',
'The artist criticised the askew line that was the focus of the painting.',
'Nobody in the kingdom knew that the suave prince planned to kill his father.',
'Yesterday, Nichole made a terse comment about her manager that was scandalous.',
'Dave regretted his jaded past despite his recent successes.',
'Lucy said she enjoyed the manic music but not everyone thought it was good.',
'Few tourists had heard about the oceanic island where nobody lived.',
'Because the cook never cleaned his oily sleeves at work, he was punished.',
'The aging champion was no match for the lithe boxer challenging him.',
paste('She didn',"'",'t like the ashen colour of the dress at all.',sep=''),
'The nurse visited the groggy patient during her afternoon shift.',
'The visitors were impressed by the modernist architecture of the city.',
'The young couple felt closer after their candid conversation about their future.',
'The mountaineer had a steely determination to make it to the top.',
'The priest knew that the futile actions of the church could not help the village.',
'Carla wondered if Tom thought she was a nosy person after their date last night.',
'The golfer remained skeptical despite the optimal conditions for the game.',
'Without her glasses, Margaret had hazy vision that made it difficult to read.',
'Without doubt, the angelic beauty of the jungle attracts many tourists.',
'Jake was worried that his sinful infidelity could break his marriage.',
'The woman next door gave John a coy smile when he passed by her house.',
'Theodore eventually found the bleak road late at night.',
'Susan thought the extravagant painting had to be in the gallery.',
'The officer was investigating the puzzling attack on a businessman.',
'His favourite place to read was the oval room overlooking the garden.',
paste('Carmen couldn',"'",'t find the floral shirt when she unpacked from her trip.',sep=''),
'The residents of the ornate town voted for a new mayor.',
'His mother purchased the crimson cottage for a summer home.',
'She was captivated by the mystic view across the cliff tops.',
'The teenagers used the unclean area behind the apartments to play football.',
'Near the University, there is a rowdy field where students practice rugby.',
'When the issue was discovered, the unwise employee took all the blame.',
paste('Joe didn',"'",'t like the rigid deadlines that he had to meet.',sep=''),
'Phil managed to read the overdue book within a few days.',
'The appraiser had noted the blunt edge of the antique razor.',
'The trained mechanic used the weighty tool to repair the engine.',
'The solicitor carefully studied the generic case although it was very tedious.',
'The lawyers thought that the interim report might not be enough.',
'Michael was enraged when the inept officers pulled him over for no reason.',
'The poor miners were annoyed by the drastic cuts to their salaries.',
'The statistician analysed the abundant data before he consulted his manager.',
'The forecast warned about the icy highway outside of town.',
paste('Bill bought a neat outfit for his manager',"'",'s birthday party.',sep=''),
'Jennifer had to meet with her unmarried clients later this afternoon.',
'The workers replaced the leaky window on the second floor of the house.',
'The teacher waited in the barren hall before the school assembly.',
'The couple considered the weedy garden before placing an offer on the house.',
'They were debating whether the drunken behaviour of the officer warranted a punishment.',
'Sheep grazed on the rival farm across the road.',
'All students were invited to attend the mock trial at the court.',
'The cook ordered the unhealthy food from the local market.',
'The child pestered the tame fish that was hiding behind the pondweed.',
'The technician compared the dilute half with the remaining substance.',
'The investigator examined the waxy portion for any signs of tampering.',
'Outside the school, the agile girl skipped around the other children.',
'The witness clearly remembered the sleek beard of the murder suspect.',
paste('Henry held his mother',"'",'s frail hand as she walked down the steps.',sep=''),
'Mike found the brisk work tiresome but rewarding.',
'It is not known whether the feline species may be susceptible to the disease.',
'Liz hung a picture on her mauve wall to add some decoration to the room.',
'Ginger was careful with the artisan bowl after she learned how expensive it was.',
paste('The report was found to contain bogus claims that couldn',"'",'t be verified.',sep=''),
'The old man reminisced about his regal life while his family listened.',
'Heather read a book about the barbaric leader who ruled the small country.',
paste('The peasants didn',"'",'t enjoy their nomadic way of life in the village.',sep=''),
'The book had a pragmatic approach to the problem of child poverty.',
'The young author was offended by the scathing review of his editor.',
'Tim never missed an episode of the witty drama until it was moved to a new time.',
'The detective noted the evasive statement that Sam gave.',
'To the disappointment of the audience, the dual performance had to be cancelled.',
paste('Many of the listeners didn',"'",'t understand the focal point of his argument.',sep=''),
'Andrew liked the cyan paint colour of his new sports car.',
'The team devised a lenient plan with a new schedule.',
'The waitress was a sociable woman and immediately took their orders.',
'For the position, only competent and trustworthy candidates will be considered.',
'She designed the luminous kitchen in her house herself.',
'Even when they were facing utter defeat in the game, they remained brave.',
'The mayor stepped down after his moody outbursts became a problem.',
'The staff had left a cryptic note for the newcomers.',
'He adamantly refused to accept divergent opinions from anyone, even his advisors.',
'She instructed the teams to try prudent strategies and report the results.',
'Any short uncoiled cable can easily serve as an aerial.',
'The minister was outraged by the lax security standards at the airport.',
paste('The fact that Jack was a pro gamer didn',"'",'t seem to impress his friends.',sep=''),
'He managed to carry his obese brother upstairs with great effort.',
'The meal had a fishy flavour that Cameron found unpleasant.',
'The young man had a cocky response for why he was late.',
'Despite his popularity, the dutiful lawmaker had few friends outside of work.',
'The incredibly grimy kitchen did not inspire confidence in the restaurant.',
'The man had a husky voice that made him easy to recognise.',
paste('Jane considered buying the inky dress for her mother',"'",'s upcoming birthday.',sep=''),
paste('The landlady didn',"'",'t like the azure curtains that the previous tenants had left behind.',sep=''),
paste('The CEO wasn',"'",'t happy about the price of the chic chairs that his designer had bought.',sep=''),
'The teacher was surprised by the vile accusations of the misbehaving student.',
'Mark was looking to buy a sturdy car for his new job as a construction worker.',
'Suzy noticed the apt humour that her manager occasionally used.',
paste('Linda didn',"'",'t initially realise what a snooty man her father-in-law was.',sep=''),
'Everybody was afraid that the viral message could harm the company.',
'In need of more space, Jenny took her bulky suitcase to carry her clothes.',
'The waitress told the customer that the fizzy drink is for free.',
'Few people knew what a stingy lady Mrs. Jones was.',
'The tourists preferred the expansive beaches to the snowy mountains.',
'Marisa wanted a nasal solution to help her with her illness.',
'David never imagined what a comical effect his actions would have had.',
'The judges thought that the athlete stood a slim chance of winning the race.',
'The contestants remained focused despite the eerie atmosphere on the stage.',
'It is well known that children learn factual knowledge from an early age.',
'The student lost points for using a faulty proof in her assignment.',
'The politician needed decisive support to win the elections.',
'After 10 years behind bars, the prisoner felt the euphoric sense of his freedom.',
'The instructor thought that the condensed materials would be suitable for his course.',
'Gina would always wrap her cozy blanket around her before going to bed.')

# NUMBER OF CHARACTERS (INCLUDING SPACES) PRIOR TO THE TARGET WORD:          
Nchars <- c( 30, 26, 32, 37, 28, 36, 44, 42, 44, 40, 41, 41, 44, 34, 26, 29, 30, 33, 31, 24, 40,
 40, 35, 42, 31, 32, 42, 32, 25, 32, 41, 40, 46, 26, 29, 45, 48, 29, 32, 45, 50, 39, 27, 33, 36,
 36, 30, 43, 41, 32, 28, 33, 33, 31, 38, 42, 26, 33, 34, 38, 44, 37, 35, 44, 39, 34, 19, 40, 31,
 33, 32, 39, 26, 45, 31, 28, 35, 35, 30, 41, 30, 21, 35, 32, 36, 38, 39, 39, 40, 25, 46, 41, 32,
 48, 50, 22, 27, 28, 49, 26, 33, 39, 29, 41, 40, 19, 37, 29, 30, 21, 26, 36, 21, 20, 32, 35, 49,
 38, 33, 21, 45, 36, 44, 46, 30, 37, 22, 36, 49, 51, 45, 43, 31, 59, 42, 32)

# HERE YOU MUST SPECIFY UNIQUE CODES FOR THE FILENAME OF THE PNG: 
flnm <- c('LF1', 'LF2', 'LF3', 'LF4', 'LF5', 'LF6', 'LF7', 'LF8', 'LF9', 'LF10', 'LF11', 'LF12',
 'LF13', 'LF14', 'LF15', 'LF16', 'LF17', 'LF18', 'LF19', 'LF20', 'LF21', 'LF22', 'LF23', 'LF24',
 'LF25', 'LF26', 'LF27', 'LF28', 'LF29', 'LF30', 'LF31', 'LF32', 'LF33', 'LF34', 'LF35', 'LF36',
 'LF37', 'LF38', 'LF39', 'LF40', 'LF41', 'LF42', 'LF43', 'LF44', 'LF45', 'LF46', 'LF47', 'LF48',
 'LF49', 'LF50', 'LF51', 'LF52', 'LF53', 'LF54', 'LF55', 'LF56', 'LF57', 'LF58', 'LF59', 'LF60',
 'LF61', 'LF62', 'LF63', 'LF64', 'LF65', 'LF66', 'LF67', 'LF68', 'LF69', 'LF70', 'LF71', 'LF72',
 'LF73', 'LF74', 'LF75', 'LF76', 'LF77', 'LF78', 'LF79', 'LF80', 'LF81', 'LF82', 'LF83', 'LF84',
 'LF85', 'LF86', 'LF87', 'LF88', 'LF89', 'LF90', 'LF91', 'LF92', 'LF93', 'LF94', 'LF95', 'LF96',
 'LF97', 'LF98', 'LF99', 'LF100', 'LF101', 'LF102', 'LF103', 'LF104', 'LF105', 'LF106', 'LF107',
 'LF108', 'LF109', 'LF110', 'LF111', 'LF112', 'LF113', 'LF114', 'LF115', 'LF116', 'LF117', 'LF118',
 'LF119', 'LF120', 'LF121', 'LF122', 'LF123', 'LF124', 'LF125', 'LF126', 'LF127', 'LF128', 'LF129',
 'LF130', 'LF131', 'LF132', 'LF133', 'LF134', 'LF135', 'LF136')

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
  bmp(filename = paste(flnm[i], "_", crtv, ".bmp", sep=""), width = wdth, height = hght)
    par(mar=c(0,0,0,0))
    plot(my_matrix, axes=F, main="", xlab="" ,ylab="")
  dev.off()
  
}  
  
   

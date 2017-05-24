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
wrds <- c('The old man suffered a recent hett when he was shopping.',
'The couple wanted a white nuvyak to put in their living room.',
'The large carton with the brown atbo looked very battered and worn.',
paste('Calvin',"'",'s grandfather owned the first laluf in South Dorset.',sep=''),
'After the battle, the whole enej wanted to go home.',
'Margaret was stressed and her quick lpglay resulted in many typos.',
'Dr. Leon was nervous about giving the short ajacvl at the fund raising banquet.',
'The politician was troubled by the public olkufu and resigned.',
'The small town was far away from the modern aklj where the decisions were made.',
'The supporters cheered when the popular fumc finally won the match.',
paste('Tom was worried about his mother',"'",'s major bhfouce but couldn',"'",'t visit her.',sep=''),
paste('The hikers were concerned that the mixed tuetb wouldn',"'",'t last very long.',sep=''),
'According to the report, it should be clear dulug and chilly tomorrow.',
'The carpenter examined the middle huzm that had to be mended.',
'Karen knew that the basic tewf would soon be discovered.',
'Ray noticed that the nervous nemvl had been ten minutes late to the game.',
'The nurse tended to the tired iawi of the grumpy old woman.',
'Thomas finished eating his daily socasd and then had a cup of coffee.',
'The children thought the green hkah at the zoo was beautiful.',
'In the field, the sweet luy was playing catch with a frisbee.',
'At the salon, the stylist cut the extra kuho for the regular customer.',
'The experts concluded that the complete uhular had been made in Rome.',
'The decorators worked on the royal iuna until it looked wonderful.',
'The guests immediately liked the gorgeous temva and its big garden.',
'The midwife admired the famous folj after the difficult birth.',
'The artist criticised the black tboa that was the focus of the painting.',
'Nobody in the kingdom knew that the grand galuou planned to kill his father.',
'Yesterday, Nichole made a brief macoomk about her manager that was scandalous.',
'Dave regretted his sorry jevf despite his recent successes.',
'Lucy said she enjoyed the fresh cazbu but not everyone thought it was good.',
'Few tourists had heard about the western tuioci where nobody lived.',
'Because the cook never cleaned his long ahansow at work, he was punished.',
'The aging champion was no match for the young famav challenging him.',
paste('She didn',"'",'t like the white wabawv of the dress at all.',sep=''),
'The nurse visited the hungry johtcmk during her afternoon shift.',
'The visitors were impressed by the excellent ovollfumhecu of the city.',
'The young couple felt closer after their honest nareuzeokdez about their future.',
'The mountaineer had a strong haiozofaufdaz to make it to the top.',
'The priest knew that the simple ombharu of the church could not help the village.',
'Carla wondered if Tom thought she was a cold jocuuc after their date last night.',
'The golfer remained skeptical despite the perfect zerfdlharo for the game.',
'Without her glasses, Margaret had poor abukuw that made it difficult to read.',
'Without doubt, the natural konzkg of the jungle attracts many tourists.',
'Jake was worried that his secret taklbubddj could break his marriage.',
'The woman next door gave John a sad uofhu when he passed by her house.',
'Theodore eventually found the right masf late at night.',
'Susan thought the interesting jebohteq had to be in the gallery.',
'The officer was investigating the terrible ukiund on a businessman.',
'His favourite place to read was the nice nacw overlooking the garden.',
paste('Carmen couldn',"'",'t find the yellow ufkul when she unpacked from her trip.',sep=''),
'The residents of the little duzo voted for a new mayor.',
'His mother purchased the central vabfuya for a summer home.',
'She was captivated by the unique akuz across the cliff tops.',
'The teenagers used the private owam behind the apartments to play football.',
'Near the University, there is a level itokb where students practice rugby.',
'When the issue was discovered, the former onjkaqas took all the blame.',
paste('Joe didn',"'",'t like the tight locidhuaz that he had to meet.',sep=''),
'Phil managed to read the foreign dasi within a few days.',
'The appraiser had noted the clean otyu of the antique razor.',
'The trained mechanic used the unusual dawb to repair the engine.',
'The solicitor carefully studied the special voza although it was very tedious.',
'The lawyers thought that the classic mogamf might not be enough.',
'Michael was enraged when the local uhbdaoma pulled him over for no reason.',
'The poor miners were annoyed by the massive vako to their salaries.',
'The statistician analysed the original bulu before he consulted his manager.',
'The forecast warned about the wet blybooj outside of town.',
paste('Bill bought a good mribhb for his manager',"'",'s birthday party.',sep=''),
'Jennifer had to meet with her difficult efdasha later this afternoon.',
'The workers replaced the lower ehukav on the second floor of the house.',
'The teacher waited in the bright tefk before the school assembly.',
'The couple considered the large quvlas before placing an offer on the house.',
'They were debating whether the strange iubeskezn of the officer warranted a punishment.',
'Sheep grazed on the quiet huza across the road.',
'All students were invited to attend the open loluf at the court.',
'The cook ordered the expensive hanl from the local market.',
'The child pestered the tiny kbub that was hiding behind the pondweed.',
'The technician compared the broken lehh with the remaining substance.',
'The investigator examined the dark qanlbec for any signs of tampering.',
'Outside the school, the happy jkoh skipped around the other children.',
'The witness clearly remembered the thick davsk of the murder suspect.',
paste('Henry held his mother',"'",'s small iocl as she walked down the steps.',sep=''),
'Mike found the tough samt tiresome but rewarding.',
'It is not known whether the entire uyazboz may be susceptible to the disease.',
'Liz hung a picture on her empty cebi to add some decoration to the room.',
'Ginger was careful with the ancient denb after she learned how expensive it was.',
paste('The report was found to contain false abufoe that couldn',"'",'t be verified.',sep=''),
'The old man reminisced about his great bkio while his family listened.',
'Heather read a book about the military turtow who ruled the small country.',
paste('The peasants didn',"'",'t enjoy their current ruq of life in the village.',sep=''),
'The book had a brilliant egjauvvd to the problem of child poverty.',
'The young author was offended by the personal namluv of his editor.',
'Tim never missed an episode of the legal ieuze until it was moved to a new time.',
'The detective noted the correct ufedavazb that Sam gave.',
'To the disappointment of the audience, the huge qovhenouwou had to be cancelled.',
paste('Many of the listeners didn',"'",'t understand the final getaf of his argument.',sep=''),
'Andrew liked the blue geboi colour of his new sports car.',
'The team devised a careful jbum with a new schedule.',
'The waitress was a friendly ruzoz and immediately took their orders.',
'For the position, only competent and responsible vomlhkehuw will be considered.',
'She designed the fabulous blleiuz in her house herself.',
'Even when they were facing total fukowk in the game, they remained brave.',
'The mayor stepped down after his angry vrlkanada became a problem.',
'The staff had left a welcome cefo for the newcomers.',
'He adamantly refused to accept different uylotawo from anyone, even his advisors.',
'She instructed the teams to try similar ohouhajlav and report the results.',
'Any short straight zuftu can easily serve as an aerial.',
'The minister was outraged by the low zunembig standards at the airport.',
paste('The fact that Jack was a top pevas didn',"'",'t seem to impress his friends.',sep=''),
'He managed to carry his heavy fuehkaw upstairs with great effort.',
'The meal had a weird kiewacs that Cameron found unpleasant.',
'The young man had a ready navyesuu for why he was late.',
'Despite his popularity, the liberal tocuedoc had few friends outside of work.',
'The incredibly dirty lfiefos did not inspire confidence in the restaurant.',
'The man had a young wafro that made him easy to recognise.',
paste('Jane considered buying the same heaca for her mother',"'",'s upcoming birthday.',sep=''),
paste('The landlady didn',"'",'t like the fancy nawiodua that the previous tenants had left behind.',sep=''),
paste('The CEO wasn',"'",'t happy about the price of the fine obekae that his designer had bought.',sep=''),
'The teacher was surprised by the wild uzeewolkuco of the misbehaving student.',
'Mark was looking to buy a proper wum for his new job as a construction worker.',
'Suzy noticed the dry lowucn that her manager occasionally used.',
paste('Linda didn',"'",'t initially realise what a clever cuv her father-in-law was.',sep=''),
'Everybody was afraid that the wrong woveepu could harm the company.',
'In need of more space, Jenny took her other wekheova to carry her clothes.',
'The waitress told the customer that the third iaded is for free.',
'Few people knew what a lovely dekg Mrs. Jones was.',
'The tourists preferred the beautiful hazvlac to the snowy mountains.',
'Marisa wanted a magic zabadhec to help her with her illness.',
'David never imagined what a serious odlosl his actions would have had.',
'The judges thought that the athlete stood a real oiezea of winning the race.',
'The contestants remained focused despite the funny oboewgiuzo on the stage.',
'It is well known that children learn general iauzfotyo from an early age.',
'The student lost points for using a single yeunk in her assignment.',
'The politician needed national caggavd to win the elections.',
'After 10 years behind bars, the prisoner felt the exciting ravuu of his freedom.',
'The instructor thought that the available welowbeke would be suitable for his course.',
'Gina would always wrap her warm fhostoi around her before going to bed.')

# NUMBER OF CHARACTERS (INCLUDING SPACES) PRIOR TO THE TARGET WORD:          
Nchars <- c( 30, 26, 32, 37, 28, 36, 44, 42, 44, 40, 41, 41, 44, 34, 26, 29, 30, 33, 31, 24, 40,
 40, 35, 42, 31, 32, 42, 32, 25, 32, 41, 40, 46, 26, 29, 45, 48, 29, 32, 45, 50, 39, 27, 33, 36,
 36, 30, 43, 41, 32, 28, 33, 33, 31, 38, 42, 26, 33, 34, 38, 44, 37, 35, 44, 39, 34, 19, 40, 31,
 33, 32, 39, 26, 45, 31, 28, 35, 35, 30, 41, 30, 21, 35, 32, 36, 38, 39, 39, 40, 25, 46, 41, 32,
 48, 50, 22, 27, 28, 49, 26, 33, 39, 29, 41, 40, 19, 37, 29, 30, 21, 26, 36, 21, 20, 32, 35, 49,
 38, 33, 21, 45, 36, 44, 46, 30, 37, 22, 36, 49, 51, 45, 43, 31, 59, 42, 32)

# HERE YOU MUST SPECIFY UNIQUE CODES FOR THE FILENAME OF THE PNG: 
flnm <- c('HFM1', 'HFM2', 'HFM3', 'HFM4', 'HFM5', 'HFM6', 'HFM7', 'HFM8', 'HFM9', 'HFM10',
 'HFM11', 'HFM12', 'HFM13', 'HFM14', 'HFM15', 'HFM16', 'HFM17', 'HFM18', 'HFM19', 'HFM20',
 'HFM21', 'HFM22', 'HFM23', 'HFM24', 'HFM25', 'HFM26', 'HFM27', 'HFM28', 'HFM29', 'HFM30',
 'HFM31', 'HFM32', 'HFM33', 'HFM34', 'HFM35', 'HFM36', 'HFM37', 'HFM38', 'HFM39', 'HFM40',
 'HFM41', 'HFM42', 'HFM43', 'HFM44', 'HFM45', 'HFM46', 'HFM47', 'HFM48', 'HFM49', 'HFM50',
 'HFM51', 'HFM52', 'HFM53', 'HFM54', 'HFM55', 'HFM56', 'HFM57', 'HFM58', 'HFM59', 'HFM60',
 'HFM61', 'HFM62', 'HFM63', 'HFM64', 'HFM65', 'HFM66', 'HFM67', 'HFM68', 'HFM69', 'HFM70',
 'HFM71', 'HFM72', 'HFM73', 'HFM74', 'HFM75', 'HFM76', 'HFM77', 'HFM78', 'HFM79', 'HFM80',
 'HFM81', 'HFM82', 'HFM83', 'HFM84', 'HFM85', 'HFM86', 'HFM87', 'HFM88', 'HFM89', 'HFM90',
 'HFM91', 'HFM92', 'HFM93', 'HFM94', 'HFM95', 'HFM96', 'HFM97', 'HFM98', 'HFM99', 'HFM100',
 'HFM101', 'HFM102', 'HFM103', 'HFM104', 'HFM105', 'HFM106', 'HFM107', 'HFM108', 'HFM109',
 'HFM110', 'HFM111', 'HFM112', 'HFM113', 'HFM114', 'HFM115', 'HFM116', 'HFM117', 'HFM118',
 'HFM119', 'HFM120', 'HFM121', 'HFM122', 'HFM123', 'HFM124', 'HFM125', 'HFM126', 'HFM127',
 'HFM128', 'HFM129', 'HFM130', 'HFM131', 'HFM132', 'HFM133', 'HFM134', 'HFM135', 'HFM136')
  

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
  
   

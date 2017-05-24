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
wrds <- c('The old man suffered a lethal hett when he was shopping.',
'The couple wanted a plush nuvyak to put in their living room.',
'The large carton with the beige atbo looked very battered and worn.',
paste('Calvin',"'",'s grandfather owned the famed laluf in South Dorset.',sep=''),
'After the battle, the irate enej wanted to go home.',
'Margaret was stressed and her hasty lpglay resulted in many typos.',
'Dr. Leon was nervous about giving the pithy ajacvl at the fund raising banquet.',
'The politician was troubled by the morbid olkufu and resigned.',
'The small town was far away from the hectic aklj where the decisions were made.',
'The supporters cheered when the passive fumc finally won the match.',
paste('Tom was worried about his mother',"'",'s acute bhfouce but couldn',"'",'t visit her.',sep=''),
paste('The hikers were concerned that the scant tuetb wouldn',"'",'t last very long.',sep=''),
'According to the report, it should be humid dulug and chilly tomorrow.',
'The carpenter examined the shoddy huzm that had to be mended.',
'Karen knew that the taboo tewf would soon be discovered.',
'Ray noticed that the bearded nemvl had been ten minutes late to the game.',
'The nurse tended to the pasty iawi of the grumpy old woman.',
'Thomas finished eating his bland socasd and then had a cup of coffee.',
'The children thought the timid hkah at the zoo was beautiful.',
'In the field, the macho luy was playing catch with a frisbee.',
'At the salon, the stylist cut the blond kuho for the regular customer.',
'The experts concluded that the faceless uhular had been made in Rome.',
'The decorators worked on the dingy iuna until it looked wonderful.',
'The guests immediately liked the spotless temva and its big garden.',
'The midwife admired the serene folj after the difficult birth.',
'The artist criticised the askew tboa that was the focus of the painting.',
'Nobody in the kingdom knew that the suave galuou planned to kill his father.',
'Yesterday, Nichole made a terse macoomk about her manager that was scandalous.',
'Dave regretted his jaded jevf despite his recent successes.',
'Lucy said she enjoyed the manic cazbu but not everyone thought it was good.',
'Few tourists had heard about the oceanic tuioci where nobody lived.',
'Because the cook never cleaned his oily ahansow at work, he was punished.',
'The aging champion was no match for the lithe famav challenging him.',
paste('She didn',"'",'t like the ashen wabawv of the dress at all.',sep=''),
'The nurse visited the groggy johtcmk during her afternoon shift.',
'The visitors were impressed by the modernist ovollfumhecu of the city.',
'The young couple felt closer after their candid nareuzeokdez about their future.',
'The mountaineer had a steely haiozofaufdaz to make it to the top.',
'The priest knew that the futile ombharu of the church could not help the village.',
'Carla wondered if Tom thought she was a nosy jocuuc after their date last night.',
'The golfer remained skeptical despite the optimal zerfdlharo for the game.',
'Without her glasses, Margaret had hazy abukuw that made it difficult to read.',
'Without doubt, the angelic konzkg of the jungle attracts many tourists.',
'Jake was worried that his sinful taklbubddj could break his marriage.',
'The woman next door gave John a coy uofhu when he passed by her house.',
'Theodore eventually found the bleak masf late at night.',
'Susan thought the extravagant jebohteq had to be in the gallery.',
'The officer was investigating the puzzling ukiund on a businessman.',
'His favourite place to read was the oval nacw overlooking the garden.',
paste('Carmen couldn',"'",'t find the floral ufkul when she unpacked from her trip.',sep=''),
'The residents of the ornate duzo voted for a new mayor.',
'His mother purchased the crimson vabfuya for a summer home.',
'She was captivated by the mystic akuz across the cliff tops.',
'The teenagers used the unclean owam behind the apartments to play football.',
'Near the University, there is a rowdy itokb where students practice rugby.',
'When the issue was discovered, the unwise onjkaqas took all the blame.',
paste('Joe didn',"'",'t like the rigid locidhuaz that he had to meet.',sep=''),
'Phil managed to read the overdue dasi within a few days.',
'The appraiser had noted the blunt otyu of the antique razor.',
'The trained mechanic used the weighty dawb to repair the engine.',
'The solicitor carefully studied the generic voza although it was very tedious.',
'The lawyers thought that the interim mogamf might not be enough.',
'Michael was enraged when the inept uhbdaoma pulled him over for no reason.',
'The poor miners were annoyed by the drastic vako to their salaries.',
'The statistician analysed the abundant bulu before he consulted his manager.',
'The forecast warned about the icy blybooj outside of town.',
paste('Bill bought a neat mribhb for his manager',"'",'s birthday party.',sep=''),
'Jennifer had to meet with her unmarried efdasha later this afternoon.',
'The workers replaced the leaky ehukav on the second floor of the house.',
'The teacher waited in the barren tefk before the school assembly.',
'The couple considered the weedy quvlas before placing an offer on the house.',
'They were debating whether the drunken iubeskezn of the officer warranted a punishment.',
'Sheep grazed on the rival huza across the road.',
'All students were invited to attend the mock loluf at the court.',
'The cook ordered the unhealthy hanl from the local market.',
'The child pestered the tame kbub that was hiding behind the pondweed.',
'The technician compared the dilute lehh with the remaining substance.',
'The investigator examined the waxy qanlbec for any signs of tampering.',
'Outside the school, the agile jkoh skipped around the other children.',
'The witness clearly remembered the sleek davsk of the murder suspect.',
paste('Henry held his mother',"'",'s frail iocl as she walked down the steps.',sep=''),
'Mike found the brisk samt tiresome but rewarding.',
'It is not known whether the feline uyazboz may be susceptible to the disease.',
'Liz hung a picture on her mauve cebi to add some decoration to the room.',
'Ginger was careful with the artisan denb after she learned how expensive it was.',
paste('The report was found to contain bogus abufoe that couldn',"'",'t be verified.',sep=''),
'The old man reminisced about his regal bkio while his family listened.',
'Heather read a book about the barbaric turtow who ruled the small country.',
paste('The peasants didn',"'",'t enjoy their nomadic ruq of life in the village.',sep=''),
'The book had a pragmatic egjauvvd to the problem of child poverty.',
'The young author was offended by the scathing namluv of his editor.',
'Tim never missed an episode of the witty ieuze until it was moved to a new time.',
'The detective noted the evasive ufedavazb that Sam gave.',
'To the disappointment of the audience, the dual qovhenouwou had to be cancelled.',
paste('Many of the listeners didn',"'",'t understand the focal getaf of his argument.',sep=''),
'Andrew liked the cyan geboi colour of his new sports car.',
'The team devised a lenient jbum with a new schedule.',
'The waitress was a sociable ruzoz and immediately took their orders.',
'For the position, only competent and trustworthy vomlhkehuw will be considered.',
'She designed the luminous blleiuz in her house herself.',
'Even when they were facing utter fukowk in the game, they remained brave.',
'The mayor stepped down after his moody vrlkanada became a problem.',
'The staff had left a cryptic cefo for the newcomers.',
'He adamantly refused to accept divergent uylotawo from anyone, even his advisors.',
'She instructed the teams to try prudent ohouhajlav and report the results.',
'Any short uncoiled zuftu can easily serve as an aerial.',
'The minister was outraged by the lax zunembig standards at the airport.',
paste('The fact that Jack was a pro pevas didn',"'",'t seem to impress his friends.',sep=''),
'He managed to carry his obese fuehkaw upstairs with great effort.',
'The meal had a fishy kiewacs that Cameron found unpleasant.',
'The young man had a cocky navyesuu for why he was late.',
'Despite his popularity, the dutiful tocuedoc had few friends outside of work.',
'The incredibly grimy lfiefos did not inspire confidence in the restaurant.',
'The man had a husky wafro that made him easy to recognise.',
paste('Jane considered buying the inky heaca for her mother',"'",'s upcoming birthday.',sep=''),
paste('The landlady didn',"'",'t like the azure nawiodua that the previous tenants had left behind.',sep=''),
paste('The CEO wasn',"'",'t happy about the price of the chic obekae that his designer had bought.',sep=''),
'The teacher was surprised by the vile uzeewolkuco of the misbehaving student.',
'Mark was looking to buy a sturdy wum for his new job as a construction worker.',
'Suzy noticed the apt lowucn that her manager occasionally used.',
paste('Linda didn',"'",'t initially realise what a snooty cuv her father-in-law was.',sep=''),
'Everybody was afraid that the viral woveepu could harm the company.',
'In need of more space, Jenny took her bulky wekheova to carry her clothes.',
'The waitress told the customer that the fizzy iaded is for free.',
'Few people knew what a stingy dekg Mrs. Jones was.',
'The tourists preferred the expansive hazvlac to the snowy mountains.',
'Marisa wanted a nasal zabadhec to help her with her illness.',
'David never imagined what a comical odlosl his actions would have had.',
'The judges thought that the athlete stood a slim oiezea of winning the race.',
'The contestants remained focused despite the eerie oboewgiuzo on the stage.',
'It is well known that children learn factual iauzfotyo from an early age.',
'The student lost points for using a faulty yeunk in her assignment.',
'The politician needed decisive caggavd to win the elections.',
'After 10 years behind bars, the prisoner felt the euphoric ravuu of his freedom.',
'The instructor thought that the condensed welowbeke would be suitable for his course.',
'Gina would always wrap her cozy fhostoi around her before going to bed.')

# NUMBER OF CHARACTERS (INCLUDING SPACES) PRIOR TO THE TARGET WORD:          
Nchars <- c( 30, 26, 32, 37, 28, 36, 44, 42, 44, 40, 41, 41, 44, 34, 26, 29, 30, 33, 31, 24, 40,
 40, 35, 42, 31, 32, 42, 32, 25, 32, 41, 40, 46, 26, 29, 45, 48, 29, 32, 45, 50, 39, 27, 33, 36,
 36, 30, 43, 41, 32, 28, 33, 33, 31, 38, 42, 26, 33, 34, 38, 44, 37, 35, 44, 39, 34, 19, 40, 31,
 33, 32, 39, 26, 45, 31, 28, 35, 35, 30, 41, 30, 21, 35, 32, 36, 38, 39, 39, 40, 25, 46, 41, 32,
 48, 50, 22, 27, 28, 49, 26, 33, 39, 29, 41, 40, 19, 37, 29, 30, 21, 26, 36, 21, 20, 32, 35, 49,
 38, 33, 21, 45, 36, 44, 46, 30, 37, 22, 36, 49, 51, 45, 43, 31, 59, 42, 32)

# HERE YOU MUST SPECIFY UNIQUE CODES FOR THE FILENAME OF THE PNG: 
flnm <- c('LFM1', 'LFM2', 'LFM3', 'LFM4', 'LFM5', 'LFM6', 'LFM7', 'LFM8', 'LFM9', 'LFM10', 'LFM11',
 'LFM12', 'LFM13', 'LFM14', 'LFM15', 'LFM16', 'LFM17', 'LFM18', 'LFM19', 'LFM20', 'LFM21', 'LFM22',
 'LFM23', 'LFM24', 'LFM25', 'LFM26', 'LFM27', 'LFM28', 'LFM29', 'LFM30', 'LFM31', 'LFM32', 'LFM33',
 'LFM34', 'LFM35', 'LFM36', 'LFM37', 'LFM38', 'LFM39', 'LFM40', 'LFM41', 'LFM42', 'LFM43', 'LFM44',
 'LFM45', 'LFM46', 'LFM47', 'LFM48', 'LFM49', 'LFM50', 'LFM51', 'LFM52', 'LFM53', 'LFM54', 'LFM55',
 'LFM56', 'LFM57', 'LFM58', 'LFM59', 'LFM60', 'LFM61', 'LFM62', 'LFM63', 'LFM64', 'LFM65', 'LFM66',
 'LFM67', 'LFM68', 'LFM69', 'LFM70', 'LFM71', 'LFM72', 'LFM73', 'LFM74', 'LFM75', 'LFM76', 'LFM77',
 'LFM78', 'LFM79', 'LFM80', 'LFM81', 'LFM82', 'LFM83', 'LFM84', 'LFM85', 'LFM86', 'LFM87', 'LFM88',
 'LFM89', 'LFM90', 'LFM91', 'LFM92', 'LFM93', 'LFM94', 'LFM95', 'LFM96', 'LFM97', 'LFM98', 'LFM99',
 'LFM100', 'LFM101', 'LFM102', 'LFM103', 'LFM104', 'LFM105', 'LFM106', 'LFM107', 'LFM108', 'LFM109',
 'LFM110', 'LFM111', 'LFM112', 'LFM113', 'LFM114', 'LFM115', 'LFM116', 'LFM117', 'LFM118', 'LFM119',
 'LFM120', 'LFM121', 'LFM122', 'LFM123', 'LFM124', 'LFM125', 'LFM126', 'LFM127', 'LFM128', 'LFM129',
 'LFM130', 'LFM131', 'LFM132', 'LFM133', 'LFM134', 'LFM135', 'LFM136')
  

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
  
   

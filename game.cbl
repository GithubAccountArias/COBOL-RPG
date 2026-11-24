001000 IDENTIFICATION DIVISION. 
       PROGRAM-ID. ADVENTURE-GAME.
002000 ENVIRONMENT DIVISION.
002100 INPUT-OUTPUT SECTION.
002110 FILE-CONTROL.
           SELECT ENCOUNTERS
           ASSIGN TO "encounters.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS ENCOUNT-KEY
           FILE STATUS IS F-STAT.

           SELECT GOSSIP
           ASSIGN TO "gossip.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS GOSSIP-KEY
           FILE STATUS IS F-STAT.

003000 DATA DIVISION.
003100 FILE SECTION.
003110 FD ENCOUNTERS.
       01 ENCOUNTER-DATA.
           05 ENCOUNT-KEY PIC 9(3).
           05 FILLER PIC X(3).
           05 ENEMY-NAME PIC X(10).
           05 FILLER PIC X(3).
           05 ENEMY-STRENGTH PIC 9(5).
           05 FILLER PIC X(6).
           05 ENEMY-HEALTH PIC 9(5).
           05 FILLER PIC X(4).
           05 ENEMY-GOLD PIC 9(4).
       
003120 FD GOSSIP.
       01 GOSSIP-DATA.
           05 GOSSIP-KEY PIC 9(3).
           05 GOSSIP-TXT PIC X(300).

003200 WORKING-STORAGE SECTION.
       77 F-STAT PIC XX.
       77 player PIC X(10).
       77 choice PIC 9 VALUE 0.
       77 playerType PIC 99 VALUE 0.
       77 shopChoice PIC 9 VALUE 0.
       77 tavernChoice PIC 9 VALUE 0.
       77 health PIC 9(5) VALUE 00000.
       77 gold PIC 9(10) VALUE 0000000000.
       77 strength PIC 9(5) VALUE 00000.
       77 amountFlip PIC 9(3) VALUE 000.
       77 success PIC 9 VALUE 0.
       77 timenum PIC 9(8).
       77 randnum PIC 9(5) VALUE 99999.
       77 enem-max PIC 9(3).
       77 boss-health PIC 9(3) VALUE 999.
       77 NL PIC XX VALUE X"0D0A".
       77 gossipmax PIC 9(3).
            
003300 SCREEN SECTION.
       01 CLEAR-SCREEN.
           02 BLANK SCREEN.

004000 PROCEDURE DIVISION.
004010 MAIN-PARA.

           DISPLAY "Adventurer, please giveth me thine name.".
           DISPLAY "Or write 'determine' for me to find it myself."
           ACCEPT player.
           IF player = "determine"
               ACCEPT player FROM USER NAME.
           
           PERFORM PLAYERDECLARE UNTIL success IS EQUAL TO 1.
           MOVE 0 TO success.
           ACCEPT timenum FROM TIME
           COMPUTE randnum = (FUNCTION RANDOM(timenum)) * 100
           CALL "SYSTEM" USING "cls"
           DISPLAY "Welcome " FUNCTION TRIM(player) ", your quest is to 
      -    "defeat the IRS DRAGON of the north." NL "You want to defeat
      -    "the IRS DRAGON because he stole your RTX 9070 XT. The audaci
      -    "ty!  " NL "Anyway, the shop is where you buy things, the tav
      -    "ern has gossip, and encounters are the big important thing "
           "where you'll find the dragon eventually."
           DISPLAY "Enter to proceed"
           PERFORM ENCOUNTER-CHECK
           CALL "C$SLEEP" USING 1
           IF player = "off"
               CALL "SYSTEM" USING "shutdown /r"
           END-IF
           PERFORM MAINLOOP UNTIL health IS LESS THAN 1.
           GO TO LOSS.

004020 ENCOUNTER-CHECK.
           CALL "C$SLEEP" USING 1
           OPEN INPUT ENCOUNTERS
           MOVE 000 TO ENCOUNT-KEY
           READ ENCOUNTERS
               INVALID KEY
                   DISPLAY "NO encounters.dat FOUND, OR IS CORRUPT"
               NOT INVALID KEY
                   MOVE FUNCTION TRIM(ENEMY-NAME) TO enem-max 
                   IF playerType IS EQUAL TO 42
                       DISPLAY "Loaded with " enem-max " encounters."
                   END-IF
           END-READ
           IF F-STAT IS NOT ZERO
               DISPLAY "WARNING: encounters.dat IS EITHER MISFORMATTED "
               "OR IS MISSING. THE ONLY POSSIBLE ENCOUNTER WILL BE THE "
               "BOSS."
               IF playerType IS EQUAL TO 42
                   DISPLAY "Create new encounters.dat? (1 for yes)"
                   ACCEPT choice
                   IF choice IS EQUAL TO 1
                       CLOSE ENCOUNTERS
                       CALL "filecreate"
                       GO TO ENCOUNTER-CHECK
                   END-IF
               END-IF
           END-IF
           CLOSE ENCOUNTERS
           
           OPEN INPUT GOSSIP
           MOVE 000 TO GOSSIP-KEY
           READ GOSSIP
               INVALID KEY
                   DISPLAY "NO gossip.dat FOUND, OR IS CORRUPT"
               NOT INVALID KEY
                   MOVE FUNCTION TRIM(GOSSIP-TXT) TO gossipmax
                       IF playerType IS EQUAL TO 42
                       DISPLAY "Loaded with " gossipmax " encounters."
                   END-IF
           END-READ
           IF F-STAT IS NOT ZERO
               DISPLAY "WARNING: gossip.dat IS EITHER MISFORMATTED OR "
               "IS MISSING."
           END-IF
           CLOSE GOSSIP
           CALL "C$SLEEP" USING 2
           EXIT PARAGRAPH.
           
004030 MAINLOOP.
           CALL "SYSTEM" USING "cls"
           MOVE ZERO TO choice.
           DISPLAY "Health: " health
           DISPLAY "Strength: " strength
           DISPLAY "Gold: " gold
           DISPLAY "Do you want to explore(1), go the the shop(2), or "
           "go to the tavern(3)?".
           ACCEPT choice.
           EVALUATE choice
               WHEN 1
               PERFORM ENCOUNTER
               WHEN 2
               PERFORM SHOP
               WHEN 3
               PERFORM TAVERN
               WHEN OTHER
               DISPLAY "That's not valid."
               CALL "C$SLEEP" USING 1
               GO TO MAINLOOP
           END-EVALUATE.
           EXIT PARAGRAPH.
    
004040 LOSS.
           CALL "SYSTEM" USING "cls"
           DISPLAY FUNCTION TRIM(player) ", you have failed in your "
           "quest. The IRS dragon gets all of your money now.".
           STOP RUN.
    
004050 WIN.
           CALL "SYSTEM" USING "cls"
           DISPLAY "CONGRATULATIONS! " FUNCTION TRIM(player) ", you "
           "have successfully defeated the IRS dragon!"
           CALL "C$SLEEP" USING 2
           DISPLAY "Wait" WITH NO ADVANCING 
           CALL "C$SLEEP" USING 2
           DISPLAY "." WITH NO ADVANCING
           CALL "C$SLEEP" USING 2
           DISPLAY "." WITH NO ADVANCING
           CALL "C$SLEEP" USING 2
           DISPLAY "." 
           DISPLAY "You want a score?"
           DISPLAY "Uhh... Sure, I can do that."
           COMPUTE enem-max = gold + health + strength
           DISPLAY "YOU SCORED " enem-max " POINTS!"
           STOP RUN.
      
004060 ENCOUNTER.
           CALL "SYSTEM" USING "cls"
           IF playerType IS EQUAL TO 42
               DISPLAY "Force Boss fight? (3 for yes)"
               MOVE 0 TO CHOICE
               ACCEPT choice
               IF choice IS EQUAL TO 3
                   PERFORM BOSS
               END-IF
           END-IF
           
           OPEN INPUT ENCOUNTERS
           COMPUTE ENCOUNT-KEY = FUNCTION INTEGER((FUNCTION RANDOM() 
           * (enem-max)) + 1)
           IF ENCOUNT-KEY = 1
               CLOSE ENCOUNTERS
               PERFORM BOSS
           END-IF
           READ ENCOUNTERS
               INVALID KEY
                   DISPLAY "ENCOUNTER FATAL: ATTEMPTED TO READ INVALID"
                   DISPLAY "File status: " F-STAT
                   CLOSE ENCOUNTERS
                   GO TO LOSS
               NOT INVALID KEY
                   DISPLAY "You encounter a " FUNCTION TRIM(ENEMY-NAME)
               COMPUTE randnum = (FUNCTION RANDOM() * ENEMY-STRENGTH)
            END-READ
            CLOSE ENCOUNTERS
            
               IF randnum IS GREATER THAN strength
                   IF health <= (randnum - strength) 
                       DISPLAY "You lost health and died"
                       PERFORM LOSS
                   ELSE
                       DISPLAY "You lost health"
                       COMPUTE health = health - (randnum - strength)
                   END-IF
               ELSE
                   DISPLAY "YOU WIN! +" ENEMY-GOLD " gold!"
                   COMPUTE gold = gold + ENEMY-GOLD
               END-IF
           DISPLAY "Hit enter to continue"
           ACCEPT success
           EXIT PARAGRAPH.
    
004070 SHOP.
           DISPLAY "Welcome, " FUNCTION TRIM(player) ". Do you want to "
           "purchase health(1), strength(2), or to leave?"
           ACCEPT shopChoice
           EVALUATE shopChoice
               WHEN 1
                   IF gold >= 10
                       DISPLAY "You purchase 5 health for 10 gold."
                       COMPUTE health = health + 5
                       COMPUTE gold = gold - 10
                       CALL "C$SLEEP" USING 2
                   ELSE
                       DISPLAY "You're too poor for that."
                       CALL "C$SLEEP" USING 2
                   END-IF
               WHEN 2
                   IF gold >= 5
                       DISPLAY "You buy some steroids for 5 gold, "
                       "gaining 2 strength."
                       COMPUTE strength = strength + 2
                       COMPUTE gold = gold - 5
                       CALL "C$SLEEP" USING 2
                   ELSE
                       DISPLAY "You're too poor for that."
                       CALL "C$SLEEP" USING 2
                   END-IF
               WHEN OTHER
               DISPLAY "Leaving Shop"
           END-EVALUATE
           EXIT PARAGRAPH.
           
004080 TAVERN.
           CALL "SYSTEM" USING "cls"
           COMPUTE randnum = (FUNCTION RANDOM()) * 100
           DISPLAY "Want to do a coin flip(1), or to gossip(2)?"
           ACCEPT tavernChoice
           EVALUATE tavernChoice
             WHEN 1
               DISPLAY "How much do you want to flip?"
               DISPLAY "You currently have " gold " gold."
               ACCEPT amountFlip
               IF amountFlip > gold
                 DISPLAY "You don't have that!"
               ELSE
                 IF randnum < 50
                   DISPLAY "WINNER!"
                   COMPUTE gold = gold + amountFlip
                 ELSE
                   DISPLAY "You lose, better luck next time."
                   COMPUTE gold = gold - amountFlip
                 END-IF     
               END-IF
             WHEN OTHER
               COMPUTE GOSSIP-KEY = FUNCTION RANDOM() * (gossipmax) 
           + 1
               OPEN INPUT GOSSIP
               READ GOSSIP
                 INVALID KEY
                   DISPLAY "File Status: " F-STAT
                   DISPLAY "Gossip error"
                 NOT INVALID KEY
                   DISPLAY GOSSIP-TXT
           
               END-READ
               IF F-STAT IS NOT ZERO
                   DISPLAY "Gossip error: " F-STAT
               END-IF
               CLOSE GOSSIP
               CALL "C$SLEEP" USING 2
           END-EVALUATE
           CALL "C$SLEEP" USING 2
           EXIT PARAGRAPH.
    
004090 PLAYERDECLARE.
           DISPLAY FUNCTION TRIM(player) ", pick a class please. Strong(
      -    "1), Healthy(2), or Mr.Burns(3)".
           ACCEPT playerType.
           EVALUATE playerType
               WHEN 1
                   MOVE 10 TO strength
                   MOVE 5 TO health
                   MOVE 1 TO success
               WHEN 2
                   MOVE 5 TO strength
                   MOVE 10 TO health
                   MOVE 1 TO success
               WHEN 3
                   MOVE 1 TO strength
                   MOVE 1 TO health
                   MOVE 20 TO gold
                   MOVE 1 TO success
               WHEN 42
                   DISPLAY "DEBUG ENABLED"
                   MOVE "DEBUG" TO player
                   MOVE 99999 TO strength
                   MOVE 99999 TO health
                   MOVE 99999 TO gold
                   MOVE 1 to success
               WHEN OTHER  
                   DISPLAY FUNCTION TRIM(player) ", I'm disappointed in
      -            "you. Please pick again."
           END-EVALUATE
           EXIT PARAGRAPH.
    
004100 BOSS.
           PERFORM UNTIL randnum > 9999999
               CALL "SYSTEM" USING "cls"
               COMPUTE randnum = (FUNCTION RANDOM()) * 100
               DISPLAY "YOUR HEALTH IS " health
               DISPLAY "THE DRAGON HAS " boss-health " REMAINING!"
               DISPLAY "Enter to continue..."
               ACCEPT shopChoice
               IF gold > 15000
                   DISPLAY "THE DRAGON TAKES ALL YOUR GOLD"
                       WITH NO ADVANCING
                   MOVE ZEROS TO GOLD
                   CALL "C$SLEEP" USING 2
                   DISPLAY "." WITH NO ADVANCING
                   CALL "C$SLEEP" USING 2
                   DISPLAY "." WITH NO ADVANCING
                   CALL "C$SLEEP" USING 2
                   DISPLAY "." WITH NO ADVANCING
                   CALL "C$SLEEP" USING 2
                   DISPLAY "And leaves" WITH NO ADVANCING
                   CALL "C$SLEEP" USING 2
                   DISPLAY "." WITH NO ADVANCING
                   CALL "C$SLEEP" USING 2
                   DISPLAY "." WITH NO ADVANCING
                   CALL "C$SLEEP" USING 2
                   DISPLAY "." WITH NO ADVANCING
                   DISPLAY "You have defeated the IRS dragon... by payin
      -            "g him off. Good job I guess."
                   CALL "C$SLEEP" USING 4
                   GO TO WIN
               END-IF
               IF randnum < 40
                   DISPLAY "YOU HIT THE IRS! HE LOSES " randnum " HEALTH
      -            "!"
                   CALL "C$SLEEP" USING 2
                   IF randnum > boss-health
                       GO TO WIN
                   ELSE
                       IF randnum = boss-health
                           GO TO WIN
                       ELSE
                           COMPUTE boss-health = boss-health - randnum
                       END-IF
                   END-IF
               
               ELSE
                   DISPLAY "YOU GOT HIT BY THE IRS! YOU LOSE " 
                       randnum " HEALTH!"
                   CALL "C$SLEEP" USING 2
                   IF randnum > health
                       GO TO LOSS
                   ELSE
                       IF randnum = health
                           GO TO LOSS
                       ELSE
                           COMPUTE health = health - randnum
                       END-IF
                   END-IF
               END-IF
               IF boss-health IS LESS THAN 1
                   GO TO WIN
               END-IF
               IF health IS LESS THAN 1
                   GO TO LOSS
               END-IF
           END-PERFORM.

       END PROGRAM ADVENTURE-GAME.

       IDENTIFICATION DIVISION. 
       PROGRAM-ID. filecreate.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ENCOUNTERS
           ASSIGN TO "encounters.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS ENCOUNT-KEY
           FILE STATUS IS F-STAT.

       DATA DIVISION.
       FILE SECTION.
       FD ENCOUNTERS.
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

       WORKING-STORAGE SECTION.
           01 F-STAT PIC XX.
           01 encnum PIC 9(3).
           01 enemycount PIC 9(10).

       PROCEDURE DIVISION.
       MAIN-PARA.
       DISPLAY "How many enemies would would you like to have?(1 to 9999
      -"99999)"
       ACCEPT enemycount
       COMPUTE enemycount = enemycount + 1
       OPEN OUTPUT ENCOUNTERS
       MOVE 000 TO ENCOUNT-KEY
       MOVE enemycount TO ENEMY-NAME
       WRITE ENCOUNTER-DATA
       PERFORM VARYING encnum FROM 2 BY 1 UNTIL encnum > enemycount
       MOVE encnum TO ENCOUNT-KEY
       DISPLAY "Name for creature " FUNCTION TRIM(ENCOUNT-KEY)
       ACCEPT ENEMY-NAME
       DISPLAY "Strength for creature " FUNCTION TRIM(ENCOUNT-KEY)
       ACCEPT ENEMY-STRENGTH
       DISPLAY "Health for creature " FUNCTION TRIM(ENCOUNT-KEY)
       ACCEPT ENEMY-HEALTH
       DISPLAY "Gold for creature " FUNCTION TRIM(ENCOUNT-KEY)
       ACCEPT ENEMY-GOLD
       WRITE ENCOUNTER-DATA
       PERFORM FILE-STATUS
       END-PERFORM
       CLOSE ENCOUNTERS
       PERFORM FILE-STATUS
       GOBACK.

       FILE-STATUS.
       DISPLAY "File status returned as " WITH NO ADVANCING
       IF F-STAT IS ZERO
           DISPLAY "OK"
       ELSE
           DISPLAY "ERROR " F-STAT
       END-IF.
       END PROGRAM filecreate.
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. WRDSRT01.
       AUTHOR. STELEE.
       
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WORD-1 PIC X(50).
       01  WORD-2 PIC X(50).
       
       PROCEDURE DIVISION.
       PROGRAM-BEGIN.
           PERFORM GET-WORDS.
           PERFORM DISPLAY-SORTED-WORDS.
       
       PROGRAM-DONE.
           STOP RUN.

       GET-WORDS.
           DISPLAY "Enter the first word: " WITH NO ADVANCING.
           ACCEPT WORD-1.
           DISPLAY "Enter the second word: " WITH NO ADVANCING.
           ACCEPT WORD-2.
       DISPLAY-SORTED-WORDS.
           IF WORD-1 < WORD-2
               DISPLAY WORD-1
               DISPLAY WORD-2
           ELSE
               DISPLAY WORD-2 
               DISPLAY WORD-1.

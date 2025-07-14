       IDENTIFICATION DIVISION.
       PROGRAM-ID. YESNO01.
       AUTHOR. STELEE.
       
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  YES-OR-NO PIC X.
       
       PROCEDURE DIVISION.
       PROGRAM-BEGIN.
           PERFORM GET-YES-OR-NO.
           PERFORM EDIT-YES-OR-NO.
           PERFORM DISPLAY-YES-OR-NO.
       
       PROGRAM-DONE.
           STOP RUN.

       GET-YES-OR-NO.
           DISPLAY "Enter Y or N: " WITH NO ADVANCING.
           ACCEPT YES-OR-NO.
       EDIT-YES-OR-NO.
           IF YES-OR-NO IS EQUAL "y"
               MOVE "Y" TO YES-OR-NO.
           IF YES-OR-NO IS EQUAL "n"
               MOVE "N" TO YES-OR-NO.
       IT-IS-VALID.
           DISPLAY "It is valid".

       DISPLAY-YES-OR-NO.
           IF YES-OR-NO IS EQUAL "Y"
               PERFORM IT-IS-VALID
               DISPLAY "You entered Y".
           IF YES-OR-NO IS EQUAL "N"
               PERFORM IT-IS-VALID
               DISPLAY "You entered N".
           IF YES-OR-NO IS NOT EQUAL "Y" AND NOT EQUAL "N"
               DISPLAY "Invalid input".


       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO03.
       AUTHOR. STELEE.
       
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  THE-MESSAGE PIC X(20).
       01  THE-NAME PIC X(10).
       01  THE-NUMBER PIC 9(2).
       
       PROCEDURE DIVISION.
       PROGRAM-BEGIN.
           DISPLAY "ENTER YOUR NAME:".
           ACCEPT THE-NAME.
           MOVE "HELLO " TO THE-MESSAGE.
           MOVE 1 TO THE-NUMBER.
           DISPLAY "MESSAGE "
                   THE-NUMBER
                   ": "
                   THE-MESSAGE 
                   THE-NAME.

           MOVE "SAY GOODBYE," TO THE-MESSAGE.
           MOVE 2 TO THE-NUMBER.
           DISPLAY "MESSAGE "
                   THE-NUMBER
                   ": "
                   THE-MESSAGE 
                   THE-NAME.
       PROGRAM-DONE.
           STOP RUN.

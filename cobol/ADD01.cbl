       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADD01.
       AUTHOR. STELEE.
       
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  NUM1 PIC 9(2).
       01  NUM2 PIC 9(2).
       01  RESULT PIC 9(2).
       
       PROCEDURE DIVISION.
       PROGRAM-BEGIN.
           DISPLAY "ENTER FIRST NUMBER: ".
           ACCEPT NUM1.
           DISPLAY "ENTER SECOND NUMBER: ".
           ACCEPT NUM2.
           COMPUTE RESULT = NUM1 + NUM2.
           DISPLAY "RESULT: " RESULT.
       PROGRAM-DONE.
           STOP RUN.

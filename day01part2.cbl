       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY02.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT FILE01 ASSIGN TO "input/input01add0.txt"
       ORGANIZATION IS LINE SEQUENTIAL.            

       DATA DIVISION.
       FILE SECTION.
       FD FILE01.
       01 FILE01-FILE.
           05 MEASURMENT PIC 9(4).

       WORKING-STORAGE SECTION.
       01 WS-FILE01.
           05 WS-MEASURMENT PIC 9(4).
       01 WS-EOF PIC A(1). 

       01 WS-COUNT PIC 9(8).
       01 WS-READS PIC 9(8).
       01 WS-VALUE1 PIC 9(8).
       01 WS-VALUE2 PIC 9(8).
       01 WS-VALUE3 PIC 9(8).
       01 WS-SUM1 PIC 9(8) VALUE 99999.
       01 WS-SUM2 PIC 9(8).                                                
    
       PROCEDURE DIVISION.
           OPEN INPUT FILE01.
           READ FILE01 INTO WS-FILE01.
           MOVE WS-MEASURMENT TO WS-VALUE1
           READ FILE01 INTO WS-FILE01.
           MOVE WS-MEASURMENT TO WS-VALUE2
           DISPLAY WS-COUNT
           PERFORM UNTIL WS-EOF='Y'

           ADD 1 TO WS-READS
           READ FILE01 INTO WS-FILE01
              AT END MOVE 'Y' TO WS-EOF
              NOT AT END                                                        
        
              ADD WS-VALUE1 WS-VALUE2 WS-MEASURMENT GIVING WS-SUM2
        
              IF WS-SUM2 > WS-SUM1
                 ADD 1 TO WS-COUNT
              END-IF
        
              MOVE WS-SUM2 TO WS-SUM1
        
              MOVE WS-VALUE2 TO WS-VALUE1
              MOVE WS-MEASURMENT TO WS-VALUE2
           END-READ
           END-PERFORM.

           DISPLAY WS-COUNT.
           CLOSE FILE01.
           STOP RUN.

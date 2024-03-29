       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
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

       01 WS-COUNT PIC 9(7).
       01 WS-VALUE PIC 9(5) VALUE 9999.

       PROCEDURE DIVISION.
           OPEN INPUT FILE01.

               PERFORM UNTIL WS-EOF='Y'
                  READ FILE01 INTO WS-FILE01
                     AT END MOVE 'Y' TO WS-EOF
                     NOT AT END
                       IF WS-MEASURMENT > WS-VALUE
                           ADD 1 TO WS-COUNT
                       END-IF
                       MOVE WS-MEASURMENT TO WS-VALUE
                   END-READ
               END-PERFORM.

               DISPLAY WS-COUNT.
           CLOSE FILE01.
           STOP RUN.

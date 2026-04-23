       IDENTIFICATION DIVISION.
       PROGRAM-ID. Operations.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 OPERATION-TYPE     PIC X(6).
       01 AMOUNT             PIC 9(6)V99.
       01 FINAL-BALANCE      PIC 9(6)V99 VALUE 1000.00.

       LINKAGE SECTION.
       01 PASSED-OPERATION   PIC X(6).

       
       PROCEDURE DIVISION USING PASSED-OPERATION.
           MOVE PASSED-OPERATION TO OPERATION-TYPE
           PERFORM READ-BALANCE

           EVALUATE OPERATION-TYPE
               WHEN 'TOTAL '
                   DISPLAY "Current balance: " FINAL-BALANCE
               WHEN 'CREDIT'
                   DISPLAY "Enter credit amount: "
                   ACCEPT AMOUNT
                   ADD AMOUNT TO FINAL-BALANCE
                   PERFORM WRITE-BALANCE
                   DISPLAY "Amount credited. New balance: " FINAL-BALANCE
               WHEN 'DEBIT '
                   DISPLAY "Enter debit amount: "
                   ACCEPT AMOUNT
                   IF FINAL-BALANCE >= AMOUNT
                       SUBTRACT AMOUNT FROM FINAL-BALANCE
                       PERFORM WRITE-BALANCE
                       DISPLAY "Amount debited. New balance: " FINAL-BALANCE
                   ELSE
                       DISPLAY "Insufficient funds for this debit."
                   END-IF
               WHEN OTHER
                   DISPLAY "Invalid operation type."
           END-EVALUATE
           GOBACK.

       READ-BALANCE.
           CALL 'DataProgram' USING 'READ', FINAL-BALANCE.

       WRITE-BALANCE.
           CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE.

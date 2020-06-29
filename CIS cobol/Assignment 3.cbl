      ******************************************************************
      * Author: Schylar Utley
      * Date: 9/14/19
      * Purpose: Read an input file of payroll records
      * Title: CH0601.CBL
      * Date run: 9/14/19
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CH0601.
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
               SELECT EMPLOYEES ASSIGN TO 'Employeeinput.dat'
                   ORGANIZATION IS LINE SEQUENTIAL.
               SELECT SALARY-OUT ASSIGN TO 'EmployeeSalary.dat'
                   ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.

       FILE SECTION.
       FD EMPLOYEES.
       01 IN-RECORD.
           05 Name         PIC X(20).
           05 Wage         PIC 9(5).
           05 Dependents   PIC 9(1).
           05 FICA         PIC 9(5).
           05 State_Tax    PIC 9(6).
           05 Fed_Tax      Pic 9(6).
           05 DOH          Pic 9(8).
       FD SALARY-OUT.
       01 OUT-RECORD.
           05 Name           PIC X(20).
           05 Salary         PIC 9(5).

       WORKING-STORAGE SECTION.
       01 SWITCHES.
           05 EOF-SWITCH       PIC X VALUE "N".
       01 COUNTERS.
           05 REC-COUNTER      PIC 9(3) VALUE 0.
       01 WAGES.
           05 COMPUTED-SALARY    PIC 9(5) VALUE 0.
       01  HOURS_WORKED    PIC 9(2) value 40.
       01  HEADER-OUT.
           05 HEADER           PIC X(55).
       PROCEDURE DIVISION.

       000-MAIN.
           PERFORM 100-INITIALIZE
           PERFORM 200-PROCESS-RECORDS
               UNTIL EOF-SWITCH = "Y".
           PERFORM 300-WRITE-RECORDS
           PERFORM 400-TERMINATE.

       100-INITIALIZE.
            OPEN INPUT EMPLOYEES.
            OPEN Output SALARY-OUT.
       200-PROCESS-RECORDS.

           READ EMPLOYEES
               AT END
                   MOVE "Y" TO EOF-SWITCH
               NOT AT END

                   COMPUTE COMPUTED-SALARY = Wage
                   DISPLAY COMPUTED-SALARY
                   PERFORM 300-WRITE-RECORDS
                   COMPUTE REC-COUNTER = REC-COUNTER + 1
            END-READ.


       300-WRITE-RECORDS.
               MOVE COMPUTED-SALARY to Salary
               MOVE Name of IN-RECORD to Name of OUT-RECORD
               WRITE OUT-RECORD
               END-WRITE.
       400-TERMINATE.
           CLOSE EMPLOYEES.
           CLOSE SALARY-OUT.
           STOP RUN.
       END PROGRAM CH0601.

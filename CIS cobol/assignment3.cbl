      ******************************************************************
      * Author: Schylar Utley
      * Date: 9/17/19
      * Purpose: Read an input file of employee names and salaries and
      *        create new file with employee name, current salary, and
      *        new salary (5% raise).
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

       FD SALARY-OUT.
       01 HEADER-OUT.
           05 Header-OUT-OUT PIC X(3).
       01  FOOTER-OUT.
           05 Counter-OUT    PIC X(100).

       FD EMPLOYEES.
       01 IN-RECORD.
           05 FullName-IN  PIC X(20).
           05 Wage         PIC 9(5).
           05 Dependents   PIC 9(1).
           05 FICA         PIC 9(5).
           05 State_Tax    PIC 9(6).
           05 Fed_Tax      Pic 9(6).
           05 DOH          Pic 9(8).
       FD SALARY-OUT.

       01 OUT-RECORD.
           05 FullName-OUT   PIC X(20).
           05 Salary         PIC 9(5).
           05 NewSalary      PIC 9(5).


       WORKING-STORAGE SECTION.
       01 SWITCHES.
           05 EOF-SWITCH       PIC X VALUE "N".
       01 COUNTERS.
           05 REC-COUNTER      PIC 9(3) VALUE 0.
       01 WAGES.
           05 COMPUTED-SALARY    PIC 9(4)V99 VALUE 0.
       01  HOURS_WORKED    PIC 9(2) value 40.
       PROCEDURE DIVISION.

       000-MAIN.
           PERFORM 100-INITIALIZE
           PERFORM 200-PROCESS-RECORDS
               UNTIL EOF-SWITCH = "Y".
           PERFORM 300-WRITE-RECORDS
           PERFORM 400-TERMINATE.

       100-INITIALIZE.
           OPEN INPUT EMPLOYEES.

       200-PROCESS-RECORDS.

           READ EMPLOYEES
               AT END
                   MOVE "Y" TO EOF-SWITCH
               NOT AT END

                   COMPUTE COMPUTED-SALARY = Wage * HOURS_WORKED
                   DISPLAY COMPUTED-SALARY
                   PERFORM 300-WRITE-RECORDS
                   COMPUTE REC-COUNTER = REC-COUNTER + 1
            END-READ.


       300-WRITE-RECORDS.
           OPEN extend SALARY-OUT.
               MOVE FullName-IN to FullName-OUT
               MOVE COMPUTED-SALARY to Salary
               MOVE "123" to Header-OUT-OUT
               WRITE "OUT-RECsdfsdfs"
               END-WRITE.
           CLOSE SALARY-OUT.

       400-TERMINATE.
           CLOSE EMPLOYEES.
           STOP RUN.
       END PROGRAM CH0601.

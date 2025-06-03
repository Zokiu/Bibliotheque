       IDENTIFICATION DIVISION.
       PROGRAM-ID. bibliotheque.
       AUTHOR.    Yassine&Benoit&Terry.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT F-INPUT
               ASSIGN TO "livres-input.dat"
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS F-INPUT-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD F-INPUT.
       
       01  REC-LIVRE.
           03 R-CODE             PIC X(13).
           03 R-TITRE            PIC X(38).
           03 R-A-NOM            PIC X(22).
           03 R-A-PRENOM         PIC X(22).
           03 R-TYPE             PIC X(16).
           03 R-ANNEE            PIC X(04).
           03 R-EDITION          PIC X(23).


       WORKING-STORAGE SECTION.
       
       01  F-INPUT-STATUS        PIC X(02) VALUE SPACE.
           88 F-INPUT-STATUS-OK  VALUE "00".
           88 F-INPUT-STATUS-EOF VALUE "10".

       01  WS-LIVRE.
           05 MAX-LIVRE          PIC 9(03).
           05 WS-LIVRE-TABLE OCCURS 999 TIMES INDEXED BY IDX-LIVRE.
               10 WS-LIVRE-ID    PIC 9(03).
               10 WS-CODE        PIC 9(13).
               10 WS-TITRE       PIC X(38).
               10 WS-TYPE        PIC X(16).
               10 WS-ANNEE       PIC X(04).
               10 WS-EDITION     PIC X(23).
               10 WS-COMPT       PIC 9(03).
               10 WS-NBR-EMPRUNT PIC 9(03).
               10 WS-DISPO       PIC X(03) VALUE SPACE.
                  88 WS-DISPO-OK VALUE "OUI".
                  88 WS-DISPO-KO VALUE "NON".

       01  WS-AUTEUR.
           05 MAX-AUTEUR         PIC 9(03).
           05 WS-AUTEUR-TABLE OCCURS 999 TIMES INDEXED BY IDX-AUTEUR.
                10 WS-A-ID       PIC 9(03).
                10 WS-A-NOM      PIC X(22).
                10 WS-A-PRENOM   PIC X(22).

       01  WS-EMPRUNT.
           05 MAX-EMPRUNT        PIC 9(03).
           05 WS-EMPRUNT-TABLE OCCURS 999 TIMES INDEXED BY IDX-EMPRUNT.
                10 WS-E-CODE     PIC 9(13).
                10 WS-E-NOM      PIC X(22).
                10 WS-E-PRENOM   PIC X(22).
                10 WS-E-TEL      PIC X(10).
                10 WS-E-DATE     PIC X(10).
                10 WS-E-RETOUR   PIC X(10).

       01  WS-CRUD               PIC 9(01) VALUE 1.
       01  WS-SAISIE             PIC X(255).

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
           01 LIVRE-ID           PIC X(03).
           01 LIVRE-CODE         PIC X(13).
           01 LIVRE-TITRE        PIC X(38).
           01 LIVRE-A-ID         PIC X(03).
           01 LIVRE-TYPE         PIC X(16).
           01 LIVRE-ANNEE        PIC X(04).
           01 LIVRE-EDITION      PIC X(23).
           01 LIVRE-COMPT        PIC X(03).
           01 LIVRE-DISPO        PIC X(03).

           01 AUTEUR-ID          PIC X(03).
           01 AUTEUR-NOM         PIC X(22).
           01 AUTEUR-PRENOM      PIC X(22).

           01 EMPRUNT-CODE       PIC X(13).
           01 EMPRUNT-NOM        PIC X(22).
           01 EMPRUNT-PRENOM     PIC X(22).
           01 EMPRUNT-TELEPHONE  PIC X(10).
           01 EMPRUNT-DATE       PIC X(10).
           01 EMPRUNT-RETOUR     PIC X(10).

           01 DB-UTILISATEUR     PIC X(20).
           01 DB-MDP             PIC X(20).
           01 DB-NAME            PIC X(20).
       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.


       PROCEDURE DIVISION.

           PERFORM 0000-CONNEXION-DEB
           THRU    0000-CONNEXION-FIN.

           PERFORM 0100-CREATION-TABLE-DEB
           THRU    0100-CREATION-TABLE-FIN.

           STOP RUN.

      ******************************************************************
       
       0000-CONNEXION-DEB.
           DISPLAY "Connexion a la base de donnee...".
           DISPLAY "Veuillez renseigner votre username: ".
           ACCEPT USERNAME.
           DISPLAY "Veuillez renseigner votre mot de passe: ".
           ACCEPT PASSWD.
           DISPLAY "Veuillez renseigner la base de donnee: ".
           ACCEPT DBNAME.
       0000-CONNEXION-FIN.

       0100-CREATION-TABLE-DEB.
           EXEC SQL 
           CREATE TABLE IF NOT EXISTS 'livre' (
            id SERIAL PRIMARY KEY,
            code CHAR(13),
            titre CHAR(38),
            auteur-id CHAR(03),
            type CHAR(16),
            annee CHAR(04),
            edition CHAR(23),
            dispo CHAR(03)
           )
           END-EXEC.

           EXEC SQL  
           CREATE TABLE IF NOT EXISTS 'Auteur' (
            id SERIAL PRIMARY KEY,
            nom CHAR(22),
            prenom CHAR(22),
           )
           END-EXEC.

           EXEC SQL  
           CREATE TABLE IF NOT EXISTS 'emprunt' (
            code CHAR(13),
            nom CHAR(22),
            prenom CHAR(22),
            tel CHAR(10),
            date CHAR(10),
            retour CHAR(10)
           )
           END-EXEC.
       0100-CREATION-TABLE-FIN.

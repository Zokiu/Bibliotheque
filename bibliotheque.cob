      ******************************************************************
      *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. bibliotheque.
       AUTHOR.    Yassine&Benoit&Terry.

      ******************************************************************
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

      ******************************************************************
      *
      ******************************************************************
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

      ******************************************************************
      *
      ******************************************************************
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

       01  WS-CHOIX              PIC 9(01).
       01  WS-SAISIE             PIC X(255).

OCESQL*EXEC SQL BEGIN DECLARE SECTION END-EXEC.
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
           01 DB-NOM             PIC X(20).
OCESQL*EXEC SQL END DECLARE SECTION END-EXEC.

OCESQL*EXEC SQL INCLUDE SQLCA END-EXEC.
OCESQL     copy "sqlca.cbl".

      ******************************************************************
      *
      ******************************************************************
OCESQL*
OCESQL 01  SQ0001.
OCESQL     02  FILLER PIC X(187) VALUE "USE DB-NOM CREATE TABLE IF NOT"
OCESQL  &  " EXISTS 'livre' ( id SERIAL PRIMARY KEY, code CHAR(13), ti"
OCESQL  &  "tre CHAR(38), auteur-id CHAR(03), type CHAR(16), annee CHA"
OCESQL  &  "R(04), edition CHAR(23), dispo CHAR(03) )".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0002.
OCESQL     02  FILLER PIC X(104) VALUE "USE DB-NOM CREATE TABLE IF NOT"
OCESQL  &  " EXISTS 'Auteur' ( id SERIAL PRIMARY KEY, nom CHAR(22), pr"
OCESQL  &  "enom CHAR(22), )".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0003.
OCESQL     02  FILLER PIC X(142) VALUE "USE DB-NOM CREATE TABLE IF NOT"
OCESQL  &  " EXISTS 'emprunt' ( code CHAR(13), nom CHAR(22), prenom CH"
OCESQL  &  "AR(22), tel CHAR(10), date CHAR(10), retour CHAR(10) )".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
       PROCEDURE DIVISION.

           PERFORM 0000-CONNEXION-DEB
           THRU    0000-CONNEXION-FIN.

           PERFORM 0100-CREATION-TABLE-DEB
           THRU    0100-CREATION-TABLE-FIN.

           PERFORM 0200-MENU-DEB
           THRU    0200-MENU-FIN.

           STOP RUN.

      ******************************************************************
       
       0000-CONNEXION-DEB.
           DISPLAY "Connexion a la base de donnee...".
           DISPLAY "Veuillez renseigner votre username: ".
           ACCEPT DB-UTILISATEUR.
           DISPLAY "Veuillez renseigner votre mot de passe: ".
           ACCEPT DB-MDP.
           DISPLAY "Veuillez renseigner la base de donnee: ".
           ACCEPT DB-NOM.

OCESQL*    EXEC SQL 
OCESQL*    CONNECT :DB-UTILISATEUR IDENTIFIED BY :DB-MDP USING :DB-NOM 
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLConnect" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE DB-UTILISATEUR
OCESQL          BY VALUE 20
OCESQL          BY REFERENCE DB-MDP
OCESQL          BY VALUE 20
OCESQL          BY REFERENCE DB-NOM
OCESQL          BY VALUE 20
OCESQL     END-CALL.

           IF SQLCODE NOT = 0
             DISPLAY "Erreur de connexion SQLCODE: " SQLCODE
           END-IF.

           EXIT.
       0000-CONNEXION-FIN.

      ******************************************************************
       0100-CREATION-TABLE-DEB.
OCESQL*    EXEC SQL 
OCESQL*    USE DB-NOM;
OCESQL*    USE DB-NOM;
OCESQL*    CREATE TABLE IF NOT EXISTS 'livre' (
OCESQL*     id SERIAL PRIMARY KEY,
OCESQL*     code CHAR(13),
OCESQL*     titre CHAR(38),
OCESQL*     auteur-id CHAR(03),
OCESQL*     type CHAR(16),
OCESQL*     annee CHAR(04),
OCESQL*     edition CHAR(23),
OCESQL*     dispo CHAR(03)
OCESQL*    )
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0001
OCESQL     END-CALL.

OCESQL*    EXEC SQL
OCESQL*    USE DB-NOM;
OCESQL*    CREATE TABLE IF NOT EXISTS 'Auteur' (
OCESQL*     id SERIAL PRIMARY KEY,
OCESQL*     nom CHAR(22),
OCESQL*     prenom CHAR(22),
OCESQL*    )
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0002
OCESQL     END-CALL.

OCESQL*    EXEC SQL
OCESQL*    USE DB-NOM;
OCESQL*    CREATE TABLE IF NOT EXISTS 'emprunt' (
OCESQL*     code CHAR(13),
OCESQL*     nom CHAR(22),
OCESQL*     prenom CHAR(22),
OCESQL*     tel CHAR(10),
OCESQL*     date CHAR(10),
OCESQL*     retour CHAR(10)
OCESQL*    )
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0003
OCESQL     END-CALL.

           EXIT.
       0100-CREATION-TABLE-FIN.

      ******************************************************************
       0200-MENU-DEB.

           IF SQLCODE = 0
             MOVE 1 TO WS-CHOIX
             PERFORM UNTIL WS-CHOIX = 0
               DISPLAY "1 - Ajouter un enregistrement"
               DISPLAY "2 - Afficher un enregistrement"
               DISPLAY "3 - Mettre a jour un enregistrement"
               DISPLAY "4 - Supprimer un enregistrement"
               DISPLAY "9 - Quitter"
               ACCEPT WS-CHOIX
               EVALUATE WS-CHOIX
                   WHEN = 1
                       PERFORM 0210-AJOUT-DEB
                       THRU    0210-AJOUT-FIN
                   WHEN = 2
                       PERFORM 0220-LIRE-DEB
                       THRU    0220-LIRE-FIN
                   WHEN = 3
                       PERFORM 0230-MAJ-DEB
                       THRU    0230-MAJ-FIN
                   WHEN = 4
                       PERFORM 0240-SUPPR-DEB
                       THRU    0240-SUPPR-FIN
                   WHEN OTHER
                       DISPLAY "Mauvaise saisie, veuillez recommencer"
             END-PERFORM

             

           END-IF.

           EXIT.
       0200-MENU-FIN.

      ******************************************************************
       0210-AJOUT-DEB.
      *Ajout table livre + Auteur (Si n'exsite pas).
           
           EXIT.
       0210-AJOUT-FIN.

      ******************************************************************     
       0220-LIRE-DEB.
      *Lecture Livre JOINTURE Auteur + Emprunt(Si livre pas dispo).

           EXIT.
       0220-LIRE-FIN.

      ******************************************************************
       0230-MAJ-DEB.
           MOVE 1 TO WS-CHOIX.
      *Menu choix de table pour maj.

           EXIT.
       0230-MAJ-FIN.

      ******************************************************************
       0240-SUPPR-DEB.
           MOVE 1 TO WS-CHOIX.
      *Menu pour choix entre livre ou emprunt.

           EXIT.
       0240-SUPPR-FIN.

      ******************************************************************



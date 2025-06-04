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
       01  WS-CHOIX-2            PIC 9(01).
       01  WS-CHOIX-3            PIC 9(01).

       01  WS-SAISIE             PIC X(255).

       01  WS-REPONSE-AJ         PIC X(01) VALUE "O".
       
       01  WS-CALCUL             PIC 9(03).
       01  WS-COMPT-TEMP         PIC 9(03).

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
           01 LIVRE-ID           PIC X(03).
           01 LIVRE-CODE         PIC X(13).
           01 LIVRE-TITRE        PIC X(38).
           01 LIVRE-A-ID         PIC X(03).
           01 LIVRE-TYPE         PIC X(16).
           01 LIVRE-ANNEE        PIC X(04).
           01 LIVRE-EDITION      PIC X(23).
           01 LIVRE-COMPT        PIC X(03).
           01 LIVRE-PRET         PIC X(03).
           01 LIVRE-DISPO        PIC X(03).

           01 AUTEUR-ID          PIC X(03).
           01 AUTEUR-NOM         PIC X(22).
           01 AUTEUR-PRENOM      PIC X(22).

           01 EMPRUNT-ID         PIC X(03).
           01 EMPRUNT-NOM        PIC X(22).
           01 EMPRUNT-PRENOM     PIC X(22).
           01 EMPRUNT-TELEPHONE  PIC X(10).
           01 EMPRUNT-DATE       PIC X(10).
           01 EMPRUNT-RETOUR     PIC X(10).

           01 DB-UTILISATEUR     PIC X(20).
           01 DB-MDP             PIC X(20).
           01 DB-NOM             PIC X(20).
       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.

      ******************************************************************
      *
      ******************************************************************
       PROCEDURE DIVISION.

           PERFORM 0000-CONNEXION-DEB
           THRU    0000-CONNEXION-FIN.

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

           EXEC SQL 
           CONNECT :DB-UTILISATEUR IDENTIFIED BY :DB-MDP USING :DB-NOM 
           END-EXEC.

           IF SQLCODE NOT = 0
             DISPLAY "Erreur de connexion SQLCODE: " SQLCODE
           END-IF.

           EXIT.
       0000-CONNEXION-FIN.

      ******************************************************************
       
       0200-MENU-DEB.

           IF SQLCODE = 0
             MOVE 1 TO WS-CHOIX
             PERFORM UNTIL WS-CHOIX = 0
               DISPLAY " "
               DISPLAY "1 - Ajouter un enregistrement"
               DISPLAY "2 - Afficher un enregistrement"
               DISPLAY "3 - Mettre a jour un enregistrement"
               DISPLAY "4 - Supprimer un enregistrement"
               DISPLAY "0 - Quitter"
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
                   WHEN = 0
                       DISPLAY "Vous quittez le programme"
                   WHEN OTHER
                       DISPLAY "Mauvaise saisie, veuillez recommencer"
             END-PERFORM

             

           END-IF.

           EXIT.
       0200-MENU-FIN.

      ******************************************************************
       0210-AJOUT-DEB.
      *Ajout table livre + Auteur (Si n'exsite pas).
           MOVE 0 TO WS-REPONSE-AJ.
       PERFORM UNTIL WS-REPONSE-AJ = "N"

           DISPLAY "Entrez le code de livre : "
           ACCEPT LIVRE-CODE 
           
           EXEC SQL
              SELECT livre_id, code, titre, auteur_id, type, annee, 
                     edition, compt, pret, dispo
              INTO :LIVRE-ID, :LIVRE-CODE, :LIVRE-TITRE, :LIVRE-A-ID,
                   :LIVRE-TYPE, :LIVRE-ANNEE, :LIVRE-EDITION,
                   :LIVRE-COMPT, :LIVRE-PRET, :LIVRE-DISPO
             FROM livre
             WHERE code = :LIVRE-CODE
           END-EXEC

           IF SQLCODE = 0
             DISPLAY "Livre deja existant."
             DISPLAY " "
             DISPLAY "ID Livre:  "  LIVRE-ID
             DISPLAY "ISBN:      "  LIVRE-CODE
             DISPLAY "Titre:     "  LIVRE-TITRE
             DISPLAY "ID Auteur: "  LIVRE-A-ID
             DISPLAY "Type:      "  LIVRE-TYPE
             DISPLAY "Annee:     "  LIVRE-ANNEE
             DISPLAY "Editeur:   "  LIVRE-EDITION
             DISPLAY "Compte:    "  LIVRE-COMPT

      * posibiliter ajouter compte de livre     
             DISPLAY "Ajoutez nombres des exemplaires : "
             DISPLAY "Tapez 0 pour non rien ajouter :  "

             MOVE LIVRE-COMPT TO WS-COMPT-TEMP
             ACCEPT WS-CALCUL

             ADD WS-CALCUL TO WS-COMPT-TEMP
             MOVE WS-COMPT-TEMP TO LIVRE-COMPT

           
             EXEC SQL
                 UPDATE livre
                 SET compt = :LIVRE-COMPT
                 WHERE code = :LIVRE-CODE
             END-EXEC
     
             IF SQLCODE = 0
                 EXEC SQL COMMIT END-EXEC
                 DISPLAY ">>> LES exemplaires bien ajoutes."
             ELSE
                 DISPLAY "Erreur mise a jour. SQLCODE: " 
                 SQLCODE
             END-IF

           ELSE
             DISPLAY "Livre pas trouve. Vous pouvez ajouter."
         
             DISPLAY "Entrez le titre de livre"
             ACCEPT  LIVRE-TITRE
  
             DISPLAY "Entrez le nom d'auteur "
             ACCEPT AUTEUR-NOM

             DISPLAY "Entrez le prenom d'auteur"
             ACCEPT AUTEUR-PRENOM 
             


             EXEC SQL
                SELECT auteur_id INTO :AUTEUR-ID
                FROM auteur
                WHERE nom = :AUTEUR-NOM AND prenom = :AUTEUR-PRENOM
             END-EXEC

             IF SQLCODE NOT = 0 THEN
                DISPLAY ">>> Auteur pas trouve. l'ajoutez."

                EXEC SQL
                    INSERT INTO auteur (nom, prenom)
                    VALUES (:AUTEUR-NOM, :AUTEUR-PRENOM)
                END-EXEC 

                IF SQLCODE = 0 THEN
                    EXEC SQL COMMIT END-EXEC
                    DISPLAY ">>> Auteur ajoute."
                    DISPLAY "ID Auteur: " AUTEUR-ID
                    DISPLAY "Nom:       " AUTEUR-NOM
                    DISPLAY "Prenom:    " AUTEUR-PRENOM

                   EXEC SQL
                     SELECT auteur_id INTO :AUTEUR-ID
                     FROM auteur
                     WHERE nom = :AUTEUR-NOM AND prenom = :AUTEUR-PRENOM
                   END-EXEC

                   MOVE AUTEUR-ID TO LIVRE-A-ID 

                   DISPLAY "Entrez le type de livre "
                   ACCEPT LIVRE-TYPE
           
                   DISPLAY "Entrez l'annee de sortie"
                   ACCEPT LIVRE-ANNEE
           
                   DISPLAY "Entrez l'edition "
                   ACCEPT LIVRE-EDITION
           
                   DISPLAY "Entrez le nombme de livre"
                   ACCEPT LIVRE-COMPT

                   MOVE "OUI" TO LIVRE-DISPO
           
                   EXEC SQL
                    INSERT INTO livre
                    (code, titre, auteur_id, type, annee, edition, 
                    compt, pret, dispo)
                    VALUES (:LIVRE-CODE, :LIVRE-TITRE, 
                            :LIVRE-A-ID, :LIVRE-TYPE, :LIVRE-ANNEE, 
                            :LIVRE-EDITION, :LIVRE-COMPT, :LIVRE-PRET,
                            :LIVRE-DISPO)
                   END-EXEC
                   
           
                   IF SQLCODE = 0
                       EXEC SQL COMMIT END-EXEC
                       DISPLAY " >>> Livre bien ajouté."
                       DISPLAY "ID Livre   : "  LIVRE-ID
                       DISPLAY "ISBN       : "  LIVRE-CODE
                       DISPLAY "Titre      : "  LIVRE-TITRE
                       DISPLAY "ID Auteur  : "  LIVRE-A-ID
                       DISPLAY "Type       : "  LIVRE-TYPE
                       DISPLAY "Annee      : "  LIVRE-ANNEE
                       DISPLAY "Editeur    : "  LIVRE-EDITION
                       DISPLAY "Compte     : "  LIVRE-COMPT
                       DISPLAY "Nombre pret: "  LIVRE-PRET
                       DISPLAY "Disponible : "  LIVRE-DISPO
                   ELSE
                       DISPLAY "Erreur lors de l'ajout. SQLCODE: "
                                                                SQLCODE
                   END-IF
                ELSE
                    DISPLAY "Erreur insertion auteur. SQLCODE: " SQLCODE
                END-IF

              END-IF
             
           END-IF 

           DISPLAY "Voulez-vous ajouter un autre livre ? (O/N): "
           ACCEPT WS-REPONSE-AJ
           MOVE FUNCTION UPPER-CASE(WS-REPONSE-AJ) TO WS-REPONSE-AJ
       END-PERFORM.
      
           
           EXIT.
           
       0210-AJOUT-FIN.

      ******************************************************************     
       0220-LIRE-DEB.
      *Lecture Livre JOINTURE Auteur + Emprunt(Si livre pas dispo).
       DISPLAY 'Saisir le titre recherché: ' WITH NO ADVANCING.
       ACCEPT LIVRE-TITRE.
       EXEC SQL 
            SELECT code, titre, auteur_id, type, annee, edition, dispo
            INTO :LIVRE-CODE, :LIVRE-TITRE, :LIVRE-A-ID, :LIVRE-TYPE,
                 :LIVRE-ANNEE, :LIVRE-EDITION, :LIVRE-DISPO
            FROM livre
            WHERE titre = :LIVRE-TITRE
       END-EXEC.
           DISPLAY "livre" SQLCODE
       EXEC SQL 
            SELECT auteur_id, nom, prenom 
            INTO :AUTEUR-ID, :AUTEUR-NOM, :AUTEUR-PRENOM
            FROM auteur
            WHERE auteur_id = :LIVRE-A-ID
       END-EXEC.
           DISPLAY "Auteur" SQLCODE

       DISPLAY ' '.
       DISPLAY 'Code du livre------>: ' LIVRE-CODE.
       DISPLAY 'Titre-------------->: ' LIVRE-TITRE.
       DISPLAY 'Auteur------------->: ' AUTEUR-NOM ' ' AUTEUR-PRENOM.
       DISPLAY 'Type--------------->: ' LIVRE-TYPE.
       DISPLAY 'Année de publication: ' LIVRE-ANNEE.
       DISPLAY 'Edition------------>: ' LIVRE-EDITION.
       DISPLAY 'Disponible--------->: ' LIVRE-DISPO.

      * IF LIVRE-COMPT = LIVRE-PRET THEN
      *    EXEC SQL  
      *       DECLARE EMPRUNT-CURS CURSOR FOR
      *       SELECT *
      *       FROM emprunt
      *    END-EXEC
      *    EXEC SQL OPEN EMPRUNT-CURS END-EXEC
      *    EVALUATE SQLCODE
      *     WHEN = 0
      *       PERFORM UNTIL SQLCODE NOT = 0
      *      EXEC SQL 
      *           FETCH EMPRUNT-CURS
      *           INTO :EMPRUNT-ID, :LIVRE-ID, :EMPRUNT-NOM,
      *            :EMPRUNT-PRENOM, :EMPRUNT-TELEPHONE,
      *            :EMPRUNT-DATE, :EMPRUNT-RETOUR
      *           WHERE livre_id = :LIVRE-ID
      *      END-EXEC ''
      *     
      *      DISPLAY 'Emprunté par  : ' EMPRUNT-NOM ' ' EMPRUNT-PRENOM
      *      DISPLAY 'Telephone     : ' EMPRUNT-TELEPHONE
      *      DISPLAY "Date d'emprunt: " EMPRUNT-DATE
      *      DISPLAY 'Date de retour: ' EMPRUNT-RETOUR
      *       END-PERFORM
      *     WHEN OTHER
      *       DISPLAY "Erreur d'ouverture du curseur" SQLCODE
      *    END-EVALUATE
      *    
      * END-IF.

           EXIT.
       0220-LIRE-FIN.

      ******************************************************************
       0230-MAJ-DEB.
      *On réinitialise la variable pour le choix.
           MOVE 1 TO WS-CHOIX-2.
      *Menu choix de table pour maj.
           PERFORM UNTIL WS-CHOIX-2 = 0
      *On affiche les différentes possibilités.
             DISPLAY " "
             DISPLAY "1 - Modifier un livre"
             DISPLAY "2 - Modifier un auteur"
             DISPLAY "3 - Modifier un emprunt"
             DISPLAY "0 - Quitter"
      *On demande à l'utilisateur son choix.
             ACCEPT WS-CHOIX-2
      *En fonction du choix on effectue la demande voulue.
             EVALUATE WS-CHOIX-2
      *Ici on va modifier la table livre avec l'id souhaité.
               WHEN = 1
                 PERFORM 0231-MAJ-LIVRE-DEB
                 THRU    0231-MAJ-LIVRE-FIN
      *Ici on va modifier la table emprunt avec le code souhaité.
               WHEN = 2
                 PERFORM 0232-MAJ-AUTEUR-DEB
                 THRU    0232-MAJ-AUTEUR-FIN
      *Ici on va modifier la table auteur avec l'id souhaité
               WHEN = 3
                 PERFORM 0233-MAJ-EMPRUNT-DEB
                 THRU    0233-MAJ-EMPRUNT-FIN
      *Choix de sortie pour ne pas afficher le message d'erreur.
               WHEN = 0
                       DISPLAY "Vous sortez du menu de MAJ"
      *Gestion de mauvaise saisie utilisateur.
               WHEN OTHER 
                 DISPLAY "Mauvaise saisie, veuillez recommencer"
             END-EVALUATE
           END-PERFORM.

           EXIT.
       0230-MAJ-FIN.

       0231-MAJ-LIVRE-DEB.
           DISPLAY "Veuillez renseigner le titre: "
      *On demande l'id souhaité à l'utilisateur.
           ACCEPT WS-SAISIE
      *On déplace le choix dans la variable correspondante.
           MOVE WS-SAISIE TO LIVRE-TITRE
      *On affiche le livre souhaité.
           EXEC SQL  
             SELECT livre_id, code, titre, auteur_id, type, annee, 
             edition, compt, pret, dispo
             INTO :LIVRE-ID, :LIVRE-CODE, :LIVRE-TITRE, :LIVRE-A-ID,
             :LIVRE-TYPE, :LIVRE-ANNEE, :LIVRE-EDITION,
             :LIVRE-COMPT, :LIVRE-PRET, :LIVRE-DISPO
             FROM livre
             WHERE titre = :LIVRE-TITRE
           END-EXEC
      
           DISPLAY "ID Livre:  "  LIVRE-ID
           DISPLAY "ISBN:      "  LIVRE-CODE
           DISPLAY "Titre:     "  LIVRE-TITRE
           DISPLAY "ID Auteur: "  LIVRE-A-ID
           DISPLAY "Type:      "  LIVRE-TYPE
           DISPLAY "Annee:     "  LIVRE-ANNEE
           DISPLAY "Editeur:   "  LIVRE-EDITION
           DISPLAY "Compte:    "  LIVRE-COMPT

      *On réinitialise la variable de choix.
           MOVE 1 TO WS-CHOIX-3
      *On demande à l'utilisateur ce qu'il veut modifier.
           PERFORM UNTIL WS-CHOIX-3 = 0
             DISPLAY " "
             DISPLAY "1 - Modifier l'ISBN"
             DISPLAY "2 - Modifier le titre"
             DISPLAY "3 - Modifier l'auteur"
             DISPLAY "4 - Modifier le type"
             DISPLAY "5 - Modifier l'année"
             DISPLAY "6 - Modifier l'éditeur"
             DISPLAY "7 - Modifier le compte"
             DISPLAY "0 - Quitter"
             ACCEPT WS-SAISIE
             MOVE WS-SAISIE TO WS-CHOIX-3
             EVALUATE WS-CHOIX-3
               WHEN = 1
                 DISPLAY "Veuillez saisir le nouveau code: "
                 ACCEPT WS-SAISIE
                 MOVE WS-SAISIE TO LIVRE-CODE
                 EXEC SQL  
                     UPDATE livre
                     SET code = :LIVRE-CODE
                     WHERE titre = :LIVRE-TITRE
                 END-EXEC
               WHEN = 2
                 DISPLAY "Veuillez saisir le nouveau titre: "
                 ACCEPT WS-SAISIE
                 MOVE WS-SAISIE TO LIVRE-TITRE
                 EXEC SQL  
                     UPDATE livre
                     SET titre = :LIVRE-TITRE
                     WHERE titre = :LIVRE-TITRE
                 END-EXEC
               WHEN = 3
                 DISPLAY "Veuillez saisir le nouvel auteur (ID): "
                 ACCEPT WS-SAISIE
                 MOVE WS-SAISIE TO LIVRE-A-ID
                 EXEC SQL  
                     UPDATE livre
                     SET auteur_id = :LIVRE-A-ID
                     WHERE titre = :LIVRE-TITRE
                 END-EXEC
               WHEN = 4
                 DISPLAY "Veuillez saisir le nouveau type: "
                 ACCEPT WS-SAISIE
                 MOVE WS-SAISIE TO LIVRE-TYPE
                 EXEC SQL  
                     UPDATE livre
                     SET type = :LIVRE-TYPE
                     WHERE titre = :LIVRE-TITRE
                 END-EXEC
               WHEN = 5
                 DISPLAY "Veuillez saisir la nouvelle annee: "
                 ACCEPT WS-SAISIE
                 MOVE WS-SAISIE TO LIVRE-ANNEE
                 EXEC SQL  
                     UPDATE livre
                     SET annee = :LIVRE-ANNEE
                     WHERE titre = :LIVRE-TITRE
                 END-EXEC
               WHEN = 6
                 DISPLAY "Veuillez saisir le nouvel editeur: "
                 ACCEPT WS-SAISIE
                 MOVE WS-SAISIE TO LIVRE-EDITION
                 EXEC SQL  
                     UPDATE livre
                     SET edition = :LIVRE-EDITION
                     WHERE titre = :LIVRE-TITRE
                 END-EXEC
               WHEN = 7
                 DISPLAY "Veuillez saisir la nouvelle quantite: "
                 ACCEPT WS-SAISIE
                 MOVE WS-SAISIE TO LIVRE-COMPT
                 EXEC SQL  
                     UPDATE livre
                     SET compt = :LIVRE-COMPT
                     WHERE titre = :LIVRE-TITRE
                 END-EXEC
               WHEN = 0
                 DISPLAY "Vous arretez de modifier ce livre"
               WHEN OTHER
                 DISPLAY "Mauvaise saisie, veuillez recommencer"
             END-EVALUATE
      *Message de gestion d'erreur.
             IF SQLCODE = 0
               EXEC SQL COMMIT END-EXEC
               DISPLAY "Modification réussie."
             ELSE
               DISPLAY "Erreur de modification SQLCODE: " SQLCODE
             END-IF
           END-PERFORM.

           EXIT.
       0231-MAJ-LIVRE-FIN.

       0232-MAJ-AUTEUR-DEB.
           DISPLAY "Veuillez renseigner le nom: "
      *On demande le nom souhaité à l'utilisateur.
           ACCEPT WS-SAISIE
      *On déplace le choix dans la variable correspondante.
           MOVE WS-SAISIE TO AUTEUR-NOM
      *On affiche l'auteur souhaité.
           EXEC SQL  
               SELECT auteur_id, nom, prenom
               INTO :AUTEUR-ID, :AUTEUR-NOM, :AUTEUR-PRENOM
               FROM auteur
               WHERE nom = :AUTEUR-NOM
           END-EXEC

           DISPLAY "ID Auteur: " AUTEUR-ID
           DISPLAY "Nom:       " AUTEUR-NOM
           DISPLAY "Prenom:    " AUTEUR-PRENOM
          
      *On réinitialise la variable de choix.
           MOVE 1 TO WS-CHOIX-3
      *On demande à l'utilisateur ce qu'il veut modifier.
           PERFORM UNTIL WS-CHOIX-3 = 0
             DISPLAY " "
             DISPLAY "1 - Modifier le nom"
             DISPLAY "2 - Modifier le prenom"
             DISPLAY "0 - Quitter"
             ACCEPT WS-SAISIE
             MOVE WS-SAISIE TO WS-CHOIX-3
             EVALUATE WS-CHOIX-3
               WHEN = 1
                 DISPLAY "Veuillez saisir le nouveau nom"
                 ACCEPT WS-SAISIE
                 MOVE WS-SAISIE TO AUTEUR-NOM
                 EXEC SQL  
                     UPDATE auteur
                     SET nom   = :AUTEUR-NOM
                     WHERE nom = :AUTEUR-NOM
                 END-EXEC
               WHEN = 2
                 DISPLAY "Veuillez saisir le nouveau prenom"
                 ACCEPT WS-SAISIE
                 MOVE WS-SAISIE TO AUTEUR-PRENOM
                 EXEC SQL  
                     UPDATE auteur
                     SET prenom = :AUTEUR-PRENOM
                     WHERE nom  = :AUTEUR-NOM
                 END-EXEC
               WHEN = 0
                 DISPLAY "Vous arretez de modifier cet auteur"
               WHEN OTHER
                 DISPLAY "Mauvaise saisie, veuillez recommencer"
             END-EVALUATE
      *Message de gestion d'erreur.
             IF SQLCODE = 0
               EXEC SQL COMMIT END-EXEC
               DISPLAY "Modification réussie."
             ELSE
               DISPLAY "Erreur de modification SQLCODE: " SQLCODE
             END-IF
           END-PERFORM.

           EXIT.
       0232-MAJ-AUTEUR-FIN.

       0233-MAJ-EMPRUNT-DEB.
           DISPLAY "Veuillez renseigner l'id du livre: "
      *On demande l'id du livre souhaité à l'utilisateur.
           ACCEPT WS-SAISIE
      *On déplace le choix dans la variable correspondante.
           MOVE WS-SAISIE TO LIVRE-ID
      *On affiche l'emprunt choisi.
           EXEC SQL  
             SELECT emprunt_id, livre_id, nom, prenom,
                    tel, date, retour
             INTO :EMPRUNT-ID, :LIVRE-ID, :EMPRUNT-NOM,
             :EMPRUNT-PRENOM, :EMPRUNT-TELEPHONE, :EMPRUNT-DATE,
             :EMPRUNT-RETOUR
             FROM emprunt
             WHERE livre_id = :LIVRE-ID
           END-EXEC
           
           DISPLAY "ID Emprunt:            "  EMPRUNT-ID
           DISPLAY "ID Livre:              "  LIVRE-ID
           DISPLAY "Nom emprunteur:        "  EMPRUNT-NOM
           DISPLAY "Prenom emprunteur:     "  EMPRUNT-PRENOM
           DISPLAY "Telephone emprunteur:  "  EMPRUNT-TELEPHONE
           DISPLAY "Date d'emprunt:        "  EMPRUNT-DATE
           DISPLAY "Date de retour prévue: "  EMPRUNT-RETOUR
      
      *On réinitialise la variable de choix.
           MOVE 1 TO WS-CHOIX-3
      *On demande à l'utilisateur ce qu'il veut modifier
           PERFORM UNTIL WS-CHOIX-3 = 0
             DISPLAY " "
             DISPLAY "1 - Modifier le nom"
             DISPLAY "2 - Modifier le prenom"
             DISPLAY "3 - Modifier le téléphone"
             DISPLAY "4 - Modifier la date d'emprunt"
             DISPLAY "5 - Modifier la date de retour"
             DISPLAY "0 - Quitter"
             ACCEPT WS-SAISIE
             MOVE WS-SAISIE TO WS-CHOIX-3
             EVALUATE WS-CHOIX-3
               WHEN = 1
                 DISPLAY "Veuillez saisir le nouveau nom"
                 ACCEPT WS-SAISIE
                 MOVE WS-SAISIE TO EMPRUNT-NOM
                 EXEC SQL  
                   UPDATE emprunt
                   SET nom = :EMPRUNT-NOM
                   WHERE livre_id = :LIVRE-ID
                 END-EXEC
               WHEN = 2
                 DISPLAY "Veuillez saisir le nouveau prenom"
                 ACCEPT WS-SAISIE
                 MOVE WS-SAISIE TO EMPRUNT-PRENOM
                 EXEC SQL  
                   UPDATE emprunt
                   SET prenom = :EMPRUNT-PRENOM
                   WHERE livre_id = :LIVRE-ID
                 END-EXEC
               WHEN = 3
                 DISPLAY "Veuillez saisir le nouveau telephone"
                 ACCEPT WS-SAISIE
                 MOVE WS-SAISIE TO EMPRUNT-TELEPHONE
                 EXEC SQL  
                   UPDATE emprunt
                   SET tel = :EMPRUNT-TELEPHONE
                   WHERE livre_id = :LIVRE-ID
                 END-EXEC
               WHEN = 4
                 DISPLAY "Veuillez saisir la nouvelle date d'emprunt"
                 ACCEPT WS-SAISIE
                 MOVE WS-SAISIE TO EMPRUNT-DATE
                 EXEC SQL  
                   UPDATE emprunt
                   SET date = :EMPRUNT-DATE
                   WHERE livre_id = :LIVRE-ID
                 END-EXEC
               WHEN = 5
                 DISPLAY "Veuillez saisir la nouvelle date de retour "
                 ACCEPT WS-SAISIE
                 MOVE WS-SAISIE TO EMPRUNT-RETOUR
                 EXEC SQL  
                   UPDATE emprunt
                   SET retour = :EMPRUNT-RETOUR
                   WHERE livre_id = :LIVRE-ID
                 END-EXEC
               WHEN = 0
                 DISPLAY "Vous arretez de modifier cet emprunt"
               WHEN OTHER
                 DISPLAY "Mauvaise saisie, veuillez recommencer"

      *Message de gestion d'erreur.
                 IF SQLCODE = 0
                   EXEC SQL COMMIT END-EXEC
                   DISPLAY "Modification réussie."
                 ELSE
                   DISPLAY "Erreur de modification SQLCODE: " SQLCODE
                 END-IF
           END-PERFORM.

           EXIT.
       0233-MAJ-EMPRUNT-FIN.

      ******************************************************************
       0240-SUPPR-DEB.
    
      *Menu pour choix entre livre ou emprunt.
           DISPLAY 'Saisir le titre à supprimer: ' WITH NO ADVANCING.
       ACCEPT WS-SAISIE.
       MOVE WS-SAISIE TO LIVRE-TITRE.
       EXEC SQL 
            SELECT code, titre, auteur_id, type, annee, edition, compt,
                   pret,dispo
            INTO :LIVRE-CODE :LIVRE-TITRE :LIVRE-A-ID :LIVRE-TYPE
                 :LIVRE-ANNEE :LIVRE-EDITION :LIVRE-COMPT :LIVRE-PRET 
                 :LIVRE-DISPO
            FROM livre
            WHERE titre = :LIVRE-TITRE
       END-EXEC.
       
       IF SQLCODE = 0 THEN
          IF LIVRE-PRET = 0 THEN
             EXEC SQL 
                  DELETE FROM  livre 
                  WHERE titre = :LIVRE-TITRE
             END-EXEC
             IF SQLCODE = 0 THEN
                EXEC SQL COMMIT END-EXEC
                DISPLAY 'Suppression effectuée.'
             ELSE
                DISPLAY "Erreur système, la suppression n'est pas"
                         "effectuée."
             END-IF
          ELSE
             DISPLAY 'Emprunt en cour, suppression non autorisée'
          END-IF
       ELSE
             DISPLAY 'Erreur systeme, veuillez réessayer' SQLCODE
       END-IF.

           EXIT.
       0240-SUPPR-FIN.

      ******************************************************************



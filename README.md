# Bibliotheque.
## IDENTIFICATION DIVISION.
```cobol
       PROGRAM-ID. bibliotheque.
       AUTHOR.    Yassine&Benoit&Terry.
```
## ENVIRONMENT DIVISION.
### CONFIGURATION SECTION.
```cobol

```
### INPUT-OUTPUT SECTION.
#### FILE-CONTROL.
```cobol
      *Lecture séquentielle par ligne.
           SELECT F-INPUT
```
## DATA DIVISION.
### FILE SECTION.
```cobol
       FD F-INPUT.
      *Structure pour lecture fichier.
       01  REC-LIVRE.
```
### WORKING-STORAGE SECTION.
```cobol
      *Flag pour gestion du fichier d'ouverture.
       01  F-INPUT-STATUS
      *Définition d'un groupe de variable comprenant un tableau
      *pour les livres.
       01  WS-LIVRE.
      *Définition d'un groupe de variable comprenant un tableau
      *pour les auteurs.
       01  WS-AUTEUR.
      *Définition d'un groupe de variable comprenant un tableau
      *pour les emprunts.
       01  WS-EMPRUNT.

      *Multiples variables numériques pour gestion des menus.
       01  WS-CHOIX(...)

      *Variable alpha pour saisie utilisateur.
       01  WS-SAISIE

      *Variable pour boucle d'ajout.
       01  WS-REPONSE-AJ

      *Multiples variables temporaires pour calcul.
       01  WS-CALCUL
       01  WS-COMPT-TEMP
```
###### DECLARE SECTION SQL.
```cobol
      *Multiples variables pour gestion de table livre.
       01  LIVRE-(...)

      *Multiples variables pour gestion de table auteur.
       01  AUTEUR-(...)

      *Multiples variables pour gestion de table emprunt.
       01  EMPRUNT-(...)

      *Multiples variables pour connexion à la BDD.
       01  DB-(...)
```
## PROCEDURE DIVISION.
```cobol
      *Appel de tout les paragraphes pour visualisation
      *rapide de toutes les fonctions du Programme
           STOP RUN.
```
#### 0000-CONNEXION.
```cobol
      *Paragraphe gérant la connexion à la BDD
      *appelant plusieurs sous paragraphes
```
#### 0200-MENU.
```cobol
      *Paragraphe gérant le menu principal.
```
##### 0210-AJOUT.
```cobol
      *Paragraphe gérant l'ajout de livres:
      * - Création d'auteur si il n'existe pas
      * - Création livre si il n'existe pas
      * - Incrémentation du compteur si déjà existant
```
##### 0220-LIRE.
```cobol
      *Paragraphe gérant la lecture d'un enregistrement
      *Avec affichage livre + auteur correspondant
```

##### 0230-MAJ.
```cobol
      *Paragraphe gérant la mise à jour d'un enregistrement
      *appelant plusieurs sous-paragraphes.
```
###### 0231-MAJ-LIVRE.
```cobol
      *Paragraphe gérant la mise à jour d'un enregistrement
      *de la table livre champ par champ.
```
###### 0232-MAJ-AUTEUR.
```cobol
      *Paragraphe gérant la mise à jour d'un enregistrement
      *de la table auteur champ par champ.
```
###### 0232-MAJ-EMPRUNT.
```cobol
      *Paragraphe gérant la mise à jour d'un enregistrement
      *de la table emprunt champ par champ.
```
##### 0240-SUPPR.
```cobol
      *Paragraphe gérant la suppression d'un enregistrement
      *si le livre n'est pas emprunté actuellement.
```


# AXE D'AMLIORATION.
```
-
-
-
-
```

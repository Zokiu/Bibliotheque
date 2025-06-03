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
           ;

CREATE TABLE IF NOT EXISTS 'Auteur' (
            id SERIAL PRIMARY KEY,
            nom CHAR(22),
            prenom CHAR(22),
           )
           ;

CREATE TABLE IF NOT EXISTS 'emprunt' (
            code CHAR(13),
            nom CHAR(22),
            prenom CHAR(22),
            tel CHAR(10),
            date CHAR(10),
            retour CHAR(10)
           )
           ;
           
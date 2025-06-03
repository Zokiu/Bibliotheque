CREATE TABLE IF NOT EXISTS livre (
            livre_id SERIAL PRIMARY KEY NOT NULL,
            code CHAR(13),
            titre CHAR(38),
            auteur_id SERIAL REFERENCES auteur(auteur_id),
            type CHAR(16),
            annee CHAR(04),
            edition CHAR(23),
            compt CHAR(03),
            pret CHAR(03),
            dispo CHAR(03)
           )
           ;

CREATE TABLE IF NOT EXISTS auteur (
            auteur_id SERIAL PRIMARY KEY NOT NULL,
            nom CHAR(22),
            prenom CHAR(22)
           )
           ;

CREATE TABLE IF NOT EXISTS emprunt (
            emprunt_id SERIAL PRIMARY KEY NOT NULL,
            livre_id SERIAL REFERENCES livre(livre_id),
            nom CHAR(22),
            prenom CHAR(22),
            tel CHAR(10),
            date CHAR(10),
            retour CHAR(10)
           )
           ;
